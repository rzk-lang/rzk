{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Entering a scope, evaluation, and the tope solver.
--
-- These three are one recursive knot and cannot be separated:
--
--   * entering a binder needs 'whnfT', to see whether a flat variable is a point
--     of a cube and so brings a discreteness axiom with it;
--   * 'whnfT' strips an extension type's restrictions, which asks the solver
--     whether a face's tope holds ('checkTope');
--   * 'nfT' normalises under a binder and under a tope ('localTope');
--   * and the solver normalises the topes it reasons about ('nfTope').
module Rzk.TypeCheck.Eval where

import           Control.Monad               (forM, forM_, unless, when)
import           Control.Monad.Except        (runExcept)
import           Control.Monad.Reader        (ask, asks, local,
                                              runReaderT)
import           Control.Monad.Trans.Writer.CPS (runWriterT)
import           Data.List                   (intercalate, nub, nubBy,
                                              tails)
import           Data.Maybe                  (catMaybes)

import           Control.Monad.Foil          (DExt, Distinct, NameBinder)
import qualified Control.Monad.Foil          as Foil
import           Control.Monad.Free.Foil     (AST (Node, Var),
                                              ScopedAST (..))
import           Data.Bifunctor              (Bifunctor)

import           Control.Monad.Free.Foil.Annotated (AnnSig (..))
import           Language.Rzk.Foil.Syntax
import           Language.Rzk.Foil.Names    (Binder (..), TModality (..),
                                              TypeInfo (..), VarIdent)
import           Rzk.TypeCheck.Context
import           Rzk.TypeCheck.Display
import           Rzk.TypeCheck.Error
import           Rzk.TypeCheck.Monad

-- * Variables

-- | Look up a name and project one field of its 'VarInfo'.
infoOfVar :: (VarInfo n -> a) -> Foil.Name n -> TypeCheck n a
infoOfVar f x = asks (f . lookupVarInfo x)

valueOfVar :: Foil.Name n -> TypeCheck n (Maybe (TermT n))
valueOfVar = infoOfVar varValue

typeOfVar :: Foil.Name n -> TypeCheck n (TermT n)
typeOfVar = infoOfVar varType

modalityOfVar :: Foil.Name n -> TypeCheck n TModality
modalityOfVar = infoOfVar varModality

locksOfVar :: Foil.Name n -> TypeCheck n TModality
locksOfVar = infoOfVar varModAccum

isTopLevelVar :: Foil.Name n -> TypeCheck n Bool
isTopLevelVar = infoOfVar varIsTopLevel

-- | Is a surface name defined?
checkDefinedVar :: Distinct n => VarIdent -> TypeCheck n ()
checkDefinedVar name = asks (lookupNamed name) >>= \case
  Nothing -> issueTypeError (TypeErrorUndefined name)
  Just _  -> return ()

typeOfUncomputed :: TermT n -> TypeCheck n (TermT n)
typeOfUncomputed = \case
  Var x -> typeOfVar x
  t     -> case typeInfoOf t of
    Just info -> pure (infoType info)
    Nothing   -> panicImpossible "a node with no annotation"

typeOf :: Distinct n => TermT n -> TypeCheck n (TermT n)
typeOf t = typeOfUncomputed t >>= whnfT

-- | The free variables of a typed term, including those that occur only in the
-- /types/ of the variables it mentions.
--
-- A definition can depend on a section assumption without naming it: through the
-- type of something else it uses. Closing a section has to see that dependency, and
-- it is exactly what distinguishes an implicit assumption from an explicit one.
freeVarsDeep :: TermT n -> TypeCheck n [Foil.Name n]
freeVarsDeep t = do
  ctx <- ask
  let typeOfName v = varType (lookupVarInfo v ctx)

      -- a node's own free variables, and those of its type
      partial term = case typeInfoOf term of
        Nothing   -> freeVarsOfTermT term
        Just info -> freeVarsOfTermT term <> freeVarsOfTermT (infoType info)

      go vars latest
        | null new  = vars
        | otherwise = go (new <> vars) (foldMap (partial . typeOfName) new)
        where
          new = filter (`notElemName` vars) (nubNames latest)

  pure (go [] (partial t))

nubNames :: [Foil.Name n] -> [Foil.Name n]
nubNames = nubBy (\a b -> Foil.nameId a == Foil.nameId b)

elemName :: Foil.Name n -> [Foil.Name n] -> Bool
elemName x = any (\y -> Foil.nameId x == Foil.nameId y)

notElemName :: Foil.Name n -> [Foil.Name n] -> Bool
notElemName x = not . elemName x

-- * Substitution, in the monad

-- | Instantiate a scoped term with an argument: the old @substituteT@.
instantiate :: Distinct n => ScopedTermT n -> TermT n -> TypeCheck n (TermT n)
instantiate scoped arg = do
  scope <- asks ctxScope
  pure (instantiateT scope scoped arg)

-- * Entering a binder

-- | The discreteness axiom a flat cube variable brings with it: a flat point of
-- @2@ (or of @I@) is one of the endpoints. Maintained at binder entry so that
-- entailment does not have to rescan the context on every query.
discreteAxiomOf
  :: forall n l. Distinct n
  => TModality -> TermT n -> NameBinder n l -> TypeCheck n [ModalTope l]
discreteAxiomOf Flat ty binder = whnfT ty >>= \case
    Cube2T{} -> pure [endpoints cube2_0T cube2_1T]
    CubeIT{} -> pure [endpoints cubeI_0T cubeI_1T]
    _        -> pure []
  where
    z = Var (Foil.nameOf binder) :: TermT l
    endpoints zero one = plainTope (topeOrT (topeEQT z zero) (topeEQT z one))
discreteAxiomOf _ _ _ = pure []

-- | What a binder adds to the context.
binderInfo
  :: Binder -> TModality -> TermT n -> Maybe (TermT n) -> Maybe LocationInfo
  -> VarInfo n
binderInfo orig md ty mval loc = VarInfo
  { varType = ty
  , varValue = mval
  , varModality = md
  , varModAccum = Id
  , varOrig = orig
  , varIsAssumption = False
  , varIsTopLevel = False
  , varDeclaredAssumptions = []
  , varLocation = loc
  , varDataRole = Nothing
  , varMetaPrefix = 0
  }

-- | Run an action under a binder that has already been chosen.
underBinder
  :: (Distinct n, DExt n l)
  => NameBinder n l -> Binder -> TModality -> TermT n -> Maybe (TermT n)
  -> TypeCheck l a -> TypeCheck n a
underBinder binder orig md ty mval action = do
  ctx <- ask
  discrete <- discreteAxiomOf md ty binder
  let info = binderInfo orig md ty mval (ctxLocation ctx)
      ctx' = enterBinder binder info discrete ctx
  -- A new discreteness axiom changes the saturation input; an ordinary binder
  -- carries the cached value in with the rest of the context (saturation
  -- commutes with renaming).
  inContext ctx' $
    if null discrete then action else withRefreshedTopes id action

-- | Enter a fresh binder (one the checker invents) and run an action whose result
-- says nothing about the new scope.
withBinder
  :: Distinct n
  => Binder -> TModality -> TermT n
  -> (forall l. (DExt n l, Distinct l) => NameBinder n l -> TypeCheck l a)
  -> TypeCheck n a
withBinder orig md ty k = do
  scope <- asks ctxScope
  withFreshIn scope $ \binder ->
    underBinder binder orig md ty Nothing (k binder)

withFreshIn
  :: Distinct n
  => Foil.Scope n
  -> (forall l. (DExt n l, Distinct l) => NameBinder n l -> r)
  -> r
withFreshIn scope k = Foil.withFresh scope k

-- | Open a scoped term under its own binder, run the action on the body, and pack
-- the result back up as a scoped term.
underScope
  :: Distinct n
  => Binder -> TModality -> TermT n -> Maybe (TermT n)
  -> ScopedTermT n
  -> (forall l. (DExt n l, Distinct l) => TermT l -> TypeCheck l (TermT l))
  -> TypeCheck n (ScopedTermT n)
underScope orig md ty mval scoped k = do
  scope <- asks ctxScope
  withScopedT scope scoped $ \binder body ->
    ScopedAST binder <$> underBinder binder orig md ty mval (k body)

-- | Like 'underScope', for a Π (or a λ over a shape), which binds a tope scope
-- beside the body under what the user wrote as one binder.
underScope2
  :: Distinct n
  => Binder -> TModality -> TermT n
  -> ScopedTermT n -> ScopedTermT n
  -> (forall l. (DExt n l, Distinct l) => TermT l -> TermT l -> TypeCheck l (TermT l, TermT l))
  -> TypeCheck n (ScopedTermT n, ScopedTermT n)
underScope2 orig md ty scoped1 scoped2 k = do
  scope <- asks ctxScope
  withScopedT2 scope scoped1 scoped2 $ \binder body1 body2 -> do
    (r1, r2) <- underBinder binder orig md ty Nothing (k body1 body2)
    pure (ScopedAST binder r1, ScopedAST binder r2)

-- | Open a scoped term for a computation whose result says nothing about the new
-- scope (a check, or a rendered string).
inScope
  :: (Bifunctor sig, Distinct n)
  => Binder -> TModality -> TermT n -> ScopedAST NameBinder sig n
  -> (forall l. (DExt n l, Distinct l) => AST NameBinder sig l -> TypeCheck l a)
  -> TypeCheck n a
inScope orig md ty = inScopeWith orig md ty Nothing

-- | Like 'inScope', for a binder that stands for a known value (a @let@).
inScopeWith
  :: (Bifunctor sig, Distinct n)
  => Binder -> TModality -> TermT n -> Maybe (TermT n)
  -> ScopedAST NameBinder sig n
  -> (forall l. (DExt n l, Distinct l) => AST NameBinder sig l -> TypeCheck l a)
  -> TypeCheck n a
inScopeWith orig md ty mval scoped k = do
  scope <- asks ctxScope
  withScopedT scope scoped $ \binder body ->
    underBinder binder orig md ty mval (k body)

-- | Open a scoped term with a binder that has just been entered.
--
-- The scoped term lives in the enclosing scope, and so may be the codomain of the
-- type a λ is being checked against, or the tope of a shape: all of them are
-- opened under the /one/ binder the λ introduces.
openScoped
  :: (Bifunctor sig, DExt n l)
  => NameBinder n l -> ScopedAST NameBinder sig n -> TypeCheck l (AST NameBinder sig l)
openScoped binder scoped = do
  scope <- asks ctxScope
  pure (openWith scope (Foil.nameOf binder) scoped)

-- | A scope that does not use its binder: the codomain of a non-dependent function
-- type, say.
constScope :: Distinct n => TermT n -> TypeCheck n (ScopedTermT n)
constScope t = do
  scope <- asks ctxScope
  pure (Foil.withFresh scope $ \binder -> ScopedAST binder (Foil.sink t))

-- | Enter the binder of an /untyped/ scope — the body of a λ, or of a let, as the
-- user wrote it — and elaborate it into a typed one.
--
-- The binder comes from the term being checked, and the scopes of the /type/ it is
-- checked against are opened under that same binder with 'openScoped'.
elaborateUnder
  :: Distinct n
  => Binder -> TModality -> TermT n -> Maybe (TermT n)
  -> ScopedTerm n
  -> (forall l. (DExt n l, Distinct l)
        => NameBinder n l -> Term l -> TypeCheck l (TermT l))
  -> TypeCheck n (ScopedTermT n)
elaborateUnder orig md ty mval scoped k = do
  scope <- asks ctxScope
  withScopedT scope scoped $ \binder body ->
    ScopedAST binder <$> underBinder binder orig md ty mval (k binder body)

-- | Enter the binder of an untyped scope and run a computation under it.
--
-- The result may not mention the new scope — but a 'ScopedAST' /hides/ its scope,
-- so the continuation can pack whatever it built with the binder it was given and
-- hand back as many scoped terms as it likes. That is how a λ returns its
-- elaborated body, its shape tope and the type it turned out to have, all at once.
checkUnderWith
  :: Distinct n
  => Binder -> TModality -> TermT n -> Maybe (TermT n) -> ScopedTerm n
  -> (forall l. (DExt n l, Distinct l)
        => NameBinder n l -> Term l -> TypeCheck l a)
  -> TypeCheck n a
checkUnderWith orig md ty mval scoped k = do
  scope <- asks ctxScope
  withScopedT scope scoped $ \binder body ->
    underBinder binder orig md ty mval (k binder body)

checkUnder
  :: Distinct n
  => Binder -> TModality -> TermT n -> ScopedTerm n
  -> (forall l. (DExt n l, Distinct l)
        => NameBinder n l -> Term l -> TypeCheck l a)
  -> TypeCheck n a
checkUnder orig md ty = checkUnderWith orig md ty Nothing

-- * Modalities

enterModality :: Distinct n => TModality -> TypeCheck n b -> TypeCheck n b
enterModality Id action = action
enterModality md action = do
  ctx <- asks (applyModality md)
  let ctx' = ctx { ctxTopesEntailBottom = Nothing }
  -- 'applyModality' invalidated the saturation cache (accessibility changed);
  -- refresh it under the shifted context.
  inContext ctx' (withRefreshedTopes id action)

-- * The tope context

-- | Assume a tope for the enclosed action.
localTope :: Distinct n => TermT n -> TypeCheck n a -> TypeCheck n a
localTope tope tc = do
  ctx <- ask'
  tope' <- nfTope tope
  let modalTope' = plainTope tope'
  -- A small optimisation to help unify terms faster.
  let noNewInformation = case tope' of
        TopeEQT _ x y | eqT x y -> True
        _ -> any (eqModalTope modalTope') (ctxTopesNF ctx)
  if noNewInformation
    then tc
    else do
      entailsBottom <- (modalTope' : ctxTopesNF ctx) `entailM` topeBottomT
      withRefreshedTopes (extend modalTope' entailsBottom) tc
  where
    ask' = asks id
    extend tope' entailsBottom ctx = ctx
      { ctxTopes = plainTope tope : ctxTopes ctx
      , ctxTopesNF = tope' : ctxTopesNF ctx
      , ctxTopesNFUnion = map nubModalTopes
          [ new <> old
          | new <- simplifyLHSwithDisjunctions [tope']
          , old <- ctxTopesNFUnion ctx ]
      , ctxTopesEntailBottom = Just entailsBottom
      }

-- | Install a deferred saturation cache for the transformed context, and run the
-- action with it.
--
-- The pipeline's effects are discharged purely into a thunk: installing costs
-- nothing, holes recorded by the speculative run are discarded, and a pipeline
-- error (a tope guard with a hole in lenient mode, say, which the per-query path
-- would never have evaluated) becomes 'Nothing', so errors surface exactly where
-- they did before.
withRefreshedTopes
  :: Distinct n
  => (Context n -> Context n) -> TypeCheck n a -> TypeCheck n a
withRefreshedTopes f action = do
  ctx' <- asks f
  let sat = case runExcept (runWriterT (runReaderT (saturateForEntailment (ctxTopesNF ctx')) ctx')) of
        Left _       -> Nothing
        Right (s, _) -> Just s
  local (const ctx' { ctxTopesSaturated = SaturationCached sat }) action

-- | Run a check in every alternative of a disjunctive tope context.
inAllSubContexts :: Distinct n => TypeCheck n () -> TypeCheck n () -> TypeCheck n ()
inAllSubContexts handleSingle tc = do
  topeSubContexts <- asks ctxTopesNFUnion
  case topeSubContexts of
    []  -> panicImpossible "empty set of alternative contexts"
    [_] -> handleSingle
    _:_:_ ->
      forM_ topeSubContexts $ \topes' ->
        withRefreshedTopes (\ctx -> ctx
            { ctxTopes = topes'
            , ctxTopesNF = topes'
            , ctxTopesNFUnion = [topes']
            }) tc

-- * Equality of topes

eqModalTope :: Distinct n => ModalTope n -> ModalTope n -> Bool
eqModalTope l r = and
  [ tModAccum l == tModAccum r
  , tModVar l == tModVar r
  , eqT (tTope l) (tTope r)
  ]

nubModalTopes :: Distinct n => [ModalTope n] -> [ModalTope n]
nubModalTopes []       = []
nubModalTopes (t : ts) = t : nubModalTopes (filter (not . eqModalTope t) ts)

elemModalTope :: Distinct n => ModalTope n -> [ModalTope n] -> Bool
elemModalTope t = any (eqModalTope t)

-- * Entailment

-- | Monadic 'all' that stops at the first failing element.
allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM p = go
  where
    go []     = return True
    go (x:xs) = p x >>= \case
      False -> return False
      True  -> go xs

entailM :: Distinct n => [ModalTope n] -> TermT n -> TypeCheck n Bool
entailM modalTopes goal = do
  saturated <- saturateForEntailment modalTopes
  entailSaturatedM saturated goal

-- | The preprocessing 'entailM' does before searching: dedup, split off the
-- context's disjunctions, and saturate each alternative. Depends only on the
-- given topes (plus the discreteness axioms of the context), not on the goal.
saturateForEntailment
  :: Distinct n => [ModalTope n] -> TypeCheck n [[ModalTope n]]
saturateForEntailment modalTopes = do
  discreteAxioms <- asks ctxDiscreteTopes
  let topes'  = nubModalTopes (modalTopes <> discreteAxioms)
      topes'' = simplifyLHSwithDisjunctions topes'
  mapM (fmap (saturateTopes . saturateBottom) . saturateInv) topes''

-- | Search each saturated alternative for the goal.
entailSaturatedM
  :: Distinct n => [[ModalTope n]] -> TermT n -> TypeCheck n Bool
entailSaturatedM saturated goal = asks ctxVerbosity >>= \case
  Debug -> do
    naming <- asks namingOfContext
    let prettyTopes = map (ppTerm naming . untyped . tTope) (concat saturated)
        prettyTope = ppTerm naming (untyped goal)
    traceTypeCheck Debug
      ("entail " <> intercalate ", " prettyTopes <> " |- " <> prettyTope) $
        allM (`solveRHSM` goal) saturated
  _ -> allM (`solveRHSM` goal) saturated

-- | Entailment against the context's own tope context, using the cached
-- saturation when one was installed. Matching on the payload of
-- 'SaturationCached' is what forces the deferred pipeline, so the cost is paid at
-- the first query under a context, and never for one that is never queried.
entailContextM :: Distinct n => TermT n -> TypeCheck n Bool
entailContextM goal = asks ctxTopesSaturated >>= \case
  SaturationCached (Just saturated) -> entailSaturatedM saturated goal
  SaturationCached Nothing          -> fallback
  SaturationUncached                -> fallback
  where
    fallback = asks ctxTopesNF >>= (`entailM` goal)

-- * Saturation

saturateTopes :: Distinct n => [ModalTope n] -> [ModalTope n]
saturateTopes topes = saturated <> inaccessible
  where
    (accessible, inaccessible) = partitionAccessible topes
    saturated = saturateWith
      elemModalTope
      (\new old -> map plainTope (generateTopes (map tTope new) (map tTope old)))
      accessible

saturateInv :: Distinct n => [ModalTope n] -> TypeCheck n [ModalTope n]
saturateInv modalTopes
  -- When every tope sits at the identity modality, saturateInv adds nothing
  -- consultable: the op-inversions it would produce are tagged at 'Op' with an
  -- 'Id' lock, so 'coe Op Id = False' makes them inaccessible, and the
  -- un-inversion set 'accessibleUnderOp' is empty ('coe Id Op = False'). The
  -- op-inverted topes only become live once a modality shift puts 'Op' (or
  -- 'Sharp') into the lock, and a modal goal re-runs saturateInv on the shifted
  -- context (see the 'TypeModalT' case of 'solveRHSM'), which is where op
  -- reasoning needs them. Skipping here keeps the whole modality-free fragment
  -- (all of ordinary sHoTT) off the op-inversion machinery, which otherwise
  -- 'nfTope'-inverts every context tope on every entailment.
  | all isIdentityTope modalTopes = return modalTopes
  | otherwise = do
    -- FIXME: this is a workaround; ideally we should regenerate all topes on
    -- EVERY modality change in any layer, but that would produce too many; for
    -- now we also invert topes that were accessible before the modality shift.
    let accessible = filterAccessible modalTopes
        accessibleById = filter (\mt -> coe (tModVar mt) Id) modalTopes
    invResults <- forM (nubModalTopes (accessible <> accessibleById)) $ \mt -> do
      nf <- nfTope $ modExtractT topeT Id Op (topeInvT (tTope mt))
      return $ ModalTope (tModAccum mt) Op nf
    let accessibleUnderOp =
          filter (\mt -> coe (tModVar mt) (comp (tModAccum mt) Op)) modalTopes
    uninvResults <- forM accessibleUnderOp $ \(ModalTope acc var' phi) -> do
      nf <- nfTope $ topeUninvT (modAppT topeT Op phi)
      return $ ModalTope (comp acc Op) var' nf
    let newTopes = nubModalTopes (invResults <> uninvResults)
        fresh = filter (\t -> not (elemModalTope t modalTopes)) newTopes
    return (modalTopes <> fresh)
  where
    isIdentityTope mt = tModVar mt == Id && tModAccum mt == Id

-- | Ex falso for BOT, lifted across modalities.
--
-- A contradiction in the topes that are genuinely available at the identity
-- modality entails BOT, and BOT entails @_μ BOT@ for every modality @μ@ by the
-- absurd rule (this holds for BOT specifically; a general tope @φ@ does NOT give
-- @_μ φ@, which would need the missing unit @id ⇒ μ@). Re-asserting @_μ BOT@ at
-- each lock @μ@ where an available tope was hidden lets the contradiction survive
-- the lock: @_b BOT@ is accessible under a @_b@ lock (@coe Flat Flat@), so
-- @mod _b recBOT@ in a vacuous context is accepted.
--
-- A tope counts as available at the identity modality when its variable modality
-- coerces into @Id@: a @_b@-modal tope qualifies via the counit (@coe Flat Id@),
-- but a @_#@-modal one does not (@coe Sharp Id@ is False) — which is exactly why
-- @_# BOT@ does not leak to plain BOT.
saturateBottom :: Distinct n => [ModalTope n] -> [ModalTope n]
saturateBottom modalTopes
  | null droppedAccums = modalTopes  -- nothing hidden by a lock
  | botDerivable       = modalTopes <> fresh
  | otherwise          = modalTopes
  where
    idAccessible  = filter (\mt -> coe (tModVar mt) Id) modalTopes
    droppedAccums = nub [ tModAccum mt | mt <- idAccessible, not (isAccessible mt) ]
    saturatedId   = saturateWith elemT generateTopes (map tTope idAccessible)
    botDerivable  = topeBottomT `elemT` saturatedId
    fresh = [ mt
            | acc <- droppedAccums
            , let mt = ModalTope acc acc topeBottomT
            , not (elemModalTope mt modalTopes) ]

-- FIXME: cleanup
saturateWith :: (a -> [a] -> Bool) -> ([a] -> [a] -> [a]) -> [a] -> [a]
saturateWith elem' step zs = go (nub' zs) []
  where
    go lastNew xs
      | null new = lastNew
      | otherwise = lastNew <> go new xs'
      where
        xs' = lastNew <> xs
        new = filter (not . (`elem'` xs')) (nub' $ step lastNew xs)
    nub' []     = []
    nub' (x:xs) = x : nub' (filter (not . (`elem'` [x])) xs)

generateTopes :: Distinct n => [TermT n] -> [TermT n] -> [TermT n]
generateTopes newTopes oldTopes
  | topeBottomT `elemT` newTopes = []
  | topeEQT cube2_0T cube2_1T `elemT` newTopes = [topeBottomT]
  | topeEQT cubeI_0T cubeI_1T `elemT` newTopes = [topeBottomT]
  | length oldTopes > 100 = []    -- FIXME
  | otherwise = concat
      [  -- symmetry EQ
        [ topeEQT y x | TopeEQT _ty x y <- newTopes ]
        -- transitivity EQ (1)
      , [ topeEQT x z
        | TopeEQT _ty x y : newTopes' <- tails newTopes
        , TopeEQT _ty y' z <- newTopes' <> oldTopes
        , eqT y y' ]
        -- transitivity EQ (2)
      , [ topeEQT x z
        | TopeEQT _ty y z : newTopes' <- tails newTopes
        , TopeEQT _ty x y' <- newTopes' <> oldTopes
        , eqT y y' ]

        -- transitivity LEQ (1)
      , [ topeLEQT x z
        | TopeLEQT _ty x y : newTopes' <- tails newTopes
        , TopeLEQT _ty y' z <- newTopes' <> oldTopes
        , eqT y y' ]
        -- transitivity LEQ (2)
      , [ topeLEQT x z
        | TopeLEQT _ty y z : newTopes' <- tails newTopes
        , TopeLEQT _ty x y' <- newTopes' <> oldTopes
        , eqT y y' ]

        -- antisymmetry LEQ
      , [ topeEQT x y
        | TopeLEQT _ty x y : newTopes' <- tails newTopes
        , TopeLEQT _ty y' x' <- newTopes' <> oldTopes
        , eqT y y'
        , eqT x x' ]

        -- FIXME: special case of substitution of EQ
        -- transitivity EQ-LEQ (1)
      , [ topeLEQT x z
        | TopeEQT  _ty y z : newTopes' <- tails newTopes
        , TopeLEQT _ty x y' <- newTopes' <> oldTopes
        , eqT y y' ]

        -- transitivity EQ-LEQ (2)
      , [ topeLEQT x z
        | TopeEQT  _ty x y : newTopes' <- tails newTopes
        , TopeLEQT _ty y' z <- newTopes' <> oldTopes
        , eqT y y' ]

        -- transitivity EQ-LEQ (3)
      , [ topeLEQT x z
        | TopeLEQT  _ty y z : newTopes' <- tails newTopes
        , TopeEQT _ty x y' <- newTopes' <> oldTopes
        , eqT y y' ]

        -- transitivity EQ-LEQ (4)
      , [ topeLEQT x z
        | TopeLEQT  _ty x y : newTopes' <- tails newTopes
        , TopeEQT _ty y' z <- newTopes' <> oldTopes
        , eqT y y' ]

        -- FIXME: consequence of LEM for LEQ and antisymmetry for LEQ
      , [ topeEQT x y | TopeLEQT _ty x y@Cube2_0T{} <- newTopes ]
      , [ topeEQT x y | TopeLEQT _ty x@Cube2_1T{} y <- newTopes ]
      , [ topeEQT x y | TopeLEQT _ty x y@CubeI_0T{} <- newTopes ]
      , [ topeEQT x y | TopeLEQT _ty x@CubeI_1T{} y <- newTopes ]

        -- subtyping 2 <: II: endpoints and order of 2 lift to II
      , [ topeEQT x cubeI_0T | TopeEQT _ty x Cube2_0T{} <- newTopes ]
      , [ topeEQT cubeI_0T x | TopeEQT _ty Cube2_0T{} x <- newTopes ]
      , [ topeEQT x cubeI_1T | TopeEQT _ty x Cube2_1T{} <- newTopes ]
      , [ topeEQT cubeI_1T x | TopeEQT _ty Cube2_1T{} x <- newTopes ]
      , [ topeLEQT x cubeI_0T | TopeLEQT _ty x Cube2_0T{} <- newTopes ]
      , [ topeLEQT cubeI_0T x | TopeLEQT _ty Cube2_0T{} x <- newTopes ]
      , [ topeLEQT x cubeI_1T | TopeLEQT _ty x Cube2_1T{} <- newTopes ]
      , [ topeLEQT cubeI_1T x | TopeLEQT _ty Cube2_1T{} x <- newTopes ]

      , [ topeLEQT a c | TopeLEQT _ty (CubeSupT _ a _) c <- newTopes ]
      , [ topeLEQT b c | TopeLEQT _ty (CubeSupT _ _ b) c <- newTopes ]
      , [ topeLEQT c a | TopeLEQT _ty c (CubeInfT _ a _) <- newTopes ]
      , [ topeLEQT c b | TopeLEQT _ty c (CubeInfT _ _ b) <- newTopes ]

      -- Turn an equality into the two inequalities, but only when a side is a
      -- lattice term, so it can feed the sup/inf extraction rules above (e.g.
      -- @sup s u ≡ t@ ⊢ @sup s u ≤ t@ ⊢ @s ≤ t@). Gating on a lattice operand
      -- keeps this off the equality-heavy non-lattice fragment (extension-type
      -- faces), where the direct @solveRHS (topeEQT l r)@ guard already proves
      -- @l ≤ r@ from @l ≡ r@ and the extra topes would only bloat saturation.
      , [ leq
        | TopeEQT _ty x y <- newTopes
        , isLatticePoint x || isLatticePoint y
        , leq <- [topeLEQT x y, topeLEQT y x] ]
      ]

-- | Is this cube point a lattice term (a @sup@ or @inf@)? Used to keep the
-- lattice-specific solver work (equality-to-order generation, the
-- antisymmetry fallback for equality goals) off the equality-heavy
-- non-lattice fragment, where it would only cost time without proving
-- anything new.
isLatticePoint :: TermT n -> Bool
isLatticePoint CubeSupT{} = True
isLatticePoint CubeInfT{} = True
isLatticePoint _          = False

generateTopesForPointsM :: Distinct n => [TermT n] -> TypeCheck n [TermT n]
generateTopesForPointsM points = do
  let endpoints = [cube2_0T, cube2_1T, cubeI_0T, cubeI_1T]
      pairs = nubPairs $ concat
        [ [ (x, y)
          | x : points' <- tails (filter (\p -> not (p `elemT` endpoints)) points)
          , y <- points'
          , not (eqT x y) ]
        ]
  stars <- forM points $ \x -> do
    xType <- typeOf x
    return $ case xType of
      CubeUnitT{} -> [topeEQT x cubeUnitStarT]
      _           -> []
  topes <- forM pairs $ \(x, y) -> do
    xType <- typeOf x
    yType <- typeOf y
    return $ case (xType, yType) of
      (Cube2T{}, Cube2T{}) -> [topeOrT (topeLEQT x y) (topeLEQT y x)]
      _                    -> []
  return (concat (topes ++ stars))
  where
    nubPairs [] = []
    nubPairs (p@(x, y) : ps) =
      p : nubPairs (filter (\(x', y') -> not (eqT x x' && eqT y y')) ps)

allTopePoints :: Distinct n => TermT n -> [TermT n]
allTopePoints = nubT . foldMap subPoints . nubT . topePoints

topePoints :: TermT n -> [TermT n]
topePoints = \case
  TopeTopT{}     -> []
  TopeBottomT{}  -> []
  TopeAndT _ l r -> topePoints l <> topePoints r
  TopeOrT  _ l r -> topePoints l <> topePoints r
  TopeEQT  _ x y -> [x, y]
  TopeLEQT _ x y -> [x, y]
  _              -> []

subPoints :: TermT n -> [TermT n]
subPoints = \case
  p@(PairT _ x y) -> p : foldMap subPoints [x, y]
  p@(Var _)       -> [p]
  p -> case typeInfoOf p of
    Just TypeInfo{ infoType = CubeUnitT{} } -> [p]
    Just TypeInfo{ infoType = Cube2T{} }    -> [p]
    _                                       -> []

-- * Simplifying the left-hand side

-- | Simplify the context, including disjunctions.
simplifyLHSwithDisjunctions :: Distinct n => [ModalTope n] -> [[ModalTope n]]
simplifyLHSwithDisjunctions topes = map nubModalTopes $
  case topes of
    [] -> [[]]
    ModalTope _ _ TopeTopT{} : topes' -> simplifyLHSwithDisjunctions topes'
    ModalTope mAcc mVar TopeBottomT{} : _ -> [[ModalTope mAcc mVar topeBottomT]]
    ModalTope mAcc mVar (TopeAndT _ l r) : topes' ->
      simplifyLHSwithDisjunctions (ModalTope mAcc mVar l : ModalTope mAcc mVar r : topes')

    -- NOTE: it is inefficient to expand disjunctions immediately
    ModalTope mAcc mVar (TopeOrT _ l r) : topes' ->
      simplifyLHSwithDisjunctions (ModalTope mAcc mVar l : topes')
        <> simplifyLHSwithDisjunctions (ModalTope mAcc mVar r : topes')

    ModalTope mAcc mVar (TopeEQT _ (PairT _ x y) (PairT _ x' y')) : topes' ->
      simplifyLHSwithDisjunctions
        (ModalTope mAcc mVar (topeEQT x x') : ModalTope mAcc mVar (topeEQT y y') : topes')
    ModalTope mAcc mVar (TypeModalT _ md inTope) : topes' ->
      simplifyLHSwithDisjunctions (ModalTope mAcc (comp mVar md) inTope : topes')
    t : topes' -> map (t :) (simplifyLHSwithDisjunctions topes')

-- * Solving the right-hand side

solveRHSM :: Distinct n => [ModalTope n] -> TermT n -> TypeCheck n Bool
solveRHSM modalTopes goal =
  let topes = accessibleTopes modalTopes
  in case goal of
    _ | topeBottomT `elemT` topes -> return True
    TopeTopT{}     -> return True
    TypeModalT _ty md inTope -> do
      let shifted = applyModalityToTopes md modalTopes
          resaturated = saturateTopes shifted
      resaturatedInv <- saturateInv resaturated
      solveRHSM resaturatedInv inTope
    TopeEQT  _ty (PairT _ty1 x y) (PairT _ty2 x' y') ->
      solveRHSM modalTopes $ topeAndT (topeEQT x x') (topeEQT y y')
    TopeEQT  _ty (PairT TypeInfo{ infoType = CubeProductT _ cubeI cubeJ } x y) r ->
      solveRHSM modalTopes $ topeAndT
        (topeEQT x (firstT cubeI r))
        (topeEQT y (secondT cubeJ r))
    TopeEQT  _ty l (PairT TypeInfo{ infoType = CubeProductT _ cubeI cubeJ } x y) ->
      solveRHSM modalTopes $ topeAndT
        (topeEQT (firstT cubeI l) x)
        (topeEQT (secondT cubeJ l) y)
    TopeEQT  _ty Cube2_0T{} CubeI_0T{} -> return True
    TopeEQT  _ty CubeI_0T{} Cube2_0T{} -> return True
    TopeEQT  _ty Cube2_1T{} CubeI_1T{} -> return True
    TopeEQT  _ty CubeI_1T{} Cube2_1T{} -> return True
    TopeEQT  _ty l r -> do
      let old = or
            [ eqT l r
            , goal `elemT` topes
            , topeEQT r l `elemT` topes
            ]
      if old
        then return True
        else do
          lType <- typeOf l
          rType <- typeOf r
          case (lType, rType) of
            (CubeUnitT{}, CubeUnitT{}) -> return True
            -- Antisymmetry: prove @l ≡ r@ from @l ≤ r ∧ r ≤ l@. Only worth
            -- trying when a side is a lattice term (the law goals like
            -- @sup a b ≡ sup b a@); on the non-lattice fragment this cannot
            -- prove anything the @old@ check above did not, so we skip it to
            -- leave ordinary sHoTT solving unchanged from before the lattice.
            _ | isLatticePoint l || isLatticePoint r ->
                  solveRHSM modalTopes (topeAndT (topeLEQT l r) (topeLEQT r l))
            _ -> return False
    TopeLEQT _ty l r
      | eqT l r -> return True
      | goal `elemT` topes -> return True
      | solveRHS topes (topeEQT l r) -> return True
      | solveRHS topes (topeEQT l cube2_0T) -> return True
      | solveRHS topes (topeEQT r cube2_1T) -> return True
    TopeLEQT _ty (CubeSupT _ a b) r ->
      solveRHSM modalTopes (topeAndT (topeLEQT a r) (topeLEQT b r))
    TopeLEQT _ty l (CubeInfT _ a b) ->
      solveRHSM modalTopes (topeAndT (topeLEQT l a) (topeLEQT l b))
    TopeLEQT _ty l (CubeSupT _ a b) ->
      solveRHSM modalTopes (topeOrT (topeLEQT l a) (topeLEQT l b))
    TopeLEQT _ty (CubeInfT _ a b) r ->
      solveRHSM modalTopes (topeOrT (topeLEQT a r) (topeLEQT b r))
    TopeAndT _ l r -> solveRHSM modalTopes l >>= \case
      False -> return False
      True  -> solveRHSM modalTopes r
    _ | goal `elemT` topes -> return True
    TopeInvT{} -> do
      goal' <- nfTope goal
      case goal' of
        TopeInvT{} -> return False
        _          -> solveRHSM modalTopes goal'
    TopeUninvT{} -> do
      goal' <- nfTope goal
      case goal' of
        TopeUninvT{} -> return False
        _            -> solveRHSM modalTopes goal'
    TopeOrT  _ l r -> do
      found <- solveRHSM modalTopes l >>= \case
        True  -> return True
        False -> solveRHSM modalTopes r
      if found
        then return True
        else do
          lems <- generateTopesForPointsM (allTopePoints goal)
          let lems' = [ lem | lem@(TopeOrT _ t1 t2) <- lems, all (`notElemT` topes) [t1, t2] ]
              (accessible, hidden) = partitionAccessible modalTopes
              withTope t = hidden ++ saturateTopes (plainTope t : accessible)

          case lems' of
            TopeOrT _ t1 t2 : _ ->
              solveRHSM (withTope t1) goal >>= \case
                False -> return False
                True  -> solveRHSM (withTope t2) goal
            _ -> return False
    _ -> return False

solveRHS :: Distinct n => [TermT n] -> TermT n -> Bool
solveRHS topes tope =
  case tope of
    _ | topeBottomT `elemT` topes -> True
    TopeTopT{}     -> True
    TopeEQT  _ty (PairT _ty1 x y) (PairT _ty2 x' y')
      | solveRHS topes (topeEQT x x') && solveRHS topes (topeEQT y y') -> True
    TopeEQT  _ty (PairT TypeInfo{ infoType = CubeProductT _ cubeI cubeJ } x y) r
      | solveRHS topes (topeEQT x (firstT cubeI r))
      , solveRHS topes (topeEQT y (secondT cubeJ r)) -> True
    TopeEQT  _ty l (PairT TypeInfo{ infoType = CubeProductT _ cubeI cubeJ } x y)
      | solveRHS topes (topeEQT (firstT cubeI l) x)
      , solveRHS topes (topeEQT (secondT cubeJ l) y) -> True
    TopeEQT  _ty Cube2_0T{} CubeI_0T{} -> True
    TopeEQT  _ty CubeI_0T{} Cube2_0T{} -> True
    TopeEQT  _ty Cube2_1T{} CubeI_1T{} -> True
    TopeEQT  _ty CubeI_1T{} Cube2_1T{} -> True
    TopeEQT  _ty l r -> or
      [ eqT l r
      , tope `elemT` topes
      , topeEQT r l `elemT` topes
      ]
    TopeLEQT _ty l r
      | eqT l r -> True
      | solveRHS topes (topeEQT l r) -> True
      | solveRHS topes (topeEQT l cube2_0T) -> True
      | solveRHS topes (topeEQT r cube2_1T) -> True
    TopeAndT _ l r -> solveRHS topes l && solveRHS topes r
    TopeOrT  _ l r -> solveRHS topes l || solveRHS topes r
    _ -> tope `elemT` topes

-- | Accumulate a modality over a list of topes.
applyModalityToTopes :: TModality -> [ModalTope n] -> [ModalTope n]
applyModalityToTopes md = map (\mt -> mt { tModAccum = comp (tModAccum mt) md })

partitionAccessible :: [ModalTope n] -> ([ModalTope n], [ModalTope n])
partitionAccessible topes = (filter isAccessible topes, filter (not . isAccessible) topes)

-- * The checks the rest of the checker calls

checkTope :: Distinct n => TermT n -> TypeCheck n Bool
checkTope tope = do
  topes <- asks availableTopes
  performing (ActionContextEntails topes tope) $ do
    tope' <- nfTope tope
    entailContextM tope'

checkTopeEntails :: Distinct n => TermT n -> TypeCheck n Bool
checkTopeEntails tope = do
  topes <- asks availableTopes
  performing (ActionContextEntailedBy topes tope) $ do
    contextTopes <- asks availableTopesNF
    restrictionTope <- nfTope tope
    let contextTopesRHS = foldr topeAndT topeTopT contextTopes
    [plainTope restrictionTope] `entailM` contextTopesRHS

checkEntails :: Distinct n => TermT n -> TermT n -> TypeCheck n Bool
checkEntails l r = do  -- FIXME: add action
  l' <- nfTope l
  r' <- nfTope r
  [plainTope l'] `entailM` r'

contextEntails :: Distinct n => TermT n -> TypeCheck n ()
contextEntails tope = do
  topes <- asks availableTopes
  performing (ActionContextEntails topes tope) $ do
    topeIsEntailed <- checkTope tope
    topes' <- asks availableTopesNF
    -- When a hole is used in a cube/tope position (as the argument of a
    -- shape-restricted function, say), the tope being checked mentions the hole
    -- and cannot be decided. Treat it as satisfied (defer) rather than failing.
    unless (topeIsEntailed || containsHole tope) $
      issueTypeError $ TypeErrorTopeNotSatisfied topes' tope

-- | Is the local tope context contradictory (does it entail ⊥)?
contextEntailsBottom :: Distinct n => TypeCheck n Bool
contextEntailsBottom = asks ctxTopesEntailBottom >>= \case
  Just entails -> pure entails
  Nothing      -> asks ctxTopesNF >>= (`entailM` topeBottomT)

topesEquiv :: Distinct n => TermT n -> TermT n -> TypeCheck n Bool
topesEquiv expected actual = performing (ActionUnifyTerms expected actual) $ do
  expected' <- nfT expected
  actual' <- nfT actual
  (&&)
    <$> [plainTope expected'] `entailM` actual'
    <*> [plainTope actual'] `entailM` expected'

-- | Check that the local tope context is included in (entails) the union of the
-- given topes. This is the COVERAGE obligation of @recOR@: every point of the
-- context must be covered by some branch guard.
--
-- Only coverage is required, not equivalence: branch guards may overhang the
-- context (when splitting with an already-defined shape, say), so we do not
-- require @OR(guards) |- context@.
contextEntailsUnion :: Distinct n => [TermT n] -> TypeCheck n ()
contextEntailsUnion topes = do
  ctxTopes' <- asks availableTopes
  performing (ActionContextEntailsUnion ctxTopes' topes) $ do
    contextTopes <- asks ctxTopesNF
    topesNF <- mapM nfTope topes
    let unionRHS = foldr topeOrT topeBottomT topesNF
    entailContextM unionRHS >>= \case
      -- a guard mentioning an (unfilled) hole can't be decided; defer coverage
      False | not (any containsHole topesNF) ->
        issueTypeError $ TypeErrorTopeNotSatisfied (accessibleTopes contextTopes) unionRHS
      _ -> return ()

-- | Diagnose a @recOR@ branch guard or a restriction face against the local tope
-- context. There are three cases, by how the tope relates to the context:
--
--   * DISJOINT — the tope and a consistent context have empty overlap (their
--     conjunction is ⊥). The face or branch is then vacuous everywhere, so this is
--     a hard error.
--   * OVERHANG — the tope is not entailed by the context but still overlaps it.
--     This is allowed and often intentional (splitting or restricting with an
--     already-defined shape, whose faces live on the whole cube rather than being
--     relativised to the context), so we only emit a non-fatal hint.
--   * CONTAINED — the tope entails the context: nothing to report.
checkTopeAgainstContext :: Distinct n => String -> TermT n -> TypeCheck n ()
checkTopeAgainstContext what tope = do
  -- a contradictory context is handled elsewhere (recBOT)
  ctxEntailsBottom <- contextEntailsBottom
  unless ctxEntailsBottom $ do
    contextTopes <- asks ctxTopesNF
    let topes = filter (not . eqT topeTopT) (accessibleTopes contextTopes)
    disjoint <- (plainTope tope : contextTopes) `entailM` topeBottomT
    -- a face or guard mentioning an (unfilled) hole can't be decided; defer
    if disjoint && not (containsHole tope)
      then issueTypeError (TypeErrorTopeContextDisjoint tope topes)
      else do
        -- The hint below is opt-in (#set-option "warn-overhang"): deciding
        -- whether the tope overhangs costs a solver entailment per face and
        -- guard, and overhang is legitimate.
        warnOverhang <- asks ctxWarnOverhang
        when warnOverhang $ do
          entailed <- checkTopeEntails tope   -- tope |- AND(accessible context)
          unless entailed $ do
            naming <- asks namingOfContext
            traceTypeCheck Normal
              (intercalate "\n" $
                [ "Warning: " <> what <> " overhangs the local tope context"
                , "  " <> ppTerm naming (untyped tope)
                , "is not entailed by the local context (normalised)"
                ] <> map (("  " <>) . ppTerm naming . untyped) topes)
              (return ())

-- * Restrictions and η

stripTypeRestrictions :: TermT n -> TermT n
stripTypeRestrictions (TypeRestrictedT _ty ty _restriction) = stripTypeRestrictions ty
stripTypeRestrictions t = t

-- | The term a restriction face pins down, when one of the faces holds.
tryRestriction :: Distinct n => TermT n -> TypeCheck n (Maybe (TermT n))
tryRestriction = \case
  TypeRestrictedT _ _ rs -> go rs
  _ -> pure Nothing
  where
    go [] = pure Nothing
    go ((tope, term') : rs') = checkTope tope >>= \case
      True  -> pure (Just term')
      False -> go rs'

-- | Perform at most one η-expansion at the top level, to assist unification.
etaMatch
  :: Distinct n
  => Maybe (TermT n) -> TermT n -> TermT n -> TypeCheck n (TermT n, TermT n)
-- FIXME: double check the next 3 rules
etaMatch _mterm expected@TypeRestrictedT{} actual@TypeRestrictedT{} = pure (expected, actual)
etaMatch  mterm expected (TypeRestrictedT _ty ty _rs) = etaMatch mterm expected ty
etaMatch (Just term) expected@TypeRestrictedT{} actual =
  etaMatch (Just term) expected (typeRestrictedT actual [(topeTopT, term)])
-- Subtyping on the interval.
etaMatch _mterm CubeIT{} Cube2T{} = pure (cubeIT, cubeIT)
etaMatch _mterm expected@LambdaT{} actual@LambdaT{} = pure (expected, actual)
etaMatch _mterm expected@PairT{}   actual@PairT{}   = pure (expected, actual)
etaMatch _mterm expected@LambdaT{} actual = do
  actual' <- etaExpand actual
  pure (expected, actual')
etaMatch _mterm expected actual@LambdaT{} = do
  expected' <- etaExpand expected
  pure (expected', actual)
etaMatch _mterm expected@PairT{} actual = do
  actual' <- etaExpand actual
  pure (expected, actual')
etaMatch _mterm expected actual@PairT{} = do
  expected' <- etaExpand expected
  pure (expected', actual)
etaMatch _mterm expected actual = pure (expected, actual)

etaExpand :: Distinct n => TermT n -> TypeCheck n (TermT n)
etaExpand term@LambdaT{} = pure term
etaExpand term@PairT{} = pure term
etaExpand term = do
  ty <- typeOf term
  case stripTypeRestrictions ty of
    TypeFunT _ty orig md param mtope ret -> do
      scope <- asks ctxScope
      pure $ withFreshIn scope $ \binder ->
        let z = Var (Foil.nameOf binder)
            body = appT (openWith (Foil.extendScope binder scope) (Foil.nameOf binder) ret)
                        (Foil.sink term) z
            mtope' = fmap (\t -> ScopedAST binder (openWith (Foil.extendScope binder scope) (Foil.nameOf binder) t)) mtope
         in lambdaT ty orig
              (Just (LambdaParam md param mtope'))
              (ScopedAST binder body)

    TypeSigmaT _ty _orig _md a b -> do
      let firstTerm = firstT a term
      bInstantiated <- instantiate b firstTerm
      pure $ pairT ty firstTerm (secondT bInstantiated term)

    CubeProductT _ty a b -> pure $
      pairT ty (firstT a term) (secondT b term)

    _ -> pure term

-- * Layers

inCubeLayer :: Distinct n => TermT n -> TypeCheck n Bool
inCubeLayer = \case
  RecBottomT{}    -> pure False
  UniverseT{}     -> pure False

  UniverseCubeT{} -> pure True
  CubeProductT{}  -> pure True
  CubeUnitT{}     -> pure True
  CubeUnitStarT{} -> pure True
  Cube2T{}        -> pure True
  Cube2_0T{}      -> pure True
  Cube2_1T{}      -> pure True

  t               -> typeOf t >>= inCubeLayer

inTopeLayer :: Distinct n => TermT n -> TypeCheck n Bool
inTopeLayer = \case
  RecBottomT{} -> pure False
  UniverseT{} -> pure False

  UniverseCubeT{} -> pure True
  UniverseTopeT{} -> pure True

  CubeProductT{} -> pure True
  CubeUnitT{} -> pure True
  CubeUnitStarT{} -> pure True
  Cube2T{} -> pure True
  Cube2_0T{} -> pure True
  Cube2_1T{} -> pure True

  TopeTopT{} -> pure True
  TopeBottomT{} -> pure True
  TopeAndT{} -> pure True
  TopeOrT{} -> pure True
  TopeEQT{} -> pure True
  TopeLEQT{} -> pure True

  TypeFunT _ty orig md param _mtope ret ->
    inScope orig md param ret inTopeLayer

  t -> typeOfUncomputed t >>= inTopeLayer

-- * Weak head normal form

-- | Memoise a term's WHNF on its top node without reducing the term itself.
--
-- The returned term has the same (unreduced) structure, so free-variable and
-- @uses@ detection see exactly what the user wrote, while a later 'whnfT' is O(1)
-- via the cached form. Used when storing a definition's elaborated type and value,
-- where an in-place reduction could otherwise discard or expose a variable
-- occurrence.
memoizeWHNF :: Distinct n => TermT n -> TypeCheck n (TermT n)
memoizeWHNF t@(Var _) = pure t
memoizeWHNF t@(Node (AnnSig info sig)) = do
  w <- whnfT t
  pure (Node (AnnSig info { infoWHNF = Just w } sig))

whnfT :: Distinct n => TermT n -> TypeCheck n (TermT n)
-- A memoised weak head normal form is answered before entering 'performing',
-- which would push an action and rebuild the context just to look a value up. The
-- caches are hit constantly (every 'typeOf' consults one), and the bookkeeping
-- costs more than the answer.
whnfT t | Just info <- typeInfoOf t, Just t' <- infoWHNF info = pure t'
whnfT tt = performing (ActionWHNF tt) $ case tt of
  -- universe constants
  UniverseT{} -> pure tt
  UniverseCubeT{} -> pure tt
  UniverseTopeT{} -> pure tt

  -- cube layer (except vars, pairs, and applications)
  CubeProductT{} -> nfTope tt
  CubeUnitT{} -> pure tt
  CubeUnitStarT{} -> pure tt
  Cube2T{} -> pure tt
  Cube2_0T{} -> pure tt
  Cube2_1T{} -> pure tt
  CubeIT{} -> pure tt
  CubeI_0T{} -> pure tt
  CubeI_1T{} -> pure tt
  CubeFlipT{} -> nfTope tt
  CubeUnflipT{} -> nfTope tt
  CubeSupT{} -> nfTope tt
  CubeInfT{} -> nfTope tt

  -- tope layer (except vars, pairs of points, and applications)
  TopeTopT{} -> pure tt
  TopeBottomT{} -> pure tt
  TopeAndT{} -> nfTope tt
  TopeOrT{} -> nfTope tt
  TopeEQT{} -> nfTope tt
  TopeLEQT{} -> nfTope tt
  TopeInvT{} -> nfTope tt
  TopeUninvT{} -> nfTope tt

  -- type layer terms that should not be evaluated further
  LambdaT{} -> pure tt
  PairT{} -> pure tt
  ReflT{} -> pure tt
  TypeFunT{} -> pure tt
  TypeSigmaT{} -> pure tt
  TypeIdT{} -> pure tt
  TypeModalT{} -> pure tt
  RecBottomT{} -> pure tt
  TypeUnitT{} -> pure tt
  UnitT{} -> pure tt

  -- type ascriptions are ignored, since we already have a typechecked term
  TypeAscT _ty term _ty' -> whnfT term

  -- check if we have a cube or a tope term (if so, compute NF)
  _ -> typeOf tt >>= \case
    UniverseCubeT{} -> nfTope tt
    UniverseTopeT{} -> nfTope tt

    TypeUnitT{} -> pure unitT -- compute an expression of Unit type to unit
    -- FIXME: next line is ad hoc, should be improved!
    TypeRestrictedT _info TypeUnitT{} _rs -> pure unitT

    -- check if we have a cube point term (if so, compute NF)
    typeOf_tt -> typeOf typeOf_tt >>= \case
      UniverseCubeT{} -> nfTope tt

      -- now we are in the type layer
      _ -> fmap termIsWHNF $ do
        tryRestriction typeOf_tt >>= \case
          Just tt' -> whnfT tt'
          Nothing -> case tt of
            -- a hole is opaque: it never reduces, it is already a normal form
            HoleT{} -> pure tt
            t@(Var x) ->
              valueOfVar x >>= \case
                Nothing   -> pure t
                Just term -> whnfT term

            AppT{} -> do
              scope <- asks ctxScope
              uncurry (applySpine scope) (collectAppSpine tt)

            LetT _ty _orig _mparam val body ->
              instantiate body val >>= whnfT
            LetModT ty orig app inn mparam val body ->
              (enterModality app $ whnfT val) >>= \case
                ModAppT _ md t | md == inn -> do
                  val' <- enterModality md $ whnfT t
                  instantiate body val' >>= whnfT
                b' | isRA inn -> do
                  bty <- typeOf b' >>= \case
                    TypeModalT _ _ t -> pure t
                    _ -> panicImpossible "not modal in letmod"
                  instantiate body (modExtractT bty app inn b') >>= whnfT
                _ -> pure (LetModT ty orig app inn mparam val body)
            FirstT ty t ->
              whnfT t >>= \case
                PairT _ l _r -> whnfT l
                t'           -> pure (FirstT ty t')

            SecondT ty t ->
              whnfT t >>= \case
                PairT _ _l r -> whnfT r
                t'           -> pure (SecondT ty t')
            ModAppT ty md b ->
              (enterModality md $ whnfT b) >>= \case
                ModExtractT _ app inn t | inn == md -> enterModality (comp md app) $ whnfT t
                b' -> pure $ ModAppT ty md b'
            ModExtractT ty app inn b ->
              (enterModality app $ whnfT b) >>= \case
                ModAppT _ md t | inn == md -> enterModality inn $ whnfT t
                b' -> pure (ModExtractT ty app inn b')
            IdJT ty tA a tC d x p ->
              whnfT p >>= \case
                ReflT{} -> whnfT d
                p'      -> pure (IdJT ty tA a tC d x p')

            RecOrT _ty rs -> do
              firstMatching rs >>= \case
                Just tt' -> whnfT tt'
                Nothing
                  | [tt'] <- nubT (map snd rs) -> whnfT tt'
                  | otherwise -> pure tt

            TypeRestrictedT ty type_ rs -> do
              rs' <- traverse (\(tope, term) -> (,) <$> nfT tope <*> pure term) rs
              case filter (not . eqT topeBottomT . fst) rs' of
                []   -> whnfT type_  -- get rid of restrictions at BOT
                rs'' -> TypeRestrictedT ty <$> whnfT type_ <*> pure rs''

-- | The branch of a @recOR@ (or the face of a restriction) whose guard holds.
firstMatching :: Distinct n => [(TermT n, TermT n)] -> TypeCheck n (Maybe (TermT n))
firstMatching [] = pure Nothing
firstMatching ((tope, t) : rest) = checkTope tope >>= \case
  True  -> pure (Just t)
  False -> firstMatching rest

-- * Application, reducing a whole spine at once
--
-- A curried application @f x y z@ is a left-nested tower of 'AppT'. Reducing it
-- one argument at a time rebuilds the intermediate lambdas — @f x@ produces
-- @\\ y z -> …@ only for the next argument to tear it apart — and each rebuild is
-- a full 'substituteT' traversal (~70% of beta reductions on sHoTT are such
-- spines). Instead, collect the spine, then peel the head's syntactic lambda
-- chain into a /single/ substitution: @\\ a b c -> body@ applied to @x y z@ maps
-- @{a↦x, b↦y, c↦z}@ and substitutes into @body@ once.
--
-- Sound because 'whnfT' of a lambda is the identity — a lambda, its binder
-- shape-restricted or not, is already in weak head normal form — so the
-- intermediate lambdas this skips building would have been returned unchanged,
-- and substitution composes. Beta reduction ignores the binder's domain (the
-- shape restriction is a typing obligation, not enforced during reduction); the
-- tope and modality side-conditions fire only for a /neutral/ function of
-- shape-restricted function type, which is never a lambda. The moment the body
-- is not a syntactic lambda the substitution is applied and control returns to
-- 'whnfT' via 'applySpine'; a neutral head goes to 'applyWhnfFun', unchanged.

-- | The head of an application spine and its arguments, in application order,
-- each paired with the type annotation of its 'AppT' node (needed to rebuild a
-- neutral application).
collectAppSpine :: TermT n -> (TermT n, [(TypeInfo (TermT n), TermT n)])
collectAppSpine = go []
  where
    go acc (AppT ty f x) = go ((ty, x) : acc) f
    go acc h             = (h, acc)

-- | Apply a function term to a spine of arguments, reducing.
applySpine
  :: Distinct n
  => Foil.Scope n -> TermT n -> [(TypeInfo (TermT n), TermT n)] -> TypeCheck n (TermT n)
applySpine _ h [] = whnfT h
applySpine scope h pairs = whnfT h >>= \h' -> case h' of
  LambdaT _ _ _ (ScopedAST binder body) | (_, x) : rest <- pairs ->
    peelLambdas scope (Foil.addSubst Foil.identitySubst binder x) body rest
  _ -> do
    -- The head may itself be a (neutral) application: a definition whose
    -- value is an under-applied eliminator, say. The ι-rule needs the full
    -- spine, so the head's own arguments are collected back in.
    let (h'', headPairs) = collectAppSpine h'
    tryDataElimStep h'' (headPairs <> pairs) >>= \case
      Just stepped -> whnfT stepped
      Nothing      -> applyNeutral scope h' pairs

-- | Try to fire a @#data@ ι-rule on an application spine: the head is a
-- generated eliminator, the scrutinee argument is headed by a fully applied
-- constructor of the same datatype. Returns the method applied to the
-- constructor's fields (and any leftover spine arguments), unreduced.
--
-- A non-'Var' head answers 'Nothing' immediately, so the common neutral
-- spine pays one pattern match; a 'Var' head pays one 'lookupVarInfo'.
tryDataElimStep
  :: Distinct n
  => TermT n                          -- ^ the head, in WHNF
  -> [(TypeInfo (TermT n), TermT n)]  -- ^ the collected spine arguments
  -> TypeCheck n (Maybe (TermT n))
tryDataElimStep (Var v) pairs = asks (varDataRole . lookupVarInfo v) >>= \case
  Just (DataRole dataType numParams (DataElimKind numMethods _elimKind))
    | (_, (_, scrut) : after) <- splitAt (numParams + 1 + numMethods) pairs ->
        whnfT scrut >>= \scrut' -> case collectAppSpine scrut' of
          (Var c, cargs) -> asks (varDataRole . lookupVarInfo c) >>= \case
            Just (DataRole dataType' conNumParams (DataConKind conIndex conNumFields))
              | Foil.nameId dataType' == Foil.nameId dataType
              , length cargs == conNumParams + conNumFields -> do
                  let method = snd (pairs !! (numParams + 1 + conIndex))
                      args = map snd (drop conNumParams cargs) <> map snd after
                  Just <$> applyTyped method args
            _ -> pure Nothing
          _ -> pure Nothing
  _ -> pure Nothing
tryDataElimStep _ _ = pure Nothing

-- | Apply a term to arguments left to right, annotating each application
-- node with its actual type. The spine machinery reuses the annotations of
-- existing nodes, which a freshly built ι-redex does not have.
applyTyped :: Distinct n => TermT n -> [TermT n] -> TypeCheck n (TermT n)
applyTyped f [] = pure f
applyTyped f (x : xs) = typeOf f >>= \case
  TypeFunT _info _orig _md _param _mtope ret -> do
    retTy <- instantiate ret x
    applyTyped (appT retTy f x) xs
  _ -> panicImpossible "ι-rule applies a method beyond its arity"

-- | Peel the head's syntactic lambda chain into one substitution, then reduce.
-- @subst@ maps the binders consumed so far to their arguments; @body@ is the
-- current lambda's body, at the scope those binders extended into.
peelLambdas
  :: forall i n. Distinct n
  => Foil.Scope n -> Foil.Substitution TermT i n -> TermT i
  -> [(TypeInfo (TermT n), TermT n)] -> TypeCheck n (TermT n)
peelLambdas scope subst body pairs = case pairs of
  [] -> whnfT (substituteT scope subst body)
  (_, x) : rest -> case body of
    LambdaT _ _ _ (ScopedAST binder body') ->
      peelLambdas scope (Foil.addSubst subst binder x) body' rest
    _ -> applySpine scope (substituteT scope subst body) pairs

-- | Apply a non-lambda (already WHNF) function to a spine, one argument at a
-- time: this is the type-directed part of application, unchanged from before.
applyNeutral
  :: Distinct n
  => Foil.Scope n -> TermT n -> [(TypeInfo (TermT n), TermT n)] -> TypeCheck n (TermT n)
applyNeutral _ h [] = pure h
applyNeutral scope h ((ty, x) : rest) = do
  r <- applyWhnfFun ty h x
  if null rest then pure r else applySpine scope r rest

-- | Apply a non-lambda function @f'@ (already WHNF) to one argument @x@. A
-- shape-restricted function contributes a tope side-condition; a function whose
-- return type is restricted refines the application's type; everything else is a
-- neutral application. Extracted verbatim from the old single-argument @AppT@ case.
applyWhnfFun :: Distinct n => TypeInfo (TermT n) -> TermT n -> TermT n -> TypeCheck n (TermT n)
applyWhnfFun ty f' x = typeOf f' >>= \case
  TypeFunT _ty _orig md _param (Just tope) (ScopedAST _ UniverseTopeT{}) -> do
    x' <- enterModality md $ nfT x
    sideCondition <- instantiate tope x' >>= nfT
    pure (topeAndT (AppT ty f' x') sideCondition)
  -- FIXME: this seems to be a hack, and will not work in all
  -- situations! FIXME: for now, it seems to add ~2x slowdown
  TypeFunT info _orig md _param _mtope ret@(ScopedAST _ TypeRestrictedT{})
    | TypeRestrictedT{} <- infoType info -> pure (AppT ty f' x)
    | otherwise -> do
        x' <- enterModality md $ whnfT x
        ret' <- instantiate ret x'
        tryRestriction ret' >>= \case -- FIXME: too many unnecessary checks?
          Nothing  -> pure (AppT ty { infoType = ret' } f' x')
          Just tt' -> whnfT tt'
  _ -> pure (AppT ty f' x)

-- * Normal form of the tope layer

nfSupT :: TermT n -> TermT n -> TermT n -> TermT n
nfSupT ty l r = case (l, r) of
  (Cube2_0T{}, _) -> r
  (_, Cube2_0T{}) -> l
  (Cube2_1T{}, _) -> l
  (_, Cube2_1T{}) -> r
  (CubeI_0T{}, _) -> r
  (_, CubeI_0T{}) -> l
  (CubeI_1T{}, _) -> l
  (_, CubeI_1T{}) -> r
  _               -> cubeSupT ty l r

nfInfT :: TermT n -> TermT n -> TermT n -> TermT n
nfInfT ty l r = case (l, r) of
  (Cube2_0T{}, _) -> l
  (_, Cube2_0T{}) -> r
  (Cube2_1T{}, _) -> r
  (_, Cube2_1T{}) -> l
  (CubeI_0T{}, _) -> l
  (_, CubeI_0T{}) -> r
  (CubeI_1T{}, _) -> r
  (_, CubeI_1T{}) -> l
  (CubeSupT _ a b, _) -> nfSupT ty (nfInfT ty a r) (nfInfT ty b r)
  (_, CubeSupT _ a b) -> nfSupT ty (nfInfT ty l a) (nfInfT ty l b)
  _                   -> cubeInfT ty l r

nfTope :: Distinct n => TermT n -> TypeCheck n (TermT n)
nfTope tt = performing (ActionNF tt) $ fmap termIsNF $ case tt of
  HoleT{} -> pure tt
  Var x ->
    valueOfVar x >>= \case
      Nothing   -> return tt
      Just term -> nfTope term

  -- see if a normal form is already available
  _ | Just info <- typeInfoOf tt, Just tt' <- infoNF info -> pure tt'

  -- universe constants
  UniverseT{} -> pure tt
  UniverseCubeT{} -> pure tt
  UniverseTopeT{} -> pure tt

  -- cube layer constants
  CubeUnitT{} -> pure tt
  CubeUnitStarT{} -> pure tt
  Cube2T{} -> pure tt
  Cube2_0T{} -> pure tt
  Cube2_1T{} -> pure tt
  CubeIT{} -> pure tt
  CubeI_0T{} -> pure tt
  CubeI_1T{} -> pure tt

  -- type layer constants
  TypeUnitT{} -> pure tt
  UnitT{} -> pure tt

  -- cube layer with computation
  CubeProductT _ty l r -> cubeProductT <$> nfTope l <*> nfTope r

  CubeFlipT ty t ->
    nfTope t >>= \case
      CubeUnflipT _ t' -> pure t'
      Cube2_0T{}       -> pure (modAppT (typeModalT cubeT Op cube2T) Op cube2_1T)
      Cube2_1T{}       -> pure (modAppT (typeModalT cubeT Op cube2T) Op cube2_0T)
      CubeI_0T{}       -> pure (modAppT (typeModalT cubeT Op cubeIT) Op cubeI_1T)
      CubeI_1T{}       -> pure (modAppT (typeModalT cubeT Op cubeIT) Op cubeI_0T)
      t'               -> pure (CubeFlipT ty t')

  CubeUnflipT ty t ->
    nfTope t >>= \case
      CubeFlipT _ t'          -> pure t'
      ModAppT _ Op Cube2_0T{} -> pure cube2_1T
      ModAppT _ Op Cube2_1T{} -> pure cube2_0T
      ModAppT _ Op CubeI_0T{} -> pure cubeI_1T
      ModAppT _ Op CubeI_1T{} -> pure cubeI_0T
      t'                      -> pure (CubeUnflipT ty t')

  CubeSupT ty l r -> do
    l' <- nfTope l
    r' <- nfTope r
    pure (nfSupT (infoType ty) l' r')

  CubeInfT ty l r -> do
    l' <- nfTope l
    r' <- nfTope r
    pure (nfInfT (infoType ty) l' r')

  -- tope layer constants
  TopeTopT{} -> pure tt
  TopeBottomT{} -> pure tt

  -- tope layer with computation
  TopeAndT ty l r ->
    nfTope l >>= \case
      TopeBottomT{} -> pure topeBottomT
      l' -> nfTope r >>= \case
        TopeBottomT{} -> pure topeBottomT
        r'            -> pure (TopeAndT ty l' r')

  TopeOrT  ty l r -> do
    l' <- nfTope l
    r' <- nfTope r
    case (l', r') of
      (TopeBottomT{}, _) -> pure r'
      (_, TopeBottomT{}) -> pure l'
      _                  -> pure (TopeOrT ty l' r')

  TopeEQT  ty l r -> TopeEQT  ty <$> nfTope l <*> nfTope r
  TopeLEQT ty l r -> TopeLEQT ty <$> nfTope l <*> nfTope r

  TopeInvT ty t ->
    -- Match And/Or on the *unnormalised* input: nfTope of a shape-restricted App
    -- produces a TopeAnd via shape-side-condition propagation, and distributing
    -- inv over that synthetic conjunction loops forever, because the recursive
    -- topeInvT renormalises the same App back into a TopeAnd.
    case t of
      TopeTopT _ -> pure $ modAppT topeT Op topeTopT
      TopeBottomT _ -> pure $ modAppT topeT Op topeBottomT
      TopeLEQT _ x y -> invOf topeLEQT x y
      TopeEQT _ x y -> invOf topeEQT x y
      TopeAndT _ phi psi -> nfTope $
        modAppT (typeModalT universeT Op topeT) Op
          (topeAndT
            (modExtractT topeT Id Op (topeInvT phi))
            (modExtractT topeT Id Op (topeInvT psi)))
      TopeOrT _ phi psi -> nfTope $
        modAppT (typeModalT universeT Op topeT) Op
          (topeOrT
            (modExtractT topeT Id Op (topeInvT phi))
            (modExtractT topeT Id Op (topeInvT psi)))
      _ ->
        nfTope t >>= \case
          TopeTopT _       -> pure topeTopT
          TopeBottomT _    -> pure topeBottomT
          TopeUninvT _ phi -> pure phi
          TopeLEQT _ x y   -> invOf topeLEQT x y
          TopeEQT _ x y    -> invOf topeEQT x y
          t'               -> pure (TopeInvT ty t')
    where
      invOf mk x y = do
        xTy <- typeOf x
        yTy <- typeOf y
        nfTope $
          modAppT (typeModalT universeT Op topeT) Op
            (mk (modExtractT topeT Id Op (cubeFlipT xTy y))
                (modExtractT topeT Id Op (cubeFlipT yTy x)))

  TopeUninvT ty t ->
    case t of
      ModAppT _ Op inner -> case inner of
        TopeTopT _ -> pure topeTopT
        TopeBottomT _ -> pure topeBottomT
        TopeAndT _ phi psi ->
          nfTope (topeAndT (topeUninvT phi) (topeUninvT psi))
        TopeOrT _ phi psi ->
          nfTope (topeOrT (topeUninvT phi) (topeUninvT psi))
        _ ->
          nfTope t >>= \case
            TopeTopT _ -> pure topeTopT
            TopeBottomT _ -> pure topeBottomT
            TopeInvT _ phi -> pure phi
            ModAppT _ Op inner'' -> case inner'' of
              TopeLEQT _ x y -> uninvOf topeLEQT x y
              TopeEQT _ x y -> uninvOf topeEQT x y
              inner' ->
                pure $ TopeUninvT ty
                  (modAppT (typeModalT universeT Op topeT) Op inner')
            t' -> pure (TopeUninvT ty t')
      _ ->
        nfTope t >>= \case
          TopeInvT _ phi -> pure phi
          t'@(ModAppT _ Op _) -> nfTope (TopeUninvT ty t')
          t' -> pure (TopeUninvT ty t')
    where
      uninvOf mk x y = do
        xTy <- typeOf x
        yTy <- typeOf y
        nfTope $
          mk (cubeUnflipT xTy (modAppT (typeModalT cubeT Op xTy) Op y))
             (cubeUnflipT yTy (modAppT (typeModalT cubeT Op yTy) Op x))

  -- type ascriptions are ignored, since we already have a typechecked term
  TypeAscT _ty term _ty' -> nfTope term

  PairT ty l r -> PairT ty <$> nfTope l <*> nfTope r

  AppT ty f x ->
    nfTope f >>= \case
      LambdaT _ty _orig _arg body ->
        instantiate body x >>= nfTope
      f' -> typeOfUncomputed f' >>= \case
        TypeFunT _ty _orig md _param (Just tope) (ScopedAST _ UniverseTopeT{}) -> do
          x' <- enterModality md $ nfTope x
          sideCondition <- instantiate tope x' >>= nfTope
          pure (topeAndT (AppT ty f' x') sideCondition)
        _ -> AppT ty f' <$> nfTope x

  FirstT ty t ->
    nfTope t >>= \case
      PairT _ty x _y -> pure x
      t'             -> pure (FirstT ty t')

  SecondT ty t ->
    nfTope t >>= \case
      PairT _ty _x y -> pure y
      t'             -> pure (SecondT ty t')

  LambdaT ty orig _mparam body
    | TypeFunT _ty _origF md param mtope _ret <- infoType ty -> do
        -- NOTE: the domain @param@ is left unnormalised: in the tope layer it may
        -- be a shape (a function type into TOPE), which nfTope cannot normalise.
        body' <- underScope orig md param Nothing body nfTope
        pure (LambdaT ty orig (Just (LambdaParam md param mtope)) body')
  LambdaT{} -> panicImpossible "lambda with a non-function type in the tope layer"

  ModAppT ty md b ->
    (enterModality md $ nfTope b) >>= \case
      ModExtractT _ _ inn t | inn == md -> pure t
      b' -> pure $ ModAppT ty md b'
  ModExtractT ty app inn b ->
    (enterModality app $ nfTope b) >>= \case
      ModAppT _ md t | inn == md -> pure t
      b' -> pure $ ModExtractT ty app inn b'
  LetModT ty orig app inn mparam val body ->
    (enterModality app $ nfTope val) >>= \case
      ModAppT _ md t | md == inn ->
        instantiate body t >>= nfTope
      b' | isRA inn -> do
        bty <- typeOf b' >>= \case
          TypeModalT _ _ t -> pure t
          _ -> panicImpossible "not modal in letmod"
        instantiate body (modExtractT bty app inn b') >>= nfTope
      b' -> do
        bty <- typeOf b' >>= \case
          TypeModalT _ _ t -> pure t
          _ -> panicImpossible "not modal in letmod"
        val' <- enterModality app $ nfTope b'
        body' <- underScope orig (comp app inn) bty Nothing body nfTope
        pure (LetModT ty orig app inn mparam val' body')

  TypeModalT ty md inner -> TypeModalT ty md <$> (enterModality md $ nfTope inner)
  LetT _ty _orig _mparam val body -> instantiate body val >>= nfTope
  TypeFunT{} -> panicImpossible "exposed function type in the tope layer"
  TypeSigmaT{} -> panicImpossible "dependent sum type in the tope layer"
  TypeIdT{} -> panicImpossible "identity type in the tope layer"
  ReflT{} -> panicImpossible "refl in the tope layer"
  IdJT{} -> panicImpossible "idJ eliminator in the tope layer"
  TypeRestrictedT{} -> panicImpossible "extension types in the tope layer"

  -- A recOR/recBOT is a term-level eliminator, never a tope. It should have been
  -- rejected before reaching here (see the RecOr case of 'typecheck'); as a safety
  -- net for any other path, report a type error rather than panicking.
  RecOrT{} -> issueTypeError $ TypeErrorOther "a recOR cannot appear in the tope layer"
  RecBottomT{} -> issueTypeError $ TypeErrorOther "a recBOT cannot appear in the tope layer"

-- * Normal form

nfT :: Distinct n => TermT n -> TypeCheck n (TermT n)
nfT tt = performing (ActionNF tt) $ case tt of
  -- universe constants
  UniverseT{} -> pure tt
  UniverseCubeT{} -> pure tt
  UniverseTopeT{} -> pure tt

  -- cube layer constants
  CubeUnitT{} -> pure tt
  CubeUnitStarT{} -> pure tt
  Cube2T{} -> pure tt
  Cube2_0T{} -> pure tt
  Cube2_1T{} -> pure tt
  CubeIT{} -> pure tt
  CubeI_0T{} -> pure tt
  CubeI_1T{} -> pure tt

  -- cube layer with computation
  CubeProductT{} -> nfTope tt
  CubeFlipT{} -> nfTope tt
  CubeUnflipT{} -> nfTope tt
  CubeSupT{} -> nfTope tt
  CubeInfT{} -> nfTope tt

  -- tope layer constants
  TopeTopT{} -> pure tt
  TopeBottomT{} -> pure tt

  -- tope layer with computation
  TopeAndT{} -> nfTope tt
  TopeOrT{} -> nfTope tt
  TopeEQT{} -> nfTope tt
  TopeLEQT{} -> nfTope tt
  TopeInvT{} -> nfTope tt
  TopeUninvT{} -> nfTope tt

  -- type layer constants
  ReflT ty _x -> pure (ReflT ty Nothing)
  RecBottomT{} -> pure tt
  TypeUnitT{} -> pure tt
  UnitT{} -> pure tt

  -- type ascriptions are ignored, since we already have a typechecked term
  TypeAscT _ty term _ty' -> nfT term

  -- now we are in the type layer
  _ ->
    typeOf tt >>= tryRestriction >>= \case
      Just tt' -> nfT tt'
      Nothing -> case tt of
        -- a hole is opaque: it never reduces, it is already a normal form
        HoleT{} -> pure tt
        t@(Var x) ->
          valueOfVar x >>= \case
            Nothing   -> pure t
            Just term -> nfT term

        TypeFunT ty orig md param mtope ret -> do
          param' <- enterModality md $ nfT param
          case mtope of
            Nothing -> do
              ret' <- underScope orig md param' Nothing ret nfT
              pure (TypeFunT ty orig md param' Nothing ret')
            Just tope -> do
              (tope', ret') <- underScope2 orig md param' tope ret $ \topeBody retBody -> do
                topeNF <- nfT topeBody
                retNF <- localTope topeNF (nfT retBody)
                pure (topeNF, retNF)
              pure (TypeFunT ty orig md param' (Just tope') ret')

        AppT ty f x ->
          whnfT f >>= \case
            LambdaT _ty _orig _arg body ->
              instantiate body x >>= nfT
            f' -> typeOf f' >>= \case
              TypeFunT _ty _orig md _param (Just tope) (ScopedAST _ UniverseTopeT{}) -> do
                x' <- enterModality md $ nfT x
                sideCondition <- instantiate tope x' >>= nfT
                pure (topeAndT (AppT ty f' x') sideCondition)
              _ -> do
                -- The ι-redex of a generated eliminator only exists at the
                -- node holding the full spine, which the recursion above
                -- never hands to 'whnfT' as a whole.
                let (h, pairs) = collectAppSpine (AppT ty f' x)
                tryDataElimStep h pairs >>= \case
                  Just stepped -> nfT stepped
                  Nothing      -> AppT ty <$> nfT f' <*> nfT x
        LetT _ty _orig _mparam val body ->
          instantiate body val >>= nfT
        LetModT ty orig app inn mparam val body ->
          (enterModality app $ whnfT val) >>= \case
            ModAppT _ md t | md == inn -> do
              val' <- enterModality md $ nfT t
              instantiate body val' >>= nfT
            b' | isRA inn -> do
              bty <- typeOf b' >>= \case
                TypeModalT _ _ t -> pure t
                _ -> panicImpossible "not modal in letmod"
              instantiate body (modExtractT bty app inn b') >>= nfT
            b' -> do
              bty <- typeOf b' >>= \case
                TypeModalT _ _ t -> pure t
                _ -> panicImpossible "not modal in letmod"
              val' <- enterModality app $ nfT b'
              body' <- underScope orig (comp app inn) bty Nothing body nfT
              pure (LetModT ty orig app inn mparam val' body')
        LambdaT ty orig _mparam body ->
          case stripTypeRestrictions (infoType ty) of
            TypeFunT _ty _orig md param mtope _ret -> do
              param' <- enterModality md $ nfT param
              case mtope of
                Nothing -> do
                  body' <- underScope orig md param' Nothing body nfT
                  pure (LambdaT ty orig (Just (LambdaParam md param' Nothing)) body')
                Just tope -> do
                  (tope', body') <- underScope2 orig md param' tope body $ \topeBody bodyBody -> do
                    topeNF <- nfT topeBody
                    bodyNF <- localTope topeNF (nfT bodyBody)
                    pure (topeNF, bodyNF)
                  pure (LambdaT ty orig (Just (LambdaParam md param' (Just tope'))) body')
            _ -> panicImpossible "lambda with a non-function type"

        TypeSigmaT ty orig md a b -> do
          a' <- enterModality md $ nfT a
          b' <- underScope orig md a' Nothing b nfT
          pure (TypeSigmaT ty orig md a' b')
        PairT ty l r -> PairT ty <$> nfT l <*> nfT r
        FirstT ty t ->
          whnfT t >>= \case
            PairT _ l _r -> nfT l
            t'           -> FirstT ty <$> nfT t'
        SecondT ty t ->
          whnfT t >>= \case
            PairT _ _l r -> nfT r
            t'           -> SecondT ty <$> nfT t'

        TypeIdT ty x _tA y -> TypeIdT ty <$> nfT x <*> pure Nothing <*> nfT y
        IdJT ty tA a tC d x p ->
          whnfT p >>= \case
            ReflT{} -> nfT d
            p' -> IdJT ty <$> nfT tA <*> nfT a <*> nfT tC <*> nfT d <*> nfT x <*> nfT p'

        RecOrT _ty rs ->
          firstMatching rs >>= \case
            Just tt' -> nfT tt'
            Nothing
              | [tt'] <- nubT (map snd rs) -> nfT tt'
              | otherwise -> pure tt
        TypeModalT ty md b -> do
          b' <- enterModality md $ nfT b
          pure (TypeModalT ty md b')
        ModAppT ty md b ->
          (enterModality md $ whnfT b) >>= \case
            ModExtractT _ app inn t | inn == md -> enterModality (comp app inn) $ nfT t
            b' -> ModAppT ty md <$> (enterModality md $ nfT b')
        ModExtractT ty app inn b ->
          (enterModality app $ whnfT b) >>= \case
            ModAppT _ md t | inn == md -> enterModality (comp app inn) $ nfT t
            b' -> ModExtractT ty app inn <$> (enterModality app $ nfT b')
        TypeRestrictedT ty type_ rs -> do
          rs' <- forM rs $ \(tope, term) ->
            nfTope tope >>= \case
              TopeBottomT{} -> pure Nothing
              tope' -> do
                term' <- localTope tope' (nfT term)
                return (Just (tope', term'))
          case catMaybes rs' of
            []   -> nfT type_
            rs'' -> TypeRestrictedT ty <$> nfT type_ <*> pure rs''
