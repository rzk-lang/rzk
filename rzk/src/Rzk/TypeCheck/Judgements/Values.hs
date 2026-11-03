{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
-- | The judgements over values: 'check' and 'infer' take and return types as
-- free-foil-NBE values ("Rzk.TypeCheck.NbE" gives the rules) and produce the
-- same elaborated terms as "Rzk.TypeCheck.Judgements", whose type annotations
-- are read back only when a consumer looks at them.
--
-- A type is matched by forcing its value ('whnfV'), the codomain of a Π or Σ
-- at an argument is its scope evaluated under the extended environment
-- ('codomainAtV'), and an inferred type is compared with the expected one by
-- the library's conversion in the context's variance, with the unification
-- on the read-back types where that answers no ('subtypeV'). The tope solver
-- works on terms: a tope the user wrote is elaborated into one, and a tope
-- that comes out of a type value is read back before it is assumed.
--
-- The algorithm is Coquand's (/An algorithm for type-checking dependent
-- types/, 1996), in the form of free-foil-NBE's @DependentTypes.Elaborate@
-- (Renata Shakirova and Nikolai Kudasov) and of Kovács' elaboration-zoo,
-- where types are values and the elaborated syntax carries read-back types.
-- The rules mirror "Rzk.TypeCheck.Judgements" case by case. A @let mod@ or a
-- @match@ is left to the judgements on terms ('checkByTerms').
module Rzk.TypeCheck.Judgements.Values
  ( check
  , infer
  , inferAs
  ) where

import           Control.Monad                     (forM, forM_, unless, when)
import           Control.Monad.Except              (catchError)
import           Control.Monad.Reader              (ask, asks)
import           Data.List                         (tails)
import           Data.Maybe                        (fromMaybe, isNothing)

import           Control.Monad.Foil                (DExt, Distinct, NameBinder)
import qualified Control.Monad.Foil                as Foil
import           Control.Monad.Free.Foil           (AST (Var),
                                                    ScopedAST (..))
import           Control.Monad.Free.Foil.Annotated (AnnSig (..))
import qualified FreeFoil.NbE                      as NbE
import qualified FreeFoil.NbE.Conversion           as Conv

import           Language.Rzk.Foil.Names           (Binder (..), TModality (..),
                                                    binderLeaves, binderName,
                                                    unmarkUnresolved)
import           Language.Rzk.Foil.Syntax
import           Rzk.TypeCheck.Context
import           Rzk.TypeCheck.Display             (namingOfContext,
                                                    panicImpossible, ppTerm)
import           Rzk.TypeCheck.Error
import           Rzk.TypeCheck.Eval
import           Rzk.TypeCheck.Judgements          (checkHoleAgainstShape,
                                                    checkNameShadowing,
                                                    modalizeTope, motiveType,
                                                    recordHole)
import qualified Rzk.TypeCheck.Judgements          as Terms
import           Rzk.TypeCheck.Monad
import           Rzk.TypeCheck.NbE                 hiding (openScoped)
import qualified Rzk.TypeCheck.NbE                 as Rules
import           Rzk.TypeCheck.Render              (ppInContext)
import           Rzk.TypeCheck.Unify

-- * Values at the boundary

-- | A typed term as a value.
evalV :: Distinct n => TermT n -> TypeCheck n (Value n)
evalV t = asks (`evalTerm` t)

-- | A value read back. The result is a thunk: nothing is read back until a
-- consumer looks.
quoteV :: Distinct n => Value n -> TypeCheck n (TermT n)
quoteV v = asks $ \ctx -> NbE.quote (ctxScope ctx) (rootCtx ctx) v

-- | A weak head normal form as a value again.
valueOfWhnf :: NbE.Whnf NameBinder Sig n -> Value n
valueOfWhnf = \case
  NbE.VVar x             -> NbE.vvar x
  NbE.VNode node         -> NbE.vnode node
  NbE.VSuspended env node -> NbE.vsuspended env node

-- | A weak head normal form read back.
quoteW :: Distinct n => NbE.Whnf NameBinder Sig n -> TypeCheck n (TermT n)
quoteW = quoteV . valueOfWhnf

-- | A value read back in weak head normal form, as the judgements on terms
-- store a type they matched ('typeOf' and 'whnfT' give the form) in an
-- annotation; so that the two judgements annotate alike.
quoteF :: Distinct n => Value n -> TypeCheck n (TermT n)
quoteF v = whnfV v >>= quoteW

-- | The type of a value.
typeOfV :: Distinct n => Value n -> TypeCheck n (Value n)
typeOfV v = asks $ \ctx -> typeOfValue (ctxScope ctx) (rootCtx ctx) v

-- | A term the judgement returns as it is, with its type from its annotation.
constant :: Distinct n => TermT n -> TypeCheck n (TermT n, Value n)
constant t = (t,) <$> (typeOfUncomputed t >>= evalV)

-- | The universe of cubes, as a value.
cubeUniverse :: Distinct n => TypeCheck n (Value n)
cubeUniverse = evalV cubeT

-- | A value in weak head normal form: the library's 'NbE.force', and where
-- that leaves a neutral, the rules of 'whnfT' the library's evaluation does
-- not have (the ι-rule of a @#data@ eliminator, reduction by a restriction
-- face of the type, a tope family applied), through readback. A neutral
-- that 'whnfT' leaves alone is returned as it is.
whnfV :: Distinct n => Value n -> TypeCheck n (NbE.Whnf NameBinder Sig n)
whnfV v = case NbE.force v of
  w | stuck w -> do
        t <- quoteV v
        t' <- whnfT t
        scope <- asks ctxScope
        if alphaEqT scope t t' then pure w else NbE.force <$> evalV t'
  -- A restriction at ⊥ is no restriction (as 'whnfT' drops it): the faces
  -- whose tope normalises to ⊥ go, and a type with none left is its
  -- underlying type.
  w@(NbE.VNode (AnnSig info (TypeRestrictedF ty rs))) -> do
    kept <- fmap concat $ forM rs $ \face@(tope, _) -> do
      topeNF <- quoteV tope >>= nfTope
      pure [ face | not (eqT topeBottomT topeNF) ]
    case kept of
      [] -> whnfV ty
      _ | length kept == length rs -> pure w
        | otherwise -> pure (NbE.force (NbE.vnode (AnnSig info (TypeRestrictedF ty kept))))
  w -> pure w
  where
    stuck = \case
      NbE.VVar _ -> True
      NbE.VNode (AnnSig _ node) -> stuckNode node
      NbE.VSuspended _ (AnnSig _ node) -> stuckNode node
    stuckNode :: TermSig a b -> Bool
    stuckNode = \case
      AppF{}        -> True
      FirstF{}      -> True
      SecondF{}     -> True
      IdJF{}        -> True
      RecOrF{}      -> True
      LetModF{}     -> True
      ModAppF{}     -> True
      ModExtractF{} -> True
      _             -> False

-- | The interval cube a point's type is, for @≤@, @sup@ and @inf@.
data Interval = IntervalHole | Interval2 | IntervalI | NotInterval

intervalOf :: NbE.Whnf NameBinder Sig n -> Interval
intervalOf = \case
  NbE.VNode (AnnSig _ HoleF{}) -> IntervalHole
  NbE.VNode (AnnSig _ Cube2F)  -> Interval2
  NbE.VNode (AnnSig _ CubeIF)  -> IntervalI
  _                            -> NotInterval

-- * Conversion

-- | The actual type of a term is a subtype of the expected one, in the
-- context's variance: the library's conversion decides the common case, and
-- where it answers no, the unification on the read-back types decides, with
-- its error messages and its treatment of holes.
subtypeV :: Distinct n => TermT n -> Value n -> Value n -> TypeCheck n ()
subtypeV term expected actual = do
  ctx <- ask
  unless (Conv.conv (ctxScope ctx) (rootCtx ctx) expected actual) $ do
    expected' <- quoteV expected
    actual' <- quoteV actual
    unifyTypes term expected' actual'

-- | Two values agree as terms (invariantly), by the library's conversion,
-- and otherwise by 'unifyTerms' on the read-back terms, in the direction
-- the judgement on terms would call it.
convV :: Distinct n => Value n -> Value n -> TypeCheck n ()
convV expected actual = do
  ctx <- ask
  unless (Conv.conv (ctxScope ctx) (withVariance Invariant (rootCtx ctx)) expected actual) $ do
    expected' <- quoteV expected
    actual' <- quoteV actual
    unifyTerms expected' actual'

-- * Binders

-- | Run an action under a binder whose type (and value) are values; the
-- entry holds them, and their read-back for whoever reads terms.
underBinderV
  :: (Distinct n, DExt n l)
  => NameBinder n l -> Binder -> TModality -> Value n -> Maybe (Value n)
  -> TypeCheck l a -> TypeCheck n a
underBinderV binder orig md ty mval action = do
  ctx <- ask
  let quote = NbE.quote (ctxScope ctx) (rootCtx ctx)
      info = (binderInfo orig md (quote ty) (quote <$> mval) (ctxLocation ctx))
        { varTypeValue = Just ty, varDefinition = mval }
  underBinderInfo binder info action

-- | 'withBinder' with the binder's type a value.
withBinderV
  :: Distinct n
  => Binder -> TModality -> Value n
  -> (forall l. (DExt n l, Distinct l) => NameBinder n l -> TypeCheck l a)
  -> TypeCheck n a
withBinderV orig md ty k = do
  scope <- asks ctxScope
  Foil.withFresh scope $ \binder -> underBinderV binder orig md ty Nothing (k binder)

-- | 'elaborateUnder' with the binder's type and value values.
elaborateUnderV
  :: Distinct n
  => Binder -> TModality -> Value n -> Maybe (Value n) -> ScopedTerm n
  -> (forall l. (DExt n l, Distinct l) => NameBinder n l -> Term l -> TypeCheck l (TermT l))
  -> TypeCheck n (ScopedTermT n)
elaborateUnderV orig md ty mval scoped k = do
  scope <- asks ctxScope
  withScopedT scope scoped $ \binder body ->
    ScopedAST binder <$> underBinderV binder orig md ty mval (k binder body)

-- | 'checkUnderWith' with the binder's type and value values.
checkUnderWithV
  :: Distinct n
  => Binder -> TModality -> Value n -> Maybe (Value n) -> ScopedTerm n
  -> (forall l. (DExt n l, Distinct l) => NameBinder n l -> Term l -> TypeCheck l a)
  -> TypeCheck n a
checkUnderWithV orig md ty mval scoped k = do
  scope <- asks ctxScope
  withScopedT scope scoped $ \binder body ->
    underBinderV binder orig md ty mval (k binder body)

checkUnderV
  :: Distinct n
  => Binder -> TModality -> Value n -> ScopedTerm n
  -> (forall l. (DExt n l, Distinct l) => NameBinder n l -> Term l -> TypeCheck l a)
  -> TypeCheck n a
checkUnderV orig md ty = checkUnderWithV orig md ty Nothing

-- | A scoped position of a Π or Σ value, opened under a binder that has just
-- been entered: the body evaluated with the bound name for the binder.
openScopedV
  :: (Distinct i, DExt n l)
  => NameBinder n l -> Env i n -> ScopedAST NameBinder Sig i -> TypeCheck l (Value l)
openScopedV binder env scoped = asks $ \ctx ->
  Rules.openScoped (ctxScope ctx) (rootCtx ctx) env binder scoped

-- | A scoped position of a Π or Σ value, opened under a binder and read back.
openScopedQ
  :: (Distinct i, DExt n l)
  => NameBinder n l -> Env i n -> ScopedAST NameBinder Sig i -> TypeCheck l (TermT l)
openScopedQ binder env scoped = openScopedV binder env scoped >>= quoteV

-- | The codomain of a Π or Σ value at an argument.
codomainAtV
  :: (Distinct n, Distinct i)
  => Env i n -> ScopedAST NameBinder Sig i -> Value n -> TypeCheck n (Value n)
codomainAtV env scoped arg = asks $ \ctx ->
  codomainAt (ctxScope ctx) (rootCtx ctx) env scoped arg

-- * The kinds of a domain

-- | 'ensureTypeAtU' on a value.
ensureTypeAtUV :: Distinct n => String -> Value n -> TypeCheck n ()
ensureTypeAtUV position ty = classifyDomainV ty >>= \case
  DomainType      -> pure ()
  DomainCubePoint -> pure ()
  kind -> issueTypeError $ TypeErrorOther $
    position <> " must be a type, got " <> domainKindName kind

-- | 'classifyDomain' on a value.
classifyDomainV :: Distinct n => Value n -> TypeCheck n DomainKind
classifyDomainV ty = typeOfV ty >>= whnfV >>= \case
  NbE.VNode (AnnSig _ UniverseCubeF) -> pure DomainCubePoint
  _ -> whnfV ty >>= \case
    NbE.VNode (AnnSig _ UniverseCubeF) -> pure DomainCubeUniverse
    NbE.VNode (AnnSig _ UniverseTopeF) -> pure DomainTopeUniverse
    NbE.VNode (AnnSig _ (TypeModalF _md inner)) -> classifyDomainV inner >>= \case
      DomainType      -> pure DomainType
      DomainCubePoint -> pure DomainType
      _               -> pure DomainModalTope
    NbE.VSuspended env (AnnSig _ (TypeFunF orig md param mtope ret)) -> do
      cubeDom <- case mtope of
        Just _  -> pure True
        Nothing -> typeOfV param >>= whnfV >>= \case
          NbE.VNode (AnnSig _ UniverseCubeF) -> pure True
          _                                  -> pure False
      inner <- withBinderV orig md param $ \binder ->
        openScopedV binder env ret >>= classifyDomainV
      pure $ case inner of
        DomainTopeUniverse
          | cubeDom   -> DomainTopeFamily
          | otherwise -> DomainOtherTopeFamily
        DomainTopeFamily
          | cubeDom   -> DomainTopeFamily
          | otherwise -> DomainOtherTopeFamily
        DomainOtherTopeFamily -> DomainOtherTopeFamily
        DomainModalTope       -> DomainModalTope
        DomainCubeUniverse    -> DomainCubeFamily
        DomainCubeFamily      -> DomainCubeFamily
        _                     -> DomainType
    _ -> pure DomainType

-- * The judgements on terms, for what is not covered here

-- | The judgement on terms for a construct these judgements do not cover
-- yet (@let mod@ and @match@): the expected type read back, the result as
-- it is.
checkByTerms :: Distinct n => Term n -> Value n -> TypeCheck n (TermT n)
checkByTerms term ty = quoteV ty >>= Terms.typecheck term

inferByTerms :: Distinct n => Term n -> TypeCheck n (TermT n, Value n)
inferByTerms term = do
  term' <- Terms.infer term
  ty <- typeOfUncomputed term' >>= evalV
  pure (term', ty)

-- * Checking

-- | Check a term against a type value; the elaborated term.
check :: Distinct n => Term n -> Value n -> TypeCheck n (TermT n)
check term ty = do
  tyQ <- quoteV ty
  performing (ActionTypeCheck term tyQ) $ case term of
    Hole (Just name) | Just x <- unmarkUnresolved name ->
      issueTypeError (TypeErrorUndefined x)

    Hole mname -> do
      reject <- asks ctxHolesAreErrors
      if reject
        then issueTypeError (TypeErrorUnsolvedHole mname tyQ)
        else do
          recordHole mname tyQ
          pure (holeT tyQ mname)

    _ -> whnfV ty >>= \ty' -> quoteW ty' >>= \tyW -> case ty' of
      -- Under an absurd context the term must still be well-formed.
      NbE.VNode (AnnSig _ RecBottomF) -> do
        _ <- infer term
        pure recBottomT

      -- A recOR against a restricted type takes the restriction into each
      -- branch; any other term is checked against the underlying type and
      -- then meets each face on its overlap with the context.
      NbE.VNode (AnnSig info (TypeRestrictedF ty' rs)) -> case term of
        RecOr branches -> checkRecOrAgainstV (NbE.vnode (AnnSig info (TypeRestrictedF ty' rs))) branches
        _ -> do
          term' <- check term ty'
          termV <- evalV term'
          forM_ rs $ \(tope, rterm) -> do
            topeQ <- quoteV tope
            checkTopeAgainstContext "restriction face" topeQ
            localTope topeQ $ convV rterm termV
          pure term'

      _ -> case term of
        Lambda orig mparam body -> case ty' of
          NbE.VSuspended env (AnnSig info node@(TypeFunF _orig' md' param' mtope' ret)) -> do
            -- The Π as the judgement on terms sees it: its domain and tope
            -- read back for the λ's parameter, and itself for the λ's type.
            piQ <- quoteV (NbE.vsuspended env (AnnSig info node))
            let (paramQ, mtopeQ) = case piQ of
                  TypeFunT _ _ _ p mt _ -> (p, mt)
                  _ -> panicImpossible "a Π value read back as something else"
            -- The λ's own domain annotation, if it wrote one, must agree with
            -- the domain of the type it is checked against.
            case mparam of
              Nothing -> pure ()

              Just (LambdaParam md param Nothing) -> do
                when (md /= md') $
                  issueTypeError (TypeErrorModalityMismatch
                    (MismatchLambdaParameter orig) md' md (Just term))
                (paramE, paramK) <- enterModality md $ infer param
                -- an argument can be a shape, which is a function into TOPE;
                -- the domain is then its cube, and its tope is the λ's shape
                -- tope.
                mcube <- whnfV paramK >>= \case
                  NbE.VSuspended _ (AnnSig _ (TypeFunF _ _ cube _ (ScopedAST _ UniverseTopeT{}))) -> do
                    mapM_ checkNameShadowing (binderLeaves orig)
                    pure (Just cube)
                  _ -> pure Nothing
                paramEV <- evalV paramE
                convV param' (fromMaybe paramEV mcube)
                mapM_ checkNameShadowing (binderLeaves orig)
                case mcube of
                  Nothing -> pure ()
                  Just _ ->
                    withBinderV orig md param' $ \binder -> do
                      -- eta expand the shape into a tope over the bound variable
                      let etaTope = modalizeTope md $
                            appT topeT (Foil.sink paramE) (Var (Foil.nameOf binder))
                      expected <- maybe (pure topeTopT) (openScopedQ binder env) mtope'
                      unifyTerms expected etaTope

              Just (LambdaParam md param (Just tope)) -> do
                when (md /= md') $
                  issueTypeError (TypeErrorModalityMismatch
                    (MismatchLambdaParameter orig) md' md (Just term))
                paramK <- typeOfV param'
                param'' <- enterModality md $ check param paramK
                param''V <- evalV param''
                convV param' param''V
                mapM_ checkNameShadowing (binderLeaves orig)
                checkUnderV orig md param' tope $ \binder topeTerm -> do
                  tope'' <- check topeTerm topeUniverseValue
                  expected <- maybe (pure topeTopT) (openScopedQ binder env) mtope'
                  unifyTerms expected tope''

            mapM_ checkNameShadowing (binderLeaves orig)
            naming <- asks namingOfContext
            body' <- elaborateUnderV orig md' param' Nothing body $ \binder bodyTerm -> do
              mtopeIn <- traverse (openScopedQ binder env) mtope'
              maybe id localTope mtopeIn $ do
                retIn <- openScopedV binder env ret
                bodyIn <- check bodyTerm retIn
                -- A tope family checked against @{t : I | ψ} → TOPE@ carries
                -- its domain (see the judgement on terms).
                case (ret, mtopeIn) of
                  (ScopedAST _ UniverseTopeT{}, Just domainIn) -> do
                    bodyNF <- nfT bodyIn
                    domainNF <- nfT domainIn
                    included <- [plainTope bodyNF] `entailM` domainNF
                    if included then pure bodyIn else do
                      warn <- asks ctxWarnTopeFamilyDomain
                      when warn $ do
                        namingIn <- asks namingOfContext
                        loc <- asks ctxLocation
                        recordCheckWarning $ TopeFamilyDomainWarning
                          (ppTerm naming term)
                          (ppTerm namingIn (untyped domainNF))
                          loc
                      pure (topeAndT domainIn bodyIn)
                  _ -> pure bodyIn
            pure (lambdaT piQ orig (Just (LambdaParam md' paramQ mtopeQ)) body')

          _ -> issueTypeError $ TypeErrorUnexpectedLambda term tyQ

        Let orig annot val body -> do
          (val', bindTy) <- performing (ActionCheckLetValue (binderName orig)) $ case annot of
            Nothing -> infer val
            Just bindType -> do
              bindType' <- check bindType universeValue
              bindTyV <- evalV bindType'
              val' <- check val bindTyV
              pure (val', bindTyV)
          valV <- evalV val'
          bindTyQ <- quoteF bindTy
          body' <- elaborateUnderV orig Id bindTy (Just valV) body $ \_binder bodyTerm ->
            check bodyTerm (Foil.sink ty)
          pure (letT tyW orig (Just bindTyQ) val' body')

        LetMod{} -> checkByTerms term ty

        Pair l r -> case ty' of
          NbE.VNode (AnnSig _ (CubeProductF a b)) -> do
            l' <- check l a
            r' <- check r b
            pure (pairT tyW l' r')
          NbE.VSuspended env (AnnSig _ (TypeSigmaF _orig md a b)) -> do
            l' <- enterModality md $ check l a
            whnfV a >>= \case
              NbE.VNode (AnnSig _ UniverseF) -> evalV l' >>= ensureTypeAtUV "a pair component at type U"
              _ -> pure ()
            lV <- evalV l'
            bAt <- codomainAtV env b lV
            r' <- check r bAt
            pure (pairT tyW l' r')
          _ -> issueTypeError $ TypeErrorUnexpectedPair term tyQ

        Refl mx -> case ty' of
          NbE.VNode (AnnSig _ (TypeIdF y _tA z)) -> do
            tA <- typeOfV y
            forM_ mx $ \(x, mxty) -> do
              forM_ mxty $ \xty -> do
                xty' <- check xty universeValue
                xtyV <- evalV xty'
                convV tA xtyV
              x' <- check x tA
              xV <- evalV x'
              convV xV y >> convV y xV
              convV xV z >> convV z xV
            when (isNothing mx) $
              convV y z >> convV z y
            yQ <- quoteV y
            tAQ <- quoteF tA
            pure (reflT tyW (Just (yQ, Just tAQ)))
          _ -> issueTypeError $ TypeErrorUnexpectedRefl term tyQ

        ModExtract{} -> panicImpossible "extract is an internal term and cannot be typechecked"

        ModApp md body -> case ty' of
          NbE.VNode (AnnSig _ (TypeModalF md' tpe)) -> do
            when (md /= md') $ issueTypeError $
              TypeErrorModalityMismatch
                MismatchModalIntroduction md' md (Just term)
            body' <- enterModality md $ check body tpe
            pure (modAppT tyW md body')
          _ -> issueTypeError $
            TypeErrorNotModal ModalTypeForIntroduction term md tyQ

        -- A recOR is a term-level eliminator, not a tope.
        RecOr rs -> case ty' of
          NbE.VNode (AnnSig _ UniverseTopeF) -> issueTypeError $
            TypeErrorOther "a recOR cannot be used as a tope"
          _ -> checkRecOrAgainstV ty rs

        Match{} -> checkByTerms term ty

        -- A neutral term is inferred, then its type compared with the
        -- expected one; in lenient mode a term that still carries a hole is
        -- accepted whatever the comparison says (see the judgement on
        -- terms).
        _ -> do
          (term', inferredType) <- infer term
          lenient <- not <$> asks ctxHolesAreErrors
          if lenient && containsHole term'
            then subtypeV term' ty inferredType `catchError` \_ -> pure ()
            else subtypeV term' ty inferredType
          pure term'

-- | Check a @recOR@ against a known expected type: each branch is checked
-- against it under its own guard, the branches must agree on their overlaps,
-- and together they must cover the context.
checkRecOrAgainstV :: Distinct n => Value n -> [(Term n, Term n)] -> TypeCheck n (TermT n)
checkRecOrAgainstV expected rs = do
  rs' <- forM rs $ \(tope, rterm) -> do
    tope' <- check tope topeUniverseValue
    checkTopeAgainstContext "recOR branch guard" tope'
    localTope tope' $ do
      expected' <- pruneVacuousFacesV expected
      rterm' <- check rterm expected'
      pure (tope', rterm')
  sequence_ [ checkCoherence l r | l:rs'' <- tails rs', r <- rs'' ]
  contextEntailsUnion (map fst rs')
  expectedQ <- quoteF expected
  pure (recOrT expectedQ rs')

-- | 'Rzk.TypeCheck.Judgements.pruneVacuousFaces' on a value.
pruneVacuousFacesV :: Distinct n => Value n -> TypeCheck n (Value n)
pruneVacuousFacesV ty = whnfV ty >>= \case
  NbE.VNode (AnnSig info (TypeRestrictedF ty' rs)) -> do
    contextTopes <- asks ctxTopesNF
    kept <- fmap concat $ forM rs $ \face@(tope, _) -> do
      topeQ <- quoteV tope
      vacuous <- if containsHole topeQ
        then pure False
        else (plainTope topeQ : contextTopes) `entailM` topeBottomT
      pure [ face | not vacuous ]
    pure $ case kept of
      [] -> ty'
      _  -> NbE.vnode (AnnSig info (TypeRestrictedF ty' kept))
  _ -> pure ty

-- * Inference

-- | Infer the type of a term: the elaborated term and its type as a value.
infer :: Distinct n => Term n -> TypeCheck n (TermT n, Value n)
infer tt = performing (ActionInfer tt) $ case tt of
  Hole (Just name) | Just x <- unmarkUnresolved name ->
    issueTypeError (TypeErrorUndefined x)
  Hole _mname -> issueTypeError (TypeErrorCannotInferHole tt)
  Var x -> do
    topLevel <- isTopLevelVar x
    unless topLevel $ do
      varMod <- modalityOfVar x
      locks <- locksOfVar x
      unless (coe varMod locks) $
        issueTypeError $ TypeErrorUnaccessibleVar x varMod locks
    ty <- typeOfV (NbE.vvar x)
    pure (Var x, ty)

  Universe     -> constant universeT
  UniverseCube -> constant cubeT
  UniverseTope -> constant topeT

  CubeUnit     -> constant cubeUnitT
  CubeUnitStar -> constant cubeUnitStarT

  Cube2   -> constant cube2T
  Cube2_0 -> constant cube2_0T
  Cube2_1 -> constant cube2_1T

  CubeI   -> constant cubeIT
  CubeI_0 -> constant cubeI_0T
  CubeI_1 -> constant cubeI_1T
  CubeProduct l r -> do
    cubes <- cubeUniverse
    l' <- check l cubes
    r' <- check r cubes
    constant (cubeProductT l' r')

  CubeFlip t -> do
    (t', ty) <- infer t
    whnfV ty >>= \case
      NbE.VNode (AnnSig _ CubeIF) -> constant (cubeFlipT cubeIT t')
      NbE.VNode (AnnSig _ Cube2F) -> constant (cubeFlipT cube2T t')
      _ -> do
        tyStr <- quoteV ty >>= ppInContext
        issueTypeError $ TypeErrorOther $
          "flip expects an interval cube (2 or 𝕀); got " <> tyStr
  CubeUnflip t -> do
    (t', ty) <- infer t
    inner <- whnfV ty >>= \case
      NbE.VNode (AnnSig _ (TypeModalF Op inner)) -> Just <$> whnfV inner
      _ -> pure Nothing
    case inner of
      Just (NbE.VNode (AnnSig _ CubeIF)) -> constant (cubeUnflipT cubeIT t')
      Just (NbE.VNode (AnnSig _ Cube2F)) -> constant (cubeUnflipT cube2T t')
      _ -> do
        tyStr <- quoteV ty >>= ppInContext
        issueTypeError $ TypeErrorOther $
          "unflip expects an interval cube (2 or 𝕀) under _op; got " <> tyStr

  CubeSup l r -> lattice "sup" cubeSupT l r
  CubeInf l r -> lattice "inf" cubeInfT l r

  Pair l r -> do
    (l', lt) <- infer l
    (r', rt) <- infer r
    typeOfV lt >>= whnfV >>= \case
      --    Γ ⊢ l ⇒ (I : CUBE)
      --    Γ ⊢ r ⇒ (J : CUBE)
      -- ———————————————————————————
      -- Γ ⊢ (l, r) ⇒ (I × J : CUBE)
      NbE.VNode (AnnSig _ UniverseCubeF) -> do
        ltQ <- quoteF lt
        rtQ <- quoteF rt
        constant (pairT (cubeProductT ltQ rtQ) l' r')
      --    Γ ⊢ l ⇒ (A : U)
      --    Γ ⊢ r ⇒ (B : U)
      -- ———————————————————————————
      -- Γ ⊢ (l, r) ⇒ (A × B : U)             where A × B = Σ (_ : A), B
      _ -> do
        -- NOTE: infer as a non-dependent pair!
        ltQ <- quoteF lt
        rtQ <- quoteF rt
        rtScope <- constScope rtQ
        constant (pairT (typeSigmaT (BinderVar Nothing) Id ltQ rtScope) l' r')

  First t -> do
    (t', tTy) <- infer t
    whnfV (stripRestrictions tTy) >>= \case
      NbE.VNode (AnnSig _ RecBottomF) -> constant recBottomT -- FIXME: is this ok?
      NbE.VSuspended _ (AnnSig _ (TypeSigmaF _orig _md lt _rt)) -> do
        ltQ <- quoteV lt
        pure (firstT ltQ t', lt)
      NbE.VNode (AnnSig _ (CubeProductF l _r)) -> do
        lQ <- quoteV l
        pure (firstT lQ t', l)
      _ -> do
        tyQ <- quoteV tTy
        issueTypeError $ TypeErrorNotPair t' tyQ

  Second t -> do
    (t', tTy) <- infer t
    whnfV (stripRestrictions tTy) >>= \case
      NbE.VNode (AnnSig _ RecBottomF) -> constant recBottomT -- FIXME: is this ok?
      NbE.VSuspended env (AnnSig _ (TypeSigmaF _orig _md lt rt)) -> do
        ltQ <- quoteV lt
        firstV <- evalV (firstT ltQ t')
        rtAt <- codomainAtV env rt firstV
        rtAtQ <- quoteV rtAt
        pure (secondT rtAtQ t', rtAt)
      NbE.VNode (AnnSig _ (CubeProductF _l r)) -> do
        rQ <- quoteV r
        pure (secondT rQ t', r)
      _ -> do
        tyQ <- quoteV tTy
        issueTypeError $ TypeErrorNotPair t' tyQ

  TypeUnit -> constant typeUnitT
  Unit -> constant unitT

  TopeTop -> constant topeTopT
  TopeBottom -> constant topeBottomT

  TopeEQ l r -> do
    (l', lt) <- inferPoint l
    r' <- check r lt
    constant (topeEQT l' r')

  TopeLEQ l r -> do
    (l', lTy) <- inferDirectedPoint l
    (r', rTy) <- inferDirectedPoint r
    lK <- whnfV lTy
    rK <- whnfV rTy
    case (intervalOf lK, intervalOf rK) of
      -- A point hole in lenient mode has an unknown directed cube. Keep the
      -- relation as an incomplete sketch until that cube is known.
      (IntervalI, Interval2) -> do
        r'' <- evalV cubeIT >>= check r
        constant (topeLEQT l' r'')
      (Interval2, IntervalI) -> do
        l'' <- evalV cubeIT >>= check l
        constant (topeLEQT l'' r')
      (NotInterval, _) -> notIntervals lTy rTy
      (_, NotInterval) -> notIntervals lTy rTy
      _ -> constant (topeLEQT l' r')
    where
      notIntervals lTy rTy = do
        lStr <- quoteV lTy >>= ppInContext
        rStr <- quoteV rTy >>= ppInContext
        issueTypeError $ TypeErrorOther $
          "the (t ≤ s) tope expects points in interval cubes (2 or 𝕀); got "
            <> lStr <> " and " <> rStr

  TopeAnd l r -> do
    l' <- check l topeUniverseValue
    r' <- check r topeUniverseValue
    constant (topeAndT l' r')

  TopeOr l r -> do
    l' <- check l topeUniverseValue
    r' <- check r topeUniverseValue
    constant (topeOrT l' r')

  TopeInv t -> do
    t' <- check t topeUniverseValue
    constant (topeInvT t')

  TopeUninv t -> do
    t' <- evalV (typeModalT universeT Op topeT) >>= check t
    constant (topeUninvT t')

  RecBottom -> do
    contextEntails topeBottomT
    constant recBottomT

  RecOr rs -> do
    ttts <- forM rs $ \(tope, term) -> do
      tope' <- check tope topeUniverseValue
      -- A guard may overhang the context; only coverage is required.
      checkTopeAgainstContext "recOR branch guard" tope'
      localTope tope' $ do
        (term', ty) <- inferAs universeValue term
        tyQ <- quoteV ty
        pure (tope', (term', tyQ))
    let rs' = map (fmap fst) ttts
        ts  = map (fmap snd) ttts
    sequence_ [ checkCoherence l r | l:rs'' <- tails rs', r <- rs'' ]
    contextEntailsUnion (map fst ttts)
    constant (recOrT (recOrT universeT ts) rs')

  TypeFun orig md a Nothing b -> do
    (a', aK) <- enterModality md $ infer a
    let ordinary = do
          mapM_ checkNameShadowing (binderLeaves orig)
          aV <- evalV a'
          b' <- elaborateUnderV orig md aV Nothing b $ \_binder bTerm ->
            check bTerm universeValue
          pure (typeFunT orig md a' Nothing b', universeValue)
    whnfV aK >>= \case
      -- an argument can be a type, including TOPE: a bare tope parameter
      -- (phi : TOPE) means the same as the tope family (phi : 1 -> TOPE)
      NbE.VNode (AnnSig _ UniverseF) -> ordinary
      -- an argument can be a cube
      NbE.VNode (AnnSig _ UniverseCubeF) -> ordinary
      -- an argument can be a shape
      NbE.VSuspended env (AnnSig _ (TypeFunF _ _ cube mtope (ScopedAST _ UniverseTopeT{}))) -> do
        mapM_ checkNameShadowing (binderLeaves orig)
        cubeQ <- quoteV cube
        (tope', b') <- checkUnderV orig md cube b $ \binder bTerm -> do
          -- eta expand a' into a tope over the bound variable
          let etaTope = modalizeTope md $
                appT topeT (Foil.sink a') (Var (Foil.nameOf binder))
          tope' <- case mtope of
            Nothing     -> pure etaTope
            Just tope'' -> do
              inner <- openScopedQ binder env tope''
              pure (topeAndT inner etaTope)
          bTyped <- localTope etaTope $ check bTerm universeValue
          pure (ScopedAST binder tope', ScopedAST binder bTyped)
        pure (typeFunT orig md cubeQ (Just tope') b', universeValue)
      _ -> do
        aKQ <- quoteV aK
        issueTypeError $ TypeErrorInvalidArgumentType a aKQ

  TypeFun orig md cube (Just tope) ret -> do
    cube' <- cubeUniverse >>= \cubes -> enterModality md $ check cube cubes
    mapM_ checkNameShadowing (binderLeaves orig)
    cubeV <- evalV cube'
    (tope', ret') <- checkUnderV orig md cubeV tope $ \binder topeTerm -> do
      topeTyped <- check topeTerm topeUniverseValue
      retTerm <- openScoped binder ret
      retTyped <- localTope topeTyped $ check retTerm universeValue
      pure (ScopedAST binder topeTyped, ScopedAST binder retTyped)
    pure (typeFunT orig md cube' (Just tope') ret', universeValue)

  TypeSigma orig md a b -> do
    a' <- enterModality md $ check a universeValue
    aV <- evalV a'
    ensureTypeAtUV "a Σ component" aV
    mapM_ checkNameShadowing (binderLeaves orig)
    b' <- elaborateUnderV orig md aV Nothing b $ \_binder bTerm -> do
      bTyped <- check bTerm universeValue
      evalV bTyped >>= ensureTypeAtUV "a Σ component"
      pure bTyped
    pure (typeSigmaT orig md a' b', universeValue)

  TypeId x (Just tA) y -> do
    tA' <- check tA universeValue
    tAV <- evalV tA'
    ensureTypeAtUV "the carrier of an identity type" tAV
    x' <- check x tAV
    y' <- check y tAV
    pure (typeIdT x' (Just tA') y', universeValue)

  TypeId x Nothing y -> do
    (x', tA) <- inferAs universeValue x
    ensureTypeAtUV "the carrier of an identity type" tA
    y' <- check y tA
    tAQ <- quoteF tA
    pure (typeIdT x' (Just tAQ) y', universeValue)

  App f x -> do
    (f', fTy) <- inferAs universeValue f
    whnfV (stripRestrictions fTy) >>= \case
      NbE.VSuspended env (AnnSig info node@(TypeFunF orig md a mtope b)) -> do
        -- A hole argument to a shape-restricted function carries the shape
        -- as its goal: record (binder : a | tope) rather than just the cube.
        x' <- enterModality md $ case (x, mtope) of
          (Hole mname, Just _) -> do
            piQ <- quoteV (NbE.vsuspended env (AnnSig info node))
            case piQ of
              TypeFunT _ _ _ aQ (Just topeQ) _ -> checkHoleAgainstShape mname orig aQ topeQ
              _ -> panicImpossible "a Π value with a tope read back as something else"
          _ -> check x a
        whnfV a >>= \case
          NbE.VNode (AnnSig _ UniverseF) -> evalV x' >>= ensureTypeAtUV "a type argument"
          _ -> pure ()
        xV <- evalV x'
        bAt <- codomainAtV env b xV
        bAtQ <- quoteV bAt
        let result = appT bAtQ f' x'
        case b of
          ScopedAST _ UniverseTopeT{} ->
            case mtope of
              Nothing -> pure (result, bAt)
              Just tope -> do
                topeAt <- codomainAtV env tope xV >>= quoteV
                pure (topeAndT topeAt result, topeUniverseValue)
          _ -> do
            -- FIXME: need to check?
            forM_ mtope $ \tope -> do
              topeAt <- codomainAtV env tope xV >>= quoteV
              contextEntails topeAt
            pure (result, bAt)
      _ -> do
        fTyQ <- quoteV fTy
        issueTypeError $ TypeErrorNotFunction f' fTyQ

  Lambda _orig Nothing _body ->
    issueTypeError $ TypeErrorCannotInferBareLambda tt

  Lambda orig (Just (LambdaParam md ty Nothing)) body -> do
    (ty', tyK) <- enterModality md $ infer ty
    mcube <- whnfV tyK >>= \case
      -- an argument can be a type, including TOPE (a bare tope parameter)
      NbE.VNode (AnnSig _ UniverseF) -> pure Nothing
      -- an argument can be a cube
      NbE.VNode (AnnSig _ UniverseCubeF) -> pure Nothing
      -- an argument can be a shape
      NbE.VSuspended _ (AnnSig _ (TypeFunF _ _ cube _ (ScopedAST _ UniverseTopeT{}))) -> do
        mapM_ checkNameShadowing (binderLeaves orig)
        pure (Just cube)
      _ -> do
        kindQ <- quoteV tyK
        issueTypeError $ TypeErrorInvalidArgumentType ty kindQ
    mapM_ checkNameShadowing (binderLeaves orig)
    tyV <- evalV ty'
    let param = fromMaybe tyV mcube
    paramQ <- maybe (pure ty') quoteV mcube
    case mcube of
      Nothing -> do
        (body', ret) <- checkUnderV orig md param body $ \binder bodyTerm -> do
          (body', retV) <- infer bodyTerm
          retQ <- quoteF retV
          pure (ScopedAST binder body', ScopedAST binder retQ)
        let piT = typeFunT orig md paramQ Nothing ret
        piV <- evalV piT
        pure (lambdaT piT orig (Just (LambdaParam md paramQ Nothing)) body', piV)
      Just _ -> do
        (tope', body', ret) <- checkUnderV orig md param body $ \binder bodyTerm -> do
          -- eta expand the shape into a tope over the bound variable
          let etaTope = modalizeTope md $
                appT topeT (Foil.sink ty') (Var (Foil.nameOf binder))
          (body', retV) <- localTope etaTope $ infer bodyTerm
          retQ <- quoteF retV
          pure (ScopedAST binder etaTope, ScopedAST binder body', ScopedAST binder retQ)
        let piT = typeFunT orig md paramQ (Just tope') ret
        piV <- evalV piT
        pure (lambdaT piT orig (Just (LambdaParam md paramQ (Just tope'))) body', piV)

  Lambda orig (Just (LambdaParam md cube (Just tope))) body -> do
    cube' <- cubeUniverse >>= \cubes -> enterModality md $ check cube cubes
    mapM_ checkNameShadowing (binderLeaves orig)
    cubeV <- evalV cube'
    (tope', body', ret) <- checkUnderV orig md cubeV tope $ \binder topeTerm -> do
      (topeTyped, _) <- infer topeTerm
      bodyTerm <- openScoped binder body
      (body', retV) <- localTope topeTyped $ infer bodyTerm
      retQ <- quoteF retV
      pure (ScopedAST binder topeTyped, ScopedAST binder body', ScopedAST binder retQ)
    let piT = typeFunT orig md cube' (Just tope') ret
    piV <- evalV piT
    pure (lambdaT piT orig (Just (LambdaParam md cube' (Just tope'))) body', piV)

  Let orig annot val body -> do
    (val', bindTy) <- performing (ActionCheckLetValue (binderName orig)) $ case annot of
      Nothing -> infer val
      Just ty -> do
        bindType' <- check ty universeValue
        bindTyV <- evalV bindType'
        val' <- check val bindTyV
        pure (val', bindTyV)
    valV <- evalV val'
    bindTyQ <- quoteF bindTy
    (body', ret) <- checkUnderWithV orig Id bindTy (Just valV) body $ \binder bodyTerm -> do
      (body', retV) <- infer bodyTerm
      retQ <- quoteF retV
      pure (ScopedAST binder body', ScopedAST binder retQ)
    retAt <- codomainAtV Foil.identitySubst ret valV
    retAtQ <- quoteV retAt
    pure (letT retAtQ orig (Just bindTyQ) val' body', retAt)

  LetMod{} -> inferByTerms tt

  Refl Nothing -> issueTypeError $ TypeErrorCannotInferBareRefl tt
  Refl (Just (x, Nothing)) -> do
    (x', ty) <- inferAs universeValue x
    tyQ <- quoteF ty
    constant (reflT (typeIdT x' (Just tyQ) x') (Just (x', Just tyQ)))
  Refl (Just (x, Just ty)) -> do
    ty' <- check ty universeValue
    x' <- evalV ty' >>= check x
    constant (reflT (typeIdT x' (Just ty') x') (Just (x', Just ty')))

  IdJ tA a tC d x p -> do
    tA' <- check tA universeValue
    tAV <- evalV tA'
    a' <- check a tAV
    typeOf_C <- motiveType tA' a'
    tC' <- evalV typeOf_C >>= check tC
    univScope <- constScope universeT
    let typeOf_d =
          appT universeT
            (appT (typeFunT (BinderVar Nothing) Id (typeIdT a' (Just tA') a') Nothing univScope)
              tC' a')
            (reflT (typeIdT a' (Just tA') a') Nothing)
    d' <- evalV typeOf_d >>= check d
    x' <- check x tAV
    p' <- evalV (typeIdT a' (Just tA') x') >>= check p
    let ret =
          appT universeT
            (appT (typeFunT (BinderVar Nothing) Id (typeIdT a' (Just tA') x') Nothing univScope)
              tC' x')
            p'
    constant (idJT ret tA' a' tC' d' x' p')

  TypeAsc term ty -> do
    (ty', _) <- inferAs universeValue ty -- this works on types AND cubes
    tyV <- evalV ty'
    term' <- check term tyV
    pure (typeAscT term' ty', tyV)

  TypeRestricted ty rs -> do
    ty' <- check ty universeValue
    tyV <- evalV ty'
    rs' <- forM rs $ \(tope, term) -> do
      tope' <- check tope topeUniverseValue
      term' <- localTope tope' $ check term tyV
      pure (tope', term')
    sequence_ [ checkCoherence l r | l:rs'' <- tails rs', r <- rs'' ]
    pure (typeRestrictedT ty' rs', universeValue)

  TypeModal md ty -> do
    (ty', universeTy) <- enterModality md $ infer ty
    universeTyQ <- quoteF universeTy
    whnfV universeTy >>= \case
      NbE.VNode (AnnSig _ UniverseF)     -> pure ()
      NbE.VNode (AnnSig _ UniverseCubeF) -> pure ()
      NbE.VNode (AnnSig _ UniverseTopeF) -> pure ()
      _ -> issueTypeError $ TypeErrorNotTypeInModal md ty universeTyQ
    pure (typeModalT universeTyQ md ty', universeTy)

  ModApp md term -> do
    (term', ty) <- enterModality md $ infer term
    tyQ <- quoteF ty
    tyUniv <- typeOfV ty >>= quoteF
    constant (modAppT (typeModalT tyUniv md tyQ) md term')

  ModExtract{} -> panicImpossible "extract is an internal term and cannot be inferred"

  Match{} -> inferByTerms tt
  MatchArm{} -> panicImpossible "a match arm outside of a match branch"
  where
    -- @sup@ and @inf@: both points in one interval cube, a 2 beside an 𝕀
    -- coerced up to 𝕀.
    lattice what mk l r = do
      cubes <- cubeUniverse
      (l', lTy) <- inferAs cubes l
      (r', rTy) <- inferAs cubes r
      lK <- whnfV lTy
      rK <- whnfV rTy
      case (intervalOf lK, intervalOf rK) of
        (Interval2, Interval2) -> constant (mk cube2T l' r')
        (IntervalI, IntervalI) -> constant (mk cubeIT l' r')
        (IntervalI, Interval2) -> do
          r'' <- evalV cubeIT >>= check r
          constant (mk cubeIT l' r'')
        (Interval2, IntervalI) -> do
          l'' <- evalV cubeIT >>= check l
          constant (mk cubeIT l'' r')
        _ -> do
          lTyQ <- quoteV lTy
          rTyQ <- quoteV rTy
          issueTypeError $ TypeErrorNotIntervalCube what lTyQ rTyQ

-- | Infer a term and require its type to be of the given kind (@U@, @CUBE@).
inferAs :: Distinct n => Value n -> Term n -> TypeCheck n (TermT n, Value n)
inferAs expectedKind term = do
  (term', ty) <- infer term
  kind <- typeOfV ty
  tyQ <- quoteV ty
  subtypeV tyQ expectedKind kind
  pure (term', ty)

-- | Infer a cube point used by @≡@; a bare hole in lenient mode is recorded
-- with an as-yet-unknown cube as its goal.
inferPoint :: Distinct n => Term n -> TypeCheck n (TermT n, Value n)
inferPoint term@(Hole (Just name))
  | Just x <- unmarkUnresolved name =
      performing (ActionInfer term) $
        issueTypeError (TypeErrorUndefined x)
inferPoint term@(Hole mname) = performing (ActionInfer term) $ do
  reject <- asks ctxHolesAreErrors
  if reject
    then issueTypeError (TypeErrorCannotInferHole term)
    else do
      let cube = holeT cubeT Nothing
      recordHole mname cube
      (holeT cube mname,) <$> evalV cube
inferPoint term = cubeUniverse >>= \cubes -> inferAs cubes term

-- | Infer a point of one of the directed cubes, @2@ or @𝕀@, for @≤@.
inferDirectedPoint :: Distinct n => Term n -> TypeCheck n (TermT n, Value n)
inferDirectedPoint term = performing (ActionInfer term) $ do
  (point, ty) <- inferPoint term
  whnfV ty >>= \case
    NbE.VNode (AnnSig _ HoleF{}) -> pure (point, ty)
    NbE.VNode (AnnSig _ Cube2F)  -> pure (point, ty)
    NbE.VNode (AnnSig _ CubeIF)  -> pure (point, ty)
    _ -> do
      tyStr <- quoteV ty >>= ppInContext
      issueTypeError $ TypeErrorOther $
        "the (t ≤ s) tope expects a point in a directed cube (2 or 𝕀); got "
          <> tyStr
