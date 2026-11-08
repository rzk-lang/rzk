{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
-- | The typing rules: 'check' and 'infer' on raw terms, with types as
-- values, producing core terms.
--
-- The algorithm is Coquand's bidirectional one (/An algorithm for
-- type-checking dependent types/, 1996), in the form of free-foil-NBE's @DependentTypes.Elaborate@
-- (Renata Shakirova and Nikolai Kudasov): a λ is checked against a Π
-- value by entering its binder with the domain and checking the body
-- against the codomain at the bound names; an inferred type is compared
-- with the expected one by subtyping ("Rzk.Check.Conversion"). The rules
-- mirror those of "Rzk.TypeCheck.Judgements" case by case, on values.
--
-- The modal constructs and @match@ are not covered yet.
module Rzk.Check.Rules
  ( check
  , infer
  , inferAs
  , checkTopeAgainstContext
  , contextEntails
  , ensureTypeAtU
  , DomainKind (..)
  , classifyType
  , evalM
  , quoteM
  , underRawScope
  ) where

import           Control.Monad            (forM, forM_, unless, when)
import           Control.Monad.Foil       (DExt, Distinct)
import qualified Control.Monad.Foil       as Foil
import           Control.Monad.Free.Foil  (AST (..), ScopedAST (..),
                                           substitute)
import           Data.List                (tails)
import           Data.Maybe               (isNothing)

import qualified FreeFoil.NbE             as NbE

import           Language.Rzk.Foil.Names  (TModality (..), VarIdent,
                                           ppVarIdentWithLocation,
                                           unmarkUnresolved)
import           Rzk.Check.Context
import           Rzk.Check.Conversion
import           Rzk.Check.Display        (namingOfContext, ppName, ppValue, ppValueIn)
import           Rzk.Check.Error
import           Rzk.Check.Monad
import           Rzk.Check.Syntax
import           Rzk.Check.Topes
import           Rzk.Check.Value

-- * Values in the monad

evalM :: Distinct n => Core n -> Check n (Value n)
evalM t = asksContext $ \ctx -> evalCore (ctxScope ctx) ctx t
{-# INLINE evalM #-}

-- | A value read back, with defined names folded. Lazy: nothing is read
-- back until a consumer looks.
quoteM :: Distinct n => Value n -> Check n (Core n)
quoteM v = asksContext $ \ctx -> quoteFolded (ctxScope ctx) ctx v
{-# INLINE quoteM #-}

-- * Binders

-- | Enter a raw scope: refresh its pattern against the scope if its names
-- clash, extend the context with the pattern by the given function, and
-- run the continuation on the body.
enterRawScope
  :: forall n r. (Distinct n)
  => (forall l. (Distinct l, DExt n l) => Foil.Scope l -> Pattern n l -> Context n -> Context l)
  -> ScopedRaw n
  -> (forall l. (Distinct l, DExt n l) => Pattern n l -> Raw l -> Check l r)
  -> Check n r
enterRawScope extend (ScopedAST pat body) k = do
  warnShadowing pat
  ctx <- askContext
  let scope = ctxScope ctx
      enter :: forall l. DExt n l => Pattern n l -> Raw l -> Check n r
      enter pat' body' = case (Foil.assertDistinct pat', Foil.assertExt pat') of
        (Foil.Distinct, Foil.Ext) ->
          let scope' = Foil.extendScopePattern pat' scope
          in inContext (extend scope' pat' ctx) (k pat' body')
      fresh = all (\(_, x) -> not (Foil.member x scope)) (patternNames pat)
  if fresh
    then case (Foil.assertExt pat, Foil.assertDistinct pat) of
      (Foil.Ext, Foil.Distinct) -> enter pat body
    else Foil.withRefreshedPattern scope pat $ \extendSubst pat' scope' ->
      enter pat' (substitute scope' (extendSubst Foil.identitySubst) body)

-- | Enter a raw scope bound at a domain, with a modality and a shape.
underRawScope
  :: forall n r. (Distinct n)
  => TModality -> Value n -> Maybe (Value n) -> ScopedRaw n
  -> (forall l. (Distinct l, DExt n l) => Pattern n l -> Raw l -> Check l r)
  -> Check n r
underRawScope md dom mshape =
  enterRawScope (\scope' pat ctx -> underPattern scope' ctx md dom mshape pat)

-- | Check a λ parameter written with a shape, @(t : cube | shape)@, against
-- the expected domain and, if there is one, the expected shape: the cube
-- and the shape, as values.
checkShapedParam
  :: (Distinct n)
  => Value n -> Maybe (Value n) -> Raw n -> Raw n -> Check n (Value n, Value n)
checkShapedParam dom mexpectedShape param shape = do
  cube <- check param cubeUniverse
  cubeV <- evalM cube
  convertTypes dom cubeV
  shape' <- check shape (familyType Id cubeV Nothing)
  shapeV <- evalM shape'
  forM_ mexpectedShape $ \s -> convertTypes s shapeV
  pure (cubeV, shapeV)

-- | A tope family over a shape carries its domain: the body of the family
-- becomes the conjunction of the shape at the pattern and the body, unless
-- the body already entails the shape, with a warning.
carryFamilyDomain
  :: (DExt n l)
  => Pattern n l -> Value n -> Core l -> Check l (Core l)
carryFamilyDomain pat shape body = do
  ctx <- askContext
  let scope = ctxScope ctx
  bodyV <- evalM body
  let domainV = appValue scope ctx (Foil.sink shape) (patternValue pat)
      bodyT = topeOf scope ctx bodyV
      domainT = topeOf scope ctx domainV
  if topesEntail (ctxTopes ctx) [bodyT] domainT then pure body else do
    warn <- asksContext (optWarnTopeFamilyDomain . ctxOptions)
    when warn $ recordWarning $ \c ->
      TopeFamilyDomainWarning (ppValueIn c bodyV) (ppValueIn c domainV) (ctxLocation c)
    domainC <- quoteM domainV
    pure (TopeAnd domainC body)

-- | Warn about each name of a binder that shadows a name in scope, as the
-- old checker did with a trace.
warnShadowing :: Pattern n l -> Check n ()
warnShadowing pat = do
  ctx <- askContext
  sequence_
    [ recordWarning $ \c -> ShadowingWarning
        (ppVarIdentWithLocation x)
        [ ppVarIdentWithLocation previous | Just previous <- [entryName (lookupEntry v c)] ]
        (ctxLocation c)
    | (Just x, _) <- patternNames pat
    , Just v <- [lookupNamed x ctx] ]

-- | The type of a family over a cube: @{t : cube | shape} → TOPE@.
familyType :: TModality -> Value n -> Maybe (Value n) -> Value n
familyType md cube mshape = vnode (TypeShapeF md cube mshape)

-- | The shape of a binder written with a family as its type: the family,
-- conjoined with the shape of the family's own domain when it has one.
familyShape :: Distinct n => Foil.Scope n -> Core n -> Core n -> Maybe (Core n) -> Core n
familyShape scope cube family mdom = case mdom of
  Nothing -> family
  Just (Lambda _ (ScopedAST _ TopeTop)) -> family
  Just dom -> Foil.withFresh scope $ \b ->
    Lambda (Just (Param Id cube Nothing))
      (ScopedAST (singlePattern b)
        (TopeAnd (App (Foil.sink dom) (Var (Foil.nameOf b))) (App (Foil.sink family) (Var (Foil.nameOf b)))))

-- | A binder over a cube has a shape: ⊤ when none is written.
withTopShape :: Distinct n => Foil.Scope n -> Context n -> Value n -> Maybe (Value n) -> Maybe (Value n)
withTopShape scope ctx dom = \case
  Nothing | isCube scope ctx dom -> Just (topShapeValue scope dom)
  mshape -> mshape

-- * The layer discipline

-- | The kind of a type standing at @U@: the universes and the families
-- into them are @U@-sorted but are not ordinary types.
data DomainKind
  = DomainType
  | DomainCubeUniverse
  | DomainCubeFamily
  | DomainTopeUniverse
  | DomainTopeFamily
  | DomainOtherTopeFamily
  | DomainModalTope
  deriving (Eq, Show)

-- | Classify a type value.
classifyType :: Distinct n => Foil.Scope n -> Context n -> Value n -> DomainKind
classifyType scope ctx ty = case force (stripRestrictions ty) of
  VUniverseCube -> DomainCubeUniverse
  VUniverseTope -> DomainTopeUniverse
  VTypeShape _ _ _ -> DomainTopeFamily
  VTypeModal _ inner -> case classifyType scope ctx inner of
    DomainType -> DomainType
    _          -> DomainModalTope
  VTypeFun env md dom mshape ret ->
    let cubeDom = case mshape of
          Just _  -> True
          Nothing -> isCube scope ctx dom
        inner = underFreshVar scope ctx md dom mshape $ \scope' ctx' x ->
          classifyType scope' ctx' (codomainAt scope' ctx' (Foil.sink env) ret (NbE.vvar x))
    in case inner of
         DomainTopeUniverse | cubeDom -> DomainTopeFamily
                            | otherwise -> DomainOtherTopeFamily
         DomainTopeFamily | cubeDom -> DomainTopeFamily
                          | otherwise -> DomainOtherTopeFamily
         DomainOtherTopeFamily -> DomainOtherTopeFamily
         DomainModalTope -> DomainModalTope
         DomainCubeUniverse -> DomainCubeFamily
         DomainCubeFamily -> DomainCubeFamily
         _ -> DomainType
  _ -> DomainType

domainKindName :: DomainKind -> String
domainKindName = \case
  DomainType            -> "a type"
  DomainCubeUniverse    -> "the cube universe"
  DomainCubeFamily      -> "a cube family"
  DomainTopeUniverse    -> "the tope universe"
  DomainTopeFamily      -> "a tope family"
  DomainOtherTopeFamily -> "a tope family over a non-cube domain"
  DomainModalTope       -> "a modal cube or tope kind"

-- | Reject a universe kind standing where an ordinary type is required.
ensureTypeAtU :: Distinct n => String -> Value n -> Check n ()
ensureTypeAtU position ty = do
  ctx <- askContext
  case classifyType (ctxScope ctx) ctx ty of
    DomainType -> pure ()
    kind -> issueTypeError $ TypeErrorOther $
      position <> " must be a type, got " <> domainKindName kind

-- * Topes

-- | The tope context, for an error.
contextTopesM :: Check n [Tope]
contextTopesM = asksContext (accessibleTopes . ctxTopes)

-- | The context entails the tope, or the tope mentions a hole and cannot
-- be decided.
contextEntails :: Distinct n => Value n -> Check n ()
contextEntails topeV = do
  ctx <- askContext
  let tope = topeOf (ctxScope ctx) ctx topeV
  topes <- contextTopesM
  performing (ActionEntails topes tope) $
    unless (entails (ctxTopes ctx) tope || topeHasHole tope) $
      issueTypeError (TypeErrorTopeNotSatisfied topes tope)

-- | The context is covered by the disjunction of the topes: the coverage
-- obligation of a @recOR@.
contextEntailsUnion :: Distinct n => [Value n] -> Check n ()
contextEntailsUnion topeVs = do
  ctx <- askContext
  let topes = map (topeOf (ctxScope ctx) ctx) topeVs
  ctxTopes' <- contextTopesM
  performing (ActionEntailsUnion ctxTopes' topes) $
    unless (entailsUnion (ctxTopes ctx) topes || any topeHasHole topes) $
      issueTypeError (TypeErrorTopeNotSatisfied ctxTopes' (foldr topeOr TBot topes))

topeHasHole :: Tope -> Bool
topeHasHole = \case
  THole _ -> True
  TEq x y -> pointHasHole x || pointHasHole y
  TLeq x y -> pointHasHole x || pointHasHole y
  TAnd l r -> topeHasHole l || topeHasHole r
  TOr l r -> topeHasHole l || topeHasHole r
  TInv t -> topeHasHole t
  TUninv t -> topeHasHole t
  TModal _ t -> topeHasHole t
  TModApp _ t -> topeHasHole t
  TNeutral p -> pointHasHole p
  _ -> False

pointHasHole :: Point -> Bool
pointHasHole = \case
  PHole _ -> True
  PPair a b -> pointHasHole a || pointHasHole b
  PFst p -> pointHasHole p
  PSnd p -> pointHasHole p
  PSup a b -> pointHasHole a || pointHasHole b
  PInf a b -> pointHasHole a || pointHasHole b
  PFlip p -> pointHasHole p
  PUnflip p -> pointHasHole p
  PModApp _ p -> pointHasHole p
  PModExtract _ _ p -> pointHasHole p
  PApp _ f a -> pointHasHole f || pointHasHole a
  _ -> False

-- | Diagnose a @recOR@ guard or a restriction face against the context: a
-- tope disjoint from a consistent context is vacuous, an error; a tope
-- that overhangs the context is reported when the option asks.
checkTopeAgainstContext :: Distinct n => String -> Value n -> Check n ()
checkTopeAgainstContext what topeV = do
  ctx <- askContext
  let topes = ctxTopes ctx
      tope = topeOf (ctxScope ctx) ctx topeV
  unless (entailsBottom topes) $
    if disjoint topes tope && not (topeHasHole tope)
      then issueTypeError (TypeErrorTopeContextDisjoint tope (filter (/= TTop) (accessibleTopes topes)))
      else when (optWarnOverhang (ctxOptions ctx) && not (entailedBy topes tope)) $
        recordWarning $ \c ->
          let naming = namingOfContext c
          in OverhangWarning what (ppTope naming tope)
               (map (ppTope naming) (filter (/= TTop) (accessibleTopes topes))) (ctxLocation c)

-- | Two faces, or two branches, agree where their topes overlap.
checkCoherence :: Distinct n => (Value n, Value n) -> (Value n, Value n) -> Check n ()
checkCoherence (ltope, lterm) (rtope, rterm) = do
  ctx <- askContext
  let scope = ctxScope ctx
  underTope (topeOf scope ctx (topeAndValue ltope rtope)) $
    convertTerms lterm rterm

-- * Holes

-- | A hole in checking position: an error in strict mode; otherwise its
-- goal and context are recorded and it stands for a term of the goal.
checkHole :: Distinct n => Maybe VarIdent -> Value n -> Check n (Core n)
checkHole mname goal = do
  mode <- asksContext (optHoles . ctxOptions)
  case mode of
    HolesAreErrors -> issueTypeError (TypeErrorUnsolvedHole mname goal)
    _ -> do
      recordHole $ \ctx ->
        let naming = namingOfContext ctx
            scope = ctxScope ctx
            entry (x, e) = (ppName naming x, ppValueIn ctx (entryType e))
            locals = [ (x, e) | (x, e) <- entriesInScope ctx, entryKind e == EntryLocal ]
            isCubeEntry e = cubeOfType (entryType e) /= KOther
        in HoleInfo
          { holeName = mname
          , holeGoal = ppValue scope ctx goal
          , holeTermVars = [ entry xe | xe@(_, e) <- locals, not (isCubeEntry e) ]
          , holeCubeVars = [ entry xe | xe@(_, e) <- locals, isCubeEntry e ]
          , holeTopes = [ ppTope naming (tTope mt) | mt <- contextTopes (ctxTopes ctx), tTope mt /= TTop ]
          , holeLocation = ctxLocation ctx
          }
      pure (Hole mname)

-- * Checking

check :: forall n. Distinct n => Raw n -> Value n -> Check n (Core n)
check term ty = performing (ActionCheck term ty) $ case term of
  RHole (Just name) | Just x <- unmarkUnresolved name ->
    issueTypeError (TypeErrorUndefined x)
  RHole mname -> checkHole mname ty
  _ -> do
    ctx <- askContext
    let scope = ctxScope ctx
    case force ty of
      -- even under an absurd context the term must be well-formed
      VRecBottom -> do
        _ <- infer term
        pure RecBottom

      VTypeRestricted ty' rs -> case term of
        RRecOr branches -> checkRecOrAgainst ty branches
        _ -> do
          term' <- check term ty'
          termV <- evalM term'
          forM_ rs $ \(tope, face) -> do
            checkTopeAgainstContext "restriction face" tope
            underTope (topeOf scope ctx tope) $ convertTerms face termV
          pure term'

      tyW -> case term of
        -- a λ against a family type: its body is a tope under the binder,
        -- carrying the family's domain
        RLambda mparam body | Just (Family md' cube' mshape') <- familyOf (NbE.fromWhnf tyW) -> do
          mparam' <- forM mparam $ \(Param md param mshape) -> do
            when (md /= md') $ issueTypeError $ TypeErrorOther
              "modality mismatch in lambda parameter"
            cubeV <- case mshape of
              Nothing -> do
                (param', paramTy) <- infer param
                paramV <- evalM param'
                case familyOf paramTy of
                  Just (Family _ cube _) -> convertTypes cube' cube >> pure cube
                  Nothing -> convertTypes cube' paramV >> pure paramV
              Just shape -> fst <$> checkShapedParam cube' mshape' param shape
            pure (Param md cubeV mshape')
          let mshapeAssumed = maybe mshape' paramShape mparam'
          body' <- underRawScope md' cube' mshapeAssumed body $ \pat bodyTerm -> do
            bodyIn <- check bodyTerm topeUniverse
            bodyIn' <- case mshapeAssumed of
              Just shape -> carryFamilyDomain pat shape bodyIn
              Nothing -> pure bodyIn
            pure (ScopedAST pat bodyIn')
          cubeC <- quoteM cube'
          mshapeC <- traverse quoteM mshapeAssumed
          pure (Lambda (Just (Param md' cubeC mshapeC)) body')

        RLambda mparam body -> case piOf (NbE.fromWhnf tyW) of
          Just (Pi env md' dom' mshape' ret) -> do
            -- the λ's own parameter, if written, must agree with the Π's
            mparam' <- forM mparam $ \(Param md param mshape) -> do
              when (md /= md') $ issueTypeError $ TypeErrorOther
                "modality mismatch in lambda parameter"
              case mshape of
                Nothing -> do
                  (param', paramTy) <- infer param
                  paramV <- evalM param'
                  -- a shape written as the parameter type: the domain is its cube
                  case familyOf paramTy of
                    Just (Family _ cube mdomShape) -> do
                      convertTypes dom' cube
                      cubeC <- quoteM cube
                      mdomC <- traverse quoteM mdomShape
                      shapeV <- evalM (familyShape scope cubeC param' mdomC)
                      case mshape' of
                        Just shape' -> subtypeValues shape' shapeV
                        Nothing -> pure ()
                      pure (Param md cube (Just shapeV))
                    Nothing -> do
                      convertTypes dom' paramV
                      pure (Param md paramV (withTopShape scope ctx paramV Nothing))
                Just shape -> do
                  (cubeV, shapeV) <- checkShapedParam dom' mshape' param shape
                  pure (Param md cubeV (Just shapeV))
            let mshapeAssumed = case mparam' of
                  Just (Param _ _ (Just s)) -> Just s
                  _ -> mshape'
            body' <- underRawScope md' dom' mshapeAssumed body $ \pat bodyTerm -> do
              ctxIn <- askContext
              let scopeIn = ctxScope ctxIn
                  retIn = codomainAt scopeIn ctxIn (Foil.sink env) ret (patternValue pat)
              bodyIn <- check bodyTerm retIn
              -- A tope family checked against a shape domain carries the
              -- domain: its body becomes the conjunction, unless the body
              -- already entails the domain
              bodyIn' <- case (force retIn, mshapeAssumed) of
                (VUniverseTope, Just shape) -> carryFamilyDomain pat shape bodyIn
                _ -> pure bodyIn
              pure (ScopedAST pat bodyIn')
            domC <- quoteM dom'
            mshapeC <- traverse quoteM mshapeAssumed
            pure (Lambda (Just (Param md' domC mshapeC)) body')
          Nothing -> issueTypeError (TypeErrorUnexpectedLambda term ty)

        RLet mannot val body -> do
          (val', valTy) <- performing (ActionCheckLetValue (patternName body)) $ case mannot of
            Nothing -> infer val
            Just annot -> do
              annot' <- check annot universe
              annotV <- evalM annot'
              (, annotV) <$> check val annotV
          valV <- evalM val'
          body' <- underLet valTy valV body $ \pat bodyTerm -> do
            bodyIn <- check bodyTerm (Foil.sink ty)
            pure (ScopedAST pat bodyIn)
          valTyC <- quoteM valTy
          pure (Let (Just valTyC) val' body')

        RPair l r -> case tyW of
          VCubeProduct a b -> do
            l' <- check l a
            r' <- check r b
            pure (Pair l' r')
          _ | Just (Sigma env md a b) <- sigmaOf (NbE.fromWhnf tyW) -> do
            when (md /= Id) $ issueTypeError (TypeErrorUnsupported "a modal Σ type")
            l' <- check l a
            case force a of
              VUniverse -> evalM l' >>= ensureTypeAtU "a pair component at type U"
              _ -> pure ()
            lV <- evalM l'
            r' <- check r (codomainAt scope ctx env b lV)
            pure (Pair l' r')
          _ -> issueTypeError (TypeErrorUnexpectedPair term ty)

        RRefl mx -> case tyW of
          VTypeId y mtA z -> do
            tA <- case mtA of
              Just tA -> pure tA
              Nothing -> issueTypeError (TypeErrorOther "an identity type without its carrier")
            forM_ mx $ \(x, mxty) -> do
              forM_ mxty $ \xty -> do
                xty' <- check xty universe
                xtyV <- evalM xty'
                convertTypes tA xtyV
              x' <- check x tA
              xV <- evalM x'
              convertTerms xV y
              convertTerms xV z
            when (isNothing mx) $ convertTerms y z
            pure (Refl Nothing)
          _ -> issueTypeError (TypeErrorUnexpectedRefl term ty)

        RRecOr rs -> case tyW of
          VUniverseTope -> issueTypeError $ TypeErrorOther "a recOR cannot be used as a tope"
          _ -> checkRecOrAgainst ty rs

        RMatch{} -> issueTypeError (TypeErrorUnsupported "match")
        RLetMod{} -> issueTypeError (TypeErrorUnsupported "let mod")
        RModApp{} -> issueTypeError (TypeErrorUnsupported "mod")

        -- a neutral term is inferred, then its type compared with the
        -- expected one
        _ -> do
          (term', inferred) <- infer term
          subtype term' ty inferred
          pure term'

-- | The name a let binds, for the trace.
patternName :: ScopedRaw n -> Maybe VarIdent
patternName (ScopedAST pat _) = case pat of
  PatternVar x _ -> x
  _              -> Nothing

-- | Enter the scope of a let: its names have the components of the value.
underLet
  :: forall n r. (Distinct n)
  => Value n -> Value n -> ScopedRaw n
  -> (forall l. (Distinct l, DExt n l) => Pattern n l -> Raw l -> Check l r)
  -> Check n r
underLet ty val = enterRawScope $ \scope' pat ctx ->
  enterPattern pat
    (letEntries scope' (sinkContextUnchecked ctx) (ctxLocation ctx) (Foil.sink ty) (Foil.sink val) pat)
    ctx

-- | A @recOR@ against a known type: every branch is checked against it
-- under its guard, the branches cohere, and the guards cover the context.
checkRecOrAgainst :: Distinct n => Value n -> [(Raw n, Raw n)] -> Check n (Core n)
checkRecOrAgainst ty rs = do
  ctx <- askContext
  let scope = ctxScope ctx
  branches <- forM rs $ \(tope, branch) -> do
    tope' <- check tope topeUniverse
    topeV <- evalM tope'
    checkTopeAgainstContext "recOR branch guard" topeV
    branch' <- underTope (topeOf scope ctx topeV) $ check branch ty
    branchV <- evalM branch'
    pure ((tope', branch'), (topeV, branchV))
  sequence_ [ checkCoherence l r | l : rest <- tails (map snd branches), r <- rest ]
  contextEntailsUnion (map (fst . snd) branches)
  pure (RecOr (map fst branches))

-- * Inference

-- | Infer a term and compare its type's sort with the expected one.
inferAs :: Distinct n => Value n -> Raw n -> Check n (Core n, Value n)
inferAs expectedKind term = do
  (term', ty) <- infer term
  kind <- sortOf ty
  subtypeValues expectedKind kind
  pure (term', ty)

-- | The sort of a type value: the universe it lives in. The universes are
-- all @U@-sorted; a cube is in @CUBE@, a tope in @TOPE@; anything else is
-- a type, in @U@.
sortOf :: Distinct n => Value n -> Check n (Value n)
sortOf ty = do
  ctx <- askContext
  pure $ case force ty of
    VUniverse -> universe
    VUniverseCube -> universe
    VUniverseTope -> universe
    _ | cubeOfType ty /= KOther -> cubeUniverse
    _ -> case neutralType (ctxScope ctx) ctx ty of
      Just s -> stripRestrictions s
      Nothing -> universe

-- | Infer a cube point used by @≡@. A bare hole here has an unknown cube.
inferPoint :: Distinct n => Raw n -> Check n (Core n, Value n)
inferPoint term@((RHole (Just name)))
  | Just x <- unmarkUnresolved name = performing (ActionInfer term) $ issueTypeError (TypeErrorUndefined x)
inferPoint term@((RHole mname)) = performing (ActionInfer term) $ do
  mode <- asksContext (optHoles . ctxOptions)
  case mode of
    HolesAreErrors -> issueTypeError (TypeErrorCannotInferHole term)
    _ -> do
      let cube = vhole Nothing
      hole <- checkHole mname cube
      pure (hole, cube)
inferPoint term = inferAs cubeUniverse term

-- | Infer a point of a directed cube, @2@ or @𝕀@, for @≤@.
inferDirectedPoint :: Distinct n => Raw n -> Check n (Core n, Value n)
inferDirectedPoint term = performing (ActionInfer term) $ do
  (point, cube) <- inferPoint term
  case force cube of
    VHole _ -> pure (point, cube)
    VCube2 -> pure (point, cube)
    VCubeI -> pure (point, cube)
    _ -> do
      ctx <- askContext
      issueTypeError $ TypeErrorOther $
        "the (t ≤ s) tope expects a point in a directed cube (2 or 𝕀); got " <> ppValueIn ctx cube

infer :: forall n. Distinct n => Raw n -> Check n (Core n, Value n)
infer tt = performing (ActionInfer tt) $ case tt of
  RHole (Just name) | Just x <- unmarkUnresolved name ->
    issueTypeError (TypeErrorUndefined x)
  RHole _ -> issueTypeError (TypeErrorCannotInferHole tt)
  Var x -> do
    entry <- asksContext (lookupEntry x)
    pure (Var x, entryType entry)

  RUniverse -> pure (Universe, universe)
  RUniverseCube -> pure (UniverseCube, universe)
  RUniverseTope -> pure (UniverseTope, universe)

  RCubeUnit -> pure (CubeUnit, cubeUniverse)
  RCubeUnitStar -> pure (CubeUnitStar, vnode CubeUnitF)
  RCube2 -> pure (Cube2, cubeUniverse)
  RCube2_0 -> pure (Cube2_0, vcube2)
  RCube2_1 -> pure (Cube2_1, vcube2)
  RCubeI -> pure (CubeI, cubeUniverse)
  RCubeI_0 -> pure (CubeI_0, vcubeI)
  RCubeI_1 -> pure (CubeI_1, vcubeI)
  RCubeProduct l r -> do
    l' <- check l cubeUniverse
    r' <- check r cubeUniverse
    pure (CubeProduct l' r', cubeUniverse)

  RCubeFlip{} -> issueTypeError (TypeErrorUnsupported "flip")
  RCubeUnflip{} -> issueTypeError (TypeErrorUnsupported "unflip")

  RCubeSup l r -> lattice "sup" CubeSup l r
  RCubeInf l r -> lattice "inf" CubeInf l r

  RPair l r -> do
    (l', lt) <- infer l
    (r', rt) <- infer r
    ltSort <- sortOf lt
    case force ltSort of
      VUniverseCube -> pure (Pair l' r', vnode (CubeProductF lt rt))
      _ -> do
        -- a non-dependent pair
        ctx <- askContext
        rtC <- quoteM rt
        let sigma = Foil.withFresh (ctxScope ctx) $ \b ->
              NbE.vsuspended Foil.identitySubst
                (TypeSigmaF Id lt (ScopedAST (singlePattern b) (Foil.sink rtC)))
        pure (Pair l' r', sigma)

  RFirst t -> do
    (t', ty) <- infer t
    case force (stripRestrictions ty) of
      VRecBottom -> pure (First t', vrecBottom)
      VCubeProduct a _ -> pure (First t', a)
      _ | Just (Sigma _ _ a _) <- sigmaOf ty -> pure (First t', a)
      _ -> issueTypeError (TypeErrorNotPair t' ty)

  RSecond t -> do
    (t', ty) <- infer t
    ctx <- askContext
    let scope = ctxScope ctx
    case force (stripRestrictions ty) of
      VRecBottom -> pure (Second t', vrecBottom)
      VCubeProduct _ b -> pure (Second t', b)
      _ | Just (Sigma env _ _ b) <- sigmaOf ty -> do
        tV <- evalM t'
        pure (Second t', codomainAt scope ctx env b (firstValue tV))
      _ -> issueTypeError (TypeErrorNotPair t' ty)

  RTypeUnit -> pure (TypeUnit, universe)
  RUnit -> pure (Unit, vtypeUnit)

  RTopeTop -> pure (TopeTop, topeUniverse)
  RTopeBottom -> pure (TopeBottom, topeUniverse)

  RTopeEQ l r -> do
    (l', lt) <- inferPoint l
    r' <- check r lt
    pure (TopeEQ l' r', topeUniverse)

  RTopeLEQ l r -> do
    (l', lTy) <- inferDirectedPoint l
    (r', rTy) <- inferDirectedPoint r
    case (force lTy, force rTy) of
      ((VHole _), _) -> pure (TopeLEQ l' r', topeUniverse)
      (_, (VHole _)) -> pure (TopeLEQ l' r', topeUniverse)
      (VCube2, VCube2) -> pure (TopeLEQ l' r', topeUniverse)
      (VCubeI, VCubeI) -> pure (TopeLEQ l' r', topeUniverse)
      (VCubeI, VCube2) -> do
        r'' <- check r vcubeI
        pure (TopeLEQ l' r'', topeUniverse)
      (VCube2, VCubeI) -> do
        l'' <- check l vcubeI
        pure (TopeLEQ l'' r', topeUniverse)
      _ -> do
        ctx <- askContext
        issueTypeError $ TypeErrorOther $
          "the (t ≤ s) tope expects points in interval cubes (2 or 𝕀); got "
            <> ppValueIn ctx lTy <> " and " <> ppValueIn ctx rTy

  RTopeAnd l r -> do
    l' <- check l topeUniverse
    r' <- check r topeUniverse
    pure (TopeAnd l' r', topeUniverse)

  RTopeOr l r -> do
    l' <- check l topeUniverse
    r' <- check r topeUniverse
    pure (TopeOr l' r', topeUniverse)

  RTopeInv{} -> issueTypeError (TypeErrorUnsupported "a tope inversion")
  RTopeUninv{} -> issueTypeError (TypeErrorUnsupported "a tope inversion")

  RRecBottom -> do
    contextEntails vtopeBottom
    pure (RecBottom, vrecBottom)

  RRecOr rs -> do
    ctx <- askContext
    let scope = ctxScope ctx
    branches <- forM rs $ \(tope, branch) -> do
      tope' <- check tope topeUniverse
      topeV <- evalM tope'
      checkTopeAgainstContext "recOR branch guard" topeV
      (branch', ty) <- underTope (topeOf scope ctx topeV) $ inferAs universe branch
      branchV <- evalM branch'
      pure ((tope', branch'), (topeV, branchV), (topeV, ty))
    sequence_ [ checkCoherence l r | l : rest <- tails [ b | (_, b, _) <- branches ], r <- rest ]
    contextEntailsUnion [ t | (_, (t, _), _) <- branches ]
    pure (RecOr [ b | (b, _, _) <- branches ], vnode (RecOrF [ t | (_, _, t) <- branches ]))

  RTypeFun md a Nothing b -> do
    when (md /= Id) $ issueTypeError (TypeErrorUnsupported "a modal binder")
    (a', aTy) <- infer a
    aV <- evalM a'
    ctx <- askContext
    let scope = ctxScope ctx
    case force aTy of
      -- a type, including TOPE: a bare tope parameter
      VUniverse -> do
        b' <- underRawScope md aV Nothing b $ \pat bTerm -> do
          bIn <- check bTerm universe
          pure (ScopedAST pat bIn)
        pure (TypeFun md a' Nothing b', universe)
      -- a cube: the shape is ⊤
      VUniverseCube -> do
        let shapeV = topShapeValue scope aV
        shapeC <- quoteM shapeV
        piOrFamily md a' shapeC aV shapeV b
      -- a shape: a tope family over a cube
      _ | Just (Family _ cube mdomShape) <- familyOf aTy -> do
          cubeC <- quoteM cube
          mdomC <- traverse quoteM mdomShape
          let shapeC = familyShape scope cubeC a' mdomC
          shapeV <- evalM shapeC
          piOrFamily md cubeC shapeC cube shapeV b
      _ -> issueTypeError (TypeErrorInvalidArgumentType a aTy)

  RTypeFun md cube (Just shape) ret -> do
    when (md /= Id) $ issueTypeError (TypeErrorUnsupported "a modal binder")
    cube' <- check cube cubeUniverse
    cubeV <- evalM cube'
    shape' <- check shape (familyType Id cubeV Nothing)
    shapeV <- evalM shape'
    piOrFamily md cube' shape' cubeV shapeV ret

  RTypeSigma md a b -> do
    when (md /= Id) $ issueTypeError (TypeErrorUnsupported "a modal Σ type")
    a' <- check a universe
    aV <- evalM a'
    ensureTypeAtU "a Σ component" aV
    b' <- underRawScope md aV Nothing b $ \pat bTerm -> do
      bIn <- check bTerm universe
      evalM bIn >>= ensureTypeAtU "a Σ component"
      pure (ScopedAST pat bIn)
    pure (TypeSigma md a' b', universe)

  RTypeId x (Just tA) y -> do
    tA' <- check tA universe
    tAV <- evalM tA'
    ensureTypeAtU "the carrier of an identity type" tAV
    x' <- check x tAV
    y' <- check y tAV
    pure (TypeId x' (Just tA') y', universe)

  RTypeId x Nothing y -> do
    (x', tAV) <- inferAs universe x
    ensureTypeAtU "the carrier of an identity type" tAV
    y' <- check y tAV
    tA' <- quoteM tAV
    pure (TypeId x' (Just tA') y', universe)

  RApp f x -> do
    (f', fTy) <- inferAs universe f
    ctx <- askContext
    let scope = ctxScope ctx
    case piOf fTy of
      _ | Just (Family md cube _) <- familyOf fTy -> do
        when (md /= Id) $ issueTypeError (TypeErrorUnsupported "a modal function")
        x' <- check x cube
        pure (App f' x', topeUniverse)
      Just (Pi env md a mshape b) -> do
        when (md /= Id) $ issueTypeError (TypeErrorUnsupported "a modal function")
        x' <- check x a
        xV <- evalM x'
        case force a of
          VUniverse -> ensureTypeAtU "a type argument" xV
          _ -> pure ()
        forM_ mshape $ \shape -> contextEntails (appValue scope ctx shape xV)
        pure (App f' x', codomainAt scope ctx env b xV)
      Nothing -> issueTypeError (TypeErrorNotFunction f' fTy)

  RLambda Nothing _ -> issueTypeError (TypeErrorCannotInferBareLambda tt)

  RLambda (Just (Param md ty Nothing)) body -> do
    when (md /= Id) $ issueTypeError (TypeErrorUnsupported "a modal binder")
    (ty', tyTy) <- infer ty
    tyV <- evalM ty'
    ctx <- askContext
    let scope = ctxScope ctx
    (dom, mshape) <- case force tyTy of
      VUniverse -> pure (tyV, Nothing)
      VUniverseCube -> pure (tyV, Just (topShapeValue scope tyV))
      _ | Just (Family _ cube mdomShape) <- familyOf tyTy -> do
          cubeC <- quoteM cube
          mdomC <- traverse quoteM mdomShape
          shapeV <- evalM (familyShape scope cubeC ty' mdomC)
          pure (cube, Just shapeV)
      _ -> issueTypeError (TypeErrorInvalidArgumentType ty tyTy)
    inferLambda md dom mshape body

  RLambda (Just (Param md cube (Just shape))) body -> do
    when (md /= Id) $ issueTypeError (TypeErrorUnsupported "a modal binder")
    cube' <- check cube cubeUniverse
    cubeV <- evalM cube'
    shape' <- check shape (familyType Id cubeV Nothing)
    shapeV <- evalM shape'
    inferLambda md cubeV (Just shapeV) body

  RLet mannot val body -> do
    (val', valTy) <- performing (ActionCheckLetValue (patternName body)) $ case mannot of
      Nothing -> infer val
      Just annot -> do
        annot' <- check annot universe
        annotV <- evalM annot'
        (, annotV) <$> check val annotV
    valV <- evalM val'
    valTyC <- quoteM valTy
    (body', retScoped) <- underLet valTy valV body $ \pat bodyTerm -> do
      (bodyIn, retIn) <- infer bodyTerm
      retC <- quoteM retIn
      pure (ScopedAST pat bodyIn, ScopedAST pat retC)
    ctx <- askContext
    let ret = evalScope (ctxScope ctx) ctx Foil.identitySubst retScoped valV
    pure (Let (Just valTyC) val' body', ret)

  RLetMod{} -> issueTypeError (TypeErrorUnsupported "let mod")

  RRefl Nothing -> issueTypeError (TypeErrorCannotInferBareRefl tt)
  RRefl (Just (x, Nothing)) -> do
    (x', ty) <- inferAs universe x
    xV <- evalM x'
    tyC <- quoteM ty
    pure (Refl (Just (x', Just tyC)), vnode (TypeIdF xV (Just ty) xV))
  RRefl (Just (x, Just ty)) -> do
    ty' <- check ty universe
    tyV <- evalM ty'
    x' <- check x tyV
    xV <- evalM x'
    pure (Refl (Just (x', Just ty')), vnode (TypeIdF xV (Just tyV) xV))

  RIdJ tA a tC d x p -> do
    tA' <- check tA universe
    tAV <- evalM tA'
    a' <- check a tAV
    aV <- evalM a'
    ctx <- askContext
    let scope = ctxScope ctx
        -- (z : A) → (a =_A z) → U
        motiveTy = Foil.withFresh scope $ \zB ->
          let z = Foil.nameOf zB
              scopeZ = Foil.extendScope zB scope
              idType = TypeId (Foil.sink a') (Just (Foil.sink tA')) (Var z)
          in Foil.withFresh scopeZ $ \pB ->
               NbE.vsuspended Foil.identitySubst
                 (TypeFunF Id tAV Nothing
                   (ScopedAST (singlePattern zB)
                     (TypeFun Id idType Nothing (ScopedAST (singlePattern pB) Universe))))
    tC' <- check tC motiveTy
    tCV <- evalM tC'
    let reflAt = vnode (ReflF Nothing)
        typeOfD = appValue scope ctx (appValue scope ctx tCV aV) reflAt
    d' <- check d typeOfD
    x' <- check x tAV
    xV <- evalM x'
    p' <- check p (vnode (TypeIdF aV (Just tAV) xV))
    pV <- evalM p'
    pure (IdJ tA' a' tC' d' x' p', appValue scope ctx (appValue scope ctx tCV xV) pV)

  -- an annotation is built by evaluation and never written
  RAnn term _ -> infer term

  RTypeAsc term ty -> do
    (ty', _) <- inferAs universe ty
    tyV <- evalM ty'
    term' <- check term tyV
    pure (TypeAsc term' ty', tyV)

  RTypeRestricted ty rs -> do
    ty' <- check ty universe
    tyV <- evalM ty'
    ctx <- askContext
    let scope = ctxScope ctx
    rs' <- forM rs $ \(tope, term) -> do
      tope' <- check tope topeUniverse
      topeV <- evalM tope'
      term' <- underTope (topeOf scope ctx topeV) $ check term tyV
      termV <- evalM term'
      pure ((tope', term'), (topeV, termV))
    sequence_ [ checkCoherence l r | l : rest <- tails (map snd rs'), r <- rest ]
    pure (TypeRestricted ty' (map fst rs'), universe)

  RTypeModal{} -> issueTypeError (TypeErrorUnsupported "a modal type")
  RModApp{} -> issueTypeError (TypeErrorUnsupported "mod")
  RModExtract{} -> issueTypeError (TypeErrorUnsupported "extract")
  RMatch{} -> issueTypeError (TypeErrorUnsupported "match")
  RMatchArm{} -> issueTypeError (TypeErrorUnsupported "a match arm")
  RTypeShape{} -> panicImpossible "a family type in a raw term"
  where
    lattice :: String -> (Core n -> Core n -> Core n) -> Raw n -> Raw n -> Check n (Core n, Value n)
    lattice name mk l r = do
      (l', lTy) <- inferAs cubeUniverse l
      (r', rTy) <- inferAs cubeUniverse r
      case (force lTy, force rTy) of
        (VCube2, VCube2) -> pure (mk l' r', vcube2)
        (VCubeI, VCubeI) -> pure (mk l' r', vcubeI)
        (VCubeI, VCube2) -> do
          r'' <- check r vcubeI
          pure (mk l' r'', vcubeI)
        (VCube2, VCubeI) -> do
          l'' <- check l vcubeI
          pure (mk l'' r', vcubeI)
        _ -> issueTypeError (TypeErrorNotIntervalCube name lTy rTy)

-- | A Π over a cube with a shape, or, when its codomain is @TOPE@, the
-- type of a family over that shape.
piOrFamily
  :: (Distinct n)
  => TModality -> Core n -> Core n -> Value n -> Value n -> ScopedRaw n -> Check n (Core n, Value n)
piOrFamily md cubeC shapeC cubeV shapeV ret = do
  (ret', isTope) <- underRawScope md cubeV (Just shapeV) ret $ \pat retTerm -> do
    retIn <- check retTerm universe
    retV <- evalM retIn
    let isTope = case force retV of
          VUniverseTope -> True
          _ -> False
    pure (ScopedAST pat retIn, isTope)
  if isTope
    then pure (TypeShape md cubeC (Just shapeC), universe)
    else pure (TypeFun md cubeC (Just shapeC) ret', universe)

-- | Infer a λ whose parameter is known: the body is inferred under it, and
-- the λ's type is the Π over the pattern with the body's type as codomain.
inferLambda
  :: (Distinct n)
  => TModality -> Value n -> Maybe (Value n) -> ScopedRaw n -> Check n (Core n, Value n)
inferLambda md dom mshape body = do
  (body', retScoped, isTope) <- underRawScope md dom mshape body $ \pat bodyTerm -> do
    (bodyIn, retIn) <- infer bodyTerm
    retC <- quoteM retIn
    let isTope = case force retIn of
          VUniverseTope -> True
          _ -> False
    pure (ScopedAST pat bodyIn, ScopedAST pat retC, isTope)
  domC <- quoteM dom
  mshapeC <- traverse quoteM mshape
  let ty | isTope, Just _ <- mshape = familyType md dom mshape
         | otherwise = NbE.vsuspended Foil.identitySubst (TypeFunF md dom mshape retScoped)
  pure (Lambda (Just (Param md domC mshapeC)) body', ty)
