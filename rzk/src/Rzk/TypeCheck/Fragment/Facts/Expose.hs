{-# LANGUAGE LambdaCase #-}

-- | What the evaluator would say of a term, read off its facts: what
-- @fragmentHead@ and @whnfT@ expose it to, whether it is a meta type, lands
-- in a universe or is a cube, and which cube points it reaches. Facts from a
-- table's body at a use are settled against the use's arguments here, as far
-- as the answers to these questions go; the checks settle the rest.
module Rzk.TypeCheck.Fragment.Facts.Expose (
  -- * Limits
  exposeLimit,
  schematicDepthLimit,
  -- * Derived facts
  derivedOf,
  restrictedOf,
  domainMetaOf,
  -- * Building facts from facts
  appFacts,
  applyOps,
  scopeOp,
  scopedFacts,
  underFrames,
  frameArg,
  -- * Function types
  functionHeadOf,
  DomainOf (..),
  FoundDomain (..),
  domainOf,
  scopedSym,
  -- * Deciding symbolic facts
  decide,
  guardOf,
  noPointsBut,
  -- * Settling needs
  settleWith,
  evalReqWith,
  resolveAnswers,
  resolveSym,
  -- * Helpers
  isRestrictedShape,
  stripRestrictions,
) where

import qualified Data.IntSet                        as IntSet
import           Data.List                          (find, partition)
import           Data.Maybe                         (listToMaybe)
import qualified Data.Set                           as Set

import           Rzk.TypeCheck.Fragment.Facts.Types

-- * Limits

-- | The reduction limit of one exposure for the fragment certificate. The
-- standalone walk gives up when its fuel (at least 'defaultRSTTFuel') runs
-- out; the step counts here are upper bounds, so a margin is kept.
exposeLimit :: Int
exposeLimit = 200

-- | The depth of terms and of exposures up to which the schematic certificate
-- stands in for the standalone check, whose fuel (at least
-- 'defaultRSTTFuel') is spent by nesting and by unfolding; beyond it the
-- standalone check runs.
schematicDepthLimit :: Int
schematicDepthLimit = 60

-- * Derived facts

-- | The equation of 'fDerived'.
derivedOf :: Facts -> Derived
derivedOf f = Derived
  { dExpose = exposeOf f
  , dHeadView = headViewOf f
  , dMeta = metaOf f
  , dLands = landsOf f
  , dCube = cubeOf f
  , dPoints = pointsOf f
  }

-- | What @fragmentHead@ exposes a term to.
exposeOf :: Facts -> Exposed
exposeOf f = case fShape f of
  SVar v -> case vfValue v of
    Just val -> later 1 (fExpose val)
    Nothing  -> neutralOf 0 f
  SParam p -> Exposed 0 [] (ToParam p)
  SParamType _ _ -> ExposedUnknown "the type of an argument"
  SApp{} -> exposeApp f
  SLet l -> later 1 (fExpose (letBody l))
  STypeAsc v _ -> later 1 (fExpose v)
  SFirst p -> projection fst OpFirst p
  SSecond p -> projection snd OpSecond p
  SIdJ j -> case fExpose (jProof j) of
    Exposed k ns (ToFacts _ proof)
      | SRefl{} <- fShape proof -> underConditions ns (later (k + 1) (fExpose (jBase j)))
    Exposed k ns (ToFacts Itself _) -> underConditions ns (neutralOf k f)
    Exposed _ _ (ToFacts Reduced _) -> ExposedUnknown "an elimination of a reducible proof"
    ExposedUnknown r -> ExposedUnknown r
    Exposed{} -> ExposedUnknown "an elimination of an argument"
  SRecOr _ -> ExposedUnknown "a case split on topes"
  SScoped frame g -> scopedExposure frame (fExpose g)
  SOther r -> ExposedUnknown r
  _ -> exposedTo f
  where
    projection component op p = case fExpose p of
      Exposed k ns (ToFacts _ pair)
        | SPair a b <- fShape pair -> underConditions ns (later (k + 1) (fExpose (component (a, b))))
      Exposed k ns (ToFacts Itself _) -> underConditions ns (neutralOf k f)
      Exposed _ _ (ToFacts Reduced _) -> ExposedUnknown "a projection of a reducible pair"
      -- A pair-valued definition: the component lives in its body.
      Exposed k ns (ToTable frame) -> inBody k ns frame (fExpose (tbBody (frameTable frame)))
      Exposed k ns (InTable frame pair) -> inBody k ns frame (fExpose pair)
      -- A projection of an argument: the use settles it.
      Exposed k ns (ToParam prm) -> Exposed (k + 1) ns (ToParamApp prm [op])
      Exposed k ns (ToParamApp prm ops) -> Exposed (k + 1) ns (ToParamApp prm (ops <> [op]))
      Exposed _ _ (ToParamType _) -> ExposedUnknown "a projection of an argument"
      ExposedUnknown r -> ExposedUnknown r
      where
        inBody k ns frame = \case
          Exposed k' ns' (ToFacts _ pair) | SPair a b <- fShape pair ->
            underConditions ns (Exposed (k + k' + 1) ns' (InTable frame (component (a, b))))
          _ -> ExposedUnknown "a projection of a definition's body"

-- | A stuck term exposes to itself, unless its type is a restricted type whose
-- face holds; the walk exposes the type to find out, and asks the solver.
neutralOf :: Int -> Facts -> Exposed
neutralOf k f = case fExpose (fType f) of
  Exposed k' ns (ToFacts _ ty)
    | isRestrictedShape ty -> ExposedUnknown "a restricted type to try"
    | otherwise -> itself k' ns
  Exposed k' ns (ToTable frame) -> restricted k' ns (resolveSym frame (restrictedOf (tbBody (frameTable frame))))
  Exposed k' ns (InTable frame ty) -> restricted k' ns (resolveSym frame (restrictedOf ty))
  Exposed k' ns (ToParam p) -> itself k' (ns <> [Need p (ReqAnswer [] QRestricted False)])
  Exposed k' ns (ToParamApp p ops) -> itself k' (ns <> [Need p (ReqAnswer ops QRestricted False)])
  Exposed _ _ (ToParamType _) -> ExposedUnknown "a type that is an argument's type"
  ExposedUnknown r -> ExposedUnknown r
  where
    itself k' ns = Exposed (k + k') ns (ToFacts Itself f)
    restricted k' ns = \case
      SymBool ns' (Known False) -> itself k' (ns <> ns')
      SymBool _ (Known True) -> ExposedUnknown "a restricted type to try"
      SymBool ns' (AnyOf qs) -> itself k' (ns <> ns' <> [guardOf q False | q <- qs])
      SymBool _ (Unknown r) -> ExposedUnknown r

-- | What an application exposes to: a definition's table if the definition
-- is applied to all its parameters, β-reduction otherwise.
exposeApp :: Facts -> Exposed
exposeApp f = case fShape h of
  SVar v
    | Just tl <- vfTop v, Just tb <- tlTable tl, Just val <- vfValue v ->
        if n == tbArity tb
          then saturated (Frame tb args)
          -- Fewer or more arguments than parameters: unfold and β-reduce
          -- through the value's own lambdas.
          else applyExposed (later 1 (fExpose val)) args
    | Nothing <- vfValue v -> neutralOf 0 f
  _ -> case fExpose h of
    -- A lambda reduces; any other stuck head leaves the application as it is.
    Exposed _ _ (ToFacts _ g) | SLam{} <- fShape g -> applyExposed (fExpose h) args
    Exposed k ns (ToFacts Itself _) -> underConditions ns (neutralOf k f)
    e -> applyExposed e args
  where
    (h, pairs) = spine f
    args = map snd pairs
    n = length args

    saturated frame = case fExpose (tbBody (frameTable frame)) of
      -- An alias of a parameter exposes to the argument, under the body's
      -- conditions, which the arguments settle.
      Exposed k ns (ToParam p) -> case frameArg frame (paramIndex p) of
        Nothing -> ExposedUnknown "an argument out of range"
        Just arg -> case resolveAnswers frame (needs ns) of
          Clean -> later (1 + n + k) (fExpose arg)
          Needs ns' -> underConditions ns' (later (1 + n + k) (fExpose arg))
          Unclean r -> ExposedUnknown r
      ExposedUnknown r -> ExposedUnknown r
      e -> Exposed (1 + n + exposeSteps e) [] (ToTable frame)

-- | Apply an exposed function to arguments: β through lambdas, and a stuck
-- head stays stuck.
applyExposed :: Exposed -> [Facts] -> Exposed
applyExposed e [] = e
applyExposed e as@(x : xs) = case e of
  Exposed k ns (ToFacts _ g) | SLam l <- fShape g ->
    applyExposed (underConditions ns (later (k + 1) (fExpose (lamOpen l x)))) xs
  Exposed k ns (ToFacts Itself g) -> underConditions ns (neutralOf k (foldl' appFacts g as))
  Exposed _ _ (ToFacts Reduced _) -> ExposedUnknown "an application of a reducible head"
  Exposed k ns (ToParam p) -> Exposed k ns (ToParamApp p (map OpApp as))
  Exposed k ns (ToParamApp p ops) -> Exposed k ns (ToParamApp p (ops <> map OpApp as))
  Exposed _ _ (ToTable _) -> ExposedUnknown "an over-applied definition"
  Exposed _ _ (InTable _ _) -> ExposedUnknown "an applied component of a definition"
  Exposed _ _ (ToParamType _) -> ExposedUnknown "an applied type"
  ExposedUnknown r -> ExposedUnknown r

-- | What @whnfT@ followed by @stripTypeRestrictions@ exposes a term to.
headViewOf :: Facts -> Exposed
headViewOf f0 = go (fExpose (stripRestrictions f0))
  where
    go = \case
      Exposed k ns (ToFacts origin g)
        | SRestricted base _ <- fShape g ->
            underConditions ns (later k (go (fExpose (stripRestrictions base))))
        | origin == Itself, eliminatorHeaded g -> ExposedUnknown "a data eliminator"
        | otherwise -> Exposed k ns (ToFacts origin g)
      Exposed k ns (InTable frame g) ->
        underConditions ns (later k (scopedExposure frame (headViewOf g)))
      e -> e

    -- whnfT reduces a data eliminator applied to a constructor; fragmentHead
    -- does not.
    eliminatorHeaded g = case fShape (fst (spine g)) of
      SVar v | Just tl <- vfTop v -> tlDataRole tl
      _ -> False

-- | A fact read off what a term exposes to: of the facts it exposes to, of a
-- table's body, or a query on a parameter's argument.
ofExposure
  :: (Query, Query) -> Exposed -> (Facts -> SymBool) -> (Facts -> SymBool) -> SymBool
ofExposure (q, qType) e onBody onShape = case e of
  Exposed _ ns (ToFacts _ g) -> withConditions ns (onShape g)
  Exposed _ ns (ToTable frame) -> withConditions ns (resolveSym frame (onBody (tbBody (frameTable frame))))
  Exposed _ ns (InTable frame g) -> withConditions ns (resolveSym frame (onShape g))
  Exposed _ ns (ToParam p) -> SymBool ns (AnyOf [ParamQuery p [] q []])
  Exposed _ ns (ToParamType p) -> SymBool ns (AnyOf [ParamQuery p [] qType []])
  Exposed _ ns (ToParamApp p ops) -> SymBool ns (AnyOf [ParamQuery p ops q []])
  ExposedUnknown r -> unknown r

-- | @isMetaType@: a universe, or a function type into or over one.
metaOf :: Facts -> SymBool
metaOf f = ofExposure (QMeta, QTypeMeta) (fHeadView f) metaOf $ \g -> case fShape g of
  SUniverse -> known True
  SCubeU -> known True
  STopeU -> known True
  SPi p -> metaOf (piDomain p) `orSym` metaOf (piCodomain p)
  _ -> known False

-- | Read as a Π-chain, the type ends in a universe.
landsOf :: Facts -> SymBool
landsOf f = ofExposure (QLands, QTypeLands) (fHeadView f) landsOf $ \g -> case fShape g of
  SUniverse -> known True
  SCubeU -> known True
  STopeU -> known True
  SPi p -> landsOf (piCodomain p)
  _ -> known False

-- | The type is @CUBE@.
cubeOf :: Facts -> SymBool
cubeOf f = ofExposure (QCube, QTypeCube) (fHeadView f) cubeOf $ \g -> case fShape g of
  SCubeU -> known True
  _ -> known False

-- | @fragmentHead@ exposes the term to a restricted type.
restrictedOf :: Facts -> SymBool
restrictedOf f = case fExpose f of
  Exposed _ ns (ToFacts _ g) -> SymBool ns (Known (isRestrictedShape g))
  Exposed _ ns (ToTable frame) -> withConditions ns (resolveSym frame (restrictedOf (tbBody (frameTable frame))))
  Exposed _ ns (InTable frame g) -> withConditions ns (resolveSym frame (restrictedOf g))
  Exposed _ ns (ToParam p) -> SymBool ns (AnyOf [ParamQuery p [] QRestricted []])
  Exposed _ ns (ToParamApp p ops) -> SymBool ns (AnyOf [ParamQuery p ops QRestricted []])
  Exposed _ _ (ToParamType _) -> unknown "the type of an argument"
  ExposedUnknown r -> unknown r

-- | The free variables through which @outerPoints@ reaches a cube point.
pointsOf :: Facts -> Points
pointsOf f = case fShape f of
  SVar v -> vfReach v
  SParam p -> mempty { pParams = [(Nothing, p)] }
  SParamType _ _ -> mempty { pUnknown = Just "the type of an argument" }
  SUniverse -> mempty
  SCubeU -> mempty
  STopeU -> mempty
  SLeaf _ -> mempty
  SCubeOrTope _ cs -> foldMap fPoints cs
  SPi p ->
    fPoints (piDomain p)
      <> without (piVar p) (foldMap fPoints (piTope p) <> fPoints (piCodomain p))
  SSigma s -> fPoints (sigmaFirst s) <> without (sigmaVar s) (fPoints (sigmaSecond s))
  SId i -> fPoints (idLeft i) <> foldMap fPoints (idType i) <> fPoints (idRight i)
  SRestricted ty faces -> fPoints ty <> foldMap facePoints faces
  SRecOr faces -> foldMap facePoints faces
  SApp g x -> fPoints g <> fPoints x
  SLam l ->
    foldMap (\p -> fPoints (lamDomain p) <> without (lamVar l) (foldMap fPoints (lamTope p))) (lamParam l)
      <> without (lamVar l) (fPoints (lamBody l))
  SLet l ->
    foldMap fPoints (letAnnotation l) <> fPoints (letValue l)
      <> without (letVar l) (fPoints (letBody l))
  SPair a b -> fPoints a <> fPoints b
  SFirst p -> fPoints p
  SSecond p -> fPoints p
  SRefl me -> foldMap (fPoints . endpointTerm) me
  SIdJ j -> foldMap fPoints [jType j, jStart j, jMotive j, jBase j, jEnd j, jProof j]
  STypeAsc v ty -> fPoints v <> fPoints ty
  STypeModal ty -> fPoints ty
  SScoped frame g -> scopedPoints frame (fPoints g)
  SOther r -> mempty { pUnknown = Just r }
  where
    facePoints (Face a b) = fPoints a <> fPoints b

-- | Leave a binder's own variable out of account.
without :: VarFacts -> Points -> Points
without bv p = p
  { pVars = IntSet.delete key (pVars p)
  , pIfCube = [e | e@(k, _) <- pIfCube p, k /= key]
  , pParams = [e | e@(via, _) <- pParams p, via /= Just key]
  }
  where
    key = vfKey bv

-- | No outer cube point is reached, leaving the binder's own variable, if
-- given, out of account.
noPointsBut :: Maybe VarFacts -> Points -> Verdict
noPointsBut bound points = case pUnknown p of
  Just r -> Unclean r
  Nothing
    | not (IntSet.null (pVars p)) -> Unclean "an outer cube point"
    | otherwise ->
        needs (pNeeds p)
          <> needs [Need prm (ReqAnswer [] QTypeCube False) | (_, prm) <- pIfCube p]
          <> needs [Need prm ReqNoPoints | (_, prm) <- pParams p]
  where
    p = maybe points (`without` points) bound

-- * Building facts from facts

-- | The facts of an application built here rather than read from a term: its
-- type is the codomain of the function's type, instantiated.
appFacts :: Facts -> Facts -> Facts
appFacts g x = mkFacts attrs (SApp g x) ty
  where
    attrs = fAttrs g
    ty = case functionHeadOf (fType g) of
      Right (frames, t) | SPi p <- fShape t -> underFrames frames (snd (piOpen p x))
      _ -> unknownFacts attrs "the type of an application built from facts"

-- | The facts of the two projections of a pair built here: their types are
-- the components of the pair's type.
projectionFacts :: Facts -> (Facts, Facts)
projectionFacts p = (mkFacts attrs (SFirst p) first, mkFacts attrs (SSecond p) second)
  where
    attrs = fAttrs p
    (first, second) = case functionHeadOf (fType p) of
      Right (frames, h)
        | SSigma s <- fShape h ->
            let a = underFrames frames (sigmaFirst s)
            in (a, underFrames frames (sigmaOpen s (mkFacts attrs (SFirst p) a)))
        | SCubeOrTope Admitted [a, b] <- fShape h -> (underFrames frames a, underFrames frames b)
      _ -> let u = unknownFacts attrs "the type of a projection built from facts" in (u, u)

-- | What a body does to a parameter, done to an argument.
applyOps :: Facts -> [Op] -> Facts
applyOps = foldl' applyOp
  where
    applyOp f = \case
      OpApp x  -> appFacts f x
      OpFirst  -> fst (projectionFacts f)
      OpSecond -> snd (projectionFacts f)

-- | An operation from the body of a table, with its argument taken to a use.
scopeOp :: (Facts -> Facts) -> Op -> Op
scopeOp k (OpApp x) = OpApp (k x)
scopeOp _ op        = op

-- | The argument of a use for a parameter.
frameArg :: Frame -> Int -> Maybe Facts
frameArg frame j
  | j < 0 = Nothing
  | otherwise = listToMaybe (drop j (frameArgs frame))

-- | Facts from the bodies of tables, at uses (innermost first).
underFrames :: [Frame] -> Facts -> Facts
underFrames frames x = foldl' (flip scopedFacts) x frames

-- | The facts of a term from the body of a table, at a use: every fact is
-- settled against the use's arguments, and a walk of the term is a walk of
-- the body's term followed by the settlement.
scopedFacts :: Frame -> Facts -> Facts
scopedFacts frame g = f
  where
    attrs = fAttrs g
    f = Facts
      { fShape = SScoped frame g
      , fType = scopedFacts frame (fType g)
      , fDerived = Derived
          { dExpose = scopedExposure frame (fExpose g)
          , dHeadView = scopedExposure frame (fHeadView g)
          , dMeta = resolveSym frame (fMeta g)
          , dLands = resolveSym frame (fLands g)
          , dCube = resolveSym frame (fCube g)
          , dPoints = scopedPoints frame (fPoints g)
          }
      , fVerdicts = attrScopedVerdicts attrs frame g
      , fSchem = attrScopedSchem attrs frame g
      , fDepth = fDepth g
      , fAttrs = attrs
      }

-- | An exposure found in the body of a table, at a use.
scopedExposure :: Frame -> Exposed -> Exposed
scopedExposure frame = \case
  Exposed k ns (ToFacts _ g) -> settled ns (Exposed k [] (InTable frame g))
  Exposed k ns (ToTable inner) -> settled ns (Exposed k [] (ToTable (scopedFrame inner)))
  Exposed k ns (InTable inner g) -> settled ns (Exposed k [] (InTable (scopedFrame inner) g))
  Exposed k ns (ToParam p)
    | mine p -> settled ns (later k (argument p fExpose))
    | otherwise -> Exposed k ns (ToParam p)
  Exposed k ns (ToParamApp p ops)
    | mine p -> settled ns (later k (argument p (\arg -> fExpose (applyOps arg (map scoped ops)))))
    | otherwise -> Exposed k ns (ToParamApp p (map scoped ops))
  Exposed k ns (ToParamType p)
    | mine p -> settled ns (later k (argument p (fExpose . fType)))
    | otherwise -> Exposed k ns (ToParamType p)
  ExposedUnknown r -> ExposedUnknown r
  where
    mine p = sameTable (frameTable frame) (paramTable p)
    scoped = scopeOp (scopedFacts frame)
    scopedFrame inner = inner { frameArgs = map (scopedFacts frame) (frameArgs inner) }
    argument p k = maybe (ExposedUnknown "an argument out of range") k (frameArg frame (paramIndex p))
    settled ns e = case resolveAnswers frame (needs ns) of
      Clean -> e
      Needs ns' -> underConditions ns' e
      Unclean r -> ExposedUnknown r

-- | Points found in the body of a table, at a use.
scopedPoints :: Frame -> Points -> Points
scopedPoints frame p = case resolveAnswers frame (needs (pNeeds p)) of
  Unclean r -> mempty { pUnknown = Just r }
  v -> foldr step (p { pIfCube = [], pParams = [], pNeeds = leftover v }) (pIfCube p)
         <> foldMap viaParam (pParams p)
  where
    leftover (Needs ns) = ns
    leftover _          = []
    mine prm = sameTable (frameTable frame) (paramTable prm)
    arg prm = frameArg frame (paramIndex prm)
    step (key, prm) acc
      | mine prm = case fCube . fType <$> arg prm of
          Just (SymBool [] (Known True)) -> acc { pVars = IntSet.insert key (pVars acc) }
          Just (SymBool [] (Known False)) -> acc
          _ -> acc { pUnknown = Just "a variable that may be a point" }
      | otherwise = acc { pIfCube = (key, prm) : pIfCube acc }
    viaParam (via, prm)
      | mine prm = maybe mempty { pUnknown = Just "an argument out of range" } fPoints (arg prm)
      | otherwise = mempty { pParams = [(via, prm)] }

-- * Function types

-- | @functionHead@ of the schematic check: what a type exposes to,
-- restrictions stripped, which may be in the bodies of tables at uses (the
-- frames, innermost first).
functionHeadOf :: Facts -> Either String ([Frame], Facts)
functionHeadOf = go [] . fExpose
  where
    go frames = \case
      Exposed steps [] (ToFacts _ g)
        | steps > schematicDepthLimit -> Left "near the reduction limit"
        | SRestricted base _ <- fShape g -> go frames (fExpose base)
        | otherwise -> Right (frames, g)
      Exposed _ _ (ToFacts _ _) -> Left "a type exposed under conditions"
      Exposed steps [] (ToTable frame)
        | steps <= schematicDepthLimit -> go (frame : frames) (fExpose (tbBody (frameTable frame)))
        | otherwise -> Left "near the reduction limit"
      Exposed steps [] (InTable frame g)
        | steps <= schematicDepthLimit -> go (frame : frames) (fExpose g)
        | otherwise -> Left "near the reduction limit"
      -- A parameter of a table in scope stands for an argument of the use.
      Exposed k [] (ToParam p) -> case argumentIn frames p of
        Just arg -> go frames (later k (fExpose arg))
        Nothing -> Left ("a function type that is " <> describe p)
      Exposed k [] (ToParamApp p ops) -> case argumentIn frames p of
        Just arg ->
          let outer = dropWhile (not . sameTable (paramTable p) . frameTable) frames
          in go frames (later k (fExpose (applyOps arg (map (scopeOp (underFrames outer)) ops))))
        Nothing -> Left ("a function type that is applied " <> describe p)
      Exposed k [] (ToParamType p) -> case argumentIn frames p of
        Just arg -> go frames (later k (fExpose (fType arg)))
        Nothing -> Left "a function type that is an argument's type"
      ExposedUnknown r -> Left r
      Exposed{} -> Left "a function type exposed under conditions"

    argumentIn frames p = do
      frame <- find (sameTable (paramTable p) . frameTable) frames
      frameArg frame (paramIndex p)

    describe p = "parameter " <> show (paramIndex p) <> " of " <> tbName (paramTable p)

-- | The domain of a function's type as @funDomain@ finds it, with the uses of
-- tables it was found in (innermost first) and the conditions of the
-- exposures on the way.
data DomainOf = DomainOf
  { domainFrames     :: [Frame]
  , domainConditions :: [Need]
  , domainFound      :: FoundDomain
  }

-- | What @funDomain@ finds.
data FoundDomain
  = Domain (Maybe Facts)
    -- ^ the domain, if the type is a function type
  | DomainOfParam Param [Need]
    -- ^ the type is the argument for a parameter, under these conditions:
    -- the use settles it
  | NoDomain String
    -- ^ the facts cannot tell, and why

domainOf :: Facts -> DomainOf
domainOf = go [] [] . fHeadView
  where
    go frames ns = \case
      Exposed _ ns' (ToFacts _ g) -> DomainOf frames (ns <> ns') $ Domain $ case fShape g of
        SPi p -> Just (piDomain p)
        _     -> Nothing
      Exposed _ ns' (ToTable frame) -> go (frame : frames) (ns <> ns') (fHeadView (tbBody (frameTable frame)))
      Exposed _ ns' (InTable frame g) -> go (frame : frames) (ns <> ns') (fHeadView g)
      Exposed _ ns' (ToParam p) -> DomainOf frames ns (DomainOfParam p ns')
      Exposed _ _ (ToParamApp _ _) -> DomainOf frames ns (NoDomain "a domain behind an applied argument")
      Exposed _ _ (ToParamType _) -> DomainOf frames ns (NoDomain "a domain behind an argument's type")
      ExposedUnknown r -> DomainOf frames ns (NoDomain r)

-- | Settle a symbolic fact found in the bodies of tables at uses (innermost
-- first).
scopedSym :: [Frame] -> SymBool -> SymBool
scopedSym frames s = foldl' (flip resolveSym) s frames

-- | Whether the domain of a function's type is a schematic kind
-- (@domainIsMeta@ in "Rzk.TypeCheck.MetaPrefix").
domainMetaOf :: Facts -> SymBool
domainMetaOf ty = case domainOf ty of
  DomainOf frames ns (Domain (Just dom)) -> scopedSym frames (withConditions ns (fMeta dom))
  DomainOf frames ns (Domain Nothing) -> scopedSym frames (withConditions ns (known False))
  DomainOf frames ns (DomainOfParam p ns') ->
    scopedSym frames (SymBool (ns <> ns') (AnyOf [ParamQuery p [] QDomainMeta []]))
  DomainOf _ _ (NoDomain r) -> unknown r

-- * Deciding symbolic facts

-- | Decide a fact as the standalone walk would at an opaque variable: a
-- disjunction of queries on parameters answers 'False', guarded by the needs
-- that each query answers 'False' on the argument.
decide :: SymBool -> (Bool, Verdict)
decide (SymBool ns a) = case a of
  Known b -> (b, needs ns)
  AnyOf qs -> (False, needs (ns <> [guardOf q False | q <- qs]))
  Unknown r -> (False, Unclean r)

-- | The need that a query answers so.
guardOf :: ParamQuery -> Bool -> Need
guardOf q b = Need (pqParam q) (foldl' (flip ReqUnder) (ReqAnswer (pqOps q) (pqQuery q) b) (pqFrames q))

-- | The answer must be this one. A single query that must answer 'True'
-- becomes a need; a disjunction cannot.
mustBe :: String -> Bool -> SymBool -> Verdict
mustBe _ True (SymBool ns (AnyOf [q])) = needs (ns <> [guardOf q True])
mustBe what b s =
  v <> if b == b' then Clean else Unclean (what <> " differs: " <> describe (symAnswer s))
  where
    (b', v) = decide s
    describe = \case
      Known x -> show x
      AnyOf qs -> "any of " <> show (map pqQuery qs)
      Unknown r -> r

-- | The answer of a query on a term.
queryOf :: Query -> Facts -> SymBool
queryOf = \case
  QMeta -> fMeta
  QLands -> fLands
  QCube -> fCube
  QRestricted -> restrictedOf
  QTypeMeta -> fMeta . fType
  QTypeLands -> fLands . fType
  QTypeCube -> fCube . fType
  QDomainMeta -> domainMetaOf

-- * Settling needs

-- | Settle the needs of a table's body against the arguments of a use, with
-- the given way of meeting a requirement. What an argument meets may need
-- more of the same arguments (an argument applied inside the body), so this
-- repeats until no need of this table is left; the needs of other tables
-- remain, those that mention this table's facts to be settled under its use.
settleWith :: (Facts -> Req -> Verdict) -> Frame -> Verdict -> Verdict
settleWith meet frame = loop
  where
    mine = sameTable (frameTable frame) . paramTable . needParam
    loop = \case
      Needs ns | (own@(_ : _), others) <- partition mine ns ->
        loop (foldMap settle (distinctNeeds own) <> needs others)
      Needs ns -> Needs (map leaving (distinctNeeds ns))
      v -> v
    settle (Need p r) =
      maybe (Unclean "an argument out of range") (`meet` r) (frameArg frame (paramIndex p))
    leaving n@(Need p r)
      | mentionsFacts r = Need p (ReqUnder frame r)
      | otherwise = n
    mentionsFacts = \case
      ReqAnswer ops _ _ -> not (null ops)
      ReqNoPoints -> False
      ReqCheck (ReqClean _ _) -> False
      ReqCheck (ReqApplied _ _ ops) -> not (null ops)
      ReqCheck (ReqBoundary ops) -> not (null ops)
      ReqCheck (ReqArgumentAt _ _) -> True
      ReqUnder _ _ -> True

-- | Meet a requirement on an argument: those about answers and points here,
-- those of the fragment check by the given function.
evalReqWith :: (Facts -> CheckReq -> Verdict) -> Facts -> Req -> Verdict
evalReqWith check f = \case
  ReqAnswer ops q b -> mustBe (show q) b (queryOf q (applyOps f ops))
  ReqNoPoints -> noPointsBut Nothing (fPoints f)
  ReqCheck r -> check f r
  ReqUnder frame r -> settleWith (evalReqWith check) frame (evalReqWith check f r)

-- | Settle the needs that are answers and points; those of the fragment
-- check depend on the checks in force and are not settled here.
resolveAnswers :: Frame -> Verdict -> Verdict
resolveAnswers = settleWith (evalReqWith noCheck)
  where
    noCheck _ _ = Unclean "a need that depends on the checks in force"

-- | The answer of a query on the body of a table, at a use. An argument's
-- answer may ask more of the same arguments, so this repeats until no query
-- on this table's parameters is left. A query on another table's parameter
-- that leaves carries this table's facts, so it is to be asked under the use.
resolveSym :: Frame -> SymBool -> SymBool
resolveSym frame = loop
  where
    mine = sameTable (frameTable frame) . paramTable . pqParam
    loop (SymBool ns a) = case resolveAnswers frame (needs ns) of
      Unclean r -> unknown r
      v -> case a of
        AnyOf qs | (own@(_ : _), others) <- partition mine qs ->
          loop $ withConditions (needsOf v) $
            foldr orSym (SymBool [] (AnyOf others)) (map answer own)
        AnyOf qs -> SymBool (needsOf v) (AnyOf (map leaving qs))
        _ -> SymBool (needsOf v) a
    needsOf (Needs ns) = ns
    needsOf _          = []
    -- A query that came from under other tables' uses is answered there.
    answer q = case frameArg frame (paramIndex (pqParam q)) of
      Nothing -> unknown "an argument out of range"
      Just arg -> scopedSym (pqFrames q) (queryOf (pqQuery q) (applyOps arg (pqOps q)))
    leaving q
      | null (pqOps q) = q
      | otherwise = q { pqFrames = pqFrames q <> [frame] }

-- | Drop the repetitions of a need (a parameter used twice at an entry); a
-- need that mentions facts is kept as it is.
distinctNeeds :: [Need] -> [Need]
distinctNeeds = go Set.empty
  where
    go _ [] = []
    go seen (n@(Need p r) : rest) = case keyOf r of
      Just k
        | Set.member (tbId (paramTable p), paramIndex p, k) seen -> go seen rest
        | otherwise -> n : go (Set.insert (tbId (paramTable p), paramIndex p, k) seen) rest
      Nothing -> n : go seen rest
    keyOf = \case
      ReqAnswer [] q b -> Just (KeyAnswer q b)
      ReqNoPoints -> Just KeyNoPoints
      ReqCheck (ReqClean v e) -> Just (KeyClean v e)
      ReqCheck (ReqBoundary []) -> Just KeyBoundary
      _ -> Nothing

-- | A requirement without facts in it, for 'distinctNeeds'.
data ReqKey
  = KeyClean View Entry
  | KeyNoPoints
  | KeyAnswer Query Bool
  | KeyBoundary
  deriving (Eq, Ord)

-- * Helpers

isRestrictedShape :: Facts -> Bool
isRestrictedShape f = case fShape f of
  SRestricted{} -> True
  _             -> False

stripRestrictions :: Facts -> Facts
stripRestrictions f = case fShape f of
  SRestricted base _ -> stripRestrictions base
  _                  -> f
