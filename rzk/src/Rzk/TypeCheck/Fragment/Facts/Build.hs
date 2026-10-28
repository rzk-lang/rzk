{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Building the facts of elaborated terms, and the tables of definitions.
module Rzk.TypeCheck.Fragment.Facts.Build (
  Env (..),
  facts,
  buildTable,
  varFacts,
  paramFacts,
  reachOf,
) where

import           Control.Applicative                (Alternative ((<|>)))
import qualified Data.IntMap                        as IntMap
import qualified Data.IntSet                        as IntSet
import           Data.Maybe                         (fromMaybe)

import           Control.Monad.Foil                 (Distinct, NameBinder)
import qualified Control.Monad.Foil                 as Foil
import           Control.Monad.Free.Foil            (AST (Var))

import           Language.Rzk.Foil.Names            (Binder, TModality (..),
                                                     TypeInfo (..))
import           Language.Rzk.Foil.Syntax
import           Rzk.TypeCheck.Fragment.Facts.Types

-- | The environment a term's facts are built in.
data Env n = Env
  { envScope :: Foil.Scope n
  , envBound :: IntMap.IntMap Facts
    -- ^ what a name bound in the term stands for: its variable, or the facts
    -- substituted for it
  , envOuter :: IntMap.IntMap VarFacts
    -- ^ the top-level entries, by name identifier
  , envOwner :: !Int
    -- ^ tells the keys of the variables bound here from those bound in other
    -- terms, whose facts may be combined: a top-level entry's facts and its
    -- table have their own, and a declaration under check a fresh negative one
  , envAttrs :: Attrs
  }

-- | The key of a variable bound in the term: unique across owners.
keyFor :: Env n -> NameBinder n l -> Int
keyFor env binder = envOwner env * 4294967296 + Foil.nameId (Foil.nameOf binder)

-- | Bind a name to the facts it stands for.
extend :: Env n -> NameBinder n l -> Facts -> Env l
extend env binder f = env
  { envScope = Foil.extendScope binder (envScope env)
  , envBound = IntMap.insert (Foil.nameId (Foil.nameOf binder)) f (envBound env)
  }

-- | The facts of a variable.
varFacts :: Attrs -> VarFacts -> Facts
varFacts attrs v = mkFacts attrs (SVar v) (vfType v)

-- | A variable bound in the term, with its type and value.
boundVar :: Int -> Binder -> Facts -> Maybe Facts -> VarFacts
boundVar key orig ty = boundVarWith key orig ty ty

-- | A variable bound in the term whose type differs for the schematic check.
boundVarWith :: Int -> Binder -> Facts -> Facts -> Maybe Facts -> VarFacts
boundVarWith key orig ty schemTy mval = VarFacts
  { vfKey = key
  , vfOrig = orig
  , vfType = ty
  , vfSchemType = schemTy
  , vfValue = mval
  , vfTop = Nothing
  , vfReach = reachOf key ty mval
  }

-- | What @outerPoints@ finds through a variable of this type and value: the
-- variable itself if it is a cube point (no value, and its type is a cube),
-- and the variable again for whatever its type and value reach.
reachOf :: Int -> Facts -> Maybe Facts -> Points
reachOf key ty mval = case own of
  SymBool ns (Known True) ->
    viaMe { pVars = IntSet.singleton key, pNeeds = ns <> pNeeds viaMe }
  SymBool ns (Known False) -> viaMe { pNeeds = ns <> pNeeds viaMe }
  SymBool ns (AnyOf qs)
    | all isTypeCube qs -> viaMe
        { pIfCube = [(key, pqParam q) | q <- qs] <> pIfCube viaMe
        , pNeeds = ns <> pNeeds viaMe
        }
  SymBool _ (Unknown r) -> mempty { pUnknown = Just r }
  _ -> mempty { pUnknown = Just "a variable that may be a point" }
  where
    own = case mval of
      Nothing -> fCube (fType ty)
      Just _  -> known False
    through = fPoints ty <> foldMap fPoints mval
    viaMe = through
      { pVars = if IntSet.null (pVars through) then IntSet.empty else IntSet.singleton key
      , pIfCube = [(key, p) | (_, p) <- pIfCube through]
      , pParams = [(Just key, p) | (_, p) <- pParams through]
      }
    isTypeCube q = null (pqOps q) && null (pqFrames q) && pqQuery q == QTypeCube

-- | A parameter of a table: every question about it is a question about the
-- argument of a use.
paramFacts :: Attrs -> Param -> Maybe Facts -> Facts
paramFacts attrs p mdeclared = f
  where
    f = Facts
      { fShape = SParam p
      , fType = paramTypeFacts attrs p mdeclared
      , fDerived = Derived
          { dExpose = Exposed 0 [] (ToParam p)
          , dHeadView = Exposed 0 [] (ToParam p)
          , dMeta = asked QMeta
          , dLands = asked QLands
          , dCube = asked QCube
          , dPoints = mempty { pParams = [(Nothing, p)] }
          }
      , fVerdicts = attrVerdicts attrs f
      , fSchem = attrSchem attrs f
      , fDepth = 1
      , fAttrs = attrs
      }
    asked q = SymBool [] (AnyOf [ParamQuery p [] q []])

-- | The type of the argument a parameter stands for. The parameter's declared
-- type, when known, gives the likely answer to a query; the answer is
-- guarded by a need, since the argument's own type may differ.
paramTypeFacts :: Attrs -> Param -> Maybe Facts -> Facts
paramTypeFacts attrs p mdeclared = f
  where
    f = Facts
      { fShape = SParamType p mdeclared
      , fType = maybe (unknownFacts attrs "the type of an argument's type") fType mdeclared
      , fDerived = Derived
          { dExpose = Exposed 0 [] (ToParamType p)
          , dHeadView = Exposed 0 [] (ToParamType p)
          , dMeta = guarded QTypeMeta fMeta
          , dLands = guarded QTypeLands fLands
          , dCube = guarded QTypeCube fCube
          , dPoints = mempty { pUnknown = Just "the type of an argument" }
          }
      , fVerdicts = attrVerdicts attrs f
      , fSchem = attrSchem attrs f
      , fDepth = 1
      , fAttrs = attrs
      }
    guarded q field = case field <$> mdeclared of
      Just (SymBool ns (Known b)) -> SymBool (ns <> [Need p (ReqAnswer [] q b)]) (Known b)
      Just (SymBool ns (AnyOf qs)) -> SymBool (ns <> [Need p (ReqAnswer [] q False)]) (AnyOf qs)
      _ -> SymBool [] (AnyOf [ParamQuery p [] q []])

-- | The facts of a term.
facts :: forall n. Distinct n => Env n -> TermT n -> Facts
facts env = go
  where
    attrs = envAttrs env
    scope = envScope env

    go :: TermT n -> Facts
    go = \case
      Var v ->
        let i = Foil.nameId v
        in fromMaybe (unknownFacts attrs "a variable out of scope") $
             IntMap.lookup i (envBound env)
               <|> (varFacts attrs <$> IntMap.lookup i (envOuter env))
      UniverseT info -> node SUniverse info
      UniverseCubeT info -> node SCubeU info
      UniverseTopeT info -> node STopeU info
      CubeUnitT info -> leaf LCubeUnit info
      CubeUnitStarT info -> leaf LOtherConst info
      Cube2T info -> leaf LCube2 info
      Cube2_0T info -> leaf LOtherConst info
      Cube2_1T info -> leaf LOtherConst info
      CubeIT info -> leaf LInterval info
      CubeI_0T info -> leaf LInterval info
      CubeI_1T info -> leaf LInterval info
      TopeTopT info -> leaf LOtherConst info
      TopeBottomT info -> leaf LOtherConst info
      UnitT info -> leaf LOtherConst info
      TypeUnitT info -> leaf LTypeUnit info
      RecBottomT info -> leaf LRecBottom info
      HoleT info _ -> leaf LHole info
      CubeProductT info a b -> operation Admitted [a, b] info
      CubeFlipT info a -> operation NotAdmitted [a] info
      CubeUnflipT info a -> operation NotAdmitted [a] info
      CubeSupT info a b -> operation Admitted [a, b] info
      CubeInfT info a b -> operation Admitted [a, b] info
      TopeEQT info a b -> operation Admitted [a, b] info
      TopeLEQT info a b -> operation Admitted [a, b] info
      TopeAndT info a b -> operation Admitted [a, b] info
      TopeOrT info a b -> operation Admitted [a, b] info
      TopeInvT info a -> operation NotAdmitted [a] info
      TopeUninvT info a -> operation NotAdmitted [a] info
      RecOrT info rs -> node (SRecOr (map face rs)) info
      TypeRestrictedT info ty rs -> node (SRestricted (go ty) (map face rs)) info
      TypeIdT info a mtA b -> node (SId (IdFacts (go a) (go <$> mtA) (go b))) info
      AppT info f x -> node (SApp (go f) (go x)) info
      PairT info a b -> node (SPair (go a) (go b)) info
      FirstT info p -> node (SFirst (go p)) info
      SecondT info p -> node (SSecond (go p)) info
      ReflT info mx -> node (SRefl (endpoint <$> mx)) info
      IdJT info tA a tC d x p ->
        node (SIdJ (IdJFacts (go tA) (go a) (go tC) (go d) (go x) (go p))) info
      TypeAscT info v ty -> node (STypeAsc (go v) (go ty)) info
      TypeModalT info _ ty -> node (STypeModal (go ty)) info
      ModAppT info _ _ -> node (SOther "a modal introduction") info
      ModExtractT info _ _ _ -> node (SOther "a modal extraction") info
      LetModT info _ _ _ _ _ _ _ -> node (SOther "a modal let") info
      MatchT info _ _ _ _ -> node (SOther "a match") info
      MatchArmT info _ _ -> node (SOther "a match arm") info
      TypeFunT info orig md param mtope ret -> node (SPi (pi' orig md param mtope ret)) info
      TypeSigmaT info orig md a b -> node (SSigma (sigma orig md a b)) info
      LetT info orig manno value body -> node (SLet (letIn orig manno value body)) info
      LambdaT info orig mparam body -> lambda info orig mparam body

    node :: Shape -> TypeInfo (TermT n) -> Facts
    node sh info = mkFacts attrs sh (go (infoType info))

    leaf :: Leaf -> TypeInfo (TermT n) -> Facts
    leaf = node . SLeaf

    operation :: Admission -> [TermT n] -> TypeInfo (TermT n) -> Facts
    operation admission operands = node (SCubeOrTope admission (map go operands))

    face :: (TermT n, TermT n) -> Face
    face (tope, term) = Face (go tope) (go term)

    endpoint :: (TermT n, Maybe (TermT n)) -> Endpoint
    endpoint (x, mty) = Endpoint (go x) (go <$> mty)

    -- Each binder is opened twice over: at its own variable (the default
    -- opening, which the walks see) and at whatever stands for it later
    -- (β-reduction and instantiation).
    pi' :: Binder -> TModality -> TermT n -> Maybe (ScopedTermT n) -> ScopedTermT n -> PiFacts
    pi' orig md param mtope ret = PiFacts
      { piOrig = orig
      , piModality = md
      , piDomain = domain
      , piVar = bv
      , piTope = tope
      , piCodomain = codomain
      , piOpen = \x -> let (_, mt, c) = under x in (mt, c)
      }
      where
        domain = go param
        under :: Facts -> (Int, Maybe Facts, Facts)
        under x = case mtope of
          Nothing -> withScopedT scope ret $ \binder body ->
            (keyFor env binder, Nothing, facts (extend env binder x) body)
          Just tp -> withScopedT2 scope tp ret $ \binder tp' body ->
            let env' = extend env binder x
            in (keyFor env binder, Just (facts env' tp'), facts env' body)
        (key, tope, codomain) = under (varFacts attrs bv)
        bv = boundVar key orig domain Nothing

    sigma :: Binder -> TModality -> TermT n -> ScopedTermT n -> SigmaFacts
    sigma orig md a b = SigmaFacts
      { sigmaOrig = orig
      , sigmaModality = md
      , sigmaFirst = first'
      , sigmaVar = bv
      , sigmaSecond = second'
      , sigmaOpen = snd . under
      }
      where
        first' = go a
        under :: Facts -> (Int, Facts)
        under x = withScopedT scope b $ \binder body ->
          (keyFor env binder, facts (extend env binder x) body)
        (key, second') = under (varFacts attrs bv)
        bv = boundVar key orig first' Nothing

    -- The let annotation records the value's concluded type; without one,
    -- the walk takes the value's annotation, and a universe for a bare
    -- variable. The schematic check gives the variable the annotation, or the
    -- type it derives for the value.
    letIn :: Binder -> Maybe (TermT n) -> TermT n -> ScopedTermT n -> LetFacts
    letIn orig manno value body = LetFacts
      { letOrig = orig
      , letAnnotation = annotation
      , letValue = valueF
      , letVar = bv
      , letBody = bodyF
      }
      where
        valueF = go value
        annotation = go <$> manno
        dom = case (annotation, value) of
          (Just a, _)        -> a
          (Nothing, Var{})   -> universeFacts
          (Nothing, _)       -> fType valueF
        (key, bodyF) = withScopedT scope body $ \binder body' ->
          (keyFor env binder, facts (extend env binder (varFacts attrs bv)) body')
        bv = boundVarWith key orig dom (fromMaybe (scType (fSchem valueF)) annotation) (Just valueF)

    -- A bare λ binds a variable of the domain of its own type.
    lambda
      :: TypeInfo (TermT n) -> Binder -> Maybe (LambdaParam (ScopedTermT n) (TermT n))
      -> ScopedTermT n -> Facts
    lambda info orig mparam body = lamF
      where
        lamF = node (SLam lam) info
        lam = LamFacts
          { lamOrig = orig
          , lamModality = case mparam of
              Just (LambdaParam md _ _) -> md
              Nothing                   -> Id
          , lamParam = param
          , lamVar = bv
          , lamBody = bodyF
          , lamOpen = \x -> let (_, _, b) = under x in b
          }
        under :: Facts -> (Int, Maybe LamParam, Facts)
        under x = case mparam of
          Just (LambdaParam _ written (Just tope)) ->
            withScopedT2 scope tope body $ \binder tope' body' ->
              let env' = extend env binder x
              in (keyFor env binder, Just (LamParam (go written) (Just (facts env' tope'))), facts env' body')
          Just (LambdaParam _ written Nothing) ->
            withScopedT scope body $ \binder body' ->
              (keyFor env binder, Just (LamParam (go written) Nothing), facts (extend env binder x) body')
          Nothing ->
            withScopedT scope body $ \binder body' ->
              (keyFor env binder, Nothing, facts (extend env binder x) body')
        (key, param, bodyF) = under (varFacts attrs bv)
        dom = case param of
          Just p -> lamDomain p
          Nothing -> case fHeadView (fType lamF) of
            Exposed _ [] (ToFacts _ g) | SPi p <- fShape g -> piDomain p
                                       | otherwise -> universeFacts
            _ -> unknownFacts attrs "the domain of a bare lambda"
        bv = boundVar key orig dom Nothing

    universeFacts :: Facts
    universeFacts = u
      where
        u = mkFacts attrs SUniverse u

-- | A definition's table: its value under its leading lambdas, with the
-- parameters symbolic. A lambda without a domain (one that abstracts a
-- section's assumption) takes its declared type from the entry's type.
buildTable :: forall n. Distinct n => Env n -> Int -> String -> Facts -> TermT n -> Table
buildTable env0 i name ty value = table
  where
    attrs = envAttrs env0
    table = peel env0 0 [] (Just ty) value

    peel :: forall l. Distinct l => Env l -> Int -> [Maybe Facts] -> Maybe Facts -> TermT l -> Table
    peel env k declareds mty = \case
      LambdaT _ _ mparam body ->
        let typeHead = case fmap fHeadView mty of
              Just (Exposed _ [] (ToFacts _ g)) | SPi p <- fShape g -> Just p
              _ -> Nothing
            declared = case mparam of
              Just (LambdaParam _ param _) -> Just (facts env param)
              Nothing                      -> piDomain <$> typeHead
            leaf = paramFacts attrs (Param table k) declared
            nextTy = (\p -> snd (piOpen p leaf)) <$> typeHead
        in withScopedT (envScope env) body $ \binder body' ->
             peel (extend env binder leaf) (k + 1) (declareds <> [declared]) nextTy body'
      body -> Table
        { tbId = i
        , tbName = name
        , tbArity = k
        , tbDeclared = declareds
        , tbBody = facts env body
        }
