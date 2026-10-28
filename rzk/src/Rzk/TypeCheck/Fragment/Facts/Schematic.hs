{-# LANGUAGE LambdaCase #-}

-- | The certificate of the schematic check ("Rzk.TypeCheck.Schematic"): the
-- same classification of types into object types, parameter kinds,
-- schematic rules and shapes, over the facts, with the parameters of a table
-- symbolic. The cases follow @classify@, @term@, @signature@, @argument@ and
-- @schematicTypeOf@ there one for one. The standalone check also checks the
-- top-level entries it meets whose own check is not recorded; the
-- certificate collects them instead, and the caller consults their status.
module Rzk.TypeCheck.Fragment.Facts.Schematic (
  schemOf,
  scopedSchemOf,
  objectFamilyOf,
  SchematicCertificate (..),
  schematicCertificate,
) where

import           Control.Monad                       (join)
import qualified Data.IntSet                         as IntSet
import           Data.List                           (partition)
import           Data.Maybe                          (fromMaybe, listToMaybe)

import           Rzk.TypeCheck.Fragment.Facts.Build
import           Rzk.TypeCheck.Fragment.Facts.Expose
import           Rzk.TypeCheck.Fragment.Facts.Types

-- | The equation of 'fSchem'.
schemOf :: Facts -> Schem
schemOf f = Schem
  { scSort = classifyOf f
  , scHead = exposedSort (fAttrs f) (fExpose f)
  , scTerm = termOf f
  , scSignature = signatureOf f
  , scType = schemTypeOf f
  }

-- | The equation of 'fSchem' for facts from a table's body at a use: the
-- body's findings, settled against the use's arguments.
scopedSchemOf :: Frame -> Facts -> Schem
scopedSchemOf frame g = Schem
  { scSort = scopedSort (classifyM g)
  , scHead = scopedSort (headSortM g)
  , scTerm = resolveS frame (termM g)
  , scSignature = resolveS frame (signatureM g)
  , scType = scopedFacts frame (schemTypeM g)
  }
  where
    scopedSort = \case
      Sorted s v -> Sorted s (resolveS frame v)
      u          -> u

-- | The certificate of the schematic check on a declaration.
data SchematicCertificate = SchematicCertificate
  { certSignature :: SVerdict
    -- ^ of the declaration's type
  , certValue     :: SVerdict
    -- ^ of its value, if any
  , certDepth     :: Int
    -- ^ the depth of its terms, which bounds the fuel the walk spends
  }

schematicCertificate :: Facts -> Maybe Facts -> SchematicCertificate
schematicCertificate ty mval = SchematicCertificate
  { certSignature = signatureM ty
  , certValue = foldMap termM mval
  , certDepth = max (fDepth ty) (maybe 0 fDepth mval)
  }

classifyM, headSortM :: Facts -> Sorted
classifyM = scSort . fSchem
headSortM = scHead . fSchem

termM, signatureM :: Facts -> SVerdict
termM = scTerm . fSchem
signatureM = scSignature . fSchem

schemTypeM :: Facts -> Facts
schemTypeM = scType . fSchem

-- | The verdict part of a classification.
sortV :: Sorted -> SVerdict
sortV (Sorted _ v)  = v
sortV (Unsorted r)  = SFail r

-- | Add what a classification relies on.
withV :: SVerdict -> Sorted -> Sorted
withV v (Sorted s v') = Sorted s (v <> v')
withV _ u             = u

-- | What @uses@ records at a node: a top-level entry.
usesOf :: Facts -> SVerdict
usesOf f = case fShape f of
  SVar v | Just _ <- vfTop v -> SOk (IntSet.singleton (vfKey v))
  _ -> mempty

-- | Settle verdicts found in the bodies of tables at uses (innermost first).
resolveScopes :: [Frame] -> SVerdict -> SVerdict
resolveScopes frames v = foldl' (flip resolveS) v frames

-- | @classify@: the uses and redexes of the term as written, then the known
-- object families, then the head of what it exposes to.
classifyOf :: Facts -> Sorted
classifyOf f = withV (usesOf f <> inspectRedex f) $ case knownObjectFamily f of
  Just v  -> Sorted SObjectType v
  Nothing -> headSortM f

-- | @inspectRedex@: check erased arguments and annotations before exposing
-- the result.
inspectRedex :: Facts -> SVerdict
inspectRedex f = case fShape f of
  SLet{} -> termM f
  STypeAsc{} -> termM f
  SFirst{} -> termM f
  SSecond{} -> termM f
  SIdJ{} -> termM f
  SRecOr{} -> termM f
  _ -> applications f

-- | A known object family, applied to as many arguments as it has indices.
knownObjectFamily :: Facts -> Maybe SVerdict
knownObjectFamily f = case fShape h of
  SVar v
    | Just tl <- vfTop v
    , Just (arity, probe) <- tlObjectFamily tl
    , arity == length args -> Just probe
  _ -> Nothing
  where
    (h, args) = spine f

-- | The arity at which an entry is an object-valued family: its type's
-- parameters are probed at fresh variables, and the value applied to them
-- must classify as an object type.
objectFamilyOf :: Facts -> Maybe Facts -> Maybe (Int, SVerdict)
objectFamilyOf ty mval = mval >>= go 0 ty
  where
    go :: Int -> Facts -> Facts -> Maybe (Int, SVerdict)
    go arity t v
      | fDepth v > schematicDepthLimit = Nothing
      | otherwise = case fExpose t of
          Exposed steps [] (ToFacts _ g) | steps <= schematicDepthLimit -> case fShape g of
            SPi p -> go (arity + 1) (piCodomain p) (appFacts v (varFacts (fAttrs v) (piVar p)))
            SUniverse -> case classifyM v of
              Sorted SObjectType verdict -> Just (arity, verdict)
              _                          -> Nothing
            _ -> Nothing
          _ -> Nothing

-- | @classifyHead@: the classification of what a term exposes to.
exposedSort :: Attrs -> Exposed -> Sorted
exposedSort attrs = \case
  Exposed steps [] (ToFacts _ g)
    | steps <= schematicDepthLimit -> classifyExposed g
    | otherwise -> Unsorted "near the reduction limit"
  Exposed _ _ (ToFacts _ _) -> Unsorted "a type exposed under conditions"
  Exposed steps [] (ToTable frame)
    | steps <= schematicDepthLimit -> scoped frame (headSortM (tableBody frame))
    | otherwise -> Unsorted "near the reduction limit"
  Exposed steps [] (InTable frame g)
    | steps <= schematicDepthLimit -> scoped frame (headSortM g)
    | otherwise -> Unsorted "near the reduction limit"
  Exposed _ [] (ToParam p) -> case declaredSort p of
    Just s -> Sorted s (SNeeds IntSet.empty [SNeed p (SReqSort s)])
    Nothing -> Unsorted ("a parameter of a kind that is not a sort: " <> describe p)
  Exposed _ [] (ToParamApp p ops) -> stuckSort (applyOps (paramFacts attrs p (declaredOf p)) ops)
  Exposed _ _ (ToParamType _) -> Unsorted "the type of an argument, classified"
  ExposedUnknown r -> Unsorted r
  Exposed{} -> Unsorted "an exposure under conditions"
  where
    scoped frame = \case
      Sorted s v -> Sorted s (resolveS frame v)
      u          -> u
    describe p = show (paramIndex p) <> " of " <> tbName (paramTable p)

-- | The declared type of a table's parameter, if known.
declaredOf :: Param -> Maybe Facts
declaredOf p
  | paramIndex p < 0 = Nothing
  | otherwise = join (listToMaybe (drop (paramIndex p) (tbDeclared (paramTable p))))

-- | The sort of a parameter as the probe at a fresh variable finds it: by its
-- declared type.
declaredSort :: Param -> Maybe SSort
declaredSort p = declaredOf p >>= \d -> case fExpose d of
  Exposed _ [] (ToFacts _ g) -> case fShape g of
    SUniverse -> Just SObjectType
    SCubeU -> Just SShape
    STopeU -> Just SShape
    _ -> Nothing
  _ -> Nothing

-- | The classification of a type former; anything else is a stuck term.
classifyExposed :: Facts -> Sorted
classifyExposed g = case fShape g of
  SUniverse -> Sorted SParameterKind mempty
  SCubeU -> Sorted SParameterKind mempty
  STopeU -> Sorted SParameterKind mempty
  SPi p -> case (classifyM (piDomain p), classifyM (piCodomain p)) of
    (Sorted d dv, Sorted r rv) ->
      let v = dv <> foldMap termM (piTope p) <> rv
      in case (d, r) of
        (SObjectType, SObjectType) -> Sorted SObjectType v
        (SShape, SObjectType) -> Sorted SObjectType v
        (_, SParameterKind) -> Sorted SParameterKind v
        (_, SShape) -> Unsorted "cube or shape family"
        _ -> Sorted SSchematicRule v
    (Unsorted r, _) -> Unsorted r
    (_, Unsorted r) -> Unsorted r
  SSigma s -> case (classifyM (sigmaFirst s), classifyM (sigmaSecond s)) of
    (Sorted d dv, Sorted r rv)
      | d == SObjectType && r == SObjectType -> Sorted SObjectType (dv <> rv)
      | otherwise -> Sorted SParameterKind (dv <> rv)
    (Unsorted r, _) -> Unsorted r
    (_, Unsorted r) -> Unsorted r
  SId i ->
    Sorted SObjectType $
      sobject (fromMaybe (schemTypeM (idLeft i)) (idType i))
        <> termM (idLeft i) <> termM (idRight i)
  SLeaf LTypeUnit -> Sorted SObjectType mempty
  SLeaf LRecBottom -> Sorted SObjectType mempty
  SRecOr faces -> case map (classifyM . faceTerm) faces of
    Sorted s v : rest | all (sameSort s) rest -> Sorted s (v <> foldMap sortV rest)
    Unsorted r : _ -> Unsorted r
    _ -> Unsorted "case split between different schematic kinds"
  SRestricted ty faces -> case classifyM ty of
    Sorted s v -> Sorted s (v <> foldMap (\(Face tope x) -> termM tope <> sargument ty x) faces)
    u -> u
  SLeaf LCubeUnit -> Sorted SShape mempty
  SLeaf LCube2 -> Sorted SShape mempty
  _ -> stuckSort g
  where
    sameSort s (Sorted s' _) = s == s'
    sameSort _ (Unsorted _)  = False

-- | A term that is no type former: a neutral term whose type is a universe.
stuckSort :: Facts -> Sorted
stuckSort g = withV (termNeutral g) $ case fExpose (schemTypeM g) of
  Exposed steps [] (ToFacts _ ty) | steps <= schematicDepthLimit -> case fShape ty of
    SUniverse -> Sorted SObjectType mempty
    SCubeU -> Sorted SShape mempty
    STopeU -> Sorted SShape mempty
    _ -> Unsorted "expected an object type, parameter kind, or schematic rule"
  Exposed _ _ (ToFacts _ _) -> Unsorted "a type exposed under conditions"
  ExposedUnknown r -> Unsorted r
  Exposed{} -> Unsorted "a kind behind a definition or an argument"

-- | @object@: the type classifies as an object type.
sobject :: Facts -> SVerdict
sobject t = case classifyM t of
  Sorted SObjectType v -> v
  Sorted s _ ->
    SFail ("expected an object type, found " <> show s <> " for " <> shapeName (fShape t))
  Unsorted r -> SFail r

-- | @argument@: a type-family argument is checked pointwise.
sargument :: Facts -> Facts -> SVerdict
sargument dom arg = case functionHeadOf dom of
  Left r -> case fExpose dom of
    -- The domain is an argument: what it accepts is up to the use.
    Exposed _ [] (ToParam p) -> SNeeds IntSet.empty [SNeed p (SReqDomainFor [] arg)]
    Exposed _ [] (ToParamApp p ops) -> SNeeds IntSet.empty [SNeed p (SReqDomainFor ops arg)]
    _ -> SFail r
  Right (frames, h) -> case fShape h of
    SUniverse -> sobject arg
    SPi p -> case classifyM dom of
      Sorted SParameterKind v ->
        let applied = appFacts arg (varFacts (fAttrs arg) (piVar p))
        in v <> resolveScopes frames (sargument (piCodomain p) applied <> foldMap termM (piTope p))
      Sorted _ v -> v <> termM arg
      Unsorted r -> SFail r
    _ -> termM arg

-- | @applications@: the function, then the argument at the function's domain.
applications :: Facts -> SVerdict
applications f = case fShape f of
  SApp g x -> termM g <> case functionHeadOf (schemTypeM g) of
    Right (frames, h)
      | SPi p <- fShape h -> resolveScopes frames (sargument (piDomain p) x)
      | otherwise ->
          SFail $ "application without a schematic or object function type: "
            <> shapeName (fShape h)
    Left r -> SFail r
  _ -> mempty

-- | @term@.
termOf :: Facts -> SVerdict
termOf f = usesOf f <> case fShape f of
  SPi{} -> sortV (classifyM f)
  SSigma{} -> sortV (classifyM f)
  SId{} -> sortV (classifyM f)
  SRestricted{} -> sortV (classifyM f)
  SLam l -> lambdaDomain l <> termM (lamBody l)
  SApp{} -> applications f
  SLet l -> foldMap signatureM (letAnnotation l) <> termM (letValue l) <> termM (letBody l)
  STypeAsc value ty -> signatureM ty <> termM value
  SIdJ j ->
    sobject (jType j) <> sargument (schemTypeM (jMotive j)) (jMotive j)
      <> foldMap termM [jStart j, jMotive j, jBase j, jEnd j, jProof j]
  _ -> termNeutral f
  where
    -- A bare λ binds a variable of the domain of its own type.
    lambdaDomain l = case lamParam l of
      Just (LamParam dom mtope) -> foldMap termM mtope <> sortV (classifyM dom)
      Nothing -> case functionHeadOf (fType f) of
        Right (frames, h)
          | SPi p <- fShape h -> resolveScopes frames (sortV (classifyM (piDomain p)))
          | otherwise -> SFail "lambda without a function type"
        Left r -> SFail r

-- | @termNeutral@: an allow-list of what may appear in a checked term.
termNeutral :: Facts -> SVerdict
termNeutral f = case fShape f of
  SVar _ -> mempty
  SParam p -> SNeeds IntSet.empty [SNeed p SReqTerm]
  SUniverse -> mempty
  SCubeU -> mempty
  STopeU -> mempty
  SLeaf LHole -> SFail "unsupported schematic expression"
  SLeaf LInterval -> SFail "unsupported schematic expression"
  SLeaf _ -> mempty
  SCubeOrTope Admitted cs -> foldMap termM cs
  SCubeOrTope NotAdmitted _ -> SFail "unsupported schematic expression"
  SPair x y -> case functionHeadOf (fType f) of
    Right (frames, h) -> case fShape h of
      SSigma s -> resolveScopes frames (sargument (sigmaFirst s) x <> sargument (sigmaOpen s x) y)
      SCubeOrTope Admitted _ -> termM x <> termM y
      _ -> SFail "pair without a product type"
    Left r -> SFail r
  SFirst p -> termM p
  SSecond p -> termM p
  SRefl me -> foldMap (\(Endpoint x mty) -> sobject (fromMaybe (schemTypeM x) mty) <> termM x) me
  SRecOr faces -> foldMap (\(Face a b) -> termM a <> termM b) faces
  SApp{} -> applications f
  SScoped frame g -> resolveS frame (termNeutral g)
  _ -> SFail ("unsupported schematic expression: " <> shapeName (fShape f))

-- | @signature@.
signatureOf :: Facts -> SVerdict
signatureOf f = usesOf f <> inspectRedex f <> case fExpose f of
  Exposed steps [] (ToFacts _ g)
    | steps > schematicDepthLimit -> SFail "near the reduction limit"
    | SPi p <- fShape g ->
        sortV (classifyM (piDomain p)) <> foldMap termM (piTope p) <> signatureM (piCodomain p)
    | otherwise -> sortV (classifyM g)
  Exposed _ _ (ToFacts _ _) -> SFail "a signature exposed under conditions"
  Exposed steps [] (ToTable frame)
    | steps <= schematicDepthLimit -> resolveS frame (signatureM (tableBody frame))
    | otherwise -> SFail "near the reduction limit"
  Exposed steps [] (InTable frame g)
    | steps <= schematicDepthLimit -> resolveS frame (signatureM g)
    | otherwise -> SFail "near the reduction limit"
  Exposed _ [] (ToParam p) -> SNeeds IntSet.empty [SNeed p SReqSignature]
  ExposedUnknown r -> SFail r
  Exposed{} -> SFail "a signature exposed under conditions"

-- | @schematicTypeOf@: the type of a term as the check derives it, from the
-- current signatures rather than from an application's annotation.
schemTypeOf :: Facts -> Facts
schemTypeOf f = case fShape f of
  SVar v -> vfSchemType v
  SParam p -> fromMaybe (unknownFacts attrs "the type of an argument") (declaredOf p)
  SApp g x -> case functionHeadOf (schemTypeM g) of
    Right (frames, h)
      | SPi p <- fShape h -> underFrames frames (snd (piOpen p x))
      | otherwise -> unknownFacts attrs ("a function type that is " <> shapeName (fShape h))
    Left r -> unknownFacts attrs r
  SFirst p -> case functionHeadOf (schemTypeM p) of
    Right (frames, h)
      | SSigma s <- fShape h -> underFrames frames (sigmaFirst s)
      | SCubeOrTope Admitted [a, _] <- fShape h -> underFrames frames a
      | otherwise -> unknownFacts attrs ("a product type that is " <> shapeName (fShape h))
    Left r -> unknownFacts attrs r
  SSecond p -> case functionHeadOf (schemTypeM p) of
    Right (frames, h)
      | SSigma s <- fShape h ->
          let first = mkFacts attrs (SFirst p) (underFrames frames (sigmaFirst s))
          in underFrames frames (sigmaOpen s first)
      | SCubeOrTope Admitted [_, b] <- fShape h -> underFrames frames b
      | otherwise -> unknownFacts attrs ("a product type that is " <> shapeName (fShape h))
    Left r -> unknownFacts attrs r
  _ -> fType f
  where
    attrs = fAttrs f

-- | Settle the needs of a table's body against the arguments of a use. What
-- an argument meets may need more of the same arguments, so this repeats
-- until no need of this table is left.
resolveS :: Frame -> SVerdict -> SVerdict
resolveS frame = loop
  where
    mine = sameTable (frameTable frame) . paramTable . sneedParam
    loop = \case
      SNeeds uses ns | (own@(_ : _), others) <- partition mine ns ->
        loop (SOk uses <> foldMap settle own <> sneeds others)
      SNeeds uses ns -> SNeeds uses (map leaving ns)
      v -> v
    sneeds [] = mempty
    sneeds ns = SNeeds IntSet.empty ns
    settle (SNeed p r) =
      maybe (SFail "an argument out of range") (`evalS` r) (frameArg frame (paramIndex p))
    -- A need of another table that mentions this table's facts is settled
    -- under this use.
    leaving n@(SNeed p r) = case r of
      SReqArgument _ -> SNeed p (SReqUnder frame r)
      SReqDomainFor _ _ -> SNeed p (SReqUnder frame r)
      SReqUnder _ _ -> SNeed p (SReqUnder frame r)
      SReqSort _ -> n
      SReqTerm -> n
      SReqSignature -> n

-- | Meet a requirement on an argument.
evalS :: Facts -> SReq -> SVerdict
evalS f = \case
  SReqSort s -> case classifyM f of
    Sorted s' v
      | s' == s -> v
      | otherwise -> SFail ("the sort differs: " <> show s' <> " for " <> show s)
    Unsorted r -> SFail r
  SReqTerm -> termM f
  SReqSignature -> signatureM f
  SReqArgument dom -> sargument dom f
  SReqDomainFor ops x -> sargument (applyOps f ops) x
  SReqUnder frame r -> resolveS frame (evalS f r)
