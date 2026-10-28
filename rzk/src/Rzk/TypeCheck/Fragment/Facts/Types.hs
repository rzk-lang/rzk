{-# LANGUAGE LambdaCase #-}

-- | The types of the facts from which the RSTT checks are certified clean
-- (see "Rzk.TypeCheck.Fragment.Facts").
--
-- A 'Facts' node describes a term: its shape, the facts of its type, and
-- memo fields that hold what the evaluator would say of it and what each
-- check finds there. The memo fields are lazy and are computed by the
-- equations in 'Attrs', which the modules of the checks supply.
module Rzk.TypeCheck.Fragment.Facts.Types (
  -- * Positions
  FragmentUse (..),
  PrefixPosition (..),
  View (..),
  Entry (..),
  -- * Parameters and their needs
  Param (..),
  sameTable,
  Frame (..),
  Need (..),
  Req (..),
  CheckReq (..),
  Query (..),
  Op (..),
  -- * Verdicts
  Verdict (..),
  needs,
  isClean,
  verdictReason,
  Verdicts (..),
  both,
  inView,
  -- * Symbolic booleans
  SymBool (..),
  Answer (..),
  ParamQuery (..),
  known,
  unknown,
  withConditions,
  orSym,
  -- * Cube points
  Points (..),
  -- * Facts
  Facts (..),
  Derived (..),
  fExpose,
  fHeadView,
  fMeta,
  fLands,
  fCube,
  fPoints,
  Shape (..),
  Admission (..),
  Leaf (..),
  Face (..),
  Endpoint (..),
  PiFacts (..),
  SigmaFacts (..),
  IdFacts (..),
  LamFacts (..),
  LamParam (..),
  LetFacts (..),
  IdJFacts (..),
  spine,
  shapeName,
  -- * Exposure
  Exposed (..),
  Target (..),
  Origin (..),
  exposedTo,
  later,
  underConditions,
  exposeSteps,
  -- * Variables, tables and declarations
  VarFacts (..),
  TopLevel (..),
  Table (..),
  DeclFacts (..),
  -- * Memos
  ByFlags,
  byFlags,
  atFlags,
  ByEntry,
  byEntry,
  atEntry,
  -- * The schematic check
  SVerdict (..),
  SNeed (..),
  SReq (..),
  SSort (..),
  Sorted (..),
  Schem (..),
  -- * Attribute equations
  Attrs (..),
  mkFacts,
  unknownFacts,
) where

import qualified Data.IntSet             as IntSet

import           Language.Rzk.Foil.Names (Binder, TModality)
import           Rzk.TypeCheck.Context   (FragmentFlags (..))

-- * Positions

-- | What assumes a free-standing restriction, which conservativity allows to
-- be concluded but not assumed.
data FragmentUse
  = UseBinder
    -- ^ the type of a binder (λ, Π, Σ, or a declaration's parameter)
  | UseMotive
    -- ^ the motive of an eliminator
  | UseData
    -- ^ inside a type passed as data, at a meta-typed argument or component
  | UseIdentity
    -- ^ the type of an identity type, whose endpoints it types
  | UseConcluded
    -- ^ a concluded type, off the spine of codomains (a Σ component, say)
  deriving (Eq, Ord, Show)

-- | Whether a term sits in a schematic argument's parameter prefix.
data PrefixPosition = InParameterPrefix | OutsideParameterPrefix
  deriving (Eq, Ord, Show)

-- | The two views of the standalone walk: types as written, and types
-- exposed by unfolding definitions.
data View = SourceTypes | ComputedTypes
  deriving (Eq, Ord, Show)

-- | Where the standalone fragment walk enters a term (its functions @goTail@,
-- @goAssumed@, @goExtCodomain@, @goTerm@ and @goData@).
data Entry
  = ETail
    -- ^ a concluded type, along the spine of codomains
  | EAssumed PrefixPosition FragmentUse
    -- ^ an assumed type, assumed by this use
  | EExtCodomain FragmentUse
    -- ^ the codomain of an extension type, whose boundary may be clipped
  | ETerm PrefixPosition
    -- ^ a term
  | EData PrefixPosition
    -- ^ data, which is walked as an assumed type if it is a type
  deriving (Eq, Ord, Show)

-- * Parameters and their needs

-- | A parameter of a table: the arguments of a use stand for it.
data Param = Param
  { paramTable :: Table
    -- ^ the table whose parameter it is
  , paramIndex :: !Int
    -- ^ its position among the table's parameters, from 0
  }

-- | Whether two tables are the same definition's.
sameTable :: Table -> Table -> Bool
sameTable a b = tbId a == tbId b

-- | A use of a table: facts found in its body are settled against these
-- arguments.
data Frame = Frame
  { frameTable :: Table
  , frameArgs  :: [Facts]
    -- ^ one for each parameter
  }

-- | What a table asks of the argument for one of its parameters.
data Need = Need
  { needParam :: Param
  , needReq   :: Req
  }

-- | A requirement on an argument.
data Req
  = ReqAnswer [Op] Query Bool
    -- ^ the query answers so on the argument under these operations
  | ReqNoPoints
    -- ^ no outer cube point is reached through the argument
  | ReqCheck CheckReq
    -- ^ a requirement of the fragment check
  | ReqUnder Frame Req
    -- ^ the requirement came from the body of this use's table: what
    -- settling it asks of that table is settled against the use's arguments

-- | A requirement of the fragment check on an argument, which only the
-- check, with its flags, can settle.
data CheckReq
  = ReqClean View Entry
    -- ^ the argument is clean at this entry
  | ReqApplied View Entry [Op]
    -- ^ the argument, under these operations, is clean at this entry
  | ReqBoundary [Op]
    -- ^ the argument, under these operations and exposed, is not a
    -- restricted type whose faces reach an outer cube point
  | ReqArgumentAt View Facts
    -- ^ the argument is the type of a function applied to this (symbolic)
    -- argument, which passes the application rule's checks

-- | The evaluator's questions about a term read as a type, and about its type.
data Query
  = QMeta
    -- ^ @isMetaType@
  | QLands
    -- ^ read as a Π-chain, it ends in a universe
  | QCube
    -- ^ it is @CUBE@
  | QRestricted
    -- ^ @fragmentHead@ exposes it to a restricted type
  | QTypeMeta
    -- ^ 'QMeta' of its type
  | QTypeLands
    -- ^ 'QLands' of its type
  | QTypeCube
    -- ^ 'QCube' of its type
  | QDomainMeta
    -- ^ read as a function type, its domain is a schematic kind
  deriving (Eq, Ord, Show)

-- | What a table's body does to a parameter before looking at it.
data Op
  = OpApp Facts
    -- ^ applies it to this (symbolic) argument
  | OpFirst
    -- ^ takes the first component
  | OpSecond
    -- ^ takes the second component

-- * Verdicts

-- | What a certificate says of a term at an entry. 'Needs' arises only
-- inside a table, whose parameters are symbolic: the verdict holds if the
-- arguments of a use settle each need.
data Verdict
  = Clean
    -- ^ the standalone check reports nothing and does not fail
  | Unclean String
    -- ^ not certified, and why: a warning, or a fact the facts cannot settle
  | Needs [Need]
    -- ^ clean if the arguments meet these needs

instance Semigroup Verdict where
  Unclean r <> _ = Unclean r
  _ <> Unclean r = Unclean r
  Clean <> v = v
  v <> Clean = v
  Needs a <> Needs b = Needs (a <> b)

instance Monoid Verdict where
  mempty = Clean

-- | Clean if there is nothing to meet.
needs :: [Need] -> Verdict
needs [] = Clean
needs ns = Needs ns

isClean :: Verdict -> Bool
isClean Clean = True
isClean _     = False

-- | Why a verdict is not 'Clean' (for diagnostics).
verdictReason :: Verdict -> String
verdictReason = \case
  Clean -> "clean"
  Unclean r -> r
  Needs ns -> "unsettled needs: " <> unwords (map describe (take 3 ns))
  where
    describe (Need p r) = tbName (paramTable p) <> ":" <> describeReq r
    describeReq = \case
      ReqAnswer _ q b -> show q <> " is " <> show b
      ReqNoPoints -> "no points"
      ReqCheck (ReqClean v e) -> "clean at " <> show v <> " " <> show e
      ReqCheck (ReqApplied v e _) -> "applied clean at " <> show v <> " " <> show e
      ReqCheck (ReqBoundary _) -> "boundary"
      ReqCheck (ReqArgumentAt v _) -> "argument at " <> show v
      ReqUnder frame r -> describeReq r <> " under " <> tbName (frameTable frame)

-- | The verdicts of the fragment check in both views.
data Verdicts = Verdicts
  { sourceVerdict   :: Verdict
    -- ^ types as written
  , computedVerdict :: Verdict
    -- ^ types exposed
  }

instance Semigroup Verdicts where
  Verdicts s c <> Verdicts s' c' = Verdicts (s <> s') (c <> c')

instance Monoid Verdicts where
  mempty = Verdicts Clean Clean

-- | The same verdict in both views.
both :: Verdict -> Verdicts
both v = Verdicts v v

inView :: View -> Verdicts -> Verdict
inView SourceTypes   = sourceVerdict
inView ComputedTypes = computedVerdict

-- * Symbolic booleans

-- | A boolean fact with what it depends on: conditions under which the answer
-- holds, and an answer that may be a disjunction of queries on parameters.
data SymBool = SymBool
  { symConditions :: [Need]
  , symAnswer     :: Answer
  }

-- | The answer of a 'SymBool'.
data Answer
  = Known Bool
  | AnyOf [ParamQuery]
    -- ^ true if any of these queries on arguments answers 'True'
  | Unknown String
    -- ^ the facts cannot tell, and why

-- | A query asked of the argument for a parameter.
data ParamQuery = ParamQuery
  { pqParam  :: Param
  , pqOps    :: [Op]
    -- ^ what the body does to the parameter before the query
  , pqQuery  :: Query
  , pqFrames :: [Frame]
    -- ^ the uses of tables whose bodies the query came from, innermost
    -- first: it is answered under them
  }

known :: Bool -> SymBool
known = SymBool [] . Known

unknown :: String -> SymBool
unknown = SymBool [] . Unknown

withConditions :: [Need] -> SymBool -> SymBool
withConditions ns (SymBool ns' a) = SymBool (ns <> ns') a

-- | Disjunction, keeping the conditions of both sides.
orSym :: SymBool -> SymBool -> SymBool
orSym (SymBool ns a) (SymBool ns' b) = SymBool (ns <> ns') (orAnswer a b)
  where
    orAnswer (Known True) _          = Known True
    orAnswer _ (Known True)          = Known True
    orAnswer (Known False) y         = y
    orAnswer x (Known False)         = x
    orAnswer (AnyOf xs) (AnyOf ys)   = AnyOf (xs <> ys)
    orAnswer (Unknown r) _           = Unknown r
    orAnswer _ (Unknown r)           = Unknown r

-- * Cube points

-- | The free variables through which @outerPoints@ reaches a cube point.
-- Inside a table the account is partly up to the arguments: a variable whose
-- type is a parameter is a point if the argument is a cube, and an argument
-- may reach points of its own, directly or through a variable whose type
-- mentions it.
data Points = Points
  { pVars    :: IntSet.IntSet
    -- ^ the keys of the variables through which a point is reached
  , pIfCube  :: [(Int, Param)]
    -- ^ variables that are points if the argument for the parameter is a cube
  , pParams  :: [(Maybe Int, Param)]
    -- ^ parameters whose arguments may reach a point, with the variable
    -- through which, if any
  , pNeeds   :: [Need]
    -- ^ conditions under which this is the whole account
  , pUnknown :: Maybe String
    -- ^ the facts cannot tell, and why
  }

instance Semigroup Points where
  Points a b c d e <> Points a' b' c' d' e' =
    Points (IntSet.union a a') (b <> b') (c <> c') (d <> d') (maybe e' Just e)

instance Monoid Points where
  mempty = Points IntSet.empty [] [] [] Nothing

-- * Facts

-- | The facts about a term. The fields are lazy on purpose: they are
-- attributes computed from the children's on first use, and most are never
-- asked.
data Facts = Facts
  { fShape    :: Shape
    -- ^ the node, with the facts of its children
  , fType     :: Facts
    -- ^ the facts of its type: the annotation, or a variable's declared type
  , fDerived  :: Derived
    -- ^ what the evaluator would say of it
  , fVerdicts :: ByFlags (ByEntry Verdicts)
    -- ^ the fragment check at each entry, in both views: a term reached
    -- through several uses of a definition is walked once
  , fSchem    :: Schem
    -- ^ what the schematic check finds
  , fDepth    :: Int
    -- ^ the depth of the term, which bounds the fuel the standalone checks use
  , fAttrs    :: Attrs
    -- ^ the equations of the memo fields, for the facts built from these
  }

-- | The evaluator's answers about a term, read as a type.
data Derived = Derived
  { dExpose   :: Exposed
    -- ^ what @fragmentHead@ exposes the term to
  , dHeadView :: Exposed
    -- ^ what @whnfT@ followed by @stripTypeRestrictions@ exposes it to
  , dMeta     :: SymBool
    -- ^ @isMetaType@
  , dLands    :: SymBool
    -- ^ read as a Π-chain, it ends in a universe (@landsInUniverse@)
  , dCube     :: SymBool
    -- ^ it is @CUBE@ (@isCubeUniverse@)
  , dPoints   :: Points
    -- ^ the free variables through which @outerPoints@ reaches a cube point
  }

fExpose, fHeadView :: Facts -> Exposed
fExpose = dExpose . fDerived
fHeadView = dHeadView . fDerived

fMeta, fLands, fCube :: Facts -> SymBool
fMeta = dMeta . fDerived
fLands = dLands . fDerived
fCube = dCube . fDerived

fPoints :: Facts -> Points
fPoints = dPoints . fDerived

-- | A node of a term, with the facts of its children.
data Shape
  = SVar VarFacts
  | SParam Param
    -- ^ a parameter of a table, standing for an argument
  | SParamType Param (Maybe Facts)
    -- ^ the type of the argument a parameter stands for, with the
    -- parameter's declared type if known
  | SUniverse
  | SCubeU
  | STopeU
  | SLeaf Leaf
    -- ^ a constant with nothing inside
  | SCubeOrTope Admission [Facts]
    -- ^ an operation on cubes or topes, with its operands
  | SPi PiFacts
  | SSigma SigmaFacts
  | SId IdFacts
  | SRestricted Facts [Face]
    -- ^ a type restricted by faces
  | SRecOr [Face]
    -- ^ a case split on topes
  | SApp Facts Facts
    -- ^ a function and its argument
  | SLam LamFacts
  | SLet LetFacts
  | SPair Facts Facts
  | SFirst Facts
  | SSecond Facts
  | SRefl (Maybe Endpoint)
    -- ^ @refl@, with its endpoint if written
  | SIdJ IdJFacts
  | STypeAsc Facts Facts
    -- ^ a term and the type it is ascribed
  | STypeModal Facts
  | SScoped Frame Facts
    -- ^ facts from the body of a table at a use: whatever is found about
    -- them is settled against the use's arguments
  | SOther String
    -- ^ not modelled: the certificate gives up

-- | Whether the schematic check admits an operation on cubes or topes.
data Admission = Admitted | NotAdmitted
  deriving (Eq)

-- | The constants, as far as the checks tell them apart.
data Leaf
  = LTypeUnit
    -- ^ an object type
  | LRecBottom
    -- ^ an object type
  | LCubeUnit
    -- ^ a cube
  | LCube2
    -- ^ a cube
  | LOtherConst
    -- ^ points, topes, @unit@
  | LInterval
    -- ^ the auxiliary interval and its endpoints, which the schematic check
    -- does not admit
  | LHole
  deriving (Eq)

-- | A face of a restriction or a branch of a case split.
data Face = Face
  { faceTope :: Facts
  , faceTerm :: Facts
  }

-- | An endpoint of @refl@, with its type if written.
data Endpoint = Endpoint
  { endpointTerm :: Facts
  , endpointType :: Maybe Facts
  }

-- | A Π-type.
data PiFacts = PiFacts
  { piOrig     :: Binder
  , piModality :: TModality
  , piDomain   :: Facts
  , piVar      :: VarFacts
    -- ^ the bound variable
  , piTope     :: Maybe Facts
    -- ^ the shape's tope, opened under the variable
  , piCodomain :: Facts
    -- ^ opened under the variable
  , piOpen     :: Facts -> (Maybe Facts, Facts)
    -- ^ the tope and the codomain with something else for the variable
  }

-- | A Σ-type.
data SigmaFacts = SigmaFacts
  { sigmaOrig     :: Binder
  , sigmaModality :: TModality
  , sigmaFirst    :: Facts
  , sigmaVar      :: VarFacts
  , sigmaSecond   :: Facts
    -- ^ opened under the variable
  , sigmaOpen     :: Facts -> Facts
    -- ^ the second component's type with something else for the variable
  }

-- | An identity type.
data IdFacts = IdFacts
  { idLeft  :: Facts
  , idType  :: Maybe Facts
    -- ^ the type of the endpoints, if written
  , idRight :: Facts
  }

-- | A λ-abstraction.
data LamFacts = LamFacts
  { lamOrig     :: Binder
  , lamModality :: TModality
  , lamParam    :: Maybe LamParam
    -- ^ the parameter, if its type is written
  , lamVar      :: VarFacts
  , lamBody     :: Facts
    -- ^ opened under the variable
  , lamOpen     :: Facts -> Facts
    -- ^ the body with something else for the variable
  }

-- | A written λ-parameter.
data LamParam = LamParam
  { lamDomain :: Facts
  , lamTope   :: Maybe Facts
    -- ^ opened under the variable
  }

-- | A let-binding.
data LetFacts = LetFacts
  { letOrig       :: Binder
  , letAnnotation :: Maybe Facts
  , letValue      :: Facts
  , letVar        :: VarFacts
    -- ^ the bound variable, which has the value
  , letBody       :: Facts
    -- ^ opened under the variable
  }

-- | An identity elimination (@idJ@).
data IdJFacts = IdJFacts
  { jType   :: Facts
  , jStart  :: Facts
  , jMotive :: Facts
  , jBase   :: Facts
    -- ^ the case for @refl@
  , jEnd    :: Facts
  , jProof  :: Facts
  }

-- | The head of an application spine and its arguments, each with the
-- function node it is applied to.
spine :: Facts -> (Facts, [(Facts, Facts)])
spine = go []
  where
    go acc f = case fShape f of
      SApp fn x -> go ((fn, x) : acc) fn
      _         -> (f, acc)

-- | The name of a shape, for diagnostics.
shapeName :: Shape -> String
shapeName = \case
  SVar _ -> "a variable"
  SParam p -> "parameter " <> show (paramIndex p) <> " of " <> tbName (paramTable p)
  SParamType p _ ->
    "the type of parameter " <> show (paramIndex p) <> " of " <> tbName (paramTable p)
  SUniverse -> "U"
  SCubeU -> "CUBE"
  STopeU -> "TOPE"
  SLeaf _ -> "a constant"
  SCubeOrTope _ _ -> "a cube or tope operation"
  SPi{} -> "a function type"
  SSigma{} -> "a pair type"
  SId{} -> "an identity type"
  SRestricted{} -> "a restricted type"
  SRecOr{} -> "a case split"
  SApp g _ -> "an application of " <> shapeName (fShape (fst (spine g)))
  SLam{} -> "a lambda"
  SLet{} -> "a let"
  SPair{} -> "a pair"
  SFirst{} -> "a first projection"
  SSecond{} -> "a second projection"
  SRefl{} -> "refl"
  SIdJ{} -> "an identity elimination"
  STypeAsc{} -> "an ascription"
  STypeModal{} -> "a modal type"
  SScoped frame _ -> "facts from the body of " <> tbName (frameTable frame)
  SOther r -> r

-- * Exposure

-- | What exposing a term gives: after some reduction steps and under some
-- conditions, a target; or nothing the facts can tell.
data Exposed
  = Exposed !Int [Need] Target
    -- ^ the number of steps (an upper bound), the conditions, the target
  | ExposedUnknown String

-- | What a term is exposed to.
data Target
  = ToFacts Origin Facts
    -- ^ a constructor, a lambda, or a stuck term
  | ToTable Frame
    -- ^ a saturated application of a definition with a table: its body,
    -- with the parameters standing for the use's arguments
  | InTable Frame Facts
    -- ^ facts from the body of a table at a use (a component of the body,
    -- say): what they say is settled against the use's arguments
  | ToParam Param
    -- ^ the argument a parameter stands for
  | ToParamApp Param [Op]
    -- ^ that argument, under these operations
  | ToParamType Param
    -- ^ the type of that argument

-- | Whether a term exposes to itself.
data Origin = Itself | Reduced
  deriving (Eq)

-- | A term that exposes to itself, at no cost.
exposedTo :: Facts -> Exposed
exposedTo = Exposed 0 [] . ToFacts Itself

-- | Count more reduction steps.
later :: Int -> Exposed -> Exposed
later k (Exposed s ns t) = Exposed (s + k) ns t
later _ e                = e

-- | Add conditions.
underConditions :: [Need] -> Exposed -> Exposed
underConditions ns (Exposed s ns' t) = Exposed s (ns <> ns') t
underConditions _ e                  = e

exposeSteps :: Exposed -> Int
exposeSteps (Exposed s _ _)    = s
exposeSteps (ExposedUnknown _) = maxBound

-- * Variables, tables and declarations

-- | A variable: one bound in a term, a top-level entry, or a table's parameter.
data VarFacts = VarFacts
  { vfKey       :: !Int
    -- ^ unique in a run, so that points can be told apart across scopes
  , vfOrig      :: Binder
  , vfType      :: Facts
  , vfSchemType :: Facts
    -- ^ the type as the schematic check derives it, which for a let-bound
    -- variable is its annotation or the type derived for the value
  , vfValue     :: Maybe Facts
  , vfTop       :: Maybe TopLevel
    -- ^ for a top-level entry
  , vfReach     :: Points
    -- ^ what @outerPoints@ reaches through it: the variable itself if it is
    -- a cube point, and whatever its type and value reach
  }

-- | What is known about a top-level entry.
data TopLevel = TopLevel
  { tlTable        :: Maybe Table
    -- ^ for a definition
  , tlDataRole     :: Bool
    -- ^ part of a @#data@ declaration
  , tlMetaPrefix   :: Int
    -- ^ the length of its meta-parameter prefix (see "Rzk.TypeCheck.MetaPrefix")
  , tlObjectFamily :: Maybe (Int, SVerdict)
    -- ^ the arity at which it is an object-valued family, as
    -- @knownObjectFamily@ probes it, with what the probe relies on
  }

-- | A definition's value with its parameters symbolic: the facts of its body,
-- with the memos computed once.
data Table = Table
  { tbId       :: !Int
    -- ^ the entry's name identifier
  , tbName     :: String
    -- ^ for diagnostics
  , tbArity    :: !Int
    -- ^ the leading lambdas of the value, which the parameters are
  , tbDeclared :: [Maybe Facts]
    -- ^ the parameters' declared types, where known
  , tbBody     :: Facts
  }

-- | The facts of a declaration under check, built once for its checks.
data DeclFacts = DeclFacts
  { declType  :: Facts
  , declValue :: Maybe Facts
  }

-- * Memos

-- | A memo over the two booleans, lazy in each.
data ByBool a = ByBool
  { whenFalse :: a
  , whenTrue  :: a
  }

byBool :: (Bool -> a) -> ByBool a
byBool f = ByBool (f False) (f True)

atBool :: Bool -> ByBool a -> a
atBool False = whenFalse
atBool True  = whenTrue

-- | A memo over the combinations of 'FragmentFlags', lazy in each.
newtype ByFlags a = ByFlags (ByBool (ByBool (ByBool a)))

byFlags :: (FragmentFlags -> a) -> ByFlags a
byFlags f = ByFlags $
  byBool $ \restriction -> byBool $ \binder -> byBool $ \dependency -> f FragmentFlags
    { flagFreeStandingRestriction = restriction
    , flagMetaBinder = binder
    , flagShapeDependency = dependency
    }

atFlags :: FragmentFlags -> ByFlags a -> a
atFlags fl (ByFlags m) =
  atBool (flagShapeDependency fl) $
    atBool (flagMetaBinder fl) $
      atBool (flagFreeStandingRestriction fl) m

-- | A memo over the prefix positions.
data ByPrefix a = ByPrefix
  { inPrefix      :: a
  , outsidePrefix :: a
  }

byPrefix :: (PrefixPosition -> a) -> ByPrefix a
byPrefix f = ByPrefix (f InParameterPrefix) (f OutsideParameterPrefix)

atPrefix :: PrefixPosition -> ByPrefix a -> a
atPrefix InParameterPrefix      = inPrefix
atPrefix OutsideParameterPrefix = outsidePrefix

-- | A memo over the uses.
data ByUse a = ByUse
  { atBinder    :: a
  , atMotive    :: a
  , atData      :: a
  , atIdentity  :: a
  , atConcluded :: a
  }

byUse :: (FragmentUse -> a) -> ByUse a
byUse f = ByUse (f UseBinder) (f UseMotive) (f UseData) (f UseIdentity) (f UseConcluded)

atUse :: FragmentUse -> ByUse a -> a
atUse = \case
  UseBinder -> atBinder
  UseMotive -> atMotive
  UseData -> atData
  UseIdentity -> atIdentity
  UseConcluded -> atConcluded

-- | A memo over the entries, lazy in each.
data ByEntry a = ByEntry
  { memoTail        :: a
  , memoAssumed     :: ByPrefix (ByUse a)
  , memoExtCodomain :: ByUse a
  , memoTerm        :: ByPrefix a
  , memoData        :: ByPrefix a
  }

byEntry :: (Entry -> a) -> ByEntry a
byEntry f = ByEntry
  { memoTail = f ETail
  , memoAssumed = byPrefix $ \p -> byUse (f . EAssumed p)
  , memoExtCodomain = byUse (f . EExtCodomain)
  , memoTerm = byPrefix (f . ETerm)
  , memoData = byPrefix (f . EData)
  }

atEntry :: Entry -> ByEntry a -> a
atEntry = \case
  ETail -> memoTail
  EAssumed p u -> atUse u . atPrefix p . memoAssumed
  EExtCodomain u -> atUse u . memoExtCodomain
  ETerm p -> atPrefix p . memoTerm
  EData p -> atPrefix p . memoData

-- * The schematic check

-- | What the schematic check finds: nothing to report, with the top-level
-- entries it relies on; a failure; or, in a table, needs on the arguments
-- with the entries relied on so far.
data SVerdict
  = SOk IntSet.IntSet
  | SFail String
  | SNeeds IntSet.IntSet [SNeed]

instance Semigroup SVerdict where
  SFail r <> _ = SFail r
  _ <> SFail r = SFail r
  SOk a <> SOk b = SOk (IntSet.union a b)
  SOk a <> SNeeds b ns = SNeeds (IntSet.union a b) ns
  SNeeds a ns <> SOk b = SNeeds (IntSet.union a b) ns
  SNeeds a ns <> SNeeds b ms = SNeeds (IntSet.union a b) (ns <> ms)

instance Monoid SVerdict where
  mempty = SOk IntSet.empty

-- | What the schematic check of a table asks of an argument.
data SNeed = SNeed
  { sneedParam :: Param
  , sneedReq   :: SReq
  }

-- | A requirement of the schematic check on an argument.
data SReq
  = SReqSort SSort
    -- ^ the argument classifies as this sort
  | SReqTerm
    -- ^ the argument is a term the check accepts
  | SReqSignature
    -- ^ the argument is a signature the check accepts
  | SReqArgument Facts
    -- ^ the argument is accepted at this (symbolic) domain
  | SReqDomainFor [Op] Facts
    -- ^ the argument, under these operations, is the domain at which this
    -- (symbolic) argument is accepted
  | SReqUnder Frame SReq
    -- ^ the requirement came from the body of this use's table

-- | The sorts of "Rzk.TypeCheck.Schematic".
data SSort = SObjectType | SParameterKind | SSchematicRule | SShape
  deriving (Eq, Show)

-- | The classification of a type, with what finding it relied on.
data Sorted
  = Sorted SSort SVerdict
  | Unsorted String
    -- ^ the classification fails, or the facts cannot tell, and why

-- | The memo of the schematic check on a term.
data Schem = Schem
  { scSort      :: Sorted
    -- ^ @classify@
  , scHead      :: Sorted
    -- ^ @classifyHead@: the classification of what the term exposes to
  , scTerm      :: SVerdict
    -- ^ @term@
  , scSignature :: SVerdict
    -- ^ @signature@
  , scType      :: Facts
    -- ^ @schematicTypeOf@
  }

-- * Attribute equations

-- | The equations of the memo fields of 'Facts'. The checks' modules supply
-- them, and "Rzk.TypeCheck.Fragment.Facts" puts them together; building facts
-- takes them as an argument, so that the modules do not import each other.
data Attrs = Attrs
  { attrDerived        :: Facts -> Derived
  , attrVerdicts       :: Facts -> ByFlags (ByEntry Verdicts)
  , attrSchem          :: Facts -> Schem
  , attrScopedVerdicts :: Frame -> Facts -> ByFlags (ByEntry Verdicts)
    -- ^ for facts from a table's body at a use
  , attrScopedSchem    :: Frame -> Facts -> Schem
    -- ^ for facts from a table's body at a use
  }

-- | A node with the given shape and type, its memo fields computed by the
-- equations.
mkFacts :: Attrs -> Shape -> Facts -> Facts
mkFacts attrs sh ty = f
  where
    f = Facts
      { fShape = sh
      , fType = ty
      , fDerived = attrDerived attrs f
      , fVerdicts = attrVerdicts attrs f
      , fSchem = attrSchem attrs f
      , fDepth = depthOf sh
      , fAttrs = attrs
      }

-- | A node the facts cannot model, and why.
unknownFacts :: Attrs -> String -> Facts
unknownFacts attrs r = f
  where
    f = mkFacts attrs (SOther r) f

-- | The depth of a term, through the default openings of its binders.
depthOf :: Shape -> Int
depthOf sh = 1 + maximum (0 : map fDepth (children sh))

-- | The children of a node, through the default openings of its binders.
children :: Shape -> [Facts]
children = \case
  SVar _ -> []
  SParam _ -> []
  SParamType _ _ -> []
  SUniverse -> []
  SCubeU -> []
  STopeU -> []
  SLeaf _ -> []
  SCubeOrTope _ cs -> cs
  SPi p -> piDomain p : piCodomain p : maybe [] pure (piTope p)
  SSigma s -> [sigmaFirst s, sigmaSecond s]
  SId i -> idLeft i : idRight i : maybe [] pure (idType i)
  SRestricted ty faces -> ty : concatMap faceChildren faces
  SRecOr faces -> concatMap faceChildren faces
  SApp g x -> [g, x]
  SLam l -> lamBody l : maybe [] paramChildren (lamParam l)
  SLet l -> letValue l : letBody l : maybe [] pure (letAnnotation l)
  SPair a b -> [a, b]
  SFirst p -> [p]
  SSecond p -> [p]
  SRefl me -> maybe [] (\e -> endpointTerm e : maybe [] pure (endpointType e)) me
  SIdJ j -> [jType j, jStart j, jMotive j, jBase j, jEnd j, jProof j]
  STypeAsc v ty -> [v, ty]
  STypeModal ty -> [ty]
  SScoped _ g -> [g]
  SOther _ -> []
  where
    faceChildren (Face a b) = [a, b]
    paramChildren (LamParam d mt) = d : maybe [] pure mt
