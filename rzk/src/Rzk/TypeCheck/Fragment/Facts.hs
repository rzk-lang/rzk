{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Facts about elaborated terms, from which the RSTT fragment check is
-- certified clean without being run.
--
-- The standalone check ("Rzk.TypeCheck.Fragment.RSTT") walks a declaration
-- and asks the evaluator about the terms it meets: what a type unfolds to,
-- whether a domain is a schematic kind, whether a term's type lands in a
-- universe, which cube points a tope depends on. This module answers the same
-- questions from the structure of the term and from facts about the top-level
-- definitions it mentions. A definition's facts are computed once, with its
-- parameters left symbolic (its 'Table'); where a fact depends on the
-- argument a parameter stands for, the fact records a 'Need', and a use of
-- the definition settles it against the argument.
--
-- The facts of a node are computed from the facts of its children and are
-- lazy, so only what a check asks for is computed: they are synthesised
-- attributes in the sense of Knuth's attribute grammars (D. E. Knuth,
-- Semantics of context-free languages, Mathematical Systems Theory 2, 1968).
-- The tables play the part of procedure summaries in interprocedural
-- analysis (M. Sharir and A. Pnueli, Two approaches to interprocedural data
-- flow analysis, 1981).
--
-- The certificate is one-sided. 'Clean' means that the standalone check would
-- report nothing and would not fail; anything else means that the standalone
-- check runs. So the facts never reproduce a warning; they recognise, exactly,
-- when there is none. Every decision below follows the corresponding decision
-- of the standalone walk, which is the specification.
--
-- The standalone walks ask the evaluator ("Rzk.TypeCheck.Eval": 'whnfT',
-- 'fragmentHead', 'isMetaType') and report an incomplete check when a query
-- fails. The facts are computed without the evaluator, so they cannot see
-- such a failure: 'Clean' assumes that the walk's queries succeed on the
-- well-typed terms it is given. Running out of fuel is the exception that is
-- modelled, by the bounds below.
module Rzk.TypeCheck.Fragment.Facts (
  -- * Positions
  FragmentUse (..),
  PrefixPosition (..),
  View (..),
  Entry (..),
  -- * Verdicts
  Verdict (..),
  isClean,
  verdictReason,
  -- * Facts
  Facts,
  VarFacts,
  Table,
  Env (..),
  facts,
  topLevelFacts,
  DeclFacts,
  declarationFactsIn,
  fragmentVerdict,
  -- * The schematic check
  SVerdict (..),
  schematicVerdict,
  schematicVerdictOf,
  schematicDepthLimit,
  -- * The meta-parameter layer check
  metaPrefixVerdict,
) where

import           Control.Applicative      ((<|>))
import qualified Data.IntMap              as IntMap
import qualified Data.IntSet              as IntSet
import           Data.List                (find, partition)
import           Data.Maybe               (fromMaybe, isJust)
import qualified Data.Set                 as Set

import           Control.Monad.Foil       (Distinct, NameBinder)
import qualified Control.Monad.Foil       as Foil
import           Control.Monad.Free.Foil  (AST (Var))

import           Language.Rzk.Foil.Names  (Binder, TModality (..),
                                           TypeInfo (..), binderName)
import           Language.Rzk.Foil.Syntax
import           Rzk.TypeCheck.Context

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

-- | Where the standalone walk enters a term.
data Entry
  = ETail
  | EAssumed PrefixPosition FragmentUse
  | EExtCodomain FragmentUse
  | ETerm PrefixPosition
  | EData PrefixPosition
  deriving (Eq, Ord, Show)

allFlags :: [FragmentFlags]
allFlags =
  [ FragmentFlags { flagFreeStandingRestriction = a, flagMetaBinder = b, flagShapeDependency = c }
  | a <- bools, b <- bools, c <- bools ]
  where bools = [False, True]

-- * Verdicts

-- | What the certificate says of a term at an entry. 'Needs' arises only
-- inside a table, whose parameters are symbolic: the verdict holds if the
-- arguments settle each need.
data Verdict
  = Clean
  | Unclean String
    -- ^ not certified, and why (a warning, or a fact the facts cannot settle)
  | Needs [Need]

instance Semigroup Verdict where
  Unclean r <> _ = Unclean r
  _ <> Unclean r = Unclean r
  Clean <> v = v
  v <> Clean = v
  Needs a <> Needs b = Needs (a <> b)

instance Monoid Verdict where
  mempty = Clean

isClean :: Verdict -> Bool
isClean Clean = True
isClean _     = False

-- | Why a verdict is not 'Clean' (for diagnostics).
verdictReason :: Verdict -> String
verdictReason = \case
  Clean -> "clean"
  Unclean r -> r
  Needs ns -> "unsettled needs: " <> unwords [tbName t <> ":" <> describeReq r | Need t _ r <- take 3 ns]
  where
    describeReq = \case
      ReqClean v e -> "clean at " <> show v <> " " <> show e
      ReqApplied v e _ -> "applied clean at " <> show v <> " " <> show e
      ReqNoPoints -> "no points"
      ReqAnswer _ q b -> show q <> " is " <> show b
      ReqBoundary _ -> "boundary"
      ReqArgumentAt v _ -> "argument at " <> show v
      ReqUnder t _ r -> describeReq r <> " under " <> tbName t

needs :: [Need] -> Verdict
needs [] = Clean
needs ns = Needs ns

-- | What a table asks of the argument for one of its parameters.
data Need = Need Table Int Req

data Req
  = ReqClean View Entry
    -- ^ the argument is clean at this entry
  | ReqApplied View Entry [Op]
    -- ^ the argument, under these operations, is clean at this entry
  | ReqNoPoints
    -- ^ no outer cube point is reached through the argument
  | ReqAnswer [Op] Query Bool
    -- ^ the query answers so on the argument under these operations
  | ReqBoundary [Op]
    -- ^ the argument, under these operations and exposed, is not a
    -- restricted type whose faces reach an outer cube point
  | ReqArgumentAt View Facts
    -- ^ the argument is the type of a function applied to this (symbolic)
    -- argument, which passes the application rule's checks
  | ReqUnder Table [Facts] Req
    -- ^ the need, whose facts come from the body of this table at a use with
    -- these arguments: what settling it asks of that table is settled against them

-- | The evaluator's questions about a term read as a type, and about its type.
data Query
  = QMeta | QLands | QCube | QRestricted
  | QTypeMeta | QTypeLands | QTypeCube
  | QDomainMeta
    -- ^ read as a function type, its domain is a schematic kind
  deriving (Eq, Ord, Show)

-- | A boolean fact with what it depends on: conditions under which the answer
-- holds, and an answer that may be a disjunction of queries on parameters.
data SymBool = SymBool [Need] Answer

-- | A disjunction of queries on parameters.
data Answer = Known Bool | AnyOf [ParamQuery] | Unknown String

-- | A query asked of the argument for a parameter, under the given
-- operations, which may come from the bodies of tables at uses with the
-- given arguments (innermost first).
data ParamQuery = ParamQuery Table Int [Op] Query [(Table, [Facts])]

-- | What a body does to a parameter before it is looked at: applies it to a
-- (symbolic) argument, or projects it.
data Op = OpApp Facts | OpFirst | OpSecond

applyOp :: Facts -> Op -> Facts
applyOp f = \case
  OpApp x -> appFacts f x
  OpFirst -> firstFacts f
  OpSecond -> secondFacts f

applyOps :: Facts -> [Op] -> Facts
applyOps = foldl applyOp

-- | The operation with its argument, if any, from the body of a table at a use.
scopeOp :: (Facts -> Facts) -> Op -> Op
scopeOp f (OpApp x) = OpApp (f x)
scopeOp _ o = o

known :: Bool -> SymBool
known = SymBool [] . Known

unknown :: String -> SymBool
unknown = SymBool [] . Unknown

withConditions :: [Need] -> SymBool -> SymBool
withConditions ns (SymBool ns' a) = SymBool (ns <> ns') a

orSym :: SymBool -> SymBool -> SymBool
orSym (SymBool ns a) (SymBool ns' b) = SymBool (ns <> ns') (a `orAnswer` b)
  where
    orAnswer (Known True) _ = Known True
    orAnswer _ (Known True) = Known True
    orAnswer (Known False) y = y
    orAnswer x (Known False) = x
    orAnswer (AnyOf xs) (AnyOf ys) = AnyOf (xs <> ys)
    orAnswer (Unknown r) _ = Unknown r
    orAnswer _ (Unknown r) = Unknown r

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
guardOf (ParamQuery t j as q frames) b =
  Need t j (foldl (\r (t', args) -> ReqUnder t' args r) (ReqAnswer as q b) frames)

-- | The answer must be this one.
mustBe :: String -> Bool -> SymBool -> Verdict
-- A single query that must answer 'True' becomes a need; a disjunction cannot.
mustBe _ True (SymBool ns (AnyOf [q])) = needs (ns <> [guardOf q True])
mustBe what b s = let (b', v) = decide s in v <> if b == b' then Clean else Unclean (what <> " differs: " <> describe s)
  where
    describe (SymBool _ (Known x)) = show x
    describe (SymBool _ (AnyOf qs)) = "any of " <> show [q | ParamQuery _ _ _ q _ <- qs]
    describe (SymBool _ (Unknown r)) = r

-- | The free variables through which 'outerPoints' reaches a cube point. A
-- variable is recorded by its key; the binder's own variable is left out of
-- account where the walk starts under it. Inside a table, the account is
-- partly up to the arguments: a variable whose type is a parameter is a point
-- if the argument is a cube, and a parameter's argument may reach points of
-- its own, directly or through a variable whose type mentions it.
data Points = Points
  { pVars    :: IntSet.IntSet
    -- ^ variables through which a point is reached
  , pIfCube  :: [(Int, Table, Int)]
    -- ^ variables that are points if the argument for the parameter is a cube
  , pParams  :: [(Maybe Int, Table, Int)]
    -- ^ parameters whose arguments may reach a point, and the variable through
    -- which, if any
  , pNeeds   :: [Need]
    -- ^ conditions under which this is the whole account
  , pUnknown :: Maybe String
  }

instance Semigroup Points where
  Points a b c d e <> Points a' b' c' d' e' =
    Points (IntSet.union a a') (b <> b') (c <> c') (d <> d') (e <|> e')

instance Monoid Points where
  mempty = Points IntSet.empty [] [] [] Nothing

-- | Leave a binder's own variable out of account.
without :: VarFacts -> Points -> Points
without bv p = p
  { pVars = IntSet.delete key (pVars p)
  , pIfCube = [e | e@(k, _, _) <- pIfCube p, k /= key]
  , pParams = [e | e@(via, _, _) <- pParams p, via /= Just key]
  }
  where key = vfKey bv

-- | No point is reached, leaving the binder's own variable out of account.
noPointsBut :: Maybe VarFacts -> Points -> Verdict
noPointsBut bound points = case pUnknown p of
  Just r -> Unclean r
  Nothing
    | not (IntSet.null (pVars p)) -> Unclean "an outer cube point"
    | otherwise ->
        needs (pNeeds p)
          <> needs [Need t j (ReqAnswer [] QTypeCube False) | (_, t, j) <- pIfCube p]
          <> needs [Need t j ReqNoPoints | (_, t, j) <- pParams p]
  where
    p = maybe points (`without` points) bound

-- * Facts

-- | The facts about a term. Every field is lazy and computed on first use.
data Facts = Facts
  { fShape    :: Shape
    -- ^ the node, with the facts of its children
  , fType     :: Facts
    -- ^ the facts of its type: the annotation, or a variable's declared type
  , fDerived  :: Derived
    -- ^ what the evaluator would say of it, behind one thunk: most nodes are
    -- only walked, and never asked
  , fVerdicts :: ByFlags ByEntry
    -- ^ the verdicts of the fragment check at each entry, in both views,
    -- computed once: a term reached through several uses of a definition is
    -- walked once, and the two views part only where a term reduces
  , fSchem    :: Schem
    -- ^ what the schematic check finds about the term, computed once
  , fDepth    :: Int
    -- ^ the depth of the term, which bounds the fuel the standalone checks use
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
    -- ^ @landsGo@: read as a Π-chain, it ends in a universe
  , dCube     :: SymBool
    -- ^ @isCubeUniverse@: it is @CUBE@
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

-- | A memo over the eight combinations of 'FragmentFlags', lazy in each.
data ByFlags a = ByFlags a a a a a a a a

byFlags :: (FragmentFlags -> a) -> ByFlags a
byFlags f = ByFlags (g 0) (g 1) (g 2) (g 3) (g 4) (g 5) (g 6) (g 7)
  where g i = f (allFlags !! i)

atFlags :: FragmentFlags -> ByFlags a -> a
atFlags flags (ByFlags x0 x1 x2 x3 x4 x5 x6 x7) =
  case (flagFreeStandingRestriction flags, flagMetaBinder flags, flagShapeDependency flags) of
  (False, False, False) -> x0
  (False, False, True)  -> x1
  (False, True, False)  -> x2
  (False, True, True)   -> x3
  (True, False, False)  -> x4
  (True, False, True)   -> x5
  (True, True, False)   -> x6
  (True, True, True)    -> x7

-- | The verdicts of both views: as written, and with types exposed.
type Verdicts = (Verdict, Verdict)

inView :: View -> Verdicts -> Verdict
inView SourceTypes = fst
inView ComputedTypes = snd

-- | A memo over the entries, lazy in each.
data ByEntry = ByEntry
  { mTail       :: Verdicts
  , mAssumedIn  :: ByUse
  , mAssumedOut :: ByUse
  , mExt        :: ByUse
  , mTermIn     :: Verdicts
  , mTermOut    :: Verdicts
  , mDataIn     :: Verdicts
  , mDataOut    :: Verdicts
  }

data ByUse = ByUse Verdicts Verdicts Verdicts Verdicts Verdicts

byEntry :: (Entry -> Verdicts) -> ByEntry
byEntry f = ByEntry
  { mTail = f ETail
  , mAssumedIn = byUse (f . EAssumed InParameterPrefix)
  , mAssumedOut = byUse (f . EAssumed OutsideParameterPrefix)
  , mExt = byUse (f . EExtCodomain)
  , mTermIn = f (ETerm InParameterPrefix)
  , mTermOut = f (ETerm OutsideParameterPrefix)
  , mDataIn = f (EData InParameterPrefix)
  , mDataOut = f (EData OutsideParameterPrefix)
  }
  where
    byUse g = ByUse (g UseBinder) (g UseMotive) (g UseData) (g UseIdentity) (g UseConcluded)

atEntry :: Entry -> ByEntry -> Verdicts
atEntry e m = case e of
  ETail -> mTail m
  EAssumed InParameterPrefix u -> atUse u (mAssumedIn m)
  EAssumed OutsideParameterPrefix u -> atUse u (mAssumedOut m)
  EExtCodomain u -> atUse u (mExt m)
  ETerm InParameterPrefix -> mTermIn m
  ETerm OutsideParameterPrefix -> mTermOut m
  EData InParameterPrefix -> mDataIn m
  EData OutsideParameterPrefix -> mDataOut m
  where
    atUse u (ByUse b mo d i c) = case u of
      UseBinder -> b
      UseMotive -> mo
      UseData -> d
      UseIdentity -> i
      UseConcluded -> c

data Shape
  = SVar VarFacts
  | SParam Table Int
    -- ^ a parameter of a table, standing for an argument
  | SParamType Table Int (Maybe Facts)
    -- ^ the type of the argument a parameter stands for, with the parameter's
    -- declared type if known
  | SUniverse | SCubeU | STopeU
  | SLeaf Leaf
    -- ^ a constant with nothing inside
  | SCubeOrTope Bool [Facts]
    -- ^ an operation on cubes or topes, with its operands; the flag says
    -- whether the schematic check admits it
  | SPi Binder TModality Facts VarFacts (Maybe Facts) Facts (Facts -> (Maybe Facts, Facts))
    -- ^ the domain, the bound variable, the tope and the codomain opened under
    -- it, and the opening with something else standing for the variable
  | SSigma Binder TModality Facts VarFacts Facts (Facts -> Facts)
  | SId Facts (Maybe Facts) Facts
  | SRestricted Facts [(Facts, Facts)]
  | SRecOr [(Facts, Facts)]
  | SApp Facts Facts
  | SLam Binder TModality (Maybe (Facts, Maybe Facts)) VarFacts Facts (Facts -> Facts)
    -- ^ the parameter's domain and tope (if written), the bound variable, the
    -- body opened under it, and the opening with something else
  | SLet Binder (Maybe Facts) Facts VarFacts Facts
    -- ^ the annotation, the value, the bound variable (which has the value),
    -- and the body opened under it
  | SPair Facts Facts
  | SFirst Facts
  | SSecond Facts
  | SRefl (Maybe (Facts, Maybe Facts))
    -- ^ the endpoint and its type, if written
  | SIdJ Facts Facts Facts Facts Facts Facts
  | STypeAsc Facts Facts
  | STypeModal Facts
  | SScoped Table [Facts] Facts
    -- ^ facts from the body of a table, with the parameters standing for
    -- these arguments: whatever is found about them is settled against the
    -- arguments (see 'scopedFacts')
  | SOther String
    -- ^ not modelled: the certificate gives up

-- | The name of a shape, for diagnostics.
shapeName :: Shape -> String
shapeName = \case
  SVar _ -> "a variable"
  SParam t j -> "parameter " <> show j <> " of " <> tbName t
  SParamType t j _ -> "the type of parameter " <> show j <> " of " <> tbName t
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
  SScoped t _ _ -> "facts from the body of " <> tbName t
  SOther r -> r

-- | The constants, as far as the checks tell them apart.
data Leaf
  = LTypeUnit | LRecBottom
    -- ^ object types
  | LCubeUnit | LCube2
    -- ^ cubes
  | LOtherConst
    -- ^ points, topes, @unit@
  | LInterval
    -- ^ the auxiliary interval and its endpoints, which the schematic check
    -- does not admit
  | LHole
  deriving (Eq)

-- | What exposing a term gives.
data Exposed
  = ExposedTo Int [Need] Bool Facts
    -- ^ after this many reduction steps, under these conditions, the term is
    -- these facts (a constructor, a lambda, or a stuck term); the flag says
    -- that it is the term itself, unchanged
  | ExposedTable Int [Need] Table [Facts]
    -- ^ a saturated application of a definition with a table: what it exposes
    -- to is the table's body, with the parameters standing for these arguments
  | ExposedIn Int [Need] Table [Facts] Facts
    -- ^ these facts from the body of a table, with the parameters standing for
    -- these arguments (a component of the body, say): what they say is
    -- settled against the arguments
  | ExposedParam Int [Need] Table Int
    -- ^ the argument a parameter stands for
  | ExposedParamApp Int [Need] Table Int [Op]
    -- ^ that argument, under these operations
  | ExposedParamType Int [Need] Table Int
    -- ^ the type of that argument
  | ExposedUnknown String

-- | The reduction limit of one exposure. The standalone walk gives up when its
-- fuel (at least 'defaultRSTTFuel') runs out; the step counts here are upper
-- bounds, so a margin is kept.
exposeLimit :: Int
exposeLimit = 200

later :: Int -> Exposed -> Exposed
later k = \case
  ExposedTo s ns self g -> ExposedTo (s + k) ns self g
  ExposedTable s ns t as -> ExposedTable (s + k) ns t as
  ExposedIn s ns t as g -> ExposedIn (s + k) ns t as g
  ExposedParam s ns t j -> ExposedParam (s + k) ns t j
  ExposedParamApp s ns t j as -> ExposedParamApp (s + k) ns t j as
  ExposedParamType s ns t j -> ExposedParamType (s + k) ns t j
  e -> e

underConditions :: [Need] -> Exposed -> Exposed
underConditions ns = \case
  ExposedTo s ns' self g -> ExposedTo s (ns <> ns') self g
  ExposedTable s ns' t as -> ExposedTable s (ns <> ns') t as
  ExposedIn s ns' t as g -> ExposedIn s (ns <> ns') t as g
  ExposedParam s ns' t j -> ExposedParam s (ns <> ns') t j
  ExposedParamApp s ns' t j as -> ExposedParamApp s (ns <> ns') t j as
  ExposedParamType s ns' t j -> ExposedParamType s (ns <> ns') t j
  e -> e

exposeSteps :: Exposed -> Int
exposeSteps = \case
  ExposedTo s _ _ _ -> s
  ExposedTable s _ _ _ -> s
  ExposedIn s _ _ _ _ -> s
  ExposedParam s _ _ _ -> s
  ExposedParamApp s _ _ _ _ -> s
  ExposedParamType s _ _ _ -> s
  ExposedUnknown _ -> maxBound

-- | A variable: one bound in the term, a top-level entry, or a table's
-- parameter.
data VarFacts = VarFacts
  { vfKey   :: Int
    -- ^ unique in a run, so that points can be told apart across scopes
  , vfOrig  :: Binder
  , vfType  :: Facts
  , vfSchemType :: Facts
    -- ^ the type as the schematic check derives it, which for a let-bound
    -- variable is its annotation or the type it derives for the value
  , vfValue :: Maybe Facts
  , vfTop   :: Maybe TopLevel
  , vfReach :: Points
    -- ^ what 'outerPoints' reaches through it: the variable itself if it is
    -- a cube point, else whatever its type and value reach
  }

data TopLevel = TopLevel
  { tlTable        :: Maybe Table
  , tlDataRole     :: Bool
  , tlMetaPrefix   :: Int
    -- ^ the length of the entry's meta-parameter prefix (see "Rzk.TypeCheck.MetaPrefix")
  , tlObjectFamily :: Maybe (Int, SVerdict)
    -- ^ the arity at which the entry is an object-valued family, as
    -- @knownObjectFamily@ probes it, with what the probe relies on
  }

-- | A top-level definition's value with its parameters symbolic: the facts of
-- its body, and the verdicts of the body at each entry, computed once.
data Table = Table
  { tbId       :: Int
  , tbName     :: String
    -- ^ for diagnostics
  , tbArity    :: Int
    -- ^ the leading lambdas of the value, which the parameters are
  , tbDeclared :: [Maybe Facts]
    -- ^ the parameters' declared types, where written
  , tbBody     :: Facts
  }

sameTable :: Table -> Table -> Bool
sameTable a b = tbId a == tbId b

tableVerdict :: Table -> FragmentFlags -> Entry -> Verdict
tableVerdict t fl e = fragment fl ComputedTypes e (tbBody t)

-- * Building facts

-- | The environment a term's facts are built in.
data Env n = Env
  { envScope :: Foil.Scope n
  , envBound :: IntMap.IntMap Facts
    -- ^ what a name bound in the term stands for: its variable, or the facts
    -- substituted for it
  , envOuter :: IntMap.IntMap VarFacts
    -- ^ the top-level entries, by name identifier
  , envOwner :: Int
    -- ^ distinguishes the keys of variables bound here from those bound in
    -- other terms (tables and declarations), whose facts may be combined:
    -- a top-level entry's facts and its table have their own, and a
    -- declaration under check a fresh negative one
  }

keyFor :: Env n -> NameBinder n l -> Int
keyFor env binder = envOwner env * 4294967296 + Foil.nameId (Foil.nameOf binder)

extend :: Env n -> NameBinder n l -> Facts -> Env l
extend env binder f = env
  { envScope = Foil.extendScope binder (envScope env)
  , envBound = IntMap.insert (Foil.nameId (Foil.nameOf binder)) f (envBound env)
  }

mkFacts :: Shape -> Facts -> Facts
mkFacts sh ty = f
  where
    f = Facts
      { fShape = sh
      , fType = ty
      , fDerived = derivedOf f
      , fVerdicts = verdictsOf f
      , fSchem = schemOf f
      , fDepth = depthOf f
      }

-- Kept out of line, so that the record and its thunks are allocated only
-- when a check asks.
derivedOf :: Facts -> Derived
derivedOf f = Derived
  { dExpose = exposeOf f
  , dHeadView = headViewOf f
  , dMeta = metaOf f
  , dLands = landsOf f
  , dCube = cubeOf f
  , dPoints = pointsOf f
  }

-- | The depth of a term, through the default openings of its binders.
depthOf :: Facts -> Int
depthOf f = 1 + maximum (0 : map fDepth children)
  where
    children = case fShape f of
      SVar _ -> []
      SParam{} -> []
      SParamType{} -> []
      SUniverse -> []
      SCubeU -> []
      STopeU -> []
      SLeaf _ -> []
      SCubeOrTope _ cs -> cs
      SPi _ _ dom _ mt cod _ -> dom : cod : maybe [] pure mt
      SSigma _ _ a _ b _ -> [a, b]
      SId a mt b -> a : b : maybe [] pure mt
      SRestricted ty rs -> ty : concat [[a, b] | (a, b) <- rs]
      SRecOr rs -> concat [[a, b] | (a, b) <- rs]
      SApp g x -> [g, x]
      SLam _ _ mparam _ body _ -> body : maybe [] (\(p, mt) -> p : maybe [] pure mt) mparam
      SLet _ manno value _ body -> value : body : maybe [] pure manno
      SPair a b -> [a, b]
      SFirst p -> [p]
      SSecond p -> [p]
      SRefl mx -> maybe [] (\(x, mt) -> x : maybe [] pure mt) mx
      SIdJ a x c d y p -> [a, x, c, d, y, p]
      STypeAsc v ty -> [v, ty]
      STypeModal ty -> [ty]
      SScoped _ _ g -> [g]
      SOther _ -> []

-- | The memo of 'fragment' on a term, lazy at every level.
verdictsOf :: Facts -> ByFlags ByEntry
verdictsOf f = byFlags $ \fl -> byEntry $ \e -> fragmentAt fl e f

unknownFacts :: String -> Facts
unknownFacts r = f where f = mkFacts (SOther r) f

universeFacts :: Facts
universeFacts = f where f = mkFacts SUniverse f

varFacts :: VarFacts -> Facts
varFacts v = mkFacts (SVar v) (vfType v)

-- | A variable bound in the term.
boundVar :: Int -> Binder -> Facts -> Maybe Facts -> VarFacts
boundVar key orig ty = boundVarWith key orig ty ty

boundVarWith :: Int -> Binder -> Facts -> Facts -> Maybe Facts -> VarFacts
boundVarWith key orig ty schemTy mval = v
  where
    v = VarFacts
      { vfKey = key, vfOrig = orig, vfType = ty, vfSchemType = schemTy, vfValue = mval
      , vfTop = Nothing, vfReach = reachOf key ty mval }

-- | What 'outerPoints' finds through a variable of this type and value: the
-- variable itself when it is a cube point (no value, and its type is a cube),
-- and the variable again for whatever its type and value reach.
reachOf :: Int -> Facts -> Maybe Facts -> Points
reachOf key ty mval = case own of
  SymBool ns (Known True) -> viaMe { pVars = IntSet.singleton key, pNeeds = ns <> pNeeds viaMe }
  SymBool ns (Known False) -> viaMe { pNeeds = ns <> pNeeds viaMe }
  SymBool ns (AnyOf qs)
    | all isTypeCube qs -> viaMe
        { pIfCube = [(key, t, j) | ParamQuery t j _ _ _ <- qs] <> pIfCube viaMe
        , pNeeds = ns <> pNeeds viaMe }
  SymBool _ (Unknown r) -> mempty { pUnknown = Just r }
  _ -> mempty { pUnknown = Just "a variable that may be a point" }
  where
    own = case mval of
      Nothing -> fCube (fType ty)
      Just _  -> known False
    through = fPoints ty <> foldMap fPoints mval
    viaMe = through
      { pVars = if IntSet.null (pVars through) then IntSet.empty else IntSet.singleton key
      , pIfCube = [(key, t, j) | (_, t, j) <- pIfCube through]
      , pParams = [(Just key, t, j) | (_, t, j) <- pParams through]
      }
    isTypeCube (ParamQuery _ _ ops q frames) = null ops && null frames && q == QTypeCube

paramFacts :: Table -> Int -> Maybe Facts -> Facts
paramFacts t j mdeclared = f
  where
    f = Facts
      { fShape = SParam t j
      , fType = paramTypeFacts t j mdeclared
      , fDerived = Derived
          { dExpose = ExposedParam 0 [] t j
          , dHeadView = ExposedParam 0 [] t j
          , dMeta = SymBool [] (AnyOf [ParamQuery t j [] QMeta []])
          , dLands = SymBool [] (AnyOf [ParamQuery t j [] QLands []])
          , dCube = SymBool [] (AnyOf [ParamQuery t j [] QCube []])
          , dPoints = mempty { pParams = [(Nothing, t, j)] }
          }
      , fVerdicts = verdictsOf f
      , fSchem = schemOf f
      , fDepth = 1
      }

-- | The type of the argument a parameter stands for. The parameter's declared
-- type, when known, gives the likely answer to a query; the answer is
-- guarded by a need, since the argument's own type may differ.
paramTypeFacts :: Table -> Int -> Maybe Facts -> Facts
paramTypeFacts t j mdeclared = f
  where
    f = Facts
      { fShape = SParamType t j mdeclared
      , fType = maybe (unknownFacts "the type of an argument's type") fType mdeclared
      , fDerived = Derived
          { dExpose = ExposedParamType 0 [] t j
          , dHeadView = ExposedParamType 0 [] t j
          , dMeta = guarded QTypeMeta fMeta
          , dLands = guarded QTypeLands fLands
          , dCube = guarded QTypeCube fCube
          , dPoints = mempty { pUnknown = Just "the type of an argument" }
          }
      , fVerdicts = verdictsOf f
      , fSchem = schemOf f
      , fDepth = 1
      }
    guarded q field = case field <$> mdeclared of
      Just (SymBool ns (Known b)) -> SymBool (ns <> [Need t j (ReqAnswer [] q b)]) (Known b)
      Just (SymBool ns (AnyOf qs)) -> SymBool (ns <> [Need t j (ReqAnswer [] q False)]) (AnyOf qs)
      _ -> SymBool [] (AnyOf [ParamQuery t j [] q []])

-- | The facts of a term.
facts :: forall n. Distinct n => Env n -> TermT n -> Facts
facts env = go
  where
    scope = envScope env

    go :: TermT n -> Facts
    go = \case
      Var v ->
        let i = Foil.nameId v
        in fromMaybe (unknownFacts "a variable out of scope")
             (IntMap.lookup i (envBound env) <|> (varFacts <$> IntMap.lookup i (envOuter env)))
      UniverseT info -> node SUniverse info
      UniverseCubeT info -> node SCubeU info
      UniverseTopeT info -> node STopeU info
      CubeUnitT info -> node (SLeaf LCubeUnit) info
      CubeUnitStarT info -> node (SLeaf LOtherConst) info
      Cube2T info -> node (SLeaf LCube2) info
      Cube2_0T info -> node (SLeaf LOtherConst) info
      Cube2_1T info -> node (SLeaf LOtherConst) info
      CubeIT info -> node (SLeaf LInterval) info
      CubeI_0T info -> node (SLeaf LInterval) info
      CubeI_1T info -> node (SLeaf LInterval) info
      TopeTopT info -> node (SLeaf LOtherConst) info
      TopeBottomT info -> node (SLeaf LOtherConst) info
      UnitT info -> node (SLeaf LOtherConst) info
      TypeUnitT info -> node (SLeaf LTypeUnit) info
      RecBottomT info -> node (SLeaf LRecBottom) info
      HoleT info _ -> node (SLeaf LHole) info
      CubeProductT info a b -> node (SCubeOrTope True [go a, go b]) info
      CubeFlipT info a -> node (SCubeOrTope False [go a]) info
      CubeUnflipT info a -> node (SCubeOrTope False [go a]) info
      CubeSupT info a b -> node (SCubeOrTope True [go a, go b]) info
      CubeInfT info a b -> node (SCubeOrTope True [go a, go b]) info
      TopeEQT info a b -> node (SCubeOrTope True [go a, go b]) info
      TopeLEQT info a b -> node (SCubeOrTope True [go a, go b]) info
      TopeAndT info a b -> node (SCubeOrTope True [go a, go b]) info
      TopeOrT info a b -> node (SCubeOrTope True [go a, go b]) info
      TopeInvT info a -> node (SCubeOrTope False [go a]) info
      TopeUninvT info a -> node (SCubeOrTope False [go a]) info
      RecOrT info rs -> node (SRecOr [(go a, go b) | (a, b) <- rs]) info
      TypeRestrictedT info ty rs -> node (SRestricted (go ty) [(go a, go b) | (a, b) <- rs]) info
      TypeIdT info a mtA b -> node (SId (go a) (go <$> mtA) (go b)) info
      AppT info f x -> node (SApp (go f) (go x)) info
      PairT info a b -> node (SPair (go a) (go b)) info
      FirstT info p -> node (SFirst (go p)) info
      SecondT info p -> node (SSecond (go p)) info
      ReflT info mx -> node (SRefl ((\(x, mty) -> (go x, go <$> mty)) <$> mx)) info
      IdJT info a x c d y p -> node (SIdJ (go a) (go x) (go c) (go d) (go y) (go p)) info
      TypeAscT info v ty -> node (STypeAsc (go v) (go ty)) info
      TypeModalT info _ ty -> node (STypeModal (go ty)) info
      ModAppT info _ _ -> node (SOther "a modal introduction") info
      ModExtractT info _ _ _ -> node (SOther "a modal extraction") info
      LetModT info _ _ _ _ _ _ _ -> node (SOther "a modal let") info
      MatchT info _ _ _ _ -> node (SOther "a match") info
      MatchArmT info _ _ -> node (SOther "a match arm") info

      TypeFunT info orig md param mtope ret ->
        let paramF = go param
            under :: Facts -> (Int, Maybe Facts, Facts)
            under x = case mtope of
              Nothing -> withScopedT scope ret $ \binder body ->
                (keyFor env binder, Nothing, facts (extend env binder x) body)
              Just tope -> withScopedT2 scope tope ret $ \binder tp body ->
                let env' = extend env binder x
                in (keyFor env binder, Just (facts env' tp), facts env' body)
            (key, mtopeF, codF) = under (varFacts bv)
            bv = boundVar key orig paramF Nothing
        in node (SPi orig md paramF bv mtopeF codF (\x -> let (_, mt, c) = under x in (mt, c))) info

      TypeSigmaT info orig md a b ->
        let aF = go a
            under :: Facts -> (Int, Facts)
            under x = withScopedT scope b $ \binder body ->
              (keyFor env binder, facts (extend env binder x) body)
            (key, bF) = under (varFacts bv)
            bv = boundVar key orig aF Nothing
        in node (SSigma orig md aF bv bF (snd . under)) info

      -- The let annotation records the value's concluded type; without one,
      -- the walk takes the value's annotation, and a universe for a bare variable.
      LetT info orig manno value body ->
        let valueF = go value
            mannoF = go <$> manno
            dom = case (mannoF, value) of
              (Just a, _)   -> a
              (Nothing, Var{}) -> universeFacts
              (Nothing, _)  -> fType valueF
            under :: Facts -> (Int, Facts)
            under x = withScopedT scope body $ \binder bd ->
              (keyFor env binder, facts (extend env binder x) bd)
            (key, bodyF) = under (varFacts bv)
            -- The schematic check gives the variable the annotation, or the
            -- type it derives for the value.
            bv = boundVarWith key orig dom (fromMaybe (schemTypeOf valueF) mannoF) (Just valueF)
        in node (SLet orig mannoF valueF bv bodyF) info

      LambdaT info orig mparam body ->
        let lamF = node (SLam orig md mparamF bv bodyF (\x -> let (_, _, b) = under x in b)) info
            md = case mparam of
              Just (LambdaParam md' _ _) -> md'
              Nothing                    -> Id
            under :: Facts -> (Int, Maybe (Facts, Maybe Facts), Facts)
            under x = case mparam of
              Just (LambdaParam _ param (Just tope)) ->
                withScopedT2 scope tope body $ \binder tp bd ->
                  let env' = extend env binder x
                  in (keyFor env binder, Just (go param, Just (facts env' tp)), facts env' bd)
              Just (LambdaParam _ param Nothing) ->
                withScopedT scope body $ \binder bd ->
                  (keyFor env binder, Just (go param, Nothing), facts (extend env binder x) bd)
              Nothing ->
                withScopedT scope body $ \binder bd ->
                  (keyFor env binder, Nothing, facts (extend env binder x) bd)
            (key, mparamF, bodyF) = under (varFacts bv)
            -- A bare λ binds a variable of the domain of its own type.
            dom = case mparamF of
              Just (param, _) -> param
              Nothing -> case fHeadView (fType lamF) of
                ExposedTo _ [] _ g | SPi _ _ d _ _ _ _ <- fShape g -> d
                                   | otherwise -> universeFacts
                _ -> unknownFacts "the domain of a bare lambda"
            bv = boundVar key orig dom Nothing
        in lamF

    node :: Shape -> TypeInfo (TermT n) -> Facts
    node sh info = mkFacts sh (go (infoType info))

-- | The facts of a top-level entry, with its table if it has a value. The
-- entries it mentions are looked up in the map given, which must hold them.
topLevelFacts
  :: Distinct n
  => IntMap.IntMap VarFacts -> Foil.Scope n -> Int -> VarInfo n -> VarFacts
topLevelFacts outer scope i info = v
  where
    env = Env scope IntMap.empty outer (2 * i + 1)
    v = VarFacts
      { vfKey = i
      , vfOrig = varOrig info
      , vfType = facts env (varType info)
      , vfSchemType = vfType v
      , vfValue = facts env <$> varValue info
      , vfTop = Just TopLevel
          { tlTable = buildTable outer scope i (maybe "_" show (binderName (varOrig info))) (vfType v) <$> varValue info
          , tlDataRole = isJust (varDataRole info)
          , tlMetaPrefix = varMetaPrefix info
          , tlObjectFamily = objectFamilyOf (vfType v) (vfValue v)
          }
      , vfReach = reachOf i (vfType v) (vfValue v)
      }

-- | A definition's table: its value under its leading lambdas, with the
-- parameters symbolic.
buildTable :: forall n. Distinct n => IntMap.IntMap VarFacts -> Foil.Scope n -> Int -> String -> Facts -> TermT n -> Table
buildTable outer scope i name ty value = table
  where
    table = peel (Env scope IntMap.empty outer (2 * i + 2)) 0 [] (Just ty) value

    -- A lambda without a domain (one that abstracts a section's assumption)
    -- takes its declared type from the entry's type.
    peel :: forall l. Distinct l => Env l -> Int -> [Maybe Facts] -> Maybe Facts -> TermT l -> Table
    peel env k declareds mty = \case
      LambdaT _ _ mparam body ->
        let fromType = case fmap fHeadView mty of
              Just (ExposedTo _ [] _ g) | SPi _ _ dom _ _ _ _ <- fShape g -> Just dom
              _ -> Nothing
            declared = case mparam of
              Just (LambdaParam _ param _) -> Just (facts env param)
              Nothing                     -> fromType
            leaf = paramFacts table k declared
            nextTy = case fmap fHeadView mty of
              Just (ExposedTo _ [] _ g) | SPi _ _ _ _ _ _ open <- fShape g -> Just (snd (open leaf))
              _ -> Nothing
        in withScopedT (envScope env) body $ \binder body' ->
             peel (extend env binder leaf) (k + 1) (declareds <> [declared]) nextTy body'
      body ->
        let bodyF = facts env body
        in Table
          { tbId = i
          , tbName = name
          , tbArity = k
          , tbDeclared = declareds
          , tbBody = bodyF
          }

-- * Exposure

-- | The head of an application spine and its arguments, each with the
-- function node it is applied to.
spine :: Facts -> (Facts, [(Facts, Facts)])
spine = go []
  where
    go acc f = case fShape f of
      SApp fn x -> go ((fn, x) : acc) fn
      _         -> (f, acc)

isRestrictedShape :: Facts -> Bool
isRestrictedShape f = case fShape f of
  SRestricted{} -> True
  _             -> False

-- | What 'fragmentHead' exposes a term to.
exposeOf :: Facts -> Exposed
exposeOf f = case fShape f of
  SVar v -> case vfValue v of
    Just val -> later 1 (fExpose val)
    Nothing  -> neutralOf 0 f
  SParam t j -> ExposedParam 0 [] t j
  SParamType{} -> ExposedUnknown "the type of an argument"
  SApp{} -> exposeApp f
  SLet _ _ _ _ body -> later 1 (fExpose body)
  STypeAsc v _ -> later 1 (fExpose v)
  SFirst p -> projection fst p
  SSecond p -> projection snd p
  SIdJ _ _ _ d _ proof -> case fExpose proof of
    ExposedTo k ns _ p' | SRefl{} <- fShape p' -> underConditions ns (later (k + 1) (fExpose d))
    ExposedTo k ns True _ -> underConditions ns (neutralOf k f)
    ExposedTo _ _ False _ -> ExposedUnknown "an elimination of a reducible proof"
    ExposedUnknown r -> ExposedUnknown r
    _ -> ExposedUnknown "an elimination of an argument"
  SRecOr _ -> ExposedUnknown "a case split on topes"
  SScoped t args g -> scopedExposure t args (fExpose g)
  SOther r -> ExposedUnknown r
  _ -> ExposedTo 0 [] True f
  where
    projection component p = case fExpose p of
      ExposedTo k ns _ p' | SPair a b <- fShape p' -> underConditions ns (later (k + 1) (fExpose (component (a, b))))
      ExposedTo k ns True _ -> underConditions ns (neutralOf k f)
      ExposedTo _ _ False _ -> ExposedUnknown "a projection of a reducible pair"
      -- A pair-valued definition: the component lives in its body.
      ExposedTable k ns t args -> inBody k ns t args (fExpose (tbBody t))
      ExposedIn k ns t args p' -> inBody k ns t args (fExpose p')
      -- A projection of an argument: the use settles it.
      ExposedParam k ns t j -> ExposedParamApp (k + 1) ns t j [projectionOp]
      ExposedParamApp k ns t j ops -> ExposedParamApp (k + 1) ns t j (ops <> [projectionOp])
      ExposedUnknown r -> ExposedUnknown r
      _ -> ExposedUnknown "a projection of an argument"
      where
        projectionOp = case fShape f of
          SFirst _ -> OpFirst
          _ -> OpSecond
        inBody k ns t args = \case
          ExposedTo k' ns' _ p' | SPair a b <- fShape p' ->
            underConditions ns (ExposedIn (k + k' + 1) ns' t args (component (a, b)))
          _ -> ExposedUnknown "a projection of a definition's body"

-- | A stuck term exposes to itself, unless its type is a restricted type whose
-- face holds; the walk exposes the type to find out, and asks the solver.
neutralOf :: Int -> Facts -> Exposed
neutralOf k f = case fExpose (fType f) of
  ExposedTo k' ns _ ty'
    | isRestrictedShape ty' -> ExposedUnknown "a restricted type to try"
    | otherwise -> ExposedTo (k + k') ns True f
  ExposedTable k' ns t args -> case resolveSym t args (restrictedOf (tbBody t)) of
    SymBool ns' (Known False) -> ExposedTo (k + k') (ns <> ns') True f
    SymBool _ (Known True) -> ExposedUnknown "a restricted type to try"
    SymBool ns' (AnyOf qs) ->
      ExposedTo (k + k') (ns <> ns' <> [guardOf q False | q <- qs]) True f
    SymBool _ (Unknown r) -> ExposedUnknown r
  ExposedIn k' ns t args g -> case resolveSym t args (restrictedOf g) of
    SymBool ns' (Known False) -> ExposedTo (k + k') (ns <> ns') True f
    SymBool _ (Known True) -> ExposedUnknown "a restricted type to try"
    SymBool ns' (AnyOf qs) -> ExposedTo (k + k') (ns <> ns' <> [guardOf q False | q <- qs]) True f
    SymBool _ (Unknown r) -> ExposedUnknown r
  ExposedParam k' ns t j -> ExposedTo (k + k') (ns <> [Need t j (ReqAnswer [] QRestricted False)]) True f
  ExposedParamApp k' ns t j ops -> ExposedTo (k + k') (ns <> [Need t j (ReqAnswer ops QRestricted False)]) True f
  ExposedParamType{} -> ExposedUnknown "a type that is an argument's type"
  ExposedUnknown r -> ExposedUnknown r

exposeApp :: Facts -> Exposed
exposeApp f = case fShape h of
  SVar v
    | Just tl <- vfTop v, Just tb <- tlTable tl, Just val <- vfValue v ->
        if n == tbArity tb
          then case fExpose (tbBody tb) of
            -- An alias of a parameter exposes to the argument, under the
            -- body's conditions, which the arguments settle.
            ExposedParam k ns _ j -> case resolveTable Nothing tb args (needs ns) of
              Clean -> later (1 + n + k) (fExpose (args !! j))
              Needs ns' -> underConditions ns' (later (1 + n + k) (fExpose (args !! j)))
              Unclean r -> ExposedUnknown r
            ExposedUnknown r -> ExposedUnknown r
            e -> ExposedTable (1 + n + exposeSteps e) [] tb args
          -- Fewer or more arguments than parameters: unfold and β-reduce
          -- through the value's own lambdas.
          else applyExposed (later 1 (fExpose val)) args
    | Nothing <- vfValue v -> neutralOf 0 f
  _ -> case fExpose h of
    -- A lambda reduces; any other stuck head leaves the application as it is.
    ExposedTo _ _ _ g | SLam{} <- fShape g -> applyExposed (fExpose h) args
    ExposedTo k ns True _ -> underConditions ns (neutralOf k f)
    e -> applyExposed e args
  where
    (h, pairs) = spine f
    args = map snd pairs
    n = length args

-- | Apply an exposed function to arguments: β through lambdas, and a stuck
-- head stays stuck.
applyExposed :: Exposed -> [Facts] -> Exposed
applyExposed e [] = e
applyExposed e as@(x : xs) = case e of
  ExposedTo k ns _ g | SLam _ _ _ _ _ open <- fShape g ->
    applyExposed (underConditions ns (later (k + 1) (fExpose (open x)))) xs
  ExposedTo k ns True g -> underConditions ns (neutralOf k (foldl appFacts g as))
  ExposedTo _ _ False _ -> ExposedUnknown "an application of a reducible head"
  ExposedParam k ns t j -> ExposedParamApp k ns t j (map OpApp as)
  ExposedParamApp k ns t j ops -> ExposedParamApp k ns t j (ops <> map OpApp as)
  ExposedTable{} -> ExposedUnknown "an over-applied definition"
  ExposedIn{} -> ExposedUnknown "an applied component of a definition"
  ExposedParamType{} -> ExposedUnknown "an applied type"
  ExposedUnknown r -> ExposedUnknown r

-- | The facts of an application built here rather than read from a term: its
-- type is the codomain of the function's type, instantiated.
appFacts :: Facts -> Facts -> Facts
appFacts g x = mkFacts (SApp g x) ty
  where
    ty = case functionHeadOf (fType g) of
      Right (scope, t) | SPi _ _ _ _ _ _ open <- fShape t -> underScopes scope (snd (open x))
      _ -> unknownFacts "the type of an application built from facts"

-- | Facts from the bodies of tables (innermost first), at uses with the given
-- arguments.
underScopes :: [(Table, [Facts])] -> Facts -> Facts
underScopes scope x = foldl (\acc (t, args) -> scopedFacts t args acc) x scope

-- | The facts of a projection built here: its type is the component of the
-- pair's type.
firstFacts :: Facts -> Facts
firstFacts p = mkFacts (SFirst p) (fst (componentTypes p))

secondFacts :: Facts -> Facts
secondFacts p = mkFacts (SSecond p) (snd (componentTypes p))

componentTypes :: Facts -> (Facts, Facts)
componentTypes p = case functionHeadOf (fType p) of
  Right (scope, h)
    | SSigma _ _ a _ _ open <- fShape h ->
        let a' = underScopes scope a
        in (a', underScopes scope (open (mkFacts (SFirst p) a')))
    | SCubeOrTope True [a, b] <- fShape h -> (underScopes scope a, underScopes scope b)
  _ -> let u = unknownFacts "the type of a projection built from facts" in (u, u)

-- | The facts of a term from the body of a table, at a use with the given
-- arguments: every fact is settled against the arguments, and a walk of the
-- term is a walk of the body's term followed by the settlement.
scopedFacts :: Table -> [Facts] -> Facts -> Facts
scopedFacts t args g = f
  where
    f = Facts
      { fShape = SScoped t args g
      , fType = scopedFacts t args (fType g)
      , fDerived = Derived
          { dExpose = scopedExposure t args (fExpose g)
          , dHeadView = scopedExposure t args (fHeadView g)
          , dMeta = resolveSym t args (fMeta g)
          , dLands = resolveSym t args (fLands g)
          , dCube = resolveSym t args (fCube g)
          , dPoints = scopedPoints t args (fPoints g)
          }
      , fVerdicts = byFlags $ \fl -> byEntry $ \e ->
          let (src, cmp) = fragmentIn fl e g
          in (resolveTable (Just fl) t args src, resolveTable (Just fl) t args cmp)
      , fSchem = Schem
          { scSort = scopedSort (classifyM g)
          , scHead = scopedSort (headSortM g)
          , scTerm = resolveS t args (termM g)
          , scSignature = resolveS t args (signatureM g)
          , scType = scopedFacts t args (schemTypeM g)
          }
      , fDepth = fDepth g
      }
    scopedSort = \case
      Sorted s v -> Sorted s (resolveS t args v)
      u -> u

-- | An exposure found in the body of a table, at a use with the arguments.
scopedExposure :: Table -> [Facts] -> Exposed -> Exposed
scopedExposure t args = \case
  ExposedTo k ns _ g -> settled ns (ExposedIn k [] t args g)
  ExposedTable k ns t' args' -> settled ns (ExposedTable k [] t' (map (scopedFacts t args) args'))
  ExposedIn k ns t' args' g -> settled ns (ExposedIn k [] t' (map (scopedFacts t args) args') g)
  ExposedParam k ns t' j
    | sameTable t t' -> settled ns (later k (fExpose (args !! j)))
    | otherwise -> ExposedParam k ns t' j
  ExposedParamApp k ns t' j ops
    | sameTable t t' -> settled ns (later k (fExpose (applyOps (args !! j) (map (scopeOp (scopedFacts t args)) ops))))
    | otherwise -> ExposedParamApp k ns t' j (map (scopeOp (scopedFacts t args)) ops)
  ExposedParamType k ns t' j
    | sameTable t t' -> settled ns (later k (fExpose (fType (args !! j))))
    | otherwise -> ExposedParamType k ns t' j
  ExposedUnknown r -> ExposedUnknown r
  where
    settled ns e = case resolveTable Nothing t args (needs ns) of
      Clean -> e
      Needs ns' -> underConditions ns' e
      Unclean r -> ExposedUnknown r

-- | Points found in the body of a table, at a use with the arguments.
scopedPoints :: Table -> [Facts] -> Points -> Points
scopedPoints t args p = case resolveTable Nothing t args (needs (pNeeds p)) of
  Unclean r -> mempty { pUnknown = Just r }
  v -> foldr step (p { pIfCube = [], pParams = [], pNeeds = leftover v }) (pIfCube p)
         <> foldMap viaParam (pParams p)
  where
    leftover (Needs ns) = ns
    leftover _ = []
    step (key, t', j) acc
      | sameTable t t' = case fCube (fType (args !! j)) of
          SymBool [] (Known True) -> acc { pVars = IntSet.insert key (pVars acc) }
          SymBool [] (Known False) -> acc
          _ -> acc { pUnknown = Just "a variable that may be a point" }
      | otherwise = acc { pIfCube = (key, t', j) : pIfCube acc }
    viaParam (via, t', j)
      | sameTable t t' = fPoints (args !! j)
      | otherwise = mempty { pParams = [(via, t', j)] }

-- | What @whnfT@ followed by @stripTypeRestrictions@ exposes a term to.
headViewOf :: Facts -> Exposed
headViewOf f0 = go (fExpose (stripRestrictions f0))
  where
    go = \case
      ExposedTo k ns self g
        | SRestricted base _ <- fShape g -> underConditions ns (later k (go (fExpose (stripRestrictions base))))
        | self, eliminatorHeaded g -> ExposedUnknown "a data eliminator"
        | otherwise -> ExposedTo k ns self g
      ExposedIn k ns t args g -> underConditions ns (later k (scopedExposure t args (headViewOf g)))
      e -> e

    -- whnfT reduces a data eliminator applied to a constructor; fragmentHead does not.
    eliminatorHeaded g = case fShape (fst (spine g)) of
      SVar v | Just tl <- vfTop v -> tlDataRole tl
      _ -> False

stripRestrictions :: Facts -> Facts
stripRestrictions f = case fShape f of
  SRestricted base _ -> stripRestrictions base
  _                  -> f

-- | The answer of a query on the body of a table, for the given arguments.
-- An argument's answer may ask more of the same arguments, so this repeats
-- until no query on this table's parameters is left. A query on another
-- table's parameter that leaves carries this table's facts, so it is to be
-- asked under these arguments.
resolveSym :: Table -> [Facts] -> SymBool -> SymBool
resolveSym t args = loop
  where
    loop (SymBool ns a) = case resolveTable Nothing t args (needs ns) of
      Unclean r -> unknown r
      v -> case a of
        AnyOf qs | (mine@(_ : _), others) <- partition (\(ParamQuery t' _ _ _ _) -> sameTable t t') qs ->
          loop $ withConditions (needsOf v) $
            foldr orSym (SymBool [] (AnyOf others))
              [ under frames (queryOf q (applyOps (args !! j) ops))
              | ParamQuery _ j ops q frames <- mine ]
        AnyOf qs -> SymBool (needsOf v) (AnyOf (map leaving qs))
        _ -> SymBool (needsOf v) a
    needsOf (Needs ns') = ns'
    needsOf _           = []
    -- A query that came from under other tables' uses is asked there first.
    under frames s = foldl (\s' (t', args') -> resolveSym t' args' s') s frames
    leaving (ParamQuery t' j ops q frames)
      | null ops = ParamQuery t' j ops q frames
      | otherwise = ParamQuery t' j ops q (frames <> [(t, args)])

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

-- | A fact read off what the term exposes to.
ofExposure :: (Query, Query) -> Exposed -> (Facts -> SymBool) -> (Facts -> SymBool) -> SymBool
ofExposure (q, qType) e onBody onShape = case e of
  ExposedTo _ ns _ g -> withConditions ns (onShape g)
  ExposedTable _ ns t args -> withConditions ns (resolveSym t args (onBody (tbBody t)))
  ExposedIn _ ns t args g -> withConditions ns (resolveSym t args (onShape g))
  ExposedParam _ ns t j -> SymBool ns (AnyOf [ParamQuery t j [] q []])
  ExposedParamType _ ns t j -> SymBool ns (AnyOf [ParamQuery t j [] qType []])
  ExposedParamApp _ ns t j ops -> SymBool ns (AnyOf [ParamQuery t j ops q []])
  ExposedUnknown r -> unknown r

metaOf :: Facts -> SymBool
metaOf f = ofExposure (QMeta, QTypeMeta) (fHeadView f) metaOf $ \g -> case fShape g of
  SUniverse -> known True
  SCubeU -> known True
  STopeU -> known True
  SPi _ _ dom _ _ cod _ -> metaOf dom `orSym` metaOf cod
  _ -> known False

landsOf :: Facts -> SymBool
landsOf f = ofExposure (QLands, QTypeLands) (fHeadView f) landsOf $ \g -> case fShape g of
  SUniverse -> known True
  SCubeU -> known True
  STopeU -> known True
  SPi _ _ _ _ _ cod _ -> landsOf cod
  _ -> known False

cubeOf :: Facts -> SymBool
cubeOf f = ofExposure (QCube, QTypeCube) (fHeadView f) cubeOf $ \g -> case fShape g of
  SCubeU -> known True
  _ -> known False

-- | Does 'fragmentHead' expose the term to a restricted type?
restrictedOf :: Facts -> SymBool
restrictedOf f = case fExpose f of
  ExposedTo _ ns _ g -> SymBool ns (Known (isRestrictedShape g))
  ExposedTable _ ns t args -> withConditions ns (resolveSym t args (restrictedOf (tbBody t)))
  ExposedIn _ ns t args g -> withConditions ns (resolveSym t args (restrictedOf g))
  ExposedParam _ ns t j -> SymBool ns (AnyOf [ParamQuery t j [] QRestricted []])
  ExposedParamApp _ ns t j ops -> SymBool ns (AnyOf [ParamQuery t j ops QRestricted []])
  ExposedParamType{} -> unknown "the type of an argument"
  ExposedUnknown r -> unknown r

-- * Points

pointsOf :: Facts -> Points
pointsOf f = case fShape f of
  SVar v -> vfReach v
  SParam t j -> mempty { pParams = [(Nothing, t, j)] }
  SParamType{} -> mempty { pUnknown = Just "the type of an argument" }
  SUniverse -> mempty
  SCubeU -> mempty
  STopeU -> mempty
  SLeaf _ -> mempty
  SCubeOrTope _ cs -> foldMap fPoints cs
  SPi _ _ dom bv mt cod _ -> fPoints dom <> without bv (foldMap fPoints mt <> fPoints cod)
  SSigma _ _ a bv b _ -> fPoints a <> without bv (fPoints b)
  SId a mt b -> fPoints a <> foldMap fPoints mt <> fPoints b
  SRestricted ty rs -> fPoints ty <> pairs rs
  SRecOr rs -> pairs rs
  SApp g x -> fPoints g <> fPoints x
  SLam _ _ mparam bv body _ ->
    foldMap (\(p, mt) -> fPoints p <> without bv (foldMap fPoints mt)) mparam <> without bv (fPoints body)
  SLet _ manno value bv body -> foldMap fPoints manno <> fPoints value <> without bv (fPoints body)
  SPair a b -> fPoints a <> fPoints b
  SFirst p -> fPoints p
  SSecond p -> fPoints p
  SRefl mx -> foldMap (fPoints . fst) mx
  SIdJ a x c d y p -> foldMap fPoints [a, x, c, d, y, p]
  STypeAsc v ty -> fPoints v <> fPoints ty
  STypeModal ty -> fPoints ty
  SScoped t args g -> scopedPoints t args (fPoints g)
  SOther r -> mempty { pUnknown = Just r }
  where
    pairs rs = foldMap (\(a, b) -> fPoints a <> fPoints b) rs

-- * Settling needs

-- | Settle the needs of a table's body against the arguments of a use. What
-- an argument answers may need more of the same arguments (an argument
-- applied inside the body), so this repeats until no need of this table is
-- left; needs of an enclosing table remain. Without the flags of the check,
-- only the needs that are answers to queries can be settled: those are the
-- conditions a fact is read under.
resolveTable :: Maybe FragmentFlags -> Table -> [Facts] -> Verdict -> Verdict
resolveTable fl t args = loop
  where
    loop = \case
      Needs ns | (mine@(_ : _), others) <- partition (\(Need t' _ _) -> sameTable t t') ns ->
        loop (foldMap settle (distinctNeeds mine) <> needs others)
      Needs ns -> Needs (map leaving (distinctNeeds ns))
      v -> v
    settle (Need _ j r) = evalReq fl (args !! j) r
    -- A need of another table that mentions this table's facts is settled
    -- under these arguments.
    leaving n@(Need t' j r)
      | mentionsFacts r = Need t' j (ReqUnder t args r)
      | otherwise = n
    mentionsFacts = \case
      ReqApplied _ _ ops -> not (null ops)
      ReqAnswer ops _ _ -> not (null ops)
      ReqBoundary ops -> not (null ops)
      ReqArgumentAt _ _ -> True
      ReqUnder{} -> True
      _ -> False

evalReq :: Maybe FragmentFlags -> Facts -> Req -> Verdict
evalReq mfl f = \case
  ReqClean view e -> withFlags $ \fl -> fragment fl view e f
  ReqApplied view e ops -> withFlags $ \fl -> fragment fl view e (applyOps f ops)
  ReqNoPoints -> noPointsBut Nothing (fPoints f)
  ReqAnswer ops q b -> mustBe (show q) b (queryOf q (applyOps f ops))
  ReqBoundary ops -> withFlags $ \fl -> boundaryAt fl ComputedTypes Nothing (applyOps f ops)
  ReqArgumentAt view arg -> withFlags $ \fl -> inView view (argumentAt fl f arg)
  ReqUnder t args r -> resolveTable mfl t args (evalReq mfl f r)
  where
    withFlags k = maybe (Unclean "a need that depends on the checks in force") k mfl

-- | Drop the repetitions of a need (a parameter used twice at an entry): a
-- need that mentions facts is kept as it is.
distinctNeeds :: [Need] -> [Need]
distinctNeeds = go Set.empty
  where
    go _ [] = []
    go seen (n@(Need t j r) : rest) = case keyOf r of
      Just k | Set.member (tbId t, j, k) seen -> go seen rest
             | otherwise -> n : go (Set.insert (tbId t, j, k) seen) rest
      Nothing -> n : go seen rest
    keyOf :: Req -> Maybe ReqKey
    keyOf = \case
      ReqClean v e -> Just (KeyClean v e)
      ReqNoPoints -> Just KeyNoPoints
      ReqAnswer [] q b -> Just (KeyAnswer q b)
      ReqBoundary [] -> Just KeyBoundary
      _ -> Nothing

-- | A need without facts in it, for 'distinctNeeds'.
data ReqKey = KeyClean View Entry | KeyNoPoints | KeyAnswer Query Bool | KeyBoundary
  deriving (Eq, Ord)

-- | A fact about a domain found through the type of a function node, which
-- may sit in the body of a table: the fact is settled against that table's
-- arguments.
data Scoped a = Scoped [(Table, [Facts])] a

scopedVerdict :: FragmentFlags -> Scoped Verdict -> Verdict
scopedVerdict fl (Scoped tables v) = foldl (\acc (t, args) -> resolveTable (Just fl) t args acc) v tables

scopedSym :: Scoped SymBool -> SymBool
scopedSym (Scoped tables s) = foldl (\acc (t, args) -> resolveSym t args acc) s tables

-- | The domain of a function's type, as @funDomain@ finds it. A type that
-- is a parameter's argument is left to the use ('Left' with the parameter).
domainOf :: Facts -> Scoped (Either (Either String (Table, Int, [Need])) (Maybe Facts), [Need])
domainOf ty = go [] (fHeadView ty)
  where
    go tables = \case
      ExposedTo _ ns _ g -> Scoped tables (Right (case fShape g of
        SPi _ _ dom _ _ _ _ -> Just dom
        _ -> Nothing), ns)
      ExposedTable _ ns t args ->
        let Scoped tables' (r, ns') = go ((t, args) : tables) (fHeadView (tbBody t))
        in Scoped tables' (r, ns <> ns')
      ExposedIn _ ns t args g ->
        let Scoped tables' (r, ns') = go ((t, args) : tables) (fHeadView g)
        in Scoped tables' (r, ns <> ns')
      ExposedParam _ ns t j -> Scoped tables (Left (Right (t, j, ns)), [])
      ExposedParamApp{} -> Scoped tables (Left (Left "a domain behind an applied argument"), [])
      ExposedParamType{} -> Scoped tables (Left (Left "a domain behind an argument's type"), [])
      ExposedUnknown r -> Scoped tables (Left (Left r), [])

-- * The fragment check

-- | The certificate of the standalone fragment walk at an entry, from the
-- term's memo.
fragment :: FragmentFlags -> View -> Entry -> Facts -> Verdict
fragment fl view e f = inView view (fragmentIn fl e f)

-- | Both views' verdicts at an entry, from the term's memo.
fragmentIn :: FragmentFlags -> Entry -> Facts -> Verdicts
fragmentIn fl e f = atEntry e (atFlags fl (fVerdicts f))

-- | The walk behind 'fragment', in both views at once: the views take the
-- same cases with the same children wherever a term exposes to itself, and
-- part only at a term that reduces. The cases follow 'checkFragmentUses' in
-- "Rzk.TypeCheck.Fragment.RSTT" one for one.
fragmentAt :: FragmentFlags -> Entry -> Facts -> Verdicts
fragmentAt fl = go
  where
    -- The entry's own walk; every walk of a subterm goes through its memo.
    go :: Entry -> Facts -> Verdicts
    go = \case
      ETail -> tailAt
      EAssumed p u -> assumedAt p u
      EExtCodomain u -> extCodomainAt u
      ETerm p -> termAt p
      EData p -> dataAt p

    goTail :: Facts -> Verdicts
    goTail = fragmentIn fl ETail
    goAssumed :: PrefixPosition -> FragmentUse -> Facts -> Verdicts
    goAssumed p u = fragmentIn fl (EAssumed p u)
    goExtCodomain :: FragmentUse -> Facts -> Verdicts
    goExtCodomain u = fragmentIn fl (EExtCodomain u)
    goTerm :: PrefixPosition -> Facts -> Verdicts
    goTerm p = fragmentIn fl (ETerm p)
    goData :: PrefixPosition -> Facts -> Verdicts
    goData p = fragmentIn fl (EData p)

    out :: PrefixPosition
    out = OutsideParameterPrefix

    both :: Verdict -> Verdicts
    both v = (v, v)

    -- The source view takes the term as it is; the computed view exposes it:
    -- a definition's application is answered by its table, a parameter by
    -- its argument.
    exposedAt :: Entry -> Facts -> (Facts -> Verdicts) -> Verdicts
    exposedAt e f k = case fExpose f of
      ExposedTo steps ns self g
        | steps >= exposeLimit -> (fst (k f), Unclean "near the reduction limit")
        | self -> let r = k f in (fst r, needs ns <> snd r)
        | otherwise -> (fst (k f), needs ns <> snd (k g))
      ExposedTable steps ns t args
        | steps < exposeLimit -> (fst (k f), needs ns <> resolveTable (Just fl) t args (tableVerdict t fl e))
        | otherwise -> (fst (k f), Unclean "near the reduction limit")
      ExposedIn steps ns t args g
        | steps < exposeLimit -> (fst (k f), needs ns <> resolveTable (Just fl) t args (fragment fl ComputedTypes e g))
        | otherwise -> (fst (k f), Unclean "near the reduction limit")
      ExposedParam _ ns t j -> (fst (k f), needs (ns <> [Need t j (ReqClean ComputedTypes e)]))
      ExposedParamApp _ ns t j ops -> (fst (k f), needs (ns <> [Need t j (ReqApplied ComputedTypes e ops)]))
      ExposedParamType{} -> (fst (k f), Unclean "the type of an argument, exposed")
      ExposedUnknown r -> (fst (k f), Unclean r)

    -- Allow restrictions along the spine of concluded codomains.
    tailAt :: Facts -> Verdicts
    tailAt f = exposedAt ETail f $ \t -> case fShape t of
      SRestricted ty rs -> goTail ty <> foldMap (\(tope, term) -> goTerm out tope <> goTerm out term) rs
      SPi _ _ param _ mtope cod _ ->
        shapeDependencies False t <> goAssumed out UseBinder param
          <> foldMap (goTerm out) mtope <> goTail cod
      SRecOr rs -> foldMap (\(tope, term) -> goTerm out tope <> goTail term) rs
      _ -> goAssumed out UseConcluded t

    -- Assumed restrictions must sit directly under a shape-Π.
    assumedAt :: PrefixPosition -> FragmentUse -> Facts -> Verdicts
    assumedAt p u f = exposedAt (EAssumed p u) f $ \t -> case fShape t of
      SRestricted ty rs ->
        both freeStanding <> goAssumed out u ty
          <> foldMap (\(tope, term) -> goTerm out tope <> goTerm out term) rs
      SPi _ _ param _ mtope cod _ ->
        let (shape, vs) = isShapeBinder mtope param
        in shapeDependencies True t <> goAssumed out UseBinder param
             <> foldMap (goTerm out) mtope <> both vs
             <> (if shape then goExtCodomain u cod else goAssumed out u cod)
      SSigma _ _ a _ b _ -> goAssumed out UseBinder a <> goAssumed out u b
      -- Identity-type arguments must be ext-style; endpoints remain terms.
      SId a mtA b -> goTerm out a <> foldMap (goAssumed out UseIdentity) mtA <> goTerm out b
      SLam _ _ mparam _ body _ -> lambdaBinder p mparam <> goAssumed p u body
      STypeModal ty -> goAssumed out u ty
      SRecOr rs -> foldMap (\(tope, term) -> goTerm out tope <> goAssumed out u term) rs
      SVar _ -> mempty
      _ -> goTerm out t

    -- Direct shape codomains admit boundary clipping to the binder's domain.
    extCodomainAt :: FragmentUse -> Facts -> Verdicts
    extCodomainAt u f = exposedAt (EExtCodomain u) f $ \t -> case fShape t of
      SRestricted ty rs ->
        goAssumed out u ty <> foldMap (\(face, term) -> goTerm out face <> goTerm out term) rs
      _ -> goAssumed out u t

    -- The binder of a λ: its domain is assumed, its tope is a term, and
    -- outside a parameter prefix it must not be schematic.
    lambdaBinder :: PrefixPosition -> Maybe (Facts, Maybe Facts) -> Verdicts
    lambdaBinder p = \case
      Nothing -> mempty
      Just (param, mtope) ->
        goAssumed out UseBinder param <> foldMap (goTerm out) mtope
          <> both (if p == out then metaBinder param else Clean)

    -- RS17 Appendix A.2 requires shapes independent of ambient cube points.
    -- Schematic families are judgements over cube contexts, not extension types.
    shapeDependencies :: Bool -> Facts -> Verdicts
    shapeDependencies assumed t
      | not (flagShapeDependency fl) = mempty
      | SPi _ _ param bv mtope cod _ <- fShape t =
          let (schematic, vs1) = decide (fMeta t)
              (shape, vs2) = isShapeBinder mtope param
          in both (vs1 <> vs2) <> if not shape then mempty else
               both (foldMap (\tope -> if schematic then Clean else noPointsBut (Just bv) (fPoints tope)) mtope
                 <> noPointsBut (Just bv) (fPoints param))
                 <> (if schematic || not assumed then mempty else
                       (boundaryAt fl SourceTypes (Just bv) cod, boundaryAt fl ComputedTypes (Just bv) cod))
      | otherwise = mempty

    -- Check term binders and route type-valued bodies through goData.
    termAt :: PrefixPosition -> Facts -> Verdicts
    termAt InParameterPrefix f | not (isLambda f) = goData InParameterPrefix f
    termAt p f = case fShape f of
      SVar _ -> mempty
      SParam t j -> (needs [Need t j (ReqClean SourceTypes (ETerm p))], needs [Need t j (ReqClean ComputedTypes (ETerm p))])
      SParamType{} -> both (Unclean "the type of an argument, as a term")

      -- Function types returned by lets or branches still introduce shapes.
      SPi{} -> goAssumed out UseData f

      -- The stored type is synthesised by elaboration; it is not assumed.
      SRefl mx -> foldMap (goTerm out . fst) mx

      STypeAsc term ty -> goTerm out term <> goTail ty

      SLam _ _ mparam _ body _ ->
        (case fShape (fType f) of
           SPi{} -> shapeDependencies False (fType f)
           _ -> mempty)
          <> lambdaBinder p mparam <> goTerm p body

      SApp{} -> case fShape h of
        -- An argument applied inside a definition: the walk sees the
        -- application of the actual argument.
        SParam t j ->
          ( needs [Need t j (ReqApplied SourceTypes (ETerm p) (map (OpApp . snd) args))]
          , needs [Need t j (ReqApplied ComputedTypes (ETerm p) (map (OpApp . snd) args))] )
        _ -> goTerm out h <> foldMap argument args
        where
          (h, args) = spine f

      -- The motive is assumed: the eliminator binds its variables at it.
      SIdJ tA a tC d x pf ->
        goData out tA <> goTerm out a <> goAssumed out UseMotive tC
          <> goTerm out d <> goTerm out x <> goTerm out pf

      SPair l r -> goData out l <> goData out r

      -- The let annotation records the value's concluded type.
      SLet _ manno value _ body -> foldMap goTail manno <> goData out value <> goTerm out body

      -- Every other node: its unscoped children are terms.
      SUniverse -> mempty
      SCubeU -> mempty
      STopeU -> mempty
      SLeaf _ -> mempty
      SCubeOrTope _ cs -> foldMap (goTerm out) cs
      SSigma _ _ a _ _ _ -> goTerm out a
      SId a mtA b -> goTerm out a <> foldMap (goTerm out) mtA <> goTerm out b
      SRestricted ty rs -> goTerm out ty <> foldMap (\(a, b) -> goTerm out a <> goTerm out b) rs
      SRecOr rs -> foldMap (\(a, b) -> goTerm out a <> goTerm out b) rs
      SFirst t -> goTerm out t
      SSecond t -> goTerm out t
      STypeModal ty -> goTerm out ty
      SScoped t args g ->
        let (src, cmp) = goTerm p g
        in (resolveTable (Just fl) t args src, resolveTable (Just fl) t args cmp)
      SOther r -> both (Unclean r)

    -- An argument, given the function node it is applied to.
    argument :: (Facts, Facts) -> Verdicts
    argument (fnode, arg) = argumentAt fl (fType fnode) arg

    -- Type-valued data may later become binder or motive types. A proof
    -- quantified over a universe must still be walked as a term.
    dataAt :: PrefixPosition -> Facts -> Verdicts
    dataAt p f = case fShape f of
      SParam t j -> (needs [Need t j (ReqClean SourceTypes (EData p))], needs [Need t j (ReqClean ComputedTypes (EData p))])
      _ ->
        let (isType, vs) = decide (fLands (fType f))
        in both vs <> (if isType then goAssumed p UseData f else goTerm out f)

    isLambda :: Facts -> Bool
    isLambda f = case fShape f of
      SLam{} -> True
      _      -> False

    -- A cube domain without a tope denotes the unrestricted shape.
    isShapeBinder :: Maybe Facts -> Facts -> (Bool, Verdict)
    isShapeBinder (Just _) _ = (True, Clean)
    isShapeBinder Nothing param = decide (fCube (fType param))

    metaBinder :: Facts -> Verdict
    metaBinder param
      | not (flagMetaBinder fl) = Clean
      | otherwise = let (meta, vs) = decide (fMeta param)
                    in vs <> (if meta then Unclean "a schematic binder inside a term" else Clean)

    freeStanding :: Verdict
    freeStanding
      | flagFreeStandingRestriction fl = Unclean "a free-standing restriction"
      | otherwise = Clean

-- | The application rule's checks of an argument, given the function's type
-- (the standalone walk's @goArgument@), in both views.
argumentAt :: FragmentFlags -> Facts -> Facts -> Verdicts
argumentAt fl ty arg =
  let Scoped tables (dom, ns) = domainOf ty
  in case dom of
    Left (Left r) -> (Unclean r, Unclean r)
    -- The type is a parameter's argument: the use settles it.
    Left (Right (t, j, ns')) ->
      ( scopedVerdict fl (Scoped tables (needs (ns <> ns' <> [Need t j (ReqArgumentAt SourceTypes arg)])))
      , scopedVerdict fl (Scoped tables (needs (ns <> ns' <> [Need t j (ReqArgumentAt ComputedTypes arg)]))) )
    Right mdom ->
      -- Substitution can create a forbidden type in a later argument domain,
      -- which the computed view checks.
      ( scopedVerdict fl (Scoped tables (needs ns))
      , scopedVerdict fl (Scoped tables (needs ns <> foldMap (fragment fl ComputedTypes (EAssumed OutsideParameterPrefix UseBinder)) mdom)) )
      -- Schema arguments may bind their own schematic parameters.
      <> let (plumbing, vs) = decide (scopedSym (Scoped tables (maybe (known False) fMeta mdom)))
             route = if plumbing then fragmentIn fl (ETerm InParameterPrefix) arg else fragmentIn fl (EData OutsideParameterPrefix) arg
         in (vs, vs) <> route

-- | The boundary of an assumed extension type must not mention outer points:
-- the codomain, exposed, is either not a restricted type or one whose faces
-- reach no cube point but the binder's own.
boundaryAt :: FragmentFlags -> View -> Maybe VarFacts -> Facts -> Verdict
boundaryAt fl view bound cod = case view of
  SourceTypes -> faces cod
  ComputedTypes -> case fExpose cod of
    ExposedTo steps ns _ body
      | steps < exposeLimit -> needs ns <> faces body
      | otherwise -> Unclean "near the reduction limit"
    ExposedTable steps ns t args
      | steps < exposeLimit -> needs ns <> resolveTable (Just fl) t args (boundaryAt fl view Nothing (tbBody t))
      | otherwise -> Unclean "near the reduction limit"
    ExposedIn steps ns t args g
      | steps < exposeLimit -> needs ns <> resolveTable (Just fl) t args (boundaryAt fl view Nothing g)
      | otherwise -> Unclean "near the reduction limit"
    ExposedParam _ ns t j -> needs (ns <> [Need t j (ReqBoundary [])])
    ExposedParamApp _ ns t j ops -> needs (ns <> [Need t j (ReqBoundary ops)])
    ExposedParamType{} -> Unclean "a boundary that is an argument's type"
    ExposedUnknown r -> Unclean r
  where
    faces body = case fShape body of
      SRestricted _ rs -> foldMap (noPointsBut bound . fPoints . fst) rs
      _ -> Clean

-- | The facts of a declaration's type and value, built once for the checks.
type DeclFacts = (Facts, Maybe Facts)

declarationFactsIn :: Distinct n => Env n -> TermT n -> Maybe (TermT n) -> DeclFacts
declarationFactsIn env ty mval = (facts env ty, facts env <$> mval)

-- | The certificate of the fragment check on a declaration: clean in both
-- views, at the entries the standalone walk starts from.
fragmentVerdict :: FragmentFlags -> DeclFacts -> Bool -> Verdict
fragmentVerdict fl (tyF, valF) isAssumption =
  foldMap (\view -> fragment fl view entry tyF <> foldMap (fragment fl view (ETerm InParameterPrefix)) valF)
    [SourceTypes, ComputedTypes]
  where
    entry = if isAssumption then EAssumed OutsideParameterPrefix UseBinder else ETail

-- * The schematic check
--
-- The certificate of "Rzk.TypeCheck.Schematic": the same classification of
-- types into object types, parameter kinds, schematic rules and shapes, from
-- the facts, with the parameters of a table symbolic. The cases follow
-- 'classify', 'term', 'signature', 'argument' and 'schematicTypeOf' there one
-- for one. The standalone check also checks the top-level entries it meets
-- whose own check is not recorded; the certificate collects them instead, and
-- the caller consults their status.

-- | What the schematic check finds: nothing to report, with the top-level
-- entries it relies on; a failure; or, in a table, needs on the arguments.
data SVerdict = SOk IntSet.IntSet | SFail String | SNeeds IntSet.IntSet [SNeed]

instance Semigroup SVerdict where
  SFail r <> _ = SFail r
  _ <> SFail r = SFail r
  SOk a <> SOk b = SOk (IntSet.union a b)
  SOk a <> SNeeds b ns = SNeeds (IntSet.union a b) ns
  SNeeds a ns <> SOk b = SNeeds (IntSet.union a b) ns
  SNeeds a ns <> SNeeds b ms = SNeeds (IntSet.union a b) (ns <> ms)

instance Monoid SVerdict where
  mempty = SOk IntSet.empty

data SNeed = SNeed Table Int SReq

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
  | SReqUnder Table [Facts] SReq

-- | The sorts of "Rzk.TypeCheck.Schematic".
data SSort = SObjectType | SParameterKind | SSchematicRule | SShape
  deriving (Eq, Show)

-- | The classification of a type, with what finding it relied on.
data Sorted = Sorted SSort SVerdict | Unsorted String

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

schemOf :: Facts -> Schem
schemOf f = Schem
  { scSort = classifyOf f
  , scHead = exposedSort (fExpose f)
  , scTerm = termOf f
  , scSignature = signatureOf f
  , scType = schemTypeOf f
  }

-- | The depth of terms and of exposures up to which the certificate stands in
-- for the standalone check, whose fuel (at least 'defaultRSTTFuel') is spent
-- by nesting and by unfolding; beyond it the standalone check runs.
schematicDepthLimit :: Int
schematicDepthLimit = 60

classifyM :: Facts -> Sorted
classifyM = scSort . fSchem

headSortM :: Facts -> Sorted
headSortM = scHead . fSchem

termM :: Facts -> SVerdict
termM = scTerm . fSchem

signatureM :: Facts -> SVerdict
signatureM = scSignature . fSchem

schemTypeM :: Facts -> Facts
schemTypeM = scType . fSchem

-- | The verdict part of a classification.
sortV :: Sorted -> SVerdict
sortV (Sorted _ v) = v
sortV (Unsorted r) = SFail r

withV :: SVerdict -> Sorted -> Sorted
withV v (Sorted s v') = Sorted s (v <> v')
withV _ u = u

-- | What @uses@ records at a node: a top-level entry.
usesOf :: Facts -> SVerdict
usesOf f = case fShape f of
  SVar v | Just _ <- vfTop v -> SOk (IntSet.singleton (vfKey v))
  _ -> mempty

-- | @classify@: the uses and redexes of the term as written, then the known
-- object families, then the head of what it exposes to.
classifyOf :: Facts -> Sorted
classifyOf f = withV (usesOf f <> inspectRedex f) $ case knownObjectFamily f of
  Just v -> Sorted SObjectType v
  Nothing -> headSortM f

-- | Check erased arguments and annotations before exposing the result.
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
  SVar v | Just tl <- vfTop v, Just (arity, probe) <- tlObjectFamily tl, arity == length args -> Just probe
  _ -> Nothing
  where (h, args) = spine f

-- | The arity at which an entry is an object-valued family: its type's
-- parameters are probed at fresh variables, and the value applied to them
-- must classify as an object type.
objectFamilyOf :: Facts -> Maybe Facts -> Maybe (Int, SVerdict)
objectFamilyOf ty mval = mval >>= go (0 :: Int) ty
  where
    go arity t v
      | fDepth v > schematicDepthLimit = Nothing
      | otherwise = case fExpose t of
          ExposedTo steps [] _ g | steps <= schematicDepthLimit -> case fShape g of
            SPi _ _ _ bv _ cod _ -> go (arity + 1) cod (appFacts v (varFacts bv))
            SUniverse -> case classifyM v of
              Sorted SObjectType verdict -> Just (arity, verdict)
              _ -> Nothing
            _ -> Nothing
          _ -> Nothing

-- | The classification of what a term exposes to (@classifyHead@).
exposedSort :: Exposed -> Sorted
exposedSort = \case
  ExposedTo steps [] _ g
    | steps <= schematicDepthLimit -> classifyExposed g
    | otherwise -> Unsorted "near the reduction limit"
  ExposedTo _ _ _ _ -> Unsorted "a type exposed under conditions"
  ExposedTable steps [] t args
    | steps <= schematicDepthLimit -> scoped t args (headSortM (tbBody t))
    | otherwise -> Unsorted "near the reduction limit"
  ExposedIn steps [] t args g
    | steps <= schematicDepthLimit -> scoped t args (headSortM g)
    | otherwise -> Unsorted "near the reduction limit"
  ExposedParam _ [] t j -> case declaredSort t j of
    Just s -> Sorted s (SNeeds IntSet.empty [SNeed t j (SReqSort s)])
    Nothing -> Unsorted ("a parameter of a kind that is not a sort: " <> show j <> " of " <> tbName t)
  ExposedParamApp _ [] t j ops -> stuckSort (applyOps (paramFacts t j (declaredOf t j)) ops)
  ExposedParamType{} -> Unsorted "the type of an argument, classified"
  ExposedUnknown r -> Unsorted r
  _ -> Unsorted "an exposure under conditions"
  where
    scoped t args = \case
      Sorted s v -> Sorted s (resolveS t args v)
      u -> u

-- | The declared type of a table's parameter, if known.
declaredOf :: Table -> Int -> Maybe Facts
declaredOf t j = tbDeclared t !! j

-- | The sort of a parameter as the probe at a fresh variable finds it: by its
-- declared type.
declaredSort :: Table -> Int -> Maybe SSort
declaredSort t j = declaredOf t j >>= \d -> case fExpose d of
  ExposedTo _ [] _ g -> case fShape g of
    SUniverse -> Just SObjectType
    SCubeU -> Just SShape
    STopeU -> Just SShape
    _ -> Nothing
  _ -> Nothing

classifyExposed :: Facts -> Sorted
classifyExposed g = case fShape g of
  SUniverse -> Sorted SParameterKind mempty
  SCubeU -> Sorted SParameterKind mempty
  STopeU -> Sorted SParameterKind mempty
  SPi _ _ dom _ mtope cod _ -> case (classifyM dom, classifyM cod) of
    (Sorted d dv, Sorted r rv) ->
      let v = dv <> foldMap termM mtope <> rv
      in case (d, r) of
        (SObjectType, SObjectType) -> Sorted SObjectType v
        (SShape, SObjectType) -> Sorted SObjectType v
        (_, SParameterKind) -> Sorted SParameterKind v
        (_, SShape) -> Unsorted "cube or shape family"
        _ -> Sorted SSchematicRule v
    (Unsorted r, _) -> Unsorted r
    (_, Unsorted r) -> Unsorted r
  SSigma _ _ a _ b _ -> case (classifyM a, classifyM b) of
    (Sorted d dv, Sorted r rv) ->
      Sorted (if d == SObjectType && r == SObjectType then SObjectType else SParameterKind) (dv <> rv)
    (Unsorted r, _) -> Unsorted r
    (_, Unsorted r) -> Unsorted r
  SId a mt b -> Sorted SObjectType (sobject (fromMaybe (schemTypeM a) mt) <> termM a <> termM b)
  SLeaf LTypeUnit -> Sorted SObjectType mempty
  SLeaf LRecBottom -> Sorted SObjectType mempty
  SRecOr rs -> case map (classifyM . snd) rs of
    Sorted s v : rest | all (sameSort s) rest -> Sorted s (v <> foldMap sortV rest)
    Unsorted r : _ -> Unsorted r
    _ -> Unsorted "case split between different schematic kinds"
  SRestricted ty rs -> case classifyM ty of
    Sorted s v -> Sorted s (v <> foldMap (\(p, x) -> termM p <> sargument ty x) rs)
    u -> u
  SLeaf LCubeUnit -> Sorted SShape mempty
  SLeaf LCube2 -> Sorted SShape mempty
  _ -> stuckSort g
  where
    sameSort s (Sorted s' _) = s == s'
    sameSort _ _ = False

-- | A term that is no type former: a neutral term whose type is a universe.
stuckSort :: Facts -> Sorted
stuckSort g = withV (termNeutral g) $ case fExpose (schemTypeM g) of
  ExposedTo steps [] _ ty | steps <= schematicDepthLimit -> case fShape ty of
    SUniverse -> Sorted SObjectType mempty
    SCubeU -> Sorted SShape mempty
    STopeU -> Sorted SShape mempty
    _ -> Unsorted "expected an object type, parameter kind, or schematic rule"
  ExposedTo _ _ _ _ -> Unsorted "a type exposed under conditions"
  ExposedUnknown r -> Unsorted r
  _ -> Unsorted "a kind behind a definition or an argument"

sobject :: Facts -> SVerdict
sobject t = case classifyM t of
  Sorted SObjectType v -> v
  Sorted s _ -> SFail ("expected an object type, found " <> show s <> " for " <> shapeName (fShape t))
  Unsorted r -> SFail r

-- | A type-family argument is checked pointwise.
sargument :: Facts -> Facts -> SVerdict
sargument dom arg = case functionHeadOf dom of
  Left r -> case fExpose dom of
    -- The domain is an argument: what it accepts is up to the use.
    ExposedParam _ [] t j -> SNeeds IntSet.empty [SNeed t j (SReqDomainFor [] arg)]
    ExposedParamApp _ [] t j ops -> SNeeds IntSet.empty [SNeed t j (SReqDomainFor ops arg)]
    _ -> SFail r
  Right (scope, h) -> case fShape h of
    SUniverse -> sobject arg
    SPi _ _ _ bv mtope cod _ -> case classifyM dom of
      Sorted SParameterKind v ->
        v <> resolveScopes scope (sargument cod (appFacts arg (varFacts bv)) <> foldMap termM mtope)
      Sorted _ v -> v <> termM arg
      Unsorted r -> SFail r
    _ -> termM arg

-- | @functionHead@: what a type exposes to, restrictions stripped, which may
-- be the body of a table (then with the table and its arguments to settle
-- what is found there).
functionHeadOf :: Facts -> Either String ([(Table, [Facts])], Facts)
functionHeadOf = go [] . fExpose
  where
    go scope = \case
      ExposedTo steps [] _ g
        | steps > schematicDepthLimit -> Left "near the reduction limit"
        | SRestricted base _ <- fShape g -> go scope (fExpose base)
        | otherwise -> Right (scope, g)
      ExposedTo _ _ _ _ -> Left "a type exposed under conditions"
      ExposedTable steps [] t args
        | steps <= schematicDepthLimit -> go ((t, args) : scope) (fExpose (tbBody t))
        | otherwise -> Left "near the reduction limit"
      ExposedIn steps [] t args g
        | steps <= schematicDepthLimit -> go ((t, args) : scope) (fExpose g)
        | otherwise -> Left "near the reduction limit"
      -- A parameter of a table in scope stands for an argument of the use.
      ExposedParam k [] t j
        | Just args <- lookupScope t scope -> go scope (later k (fExpose (args !! j)))
        | otherwise -> Left ("a function type that is parameter " <> show j <> " of " <> tbName t)
      ExposedParamApp k [] t j ops
        | Just args <- lookupScope t scope ->
            go scope (later k (fExpose (applyOps (args !! j) (map (scopeOp (underScopes (scopesFrom t scope))) ops))))
        | otherwise -> Left ("a function type that is applied parameter " <> show j <> " of " <> tbName t)
      ExposedParamType k [] t j
        | Just args <- lookupScope t scope -> go scope (later k (fExpose (fType (args !! j))))
        | otherwise -> Left "a function type that is an argument's type"
      ExposedUnknown r -> Left r
      _ -> Left "a function type exposed under conditions"

lookupScope :: Table -> [(Table, [Facts])] -> Maybe [Facts]
lookupScope t scope = snd <$> find (sameTable t . fst) scope

-- | The scopes from the given table outwards.
scopesFrom :: Table -> [(Table, [Facts])] -> [(Table, [Facts])]
scopesFrom t = dropWhile (not . sameTable t . fst)

resolveScopes :: [(Table, [Facts])] -> SVerdict -> SVerdict
resolveScopes scope v = foldl (\acc (t, args) -> resolveS t args acc) v scope

-- | @applications@: the function, then the argument at the function's domain.
applications :: Facts -> SVerdict
applications f = case fShape f of
  SApp g x -> termM g <> case functionHeadOf (schemTypeM g) of
    Right (scope, h) | SPi _ _ dom _ _ _ _ <- fShape h -> resolveScopes scope (sargument dom x)
    Right (_, h) -> SFail ("application without a schematic or object function type: " <> shapeName (fShape h))
    Left r -> SFail r
  _ -> mempty

-- | @term@.
termOf :: Facts -> SVerdict
termOf f = usesOf f <> case fShape f of
  SPi{} -> sortV (classifyM f)
  SSigma{} -> sortV (classifyM f)
  SId{} -> sortV (classifyM f)
  SRestricted{} -> sortV (classifyM f)
  SLam _ _ mparam _ body _ ->
    (case mparam of
       Just (dom, mtope) -> foldMap termM mtope <> sortV (classifyM dom)
       Nothing -> case functionHeadOf (fType f) of
         Right (scope, h) | SPi _ _ dom _ _ _ _ <- fShape h -> resolveScopes scope (sortV (classifyM dom))
         Right _ -> SFail "lambda without a function type"
         Left r -> SFail r)
      <> termM body
  SApp{} -> applications f
  SLet _ manno value _ body -> foldMap signatureM manno <> termM value <> termM body
  STypeAsc value ty -> signatureM ty <> termM value
  SIdJ a x motive d y p ->
    sobject a <> sargument (schemTypeM motive) motive <> foldMap termM [x, motive, d, y, p]
  _ -> termNeutral f

-- | @termNeutral@: an allow-list of what may appear in a checked term.
termNeutral :: Facts -> SVerdict
termNeutral f = case fShape f of
  SVar _ -> mempty
  SParam t j -> SNeeds IntSet.empty [SNeed t j SReqTerm]
  SUniverse -> mempty
  SCubeU -> mempty
  STopeU -> mempty
  SLeaf LHole -> SFail "unsupported schematic expression"
  SLeaf LInterval -> SFail "unsupported schematic expression"
  SLeaf _ -> mempty
  SCubeOrTope True cs -> foldMap termM cs
  SCubeOrTope False _ -> SFail "unsupported schematic expression"
  SPair x y -> case functionHeadOf (fType f) of
    Right (scope, h) -> case fShape h of
      SSigma _ _ a _ _ open -> resolveScopes scope (sargument a x <> sargument (open x) y)
      SCubeOrTope True _ -> termM x <> termM y
      _ -> SFail "pair without a product type"
    Left r -> SFail r
  SFirst p -> termM p
  SSecond p -> termM p
  SRefl mx -> foldMap (\(x, mty) -> sobject (fromMaybe (schemTypeM x) mty) <> termM x) mx
  SRecOr rs -> foldMap (\(a, b) -> termM a <> termM b) rs
  SApp{} -> applications f
  SScoped t args g -> resolveS t args (termNeutral g)
  _ -> SFail ("unsupported schematic expression: " <> shapeName (fShape f))

-- | @signature@.
signatureOf :: Facts -> SVerdict
signatureOf f = usesOf f <> inspectRedex f <> case fExpose f of
  ExposedTo steps [] _ g
    | steps > schematicDepthLimit -> SFail "near the reduction limit"
    | SPi _ _ dom _ mtope cod _ <- fShape g -> sortV (classifyM dom) <> foldMap termM mtope <> signatureM cod
    | otherwise -> sortV (classifyM g)
  ExposedTo _ _ _ _ -> SFail "a signature exposed under conditions"
  ExposedTable steps [] t args
    | steps <= schematicDepthLimit -> resolveS t args (signatureM (tbBody t))
    | otherwise -> SFail "near the reduction limit"
  ExposedIn steps [] t args g
    | steps <= schematicDepthLimit -> resolveS t args (signatureM g)
    | otherwise -> SFail "near the reduction limit"
  ExposedParam _ [] t j -> SNeeds IntSet.empty [SNeed t j SReqSignature]
  ExposedUnknown r -> SFail r
  _ -> SFail "a signature exposed under conditions"

-- | @schematicTypeOf@: the type of a term as the check derives it, from the
-- current signatures rather than from an application's annotation.
schemTypeOf :: Facts -> Facts
schemTypeOf f = case fShape f of
  SVar v -> vfSchemType v
  SParam t j -> fromMaybe (unknownFacts "the type of an argument") (declaredOf t j)
  SApp g x -> case functionHeadOf (schemTypeM g) of
    Right (scope, h) | SPi _ _ _ _ _ _ open <- fShape h -> underScopes scope (snd (open x))
                     | otherwise -> unknownFacts ("a function type that is " <> shapeName (fShape h))
    Left r -> unknownFacts r
  SFirst p -> case functionHeadOf (schemTypeM p) of
    Right (scope, h) | SSigma _ _ a _ _ _ <- fShape h -> underScopes scope a
                     | SCubeOrTope True [a, _] <- fShape h -> underScopes scope a
                     | otherwise -> unknownFacts ("a product type that is " <> shapeName (fShape h))
    Left r -> unknownFacts r
  SSecond p -> case functionHeadOf (schemTypeM p) of
    Right (scope, h) | SSigma _ _ a _ _ open <- fShape h -> underScopes scope (open (mkFacts (SFirst p) (underScopes scope a)))
                     | SCubeOrTope True [_, b] <- fShape h -> underScopes scope b
                     | otherwise -> unknownFacts ("a product type that is " <> shapeName (fShape h))
    Left r -> unknownFacts r
  _ -> fType f

-- | Settle the needs of a table's body against the arguments of a use, as
-- 'resolveTable' does for the fragment check.
resolveS :: Table -> [Facts] -> SVerdict -> SVerdict
resolveS t args = loop
  where
    loop = \case
      SNeeds uses ns | (mine@(_ : _), others) <- partition (\(SNeed t' _ _) -> sameTable t t') ns ->
        loop (SOk uses <> foldMap settle mine <> sneeds others)
      SNeeds uses ns -> SNeeds uses (map leaving ns)
      v -> v
    sneeds [] = mempty
    sneeds ns = SNeeds IntSet.empty ns
    settle (SNeed _ j r) = evalS (args !! j) r
    leaving n@(SNeed t' j r) = case r of
      SReqArgument _ -> SNeed t' j (SReqUnder t args r)
      SReqDomainFor _ _ -> SNeed t' j (SReqUnder t args r)
      SReqUnder{} -> SNeed t' j (SReqUnder t args r)
      _ -> n

evalS :: Facts -> SReq -> SVerdict
evalS f = \case
  SReqSort s -> case classifyM f of
    Sorted s' v | s' == s -> v
                | otherwise -> SFail ("the sort differs: " <> show s' <> " for " <> show s)
    Unsorted r -> SFail r
  SReqTerm -> termM f
  SReqSignature -> signatureM f
  SReqArgument dom -> sargument dom f
  SReqDomainFor ops x -> sargument (applyOps f ops) x
  SReqUnder t as r -> resolveS t as (evalS f r)

-- | The certificate of the schematic check on a declaration: of its
-- signature, of its value if any, and the depth of the terms.
schematicVerdict :: DeclFacts -> (SVerdict, SVerdict, Int)
schematicVerdict (tyF, valF) = schematicVerdictFacts tyF valF

-- | The same, of a top-level entry.
schematicVerdictOf :: VarFacts -> (SVerdict, SVerdict, Int)
schematicVerdictOf v = schematicVerdictFacts (vfType v) (vfValue v)

schematicVerdictFacts :: Facts -> Maybe Facts -> (SVerdict, SVerdict, Int)
schematicVerdictFacts tyF valF =
  ( signatureM tyF
  , foldMap termM valF
  , max (fDepth tyF) (maybe 0 fDepth valF) )

-- * The meta-parameter layer check
--
-- The certificate of "Rzk.TypeCheck.MetaPrefix": the use-site walk over the
-- facts, position by position as there. It never unfolds a definition, so the
-- walk is plain; the only facts it asks are the meta-ness of the domains of
-- the functions applied, which is 'domainMetaOf'.

-- | Whether the domain of a function's type is a schematic kind
-- (@domainIsMeta@ in "Rzk.TypeCheck.MetaPrefix").
domainMetaOf :: Facts -> SymBool
domainMetaOf ty = case domainOf ty of
  Scoped tables (Right (Just dom), ns) -> scopedSym (Scoped tables (withConditions ns (fMeta dom)))
  Scoped tables (Right Nothing, ns) -> scopedSym (Scoped tables (withConditions ns (known False)))
  Scoped tables (Left (Right (t, j, ns)), _) ->
    scopedSym (Scoped tables (SymBool ns (AnyOf [ParamQuery t j [] QDomainMeta []])))
  Scoped _ (Left (Left r), _) -> unknown r

-- | How a position is classified under the two candidate rules.
data PosKind = MetaPos | ObjectPos
  deriving (Eq)

-- | The certificate of the meta-parameter layer check on a declaration, under
-- the strict rule or the structural one.
metaPrefixVerdict :: Bool -> DeclFacts -> Verdict
metaPrefixVerdict strict (tyF, valF) =
  go (MetaPos, MetaPos) tyF <> foldMap (go (MetaPos, MetaPos)) valF
  where
    typePos = (MetaPos, MetaPos)
    objPos = (ObjectPos, ObjectPos)

    go :: (PosKind, PosKind) -> Facts -> Verdict
    go pos f = case fShape f of
      SVar v -> checkHead pos v 0
      SParam{} -> Unclean "a parameter"
      SParamType{} -> Unclean "the type of an argument"
      SScoped{} -> Unclean "facts from a definition's body"
      SApp{} ->
        let (h, args) = spine f
            (mk, headVerdict) = case fShape h of
              SVar v -> (vfTop v >>= \tl -> Just (tlMetaPrefix tl), checkHead pos v (length args))
              _ -> (Nothing, go objPos h)
        in headVerdict <> mconcat
             [ let (domMeta, vs) = decide (domainMetaOf (fType fnode))
                   strictPos = case mk of
                     Just k | domMeta && i <= k -> MetaPos
                     _ -> ObjectPos
               in vs <> go (if domMeta then MetaPos else ObjectPos, strictPos) arg
             | (i, (fnode, arg)) <- zip [1 :: Int ..] args ]
      SLam _ _ mparam _ body _ ->
        foldMap (\(p, mt) -> go typePos p <> foldMap (go objPos) mt) mparam <> go pos body
      SPi _ _ param _ mtope cod _ -> go typePos param <> foldMap (go objPos) mtope <> go typePos cod
      SSigma _ _ a _ b _ -> go typePos a <> go typePos b
      -- The endpoints are terms; storing an unsaturated schema in an
      -- identity type is an object-level use.
      SId a mtA b -> go objPos a <> foldMap (go typePos) mtA <> go objPos b
      SRestricted ty' rs -> go typePos ty' <> foldMap (\(a, b) -> go objPos a <> go objPos b) rs
      SLet _ manno value _ body -> foldMap (go typePos) manno <> go objPos value <> go pos body
      SOther r -> Unclean r
      -- Every other node: its unscoped children are object positions.
      SUniverse -> Clean
      SCubeU -> Clean
      STopeU -> Clean
      SLeaf _ -> Clean
      SCubeOrTope _ cs -> foldMap (go objPos) cs
      SRecOr rs -> foldMap (\(a, b) -> go objPos a <> go objPos b) rs
      SPair a b -> go objPos a <> go objPos b
      SFirst p -> go objPos p
      SSecond p -> go objPos p
      SRefl mx -> foldMap (\(x, mt) -> go objPos x <> foldMap (go objPos) mt) mx
      SIdJ a x c d y p -> foldMap (go objPos) [a, x, c, d, y, p]
      STypeAsc v t -> go objPos v <> go objPos t
      STypeModal t -> go objPos t

    -- An unsaturated top-level head at an object position warns; at a
    -- position only the strict rule rejects, under the strict rule.
    checkHead :: (PosKind, PosKind) -> VarFacts -> Int -> Verdict
    checkHead (structural, strictPos) v nargs = case vfTop v of
      Just tl | tlMetaPrefix tl > nargs
              , structural == ObjectPos || (strictPos == ObjectPos && strict) ->
          Unclean "unsaturated schematic parameters"
      _ -> Clean
