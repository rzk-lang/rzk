-- The scope-extension evidence on the three sinkers that are coercions
-- ('sinkContextUnchecked', 'sinkVars', 'sinkNamed') is their soundness contract,
-- not an argument they can consume, so GHC calls it redundant. It stays.
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-redundant-constraints #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The typing context on free-foil.
--
-- The successor of @Rzk.TypeCheck@'s @Context var@. Two things change, and they
-- are the point of the migration.
--
-- [A variable is a name, and so is a top-level entry.] A free-foil term refers to
-- a variable by 'Foil.Name' (an @Int@), and by nothing else: there is no way to
-- put a 'VarIdent' inside a term. So a top-level definition is a name too, bound
-- in the outermost scope with no binder above it, and the hypotheses — global and
-- local alike — are one 'Foil.NameMap', looked up in constant time. The old
-- context was an association list keyed by a @var@ whose equality walked an
-- @S@-chain; @lookupVarInfo@ alone was 11.5% of the checker's time. The surface
-- name of an entry is resolved through 'ctxNamed'.
--
-- This also gives sections their natural shape. A @#assume@d assumption is an
-- ordinary binder, so the definitions of a section are checked in its scope, and
-- closing the section abstracts the assumption by pairing its binder with the
-- body — no rewrite of the elaborated terms (see @Rzk.TypeCheck.Decl@).
--
-- [Entering a binder rebuilds nothing.] 'enterBinder' extends the scope and
-- sinks the rest of the context by coercion, so it costs O(1) rather than a walk
-- of the context. The old @enterScopeContext@ mapped @S \<$\>@ over the whole
-- context, rebuilding every elaborated term it held; the heap profile showed
-- those forced copies retaining most of the live heap, and @GlobalScopeInfo@ /
-- @globalEmbed@ (PR #277) exist only to keep that shift off the ~1500 top-level
-- entries. All of that machinery is gone.
module Rzk.TypeCheck.Context where

import           Control.Monad.Foil          (DExt, Distinct, NameBinder,
                                              NameMap, Scope)
import qualified Control.Monad.Foil          as Foil
-- NOTE: free-foil 0.2.0 gives 'NameMap' no 'Functor' instance (it has one on the
-- unreleased main), so 'mapNameMap' goes through the underlying 'IntMap'.
import           Control.Monad.Foil.Internal (NameMap (..))
import qualified Data.IntMap                 as IntMap
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Unsafe.Coerce               (unsafeCoerce)

import           Language.Rzk.Foil.Syntax
import           Language.Rzk.Foil.Names    (Binder (..), TModality (..),
                                              VarIdent, binderName)
import qualified Language.Rzk.Syntax         as Rzk

-- * The pieces of a context

data Covariance
  = Covariant     -- ^ Positive position.
  | Contravariant -- ^ Negative position.
  | Invariant     -- ^ Unknown position.

data RenderBackend
  = RenderSVG
  | RenderLaTeX

data Verbosity
  = Debug
  | Normal
  | Silent
  deriving (Eq, Ord)

data LocationInfo = LocationInfo
  { locationFilePath :: Maybe FilePath
  , locationLine     :: Maybe Int
  } deriving (Eq, Show)

-- | What is known about a hypothesis, local or top-level.
data VarInfo n = VarInfo
  { varType                :: TermT n
  , varValue               :: Maybe (TermT n)
  , varModality            :: TModality
  , varModAccum            :: TModality
  , varOrig                :: Binder
    -- ^ the names the binder introduced, for display only
  , varIsAssumption        :: Bool -- FIXME: perhaps, introduce something like decl kind?
  , varIsTopLevel          :: Bool
  , varDeclaredAssumptions :: [Foil.Name n]
  , varLocation            :: Maybe LocationInfo
  , varDataRole            :: Maybe (DataRole n)
    -- ^ the role the entry plays for a @#data@ declaration, if any
  , varMetaPrefix          :: Int
    -- ^ the length of the entry's meta-parameter prefix (0 for locals and
    -- for declarations with no meta parameters); see
    -- "Rzk.TypeCheck.MetaPrefix". Recomputed when a section close rewrites
    -- the entry's type.
  }

-- | The role a top-level entry plays for a @#data@ declaration. The ι-rule
-- (in 'Rzk.TypeCheck.Eval') recognises an eliminator head and a
-- constructor-headed scrutinee by these; both entries are otherwise opaque
-- (they carry no value).
--
-- The argument counts speak of the entry's elaborated type: a constructor
-- takes the datatype's parameters and then its own fields; an eliminator
-- takes the parameters, the motive, one method per constructor (in
-- declaration order), and the scrutinee. Closing a section prepends the
-- section's assumptions uniformly to all entries of a declaration, so
-- 'abstractOver' bumps the parameter counts.
data DataRole n = DataRole
  { dataRoleDataType  :: Foil.Name n
    -- ^ the type former this entry belongs to
  , dataRoleNumParams :: Int
    -- ^ the datatype parameters the entry takes before anything else
  , dataRoleKind      :: DataRoleKind
  }

data DataRoleKind
  = DataConKind ConSort Int Int [Int]
    -- ^ a constructor: its sort, its 0-based position among the
    -- constructors (= the method index), the number of its own fields
    -- after the parameters, and the 0-based positions of its recursive
    -- fields (each contributes an induction hypothesis right after the
    -- field in the method)
  | DataElimKind Int Int ElimKind
    -- ^ an eliminator: the number of methods (one per constructor, in
    -- declaration order) and the number of indices of the family; the
    -- spine is parameters, motive, methods, indices, scrutinee

-- | A point constructor inhabits the datatype and the ι-rule dispatches on
-- it as a scrutinee head. A path constructor inhabits an identity type over
-- the datatype, so it can never head a well-typed scrutinee, and the ι-rule
-- must not fire on it: its computation rule is the propositional
-- @compute-@ lemma generated alongside the eliminators.
data ConSort = PointCon | PathCon
  deriving (Eq)

-- | Which eliminator; the ι-rule is the same for both.
data ElimKind = ElimInd | ElimRec
  deriving (Eq)

-- | Add one leading parameter (a section assumption made explicit).
bumpDataRoleParams :: DataRole n -> DataRole n
bumpDataRoleParams role = role { dataRoleNumParams = dataRoleNumParams role + 1 }

-- | A tope, together with the modalities under which it is available.
data ModalTope n = ModalTope
  { tModAccum :: TModality
  , tModVar   :: TModality
  , tTope     :: TermT n
  }

-- | The judgement being performed, for tracing and for the error report.
data Action n
  = ActionTypeCheck (Term n) (TermT n)
  | ActionUnify (TermT n) (TermT n) (TermT n)
  | ActionUnifyTerms (TermT n) (TermT n)
  | ActionInfer (Term n)
  | ActionContextEntailedBy [TermT n] (TermT n)
  | ActionContextEntails [TermT n] (TermT n)
  | ActionContextEntailsUnion [TermT n] [TermT n]
  | ActionWHNF (TermT n)
  | ActionNF (TermT n)
  | ActionCheckCoherence (TermT n, TermT n) (TermT n, TermT n)
  | ActionCloseSection (Maybe Rzk.SectionName)
  | ActionCheckLetValue (Maybe VarIdent)

-- | The state of the tope-saturation cache in a 'Context'
-- (see 'ctxTopesSaturated').
data CachedSaturation n
  = SaturationUncached
    -- ^ No cache was installed for this tope context (entailment falls back to
    -- the per-query pipeline).
  | SaturationCached (Maybe [[ModalTope n]])
    -- ^ A deferred pipeline run: forced by the first query under this context.
    -- 'Nothing' records that the pipeline errored; queries then fall back, so
    -- the error surfaces exactly where it would have.

-- * The context

data Context n = Context
  { ctxScope               :: Scope n
    -- ^ Every name in scope: the top-level entries, the section assumptions and
    -- the binders entered on the way here. Substitution and freshness need it.
  , ctxVars                :: NameMap n (VarInfo n)
    -- ^ What each name in scope stands for. Total on 'ctxScope', which is what
    -- 'Foil.lookupName' assumes.
  , ctxNamed               :: Map VarIdent (Foil.Name n)
    -- ^ The surface name of each top-level entry (and of each named binder), for
    -- resolving an identifier the parser produced.
  , ctxBound               :: [Foil.Name n]
    -- ^ Every name in scope, most recently bound first.
    --
    -- The order has to be recorded, not recovered: free-foil refreshes a binder
    -- only when its name clashes, so after a substitution an inner binder may
    -- well carry a /smaller/ id than an outer one (@\\ x2 -> \\ x1 -> …@), and the
    -- ascending order of 'ctxVars' is not the order things were bound in. Display
    -- depends on this: names claim their display name oldest-first, so that an
    -- inner binder is the one refreshed away from an outer name, and not the
    -- other way round.
  , ctxSections            :: [SectionInfo n]
    -- ^ The open sections, innermost first. Each records the entries declared in
    -- it, so closing it can turn them into declarations.
  , ctxDiscreteTopes       :: [ModalTope n]
    -- ^ Discreteness axioms for the flat cube variables in scope (a flat point of
    -- @2@ or @I@ is an endpoint). Maintained at binder entry, so entailment does
    -- not rescan the context on every query.
  , ctxTopes               :: [ModalTope n]
  , ctxTopesNF             :: [ModalTope n]
  , ctxTopesNFUnion        :: [[ModalTope n]]
  , ctxTopesEntailBottom   :: Maybe Bool
  , ctxTopesSaturated      :: CachedSaturation n
    -- ^ The saturated alternatives for this tope context, cached at the points
    -- where the tope context changes.
  , ctxShadow              :: Map VarIdent [VarIdent]
    -- ^ The identifiers in scope, keyed by their spelling (a 'VarIdent' compares by
    -- spelling and carries the position of its defining occurrence, so the values
    -- are what a shadowing report names).
    --
    -- The check runs at every binder entry, and scanning every name in scope each
    -- time was O(context) per binder — with every top-level definition of a project
    -- in scope, that is most of them.
  , ctxActionStack         :: [Action n]
  , ctxActionStackDepth    :: Int
    -- ^ The length of 'ctxActionStack', maintained alongside it: measuring the
    -- list on every judgement made each action cost O(depth), and each path
    -- O(depth²).
  , ctxCurrentCommand      :: Maybe Rzk.Command
  , ctxLocation            :: Maybe LocationInfo
  , ctxVerbosity           :: Verbosity
  , ctxCovariance          :: Covariance
  , ctxRenderBackend       :: Maybe RenderBackend
  , ctxRenderHideTerm      :: Bool
    -- ^ When rendering a diagram, hide the proof term: drop the @\<title\>@ (which
    -- carries the full term) from every cell and blank the visible label of
    -- proof-coloured (interior) cells, keeping the given boundary labels.
  , ctxHolesAreErrors      :: Bool
    -- ^ When 'True' (the default), an unfilled hole is reported as a
    -- @TypeErrorUnsolvedHole@; finished work (and CI) must have no holes. The
    -- lenient mode ('allowHoles') instead records each hole's goal and context.
  , ctxDeferHoleMismatches :: Bool
    -- ^ How holes behave during unification, giving three modes overall. With
    -- 'ctxHolesAreErrors' a hole is rejected outright (strict). Otherwise a hole
    -- always unifies as a leaf; this flag then chooses what happens when the
    -- /surrounding/ structure disagrees: 'True' (the default) defers — any term
    -- containing a hole is accepted, for an in-progress sketch — while 'False'
    -- keeps such a mismatch an error ('structuralHoleUnify').
  , ctxHintLemmas          :: [VarIdent]
    -- ^ Named top-level definitions a hole's candidate list may draw on, beyond
    -- the local hypotheses (see 'withHintLemmas').
  , ctxWarnOverhang        :: Bool
    -- ^ When 'True', a restriction face or @recOR@ guard that overhangs the
    -- local tope context (is not entailed by it, while still overlapping it)
    -- is reported with a non-fatal hint. Off by default: deciding the
    -- overhang costs a solver entailment per face and guard, and the overhang
    -- is legitimate (see @happy-restrict-face-not-contained@). Enabled with
    -- @#set-option "warn-overhang" "yes"@. The /disjointness/ error next to
    -- it is unaffected: a vacuous face is always rejected.
  , ctxMetaPrefixSensitivity :: MetaPrefixSensitivity
    -- ^ How sensitively the meta-parameter layer check classifies use
    -- positions (see "Rzk.TypeCheck.MetaPrefix"). Strict by default; set
    -- with @#set-option "warn-meta-prefix" = "off" | "structural" | "strict"@.
  }

-- | The sensitivity levels of the meta-parameter layer check.
data MetaPrefixSensitivity
  = MetaPrefixOff
    -- ^ no meta-prefix warnings
  | MetaPrefixStructural
    -- ^ warn only at structurally object-level positions
  | MetaPrefixStrict
    -- ^ additionally require an unsaturated schema argument to sit within
    -- a top-level receiver's meta prefix (the default)
  deriving (Eq, Show)

-- | An open section: the entries declared in it, newest first.
data SectionInfo n = SectionInfo
  { sectionName    :: Maybe Rzk.SectionName
  , sectionEntries :: [Foil.Name n]
  }

emptyContext :: Context Foil.VoidS
emptyContext = Context
  { ctxScope = Foil.emptyScope
  , ctxVars = Foil.emptyNameMap
  , ctxNamed = Map.empty
  , ctxBound = []
  , ctxSections = [SectionInfo Nothing []]
  , ctxDiscreteTopes = []
  , ctxTopes = emptyTopeContext
  , ctxTopesNF = emptyTopeContext
  , ctxTopesNFUnion = [emptyTopeContext]
  , ctxTopesEntailBottom = Just False
  , ctxTopesSaturated = SaturationUncached
  , ctxShadow = Map.empty
  , ctxActionStack = []
  , ctxActionStackDepth = 0
  , ctxCurrentCommand = Nothing
  , ctxLocation = Nothing
  , ctxVerbosity = Normal
  , ctxCovariance = Covariant
  , ctxRenderBackend = Nothing
  , ctxRenderHideTerm = False
  , ctxHolesAreErrors = True
  , ctxDeferHoleMismatches = True
  , ctxHintLemmas = []
  , ctxWarnOverhang = False
  , ctxMetaPrefixSensitivity = MetaPrefixStrict
  }

-- | The tope context of an empty context: @⊤@ holds under every modality.
emptyTopeContext :: [ModalTope n]
emptyTopeContext =
  [ ModalTope Id Id    topeTopT
  , ModalTope Id Flat  topeTopT
  , ModalTope Id Op    topeTopT
  , ModalTope Id Sharp topeTopT
  ]

-- * Entering a binder
--
-- $sinking
--
-- Everything in a context that mentions the scope is a term, or a container of
-- terms, and so is sinkable: a term whose free names lie in @n@ has its free
-- names in any @l@ that extends @n@. free-foil sinks such a value by coercion
-- (@sink = unsafeCoerce@, sound because @DExt n l@ makes the renaming the
-- identity on raw names; see §3.5 of the Foil paper), and the same argument
-- applies to a whole record of them: renaming every field along the identity is
-- the identity on the representation.
--
-- Two fields are /not/ sinkable, and 'enterBinder' restores both in the same
-- breath, which is why the coercion below is not exported:
--
--   * 'ctxScope' is the set of names /in/ @n@. It has to grow, not coerce —
--     tellingly, free-foil gives 'Foil.Scope' no 'Foil.Sinkable' instance.
--   * 'ctxVars' must stay total on the names in scope ('Foil.lookupName' assumes
--     it), so the new binder's entry has to be added.

-- | Sink a context along a scope extension. Unsound on its own: leaves 'ctxScope'
-- and 'ctxVars' describing the /old/ scope, and the caller must fix both. See the
-- note above; 'enterBinder' is the only caller.
sinkContextUnchecked :: DExt n l => Context n -> Context l
sinkContextUnchecked = unsafeCoerce

-- | Enter the scope of a binder: extend the scope with it, record what it is
-- called and what it stands for, and carry the rest of the context in.
enterBinder
  :: DExt n l
  => NameBinder n l
  -> VarInfo n       -- ^ its type, value and modality
  -> [ModalTope l]   -- ^ discreteness axioms the binder brings (a flat cube point,
                     --   which are about the variable itself, hence at its scope)
  -> Context n
  -> Context l
enterBinder binder info discrete ctx = (sinkContextUnchecked ctx)
  { ctxScope = Foil.extendScope binder (ctxScope ctx)
  , ctxVars = Foil.addNameBinder binder (Foil.sink info) (sinkVars (ctxVars ctx))
  , ctxBound = Foil.nameOf binder : sinkNames (ctxBound ctx)
  , ctxNamed = case binderName (varOrig info) of
      Nothing   -> sinkNamed (ctxNamed ctx)
      Just name -> Map.insert name (Foil.nameOf binder) (sinkNamed (ctxNamed ctx))
  , ctxDiscreteTopes = discrete <> sinkTopes (ctxDiscreteTopes ctx)
  , ctxShadow = addBinderNames (varOrig info) (ctxShadow ctx)
  }

-- | Enter a /fresh/ binder: one the checker invents rather than one a term
-- carries (to look under a Π when comparing two of them, say).
withFreshBinder
  :: Distinct n
  => Context n
  -> VarInfo n
  -> (forall l. DExt n l => NameBinder n l -> Context l -> r)
  -> r
withFreshBinder ctx info k =
  Foil.withFresh (ctxScope ctx) $ \binder ->
    k binder (enterBinder binder info [] ctx)

-- | Record the name a binder introduces (if any), for the shadowing check.
addBinderNames :: Binder -> Map VarIdent [VarIdent] -> Map VarIdent [VarIdent]
addBinderNames orig names =
  case binderName orig of
    Nothing   -> names
    Just name -> Map.insertWith (<>) name [name] names

-- | The identifiers in scope spelled like this one.
shadowedBy :: VarIdent -> Context n -> [VarIdent]
shadowedBy name ctx = Map.findWithDefault [] name (ctxShadow ctx)

-- * Sinking the parts

instance Foil.Sinkable VarInfo where
  sinkabilityProof rename info = info
    { varType = Foil.sinkabilityProof rename (varType info)
    , varValue = Foil.sinkabilityProof rename <$> varValue info
    , varDeclaredAssumptions = rename <$> varDeclaredAssumptions info
    , varDataRole = renameRole <$> varDataRole info
    }
    where
      renameRole role = role { dataRoleDataType = rename (dataRoleDataType role) }

instance Foil.Sinkable ModalTope where
  sinkabilityProof rename tope =
    tope { tTope = Foil.sinkabilityProof rename (tTope tope) }

sinkVars :: DExt n l => NameMap n (VarInfo n) -> NameMap n (VarInfo l)
sinkVars = Foil.sinkContainer

sinkTopes :: DExt n l => [ModalTope n] -> [ModalTope l]
sinkTopes = Foil.sinkContainer

sinkNamed :: DExt n l => Map VarIdent (Foil.Name n) -> Map VarIdent (Foil.Name l)
sinkNamed = Foil.sinkContainer

sinkNames :: DExt n l => [Foil.Name n] -> [Foil.Name l]
sinkNames = Foil.sinkContainer

-- * Lookup

-- | What a name stands for. Total: every name in scope has an entry.
lookupVarInfo :: Foil.Name n -> Context n -> VarInfo n
lookupVarInfo name ctx = Foil.lookupName name (ctxVars ctx)

-- | What every name in scope stands for, in no particular order.
varInfos :: Context n -> [VarInfo n]
varInfos ctx = IntMap.elems m
  where
    NameMap m = ctxVars ctx

-- | Every hypothesis in scope, oldest binding first (see 'ctxBound': the ids
-- themselves do not tell us this).
varsInScope :: Context n -> [(Foil.Name n, VarInfo n)]
varsInScope ctx =
  [ (name, lookupVarInfo name ctx) | name <- reverse (ctxBound ctx) ]

-- | The name a surface identifier resolves to, if any.
lookupNamed :: VarIdent -> Context n -> Maybe (Foil.Name n)
lookupNamed name ctx = Map.lookup name (ctxNamed ctx)

-- | The display binder of a name (the whole pattern, for projection folding).
binderOfName :: Foil.Name n -> Context n -> Binder
binderOfName name = varOrig . lookupVarInfo name

-- * Modalities

class ModeTheory m where
    iden :: m
    comp :: m -> m -> m
    coe :: m -> m -> Bool
    isRA :: m -> Bool

instance ModeTheory TModality where
  iden = Id

  comp Flat Flat   = Flat
  comp Flat Sharp  = Flat
  comp Flat Op     = Flat
  comp Op Flat     = Flat
  comp Sharp Sharp = Sharp
  comp Sharp Flat  = Sharp
  comp Sharp Op    = Sharp
  comp Op Sharp    = Sharp
  comp Op Op       = Id
  comp Id m        = m
  comp m Id        = m

  coe Flat Id    = True
  coe Flat Op    = True
  coe Id Sharp   = True
  coe Flat Sharp = True
  coe Op Sharp   = True
  coe a b        = a == b

  isRA Sharp = True
  isRA Op    = True
  isRA Id    = True
  isRA _     = False

-- | Accumulate a modality (a lock) over the hypotheses and the topes.
--
-- Note that top-level entries are /not/ touched: a top-level variable is exempt
-- from the accessibility check (see @infer@), so accumulating a modality over
-- them was work with no observable effect. The old context did it to all ~1500
-- of them on every modal binder.
applyModality :: TModality -> Context n -> Context n
applyModality md ctx = ctx
  { ctxVars = mapNameMap addToVar (ctxVars ctx)
  , ctxTopes = map addToTope (ctxTopes ctx)
  , ctxTopesNF = map addToTope (ctxTopesNF ctx)
  , ctxTopesNFUnion = map (map addToTope) (ctxTopesNFUnion ctx)
  , ctxTopesSaturated = SaturationUncached  -- accessibility changed
  }
  where
    addToVar info
      | varIsTopLevel info = info
      | otherwise = info { varModAccum = comp (varModAccum info) md }
    addToTope tope = tope { tModAccum = comp (tModAccum tope) md }

-- | Map the values of a name map, keeping its keys.
mapNameMap :: (a -> b) -> NameMap n a -> NameMap n b
mapNameMap f (NameMap m) = NameMap (IntMap.map f m)

-- | Replace what a name stands for.
--
-- Closing a section rewrites the definitions made in it, so that each takes the
-- section's assumptions as explicit parameters; this is how the rewritten entries
-- go back into the context.
insertVarInfo :: Foil.Name n -> VarInfo n -> Context n -> Context n
insertVarInfo name info ctx = ctx { ctxVars = replace (ctxVars ctx) }
  where
    replace (NameMap m) = NameMap (IntMap.insert (Foil.nameId name) info m)

-- * Topes

isAccessible :: ModalTope n -> Bool
isAccessible mt = coe (tModVar mt) (tModAccum mt)

filterAccessible :: [ModalTope n] -> [ModalTope n]
filterAccessible = filter isAccessible

accessibleTopes :: [ModalTope n] -> [TermT n]
accessibleTopes = map tTope . filterAccessible

plainTope :: TermT n -> ModalTope n
plainTope = ModalTope Id Id

availableTopes :: Context n -> [TermT n]
availableTopes = accessibleTopes . ctxTopes

availableTopesNF :: Context n -> [TermT n]
availableTopesNF = accessibleTopes . ctxTopesNF

-- * Hole modes

-- | Switch to lenient hole mode: record each hole's goal and context instead of
-- reporting it as an error.
allowHoles :: Context n -> Context n
allowHoles ctx = ctx { ctxHolesAreErrors = False }

-- | Allow a hole's candidate list to draw on the given named top-level
-- definitions, in addition to the local hypotheses.
withHintLemmas :: [VarIdent] -> Context n -> Context n
withHintLemmas lemmas ctx = ctx { ctxHintLemmas = lemmas }

-- | Within the given action, a hole unifies only as a leaf of an otherwise
-- matching structure: a structural mismatch around a hole stays an error rather
-- than being deferred (see 'ctxDeferHoleMismatches').
structuralHoleUnify :: Context n -> Context n
structuralHoleUnify ctx = ctx { ctxDeferHoleMismatches = False }
