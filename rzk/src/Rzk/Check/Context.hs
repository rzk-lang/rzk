-- The scope-extension constraint of 'sinkContextUnchecked' (a coercion) is
-- a contract for its callers, not an argument it uses, so GHC calls it
-- redundant. It is kept as the contract.
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | The typing context of the new checker: what every name in scope is, the
-- tope context, the direction of a comparison, the options, and the stack of
-- judgements kept for diagnostics.
--
-- Every entry holds values: the type of the name, and for a definition its
-- value, evaluated once. The context is also the evaluation context of
-- free-foil-NBE (the instance is in "Rzk.Check.Value"), so evaluation reads
-- definitions and topes from the same place the checker does.
module Rzk.Check.Context
  ( -- * Values
    Value
    -- * Pieces
  , Variance (..)
  , opposite
  , Verbosity (..)
  , LocationInfo (..)
  , atPosition
  , HoleMode (..)
  , Options (..)
  , defaultOptions
  , Approximation (..)
  , Unfolding (..)
    -- * Entries
  , EntryKind (..)
  , Entry (..)
  , localEntry
  , Section (..)
    -- * Diagnostics
  , Action (..)
  , Diag (..)
  , emptyDiag
    -- * The context
  , Context (..)
  , emptyContext
  , lookupEntry
  , lookupNamed
  , entriesInScope
  , enterPattern
  , sinkContextUnchecked
  , insertEntry
  , withVariance
  ) where

import           Control.Monad.Foil          (DExt, Distinct, NameMap, Scope)
import qualified Control.Monad.Foil          as Foil
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Unsafe.Coerce               (unsafeCoerce)

import qualified FreeFoil.NbE                as NbE
import           FreeFoil.NbE.Definitions    (Unfolding (..))

import           Language.Rzk.Foil.Names     (RzkPosition (..), TModality (..),
                                              VarIdent)
import           Rzk.Check.Foil.Extras       (replaceName)
import           Rzk.Check.Syntax
import           Rzk.Check.Topes             (Tope, TopeContext,
                                              emptyTopeContext)

-- * Values

-- | A value of the core: free-foil-NBE's, over the pattern binder and the
-- signature.
type Value = NbE.Value Pattern Sig

-- * Pieces

-- | The direction of a comparison: whether the expected side is the
-- supertype, the subtype, or neither.
data Variance = Covariant | Contravariant | Invariant
  deriving (Eq, Show)

opposite :: Variance -> Variance
opposite = \case
  Covariant     -> Contravariant
  Contravariant -> Covariant
  Invariant     -> Invariant

data Verbosity = Debug | Normal | Silent
  deriving (Eq, Ord, Show)

-- | Where a diagnostic points: the start of the declaration being checked,
-- narrowed to the sub-term as the checker descends into it.
data LocationInfo = LocationInfo
  { locationFilePath :: Maybe FilePath
  , locationLine     :: Maybe Int
  , locationColumn   :: Maybe Int
  } deriving (Eq, Show)

-- | Point a location at a position in the same file.
atPosition :: RzkPosition -> LocationInfo -> LocationInfo
atPosition pos loc = case rzkLineCol pos of
  Nothing          -> loc
  Just (line, col) -> loc { locationLine = Just line, locationColumn = Just col }

-- | How a user hole is treated.
data HoleMode
  = HolesAreErrors
    -- ^ reject an unsolved hole; the default for finished work and CI
  | HolesDeferred
    -- ^ record the goal and let the hole stand for a term of any type
  | HolesStructural
    -- ^ record the goal, but tolerate a mismatch only at a hole leaf of an
    -- otherwise matching structure
  deriving (Eq, Show)

-- | The options a run or a file sets.
data Options = Options
  { optVerbosity           :: Verbosity
    -- ^ how much progress and debugging output to print
  , optHoles               :: HoleMode
    -- ^ how to treat user holes
  , optHintLemmas          :: [VarIdent]
    -- ^ lemmas requested as hints; the new checker does not use them yet
  , optWarnOverhang        :: Bool
    -- ^ whether to warn when a face or guard overhangs the local context
  , optWarnTopeFamilyDomain :: Bool
    -- ^ whether to warn when a tope family needs its domain conjoined
  , optRender              :: Maybe String
    -- ^ the render backend, which the new checker accepts but does not run
  } deriving (Eq, Show)

-- | The default options.
--
-- >>> defaultOptions
-- Options {optVerbosity = Normal, optHoles = HolesAreErrors, optHintLemmas = [], optWarnOverhang = False, optWarnTopeFamilyDomain = True, optRender = Nothing}
defaultOptions :: Options
defaultOptions = Options
  { optVerbosity = Normal
  , optHoles = HolesAreErrors
  , optHintLemmas = []
  , optWarnOverhang = False
  , optWarnTopeFamilyDomain = True
  , optRender = Nothing
  }

-- | The state of a conversion check, after the approximate conversion
-- checking of Kovács's smalltt. A check starts 'ApproxRigid'. Two glued
-- values of one defined name compare their folded sides 'ApproxFlex', with
-- no unfolding, and if these differ their unfolded sides 'ApproxFull'.
data Approximation
  = ApproxRigid
    -- ^ unfold distinct definitions; equal ones may first compare folded
  | ApproxFlex
    -- ^ compare folded sides of one definition, without unfolding
  | ApproxFull
    -- ^ allow a full unfolded comparison after the folded attempt
  deriving (Eq, Show)

-- * Entries

data EntryKind
  = EntryLocal
    -- ^ a local binder, not collected when a section closes
  | EntryAssumption
    -- ^ a section assumption, abstracted away when the section closes
  | EntryTopLevel
    -- ^ a definition or postulate that survives the section
  deriving (Eq, Show)

-- | What is known about a name in scope.
data Entry n = Entry
  { entryType     :: Value n
    -- ^ the type of the name
  , entryValue    :: Maybe (Value n)
    -- ^ the value of a definition, evaluated once; a glued value is made
    -- of it at every lookup
  , entryModality :: TModality
    -- ^ the modality at which the name was bound
  , entryLocks    :: TModality
    -- ^ accumulated locks; reserved for modal term checking
  , entryKind     :: EntryKind
    -- ^ whether the name is local, an assumption, or a top-level entry
  , entryName     :: Maybe VarIdent
    -- ^ the name the user wrote, for display
  , entryLocation :: Maybe LocationInfo
    -- ^ where the entry was introduced
  , entryUses     :: [Foil.Name n]
    -- ^ the variables a declaration says it uses
  , entryTypeCore  :: Maybe (Core n)
    -- ^ a top-level entry's type as elaborated, which closing a section
    -- reads its dependencies off and abstracts
  , entryValueCore :: Maybe (Core n)
    -- ^ a definition's body as elaborated
  }

instance Foil.Sinkable Entry where
  sinkabilityProof rename entry = entry
    { entryType = Foil.sinkabilityProof rename (entryType entry)
    , entryValue = Foil.sinkabilityProof rename <$> entryValue entry
    , entryUses = map rename (entryUses entry)
    , entryTypeCore = Foil.sinkabilityProof rename <$> entryTypeCore entry
    , entryValueCore = Foil.sinkabilityProof rename <$> entryValueCore entry
    }

-- | The entry of a local name of a type.
localEntry :: TModality -> Maybe VarIdent -> Maybe LocationInfo -> Value n -> Entry n
localEntry md mname loc ty = Entry
  { entryType = ty
  , entryValue = Nothing
  , entryModality = md
  , entryLocks = Id
  , entryKind = EntryLocal
  , entryName = mname
  , entryLocation = loc
  , entryUses = []
  , entryTypeCore = Nothing
  , entryValueCore = Nothing
  }

-- | An open section: its name, and the top-level entries made in it,
-- newest first. The module itself is an anonymous section, so that an
-- assumption outside any named section is abstracted at the end of the
-- module.
data Section n = Section
  { sectionName    :: Maybe String
  , sectionEntries :: [Foil.Name n]
  }

sinkEntries :: DExt n l => NameMap n (Entry n) -> NameMap n (Entry l)
sinkEntries = Foil.sink1

-- * Diagnostics

-- | The judgement being performed, for the trace and the error report.
data Action n
  = ActionCheck (Raw n) (Value n)
    -- ^ check a raw term against a type value
  | ActionInfer (Raw n)
    -- ^ infer the type of a raw term
  | ActionConvert (Value n) (Value n)
    -- ^ compare an expected value with an actual one
  | ActionEntails [Tope n] (Tope n)
    -- ^ check that the listed context topes entail a goal
  | ActionEntailsUnion [Tope n] [Tope n]
    -- ^ check that the listed context topes are covered by a union
  | ActionCheckLetValue (Maybe VarIdent)
    -- ^ check or infer a local definition's value
  | ActionCloseSection (Maybe String)
    -- ^ close a named section, or the anonymous module section

-- | What a full run keeps for its diagnostics: the stack of judgements.
data Diag n = Diag
  { diagActions :: [Action n]
    -- ^ the current judgements, innermost first
  , diagDepth   :: !Int
    -- ^ the number of nested judgements, used by the termination cap
  }

emptyDiag :: Diag n
emptyDiag = Diag [] 0

-- * The context

data Context n = Context
  { ctxScope         :: Scope n
    -- ^ the names in scope
  , ctxEntries       :: NameMap n (Entry n)
    -- ^ total on 'ctxScope'
  , ctxNamed         :: Map VarIdent (Foil.Name n)
    -- ^ the surface name of each entry that has one
  , ctxBound         :: [Foil.Name n]
    -- ^ every name in scope, most recently bound first
  , ctxTopes         :: TopeContext n
    -- ^ the assumed topes and their cached saturation
  , ctxVariance      :: Variance
    -- ^ the direction of the current comparison
  , ctxUnfolding     :: Unfolding
    -- ^ the side of a defined name that readback follows
  , ctxApproximation :: Approximation
    -- ^ the current glued-comparison strategy
  , ctxOptions       :: Options
    -- ^ options inherited from the run and source commands
  , ctxLocation      :: Maybe LocationInfo
    -- ^ the location narrowed to the current command and judgement
  , ctxSections      :: [Section n]
    -- ^ the open sections, innermost first
  , ctxDiag          :: Diag n
    -- ^ the stack of current judgements and its depth
  }

-- | The empty context, with 'defaultOptions'. Conversion starts covariantly,
-- with definitions unfolded and the rigid approximation strategy.
--
-- >>> ctxVariance emptyContext
-- Covariant
-- >>> case ctxUnfolding emptyContext of { Unfolded -> True; Folded -> False }
-- True
-- >>> ctxApproximation emptyContext
-- ApproxRigid
emptyContext :: Context Foil.VoidS
emptyContext = Context
  { ctxScope = Foil.emptyScope
  , ctxEntries = Foil.emptyNameMap
  , ctxNamed = Map.empty
  , ctxBound = []
  , ctxTopes = emptyTopeContext
  , ctxVariance = Covariant
  , ctxUnfolding = Unfolded
  , ctxApproximation = ApproxRigid
  , ctxOptions = defaultOptions
  , ctxLocation = Nothing
  , ctxSections = []
  , ctxDiag = emptyDiag
  }

-- | What a name stands for. Total: every name in scope has an entry.
lookupEntry :: Foil.Name n -> Context n -> Entry n
lookupEntry name ctx = Foil.lookupName name (ctxEntries ctx)

lookupNamed :: VarIdent -> Context n -> Maybe (Foil.Name n)
lookupNamed name ctx = Map.lookup name (ctxNamed ctx)

-- | Every entry in scope, oldest binding first.
entriesInScope :: Context n -> [(Foil.Name n, Entry n)]
entriesInScope ctx = [ (name, lookupEntry name ctx) | name <- reverse (ctxBound ctx) ]

-- | Sink a context along a scope extension. Unsound on its own: the scope
-- and the entries still describe the old scope, so the caller must extend
-- both, as 'enterPattern' does. Everything else in the context mentions
-- the scope only through values and names, which sink by coercion.
sinkContextUnchecked :: DExt n l => Context n -> Context l
sinkContextUnchecked = unsafeCoerce

-- | Enter the scope of a pattern: extend the scope with its names and
-- record an entry for each, in binding order.
enterPattern
  :: (Distinct n, DExt n l)
  => Pattern n l -> [Entry l] -> Context n -> Context l
enterPattern pat entries ctx = (sinkContextUnchecked ctx)
  { ctxScope = Foil.extendScopePattern pat (ctxScope ctx)
  , ctxEntries = Foil.addNameBinders pat entries (sinkEntries (ctxEntries ctx))
  , ctxBound = reverse (map snd names) <> sinkNames (ctxBound ctx)
  , ctxNamed = foldr named (sinkNamed (ctxNamed ctx)) names
  }
  where
    names = patternNames pat
    named (Just x, name) = Map.insert x name
    named (Nothing, _)   = id

sinkNames :: DExt n l => [Foil.Name n] -> [Foil.Name l]
sinkNames = Foil.sink1

sinkNamed :: DExt n l => Map VarIdent (Foil.Name n) -> Map VarIdent (Foil.Name l)
sinkNamed = Foil.sink1

-- | Replace what a name stands for.
insertEntry :: Foil.Name n -> Entry n -> Context n -> Context n
insertEntry name entry ctx = ctx { ctxEntries = replaceName name entry (ctxEntries ctx) }

withVariance :: Variance -> Context n -> Context n
withVariance v ctx = ctx { ctxVariance = v }
