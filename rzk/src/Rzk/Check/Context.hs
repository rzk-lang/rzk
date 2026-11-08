-- The scope-extension evidence on 'sinkContext' (a coercion) is its
-- soundness contract, not an argument it can consume, so GHC calls it
-- redundant. It stays.
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | The typing context of the new checker: what every name in scope is, the
-- tope context, the direction of a comparison, the options, and whatever
-- the run keeps for diagnostics, which is the parameter @d@.
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
  , NoDiag (..)
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

import           Control.Monad.Foil          (DExt, Distinct, NameMap, Scope, S)
import qualified Control.Monad.Foil          as Foil
import           Control.Monad.Foil.Internal (NameMap (..))
import qualified Data.IntMap                 as IntMap
import           Data.Kind                   (Type)
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Unsafe.Coerce               (unsafeCoerce)

import qualified FreeFoil.NbE                as NbE
import           FreeFoil.NbE.Definitions    (Unfolding (..))

import           Language.Rzk.Foil.Names     (RzkPosition (..), TModality (..),
                                              VarIdent)
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

-- | How a hole is treated: as an error (finished work, and CI), as a term of
-- any type whose goal is recorded (a sketch), or recorded but matching
-- only as a leaf of an otherwise matching structure (a probe).
data HoleMode = HolesAreErrors | HolesDeferred | HolesStructural
  deriving (Eq, Show)

-- | The options a run or a file sets.
data Options = Options
  { optVerbosity           :: Verbosity
  , optHoles               :: HoleMode
  , optHintLemmas          :: [VarIdent]
  , optWarnOverhang        :: Bool
  , optWarnTopeFamilyDomain :: Bool
  , optRender              :: Maybe String
    -- ^ the render backend, which the new checker accepts but does not run
  } deriving (Eq, Show)

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
data Approximation = ApproxRigid | ApproxFlex | ApproxFull
  deriving (Eq, Show)

-- * Entries

data EntryKind = EntryLocal | EntryAssumption | EntryTopLevel
  deriving (Eq, Show)

-- | What is known about a name in scope.
data Entry n = Entry
  { entryType     :: Value n
  , entryValue    :: Maybe (Value n)
    -- ^ the value of a definition, evaluated once; a glued value is made
    -- of it at every lookup
  , entryModality :: TModality
  , entryLocks    :: TModality
  , entryKind     :: EntryKind
  , entryName     :: Maybe VarIdent
    -- ^ the name the user wrote, for display
  , entryLocation :: Maybe LocationInfo
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
  | ActionInfer (Raw n)
  | ActionConvert (Value n) (Value n)
  | ActionEntails [Tope] Tope
  | ActionEntailsUnion [Tope] [Tope]
  | ActionCheckLetValue (Maybe VarIdent)
  | ActionCloseSection (Maybe String)

-- | No diagnostics: what a status-only run keeps.
data NoDiag (n :: S) = NoDiag

-- | What a full run keeps for its diagnostics: the stack of judgements.
data Diag n = Diag
  { diagActions :: [Action n]
  , diagDepth   :: !Int
  }

emptyDiag :: Diag n
emptyDiag = Diag [] 0

-- * The context

data Context (d :: S -> Type) n = Context
  { ctxScope         :: Scope n
  , ctxEntries       :: NameMap n (Entry n)
    -- ^ total on 'ctxScope'
  , ctxNamed         :: Map VarIdent (Foil.Name n)
    -- ^ the surface name of each entry that has one
  , ctxBound         :: [Foil.Name n]
    -- ^ every name in scope, most recently bound first
  , ctxTopes         :: TopeContext
  , ctxVariance      :: Variance
  , ctxUnfolding     :: Unfolding
    -- ^ the side of a defined name that readback follows
  , ctxApproximation :: Approximation
  , ctxOptions       :: Options
  , ctxLocation      :: Maybe LocationInfo
  , ctxSections      :: [Section n]
    -- ^ the open sections, innermost first
  , ctxDiag          :: d n
  }

emptyContext :: d Foil.VoidS -> Context d Foil.VoidS
emptyContext diag = Context
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
  , ctxDiag = diag
  }

-- | What a name stands for. Total: every name in scope has an entry.
lookupEntry :: Foil.Name n -> Context d n -> Entry n
lookupEntry name ctx = Foil.lookupName name (ctxEntries ctx)

lookupNamed :: VarIdent -> Context d n -> Maybe (Foil.Name n)
lookupNamed name ctx = Map.lookup name (ctxNamed ctx)

-- | Every entry in scope, oldest binding first.
entriesInScope :: Context d n -> [(Foil.Name n, Entry n)]
entriesInScope ctx = [ (name, lookupEntry name ctx) | name <- reverse (ctxBound ctx) ]

-- | Sink a context along a scope extension. Unsound on its own: the scope
-- and the entries still describe the old scope, and 'enterPattern' fixes
-- both. Everything else in the context mentions the scope only through
-- values and names, which sink by coercion.
sinkContextUnchecked :: DExt n l => Context d n -> Context d l
sinkContextUnchecked = unsafeCoerce

-- | Enter the scope of a pattern: extend the scope with its names and
-- record an entry for each, in binding order.
enterPattern
  :: (Distinct n, DExt n l)
  => Pattern n l -> [Entry l] -> Context d n -> Context d l
enterPattern pat entries ctx = (sinkContextUnchecked ctx)
  { ctxScope = Foil.extendScopePattern pat (ctxScope ctx)
  , ctxEntries = NameMap (foldr insert old (zip names entries))
  , ctxBound = reverse (map snd names) <> sinkNames (ctxBound ctx)
  , ctxNamed = foldr named (sinkNamed (ctxNamed ctx)) names
  }
  where
    names = patternNames pat
    NameMap old = sinkEntries (ctxEntries ctx)
    insert ((_, name), entry) = IntMap.insert (Foil.nameId name) entry
    named (Just x, name) = Map.insert x name
    named (Nothing, _)   = id

sinkNames :: DExt n l => [Foil.Name n] -> [Foil.Name l]
sinkNames = Foil.sink1

sinkNamed :: DExt n l => Map VarIdent (Foil.Name n) -> Map VarIdent (Foil.Name l)
sinkNamed = Foil.sink1

-- | Replace what a name stands for.
insertEntry :: Foil.Name n -> Entry n -> Context d n -> Context d n
insertEntry name entry ctx = ctx { ctxEntries = NameMap (IntMap.insert (Foil.nameId name) entry m) }
  where NameMap m = ctxEntries ctx

withVariance :: Variance -> Context d n -> Context d n
withVariance v ctx = ctx { ctxVariance = v }
