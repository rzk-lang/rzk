{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The typing context on free-foil (roadmap M2, stage 2).
--
-- The successor of @Rzk.TypeCheck@'s @Context var@. Three things change, and
-- they are the point of the migration.
--
-- [Locals are a name map.] A variable is a 'Foil.Name' (an @Int@), so the local
-- hypotheses are an 'Foil.NameMap', looked up directly. The old context kept an
-- association list keyed by a @var@ whose equality walked an @S@-chain, and
-- @lookupVarInfo@ was 11.5% of the checker's time.
--
-- [Globals are closed, so they never move.] A top-level definition is closed: it
-- mentions no local, so its elaborated term is a @forall n. TermT n@. It is kept
-- in a plain map keyed by its name and 'Foil.sink'ed (a coercion) on lookup. The
-- old representation had to keep the whole global scope inside the @var@-indexed
-- context, and PR #277 (@GlobalScopeInfo@ \/ @globalEmbed@) exists only to stop
-- the per-binder shift from rebuilding all ~1500 of them. That machinery is gone.
--
-- [Entering a binder does not rebuild terms.] 'enterScope' extends the scope with
-- a fresh name and carries the locals and the tope context in with 'Foil.sink',
-- which is a coercion. The old @enterScopeContext@ mapped @S \<$\>@ over the
-- entire context, rebuilding every elaborated term it held; the heap profile
-- showed those forced copies retaining most of the live heap.
module Language.Rzk.Foil.Context where

import           Control.Monad.Foil       (DExt, Distinct, NameBinder, NameMap,
                                           Scope)
import qualified Control.Monad.Foil       as Foil
-- NOTE: free-foil 0.2.0 gives NameMap no Functor instance (it has one on the
-- unreleased main), so the values are mapped through the underlying IntMap.
-- See the handoff: this is the second item for a free-foil 0.2.1 release.
import           Control.Monad.Foil.Internal (NameMap (..))
import           Data.Map                 (Map)
import qualified Data.IntMap              as IntMap
import qualified Data.Map                 as Map

import           Language.Rzk.Foil.Syntax
import           Language.Rzk.Free.Syntax (Binder (..), TModality (..), VarIdent)

-- | What is known about a local hypothesis.
data VarInfo n = VarInfo
  { varType     :: TermT n
  , varValue    :: Maybe (TermT n)
  , varModality :: TModality
  , varModAccum :: TModality
  , varOrig     :: Binder
    -- ^ the names the binder introduced, for display only
  }

-- | A tope, together with the modalities under which it is available.
data ModalTope n = ModalTope
  { topeModAccum :: TModality
  , topeModVar   :: TModality
  , topeOf       :: TermT n
  }

-- | A top-level definition. It is closed, so it is valid in /every/ scope, which
-- is why it never has to be moved.
data GlobalInfo = GlobalInfo
  { globalType         :: forall n. TermT n
  , globalValue        :: forall n. Maybe (TermT n)
  , globalIsAssumption :: Bool
  }

data Context n = Context
  { ctxScope   :: Scope n
    -- ^ the names in scope, which free-foil needs for substitution and freshness
  , ctxLocals  :: NameMap n (VarInfo n)
  , ctxGlobals :: Map VarIdent GlobalInfo
    -- ^ closed, and therefore never sunk: 'lookupGlobal' coerces on the way out
  , ctxTopes   :: [ModalTope n]
  , ctxNames   :: NameMap n Binder
    -- ^ what each bound name is called, for goals and error messages
  }

emptyContext :: Context Foil.VoidS
emptyContext = Context
  { ctxScope = Foil.emptyScope
  , ctxLocals = Foil.emptyNameMap
  , ctxGlobals = Map.empty
  , ctxTopes = []
  , ctxNames = Foil.emptyNameMap
  }

-- * Sinking
--
-- Everything scope-indexed in the context sinks, and every 'Foil.sink' is a
-- coercion. What used to be a deep rebuild of the context is now a walk over the
-- name map's spine.

sinkVarInfo :: DExt n l => VarInfo n -> VarInfo l
sinkVarInfo info = info
  { varType = Foil.sink (varType info)
  , varValue = fmap Foil.sink (varValue info)
  }

sinkModalTope :: DExt n l => ModalTope n -> ModalTope l
sinkModalTope tope = tope { topeOf = Foil.sink (topeOf tope) }

-- | Map the values of a name map, keeping its keys. Each application is a
-- coercion, so this walks the map's spine and no terms are rebuilt.
mapNameMap :: (a -> b) -> NameMap n a -> NameMap n' b
mapNameMap f (NameMap m) = NameMap (IntMap.map f m)

-- * Entering a binder

-- | Enter the scope of a binder that is already part of a term (a λ's binder,
-- say): extend the scope with it, record what it is called and what it stands
-- for, and carry the rest of the context in.
enterBinder
  :: DExt n l
  => NameBinder n l
  -> Binder          -- ^ the names it introduces (for display)
  -> VarInfo n       -- ^ its type, value and modality
  -> Context n
  -> Context l
enterBinder binder orig info ctx = Context
  { ctxScope = Foil.extendScope binder (ctxScope ctx)
  , ctxLocals = Foil.addNameBinder binder (sinkVarInfo info) (mapNameMap sinkVarInfo (ctxLocals ctx))
  , ctxGlobals = ctxGlobals ctx      -- closed: nothing to do
  , ctxTopes = map sinkModalTope (ctxTopes ctx)
  , ctxNames = Foil.addNameBinder binder orig (ctxNames ctx)
  }

-- | Enter a /fresh/ binder (one the checker invents, e.g. to look under a Π when
-- comparing two of them).
withFreshBinder
  :: Distinct n
  => Context n
  -> Binder
  -> VarInfo n
  -> (forall l. DExt n l => NameBinder n l -> Context l -> r)
  -> r
withFreshBinder ctx orig info k =
  Foil.withFresh (ctxScope ctx) $ \binder ->
    k binder (enterBinder binder orig info ctx)

-- * Lookup

-- | The local hypothesis a name stands for.
lookupLocal :: Foil.Name n -> Context n -> VarInfo n
lookupLocal name ctx = Foil.lookupName name (ctxLocals ctx)

-- | A top-level definition, in the current scope. The definition is closed, so
-- this is a coercion rather than a traversal.
lookupGlobal :: VarIdent -> Context n -> Maybe (VarInfo n)
lookupGlobal name ctx = toVarInfo <$> Map.lookup name (ctxGlobals ctx)
  where
    toVarInfo g = VarInfo
      { varType = globalType g
      , varValue = globalValue g
      , varModality = Id
      , varModAccum = Id
      , varOrig = BinderVar (Just name)
      }

-- | The display name of a bound variable.
nameOfVar :: Foil.Name n -> Context n -> Binder
nameOfVar name ctx = Foil.lookupName name (ctxNames ctx)

-- * Topes

localTope :: TModality -> TModality -> TermT n -> Context n -> Context n
localTope acc var tope ctx =
  ctx { ctxTopes = ModalTope acc var tope : ctxTopes ctx }

availableTopes :: Context n -> [TermT n]
availableTopes = map topeOf . ctxTopes
