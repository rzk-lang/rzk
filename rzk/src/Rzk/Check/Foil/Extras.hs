-- | Small operations missing from the public free-foil API.
--
-- This is the only module in the new checker that uses
-- "Control.Monad.Foil.Internal". Each operation is a candidate for upstreaming
-- to free-foil; callers should use these functions rather than the underlying
-- 'IntMap' representations.
module Rzk.Check.Foil.Extras
  ( insertName
  , insertNameId
  , nameMapFromIds
  , unionNameMaps
  , reindexNameMap
  , lookupNameId
  , singletonSubstitution
  ) where

import           Control.Monad.Foil          (Id, Name, NameMap, Substitution)
import qualified Control.Monad.Foil          as Foil
import           Control.Monad.Foil.Internal (NameMap (..), Substitution (..))
import qualified Data.IntMap                 as IntMap

-- | Insert or replace an entry without changing the scope.
insertName :: Name n -> a -> NameMap n a -> NameMap n a
insertName name value (NameMap m) = NameMap (IntMap.insert (nameId name) value m)

-- | Insert or replace an entry by its raw name ID.
insertNameId :: Id -> a -> NameMap n a -> NameMap n a
insertNameId i value (NameMap m) = NameMap (IntMap.insert i value m)

-- | Build a name map from raw name IDs and their values. Later entries win.
nameMapFromIds :: [(Id, a)] -> NameMap n a
nameMapFromIds entries = NameMap (IntMap.fromList entries)

-- | Union two name maps, preferring entries from the left one.
unionNameMaps :: NameMap n a -> NameMap n a -> NameMap n a
unionNameMaps (NameMap l) (NameMap r) = NameMap (IntMap.union l r)

-- | Reindex a name map without changing its keys. The caller must ensure that
-- those keys are meaningful in the new scope, as when two independently
-- refreshed display scopes share their original names.
reindexNameMap :: NameMap n a -> NameMap m a
reindexNameMap (NameMap m) = NameMap m

-- | Look up a raw name ID, for scope-free representations such as the tope
-- solver's points.
lookupNameId :: Id -> NameMap n a -> Maybe a
lookupNameId i (NameMap m) = IntMap.lookup i m

-- | A substitution with one free name mapped to one value.
singletonSubstitution :: Name i -> e o -> Substitution e i o
singletonSubstitution name value = UnsafeSubstitution (IntMap.singleton (nameId name) value)

-- The internal name ID is stable and independent of the scope index.
nameId :: Name n -> Id
nameId = Foil.nameId
