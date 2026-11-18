-- | Small operations missing from the public free-foil API.
--
-- This is the only module in the new checker that uses
-- "Control.Monad.Foil.Internal". These operations are candidates for
-- upstreaming to free-foil; callers should not use the underlying 'IntMap'
-- representations of 'NameMap' or 'Substitution'.
module Rzk.Check.Foil.Extras
  ( replaceName
  , nameMapFromList
  , sinkNameMap
  , singletonSubstitution
  ) where

import           Control.Monad.Foil          (Name, NameMap, Substitution)
import qualified Control.Monad.Foil          as Foil
import           Control.Monad.Foil.Internal (NameMap (..), Substitution (..))
import qualified Data.IntMap                 as IntMap

-- | Replace the value at an existing key in a total name map. The caller
-- guarantees that the key is present, as in the entry map of a context.
replaceName :: Name n -> a -> NameMap n a -> NameMap n a
replaceName name value (NameMap m) = NameMap (IntMap.insert (Foil.nameId name) value m)

-- | Build a total name map from all scoped names and their values. Later
-- duplicate entries win; the caller supplies every name in the scope.
nameMapFromList :: [(Name n, a)] -> NameMap n a
nameMapFromList entries = NameMap (IntMap.fromList [ (Foil.nameId name, value) | (name, value) <- entries ])

-- | Sink a name map's values and scope index. Entries for the new names must
-- be added before the map is used at the extended scope.
sinkNameMap :: Foil.Sinkable e => (Name n -> Name l) -> NameMap n (e n) -> NameMap l (e l)
sinkNameMap rename (NameMap entries) = NameMap (fmap (Foil.sinkabilityProof rename) entries)

-- | A substitution with one free name mapped to one value.
singletonSubstitution :: Name i -> e o -> Substitution e i o
singletonSubstitution name value = UnsafeSubstitution (IntMap.singleton (Foil.nameId name) value)
