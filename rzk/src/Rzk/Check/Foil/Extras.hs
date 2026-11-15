-- | Small operations missing from the public free-foil API.
--
-- This is the only module in the new checker that uses
-- "Control.Monad.Foil.Internal". These operations are candidates for
-- upstreaming to free-foil; callers should not use the underlying 'IntMap'
-- representations of 'NameMap' or 'Substitution'.
module Rzk.Check.Foil.Extras
  ( replaceName
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

-- | A substitution with one free name mapped to one value.
singletonSubstitution :: Name i -> e o -> Substitution e i o
singletonSubstitution name value = UnsafeSubstitution (IntMap.singleton (Foil.nameId name) value)
