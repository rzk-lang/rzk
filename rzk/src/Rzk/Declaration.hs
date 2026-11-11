-- | Declaration identities and schematic summaries, with typed wrappers around
-- the @containers@ sets and maps. Identities belong to one checker run and name
-- installed declarations, independently of term variables and their scopes.
module Rzk.Declaration
  ( DeclarationId
  , firstDeclarationId
  , nextDeclarationId
  , DeclarationSet
  , emptyDeclarationSet
  , memberDeclarationSet
  , insertDeclarationSet
  , DeclarationSort(..)
  , DeclarationMap
  , emptyDeclarationMap
  , lookupDeclarationMap
  , insertDeclarationMap
  ) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

-- | Identity of an installed declaration. Reinstalling a declaration after
-- section generalisation requires a fresh identity because its telescope changes.
newtype DeclarationId = DeclarationId Int
  deriving (Eq)

-- | Initial value of the identity supply for a checker run.
firstDeclarationId :: DeclarationId
firstDeclarationId = DeclarationId 0

-- | Advance the identity supply after installing a declaration.
nextDeclarationId :: DeclarationId -> DeclarationId
nextDeclarationId (DeclarationId key) = DeclarationId (key + 1)

-- | A set of declaration identities. Membership does not describe term scope.
newtype DeclarationSet = DeclarationSet IntSet.IntSet

emptyDeclarationSet :: DeclarationSet
emptyDeclarationSet = DeclarationSet IntSet.empty

memberDeclarationSet :: DeclarationId -> DeclarationSet -> Bool
memberDeclarationSet (DeclarationId key) (DeclarationSet keys) = IntSet.member key keys

insertDeclarationSet :: DeclarationId -> DeclarationSet -> DeclarationSet
insertDeclarationSet (DeclarationId key) (DeclarationSet keys) =
  DeclarationSet (IntSet.insert key keys)

-- | Summary of a declaration applied to fresh parameters, without the caller's
-- tope assumptions. Actual arguments still need schematic checking at each use.
data DeclarationSort
  = DeclarationUnclassified
    -- ^ No reusable summary, including an inconclusive or unfinished probe.
  | DeclarationMetaValued
    -- ^ The result is a parameter kind rather than an object type.
  | DeclarationObjectValuedAfter Int
    -- ^ The result is an object type after this many telescope parameters.
    -- The count includes all parameters, not only cube or shape indices.
  deriving (Eq)

-- | A map keyed by declaration identities.
newtype DeclarationMap a = DeclarationMap (IntMap.IntMap a)

emptyDeclarationMap :: DeclarationMap a
emptyDeclarationMap = DeclarationMap IntMap.empty

lookupDeclarationMap :: DeclarationId -> DeclarationMap a -> Maybe a
lookupDeclarationMap (DeclarationId key) (DeclarationMap values) = IntMap.lookup key values

insertDeclarationMap :: DeclarationId -> a -> DeclarationMap a -> DeclarationMap a
insertDeclarationMap (DeclarationId key) value (DeclarationMap values) =
  DeclarationMap (IntMap.insert key value values)
