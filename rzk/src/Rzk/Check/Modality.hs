-- | The mode theory of the modalities: composition, and the coercions
-- between them (what a lock lets through). The rules are those of
-- Gratzer, Weinberger, and Buchholtz, /Directed Univalence in Simplicial
-- Homotopy Type Theory/ (2024), §3.1.1. The tables match the old checker
-- ("Rzk.TypeCheck.Context").
--
-- The coercion @Flat ⇒ Op@ is not a generating rule. It follows in the
-- generated 2-category by whiskering @Flat ⇒ Id@ on the right by @Op@:
-- @Flat ∘ Op ⇒ Id ∘ Op@ simplifies to @Flat ⇒ Op@.
module Rzk.Check.Modality
  ( TModality (..)
  , comp
  , coe
  ) where

import           Language.Rzk.Foil.Names (TModality (..))

-- | Composition of modalities.
comp :: TModality -> TModality -> TModality
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

-- | Is there a 2-cell from the first modality to the second: may a variable
-- or a tope at the first be used under a lock of the second?
coe :: TModality -> TModality -> Bool
coe Flat Id    = True
coe Flat Op    = True
coe Id Sharp   = True
coe Flat Sharp = True
coe Op Sharp   = True
coe a b        = a == b

