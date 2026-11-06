-- | The mode theory of the modalities: composition, the coercions between
-- them (what a lock lets through), and which have a right adjoint.
module Rzk.Check.Modality
  ( TModality (..)
  , comp
  , coe
  , isRA
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

-- | Does the modality have a right adjoint (so that its values can be
-- extracted)?
isRA :: TModality -> Bool
isRA Sharp = True
isRA Op    = True
isRA Id    = True
isRA _     = False
