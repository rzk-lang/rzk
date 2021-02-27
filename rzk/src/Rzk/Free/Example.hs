module Rzk.Free.Example where

import           Control.Monad.Trans  (lift)

import           Rzk.Free.Syntax.Term
import           Rzk.Free.TypeCheck

-- $setup
-- >>> import Rzk.Free.Pretty

-- * Example terms

zero :: Term String String
zero = lam "s" (lam "z" (Variable "z"))

nat :: Int -> Term String String
nat n = lam "s" (lam "z" (iterate (App (Variable "s")) (Variable "z") !! n))

(-->) :: Term b a -> Term b a -> Term b a
a --> b = Pi a (lift b)

natT :: Eq a => TypedTerm b a
natT = mkType $ (Universe --> Universe) --> (Universe --> Universe)

mkType :: Eq a => Term b a -> TypedTerm b a
mkType t = typecheckClosed t universeT

ex1 :: Term String String
ex1 = lam "f" (lam "x" (App (Variable "f") (Variable "x")))

ex1Type :: TypedTerm String String
ex1Type = mkType $ piType "f" (Universe --> UnitType) (Universe --> UnitType)

ex2 :: Term String String
ex2 = lam "f" (Refl UnitType Unit)

-- |
-- Type and term individually:
--
-- >>> putStrLn $ ppTerm  ["x", "y", "z"] ex2
-- λx.refl_{UNIT} (unit)
-- >>> putStrLn $ ppTypedTerm  ["x", "y", "z"] ex2Type
-- (x : (x : U) -> UNIT) -> unit =_{UNIT} (x) (U)
--
-- Trying to typecheck:
--
-- >>> putStrLn $ ppTypedTermWithSig  ["x", "y", "z"] (typecheckClosed ex2 ex2Type)
-- λ(x : (x : U) -> UNIT).refl_{UNIT} (unit) : (x : (x : U) -> UNIT) -> unit =_{UNIT} (x) (U)
ex2Type :: TypedTerm String String
ex2Type = mkType $ piType "f" (Universe --> UnitType) (IdType UnitType Unit (App (Variable "f") Universe))

idfun :: Term String String
idfun = lam "x" (Variable "x")

-- |
-- >>> putStrLn $ ppTypedTermWithSig  ["x", "y", "z"] (typecheckClosed idfun idfunT)
-- λ(x : U).x : (x : U) -> (λ(y : U).y) (U)
idfunT :: TypedTerm String String
idfunT = mkType $ Universe --> App idfun Universe
