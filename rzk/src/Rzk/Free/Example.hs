{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Rzk.Free.Example where

import           Control.Monad.Trans       (lift)
import           Data.String               (IsString)
import           Data.Text.Prettyprint.Doc (Pretty)

import           Rzk.Free.Parser
import           Rzk.Free.Syntax.Term
import           Rzk.Free.TypeCheck

-- $setup
-- >>> import Rzk.Free.Pretty

type TypedTerm' = TypedTerm Var Var

-- * Example terms

zero :: Term String String
zero = lam "s" (lam "z" (Variable "z"))

nat :: Int -> Term String String
nat n = lam "s" (lam "z" (iterate (App (Variable "s")) (Variable "z") !! n))

(-->) :: Term b a -> Term b a -> Term b a
a --> b = Pi a (lift b)

natT :: TypedTerm String String
natT = mkType $ (Universe --> Universe) --> (Universe --> Universe)

mkType :: (Eq a, IsString a, Pretty a, Pretty b) => Term b a -> TypedTerm b a
mkType t = unsafeTypecheckClosed t universeT

ex1 :: Term String String
ex1 = lam "f" (lam "x" (App (Variable "f") (Variable "x")))

ex1Type :: TypedTerm String String
ex1Type = mkType $ piType "f" (Universe --> UnitType) (Universe --> UnitType)

ex2 :: Term String String
ex2 = lam "f" (Refl UnitType (App (Variable "f") Universe))

-- |
-- Type and term individually:
--
-- >>> ex2
-- λx₁ → refl_{x₁ U : UNIT}
-- >>> ex2Type
-- (x₁ : (x₁ : U) → UNIT) → unit =_{UNIT} x₁ U : U
--
-- Trying to typecheck:
--
-- >>> unsafeTypecheckClosed ex2 ex2Type
-- λx₁ → refl_{x₁ U : UNIT} : (x₁ : (x₁ : U) → UNIT) → unit =_{UNIT} x₁ U
ex2Type :: TypedTerm String String
ex2Type = mkType $ piType "f" (Universe --> UnitType) (IdType UnitType Unit (App (Variable "f") Universe))

idfun :: Term String String
idfun = lam "x" (Variable "x")

-- |
-- >>> unsafeTypecheckClosed idfun idfunT
-- λx₁ → x₁ : (x₁ : U) → (λx₂ → x₂) U
idfunT :: TypedTerm String String
idfunT = mkType $ Universe --> App idfun Universe

typeOfJ :: TypedTerm String String
typeOfJ = mkType $
  piType "A" Universe $
    piType "a" (v "A") $
      piType "C" (piType "z" (v "A") (IdType (v "A") (v "a") (v "z") --> Universe)) $
        piType "d" (App (App (v "C") (v "a")) (Refl (v "A") (v "a"))) $
          piType "x" (v "A") $
            piType "p" (IdType (v "A") (v "a") (v "x")) $
              App (App (v "C") (v "x")) (v "p")
  where
    v = Variable

-- |
-- >>> ex3
-- λx₁ → λx₂ → λx₃ → λx₄ → (λx₅ → λx₆ → λx₇ → λx₈ → λx₉ → λx₁₀ → J x₅ x₆ x₇ x₈ x₉ x₁₀) x₁ x₂ (λx₅ → λx₆ → x₅ =_{x₁} x₂) refl_{x₂ : x₁} x₃ x₄
ex3 :: Term'
ex3 = "\\A -> \\x -> \\y -> \\p -> J A x (\\z -> \\q -> z =_{A} x) refl_{x : A} y p"

-- | For now you can only typecheck 'ex3' in its normal form,
-- since eta-expanded J cannot be typechecked at the moment.
ex3Type :: TypedTerm'
ex3Type = mkType "(A : U) -> (x : A) -> (y : A) -> (p : x =_{A} y) -> y =_{A} x"

ex4 :: Term'
ex4 = "\\A -> \\f -> (\\x -> \\y -> f x y) A (\\z -> z)"

ex4Type :: TypedTerm'
ex4Type = mkType "(A : U) -> ((B : U) -> (g : (z : B) -> B) -> B) -> A"