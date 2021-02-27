{-# OPTIONS_GHC -fno-warn-orphans    #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Rzk.Free.Pretty where

import           Bound
import           Data.Char                 (chr, ord)
import           Data.String               (IsString (fromString))
import           Data.Text.Prettyprint.Doc

import           Rzk.Free.Syntax.Term

instance (IsString a, Pretty a, Pretty b) => Pretty (Term b a) where
  pretty = ppTerm defaultFreshVars

instance (IsString a, Pretty a, Pretty b) => Pretty (TypedTerm b a) where
  pretty = ppTypedTermWithSig defaultFreshVars

-- * Untyped terms

-- | Pretty-print an untyped term.
ppTerm :: (Pretty a, Pretty b) => [a] -> Term b a -> Doc ann
ppTerm vars = \case
  Variable x -> pretty x
  App f x -> ppTermFun vars f <+> ppTermArg vars x
  Lambda body -> ppScope1Term vars body $ \x body' ->
    "λ" <> pretty x <+> "→" <+> body'
  Refl t x -> "refl_" <> braces (ppTerm vars x <:> ppTerm vars t)
  IdType t x y -> ppTermFun vars x <+> "=_" <> braces (ppTerm vars t) <+> ppTerm vars y
  UnitType -> "UNIT"
  Unit -> "unit"
  Pi a b -> ppScope1Term vars b $ \x b' ->
    parens (pretty x <:> ppTerm vars a) <+> "→" <+> b'
  Universe -> "U"

-- | Pretty-print an untyped in a head position.
ppTermFun :: (Pretty a, Pretty b) => [a] -> Term b a -> Doc ann
ppTermFun vars = \case
  t@Variable{} -> ppTerm vars t
  t@App{} -> ppTerm vars t
  t@Refl{} -> ppTerm vars t
  t@Unit{} -> ppTerm vars t
  t@UnitType{} -> ppTerm vars t
  t@Universe{} -> ppTerm vars t

  t@Lambda{} -> parens (ppTerm vars t)
  t@IdType{} -> parens (ppTerm vars t)
  t@Pi{} -> parens (ppTerm vars t)

-- | Pretty-print an untyped in an argument position.
ppTermArg :: (Pretty a, Pretty b) => [a] -> Term b a -> Doc ann
ppTermArg vars = \case
  t@Variable{} -> ppTerm vars t
  t@Refl{} -> ppTerm vars t
  t@Unit{} -> ppTerm vars t
  t@UnitType{} -> ppTerm vars t
  t@Universe{} -> ppTerm vars t

  t@App{} -> parens (ppTerm vars t)
  t@Lambda{} -> parens (ppTerm vars t)
  t@IdType{} -> parens (ppTerm vars t)
  t@Pi{} -> parens (ppTerm vars t)

ppScope1Term
  :: (Pretty a, Pretty b)
  => [a] -> Scope1Term b a -> (a -> Doc ann -> Doc ann) -> Doc ann
ppScope1Term [] _ _            = error "not enough fresh names"
ppScope1Term (x:xs) t withScope = withScope x (ppTerm xs (instantiate1 (Variable x) t))

-- * Typed terms

ppTypedTermWithSig
  :: (Pretty a, Pretty b) => [a] -> TypedTerm b a -> Doc ann
ppTypedTermWithSig vars = \case
  VariableT x   -> pretty x
  x@(Typed t _) -> ppTerm vars (untyped x) <:> ppTerm vars (untyped t)

-- * Helpers

(<:>) :: Doc ann -> Doc ann -> Doc ann
l <:> r = l <+> colon <+> r

defaultFreshVars :: IsString a => [a]
defaultFreshVars = [ fromString ("x" <> toIndex i) | i <- [1..] ]

toIndex :: Int -> String
toIndex n = index
  where
    digitToSub c = chr ((ord c - ord '0') + ord '₀')
    index = map digitToSub (show n)

errorDoc :: Doc ann -> a
errorDoc = error . show

--    -> "λ(" <> ppTerm x <> " : " <> ppTerm a <> ") → " <> ppTerm m
--  Lambda x Nothing Nothing m
--    -> "λ" <> ppTerm x <> " → " <> ppTerm m
--  Lambda x (Just a) (Just phi) m
--    -> "λ{" <> ppTerm x <> " : " <> ppTerm a <> " | " <> ppTerm phi <> "} → " <> ppTerm m
--  Lambda x Nothing (Just phi) m
--    -> "λ{" <> ppTerm x <> " | " <> ppTerm phi <> "} → " <> ppTerm m
--  App t1 t2 -> ppTermParen t1 <> " " <> ppTermParen t2
--
--  Sigma (Lambda x (Just a) Nothing m)
--    -> "∑ (" <> ppTerm x <> " : " <> ppTerm a <> "), " <> ppTerm m
--  Sigma t -> "∑" <> ppTermParen t
--  Pair t1 t2 -> "(" <> ppTerm t1 <> ", " <> ppTerm t2 <> ")"
--  First t -> ppReserved "π₁ " <> ppTermParen t
--  Second t -> ppReserved "π₂ " <> ppTermParen t
--
--  IdType a x y -> ppTermParen x <> " =_{" <> ppTerm a <> "} " <> ppTermParen y
--  Refl (Just a) x -> ppReserved "refl" <> "_{" <> ppTerm x <> " : " <> ppTerm a <> "}"
--  Refl Nothing x -> ppReserved "refl" <> "_{" <> ppTerm x <> "}"
--  IdJ tA a tC d x p -> ppElimWithArgs (ppReserved "idJ") [tA, a, tC, d, x, p]
--
--  Cube -> ppReserved "CUBE"
--  CubeUnit -> ppReserved "𝟙"
--  CubeUnitStar -> ppReserved "⋆"
--  CubeProd i j -> ppTermParen i <> " × " <> ppTermParen j
--
--  Tope -> ppReserved "TOPE"
--  TopeTop -> ppReserved "⊤"
--  TopeBottom -> ppReserved "⊥"
--  TopeOr psi phi -> ppTermParen psi <> " ∨ " <> ppTermParen phi
--  TopeAnd psi phi -> ppTermParen psi <> " ∧ " <> ppTermParen phi
--  TopeEQ x y -> ppTermParen x <> " ≡ " <> ppTermParen y
--
--  RecBottom -> ppReserved "rec⊥"
--  RecOr psi phi a_psi a_phi -> ppElimWithArgs (ppReserved "rec∨") [psi, phi, a_psi, a_phi]
--
--  ExtensionType t cI psi tA phi a ->
--    "〈{" <> ppTerm t <> " : " <> ppTerm cI <> " | " <> ppTerm psi <> "} → " <> ppTerm tA <> "[ " <> ppTermParen phi <> " ↦ " <> ppTerm a <> " ]〉"
--
--  Cube2 -> ppReserved "𝟚"
--  Cube2_0 -> ppReserved "0"
--  Cube2_1 -> ppReserved "1"
--  TopeLEQ t s -> ppTermParen t <> " " <> ppReserved "≤" <> " " <> ppTermParen s
--
