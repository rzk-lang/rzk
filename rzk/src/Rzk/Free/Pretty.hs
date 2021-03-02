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

instance (IsString a, Pretty a, Pretty b) => Show (Term b a) where
  show = show . pretty

instance (IsString a, Pretty a, Pretty b) => Pretty (Term b a) where
  pretty = ppTerm defaultFreshVars

instance (IsString a, Pretty a, Pretty b) => Pretty (TypedTerm b a) where
  pretty = ppTypedTermWithSig defaultFreshVars

instance (IsString a, Pretty a, Pretty b) => Show (TypedTerm b a) where
  show = show . pretty

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
  IdJ tA a tC d x p -> "J" <+> hsep (map (ppTermArg vars) [tA, a, tC, d, x, p])

-- | Pretty-print an untyped in a head position.
ppTermFun :: (Pretty a, Pretty b) => [a] -> Term b a -> Doc ann
ppTermFun vars = \case
  t@Variable{} -> ppTerm vars t
  t@App{} -> ppTerm vars t
  t@Refl{} -> ppTerm vars t
  t@Unit{} -> ppTerm vars t
  t@UnitType{} -> ppTerm vars t
  t@Universe{} -> ppTerm vars t
  t@IdJ{} -> ppTerm vars t

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

  t@Pi{} -> parens (ppTerm vars t)
  t@App{} -> parens (ppTerm vars t)
  t@Lambda{} -> parens (ppTerm vars t)
  t@IdType{} -> parens (ppTerm vars t)
  t@IdJ{} -> parens (ppTerm vars t)

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
  x@(Typed t _) -> ppTerm vars (untyped x) <> nest 4 ("" <:> ppTerm vars (untyped t))

ppTypedTermWithSigs
  :: (Pretty a, Pretty b) => [a] -> TypedTerm b a -> Doc ann
ppTypedTermWithSigs vars = \case
  VariableT x   -> pretty x
  x@(Typed t@UniverseT{} _) -> ppTerm vars (untyped x) <::> ppTerm vars (untyped t)
  x@(Typed t _) -> ppTerm vars (untyped x) <::> ppTypedTermWithSigs vars t

-- * Helpers

(<:>) :: Doc ann -> Doc ann -> Doc ann
l <:> r = l <+> colon <+> r

(<::>) :: Doc ann -> Doc ann -> Doc ann
l <::> r = l <+> nest 2 (colon <+> r)

defaultFreshVars :: IsString a => [a]
defaultFreshVars = [ fromString ("x" <> toIndex i) | i <- [1..] ]

toIndex :: Int -> String
toIndex n = index
  where
    digitToSub c = chr ((ord c - ord '0') + ord '₀')
    index = map digitToSub (show n)

errorDoc :: Doc ann -> a
errorDoc = error . show
