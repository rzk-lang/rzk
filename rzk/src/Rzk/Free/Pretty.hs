{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Rzk.Free.Pretty where

import           Bound
import           Data.Text            (Text)

import           Rzk.Free.Syntax.Term

-- * Pretty-printing

parens :: Text -> Text
parens s = "(" <> s <> ")"

ppTerm :: [Text] -> Term b Text -> Text
ppTerm vars = \case
  Universe -> "U"
  Variable x -> x
  App t1 t2 -> ppTermFun vars t1 <> " " <> ppTermArg vars t2
  Lambda body ->
    let z:zs = vars
     in "λ" <> z <> "." <> ppTerm zs (instantiate1 (Variable z) body)
  Pi a b ->
    let z:zs = vars
     in parens (z <> " : " <> ppTerm vars a) <> " -> " <> ppTerm zs (instantiate1 (Variable z) b)
  Unit -> "unit"
  UnitType -> "UNIT"
  IdType t x y -> ppTerm vars x <> " =_{" <> ppTerm vars t <> "} " <> ppTerm vars y
  Refl t x -> "refl_{" <> ppTerm vars t <> "} " <> ppTermArg vars x

ppTermFun :: [Text] -> Term b Text -> Text
ppTermFun vars = \case
  t@Lambda{} -> parens (ppTerm vars t)
  t -> ppTerm vars t

ppTermArg :: [Text] -> Term b Text -> Text
ppTermArg vars = \case
  t@Variable{} -> ppTerm vars t
  t -> parens (ppTerm vars t)

ppTypedTermWithSig :: [Text] -> TypedTerm b Text -> Text
ppTypedTermWithSig vars t
  = ppTypedTerm vars t <> " : " <> ppTypedTerm vars (typeOf (error "don't know types of free vars") t)

ppTypedTerm :: [Text] -> TypedTerm b Text -> Text
ppTypedTerm vars = \case
  VariableT x -> x
  AppT _ t1 t2 -> parens (ppTypedTerm vars t1) <> " " <> parens (ppTypedTerm vars t2)
  LambdaT (PiT _ a _) body ->
    let z:zs = vars
     in "λ(" <> z <> " : " <> ppTypedTerm vars a <> ")." <> ppTypedTerm zs (instantiate1 (VariableT z) body)
  LambdaT _ body ->
    let z:zs = vars
     in "λ" <> z <> "." <> ppTypedTerm zs (instantiate1 (VariableT z) body)
  UniverseT _ -> "U"
  PiT _ a b ->
    let z:zs = vars
     in parens (z <> " : " <> ppTypedTerm vars a) <> " -> " <> ppTypedTerm zs (instantiate1 (VariableT z) b)
  UnitT _ -> "unit"
  UnitTypeT _ -> "UNIT"
  IdTypeT _ t x y -> ppTypedTerm vars x <> " =_{" <> ppTypedTerm vars t <> "} " <> ppTypedTerm vars y
  ReflT _ t x -> "refl_{" <> ppTypedTerm vars t <> "} " <> parens (ppTypedTerm vars x)

