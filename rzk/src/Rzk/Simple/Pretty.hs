{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Rzk.Simple.Pretty where

import           Bound.Scope                               (instantiate1)
import qualified Data.Text.Lazy                            as Text
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Terminal

import           Rzk.Simple.Syntax.Term
import           Rzk.Simple.Syntax.Var

instance {-# OVERLAPPING #-} Show (AnnotatedTerm ann var Var) where
  show = Text.unpack . renderLazy . layoutPretty defaultLayoutOptions . ppTermANSIviaShow

instance {-# OVERLAPPING #-} Show (AnnotatedTerm ann var SpannedVar) where
  show = Text.unpack . renderLazy . layoutPretty defaultLayoutOptions . ppTermANSIviaShow . fmap unSpanVar

data TypeOfTerm
  = TermVariable
  | TypeVariable
  | TopeVariable
  | CubeVariable

  | TermConstructor
  | TypeConstructor
  | TopeConstructor
  | CubeConstructor

  | TermEliminator
  | TypeEliminator
  | TopeEliminator
  | CubeEliminator

  | SomeTerm
  | SomeType
  | SomeTope
  | SomeCube

  | Reserved

highlightUsingAnsiStyle :: TypeOfTerm -> AnsiStyle
highlightUsingAnsiStyle = \case
  TermVariable -> color White
  TypeVariable -> color White
  TopeVariable -> color White
  CubeVariable -> color White

  TermConstructor -> color Green <> bold
  TypeConstructor -> color Green <> bold
  TopeConstructor -> color Green <> bold
  CubeConstructor -> color Green <> bold

  TermEliminator -> color Magenta <> bold
  TypeEliminator -> color Magenta <> bold
  TopeEliminator -> color Magenta <> bold
  CubeEliminator -> color Magenta <> bold

  SomeTerm -> mempty
  SomeType -> mempty
  SomeTope -> mempty
  SomeCube -> mempty

  Reserved -> color Cyan

ppTermANSI :: Show a => [Doc TypeOfTerm] -> AnnotatedTerm ann var a -> Doc AnsiStyle
ppTermANSI vars = reAnnotate highlightUsingAnsiStyle . ppTerm vars . fmap viaShow

ppTermLeft :: [Doc TypeOfTerm] -> AnnotatedTerm ann var (Doc TypeOfTerm) -> Doc TypeOfTerm
ppTermLeft vars t@(AnnotatedTerm _ann tt) = case tt of
  Lambda{} -> parens (ppTerm vars t)
  Pi{}     -> parens (ppTerm vars t)
  Sigma{}  -> parens (ppTerm vars t)
  _        -> ppTerm vars t

ppTermParens :: [Doc TypeOfTerm] -> AnnotatedTerm ann var (Doc TypeOfTerm) -> Doc TypeOfTerm
ppTermParens vars t@(AnnotatedTerm _ann tt) = case tt of
  Lambda{}       -> ppTerm vars t
  Pi{}           -> ppTerm vars t
  Sigma{}        -> ppTerm vars t
  Variable{}     -> ppTerm vars t
  Universe       -> ppTerm vars t
  Refl{}         -> ppTerm vars t
  Cube{}         -> ppTerm vars t
  CubeUnit{}     -> ppTerm vars t
  CubeUnitStar{} -> ppTerm vars t
  Tope{}         -> ppTerm vars t
  TopeTop{}      -> ppTerm vars t
  TopeBottom{}   -> ppTerm vars t
  RecBottom{}    -> ppTerm vars t
  Cube2{}        -> ppTerm vars t
  Cube2_0{}      -> ppTerm vars t
  Cube2_1{}      -> ppTerm vars t
  _              -> parens (ppTerm vars t)

ppReserved :: Doc TypeOfTerm -> Doc TypeOfTerm
ppReserved = annotate Reserved

ppTermCon :: Doc TypeOfTerm -> Doc TypeOfTerm
ppTermCon = annotate TermConstructor

ppTypeCon :: Doc TypeOfTerm -> Doc TypeOfTerm
ppTypeCon = annotate TypeConstructor

ppTopeCon :: Doc TypeOfTerm -> Doc TypeOfTerm
ppTopeCon = annotate TopeConstructor

ppCubeCon :: Doc TypeOfTerm -> Doc TypeOfTerm
ppCubeCon = annotate CubeConstructor

ppTermElim :: Doc TypeOfTerm -> Doc TypeOfTerm
ppTermElim = annotate TermEliminator

ppTypeElim :: Doc TypeOfTerm -> Doc TypeOfTerm
ppTypeElim = annotate TypeEliminator

ppTopeElim :: Doc TypeOfTerm -> Doc TypeOfTerm
ppTopeElim = annotate TopeEliminator

ppCubeElim :: Doc TypeOfTerm -> Doc TypeOfTerm
ppCubeElim = annotate CubeEliminator

ppScoped :: [Doc TypeOfTerm] -> Scope1AnnotatedTerm ann var (Doc TypeOfTerm) -> Doc TypeOfTerm
ppScoped []       = error "Not enough fresh variables!"
ppScoped (x:vars) = ppTerm vars . instantiate1 (unannotated (Variable x))

ppVar :: Doc TypeOfTerm -> Doc TypeOfTerm
ppVar = annotate TermVariable

ppTypeVar :: Doc TypeOfTerm -> Doc TypeOfTerm
ppTypeVar = annotate TypeVariable

ppTopeVar :: Doc TypeOfTerm -> Doc TypeOfTerm
ppTopeVar = annotate TopeVariable

ppCubeVar :: Doc TypeOfTerm -> Doc TypeOfTerm
ppCubeVar = annotate CubeVariable

ppElimWithArgs :: [Doc TypeOfTerm] -> Doc TypeOfTerm -> [AnnotatedTerm ann var (Doc TypeOfTerm)] -> Doc TypeOfTerm
ppElimWithArgs vars elim args
  = elim <> parens (hsep (punctuate comma (map (ppTerm vars) args)))

ppTerm :: [Doc TypeOfTerm] -> AnnotatedTerm ann var (Doc TypeOfTerm) -> Doc TypeOfTerm
ppTerm vars (AnnotatedTerm _ann tt) = case tt of
  Variable x -> ppVar x
  App t1 t2 -> ppTermLeft vars t1 <+> ppTermParens vars t2
  First t -> ppTermElim "π₁" <+> ppTermParens vars t
  Second t -> ppTermElim "π₂" <+> ppTermParens vars t
  Universe   -> ppTypeCon "𝒰 "
  Pi (AnnotatedTerm _ann (Lambda (Just a) Nothing m)) ->
    parens (ppTypeVar var <+> ":" <+> ppTerm vars a) <+> "→ " <+> ppScoped vars m
  Pi (AnnotatedTerm _ann (Lambda (Just a) (Just phi) m)) ->
    braces (ppTypeVar var <+> ":" <+> ppTerm vars a <+> "|" <+> ppScoped vars phi) <+> "→ " <+> ppScoped vars m
  Pi t -> "Pi " <> ppTermParens vars t
  Lambda (Just a) Nothing m
    -> ppReserved "λ" <> parens (ppVar var <+> ":" <+> ppTerm vars a) <+> "→ " <> ppScoped vars m
  Lambda Nothing Nothing m
    -> ppReserved "λ" <> ppVar var <> " → " <> ppScoped vars m
  Lambda (Just a) (Just phi) m
    -> ppReserved "λ" <> braces (ppVar var <+> ":" <+> ppTerm vars a <+> "|" <+> ppScoped vars phi) <+> "→" <+> ppScoped vars m
  Lambda Nothing (Just phi) m
    -> ppReserved "λ" <> braces (ppVar var <+> "|" <+> ppScoped vars phi) <+> "→" <+> ppScoped vars m

  Sigma (AnnotatedTerm _ann (Lambda (Just a) Nothing m))
    -> ppReserved "∑" <+> parens (ppTypeVar var <+> ":" <+> ppTerm vars a) <> comma <+> ppScoped vars m
  Sigma t -> ppReserved "∑" <> ppTermParens vars t
  Pair t1 t2 -> parens (ppTerm vars t1 <> comma <+> ppTerm vars t2)

  IdType a x y -> ppTermParens vars x <> " =_{" <> ppTerm vars a <> "} " <> ppTermParens vars y
  Refl (Just a) x -> ppTermCon "refl" <> "_{" <> ppTerm vars x <> " : " <> ppTerm vars a <> "}"
  Refl Nothing x -> ppTermCon "refl" <> "_{" <> ppTerm vars x <> "}"
  IdJ tA a tC d x p -> ppElimWithArgs vars (ppTermElim "idJ") [tA, a, tC, d, x, p]

  Cube -> ppTypeCon "CUBE"
  CubeUnit -> ppCubeCon "𝟙"
  CubeUnitStar -> ppTermCon "⋆"
  CubeProd i j -> ppTermParens vars i <> " × " <> ppTermParens vars j

  Tope -> ppTypeCon "TOPE"
  TopeTop -> ppTopeCon "⊤"
  TopeBottom -> ppTopeCon "⊥"
  TopeOr psi phi -> ppTermParens vars psi <> " ∨ " <> ppTermParens vars phi
  TopeAnd psi phi -> ppTermParens vars psi <> " ∧ " <> ppTermParens vars phi
  TopeEQ x y -> ppTermParens vars x <> " ≡ " <> ppTermParens vars y

  RecBottom -> ppTermCon "rec⊥"
  RecOr psi phi a_psi a_phi
    -> ppElimWithArgs vars (ppTermCon "rec∨") [psi, phi, a_psi, a_phi]

  ExtensionType cI psi tA phi a ->
    "〈" <> braces (ppCubeVar var <+> ":" <+> ppTerm vars cI <+> "|" <+> ppScoped vars psi) <+> "→" <+> ppScoped vars tA <> brackets (ppScoped vars phi <+> "↦" <+> ppScoped vars a) <> "〉"

  Cube2 -> ppCubeCon "𝟚"
  Cube2_0 -> ppTermCon "0"
  Cube2_1 -> ppTermCon "1"
  TopeLEQ t s -> ppTermParens vars t <+> ppTopeCon "≤" <+> ppTermParens vars s
  where
    var:_ = vars

ppTermANSIviaShow :: Show a => AnnotatedTerm ann var a -> Doc AnsiStyle
ppTermANSIviaShow t = ppTermANSI vars t'
  where
    vars = (viaShow . Var) <$> zipWith appendIndexText [0..] (repeat "x")
    t' = viaShow <$> t

pp :: Show a => AnnotatedTerm ann var a -> IO ()
pp t = putDoc (ppTermANSIviaShow t <> "\n")
