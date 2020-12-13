{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Rzk.Pretty.Text where

import           Data.Char         (chr, ord)
import           Data.Monoid       (Endo (..))
import           Data.Text         (Text)
import qualified Data.Text         as Text
import qualified Data.Text.IO      as Text
import           Text.Read         (readMaybe)

import           Rzk.Syntax.Decl
import           Rzk.Syntax.Module
import           Rzk.Syntax.Term
import           Rzk.Syntax.Var

-- Orphan 'Show' instances

instance Show (Term Var) where
  show = Text.unpack . ppTerm

instance Show (Decl Var) where
  show = Text.unpack . ppDecl

instance Show (Module Var) where
  show = Text.unpack . ppModule

-- * Unicode pretty-printers

-- | Pretty-print a variable.
ppVar :: Var -> Text
ppVar = identToUnicode . getVar

-- | Pretty-print a variable.
ppHole :: Var -> Text
ppHole = ("?" <>) . identToUnicode . getVar

ppElimWithArgs :: Text -> [Term Var] -> Text
ppElimWithArgs name args = name <> "(" <> Text.intercalate ", " (map ppTerm args) <> ")"

-- | Pretty-print a 'Term' with default type of variables.
ppTerm :: Term Var -> Text
ppTerm = \case
  Variable x -> ppVar x
  TypedTerm term ty -> ppTermParen term <> " : " <> ppTerm ty
  Hole x -> ppHole x
  Universe   -> "𝒰"
  Pi (Lambda x a Nothing m) ->
    "(" <> ppVar x <> " : " <> ppTerm a <> ") → " <> ppTerm m
  Pi (Lambda x a (Just phi) m) ->
    "(" <> ppVar x <> " : " <> ppTerm a <> " | " <> ppTerm phi <> ") → " <> ppTerm m
  Pi t -> "Pi " <> ppTermParen t
  Lambda x a Nothing m -> "λ(" <> ppVar x <> " : " <> ppTerm a <> ") → " <> ppTerm m
  Lambda x a (Just phi) m -> "λ{" <> ppVar x <> " : " <> ppTerm a <> " | " <> ppTerm phi <> "} → " <> ppTerm m
  App t1 t2 -> ppTermParen t1 <> " " <> ppTermParen t2

  Sigma (Lambda x a Nothing m) -> "∑ (" <> ppVar x <> " : " <> ppTerm a <> "), " <> ppTerm m
  Sigma t -> "∑" <> ppTermParen t
  Pair t1 t2 -> "(" <> ppTerm t1 <> ", " <> ppTerm t2 <> ")"
  First t -> "π₁ " <> ppTermParen t
  Second t -> "π₂ " <> ppTermParen t

  IdType a x y -> ppTermParen x <> " =_{" <> ppTerm a <> "} " <> ppTermParen y
  Refl a x -> "refl_{" <> ppTerm x <> " : " <> ppTerm a <> "}"
  IdJ tA a tC d x p -> ppElimWithArgs "idJ" [tA, a, tC, d, x, p]

  Cube -> "CUBE"
  CubeUnit -> "1"
  CubeUnitStar -> "⋆"
  CubeProd i j -> ppTermParen i <> " × " <> ppTermParen j

  Tope -> "TOPE"
  TopeTop -> "⊤"
  TopeBottom -> "⊥"
  TopeOr psi phi -> ppTermParen psi <> " ∨ " <> ppTermParen phi
  TopeAnd psi phi -> ppTermParen psi <> " ∧ " <> ppTermParen phi
  TopeEQ x y -> ppTermParen x <> " ≡ " <> ppTermParen y

  RecBottom -> "rec⊥"
  RecOr psi phi a_psi a_phi -> ppElimWithArgs "rec∨" [psi, phi, a_psi, a_phi]

  ExtensionType t cI psi tA phi a ->
    "〈(" <> ppVar t <> " : " <> ppTerm cI <> " | " <> ppTerm psi <> ") → " <> ppTerm tA <> "[ " <> ppTermParen phi <> " ↦ " <> ppTerm a <> " ]〉"

  where
    ppTermParen t@(Variable _) = ppTerm t
    ppTermParen t@(Hole     _) = ppTerm t
    ppTermParen t@Universe     = ppTerm t
    ppTermParen t@Cube         = ppTerm t
    ppTermParen t@CubeUnit     = ppTerm t
    ppTermParen t@CubeUnitStar = ppTerm t
    ppTermParen t@Tope         = ppTerm t
    ppTermParen t@TopeTop      = ppTerm t
    ppTermParen t@TopeBottom   = ppTerm t
    ppTermParen t@RecBottom    = ppTerm t
    ppTermParen t              = "(" <> ppTerm t <> ")"

ppDecl :: Decl Var -> Text
ppDecl Decl{..} = Text.intercalate "\n"
  [ ppVar declName <> " : " <> ppTerm declType
  , "  := " <> ppTerm declBody
  ]

ppModule :: Module Var -> Text
ppModule Module{..} = Text.intercalate "\n\n"
  (map ppDecl moduleDecls)

-- * ASCII pretty-printers

-- | Pretty-print a variable in ASCII.
ppVarASCII :: Var -> Text
ppVarASCII = unicodeToAscii . getVar

-- | Pretty-print a 'Term', but without fancy Unicode symbols.
ppTermASCII :: Term Var -> Text
ppTermASCII = unicodeToAscii . ppTerm

-- * Unicode and ASCII helpers

-- | Convert an identifier with number at the end into an identifier with subscript number:
--
-- >>> Var (identToUnicode "x3")
-- x₃
identToUnicode :: Text -> Text
identToUnicode s =
  case readMaybe oldIndex of
    Nothing        -> s
    Just oldIndexN -> prefix <> newIndex (oldIndexN :: Integer)
  where
    (prefix, index) = Text.break isDigitOrDigitSub s

    digits    = "0123456789" :: String
    digitsSub = "₀₁₂₃₄₅₆₇₈₉" :: String
    isDigitSub = (`elem` digitsSub)
    isDigit    = (`elem` digits)
    isDigitOrDigitSub c = isDigit c || isDigitSub c
    digitFromSub c
      | isDigitSub c = chr ((ord c - ord '₀') + ord '0')
      | otherwise    = c
    digitToSub c = chr ((ord c - ord '0') + ord '₀')

    oldIndex  = '0' : map digitFromSub (Text.unpack index)
    newIndex oldIndexN
      | Text.null index = ""
      | otherwise       = Text.pack (digitToSub <$> show oldIndexN)


-- | Convert Unicode representation into ASCII by replacing
-- known Unicode symbols by their ASCII analogues.
--
-- >>> unicodeToAscii "λ (A : 𝒰) → B"
-- "\\ (A : U) -> B"
unicodeToAscii :: Text -> Text
unicodeToAscii = appEndo (foldMap (Endo . uncurry Text.replace) knownAsciiReplacements)

-- | Known Unicode symbols and their ASCII replacements:
--
-- >>> ppReplacements knownAsciiReplacements
-- 𝒰 U
-- λ \
-- → ->
-- ∏ Pi
-- ₀ 0
-- ₁ 1
-- ₂ 2
-- ₃ 3
-- ₄ 4
-- ₅ 5
-- ₆ 6
-- ₇ 7
-- ₈ 8
-- ₉ 9
knownAsciiReplacements :: [(Text, Text)]
knownAsciiReplacements =
  [ ("𝒰", "U")
  , ("λ", "\\")
  , ("→", "->")
  , ("∏", "Pi")
  ] <> zipWith mkCharReplacement digitsSub digits
  where
    mkCharReplacement from to = (Text.pack [from], Text.pack [to])
    digitsSub = "₀₁₂₃₄₅₆₇₈₉"
    digits    = "0123456789"

-- | Pretty-print replacements. See 'knownAsciiReplacements'.
ppReplacements :: [(Text, Text)] -> IO ()
ppReplacements = mapM_ (\(from, to) -> Text.putStrLn (from <> " " <> to))

indent :: [Text] -> [Text]
indent = map ("  " <>)
