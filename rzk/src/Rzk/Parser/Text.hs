{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-unused-do-bind #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Rzk.Parser.Text where

import           Control.Applicative
import           Data.Attoparsec.Text (Parser, (<?>))
import qualified Data.Attoparsec.Text as Atto
import           Data.List            ((\\))
import           Data.String          (IsString (..))
import           Data.Text            (Text)
import qualified Data.Text            as Text
import qualified Data.Text.IO         as Text

import           Rzk.Syntax.Decl
import           Rzk.Syntax.Module
import           Rzk.Syntax.Term
import           Rzk.Syntax.Var

-- * Orphan 'IsString' instances

instance IsString (Term Var) where
  fromString = unsafeParseTerm . Text.pack

instance IsString (Decl Var) where
  fromString = unsafeParseDecl . Text.pack

instance IsString (Module Var) where
  fromString = unsafeParseModule . Text.pack

loadModuleFromFile :: FilePath -> IO (Module Var)
loadModuleFromFile path = unsafeParseModule <$> Text.readFile path

loadModuleFromMarkdownFile :: FilePath -> IO (Module Var)
loadModuleFromMarkdownFile path = unsafeParseModule . markdownToRzkSource <$> Text.readFile path

regions :: Eq a => (a, a) -> [a] -> [[a]]
regions (l, r) xs =
  case span (/= r) (drop 1 (dropWhile (/= l) xs)) of
    ([], []) -> []
    (ys, zs) -> ys : regions (l, r) (drop 1 zs)

markdownToRzkSource :: Text -> Text
markdownToRzkSource
  = Text.intercalate "\n"
  . map (Text.intercalate "\n")
  . regions ("```rzk", "```")
  . Text.lines

unsafeParseTerm :: Text -> Term Var
unsafeParseTerm = unsafeParse "term" term

unsafeParseDecl :: Text -> Decl Var
unsafeParseDecl = unsafeParse "declaration" decl

unsafeParseModule :: Text -> Module Var
unsafeParseModule = unsafeParse "module" module_

module_ :: Parser (Module Var)
module_ = do
  moduleDecls <- decl `Atto.sepBy` (skipSpace >> Atto.many1 Atto.endOfLine)
  return Module{..}

decl :: Parser (Decl Var)
decl = do
  declName <- var
  skipSpace >> ":" >> skipSpace
  declType <- term
  Atto.skipSpace >> ":=" >> skipSpace
  declBody <- term
  return Decl{..}

term :: Parser (Term Var)
term = termParens False

termParens :: Bool -> Parser (Term Var)
termParens useParens
    = termParens' useParens
  <|> parens (termParens useParens)

termParens' :: Bool -> Parser (Term Var)
termParens' useParens
    = parens' idType
  <|> parens' firstP <|> parens' secondP
  <|> parens' idJ
  <|> parens' recOr
  <|> parens' cubeProd
  <|> refl
  <|> recBottom
  <|> cubeU <|> topeU <|> universe
  <|> topeTop <|> topeBottom
  <|> cubeUnit <|> cubeUnitStar
  <|> parens' piApp
  <|> piType <|> sigmaType
  <|> pair
  <|> parens' piLambda
  <|> parens' topeOr        -- FIXME: slow
  <|> parens' topeAnd       -- FIXME: slow
  <|> parens' topeEQ        -- FIXME: slow
  <|> hole <|> (Variable <$> var)
    where
      parens' = if useParens then parens else id

parseTuple :: Parser [Term Var]
parseTuple
  = "(" *> skipSpace *> Atto.sepBy1 term (skipSpace *> "," <* skipSpace) <* skipSpace <* ")"

cubeU :: Parser (Term var)
cubeU = Cube <$ "CUBE"

topeU :: Parser (Term var)
topeU = Tope <$ "TOPE"

cubeUnit :: Parser (Term var)
cubeUnit = CubeUnit <$ "1"

cubeUnitStar :: Parser (Term var)
cubeUnitStar = CubeUnit <$ ("*_1" <|> "⋆")

cubeProd :: Parser (Term Var)
cubeProd = do
  i <- termParens True
  skipSpace
  "×" <|> "*"
  skipSpace
  j <- termParens True
  return (CubeProd i j)

topeTop :: Parser (Term var)
topeTop = TopeTop <$ ("TOP" <|> "⊤")

topeBottom :: Parser (Term var)
topeBottom = TopeBottom <$ ("BOT" <|> "⊥")

topeOr :: Parser (Term Var)
topeOr = do
  phi <- termParens True
  skipSpace
  "\\/" <|> "∨"
  skipSpace
  psi <- termParens True
  return (TopeOr phi psi)

topeAnd :: Parser (Term Var)
topeAnd = do
  phi <- termParens True
  skipSpace
  "/\\" <|> "∧"
  skipSpace
  psi <- termParens True
  return (TopeAnd phi psi)

topeEQ :: Parser (Term Var)
topeEQ = do
  t <- termParens True
  skipSpace
  "===" <|> "≡"
  skipSpace
  s <- termParens True
  return (TopeEQ t s)

recBottom :: Parser (Term Var)
recBottom = RecBottom <$ ("recBOT" <|> "rec⊥")

recOr :: Parser (Term Var)
recOr = do
  "recOR" <|> "rec∨"
  [psi, phi, a, b] <- parseTuple
  return (RecOr psi phi a b)

parens :: Parser a -> Parser a
parens p = "(" *> skipSpace *> p <* skipSpace <* ")"

piType :: Parser (Term Var)
piType = do
  "{"
  skipSpace
  x <- var <?> "variable identifier"
  skipSpace
  ":"
  skipSpace
  a <- term <?> "type"
  skipSpace
  "}"
  skipSpace
  "->" <|> "→"
  skipSpace
  b <- term <?> "type"
  return (Pi (Lambda x a b))

piLambda :: Parser (Term Var)
piLambda = do
  "λ" <|> "\\"
  skipSpace
  "("
  skipSpace
  x <- var <?> "variable identifier"
  skipSpace
  ":"
  skipSpace
  a <- term <?> "type"
  skipSpace
  ")"
  skipSpace
  "->" <|> "→"
  skipSpace
  t <- term <?> "term"
  return (Lambda x a t)

piApp :: Parser (Term Var)
piApp = do
  t1 <- termParens True
  skipSpace
  t2 <- termParens True
  return (App t1 t2)

sigmaType :: Parser (Term Var)
sigmaType = do
  "Sigma" <|> "∑"
  skipSpace
  "("
  skipSpace
  x <- var <?> "variable identifier"
  skipSpace
  ":"
  skipSpace
  a <- term <?> "type"
  skipSpace
  ")"
  skipSpace
  ","
  skipSpace
  b <- term <?> "type"
  return (Sigma (Lambda x a b))

pair :: Parser (Term Var)
pair = do
  "("
  skipSpace
  f <- term
  skipSpace
  ","
  skipSpace
  s <- term
  skipSpace
  ")"
  return (Pair f s)

firstP :: Parser (Term Var)
firstP = do
  "first" <|> "π₁"
  skipSpace
  First <$> termParens True

secondP :: Parser (Term Var)
secondP = do
  "second" <|> "π₂"
  skipSpace
  Second <$> termParens True

idType :: Parser (Term Var)
idType = do
  x <- termParens True
  skipSpace
  "=_{"
  skipSpace
  a <- termParens False
  skipSpace
  "}"
  skipSpace
  y <- termParens True
  return (IdType a x y)

refl :: Parser (Term Var)
refl = do
  "refl_{"
  skipSpace
  x <- term
  skipSpace
  ":"
  skipSpace
  a <- term
  skipSpace
  "}"
  return (Refl a x)

idJ :: Parser (Term Var)
idJ = do
  "idJ"
  [tA, a, tC, d, x, p] <- parseTuple
  return (IdJ tA a tC d x p)

universe :: Parser (Term Var)
universe = do
  "U" <|> "𝒰"
  return Universe

hole :: Parser (Term Var)
hole = Hole <$> ("?" >> var)

var :: Parser Var
var = do
  first <- Atto.satisfy (Atto.inClass (letters <> "_"))
  rest <- Atto.takeWhile (Atto.inClass (letters <> digits <> digitsSub <> "_"))
  return (Var (Text.cons first rest))
  where
    digits        = "0123456789"
    digitsSub     = "₀₁₂₃₄₅₆₇₈₉"

    letters = latinSmall <> latinCapital <> greekSmall
    latinSmall    = "abcdefghijklmnopqrstuvwxyz"
    latinCapital  = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    greekSmall    = "αβγδεζηθικλμνξοπρςστυφχψω" \\ "λ"

unsafeParse :: String -> Parser a -> Text -> a
unsafeParse name parser input =
  case Atto.parseOnly (parser <* Atto.endOfInput) input of
    Right t  -> t
    Left err -> error $ unlines
      [ "Failed parsing " <> name
      , "    " <> Text.unpack input
      , "Parsing error was:"
      , err
      ]

skipSpace :: Parser ()
skipSpace = Atto.skipWhile (Atto.inClass " \t")
