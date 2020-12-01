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
  = parens' piApp
  <|> universe <|> hole <|> (Variable <$> var) <|> piType
  <|> parens' piLambda
    where
      parens' = if useParens then parens else id

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
