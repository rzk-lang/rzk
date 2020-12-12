{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-unused-do-bind #-}
{-# LANGUAGE ApplicativeDo        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Rzk.Parser.Text where

import           Control.Applicative
import           Data.Char                                 (isPrint, isSpace)
import qualified Data.HashSet                              as HashSet
import           Data.String                               (IsString (..))
import           Data.Text                                 (Text)
import qualified Data.Text                                 as Text
import           Data.Text.Prettyprint.Doc.Render.Terminal (putDoc)
import           System.IO.Unsafe

import           Text.Parser.Expression
import           Text.Parser.Token
import           Text.Parser.Token.Style                   (emptyIdents)
import           Text.Trifecta

import           Rzk.Syntax.Decl
import           Rzk.Syntax.Module
import           Rzk.Syntax.Term
import           Rzk.Syntax.Var

type RzkParser = Unlined Parser

rzkModuleMarkdown :: RzkParser (Module Var)
rzkModuleMarkdown = "literate rzk module in Markdown" <??> do
  string "#"
  moduleDecls <- literateMarkdownDecls "rzk" (rzkDecl <* skipMany (string "\n"))
  return Module{..}

literateMarkdownDecls :: (CharParsing m, Parsing m) => String -> m a -> m [a]
literateMarkdownDecls lang =
  regions anyChar (string ("\n```" <> lang <> "\n")) (string "```\n")

regions :: Parsing f => f text -> f bra -> f ket -> f a -> f [a]
regions txt bra ket parser = go
  where
    go = do
      manyTill txt ((bra *> notFollowedBy eof) <|> eof)
      xs <- ([] <$ eof) <|> manyTill parser ket
      ys <- ([] <$ eof) <|> go
      return (xs <> ys)

rzkModule :: RzkParser (Module Var)
rzkModule = "module" <??> do
  moduleDecls <- many rzkDecl
  return Module{..}

rzkDecl :: RzkParser (Decl Var)
rzkDecl = "declaration" <??> do
  declName <- Var <$> rzkIdent
  symbol ":"
  declType <- rzkTerm
  symbol "\n"
  symbol ":="
  declBody <- rzkTerm
  symbol "\n"
  return Decl{..}

-- ** Term

rzkTerm :: RzkParser (Term Var)
rzkTerm = "term" <??>
  buildExpressionParser rzkOperatorTable rzkTerm'

rzkTerm' :: RzkParser (Term Var)
rzkTerm' = "simple term" <??>
      try rzkTermPiType
  <|> rzkTermPiShape
  <|> try rzkTermPair
  <|> parens rzkTerm
  <|> try rzkTermLambda
  <|> try rzkTermLambdaShape
  <|> rzkTermSigmaType
  <|> rzkTermRefl
  <|> rzkTermIdJ
  <|> rzkTermRecOr
  <|> rzkTermFirst
  <|> rzkTermSecond
  <|> rzkTermExtensionType
  -- constants
  <|> Universe <$ (symbol "U" <|> symbol "𝒰")
  <|> Cube <$ symbol "CUBE"
  <|> CubeUnit <$ (symbol "1" <|> symbol "𝟏")
  <|> CubeUnitStar <$ (symbol "*_1" <|> symbol "⋆")
  <|> Tope <$ symbol "TOPE"
  <|> TopeTop <$ (symbol "TOP" <|> symbol "⊤")
  <|> TopeBottom <$ (symbol "BOT" <|> symbol "⊥")
  <|> RecBottom <$ (symbol "recBOT" <|> symbol "rec⊥")
  <|> rzkTermVar

rzkTermVar :: RzkParser (Term Var)
rzkTermVar = "variable" <??>
  (Variable <$> (Var <$> rzkIdent))

rzkTermColonType :: RzkParser (Term Var, Term Var)
rzkTermColonType = do
  term <- rzkTerm
  colon
  type_ <- rzkTerm
  return (term, type_)

rzkVarColonType :: RzkParser (Var, Term Var)
rzkVarColonType = do
  x <- Var <$> rzkIdent
  colon
  type_ <- rzkTerm
  return (x, type_)

rzkTermPiType :: RzkParser (Term Var)
rzkTermPiType = "dependent function type" <??> do
  (x, a) <- parens rzkVarColonType
  symbol "->" <|> symbol "→"
  t <- rzkTerm
  return (Pi (Lambda x a Nothing t))

rzkTermPiShape :: RzkParser (Term Var)
rzkTermPiShape = "dependent function type (from a shape)" <??> do
  symbol "{"
  t <- Var <$> rzkIdent
  symbol ":"
  i <- rzkTerm
  symbol "|"
  phi <- rzkTerm
  symbol "}"
  symbol "->" <|> symbol "→"
  a <- rzkTerm
  return (Pi (Lambda t i (Just phi) a))

rzkTermLambda :: RzkParser (Term Var)
rzkTermLambda = "lambda abstraction (anonymous function from a type)" <??> do
  symbol "λ" <|> symbol "\\"
  (x, a) <- parens rzkVarColonType
  symbol "->" <|> symbol "→"
  t <- rzkTerm
  return (Lambda x a Nothing t)

rzkTermLambdaShape :: RzkParser (Term Var)
rzkTermLambdaShape = "lambda abstraction (anonymous function from a shape)" <??> do
  symbol "λ" <|> symbol "\\"
  symbol "{"
  t <- Var <$> rzkIdent
  symbol ":"
  i <- rzkTerm
  symbol "|"
  phi <- rzkTerm
  symbol "}"
  symbol "->" <|> symbol "→"
  a <- rzkTerm
  return (Lambda t i (Just phi) a)

rzkTermSigmaType :: RzkParser (Term Var)
rzkTermSigmaType = "dependent sum type" <??> do
  symbol "∑" <|> symbol "Sigma"
  (x, a) <- parens rzkVarColonType
  symbol ","
  t <- rzkTerm
  return (Sigma (Lambda x a Nothing t))

rzkTermRefl :: RzkParser (Term Var)
rzkTermRefl = do
  symbol "refl_{"
  (x, a) <- rzkTermColonType
  symbol "}"
  return (Refl a x)

rzkTermIdJ :: RzkParser (Term Var)
rzkTermIdJ = do
  symbol "idJ"
  symbol "("
  tA <- rzkTerm <* comma
  a  <- rzkTerm <* comma
  tC <- rzkTerm <* comma
  d  <- rzkTerm <* comma
  x  <- rzkTerm <* comma
  p  <- rzkTerm
  symbol ")"
  return (IdJ tA a tC d x p)

rzkTermRecOr :: RzkParser (Term Var)
rzkTermRecOr = do
  symbol "recOR" <|> symbol "rec∨"
  symbol "("
  tA <- rzkTerm <* comma
  a  <- rzkTerm <* comma
  tC <- rzkTerm <* comma
  d  <- rzkTerm <* comma
  x  <- rzkTerm <* comma
  p  <- rzkTerm <* comma
  return (IdJ tA a tC d x p)

rzkTermFirst :: RzkParser (Term Var)
rzkTermFirst = do
  (symbol "first" <|> symbol "π₁") <?> "π₁"
  First <$> rzkTerm

rzkTermSecond :: RzkParser (Term Var)
rzkTermSecond = do
  (symbol "second" <|> symbol "π₂") <?> "π₂"
  Second <$> rzkTerm

rzkTermExtensionType :: RzkParser (Term Var)
rzkTermExtensionType = between (symbol "<") (symbol ">") $ do
  symbol "("
  t <- Var <$> rzkIdent
  symbol ":"
  cI <- rzkTerm
  symbol "|"
  psi <- rzkTerm
  symbol ")"
  symbol "->" <|> symbol "→"
  tA <- rzkTerm
  symbol "["
  phi <- rzkTerm
  symbol "|->"
  a <- rzkTerm
  symbol "]"
  return (ExtensionType t cI psi tA phi a)

-- firstP :: Parser (Term Var)
-- firstP = do
--   "first" <|> "π₁"
--   skipSpace
--   First <$> termParens True
--
-- secondP :: Parser (Term Var)
-- secondP = do
--   "second" <|> "π₂"
--   skipSpace
--   Second <$> termParens True

rzkTermPair :: RzkParser (Term Var)
rzkTermPair = parens (Pair <$> rzkTerm <* comma <*> rzkTerm)

rzkTermApp :: RzkParser (Term Var)
rzkTermApp = do
  t1 <- rzkTerm
  t2 <- rzkTerm
  return (App t1 t2)

rzkOperatorTable :: OperatorTable RzkParser (Term Var)
rzkOperatorTable =
  [ [Infix (pure App) AssocLeft]
  , [Infix (CubeProd <$ (symbol "*" <|> symbol "×")) AssocLeft]
  , [Infix (TopeEQ   <$ (symbol "===" <|> symbol "≡")) AssocNone]
  , [Infix (TopeAnd  <$ (symbol "/\\" <|> symbol "∧")) AssocLeft]
  , [Infix (TopeOr   <$ (symbol "\\/" <|> symbol "∨")) AssocLeft]
  , [Infix (do
      { symbol "=_{" ;
        t <- rzkTerm ;
        symbol "}" ;
        return (IdType t)
      }) AssocNone]
  ]

-- ** Identifiers

rzkIdent :: RzkParser Text
rzkIdent = Text.pack <$> ident rzkIdentStyle

rzkIdentStyle :: IdentifierStyle RzkParser
rzkIdentStyle = (emptyIdents @RzkParser)
  { _styleStart     = satisfy isIdentChar
  , _styleLetter    = satisfy isIdentChar
  , _styleReserved  = HashSet.fromList
  [ "data", "where", "let"
  , "if", "then", "else"
  , ":", ":=", "."
  , "\\", "->"
  , "=_", "=_{"
  , "*"
  , "/\\"
  , "\\/"
  , "==="
  , "=>", "⇒"
  , "U"
  , "recBOT"
  , "BOT"
  , "TOP"
  , "CUBE"
  , "TOPE"
  , "∑", "Sigma"
  , "refl_", "refl_{"
  , "```"
  , "<", ">" , "|->", "|"
  ]
  }

-- ** Char predicates

isIdentChar :: Char -> Bool
isIdentChar c = isPrint c && not (isSpace c) && not (isDelim c)

isDelim :: Char -> Bool
isDelim c = c `elem` ("()[]{}," :: String)

-- * Orphan 'IsString' instances

instance IsString (Term Var) where
  fromString = unsafeParseTerm

instance IsString (Decl Var) where
  fromString = unsafeParseDecl

instance IsString (Module Var) where
  fromString = unsafeParseModule

loadModuleFromFile :: FilePath -> IO (Module Var)
loadModuleFromFile path = do
  result <- parseFromFileEx (runUnlined rzkModule) path
  case result of
    Success m       -> return m
    Failure errInfo -> do
      putDoc (_errDoc errInfo <> "\n")
      error "Parser error while attempting loadModuleFromFile"

loadModuleFromMarkdownFile :: FilePath -> IO (Module Var)
loadModuleFromMarkdownFile path = do
  result <- parseFromFileEx (runUnlined rzkModuleMarkdown) path
  case result of
    Success m       -> return m
    Failure errInfo -> do
      putDoc (_errDoc errInfo <> "\n")
      error "Parser error while attempting loadModuleFromFile"

unsafeParseTerm :: String -> Term Var
unsafeParseTerm = unsafeParseString rzkTerm

unsafeParseDecl :: String -> Decl Var
unsafeParseDecl = unsafeParseString rzkDecl

unsafeParseModule :: String -> Module Var
unsafeParseModule = unsafeParseString rzkModule

unsafeParseString :: RzkParser a -> String -> a
unsafeParseString parser input =
  case parseString (runUnlined parser) mempty input of
    Success x       -> x
    Failure errInfo -> unsafePerformIO $ do
      putDoc (_errDoc errInfo <> "\n")
      error "Parser error while attempting unsafeParseString"

--
-- module_ :: Parser (Module Var)
-- module_ = do
--   moduleDecls <- decl `Atto.sepBy` (skipSpace >> Atto.many1 Atto.endOfLine)
--   return Module{..}
--
-- decl :: Parser (Decl Var)
-- decl = do
--   declName <- var
--   skipSpace >> ":" >> skipSpace
--   declType <- term
--   Atto.skipSpace >> ":=" >> skipSpace
--   declBody <- term
--   return Decl{..}
--
-- term :: Parser (Term Var)
-- term = termParens False
--
-- termParens :: Bool -> Parser (Term Var)
-- termParens useParens
--     = termParens' useParens
--   <|> parens (termParens useParens)
--
-- termParens' :: Bool -> Parser (Term Var)
-- termParens' useParens
--     = parens' idType
--   <|> parens' firstP <|> parens' secondP
--   <|> parens' idJ
--   <|> parens' recOr
--   <|> parens' cubeProd
--   <|> parens' constrainedType
--   <|> refl
--   <|> recBottom
--   <|> cubeU <|> topeU <|> universe
--   <|> topeTop <|> topeBottom
--   <|> cubeUnit <|> cubeUnitStar
--   <|> parens' piApp
--   <|> piType <|> sigmaType
--   <|> pair
--   <|> parens' piLambda
--   <|> parens' topeOr        -- FIXME: slow
--   <|> parens' topeAnd       -- FIXME: slow
--   <|> parens' topeEQ        -- FIXME: slow
--   <|> hole <|> (Variable <$> var)
--     where
--       parens' = if useParens then parens else id
--
-- parseTuple :: Parser [Term Var]
-- parseTuple
--   = "(" *> skipSpace *> Atto.sepBy1 term (skipSpace *> "," <* skipSpace) <* skipSpace <* ")"
--
-- cubeU :: Parser (Term var)
-- cubeU = Cube <$ "CUBE"
--
-- topeU :: Parser (Term var)
-- topeU = Tope <$ "TOPE"
--
-- cubeUnit :: Parser (Term var)
-- cubeUnit = CubeUnit <$ "1"
--
-- cubeUnitStar :: Parser (Term var)
-- cubeUnitStar = CubeUnit <$ ("*_1" <|> "⋆")
--
-- cubeProd :: Parser (Term Var)
-- cubeProd = do
--   i <- termParens True
--   skipSpace
--   "×" <|> "*"
--   skipSpace
--   j <- termParens True
--   return (CubeProd i j)
--
-- topeTop :: Parser (Term var)
-- topeTop = TopeTop <$ ("TOP" <|> "⊤")
--
-- topeBottom :: Parser (Term var)
-- topeBottom = TopeBottom <$ ("BOT" <|> "⊥")
--
-- topeOr :: Parser (Term Var)
-- topeOr = do
--   phi <- termParens True
--   skipSpace
--   "\\/" <|> "∨"
--   skipSpace
--   psi <- termParens True
--   return (TopeOr phi psi)
--
-- topeAnd :: Parser (Term Var)
-- topeAnd = do
--   phi <- termParens True
--   skipSpace
--   "/\\" <|> "∧"
--   skipSpace
--   psi <- termParens True
--   return (TopeAnd phi psi)
--
-- topeEQ :: Parser (Term Var)
-- topeEQ = do
--   t <- termParens True
--   skipSpace
--   "===" <|> "≡"
--   skipSpace
--   s <- termParens True
--   return (TopeEQ t s)
--
-- recBottom :: Parser (Term Var)
-- recBottom = RecBottom <$ ("recBOT" <|> "rec⊥")
--
-- recOr :: Parser (Term Var)
-- recOr = do
--   "recOR" <|> "rec∨"
--   [psi, phi, a, b] <- parseTuple
--   return (RecOr psi phi a b)
--
-- constrainedType :: Parser (Term Var)
-- constrainedType = do
--   phi <- termParens True
--   skipSpace
--   "=>"
--   skipSpace
--   a <- term
--   return (ConstrainedType phi a)
--
-- parens :: Parser a -> Parser a
-- parens p = "(" *> skipSpace *> p <* skipSpace <* ")"
--
-- piType :: Parser (Term Var)
-- piType = do
--   "{"
--   skipSpace
--   x <- var <?> "variable identifier"
--   skipSpace
--   ":"
--   skipSpace
--   a <- term <?> "type"
--   skipSpace
--   "}"
--   skipSpace
--   "->" <|> "→"
--   skipSpace
--   b <- term <?> "type"
--   return (Pi (Lambda x a b))
--
-- piLambda :: Parser (Term Var)
-- piLambda = do
--   "λ" <|> "\\"
--   skipSpace
--   "("
--   skipSpace
--   x <- var <?> "variable identifier"
--   skipSpace
--   ":"
--   skipSpace
--   a <- term <?> "type"
--   skipSpace
--   ")"
--   skipSpace
--   "->" <|> "→"
--   skipSpace
--   t <- term <?> "term"
--   return (Lambda x a t)
--
-- piApp :: Parser (Term Var)
-- piApp = do
--   t1 <- termParens True
--   skipSpace
--   t2 <- termParens True
--   return (App t1 t2)
--
-- sigmaType :: Parser (Term Var)
-- sigmaType = do
--   "Sigma" <|> "∑"
--   skipSpace
--   "("
--   skipSpace
--   x <- var <?> "variable identifier"
--   skipSpace
--   ":"
--   skipSpace
--   a <- term <?> "type"
--   skipSpace
--   ")"
--   skipSpace
--   ","
--   skipSpace
--   b <- term <?> "type"
--   return (Sigma (Lambda x a b))
--
-- pair :: Parser (Term Var)
-- pair = do
--   "("
--   skipSpace
--   f <- term
--   skipSpace
--   ","
--   skipSpace
--   s <- term
--   skipSpace
--   ")"
--   return (Pair f s)
--
-- firstP :: Parser (Term Var)
-- firstP = do
--   "first" <|> "π₁"
--   skipSpace
--   First <$> termParens True
--
-- secondP :: Parser (Term Var)
-- secondP = do
--   "second" <|> "π₂"
--   skipSpace
--   Second <$> termParens True
--
-- idType :: Parser (Term Var)
-- idType = do
--   x <- termParens True
--   skipSpace
--   "=_{"
--   skipSpace
--   a <- termParens False
--   skipSpace
--   "}"
--   skipSpace
--   y <- termParens True
--   return (IdType a x y)
--
-- refl :: Parser (Term Var)
-- refl = do
--   "refl_{"
--   skipSpace
--   x <- term
--   skipSpace
--   ":"
--   skipSpace
--   a <- term
--   skipSpace
--   "}"
--   return (Refl a x)
--
-- idJ :: Parser (Term Var)
-- idJ = do
--   "idJ"
--   [tA, a, tC, d, x, p] <- parseTuple
--   return (IdJ tA a tC d x p)
--
-- universe :: Parser (Term Var)
-- universe = do
--   "U" <|> "𝒰"
--   return Universe
--
-- hole :: Parser (Term Var)
-- hole = Hole <$> ("?" >> var)
--
-- var :: Parser Var
-- var = do
--   first <- Atto.satisfy (Atto.inClass (letters <> "_"))
--   rest <- Atto.takeWhile (Atto.inClass (letters <> digits <> digitsSub <> "_"))
--   return (Var (Text.cons first rest))
--   where
--     digits        = "0123456789"
--     digitsSub     = "₀₁₂₃₄₅₆₇₈₉"
--
--     letters = latinSmall <> latinCapital <> greekSmall
--     latinSmall    = "abcdefghijklmnopqrstuvwxyz"
--     latinCapital  = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
--     greekSmall    = "αβγδεζηθικλμνξοπρςστυφχψω" \\ "λ"
--
-- unsafeParse :: String -> Parser a -> Text -> a
-- unsafeParse name parser input =
--   case Atto.parseOnly (parser <* Atto.endOfInput) input of
--     Right t  -> t
--     Left err -> error $ unlines
--       [ "Failed parsing " <> name
--       , "    " <> Text.unpack input
--       , "Parsing error was:"
--       , err
--       ]
--
-- skipSpace :: Parser ()
-- skipSpace = Atto.skipWhile (Atto.inClass " \t")

(<??>) :: Parsing m => String -> m a -> m a
(<??>) = flip (<?>)
