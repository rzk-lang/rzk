{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-unused-do-bind #-}
{-# LANGUAGE ApplicativeDo        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Rzk.Simple.Parser where

import           Bound.Name
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

import           Rzk.Simple.Syntax.Term
import           Rzk.Simple.Syntax.Var

type RzkParser = Parser

-- ** Term

parseSpanned :: RzkParser (Term Span Var Var) -> RzkParser (Term Span Var Var)
parseSpanned parser = annotateWithSpan <$> spanned parser
  where
    annotateWithSpan (t :~ span') = Annotated span' t

rzkVar :: RzkParser Var
rzkVar = Var <$> rzkIdent

rzkTerm :: RzkParser (Term Span Var Var)
rzkTerm = "term" <??>
  buildExpressionParser rzkOperatorTable rzkTerm'

rzkTerm' :: RzkParser (Term Span Var Var)
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
  <|> rzkTermExtensionTypeFromCube
  -- constants
  <|> Universe <$ (symbol "U" <|> symbol "𝒰")
  <|> Cube <$ symbol "CUBE"
  <|> CubeUnitStar <$ (symbol "*_1" <|> symbol "⋆")
  <|> Cube2 <$ (symbol "2" <|> symbol "𝟚")
  <|> Cube2_0 <$ symbol "0_2"
  <|> Cube2_1 <$ symbol "1_2"
  <|> CubeUnit <$ (symbol "1" <|> symbol "𝟙")
  <|> Tope <$ symbol "TOPE"
  <|> TopeTop <$ (symbol "TOP" <|> symbol "⊤")
  <|> TopeBottom <$ (symbol "BOT" <|> symbol "⊥")
  <|> RecBottom <$ (symbol "recBOT" <|> symbol "rec⊥")
  <|> rzkTermVar

rzkTermVar :: RzkParser (Term Span Var Var)
rzkTermVar = "variable" <??>
  (Variable <$> (Var <$> rzkIdent))

rzkTermColonType :: RzkParser (Term Span Var Var, Term Span Var Var)
rzkTermColonType = do
  term <- rzkTerm
  colon
  type_ <- rzkTerm
  return (term, type_)

rzkTermColonType' :: RzkParser (Term Span Var Var, Maybe (Term Span Var Var))
rzkTermColonType' = try withType <|> withoutType
  where
    withType = fmap Just <$> rzkTermColonType
    withoutType = (\t -> (t, Nothing)) <$> rzkTerm

rzkVarColonType' :: RzkParser (Var, Maybe (Term Span Var Var))
rzkVarColonType' = try withType <|> withoutType
  where
    withType = fmap Just <$> parens rzkVarColonType
    withoutType = (\x -> (Var x, Nothing)) <$> rzkIdent

rzkVarColonType :: RzkParser (Var, Term Span Var Var)
rzkVarColonType = do
  x <- Var <$> rzkIdent
  colon
  type_ <- rzkTerm
  return (x, type_)

rzkTermPiType :: RzkParser (Term Span Var Var)
rzkTermPiType = "dependent function type" <??> do
  (var, a) <- parens rzkVarColonType
  symbol "->" <|> symbol "→"
  t <- rzkTerm
  return (Pi (Lambda (Just a) Nothing (abstract1Name var t)))

rzkTermPiShape :: RzkParser (Term Span Var Var)
rzkTermPiShape = "dependent function type (from a shape)" <??> do
  symbol "{"
  (var, i) <- rzkVarColonType'
  symbol "|"
  phi <- rzkTerm
  symbol "}"
  symbol "->" <|> symbol "→"
  a <- rzkTerm
  return (Pi (Lambda i (Just (abstract1Name var phi)) (abstract1Name var a)))

rzkTermLambda :: RzkParser (Term Span Var Var)
rzkTermLambda = "lambda abstraction (anonymous function from a type)" <??> do
  symbol "λ" <|> symbol "\\"
  (x, a) <- rzkVarColonType'
  symbol "->" <|> symbol "→"
  t <- rzkTerm
  return (Lambda a Nothing (abstract1Name x t))

rzkTermLambdaShape :: RzkParser (Term Span Var Var)
rzkTermLambdaShape = "lambda abstraction (anonymous function from a shape)" <??> do
  symbol "λ" <|> symbol "\\"
  symbol "{"
  (t, i) <- rzkVarColonType
  symbol "|"
  phi <- rzkTerm
  symbol "}"
  symbol "->" <|> symbol "→"
  a <- rzkTerm
  return (Lambda (Just i) (Just (abstract1Name t phi)) (abstract1Name t a))

rzkTermSigmaType :: RzkParser (Term Span Var Var)
rzkTermSigmaType = "dependent sum type" <??> do
  symbol "∑" <|> symbol "Sigma"
  (x, a) <- parens rzkVarColonType
  symbol ","
  t <- rzkTerm
  return (Sigma (Lambda (Just a) Nothing (abstract1Name x t)))

rzkTermRefl :: RzkParser (Term Span Var Var)
rzkTermRefl = do
  symbol "refl_{"
  (x, a) <- rzkTermColonType'
  symbol "}"
  return (Refl a x)

rzkTermIdJ :: RzkParser (Term Span Var Var)
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

rzkTermRecOr :: RzkParser (Term Span Var Var)
rzkTermRecOr = do
  symbol "recOR" <|> symbol "rec∨"
  symbol "("
  psi <- rzkTerm <* comma
  phi <- rzkTerm <* comma
  a   <- rzkTerm <* comma
  b   <- rzkTerm
  symbol ")"
  return (RecOr psi phi a b)

rzkTermFirst :: RzkParser (Term Span Var Var)
rzkTermFirst = do
  (symbol "first" <|> symbol "π₁") <?> "π₁"
  First <$> rzkTerm

rzkTermSecond :: RzkParser (Term Span Var Var)
rzkTermSecond = do
  (symbol "second" <|> symbol "π₂") <?> "π₂"
  Second <$> rzkTerm

rzkTermExtensionTypeFromCube :: RzkParser (Term Span Var Var)
rzkTermExtensionTypeFromCube = between (symbol "<(") (symbol ">") $ do
  t <- rzkVar
  symbol ":"
  cI <- rzkTerm
  symbol ")"
  symbol "->" <|> symbol "→"
  tA <- rzkTerm
  mphi_a <- optional $ do
    symbol "["
    phi <- rzkTerm
    symbol "|->"
    a <- rzkTerm
    symbol "]"
    return (phi, a)
  let (phi, a) = case mphi_a of
                   Just x  -> x
                   Nothing -> (TopeBottom, RecBottom)
  return (ExtensionType cI (abstract1Name t TopeTop) (abstract1Name t tA) (abstract1Name t phi) (abstract1Name t a))


rzkTermExtensionType :: RzkParser (Term Span Var Var)
rzkTermExtensionType = between (symbol "<{") (symbol ">") $ do
  t <- rzkVar
  symbol ":"
  cI <- rzkTerm
  symbol "|"
  psi <- rzkTerm
  symbol "}"
  symbol "->" <|> symbol "→"
  tA <- rzkTerm
  mphi_a <- optional $ do
    symbol "["
    phi <- rzkTerm
    symbol "|->"
    a <- rzkTerm
    symbol "]"
    return (phi, a)
  let (phi, a) = case mphi_a of
                   Just x  -> x
                   Nothing -> (TopeBottom, RecBottom)
  return (ExtensionType cI (abstract1Name t psi) (abstract1Name t tA) (abstract1Name t phi) (abstract1Name t a))

-- firstP :: Parser (Term Span Var Var)
-- firstP = do
--   "first" <|> "π₁"
--   skipSpace
--   First <$> termParens True
--
-- secondP :: Parser (Term Span Var Var)
-- secondP = do
--   "second" <|> "π₂"
--   skipSpace
--   Second <$> termParens True

rzkTermPair :: RzkParser (Term Span Var Var)
rzkTermPair = parens (Pair <$> rzkTerm <* comma <*> rzkTerm)

rzkTermApp :: RzkParser (Term Span Var Var)
rzkTermApp = do
  t1 <- rzkTerm
  t2 <- rzkTerm
  return (App t1 t2)

rzkOperator :: RzkParser a -> RzkParser a
rzkOperator op = op -- <* skipMany (satisfy isSpace)

rzkOperatorTable :: OperatorTable RzkParser (Term Span Var Var)
rzkOperatorTable =
  [ [ Infix (pure App) AssocLeft ]
  , [ Infix (CubeProd <$ rzkOperator (symbol "*" <|> symbol "×")) AssocLeft ]
  , [ Infix (TopeEQ   <$ rzkOperator (symbol "===" <|> symbol "≡")) AssocNone
    , Infix (TopeLEQ  <$ rzkOperator (symbol "<="  <|> symbol "≤")) AssocNone ]
  , [ Infix (TopeAnd  <$ rzkOperator (symbol "/\\" <|> symbol "∧")) AssocLeft ]
  , [ Infix (TopeOr   <$ rzkOperator (symbol "\\/" <|> symbol "∨")) AssocLeft ]
  , [ Infix (rzkOperator $ do
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
  , "<="
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

instance IsString (Term Span Var Var) where
  fromString = unsafeParseTerm

unsafeParseTerm :: String -> Term Span Var Var
unsafeParseTerm = unsafeParseString rzkTerm

unsafeParseString :: RzkParser a -> String -> a
unsafeParseString parser input =
  case parseString parser mempty input of
    Success x       -> x
    Failure errInfo -> unsafePerformIO $ do
      putDoc (_errDoc errInfo <> "\n")
      error "Parser error while attempting unsafeParseString"

(<??>) :: Parsing m => String -> m a -> m a
(<??>) = flip (<?>)
