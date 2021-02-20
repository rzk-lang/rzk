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

type RzkParser = Unlined Parser

-- ** Term

rzkVar :: RzkParser Var
rzkVar = Var <$> rzkIdent

rzkTerm :: RzkParser (Term Var Var)
rzkTerm = "term" <??>
  buildExpressionParser rzkOperatorTable rzkTerm'

rzkTerm' :: RzkParser (Term Var Var)
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

rzkTermVar :: RzkParser (Term Var Var)
rzkTermVar = "variable" <??>
  (Variable <$> (Var <$> rzkIdent))

rzkTermColonType :: RzkParser (Term Var Var, Term Var Var)
rzkTermColonType = do
  term <- rzkTerm
  colon
  type_ <- rzkTerm
  return (term, type_)

rzkTermColonType' :: RzkParser (Term Var Var, Maybe (Term Var Var))
rzkTermColonType' = try withType <|> withoutType
  where
    withType = fmap Just <$> rzkTermColonType
    withoutType = (\t -> (t, Nothing)) <$> rzkTerm

rzkVarColonType' :: RzkParser (Var, Maybe (Term Var Var))
rzkVarColonType' = try withType <|> withoutType
  where
    withType = fmap Just <$> parens rzkVarColonType
    withoutType = (\x -> (Var x, Nothing)) <$> rzkIdent

rzkVarColonType :: RzkParser (Var, Term Var Var)
rzkVarColonType = do
  x <- Var <$> rzkIdent
  colon
  type_ <- rzkTerm
  return (x, type_)

rzkTermPiType :: RzkParser (Term Var Var)
rzkTermPiType = "dependent function type" <??> do
  (var, a) <- parens rzkVarColonType
  symbol "->" <|> symbol "→"
  t <- rzkTerm
  return (Pi (Lambda (Just a) Nothing (abstract1Name var t)))

rzkTermPiShape :: RzkParser (Term Var Var)
rzkTermPiShape = "dependent function type (from a shape)" <??> do
  symbol "{"
  (var, i) <- rzkVarColonType'
  symbol "|"
  phi <- rzkTerm
  symbol "}"
  symbol "->" <|> symbol "→"
  a <- rzkTerm
  return (Pi (Lambda i (Just (abstract1Name var phi)) (abstract1Name var a)))

rzkTermLambda :: RzkParser (Term Var Var)
rzkTermLambda = "lambda abstraction (anonymous function from a type)" <??> do
  symbol "λ" <|> symbol "\\"
  (x, a) <- rzkVarColonType'
  symbol "->" <|> symbol "→"
  t <- rzkTerm
  return (Lambda a Nothing (abstract1Name x t))

rzkTermLambdaShape :: RzkParser (Term Var Var)
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

rzkTermSigmaType :: RzkParser (Term Var Var)
rzkTermSigmaType = "dependent sum type" <??> do
  symbol "∑" <|> symbol "Sigma"
  (x, a) <- parens rzkVarColonType
  symbol ","
  t <- rzkTerm
  return (Sigma (Lambda (Just a) Nothing (abstract1Name x t)))

rzkTermRefl :: RzkParser (Term Var Var)
rzkTermRefl = do
  symbol "refl_{"
  (x, a) <- rzkTermColonType'
  symbol "}"
  return (Refl a x)

rzkTermIdJ :: RzkParser (Term Var Var)
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

rzkTermRecOr :: RzkParser (Term Var Var)
rzkTermRecOr = do
  symbol "recOR" <|> symbol "rec∨"
  symbol "("
  psi <- rzkTerm <* comma
  phi <- rzkTerm <* comma
  a   <- rzkTerm <* comma
  b   <- rzkTerm
  symbol ")"
  return (RecOr psi phi a b)

rzkTermFirst :: RzkParser (Term Var Var)
rzkTermFirst = do
  (symbol "first" <|> symbol "π₁") <?> "π₁"
  First <$> rzkTerm

rzkTermSecond :: RzkParser (Term Var Var)
rzkTermSecond = do
  (symbol "second" <|> symbol "π₂") <?> "π₂"
  Second <$> rzkTerm

rzkTermExtensionTypeFromCube :: RzkParser (Term Var Var)
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


rzkTermExtensionType :: RzkParser (Term Var Var)
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

-- firstP :: Parser (Term Var Var)
-- firstP = do
--   "first" <|> "π₁"
--   skipSpace
--   First <$> termParens True
--
-- secondP :: Parser (Term Var Var)
-- secondP = do
--   "second" <|> "π₂"
--   skipSpace
--   Second <$> termParens True

rzkTermPair :: RzkParser (Term Var Var)
rzkTermPair = parens (Pair <$> rzkTerm <* comma <*> rzkTerm)

rzkTermApp :: RzkParser (Term Var Var)
rzkTermApp = do
  t1 <- rzkTerm
  t2 <- rzkTerm
  return (App t1 t2)

rzkOperator :: RzkParser a -> RzkParser a
rzkOperator op = op -- <* skipMany (satisfy isSpace)

rzkOperatorTable :: OperatorTable RzkParser (Term Var Var)
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

instance IsString (Term Var Var) where
  fromString = unsafeParseTerm

unsafeParseTerm :: String -> Term Var Var
unsafeParseTerm = unsafeParseString rzkTerm

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
-- term :: Parser (Term Var Var)
-- term = termParens False
--
-- termParens :: Bool -> Parser (Term Var Var)
-- termParens useParens
--     = termParens' useParens
--   <|> parens (termParens useParens)
--
-- termParens' :: Bool -> Parser (Term Var Var)
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
-- parseTuple :: Parser [Term Var Var]
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
-- cubeProd :: Parser (Term Var Var)
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
-- topeOr :: Parser (Term Var Var)
-- topeOr = do
--   phi <- termParens True
--   skipSpace
--   "\\/" <|> "∨"
--   skipSpace
--   psi <- termParens True
--   return (TopeOr phi psi)
--
-- topeAnd :: Parser (Term Var Var)
-- topeAnd = do
--   phi <- termParens True
--   skipSpace
--   "/\\" <|> "∧"
--   skipSpace
--   psi <- termParens True
--   return (TopeAnd phi psi)
--
-- topeEQ :: Parser (Term Var Var)
-- topeEQ = do
--   t <- termParens True
--   skipSpace
--   "===" <|> "≡"
--   skipSpace
--   s <- termParens True
--   return (TopeEQ t s)
--
-- recBottom :: Parser (Term Var Var)
-- recBottom = RecBottom <$ ("recBOT" <|> "rec⊥")
--
-- recOr :: Parser (Term Var Var)
-- recOr = do
--   "recOR" <|> "rec∨"
--   [psi, phi, a, b] <- parseTuple
--   return (RecOr psi phi a b)
--
-- constrainedType :: Parser (Term Var Var)
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
-- piType :: Parser (Term Var Var)
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
-- piLambda :: Parser (Term Var Var)
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
-- piApp :: Parser (Term Var Var)
-- piApp = do
--   t1 <- termParens True
--   skipSpace
--   t2 <- termParens True
--   return (App t1 t2)
--
-- sigmaType :: Parser (Term Var Var)
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
-- pair :: Parser (Term Var Var)
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
-- firstP :: Parser (Term Var Var)
-- firstP = do
--   "first" <|> "π₁"
--   skipSpace
--   First <$> termParens True
--
-- secondP :: Parser (Term Var Var)
-- secondP = do
--   "second" <|> "π₂"
--   skipSpace
--   Second <$> termParens True
--
-- idType :: Parser (Term Var Var)
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
-- refl :: Parser (Term Var Var)
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
-- idJ :: Parser (Term Var Var)
-- idJ = do
--   "idJ"
--   [tA, a, tC, d, x, p] <- parseTuple
--   return (IdJ tA a tC d x p)
--
-- universe :: Parser (Term Var Var)
-- universe = do
--   "U" <|> "𝒰"
--   return Universe
--
-- hole :: Parser (Term Var Var)
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
