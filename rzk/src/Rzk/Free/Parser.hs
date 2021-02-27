{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-unused-do-bind #-}
{-# LANGUAGE ApplicativeDo        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Rzk.Free.Parser where

import           Bound.Name                                (abstract1Name)
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

import           Rzk.Free.Syntax.Decl
import           Rzk.Free.Syntax.Module
import           Rzk.Free.Syntax.Term

type Var = Text
type Term' = Term Var Var
type Decl' = Decl Var Var
type Module' = Module Var Var

type RzkParser = Unlined Parser

rzkModuleMarkdown :: RzkParser Module'
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

rzkModule :: RzkParser Module'
rzkModule = "module" <??> do
  moduleDecls <- many rzkDecl
  return Module{..}

rzkDecl :: RzkParser Decl'
rzkDecl = "declaration" <??> do
  declName <- rzkIdent
  symbol ":"
  declType <- rzkTerm
  symbol "\n"
  symbol ":="
  declBody <- rzkTerm
  symbol "\n"
  return Decl{..}

-- ** Term

rzkTerm :: RzkParser Term'
rzkTerm = "term" <??>
  buildExpressionParser rzkOperatorTable rzkTerm'

rzkTerm' :: RzkParser Term'
rzkTerm' = "simple term" <??>
      try rzkTermPiType
--   <|> rzkTermPiShape
  -- <|> try rzkTermPair
  <|> parens rzkTerm
  <|> try rzkTermLambda
  -- <|> try rzkTermLambdaShape
  -- <|> rzkTermSigmaType
  <|> rzkTermRefl
  -- <|> rzkTermIdJ
  -- <|> rzkTermRecOr
  -- <|> rzkTermFirst
  -- <|> rzkTermSecond
  -- <|> rzkTermExtensionType
  -- <|> rzkTermExtensionTypeFromCube
  -- constants
  <|> Universe <$ (symbol "U" <|> symbol "𝒰")
--  <|> Cube <$ symbol "CUBE"
--  <|> CubeUnitStar <$ (symbol "*_1" <|> symbol "⋆")
--  <|> Cube2 <$ (symbol "2" <|> symbol "𝟚")
--  <|> Cube2_0 <$ symbol "0_2"
--  <|> Cube2_1 <$ symbol "1_2"
--  <|> CubeUnit <$ (symbol "1" <|> symbol "𝟙")
--  <|> Tope <$ symbol "TOPE"
--  <|> TopeTop <$ (symbol "TOP" <|> symbol "⊤")
--  <|> TopeBottom <$ (symbol "BOT" <|> symbol "⊥")
--  <|> RecBottom <$ (symbol "recBOT" <|> symbol "rec⊥")
  <|> rzkTermVar

rzkTermVar :: RzkParser Term'
rzkTermVar = "variable" <??>
  (Variable <$> rzkIdent)

rzkTermColonType :: RzkParser (Term', Term')
rzkTermColonType = do
  term <- rzkTerm
  colon
  type_ <- rzkTerm
  return (term, type_)

rzkTermColonType' :: RzkParser (Term', Maybe Term')
rzkTermColonType' = try withType <|> withoutType
  where
    withType = fmap Just <$> rzkTermColonType
    withoutType = (\t -> (t, Nothing)) <$> rzkTerm

rzkVarColonType' :: RzkParser (Var, Maybe Term')
rzkVarColonType' = try withType <|> withoutType
  where
    withType = fmap Just <$> parens rzkVarColonType
    withoutType = (\x -> (x, Nothing)) <$> rzkIdent

rzkVarColonType :: RzkParser (Var, Term')
rzkVarColonType = do
  x <- rzkIdent
  colon
  type_ <- rzkTerm
  return (x, type_)

rzkPattern :: RzkParser Term'
rzkPattern =
      Variable <$> rzkIdent
  -- <|> rzkTermPair

rzkPatternColonType :: RzkParser (Term', Term')
rzkPatternColonType = do
  x <- rzkPattern
  colon
  type_ <- rzkTerm
  return (x, type_)

rzkPatternColonType' :: RzkParser (Term', Maybe Term')
rzkPatternColonType' = try withType <|> withoutType
  where
    withType = fmap Just <$> parens rzkPatternColonType
    withoutType = (\pat -> (pat, Nothing)) <$> rzkPattern

rzkTermPiType :: RzkParser Term'
rzkTermPiType = "dependent function type" <??> do
  (x, a) <- parens rzkVarColonType
  symbol "->" <|> symbol "→"
  t <- rzkTerm
  return (Pi a (abstract1Name x t))

-- rzkTermPiShape :: RzkParser Term'
-- rzkTermPiShape = "dependent function type (from a shape)" <??> do
--   symbol "{"
--   (pattern, i) <- rzkPatternColonType'
--   symbol "|"
--   phi <- rzkTerm
--   symbol "}"
--   symbol "->" <|> symbol "→"
--   a <- rzkTerm
--   return (Pi (Lambda pattern i (Just phi) a))

rzkTermLambda :: RzkParser Term'
rzkTermLambda = "lambda abstraction (anonymous function from a type)" <??> do
  symbol "λ" <|> symbol "\\"
  (x, _a) <- rzkVarColonType'
  symbol "->" <|> symbol "→"
  t <- rzkTerm
  return (Lambda (abstract1Name x t))

-- rzkTermLambdaShape :: RzkParser Term'
-- rzkTermLambdaShape = "lambda abstraction (anonymous function from a shape)" <??> do
--   symbol "λ" <|> symbol "\\"
--   symbol "{"
--   (t, i) <- rzkPatternColonType
--   symbol "|"
--   phi <- rzkTerm
--   symbol "}"
--   symbol "->" <|> symbol "→"
--   a <- rzkTerm
--   return (Lambda t (Just i) (Just phi) a)

-- rzkTermSigmaType :: RzkParser Term'
-- rzkTermSigmaType = "dependent sum type" <??> do
--   symbol "∑" <|> symbol "Sigma"
--   (x, a) <- parens rzkPatternColonType
--   symbol ","
--   t <- rzkTerm
--   return (Sigma (Lambda x (Just a) Nothing t))

rzkTermRefl :: RzkParser Term'
rzkTermRefl = do
  symbol "refl_{"
  (x, a) <- rzkTermColonType
  symbol "}"
  return (Refl a x)

-- rzkTermIdJ :: RzkParser Term'
-- rzkTermIdJ = do
--   symbol "idJ"
--   symbol "("
--   tA <- rzkTerm <* comma
--   a  <- rzkTerm <* comma
--   tC <- rzkTerm <* comma
--   d  <- rzkTerm <* comma
--   x  <- rzkTerm <* comma
--   p  <- rzkTerm
--   symbol ")"
--   return (IdJ tA a tC d x p)
--
-- rzkTermRecOr :: RzkParser Term'
-- rzkTermRecOr = do
--   symbol "recOR" <|> symbol "rec∨"
--   symbol "("
--   psi <- rzkTerm <* comma
--   phi <- rzkTerm <* comma
--   a   <- rzkTerm <* comma
--   b   <- rzkTerm
--   symbol ")"
--   return (RecOr psi phi a b)
--
-- rzkTermFirst :: RzkParser Term'
-- rzkTermFirst = do
--   (symbol "first" <|> symbol "π₁") <?> "π₁"
--   First <$> rzkTerm
--
-- rzkTermSecond :: RzkParser Term'
-- rzkTermSecond = do
--   (symbol "second" <|> symbol "π₂") <?> "π₂"
--   Second <$> rzkTerm
--
-- rzkTermExtensionTypeFromCube :: RzkParser Term'
-- rzkTermExtensionTypeFromCube = between (symbol "<(") (symbol ">") $ do
--   t <- rzkPattern
--   symbol ":"
--   cI <- rzkTerm
--   symbol ")"
--   symbol "->" <|> symbol "→"
--   tA <- rzkTerm
--   mphi_a <- optional $ do
--     symbol "["
--     phi <- rzkTerm
--     symbol "|->"
--     a <- rzkTerm
--     symbol "]"
--     return (phi, a)
--   let (phi, a) = case mphi_a of
--                    Just x  -> x
--                    Nothing -> (TopeBottom, RecBottom)
--   return (ExtensionType t cI TopeTop tA phi a)
--
--
-- rzkTermExtensionType :: RzkParser Term'
-- rzkTermExtensionType = between (symbol "<{") (symbol ">") $ do
--   t <- rzkPattern
--   symbol ":"
--   cI <- rzkTerm
--   symbol "|"
--   psi <- rzkTerm
--   symbol "}"
--   symbol "->" <|> symbol "→"
--   tA <- rzkTerm
--   mphi_a <- optional $ do
--     symbol "["
--     phi <- rzkTerm
--     symbol "|->"
--     a <- rzkTerm
--     symbol "]"
--     return (phi, a)
--   let (phi, a) = case mphi_a of
--                    Just x  -> x
--                    Nothing -> (TopeBottom, RecBottom)
--   return (ExtensionType t cI psi tA phi a)

-- rzkTermPair :: RzkParser Term'
-- rzkTermPair = parens (Pair <$> rzkTerm <* comma <*> rzkTerm)

rzkTermApp :: RzkParser Term'
rzkTermApp = do
  t1 <- rzkTerm
  t2 <- rzkTerm
  return (App t1 t2)

rzkOperator :: RzkParser a -> RzkParser a
rzkOperator op = op -- <* skipMany (satisfy isSpace)

rzkOperatorTable :: OperatorTable RzkParser Term'
rzkOperatorTable =
  [ [ Infix (pure App) AssocLeft ]
  -- , [ Infix (CubeProd <$ rzkOperator (symbol "*" <|> symbol "×")) AssocLeft ]
  -- , [ Infix (TopeEQ   <$ rzkOperator (symbol "===" <|> symbol "≡")) AssocNone
  --   , Infix (TopeLEQ  <$ rzkOperator (symbol "<="  <|> symbol "≤")) AssocNone ]
  -- , [ Infix (TopeAnd  <$ rzkOperator (symbol "/\\" <|> symbol "∧")) AssocLeft ]
  -- , [ Infix (TopeOr   <$ rzkOperator (symbol "\\/" <|> symbol "∨")) AssocLeft ]
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

instance IsString Term' where
  fromString = unsafeParseTerm

instance IsString Decl' where
  fromString = unsafeParseDecl

instance IsString Module' where
  fromString = unsafeParseModule

loadModuleFromFile :: FilePath -> IO Module'
loadModuleFromFile path = do
  result <- parseFromFileEx (runUnlined rzkModule) path
  case result of
    Success m       -> return m
    Failure errInfo -> do
      putDoc (_errDoc errInfo <> "\n")
      error "Parser error while attempting loadModuleFromFile"

loadModuleFromMarkdownFile :: FilePath -> IO Module'
loadModuleFromMarkdownFile path = do
  result <- parseFromFileEx (runUnlined rzkModuleMarkdown) path
  case result of
    Success m       -> return m
    Failure errInfo -> do
      putDoc (_errDoc errInfo <> "\n")
      error "Parser error while attempting loadModuleFromFile"

unsafeParseTerm :: String -> Term'
unsafeParseTerm = unsafeParseString rzkTerm

unsafeParseDecl :: String -> Decl'
unsafeParseDecl = unsafeParseString rzkDecl

unsafeParseModule :: String -> Module'
unsafeParseModule = unsafeParseString rzkModule

unsafeParseString :: RzkParser a -> String -> a
unsafeParseString parser input =
  case parseString (runUnlined parser) mempty input of
    Success x       -> x
    Failure errInfo -> unsafePerformIO $ do
      putDoc (_errDoc errInfo <> "\n")
      error "Parser error while attempting unsafeParseString"

(<??>) :: Parsing m => String -> m a -> m a
(<??>) = flip (<?>)
