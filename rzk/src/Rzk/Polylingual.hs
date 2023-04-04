{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeApplications           #-}
module Rzk.Polylingual where

import           Control.Applicative
import qualified Data.List as List
import           Control.Monad                (void)
import           Data.Char                    (isPrint, isSpace, toLower,
                                               toUpper)
import           Data.Foldable                (traverse_)
import qualified Data.HashSet                 as HashSet
import qualified Data.Text                    as Text
import           Text.Parser.LookAhead        (LookAheadParsing (..))
import           Text.Parser.Token
import           Text.Parser.Token.Style      (emptyIdents)
import           Text.Trifecta

import qualified Rzk.Parser.Text              as Rzk1
import qualified Rzk.Pretty.Text              ()
import qualified Rzk.Syntax.Decl              as Rzk1
import qualified Rzk.Syntax.Module            as Rzk1
import qualified Rzk.Syntax.Term              as Rzk1
import qualified Rzk.TypeChecker              as Rzk1

import           Rzk.Syntax.Var               (Var (..))

newtype PolyParser a
  = PolyParser { runPolyParser :: Parser a }
  deriving (Functor, Applicative, Alternative, Monad, Parsing, CharParsing, LookAheadParsing)

(<??>) :: Parsing m => String -> m a -> m a
(<??>) = flip (<?>)

lineComment :: (CharParsing m, LookAheadParsing m) => m String
lineComment = (<??>) "line comment" $
  string "--" *>
    manyTill anyChar (choice [ void newline, lookAhead eof ])

instance TokenParsing PolyParser where
  someSpace = void $ some $ choice
    [ PolyParser someSpace <?> "some space"
    , void lineComment ]

data Decl var term = Decl
  { declName :: var
  , declType :: Maybe term
  , declBody :: term
  } deriving (Show, Eq)

data EvaluationMode
  = EvaluateToWHNF
  | EvaluateToNF
  deriving (Show, Eq)

data Command var term
  = TypeCheck term term
  | Infer term
  | Evaluate EvaluationMode term
  | Declare (Decl var term)
  | Unify term term
  deriving (Show, Eq)

data Module var term = Module
  { moduleCommands :: [Command var term]
  } deriving (Show, Eq)

instance Semigroup (Module var term) where
  Module cs1 <> Module cs2 = Module (cs1 <> cs2)

data LangMode
  = Rzk1
  deriving (Show, Eq)

data SomeModule
  = Module_Rzk1 (Module Var (Rzk1.Term Var))
  deriving (Show)

combineModules :: [SomeModule] -> Either String SomeModule
combineModules [] = Left "no modules provided"
combineModules (m : ms) = combineModules1 m ms

combineModules1 :: SomeModule -> [SomeModule] -> Either String SomeModule
combineModules1 m [] = return m
combineModules1 (Module_Rzk1 m1) (Module_Rzk1 m2 : ms) =
  combineModules1 (Module_Rzk1 (m1 <> m2)) ms
-- combineModules1 _ _ =
--   Left "trying to combine modules for different languages!"

compileSomeModule :: SomeModule -> String
compileSomeModule = \case
  Module_Rzk1 m -> compileModuleRzk1 (moduleToRzk1 m)

compileModule :: (Command var term -> String) -> Module var term -> String
compileModule runCommand Module{..} = unlines
  (map runCommand moduleCommands)

moduleToRzk1 :: Module Var (Rzk1.Term Var) -> Rzk1.Module Var
moduleToRzk1 Module{..} = Rzk1.Module
  [ Rzk1.Decl name ty value
  | Declare (Decl name (Just ty) value) <- moduleCommands
  ]

compileModuleRzk1 :: Rzk1.Module Var -> String
compileModuleRzk1 = show . Rzk1.typecheckModule ["{H}"]

runCommandRzk1 :: Command Var (Rzk1.Term Var) -> String
runCommandRzk1 _ = "rzk-1 is not supported at the moment"

removeComments :: String -> String
removeComments = unlines . map removeComment . lines

removeComment :: String -> String
removeComment "" = ""
removeComment ('-':'-':' ':_) = ""
removeComment s =
  case List.span (/= ' ') s of
    (before, "") -> before
    (before, after) ->
      case List.span (== ' ') after of
        (middle, end) -> before ++ middle ++ removeComment end

-- | Extract rzk code from a Markdown file
--
-- >>> putStrLn $ detectMarkdownCodeBlocks "\n```rzk\n#lang rzk-1\n```\nasd asd\n```rzk\n#def x : U\n  := U\n``` asda"
-- #lang rzk-1
-- #def x : U
--   := U
extractMarkdownCodeBlocks :: String -> String
extractMarkdownCodeBlocks = Text.unpack . Text.intercalate "\n" . concatMap (take 1 . Text.splitOn "\n```") . drop 1 . Text.splitOn "```rzk\n" . ("# lead\n" <>) . Text.pack

tryExtractMarkdownCodeBlocks :: String -> String
tryExtractMarkdownCodeBlocks input
  | "```rzk\n" `List.isInfixOf` input = extractMarkdownCodeBlocks input
  | otherwise = input

safeParseSomeModule :: String -> Either String SomeModule
safeParseSomeModule input =
  case parseString pSomeModule mempty (removeComments (tryExtractMarkdownCodeBlocks input)) of
    Success x       -> pure x
    Failure errInfo -> Left (show errInfo)

pSomeModule :: Parser SomeModule
pSomeModule = runPolyParser $ do
  mode <- pLangMode
  m <- case mode of
    Rzk1 -> Module_Rzk1 <$> pRzk1
  eof
  return m

charCI :: CharParsing m => Char -> m Char
charCI c = choice [ char (toLower c), char (toUpper c) ]

symbolCI :: (Monad m, TokenParsing m) => String -> m String
symbolCI s = s <$ token (traverse_ charCI s) <?> show s

pLangMode :: (Monad m, TokenParsing m) => m LangMode
pLangMode = do
  void (symbol "#lang")
  choice
    [ Rzk1 <$ symbolCI "rzk-1"
    ]

pRzk1 :: PolyParser (Module Var (Rzk1.Term Var))
pRzk1 = Module <$> many (pCommand (PolyParser Rzk1.rzkTerm))

pCommand :: PolyParser term -> PolyParser (Command Var term)
pCommand pTerm = "command" <??> choice
  [ Infer <$ symbol "#infer" <*> pTerm
  , TypeCheck <$ symbol "#typecheck" <*> pTerm <* symbol ":" <*> pTerm
  , Evaluate EvaluateToWHNF <$ symbol "#whnf" <*> pTerm
  , Evaluate EvaluateToNF <$ symbol "#nf" <*> pTerm
  , Unify <$ symbol "#unify" <*> pTerm <* symbol "with" <*> pTerm
  , fmap Declare $ do
      void (symbol "#def")
      declName <- PolyParser pIdent
      choice
        [ (try (symbol ":=") *>) $ do
            let declType = Nothing
            declBody <- pTerm
            return Decl{..}
        , (symbol ":" *>) $ do
            declType <- Just <$> pTerm
            void (symbol ":=")
            declBody <- pTerm
            return Decl{..}
        ]
  ]

pIdent :: Parser Var
pIdent = Var . Text.pack <$> ident pIdentStyle

pIdentStyle :: IdentifierStyle Parser
pIdentStyle = (emptyIdents @Parser)
  { _styleStart     = satisfy isIdentChar
  , _styleLetter    = satisfy isIdentChar
  , _styleReserved  = HashSet.fromList [ "λ", "\\", "→", "->" ]
  }

-- ** Char predicates

isIdentChar :: Char -> Bool
isIdentChar c = isPrint c && not (isSpace c) && not (isDelim c)

isDelim :: Char -> Bool
isDelim c = c `elem` ("()[]{},\\λ→" :: String)
