{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeApplications           #-}
module Rzk.Polylingual where

import           Control.Applicative
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

import qualified Rzk.Free.Syntax.Example.MLTT as MLTT
import qualified Rzk.Free.Syntax.Example.PCF  as PCF
import qualified Rzk.Free.Syntax.Example.STLC as STLC

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

data LangMode
  = Rzk1
  | STLC
  | PCF
  | MLTT
  deriving (Show, Eq)

data SomeModule
  = Module_Rzk1 (Module Var (Rzk1.Term Var))
  | Module_STLC (Module Var STLC.Term')
  | Module_PCF  (Module Var PCF.Term')
  | Module_MLTT (Module Var MLTT.Term')
  deriving (Show)

compileSomeModule :: SomeModule -> String
compileSomeModule = \case
  Module_Rzk1 m -> compileModuleRzk1 (moduleToRzk1 m)
  Module_STLC m -> compileModule runCommandSTLC m
  Module_PCF  m -> compileModule runCommandPCF  m
  Module_MLTT m -> compileModule runCommandMLTT m

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

runCommandSTLC :: Command Var STLC.Term' -> String
runCommandSTLC = \case
  TypeCheck term ty ->
    case STLC.execTypeCheck' (STLC.typecheck' term ty) of
      Right typedTerm -> show typedTerm
      Left msg        -> show msg
  Infer term ->
    case STLC.execTypeCheck' (STLC.infer' term) of
      Right typedTerm -> show typedTerm
      Left msg        -> show msg
  Evaluate EvaluateToWHNF term ->
    case STLC.execTypeCheck' (STLC.infer' term) of
      Right typedTerm -> show (STLC.whnf typedTerm)
      Left msg        -> show msg
  Evaluate EvaluateToNF term ->
    case STLC.execTypeCheck' (STLC.infer' term) of
      Right typedTerm -> show (STLC.nf typedTerm)
      Left msg        -> show msg
  Declare decl ->
    "declarations are not supported in STLC at the moment:\n  " <> show decl
  Unify _ _ -> "#unify is not supported in STLC at the moment"

runCommandPCF :: Command Var PCF.Term' -> String
runCommandPCF = \case
  TypeCheck term ty ->
    case PCF.execTypeCheck' (PCF.typecheck' term ty) of
      Right typedTerm -> show typedTerm
      Left msg        -> show msg
  Infer term ->
    case PCF.execTypeCheck' (PCF.infer' term) of
      Right typedTerm -> show typedTerm
      Left msg        -> show msg
  Evaluate EvaluateToWHNF term ->
    case PCF.execTypeCheck' (PCF.infer' term) of
      Right typedTerm -> show (PCF.whnf typedTerm)
      Left msg        -> show msg
  Evaluate EvaluateToNF term ->
    case PCF.execTypeCheck' (PCF.infer' term) of
      Right typedTerm -> show (PCF.nf typedTerm)
      Left msg        -> show msg
  Declare decl ->
    "declarations are not supported in PCF at the moment:\n  " <> show decl
  Unify _ _ -> "#unify is not supported in PCF at the moment"

runCommandMLTT :: Command Var MLTT.Term' -> String
runCommandMLTT = \case
  TypeCheck term ty ->
    case MLTT.execTypeCheck' (MLTT.typecheck' term ty) of
      Right typedTerm -> show typedTerm
      Left msg        -> show msg
  Infer term ->
    case MLTT.execTypeCheck' (MLTT.infer' term) of
      Right typedTerm -> show typedTerm
      Left msg        -> show msg
  Evaluate EvaluateToWHNF term ->
    case MLTT.execTypeCheck' (MLTT.infer' term) of
      Right typedTerm -> show (MLTT.whnf typedTerm)
      Left msg        -> show msg
  Evaluate EvaluateToNF term ->
    case MLTT.execTypeCheck' (MLTT.infer' term) of
      Right typedTerm -> show (MLTT.nf typedTerm)
      Left msg        -> show msg
  Declare decl ->
    "declarations are not supported in MLTT at the moment:\n  " <> show decl
  Unify _ _ -> "#unify is not supported in MLTT at the moment"


safeParseSomeModule :: String -> Either String SomeModule
safeParseSomeModule input =
  case parseString pSomeModule mempty input of
    Success x       -> pure x
    Failure errInfo -> Left (show errInfo)

pSomeModule :: Parser SomeModule
pSomeModule = runPolyParser $ do
  mode <- pLangMode
  m <- case mode of
    Rzk1 -> Module_Rzk1 <$> pRzk1
    STLC -> Module_STLC <$> pSTLC
    PCF  -> Module_PCF  <$> pPCF
    MLTT -> Module_MLTT <$> pMLTT
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
    , STLC <$ symbolCI "stlc"
    , PCF  <$ symbolCI "pcf"
    , MLTT <$ symbolCI "mltt"
    ]

pRzk1 :: PolyParser (Module Var (Rzk1.Term Var))
pRzk1 = Module <$> many (pCommand (PolyParser (runUnlined Rzk1.rzkTerm <* symbol "\n")))

pSTLC :: PolyParser (Module Var STLC.Term')
pSTLC = Module <$> many (pCommand STLC.pTerm)

pPCF :: PolyParser (Module Var PCF.Term')
pPCF = Module <$> many (pCommand PCF.pTerm)

pMLTT :: PolyParser (Module Var MLTT.Term')
pMLTT = Module <$> many (pCommand MLTT.pTerm)

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
