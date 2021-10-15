{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeApplications           #-}
module Rzk.Polylingual where

import           Control.Applicative
import           Control.Monad                (void)
import           Control.Monad.Trans          (lift)
import           Data.Char                    (isPrint, isSpace)
import qualified Data.HashSet                 as HashSet
import qualified Data.Text                    as Text
import           Text.Parser.LookAhead        (LookAheadParsing (..))
import           Text.Parser.Token
import           Text.Parser.Token.Style      (emptyIdents)
import           Text.Trifecta

import qualified Rzk.Parser.Text              as Rzk1
import qualified Rzk.Pretty.Text              ()
import qualified Rzk.Syntax.Term              as Rzk1

import qualified Rzk.Free.Syntax.Example.MLTT as MLTT
import qualified Rzk.Free.Syntax.Example.STLC as STLC

import           Rzk.Syntax.Var               (Var (..))

newtype PolyParser a
  = PolyParser { runPolyParser :: Parser a }
  deriving (Functor, Applicative, Alternative, Monad, Parsing, CharParsing, LookAheadParsing)

instance LookAheadParsing p => LookAheadParsing (Unlined p) where
  lookAhead = Unlined . lookAhead . runUnlined

(<??>) = flip (<?>)

instance TokenParsing PolyParser where
  someSpace = void $ many $ do
    optional (PolyParser someSpace)
    (<??>) "line comment" $ symbol "--" *>
      many (notChar '\n') *>
      choice [ void newline, eof ]
  nesting = PolyParser . nesting . runPolyParser
  semi = PolyParser semi
  highlight h = PolyParser . highlight h . runPolyParser
  token = PolyParser . token . runPolyParser

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
  | MLTT
  deriving (Show, Eq)

data SomeModule
  = Module_Rzk1 (Module Var (Rzk1.Term Var))
  | Module_STLC (Module Var STLC.Term')
  | Module_MLTT (Module Var MLTT.Term')
  deriving (Show)

compileSomeModule :: SomeModule -> String
compileSomeModule = \case
  Module_Rzk1 m -> compileModule runCommandRzk1 m
  Module_STLC m -> compileModule runCommandSTLC m
  Module_MLTT m -> compileModule runCommandMLTT m

compileModule :: (Command var term -> String) -> Module var term -> String
compileModule runCommand Module{..} = unlines
  (map runCommand moduleCommands)

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

runCommandMLTT :: Command Var MLTT.Term' -> String
runCommandMLTT _ = "rzk-1 is not supported at the moment"

safeParseSomeModule :: String -> Either String SomeModule
safeParseSomeModule input =
  case parseString pSomeModule mempty input of
    Success x       -> pure x
    Failure errInfo -> Left (show errInfo)


pSomeModule :: Parser SomeModule
pSomeModule = do
  mode <- pLangMode
  m <- case mode of
    Rzk1 -> Module_Rzk1 <$> pRzk1
    STLC -> Module_STLC <$> pSTLC
    MLTT -> Module_MLTT <$> pMLTT
  eof
  return m

pLangMode :: Parser LangMode
pLangMode =
  string "#lang " *>
  choice
    [ Rzk1 <$ symbol "rzk-1"
    , STLC <$ symbol "stlc"
    , MLTT <$ symbol "mltt"
    ]

pRzk1 :: Parser (Module Var (Rzk1.Term Var))
pRzk1 = Module <$> many (pCommand (PolyParser (runUnlined Rzk1.rzkTerm)))

pSTLC :: Parser (Module Var STLC.Term')
pSTLC = Module <$> many (pCommand STLC.pTerm)

pMLTT :: Parser (Module Var MLTT.Term')
pMLTT = Module <$> many (pCommand (undefined <$ string ""))

pCommand :: PolyParser term -> Parser (Command Var term)
pCommand pTerm = runPolyParser $ choice
  [ Infer <$ symbol "#infer" <*> pTerm
  , TypeCheck <$ symbol "#typecheck" <*> pTerm <* symbol ":" <*> pTerm
  , Evaluate EvaluateToWHNF <$ symbol "#whnf" <*> pTerm
  , Evaluate EvaluateToNF <$ symbol "#nf" <*> pTerm
  , Unify <$ symbol "#unify" <*> pTerm <* symbol "with" <*> pTerm
  , fmap Declare $ do
      symbol "#def "
      declName <- PolyParser pIdent
      choice
        [ (try (symbol ":=") *>) $ do
            let declType = Nothing
            declBody <- pTerm
            return Decl{..}
        , (symbol ":" *>) $ do
            declType <- Just <$> pTerm
            symbol ":="
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
