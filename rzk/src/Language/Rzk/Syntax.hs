{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Language.Rzk.Syntax (
  module Language.Rzk.Syntax.Abs,

  parseModuleSafe,
  parseModule,
  parseModuleRzk,
  parseModuleFile,
  parseTerm,
  resolveLayout,
  Print.Print(..), printTree,
  tryExtractMarkdownCodeBlocks,
  extractMarkdownCodeBlocks,
  tryOrDisplayException,
  tryOrDisplayExceptionIO,
) where

import           Control.Exception          (Exception (..), SomeException,
                                             evaluate, try)

import           Data.Char                  (isSpace)
import qualified Data.List                  as List

import           Language.Rzk.Syntax.Abs
import qualified Language.Rzk.Syntax.Layout as Layout
import qualified Language.Rzk.Syntax.Print  as Print

import           Language.Rzk.Syntax.Lex    (tokens, Token)
import           Language.Rzk.Syntax.Par    (pModule, pTerm)
import GHC.IO (unsafePerformIO)

tryOrDisplayException :: Either String a -> IO (Either String a)
tryOrDisplayException = tryOrDisplayExceptionIO . evaluate

tryOrDisplayExceptionIO :: IO (Either String a) -> IO (Either String a)
tryOrDisplayExceptionIO x =
  try x >>= \case
    Left (ex :: SomeException) -> return (Left (displayException ex))
    Right result               -> return result

parseModuleSafe :: String -> IO (Either String Module)
parseModuleSafe = tryOrDisplayException . parseModule

parseModule :: String -> Either String Module
parseModule = pModule . Layout.resolveLayout True . tokens . tryExtractMarkdownCodeBlocks "rzk"

parseModuleRzk :: String -> Either String Module
parseModuleRzk = pModule . Layout.resolveLayout True . tokens

parseModuleFile :: FilePath -> IO (Either String Module)
parseModuleFile path = do
  source <- readFile path
  parseModuleSafe source

parseTerm :: String -> Either String Term
parseTerm = pTerm . tokens

tryExtractMarkdownCodeBlocks :: String -> String -> String
tryExtractMarkdownCodeBlocks alias input
  | ("```" <> alias <> "\n") `List.isInfixOf` input = extractMarkdownCodeBlocks alias input
  | otherwise = input

data LineType = NonCode | CodeOf String

-- | Extract code for a given alias (e.g. "rzk" or "haskell") from a Markdown file
-- by replacing any lines that do not belong to the code in that language with blank lines.
-- This way the line numbers are preserved correctly from the original file.
--
-- All of the following notations are supported to start a code block:
--
-- * @```rzk@
-- * @```{.rzk title=\"Example\"}@
-- * @``` { .rzk title=\"Example\" }@
--
-- >>> example = "Example:\n```rzk\n#lang rzk-1\n```\nasd asd\n```rzk\n#def x : U\n  := U\n``` \nasda"
-- >>> putStrLn example
-- Example:
-- ```rzk
-- #lang rzk-1
-- ```
-- asd asd
-- ```rzk
-- #def x : U
--   := U
-- ```
-- asda
-- >>> putStrLn $ extractMarkdownCodeBlocks "rzk" example
-- <BLANKLINE>
-- <BLANKLINE>
-- #lang rzk-1
-- <BLANKLINE>
-- <BLANKLINE>
-- <BLANKLINE>
-- #def x : U
--   := U
-- <BLANKLINE>
-- <BLANKLINE>
-- <BLANKLINE>
extractMarkdownCodeBlocks :: String -> String -> String
extractMarkdownCodeBlocks alias = unlines . blankNonCode NonCode . map trim . lines
  where
    blankNonCode _prevType [] = []
    blankNonCode prevType (line : lines_) =
      case prevType of
        CodeOf lang
          | line == "```" -> "" : blankNonCode NonCode lines_
          | lang == alias -> line : blankNonCode prevType lines_
          | otherwise     -> ""   : blankNonCode prevType lines_
        NonCode -> "" : blankNonCode (identifyCodeBlockStart line) lines_

    trim = List.dropWhileEnd isSpace

identifyCodeBlockStart :: String -> LineType
identifyCodeBlockStart line
  | prefix == "```" =
      case words suffix of
        []                          -> CodeOf "text" -- default to text
        ('{':'.':lang) : _options   -> CodeOf lang   -- ``` {.rzk ...
        "{" : ('.':lang) : _options -> CodeOf lang   -- ``` { .rzk ...
        lang : _options             -> CodeOf lang   -- ```rzk ...
  | otherwise = NonCode
  where
    (prefix, suffix) = List.splitAt 3 line

-- * Making BNFC resolveLayout safer

-- | Replace layout syntax with explicit layout tokens.
resolveLayout
  :: Bool      -- ^ Whether to use top-level layout.
  -> [Token]   -- ^ Token stream before layout resolution.
  -> Either String [Token]   -- ^ Token stream after layout resolution.
resolveLayout isTopLevel toks = unsafePerformIO $ do
  -- NOTE: we use (length . show) as poor man's Control.DeepSeq.force
  -- NOTE: this is required to force all resolveLayout error's to come out
  try (evaluate (length (show resolvedToks))) >>= \case
    Left (err :: SomeException) -> return (Left (displayException err))
    Right _ -> return (Right resolvedToks)
  where
    resolvedToks = Layout.resolveLayout isTopLevel toks

-- * Overriding BNFC pretty-printer

-- | Like 'Print.printTree', but does not insert newlines for curly braces.
printTree :: Print.Print a => a -> String
printTree = render . Print.prt 0

-- | Like 'Print.render', but does not insert newlines for curly braces.
render :: Print.Doc -> String
render d = rend 0 False (map ($ "") $ d []) ""
  where
  rend
    :: Int        -- ^ Indentation level.
    -> Bool       -- ^ Pending indentation to be output before next character?
    -> [String]
    -> ShowS
  rend i p = \case
      "["      :ts -> char '[' . rend i False ts
      "("      :ts -> char '(' . rend i False ts
      -- "{"      :ts -> onNewLine i     p . showChar   '{'  . new (i+1) ts
      -- "}" : ";":ts -> onNewLine (i-1) p . showString "};" . new (i-1) ts
      -- "}"      :ts -> onNewLine (i-1) p . showChar   '}'  . new (i-1) ts
      [";"]        -> char ';'
      ";"      :ts -> char ';' . new i ts
      t  : ts@(s:_) | closingOrPunctuation s
                   -> pending . showString t . rend i False ts
      t        :ts -> pending . space t      . rend i False ts
      []           -> id
    where
    -- Output character after pending indentation.
    char :: Char -> ShowS
    char c = pending . showChar c

    -- Output pending indentation.
    pending :: ShowS
    pending = if p then indent i else id

  -- Indentation (spaces) for given indentation level.
  indent :: Int -> ShowS
  indent i = Print.replicateS (2*i) (showChar ' ')

  -- Continue rendering in new line with new indentation.
  new :: Int -> [String] -> ShowS
  new j ts = showChar '\n' . rend j True ts

  -- -- Make sure we are on a fresh line.
  -- onNewLine :: Int -> Bool -> ShowS
  -- onNewLine i p = (if p then id else showChar '\n') . indent i

  -- Separate given string from following text by a space (if needed).
  space :: String -> ShowS
  space t s =
    case (all isSpace t, null spc, null rest) of
      (True , _   , True ) -> []             -- remove trailing space
      (False, _   , True ) -> t              -- remove trailing space
      (False, True, False) -> t ++ ' ' : s   -- add space if none
      _                    -> t ++ s
    where
      (spc, rest) = span isSpace s

  closingOrPunctuation :: String -> Bool
  closingOrPunctuation [c] = c `elem` closerOrPunct
  closingOrPunctuation _   = False

  closerOrPunct :: String
  closerOrPunct = ")],;"
