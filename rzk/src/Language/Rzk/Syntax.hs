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
  printTree,
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
import           Language.Rzk.Syntax.Print  (printTree)

import           Language.Rzk.Syntax.Layout (resolveLayout)
import           Language.Rzk.Syntax.Lex    (tokens)
import           Language.Rzk.Syntax.Par    (pModule, pTerm)

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
parseModule = pModule . resolveLayout True . tokens . tryExtractMarkdownCodeBlocks "rzk"

parseModuleRzk :: String -> Either String Module
parseModuleRzk = pModule . resolveLayout True . tokens

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
