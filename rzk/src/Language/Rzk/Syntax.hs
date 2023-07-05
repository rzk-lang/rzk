{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Rzk.Syntax (
  module Language.Rzk.Syntax.Abs,

  parseModule,
  parseModuleRzk,
  parseModuleFile,
  parseTerm,
  printTree,
  tryExtractMarkdownCodeBlocks,
  extractMarkdownCodeBlocks,
) where

import           Data.Char                  (isSpace)
import qualified Data.List                  as List

import           Language.Rzk.Syntax.Abs
import           Language.Rzk.Syntax.Print  (printTree)

import           Language.Rzk.Syntax.Layout (resolveLayout)
import           Language.Rzk.Syntax.Lex    (tokens)
import           Language.Rzk.Syntax.Par    (pModule, pTerm)

parseModule :: String -> Either String Module
parseModule = pModule . resolveLayout True . tokens . tryExtractMarkdownCodeBlocks "rzk"

parseModuleRzk :: String -> Either String Module
parseModuleRzk = pModule . resolveLayout True . tokens

parseModuleFile :: FilePath -> IO (Either String Module)
parseModuleFile path = do
  parseModule <$> readFile path

parseTerm :: String -> Either String Term
parseTerm = pTerm . tokens

tryExtractMarkdownCodeBlocks :: String -> String -> String
tryExtractMarkdownCodeBlocks alias input
  | ("```" <> alias <> "\n") `List.isInfixOf` input = extractMarkdownCodeBlocks alias input
  | otherwise = input

data LineType = NonCode | CodeOf String

-- | Extract rzk code from a Markdown file
--
-- >>> putStrLn $ extractMarkdownCodeBlocks "rzk" "\n```rzk\n#lang rzk-1\n```\nasd asd\n```rzk\n#def x : U\n  := U\n``` \nasda"
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
        []              -> CodeOf "text" -- default to text
        lang : _options -> CodeOf lang
  | otherwise = NonCode
  where
    (prefix, suffix) = List.splitAt 3 line
