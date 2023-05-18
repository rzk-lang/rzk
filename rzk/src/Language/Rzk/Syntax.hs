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

-- | Extract rzk code from a Markdown file
--
-- >>> putStrLn $ detectMarkdownCodeBlocks "\n```rzk\n#lang rzk-1\n```\nasd asd\n```rzk\n#def x : U\n  := U\n``` asda"
-- #lang rzk-1
-- #def x : U
--   := U
extractMarkdownCodeBlocks :: String -> String -> String
extractMarkdownCodeBlocks alias = unlines . blankNonCode True . map trim . lines
  where
    blankNonCode _toBlank [] = []
    blankNonCode True (line : lines_)
      | line == "```" <> alias  = "" : blankNonCode False lines_
      | otherwise               = "" : blankNonCode True  lines_
    blankNonCode False (line : lines_)
      | line == "```"           = "" : blankNonCode True lines_
      | otherwise               = line : blankNonCode False lines_

    trim = List.dropWhileEnd isSpace
