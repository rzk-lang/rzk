{-|
Module      : Formatter
Description : This module defines the formatter for rzk files.

The formatter is designed in a way that can be consumed both by the CLI and the
LSP server.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
module Rzk.Format where

import qualified Data.Text                   as T

import           Language.LSP.Protocol.Types (Position (Position),
                                              Range (Range),
                                              TextEdit (TextEdit))
import           Language.Rzk.Syntax         (tryExtractMarkdownCodeBlocks)
import           Language.Rzk.Syntax.Layout  (resolveLayout)
import           Language.Rzk.Syntax.Lex     (Posn (Pn), Tok (TK),
                                              TokSymbol (TokSymbol), Token (PT),
                                              tokens)

mkEdit :: Int -> Int -> Int -> Int -> String -> TextEdit
mkEdit startLine startCol endLine endCol newText =
  TextEdit (Range (Position (fromIntegral startLine - 1) (fromIntegral startCol - 1)) (Position (fromIntegral endLine - 1) (fromIntegral endCol - 1))) (T.pack newText)

-- TODO: more patterns, e.g. for identifiers and literals
pattern Symbol :: String -> Tok
pattern Symbol s <- TK (TokSymbol s _)

pattern Token :: String -> Int -> Int -> Token
pattern Token s line col <- PT (Pn _ line col) (Symbol s)

data FormatState = FormatState {
  parenDepth  :: Int,
  indentation :: Int
}

formatTextEdits :: String -> [TextEdit]
formatTextEdits contents = go toks
  where
    rzkBlocks = tryExtractMarkdownCodeBlocks "rzk" contents -- TODO: replace tabs with spaces
    contentLines line = lines rzkBlocks !! (line - 1) -- Sorry
    toks = resolveLayout True (tokens rzkBlocks)
    go :: [Token] -> [TextEdit]
    go [] = []
    -- Remove extra spaces between #lang and rzk-1
    go (Token "#lang" langLine langCol : Token "rzk-1" rzkLine rzkCol : tks)
      -- FIXME: Tab characters break this because BNFC increases the column number to the next multiple of 8
      -- Should probably check the first field of Pn (always incremented by 1)
      -- Or `tabSize` param sent along the formatting request
      -- But we should probably convert tabs to spaces first before any other formatting
      | rzkLine > langLine || rzkCol > langCol + 5 + 1
        = mkEdit langLine (langCol + 5) rzkLine rzkCol " "
        : go tks

    -- TODO: only one space (or line break) after #define
    -- TODO: only one space (or line break) after #define name

    -- Ensure exactly one space after the first open paren of a line
    go (Token "(" line col : tks)
      | isFirstNonSpaceChar && spacesAfter /= 1
        = mkEdit line spaceCol line (spaceCol + spacesAfter) " "
        : go tks
      where
        spaceCol = col + 1
        lineContent = contentLines line
        isFirstNonSpaceChar = all (== ' ') (take (col - 1) lineContent)
        spacesAfter = length $ takeWhile (== ' ') (drop col lineContent)

    -- TODO: line break before : (only the top-level one) and one space after

    -- Line break before := and one space after
    go (Token ":=" line col : tks)
      -- TODO: combine these 2 rules. they are not mutually exclusive
      -- Also, ensure 2 spaces before
      | not isFirstNonSpaceChar
        -- TODO: trim possible spaces before as well
        = mkEdit line col line col "\n  "
        : go tks
      | length lineContent > col + 2 && spacesAfter /= 1
        = mkEdit line (col + 2) line (col + 2 + spacesAfter) " "
        : go tks
      where
        lineContent = contentLines line
        isFirstNonSpaceChar = all (== ' ') (take (col - 1) lineContent)
        spacesAfter = length $ takeWhile (== ' ') (drop (col + 1) lineContent)
        edits = [
            (not isFirstNonSpaceChar, mkEdit line col line col "\n  ")
          ]
    -- TOOD: move any binary operators at the end of a line to the beginning of the next
    -- TODO: any binary operator should have one space after

    -- Replace some ASCII sequences with their Unicode equivalent
    go (Token "->" line col : tks)  = mkEdit line col line (col + 2) "→" : go tks
    go (Token "|->" line col : tks) = mkEdit line col line (col + 3) "↦" : go tks
    go (Token "===" line col : tks) = mkEdit line col line (col + 3) "≡" : go tks
    go (Token "<=" line col : tks)  = mkEdit line col line (col + 2) "≤" : go tks
    go (Token "/\\" line col : tks)  = mkEdit line col line (col + 2) "∧" : go tks
    go (Token "\\/" line col : tks)  = mkEdit line col line (col + 2) "∨" : go tks
    go (Token "Sigma" line col : tks)  = mkEdit line col line (col + 5) "Σ" : go tks
    go (Token "∑" line col : tks)  = mkEdit line col line (col + 1) "Σ" : go tks

    -- TODO: 0_2, 1_2, I * J
    go (_:tks) = go tks

applyTextEdits :: [TextEdit] -> String -> String
applyTextEdits edits contents = contents


format :: String -> String
format = applyTextEdits =<< formatTextEdits


-- | Format Rzk code from a file, returning the formatted version.
formatFile :: FilePath -> IO String
formatFile path = do
  contents <- readFile path
  return (format contents)

-- | Check if the given Rzk source code is well formatted.
--   This is useful for automation tasks.
isWellFormatted :: String -> Bool
isWellFormatted src = src == format src

formatFileWrite :: FilePath -> IO ()
formatFileWrite path = do
  formatted <- formatFile path
  writeFile path formatted
