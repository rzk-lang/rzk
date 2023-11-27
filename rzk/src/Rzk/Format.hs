{-|
Module      : Formatter
Description : This module defines the formatter for rzk files.

The formatter is designed in a way that can be consumed both by the CLI and the
LSP server.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}

module Rzk.Format (
  FormattingEdit (FormattingEdit),
  formatTextEdits,
  format, formatFile, formatFileWrite,
  isWellFormatted, isWellFormattedFile,
) where

import           Control.Monad              ((<$!>))
import           Data.List                  (elemIndex, foldl', sort)

import           Language.Rzk.Syntax        (tryExtractMarkdownCodeBlocks)
import           Language.Rzk.Syntax.Layout (resolveLayout)
import           Language.Rzk.Syntax.Lex    (Posn (Pn),
                                             Tok (TK, T_VarIdentToken),
                                             TokSymbol (TokSymbol), Token (PT),
                                             tokens)

-- | All indices are 1-based (as received from the lexer)
-- Note: LSP uses 0-based indices
data FormattingEdit = FormattingEdit Int Int Int Int String
  deriving (Eq, Ord)

-- TODO: more patterns, e.g. for identifiers and literals
pattern Symbol :: String -> Tok
pattern Symbol s <- TK (TokSymbol s _)

pattern Token :: String -> Int -> Int -> Token
pattern Token s line col <- PT (Pn _ line col) (Symbol s)

-- pattern TokenSym :: String -> Int -> Int -> Token
-- pattern TokenSym s line col <- PT (Pn _ line col) (Symbol s)

pattern TokenIdent :: String -> Int -> Int -> Token
pattern TokenIdent s line col <- PT (Pn _ line col) (T_VarIdentToken s)

data FormatState = FormatState
  { parensDepth  :: Int  -- ^ The level of parentheses nesting
  , definingName :: Bool -- ^ After #define, in name or assumptions (to detect the : for the type)
  }

-- TODO: replace all tabs with 1 space before processing
formatTextEdits :: String -> [FormattingEdit]
formatTextEdits contents = go initialState toks
  where
    initialState = FormatState { parensDepth = 0, definingName = False }
    incParensDepth s = s { parensDepth = parensDepth s + 1 }
    decParensDepth s = s { parensDepth = parensDepth s - 1 }
    rzkBlocks = tryExtractMarkdownCodeBlocks "rzk" contents -- TODO: replace tabs with spaces
    contentLines line = lines rzkBlocks !! (line - 1) -- Sorry
    toks = resolveLayout True (tokens rzkBlocks)
    go :: FormatState -> [Token] -> [FormattingEdit]
    go _ [] = []
    go s (Token "#lang" langLine langCol : Token "rzk-1" rzkLine rzkCol : tks)
      -- FIXME: Tab characters break this because BNFC increases the column number to the next multiple of 8
      -- Should probably check the first field of Pn (always incremented by 1)
      -- Or `tabSize` param sent along the formatting request
      -- But we should probably convert tabs to spaces first before any other formatting
      = edits ++ go s tks
      where
        edits = map snd $ filter fst
          -- Remove extra spaces before #lang
          [ (langCol > 1, FormattingEdit langLine 1 langLine langCol "")
          -- Remove extra spaces between #lang and rzk-1
          , (rzkLine > langLine || rzkCol > langCol + 5 + 1,
              FormattingEdit langLine (langCol + 5) rzkLine rzkCol " ")
          ]

    go s (Token "#define" defLine defCol : TokenIdent _name nameLine nameCol : tks)
      = edits ++ go (s {definingName = True}) tks
      where
        edits = map snd $ filter fst
          -- Remove any space before #define
          [ (defCol > 1, FormattingEdit defLine 1 defLine defCol "")
          -- Ensure exactly one space after #define
          , (nameLine /= defLine || nameCol > defCol + 7 + 1,
              FormattingEdit defLine (defCol + 7) nameLine nameCol " ")
          ]
    -- #def is an alias for #define
    go s (Token "#def" line col : tks) = go s (PT (Pn 0 line col) (TK (TokSymbol "#define" 0)):tks)
    -- TODO: similarly for other commands

    -- Ensure exactly one space after the first open paren of a line
    go s (Token "(" line col : tks)
      | isFirstNonSpaceChar && spacesAfter /= 1
        = FormattingEdit line spaceCol line (spaceCol + spacesAfter) " "
        : go (incParensDepth s) tks
      -- Remove extra spaces if it's not the first open paren on a new line
      | not isFirstNonSpaceChar && spacesAfter > 0
        = FormattingEdit line spaceCol line (spaceCol + spacesAfter) ""
        : go (incParensDepth s) tks
      | otherwise = go (incParensDepth s) tks
      -- TODO: Split after 80 chars
      where
        spaceCol = col + 1
        lineContent = contentLines line
        isFirstNonSpaceChar = all (== ' ') (take (col - 1) lineContent)
        spacesAfter = length $ takeWhile (== ' ') (drop col lineContent)

    -- Remove any space before the closing paren
    go s (Token ")" line col : tks)
      = edits ++ go (decParensDepth s) tks
      where
        lineContent = contentLines line
        isFirstNonSpaceChar = all (== ' ') (take (col - 1) lineContent)
        spacesBefore = length $ takeWhile (== ' ') (reverse $ take (col - 1) lineContent)
        edits = map snd $ filter fst
          [ (not isFirstNonSpaceChar && spacesBefore > 0,
              FormattingEdit line (col - spacesBefore) line col "")
          ]

    -- line break before : (only the top-level one) and one space after
    go s (Token ":" line col : tks)
      | isDefinitionTypeSeparator = typeSepEdits ++ go (s {definingName = False}) tks
      | otherwise                 = normalEdits ++ go s tks
      where
        isDefinitionTypeSeparator = parensDepth s == 0 && definingName s
        lineContent = contentLines line
        isFirstNonSpaceChar = all (== ' ') (take (col - 1) lineContent)
        spacesBefore = length $ takeWhile (== ' ') (reverse $ take (col - 1) lineContent)
        spaceCol = col + 1
        spacesAfter = length $ takeWhile (== ' ') (drop col lineContent)
        typeSepEdits = map snd $ filter fst
          -- Ensure line break before :
          [ (not isFirstNonSpaceChar, FormattingEdit line col line col "\n  ")
          -- Ensure 2 spaces before : (if already on a new line)
          , (isFirstNonSpaceChar && spacesBefore /= 2, FormattingEdit line 1 line col "  ")
          -- Ensure 1 space after
          , (spacesAfter /= 1, FormattingEdit line spaceCol line (spaceCol + spacesAfter) " ")
          ]
        normalEdits = map snd $ filter fst
          -- 1 space before :
          [ (spacesBefore /= 1, FormattingEdit line (col - spacesBefore) line col " ")
          -- 1 space after
          , (spacesAfter /= 1, FormattingEdit line spaceCol line (spaceCol + spacesAfter) " ")
          ]

    -- Line break before := and one space after
    go s (Token ":=" line col : tks)
      = edits ++ go s tks
      where
        lineContent = contentLines line
        isFirstNonSpaceChar = all (== ' ') (take (col - 1) lineContent)
        spacesAfter = length $ takeWhile (== ' ') (drop (col + 1) lineContent)
        spacesBefore = length $ takeWhile (== ' ') (take (col - 1) lineContent)
        edits = map snd $ filter fst
            -- Ensure line break before `:=`
          [ (not isFirstNonSpaceChar, FormattingEdit line col line col "\n  ")
            -- Ensure 2 spaces before `:=` (if already on a new line)
          , (isFirstNonSpaceChar && spacesBefore /= 2,
              FormattingEdit line 1 line col "  ")
            -- Ensure exactly one space after
          , (length lineContent > col + 2 && spacesAfter /= 1,
              FormattingEdit line (col + 2) line (col + 2 + spacesAfter) " ")
          ]

    -- One space after \
    go s (Token "\\" line col : tks)
      = edits ++ go s tks
      where
        lineContent = contentLines line
        spacesAfter = length $ takeWhile (== ' ') (drop col lineContent)
        spaceCol = col + 1
        edits = map snd $ filter fst
          [ (spacesAfter /= 1, FormattingEdit line spaceCol line (spaceCol + spacesAfter) " ")
          ]

    -- One space (or new line) around binary operators ('->' or '→' or ',')
    go s (Token binOp line col : tks)
      | binOp `elem` ["->", "→", ","]
      = edits ++ go s tks
      where
        lineContent = contentLines line
        spacesBefore = length $ takeWhile (== ' ') (reverse $ take (col - 1) lineContent)
        spacesAfter = length $ takeWhile (== ' ') (drop (col + length binOp - 1) lineContent)
        isFirstNonSpaceChar = all (== ' ') (take (col - 1) lineContent)
        isLastNonSpaceChar = all (== ' ') (drop (col + length binOp - 1) lineContent)
        nextLine
          | line + 1 < length (lines rzkBlocks) = contentLines (line + 1)
          | otherwise = ""
        spacesNextLine = length $ takeWhile (== ' ') nextLine
        edits = map snd $ filter fst
          -- Ensure exactly one space before (unless first char in line)
          [ (not isFirstNonSpaceChar && spacesBefore /= 1,
              FormattingEdit line (col - spacesBefore) line col " ")
          -- Ensure exactly one space after (unless last char in line)
          , (not isLastNonSpaceChar && spacesAfter /= 1,
              FormattingEdit line (col + length binOp) line (col + length binOp + spacesAfter) " ")
          -- If last char in line, move it to next line
          , (isLastNonSpaceChar,
              FormattingEdit line col (line + 1) (spacesNextLine + 1) $
                "\n" ++ replicate spacesNextLine ' ' ++ binOp ++ " ")
          ]

    -- TOOD: move any binary operators at the end of a line to the beginning of the next
    -- TODO: any binary operator should have one space after (and before if not in beginning of line)

    -- Replace some ASCII sequences with their Unicode equivalent
    go s (Token tk line col : tks)
      | Just unicodeToken <- tk `lookup` unicodeTokens
      = FormattingEdit line col line (col + length tk) unicodeToken : go s tks
      where
        unicodeTokens =
          [ ("->", "→")
          , ("|->", "↦")
          , ("===", "≡")
          , ("<=", "≤")
          , ("/\\", "∧")
          , ("\\/", "∨")
          , ("Sigma", "Σ")
          , ("∑", "Σ")
          , ("*_1", "*₁")
          , ("0_2", "0₂")
          , ("1_2", "1₂")
          , ("*", "×")
          ]

    -- Reset any state necessary after finishing a command
    go s (Token ";" _ _ : tks) = go s tks
    go s (_:tks) = go s tks

-- Adapted from https://hackage.haskell.org/package/lsp-types-2.1.0.0/docs/Language-LSP-Protocol-Types.html#g:7
applyTextEdit :: FormattingEdit -> String -> String
applyTextEdit (FormattingEdit sl sc el ec newText) oldText =
  let (_, afterEnd) = splitAtPos (el-1, ec -1) oldText
      (beforeStart, _) = splitAtPos (sl-1, sc-1) oldText
   in mconcat [beforeStart, newText, afterEnd]
 where
  splitAtPos :: (Int, Int) -> String -> (String, String)
  splitAtPos (l, c) t = let index = c + startLineIndex l t in splitAt index t

  startLineIndex :: Int -> String -> Int
  startLineIndex 0 _ = 0
  startLineIndex line t' =
    case elemIndex '\n' t' of
      Just i  -> i + 1 + startLineIndex (line - 1) (drop (i + 1) t')
      Nothing -> length t'

applyTextEdits :: [FormattingEdit] -> String -> String
applyTextEdits edits contents = foldl' (flip applyTextEdit) contents (reverse $ sort edits)

-- | Format Rzk code, returning the formatted version.
format :: String -> String
format = applyTextEdits =<< formatTextEdits

-- | Format Rzk code from a file
formatFile :: FilePath -> IO String
formatFile path = format <$!> readFile path -- strict because possibility of writing to same file

-- | Format the file and write the result back to the file.
formatFileWrite :: FilePath -> IO ()
formatFileWrite path = formatFile path >>= writeFile path

-- | Check if the given Rzk source code is well formatted.
--   This is useful for automation tasks.
isWellFormatted :: String -> Bool
isWellFormatted src = null (formatTextEdits src)

-- | Same as 'isWellFormatted', but reads the source code from a file.
isWellFormattedFile :: FilePath -> IO Bool
isWellFormattedFile path = isWellFormatted <$> readFile path
