{-|
Module      : Formatter
Description : This module defines the formatter for rzk files.

The formatter is designed in a way that can be consumed both by the CLI and the
LSP server.
-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Rzk.Format (
  FormattingEdit (FormattingEdit),
  formatTextEdits,
  format, formatFile, formatFileWrite,
  isWellFormatted, isWellFormattedFile,
) where

import           Data.List               (sort)

import qualified Data.Text               as T
import qualified Data.Text.IO            as T

import           Language.Rzk.Syntax     (resolveLayout,
                                          tryExtractMarkdownCodeBlocks)
import           Language.Rzk.Syntax.Lex (Posn (Pn), Tok (..),
                                          TokSymbol (TokSymbol), Token (PT),
                                          tokens)

-- | All indices are 1-based (as received from the lexer)
-- Note: LSP uses 0-based indices
data FormattingEdit = FormattingEdit Int Int Int Int T.Text
  deriving (Eq, Ord, Show)

-- TODO: more patterns, e.g. for identifiers and literals
pattern Symbol :: T.Text -> Tok
pattern Symbol s <- TK (TokSymbol s _)

pattern Token :: T.Text -> Int -> Int -> Token
pattern Token s line col <- PT (Pn _ line col) (Symbol s)

-- pattern TokenSym :: String -> Int -> Int -> Token
-- pattern TokenSym s line col <- PT (Pn _ line col) (Symbol s)

pattern TokenIdent :: T.Text -> Int -> Int -> Token
pattern TokenIdent s line col <- PT (Pn _ line col) (T_VarIdentToken s)

data FormatState = FormatState
  { parensDepth  :: Int  -- ^ The level of parentheses nesting
  , definingName :: Bool -- ^ After #define, in name or assumptions (to detect the : for the type)
  , lambdaArrow  :: Bool -- ^ After a lambda '\', in the parameters (to leave its -> on the same line)
  , allTokens    :: [Token] -- ^ The full array of tokens after resolving the layout
  }

-- TODO: replace all tabs with 1 space before processing
formatTextEdits :: T.Text -> [FormattingEdit]
formatTextEdits contents =
  case resolveLayout True (tokens rzkBlocks) of
    Left _err     -> [] -- TODO: log error (in a CLI and LSP friendly way)
    Right allToks -> go (initialState {allTokens = allToks}) allToks
  where
    initialState = FormatState { parensDepth = 0, definingName = False, lambdaArrow = False, allTokens = [] }
    incParensDepth s = s { parensDepth = parensDepth s + 1 }
    decParensDepth s = s { parensDepth = 0 `max` (parensDepth s - 1) }
    rzkBlocks = tryExtractMarkdownCodeBlocks "rzk" contents -- TODO: replace tabs with spaces
    contentLines line = T.lines rzkBlocks !! (line - 1) -- Sorry
    lineTokensBefore toks line col = filter isBefore toks
      where
        isBefore (PT (Pn _ l c) _) = l == line && c < col
        isBefore _                 = False
    unicodeTokens :: [(T.Text, T.Text)]
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

    go s (Token "#postulate" _ _ : tks) = go (s {definingName = True}) tks

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
      | precededBySingleCharOnly && spacesAfter /= 1 && not isLastNonSpaceChar
        = FormattingEdit line spaceCol line (spaceCol + spacesAfter) " "
        : go (incParensDepth s) tks
      -- Remove extra spaces if it's not the first open paren on a new line
      | not precededBySingleCharOnly && spacesAfter > 0
        = FormattingEdit line spaceCol line (spaceCol + spacesAfter) ""
        : go (incParensDepth s) tks
      | otherwise = go (incParensDepth s) tks
      -- TODO: Split after 80 chars
      where
        spaceCol = col + 1
        lineContent = contentLines line
        precededBySingleCharOnly = all isPunctuation (lineTokensBefore (allTokens s) line col)
        singleCharUnicodeTokens = filter (\(_, unicode) -> T.length unicode == 1) unicodeTokens
        punctuations :: [T.Text]
        punctuations = concat
          [ map fst singleCharUnicodeTokens -- ASCII sequences will be converted soon
          , map snd singleCharUnicodeTokens
          , ["(", ":", ",", "="]
          ]
        isPunctuation (Token tk _ _) = tk `elem` punctuations
        isPunctuation _              = False
        spacesAfter = T.length $ T.takeWhile (== ' ') (T.drop col lineContent)
        isLastNonSpaceChar = T.all (== ' ') (T.drop col lineContent)

    -- Remove any space before the closing paren
    go s (Token ")" line col : tks)
      = edits ++ go (decParensDepth s) tks
      where
        lineContent = contentLines line
        isFirstNonSpaceChar = T.all (== ' ') (T.take (col - 1) lineContent)
        spacesBefore = T.length $ T.takeWhile (== ' ') (T.reverse $ T.take (col - 1) lineContent)
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
        isFirstNonSpaceChar = T.all (== ' ') (T.take (col - 1) lineContent)
        isLastNonSpaceChar = T.all (== ' ') (T.drop col lineContent)
        spacesBefore = T.length $ T.takeWhile (== ' ') (T.reverse $ T.take (col - 1) lineContent)
        spaceCol = col + 1
        spacesAfter = T.length $ T.takeWhile (== ' ') (T.drop col lineContent)
        typeSepEdits = map snd $ filter fst
          -- Ensure line break before : (and remove any spaces before)
          [ (not isFirstNonSpaceChar, FormattingEdit line (col - spacesBefore) line col "\n  ")
          -- Ensure 2 spaces before : (if already on a new line)
          , (isFirstNonSpaceChar && spacesBefore /= 2, FormattingEdit line 1 line col "  ")
          -- Ensure 1 space after
          , (not isLastNonSpaceChar && spacesAfter /= 1,
              FormattingEdit line spaceCol line (spaceCol + spacesAfter) " ")
          ]
        normalEdits = map snd $ filter fst
          -- 1 space before :
          [ (spacesBefore /= 1, FormattingEdit line (col - spacesBefore) line col " ")
          -- 1 space after
          , (not isLastNonSpaceChar && spacesAfter /= 1,
              FormattingEdit line spaceCol line (spaceCol + spacesAfter) " ")
          ]

    -- Line break before := and one space after
    go s (Token ":=" line col : tks)
      = edits ++ go s tks
      where
        lineContent = contentLines line
        isFirstNonSpaceChar = T.all (== ' ') (T.take (col - 1) lineContent)
        spacesAfter = T.length $ T.takeWhile (== ' ') (T.drop (col + 1) lineContent)
        spacesBefore = T.length $ T.takeWhile (== ' ') (T.reverse $ T.take (col - 1) lineContent)
        edits = map snd $ filter fst
            -- Ensure line break before `:=`
          [ (not isFirstNonSpaceChar, FormattingEdit line (col - spacesBefore) line col "\n  ")
            -- Ensure 2 spaces before `:=` (if already on a new line)
          , (isFirstNonSpaceChar && spacesBefore /= 2,
              FormattingEdit line 1 line col "  ")
            -- Ensure exactly one space after
          , (T.length lineContent > col + 2 && spacesAfter /= 1,
              FormattingEdit line (col + 2) line (col + 2 + spacesAfter) " ")
          ]

    -- One space after \
    go s (Token "\\" line col : tks)
      = edits ++ go (s { lambdaArrow = True }) tks
      where
        lineContent = contentLines line
        spacesAfter = T.length $ T.takeWhile (== ' ') (T.drop col lineContent)
        isLastNonSpaceChar = T.all (== ' ') (T.drop col lineContent)
        spaceCol = col + 1
        edits = map snd $ filter fst
          [ (not isLastNonSpaceChar && spacesAfter /= 1,
              FormattingEdit line spaceCol line (spaceCol + spacesAfter) " ")
          ]

    -- Reset any state necessary after finishing a command
    go s (Token ";" _ _ : tks) = go s tks

    -- One space (or new line) around binary operators and replace ASCII w/ unicode
    go s (Token tk line col : tks) = edits ++ go s' tks
      where
        s' | isArrow = s { lambdaArrow = False } -- reset flag after reaching the arrow
           | otherwise = s
        isArrow = tk `elem` ["->", "→"]
        lineContent = contentLines line
        spacesBefore = T.length $ T.takeWhile (== ' ') (T.reverse $ T.take (col - 1) lineContent)
        spacesAfter = T.length $ T.takeWhile (== ' ') (T.drop (col + T.length tk - 1) lineContent)
        isFirstNonSpaceChar = T.all (== ' ') (T.take (col - 1) lineContent)
        isLastNonSpaceChar = T.all (== ' ') (T.drop (col + T.length tk - 1) lineContent)
        prevLine
          | line > 0 = contentLines (line - 1)
          | otherwise = ""
        nextLine
          | line + 1 < length (T.lines rzkBlocks) = contentLines (line + 1)
          | otherwise = ""
        spacesNextLine = T.length $ T.takeWhile (== ' ') nextLine
        edits = spaceEdits ++ unicodeEdits
        spaceEdits
          | tk `elem` ["->", "→", ",", "*", "×", "="] = concatMap snd $ filter fst
              -- Ensure exactly one space before (unless first char in line, or about to move to next line)
              [ (not isFirstNonSpaceChar && spacesBefore /= 1 && not isLastNonSpaceChar,
                  [FormattingEdit line (col - spacesBefore) line col " "])
              -- Ensure exactly one space after (unless last char in line)
              , (not isLastNonSpaceChar && spacesAfter /= 1,
                  [FormattingEdit line (col + T.length tk) line (col + T.length tk + spacesAfter) " "])
              -- If last char in line, move it to next line (except for lambda arrow)
              , (isLastNonSpaceChar && not (lambdaArrow s),
                  -- This is split into 2 edits to avoid possible overlap with unicode replacement
                  -- 1. Add a new line (with relevant spaces) before the token
                  [ FormattingEdit line (col - spacesBefore) line col $
                      "\n" <> T.replicate (2 `max` (spacesNextLine - (spacesNextLine `min` 2))) " "
                  -- 2. Replace the new line and spaces after the token with a single space
                  , FormattingEdit line (col + T.length tk) (line + 1) (spacesNextLine + 1) " "
                  ])
              -- If lambda -> is first char in line, move it to the previous line
              , (isFirstNonSpaceChar && isArrow && lambdaArrow s,
                  [FormattingEdit (line - 1) (T.length prevLine + 1) line (col + T.length tk + spacesAfter) $
                    " " <> tk <> "\n" <> T.replicate spacesBefore " "])
              ]
          | otherwise = []
        unicodeEdits
          | Just unicodeToken <- tk `lookup` unicodeTokens =
              [ FormattingEdit line col line (col + T.length tk) unicodeToken
              ]
          | otherwise = []

    go s (_:tks) = go s tks

-- Adapted from https://hackage.haskell.org/package/lsp-types-2.1.0.0/docs/Language-LSP-Protocol-Types.html#g:7
applyTextEdit :: FormattingEdit -> T.Text -> T.Text
applyTextEdit (FormattingEdit sl sc el ec newText) oldText =
  let (_, afterEnd) = splitAtPos (el-1, ec -1) oldText
      (beforeStart, _) = splitAtPos (sl-1, sc-1) oldText
   in mconcat [beforeStart, newText, afterEnd]
 where
  splitAtPos :: (Int, Int) -> T.Text -> (T.Text, T.Text)
  splitAtPos (l, c) t = let index = c + startLineIndex l t in T.splitAt index t

  startLineIndex :: Int -> T.Text -> Int
  startLineIndex 0 _ = 0
  startLineIndex line t' =
    case T.findIndex (=='\n') t' of
      Just i  -> i + 1 + startLineIndex (line - 1) (T.drop (i + 1) t')
      Nothing -> T.length t'

applyTextEdits :: [FormattingEdit] -> T.Text -> T.Text
applyTextEdits edits contents = foldr applyTextEdit contents (sort edits)

-- | Format Rzk code, returning the formatted version.
format :: T.Text -> T.Text
format = applyTextEdits =<< formatTextEdits

-- | Format Rzk code from a file
formatFile :: FilePath -> IO T.Text
formatFile path = do
  contents <- T.readFile path
  return (format contents)

-- | Format the file and write the result back to the file.
formatFileWrite :: FilePath -> IO ()
formatFileWrite path = formatFile path >>= T.writeFile path

-- | Check if the given Rzk source code is well formatted.
--   This is useful for automation tasks.
isWellFormatted :: T.Text -> Bool
isWellFormatted src = null (formatTextEdits src)

-- | Same as 'isWellFormatted', but reads the source code from a file.
isWellFormattedFile :: FilePath -> IO Bool
isWellFormattedFile path = do
  contents <- T.readFile path
  return (isWellFormatted contents)
