-- | Conversion between code-point and UTF-16 positions at the LSP boundary.
--
-- LSP positions count UTF-16 code units (the default position encoding, and
-- the only one VS Code supports), while alex positions and everything derived
-- from them (the surface AST, the reference index, the typechecker) count
-- Unicode code points. The two agree except on lines containing astral-plane
-- characters (code points above U+FFFF, such as @𝕀@), each of which takes two
-- UTF-16 units. Emitting a code-point column for such a line shifts every
-- position to the right of the character, and a range boundary can land in
-- the middle of a surrogate pair, which is what shatters the glyph in the
-- editor (see issue #303).
--
-- Everything internal stays in code points; the handlers convert with this
-- module when crossing the wire, in both directions. 'AstralLines' records
-- only the lines where the two encodings differ, so for the common all-BMP
-- document every conversion is an identity after one map lookup.
module Language.Rzk.VSCode.PositionEncoding (
  AstralLines,
  astralLines,
  utf16Length,
  colToUtf16,
  colFromUtf16,
  positionFromUtf16,
  rangeToUtf16,
  tokensToUtf16,
) where

import qualified Data.IntMap.Strict          as IntMap
import qualified Data.Text                   as T
import           Language.LSP.Protocol.Types (Position (Position),
                                              Range (Range),
                                              SemanticTokenAbsolute (..))

-- | The lines of a document on which code-point and UTF-16 columns differ,
-- keyed by 0-based line number.
newtype AstralLines = AstralLines (IntMap.IntMap T.Text)

astralLines :: T.Text -> AstralLines
astralLines src = AstralLines $ IntMap.fromDistinctAscList
  [ (i, line) | (i, line) <- zip [0 ..] (T.lines src), T.any isAstral line ]

isAstral :: Char -> Bool
isAstral c = c > '\xFFFF'

utf16Width :: Char -> Int
utf16Width c = if isAstral c then 2 else 1

-- | The length of a text in UTF-16 code units.
utf16Length :: T.Text -> Int
utf16Length = T.foldl' (\n c -> n + utf16Width c) 0

-- | Convert a 0-based code-point column on the given 0-based line to UTF-16
-- units. A column beyond the end of the line keeps its distance past the end
-- (the diagnostics code uses column 99 to mean "to the end of the line").
colToUtf16 :: AstralLines -> Int -> Int -> Int
colToUtf16 (AstralLines ls) line col =
  case IntMap.lookup line ls of
    Nothing   -> col
    Just text -> utf16Length (T.take col text) + max 0 (col - T.length text)

-- | Convert a 0-based UTF-16 column on the given 0-based line to code
-- points. A column inside a surrogate pair maps to the start of its
-- character.
colFromUtf16 :: AstralLines -> Int -> Int -> Int
colFromUtf16 (AstralLines ls) line col =
  case IntMap.lookup line ls of
    Nothing   -> col
    Just text -> go 0 0 (T.unpack text)
      where
        go cp units (c : cs)
          | units >= col                = cp
          | units + utf16Width c > col  = cp
          | otherwise                   = go (cp + 1) (units + utf16Width c) cs
        go cp units []                  = cp + max 0 (col - units)

-- | Convert an incoming LSP position (UTF-16) to code points.
positionFromUtf16 :: AstralLines -> Position -> Position
positionFromUtf16 als (Position l c) =
  Position l (fromIntegral (colFromUtf16 als (fromIntegral l) (fromIntegral c)))

-- | Convert an outgoing range (code points) to UTF-16.
rangeToUtf16 :: AstralLines -> Range -> Range
rangeToUtf16 als (Range s e) = Range (posToUtf16 s) (posToUtf16 e)
  where
    posToUtf16 (Position l c) =
      Position l (fromIntegral (colToUtf16 als (fromIntegral l) (fromIntegral c)))

-- | Convert semantic tokens (code points) to UTF-16. The length is converted
-- through the token's end column, so a token that itself contains astral
-- characters (e.g. the @𝕀@ keyword) gets its UTF-16 width.
tokensToUtf16 :: AstralLines -> [SemanticTokenAbsolute] -> [SemanticTokenAbsolute]
tokensToUtf16 als@(AstralLines ls) tokens
  | IntMap.null ls = tokens
  | otherwise      = map adjust tokens
  where
    adjust token = token
      { _startChar = fromIntegral start'
      , _length    = fromIntegral (end' - start')
      }
      where
        line   = fromIntegral (_line token)
        start  = fromIntegral (_startChar token)
        start' = colToUtf16 als line start
        end'   = colToUtf16 als line (start + fromIntegral (_length token))
