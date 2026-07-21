{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Position conversion at the LSP boundary: code points vs UTF-16 units
-- (see issue #303). An astral-plane character such as @𝕀@ counts as one code
-- point but two UTF-16 units, so every position to its right on the same
-- line differs between the two encodings.
module Rzk.PositionEncodingSpec (spec) where

import           Test.Hspec

#ifdef LSP_ENABLED
import qualified Data.Text                             as T
import           Language.LSP.Protocol.Types           (SemanticTokenAbsolute (..))
import           Language.Rzk.VSCode.PositionEncoding
import           Language.Rzk.VSCode.Tokenize          (tokenizeSyntaxSymbols)

-- | @ab𝕀cd@: code points a0 b1 𝕀2 c3 d4; UTF-16 a0 b1 𝕀2 c4 d5.
astralLine :: T.Text
astralLine = "ab\120128cd"

spec :: Spec
spec = do
  describe "column conversion" $ do
    let als = astralLines astralLine

    it "is the identity before an astral character" $ do
      colToUtf16 als 0 2 `shouldBe` 2
      colFromUtf16 als 0 2 `shouldBe` 2

    it "shifts by one per astral character to the left" $ do
      colToUtf16 als 0 3 `shouldBe` 4
      colToUtf16 als 0 5 `shouldBe` 6
      colFromUtf16 als 0 4 `shouldBe` 3
      colFromUtf16 als 0 6 `shouldBe` 5

    it "maps a column inside a surrogate pair to the character start" $
      colFromUtf16 als 0 3 `shouldBe` 2

    it "keeps the distance past the end of the line" $
      -- The diagnostics code uses column 99 for "to the end of the line".
      colToUtf16 als 0 99 `shouldBe` 100

    it "is the identity on lines without astral characters" $ do
      let alsAscii = astralLines "abcd\nefgh"
      colToUtf16 alsAscii 1 3 `shouldBe` 3
      colFromUtf16 alsAscii 1 3 `shouldBe` 3

    it "round-trips on character boundaries" $ do
      let cols = [0 .. 6]
      [ colFromUtf16 als 0 (colToUtf16 als 0 c) | c <- cols ] `shouldBe` cols

  describe "utf16Length" $
    it "counts an astral character as two units" $
      utf16Length astralLine `shouldBe` 6

  describe "semantic tokens" $ do
    -- @#define f (x : 𝕀) : 𝕀 := x@ — 𝕀 at code points 15 and 20, @:=@ at 22.
    let src = "#lang rzk-1\n#define f (x : \120128) : \120128 := x\n"
        als = astralLines src
        toks = tokensToUtf16 als (tokenizeSyntaxSymbols src)
        spansAt line = [ (_startChar t, _length t) | t <- toks, _line t == line ]

    it "reports the UTF-16 width of an astral token" $
      -- The first 𝕀: same start (nothing astral before it), doubled width.
      spansAt 1 `shouldSatisfy` elem (15, 2)

    it "shifts tokens to the right of an astral character" $ do
      -- The second 𝕀 (code point 20) has one astral character before it,
      -- and @:=@ (code point 22) has two.
      spansAt 1 `shouldSatisfy` elem (21, 2)
      spansAt 1 `shouldSatisfy` elem (24, 2)

    it "does not move tokens on other lines" $
      spansAt 0 `shouldSatisfy` elem (0, 5)   -- #lang
#else
spec :: Spec
spec = describe "position encoding" (pure ())
#endif
