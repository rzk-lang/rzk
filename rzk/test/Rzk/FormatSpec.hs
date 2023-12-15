{-|
Module      : FormatterSpec
Description : Tests related to the formatter module
-}
module Rzk.FormatSpec where

import           Test.Hspec

import           Rzk.Format (format, isWellFormatted)

formatsTo :: FilePath -> FilePath -> Expectation
formatsTo beforePath afterPath = do
  beforeSrc <- readFile ("test/files/" ++ beforePath)
  afterSrc <- readFile ("test/files/" ++ afterPath)
  format beforeSrc `shouldBe` afterSrc
  isWellFormatted afterSrc `shouldBe` True -- idempotency

formats :: FilePath -> Expectation
formats path = (path ++ "-bad.rzk") `formatsTo` (path ++ "-good.rzk")


spec :: Spec
spec = do
  describe "Formatter" $ do
    it "Puts definition assumptions, conclusion, and construction on separate lines" $ do
      -- formats "definition-structure"
      pendingWith "Doesn't currently place assumptions on a new line"

    it "Replaces common ASCII sequences with their unicode equivalent" $ do
      formats "unicode"

    it "Formats Rzk blocks in Literate Rzk Markdown" $ do
      "literate-bad.rzk.md" `formatsTo` "literate-good.rzk.md"

    it "Preserves comments" $ do
      formats "comments"

    it "Moves trailing binary operators to next line (except lambda arrow)" $ do
      formats "bin-ops"

    it "Adds relevant spaces to structure constructions like a tree" $ do
      formats "tree-structure"

    it "Doesn't fail on empty inputs" $ do
      formats "empty"

    it "Fixes indentation" pending

    it "Wraps long lines" pending
