{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Tests for the LSP presentation layer: the merged semantic tokens
-- (AST-based plus lexer-based) and the hover signature formatting.
module Rzk.SemanticTokensSpec (spec) where

import           Test.Hspec

#ifdef LSP_ENABLED
import qualified Data.Text                    as T
import           Language.LSP.Protocol.Types  (SemanticTokenAbsolute (..),
                                               SemanticTokenTypes (..))
import           Language.Rzk.Syntax          (parseModule, parseTerm)
import           Language.Rzk.VSCode.Handlers (formatSignature)
import           Language.Rzk.VSCode.Tokenize (mergeTokens, tokenizeModule,
                                               tokenizeSyntaxSymbols)

-- | Merged tokens of a module, as 'provideSemanticTokens' produces them.
tokensOf :: T.Text -> [SemanticTokenAbsolute]
tokensOf src = case parseModule src of
  Left err -> error ("parse error: " <> T.unpack err)
  Right m  -> mergeTokens (tokenizeModule m) (tokenizeSyntaxSymbols src)

-- | The token type starting at a (0-based) position, if any.
tokenAt :: [SemanticTokenAbsolute] -> (Int, Int) -> Maybe SemanticTokenTypes
tokenAt toks (l, c) = case
  [ _tokenType t
  | t <- toks
  , _line t == fromIntegral l, _startChar t == fromIntegral c
  ] of
    (tt : _) -> Just tt
    []       -> Nothing

exampleModule :: T.Text
exampleModule = T.unlines
  [ "#lang rzk-1"                                 -- 0
  , "#define weird uses (A) (x : A) : A := x"     -- 1
  , "#check TOP : TOPE"                           -- 2
  ]

spec :: Spec
spec = do
  describe "semantic tokens" $ do
    let toks = tokensOf exampleModule
        positions = [ (_line t, _startChar t) | t <- toks ]

    it "are sorted by position, as the LSP delta encoding requires" $
      -- Regression: tokens of a uses-clause used to be emitted before the
      -- declaration name.
      and (zipWith (<) positions (drop 1 positions)) `shouldBe` True

    it "classify commands, keywords, and operators from the lexer" $ do
      tokenAt toks (1, 0)  `shouldBe` Just SemanticTokenTypes_Macro     -- #define
      tokenAt toks (1, 14) `shouldBe` Just SemanticTokenTypes_Keyword   -- uses
      tokenAt toks (1, 26) `shouldBe` Just SemanticTokenTypes_Operator  -- :
      tokenAt toks (1, 35) `shouldBe` Just SemanticTokenTypes_Operator  -- :=

    it "let AST-based tokens win over lexer tokens on overlap" $ do
      tokenAt toks (1, 8) `shouldBe` Just SemanticTokenTypes_Function   -- weird (declaration)
      tokenAt toks (2, 7) `shouldBe` Just SemanticTokenTypes_String     -- TOP (tope literal, not keyword)

    it "skip plain brackets" $
      tokenAt toks (1, 23) `shouldBe` Nothing                           -- (

    it "survive files that do not parse" $
      tokenizeSyntaxSymbols "#lang rzk-1\n#define broken (x : A) :=\n"
        `shouldNotBe` []

    it "mark holes, including named ones" $ do
      let toks' = tokensOf $ T.unlines
            [ "#lang rzk-1"                          -- 0
            , "#define gap (A : U) : A := ?"         -- 1
            , "#define named (A : U) : A := ?goal"   -- 2
            ]
      tokenAt toks' (1, 27) `shouldBe` Just SemanticTokenTypes_Regexp  -- ?
      tokenAt toks' (2, 29) `shouldBe` Just SemanticTokenTypes_Regexp  -- ?goal

    it "mark holes even in files that do not parse" $
      tokenizeSyntaxSymbols "#lang rzk-1\n#define broken (x : A) := ?\n"
        `shouldSatisfy` any ((== SemanticTokenTypes_Regexp) . _tokenType)

  describe "formatSignature" $ do
    let fmt name src = case parseTerm (T.pack src) of
          Left err -> error ("parse error: " <> T.unpack err)
          Right t  -> formatSignature name t

    it "keeps short types on one line" $
      fmt "x" "A -> B" `shouldBe` "x : A -> B"

    it "splits long function types one parameter per line" $
      fmt "long" "(A : U) -> (B : A -> U) -> (C : (x : A) -> B x -> U) -> ((x : A) -> B x) -> U"
        `shouldBe` unlines'
          [ "long"
          , "  : (A : U)"
          , "  → (B : A -> U)"
          , "  → (C : (x : A) -> B x -> U)"
          , "  → ((x : A) -> B x)"
          , "  → U"
          ]

    it "keeps long non-function types on one line" $ do
      let sigma = "Sigma (f : (x : A) -> (y : A) -> hom A x y -> hom A x y), (x : A) -> U"
      fmt "s" sigma `shouldBe` ("s : " <> sigma)
  where
    unlines' = foldr1 (\l r -> l <> "\n" <> r)
#else
spec :: Spec
spec = describe "semantic tokens" (pure ())
#endif
