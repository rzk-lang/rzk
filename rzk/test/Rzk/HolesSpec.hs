{-# LANGUAGE OverloadedStrings #-}
-- | Tests for the structured goal/context query (lenient hole mode). The
-- YAML fixtures cover the strict-mode errors (ill-hole-*); here we check the
-- structured 'HoleInfo' that the game and LSP consume, which substring
-- assertions in the fixture harness cannot pin down (especially the split
-- between term variables and cube variables).
module Rzk.HolesSpec (spec) where

import qualified Data.Text           as T

import qualified Language.Rzk.Syntax as Rzk
import           Rzk.TypeCheck

import           Test.Hspec

-- | Parse and typecheck a module in lenient hole mode, returning the recorded
-- holes. Errors out loudly so a broken fixture is obvious.
holesOf :: T.Text -> [HoleInfo]
holesOf src =
  case Rzk.parseModule src of
    Left err -> error ("parse error: " <> T.unpack err)
    Right m  -> case typecheckModulesWithHoles [("<test>", m)] of
      Left err            -> error ("typecheck threw: " <> ppTypeErrorInScopedContext' BottomUp err)
      Right (_, _, holes) -> holes

names :: [HoleEntry] -> [String]
names = map (show . holeEntryName)

spec :: Spec
spec = do
  describe "typecheckModulesWithHoles (structured goal/context query)" $ do
    it "records a hole's goal and local term context (no name)" $ do
      case holesOf "#lang rzk-1\n#define f : (A : U) -> A -> A\n  := \\ A a -> ?\n" of
        [h] -> do
          holeName h `shouldBe` Nothing
          show (holeGoal h) `shouldBe` "A"
          names (holeTermVars h) `shouldContain` ["A"]
          names (holeTermVars h) `shouldContain` ["a"]
          holeCubeVars h `shouldBe` []
        hs  -> expectationFailure ("expected exactly one hole, got " <> show (length hs))

    it "keeps the user's hole name and splits cube variables from term variables" $ do
      case holesOf "#lang rzk-1\n#define g : (A : U) -> (t : 2) -> A\n  := \\ A t -> ?goal\n" of
        [h] -> do
          holeName h `shouldBe` Just "goal"
          names (holeCubeVars h) `shouldBe` ["t"]
          names (holeTermVars h) `shouldContain` ["A"]
          names (holeTermVars h) `shouldNotContain` ["t"]
        hs  -> expectationFailure ("expected exactly one hole, got " <> show (length hs))

    it "records every hole in a module" $ do
      let holes = holesOf "#lang rzk-1\n#define p : (A : U) -> (B : U) -> A -> B -> A\n  := \\ A B a b -> ?\n"
      length holes `shouldBe` 1
      let holes2 = holesOf "#lang rzk-1\n#define q : (A : U) -> A -> A\n  := \\ A a -> ?\n#define r : (A : U) -> A -> A\n  := \\ A a -> ?\n"
      length holes2 `shouldBe` 2
