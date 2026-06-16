{-# LANGUAGE OverloadedStrings #-}
-- | Tests for the structured goal/context query (lenient hole mode). The
-- YAML fixtures cover the strict-mode errors (ill-hole-*); here we check the
-- structured 'HoleInfo' that the game and LSP consume, which substring
-- assertions in the fixture harness cannot pin down (especially the split
-- between term variables and cube variables).
module Rzk.HolesSpec (spec) where

import           Data.List           (isInfixOf)
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

    -- A hole whose elaborated term reaches unification (here the `refl` endpoint)
    -- must not panic ("unexpected term in UNIFY"); it unifies with anything.
    it "handles a hole that flows into unification" $ do
      case holesOf "#lang rzk-1\n#define t : (A : U) -> (a : A) -> a =_{A} a\n  := \\ A a -> refl_{?}\n" of
        [h] -> show (holeGoal h) `shouldBe` "A"
        hs  -> expectationFailure ("expected exactly one hole, got " <> show (length hs))

    -- A hole nested inside a larger term (`f ?`) checked against an
    -- extension-type boundary: the boundary face is unified against `f ?`, which
    -- must be deferred rather than reported as a mismatch.
    it "handles a nested hole under an extension-type boundary" $ do
      case holesOf "#lang rzk-1\n#define t : (A : U) -> (f : A -> A) -> (a : A) -> (t : 2) -> A [ t === 0_2 |-> a ]\n  := \\ A f a t -> f ?\n" of
        [h] -> show (holeGoal h) `shouldBe` "A"
        hs  -> expectationFailure ("expected exactly one hole, got " <> show (length hs))

    -- A hole used as the argument of a shape-restricted function: the
    -- shape-membership tope (psi ?) mentions the hole and cannot be decided, so
    -- it is deferred rather than reported as TypeErrorTopeNotSatisfied. The goal
    -- is the shape (s : I | psi s), captured in holeGoalShape.
    it "records the shape goal for a hole argument to a shape-restricted function" $ do
      case holesOf "#lang rzk-1\n#define t : (I : CUBE) -> (psi : I -> TOPE) -> (A : U) -> (a : (s : I | psi s) -> A) -> (t : I) -> A\n  := \\ I psi A a t -> a ?\n" of
        [h] -> do
          show (holeGoal h) `shouldBe` "I"
          case holeGoalShape h of
            Just (s, tope) -> do
              show s `shouldBe` "s"
              show tope `shouldBe` "psi s"
            Nothing -> expectationFailure "expected a shape goal (holeGoalShape)"
        hs  -> expectationFailure ("expected exactly one hole, got " <> show (length hs))

    -- An ordinary (non-shape) hole has no shape goal.
    it "leaves holeGoalShape empty for an ordinary hole" $ do
      case holesOf "#lang rzk-1\n#define f : (A : U) -> A -> A\n  := \\ A a -> ?\n" of
        [h] -> holeGoalShape h `shouldBe` Nothing
        hs  -> expectationFailure ("expected exactly one hole, got " <> show (length hs))

    -- A hole checked directly against an extension type shows its boundary in
    -- the goal (the extension type is a real restricted type, carried in
    -- holeGoal — not a shape, so holeGoalShape stays empty).
    it "shows the extension-type boundary in the goal" $ do
      case holesOf "#lang rzk-1\n#define t : (A : U) -> (a : A) -> (t : 2) -> A [ t === 0_2 |-> a ]\n  := \\ A a t -> ?\n" of
        [h] -> do
          holeGoalShape h `shouldBe` Nothing
          let goal = show (holeGoal h)
          ("A [" `isInfixOf` goal) `shouldBe` True   -- a restricted type, not bare A
          ("↦ a" `isInfixOf` goal) `shouldBe` True   -- the boundary face is present
        hs  -> expectationFailure ("expected exactly one hole, got " <> show (length hs))
