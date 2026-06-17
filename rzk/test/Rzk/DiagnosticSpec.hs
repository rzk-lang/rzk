{-# LANGUAGE OverloadedStrings #-}
-- | Tests for structured diagnostics (Rzk.Diagnostic): the severity/code/
-- location derived from a type error or a hole, and the JSON encoding consumed
-- by `rzk typecheck --json`.
module Rzk.DiagnosticSpec (spec) where

import           Data.Aeson                 (encode)
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.List                  (isInfixOf)
import qualified Data.Text                  as T

import qualified Language.Rzk.Syntax        as Rzk
import           Rzk.Diagnostic
import           Rzk.TypeCheck

import           Test.Hspec

-- | All diagnostics for a module, in lenient hole mode (type errors as errors,
-- holes as hints) — the same set `rzk typecheck --json` emits.
diagnose :: T.Text -> [Diagnostic]
diagnose src =
  case Rzk.parseModule src of
    Left err -> error ("parse error: " <> T.unpack err)
    Right m  -> case typecheckModulesWithHoles [("<test>", m)] of
      Left err                 -> [diagnoseTypeError BottomUp err]
      Right (_, errors, holes) ->
        map (diagnoseTypeError BottomUp) errors ++ map diagnoseHole holes

spec :: Spec
spec = do
  describe "diagnoseTypeError" $ do
    it "produces an error-severity diagnostic with a stable code and a line" $ do
      case diagnose "#lang rzk-1\n#check U U : U\n" of
        [d] -> do
          diagnosticSeverity d `shouldBe` SeverityError
          diagnosticCode d `shouldBe` "TypeErrorNotFunction"
          (diagnosticLocation d >>= locationLine) `shouldBe` Just 2
        ds  -> expectationFailure ("expected one diagnostic, got " <> show (length ds))

  describe "diagnoseHole" $ do
    it "produces a warning-severity diagnostic carrying the goal" $ do
      case diagnose "#lang rzk-1\n#define g : (A : U) -> A -> A\n  := \\ A a -> ?goal\n" of
        [d] -> do
          diagnosticSeverity d `shouldBe` SeverityWarning
          diagnosticCode d `shouldBe` "hole"
          ("goal" `isInfixOf` diagnosticMessage d) `shouldBe` True
        ds  -> expectationFailure ("expected one diagnostic, got " <> show (length ds))

  -- The structured hole payload (Rzk.Diagnostic.HoleData) exposes the goal and
  -- local context as separate rendered strings, so consumers (e.g. richer LSP
  -- hovers) need not parse the prose `message`. The pair-pattern example is
  -- reused from Rzk.HolesSpec so these assertions double as a regression that the
  -- restored binder names (t / s, not π₁ / π₂) survive into the JSON.
  describe "diagnoseHole structured payload" $ do
    let pairPattern =
          "#lang rzk-1\n\
          \#define test : (A : U) -> (x : A) -> ( (t , s) : 2 * 2 | s <= t ) -> A [ t === s |-> x ]\n\
          \  := \\ A x (t , s) -> ?\n"
    it "carries the goal, cube variables and topes as separate fields" $ do
      case diagnose pairPattern of
        [d] -> case diagnosticHole d of
          Just hd -> do
            holeDataName hd `shouldBe` Nothing
            holeDataShape hd `shouldBe` Nothing
            ("t \8801 s" `isInfixOf` holeDataGoal hd) `shouldBe` True   -- t ≡ s
            map fst (holeDataCubeVars hd) `shouldContain` ["(t, s)"]
            holeDataTopes hd `shouldContain` ["s \8804 t"]              -- s ≤ t
          Nothing -> expectationFailure "expected a structured hole payload"
        ds  -> expectationFailure ("expected one diagnostic, got " <> show (length ds))

    it "encodes the structured hole object in JSON" $ do
      case diagnose pairPattern of
        [d] -> do
          let json = BL8.unpack (encode d)
          ("\"code\":\"hole\"" `isInfixOf` json) `shouldBe` True
          ("\"hole\":{" `isInfixOf` json) `shouldBe` True
          ("\"goal\":" `isInfixOf` json) `shouldBe` True
          ("\"cubeVars\":" `isInfixOf` json) `shouldBe` True
          ("\"topes\":" `isInfixOf` json) `shouldBe` True
          -- restored binder names are present in the wire format
          ("(t, s)" `isInfixOf` json) `shouldBe` True
        ds  -> expectationFailure ("expected one diagnostic, got " <> show (length ds))

  describe "JSON encoding" $ do
    it "encodes severity, code and message" $ do
      case diagnose "#lang rzk-1\n#check U U : U\n" of
        [d] -> do
          let json = BL8.unpack (encode d)
          ("\"severity\":\"error\"" `isInfixOf` json) `shouldBe` True
          ("\"code\":\"TypeErrorNotFunction\"" `isInfixOf` json) `shouldBe` True
        ds  -> expectationFailure ("expected one diagnostic, got " <> show (length ds))

    it "emits a null hole field for a type-error diagnostic" $ do
      case diagnose "#lang rzk-1\n#check U U : U\n" of
        [d] -> do
          diagnosticHole d `shouldBe` Nothing
          ("\"hole\":null" `isInfixOf` BL8.unpack (encode d)) `shouldBe` True
        ds  -> expectationFailure ("expected one diagnostic, got " <> show (length ds))
