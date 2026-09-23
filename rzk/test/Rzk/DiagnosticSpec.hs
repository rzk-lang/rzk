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
      Left err -> [diagnoseTypeError BottomUp err]
      Right (checked, holes) ->
        map (diagnoseTypeError BottomUp) (checkedErrors checked) ++ map diagnoseHole holes

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

    it "marks ordinary identity context entries as available without modal noise" $ do
      case diagnose "#lang rzk-1\n#define g : (A : U) -> A -> A\n  := \\ A a -> ?\n" of
        [d] -> do
          diagnosticMessage d `shouldSatisfy` isInfixOf "    + A : U"
          diagnosticMessage d `shouldSatisfy` isInfixOf "    + a : A"
          diagnosticMessage d `shouldNotSatisfy` isInfixOf "[modality:"
        ds  -> expectationFailure ("expected one diagnostic, got " <> show (length ds))

    it "marks inaccessible modal term and cube variables" $ do
      let src = "#lang rzk-1\n\
                \#def g (A : U) (B :_op U) (i :_op 2) : U := ?\n"
      case diagnose src of
        [d] -> do
          diagnosticMessage d `shouldSatisfy` isInfixOf "    + A : U"
          diagnosticMessage d `shouldSatisfy`
            isInfixOf "    - B : U  [modality: ᵒᵖ, locks: _id]"
          diagnosticMessage d `shouldSatisfy`
            isInfixOf "    - i : 2  [modality: ᵒᵖ, locks: _id]"
        ds  -> expectationFailure ("expected one diagnostic, got " <> show (length ds))

    it "keeps and marks a tope that is unavailable under a modal lock" $ do
      let src = "#lang rzk-1\n\
                \#def g : (i : 2 | i === 0_2) -> _op Unit\n\
                \  := \\ i -> mod _op ?\n"
      case diagnose src of
        [d] -> do
          diagnosticMessage d `shouldSatisfy`
            isInfixOf "    - i : 2  [modality: _id, locks: ᵒᵖ]"
          diagnosticMessage d `shouldSatisfy`
            isInfixOf "    - i ≡ 0₂  [modality: _id, locks: ᵒᵖ]"
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
            map holeDataEntryName (holeDataCubeVars hd) `shouldContain` ["(t, s)"]
            map holeDataTopeValue (holeDataTopes hd) `shouldContain` ["s \8804 t"] -- s ≤ t
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
          ("\"tope\":" `isInfixOf` json) `shouldBe` True
          ("\"modality\":" `isInfixOf` json) `shouldBe` True
          ("\"locks\":" `isInfixOf` json) `shouldBe` True
          ("\"accessible\":" `isInfixOf` json) `shouldBe` True
          -- restored binder names are present in the wire format
          ("(t, s)" `isInfixOf` json) `shouldBe` True
        ds  -> expectationFailure ("expected one diagnostic, got " <> show (length ds))

  describe "diagnoseTypeError modal context" $ do
    it "marks available and unavailable variables in an error context" $ do
      let src = "#lang rzk-1\n\
                \#def bad (A : U) (B :_op U) : U := B\n"
      case diagnose src of
        [d] -> do
          diagnosticMessage d `shouldSatisfy` isInfixOf "+ A : U"
          diagnosticMessage d `shouldSatisfy`
            isInfixOf "- B : U  [modality: ᵒᵖ, locks: _id]"
          diagnosticMessage d `shouldSatisfy`
            isInfixOf "variable B is inaccessible in the current modal context"
          diagnosticMessage d `shouldSatisfy`
            isInfixOf "declared modality: ᵒᵖ"
          diagnosticMessage d `shouldSatisfy`
            isInfixOf "current locks:      _id"
        ds  -> expectationFailure ("expected one diagnostic, got " <> show (length ds))

    it "distinguishes lambda and modal-introduction mismatches" $ do
      let lambdaSrc = "#lang rzk-1\n\
                      \#def bad : (A : U) -> A -> A := \\ (A :_op U) a -> a\n"
          introSrc = "#lang rzk-1\n\
                     \#def bad (A :_b U) (a :_b A) : _b A := mod _op a\n"
      case (diagnose lambdaSrc, diagnose introSrc) of
        ([lambdaErr], [introErr]) -> do
          diagnosticCode lambdaErr `shouldBe` "TypeErrorModalityMismatch"
          diagnosticMessage lambdaErr `shouldSatisfy`
            isInfixOf "modality mismatch in lambda parameter A"
          diagnosticMessage lambdaErr `shouldSatisfy`
            isInfixOf "expected from function type: _id"
          diagnosticMessage introErr `shouldSatisfy`
            isInfixOf "modality mismatch in modal introduction"
          diagnosticMessage introErr `shouldSatisfy`
            isInfixOf "expected by result type: ♭"
          diagnosticMessage introErr `shouldSatisfy`
            isInfixOf "written after `mod`:     ᵒᵖ"
        result -> expectationFailure ("expected one diagnostic for each source, got " <> show result)

    it "reports the value and type for a non-modal let-mod elimination" $ do
      let src = "#lang rzk-1\n\
                \#def bad (A : U) (x : A) : A := let mod _# y := x in y\n"
      case diagnose src of
        [d] -> do
          diagnosticCode d `shouldBe` "TypeErrorNotModal"
          diagnosticMessage d `shouldSatisfy`
            isInfixOf "cannot eliminate a non-modal value with `let mod ♯`"
          diagnosticMessage d `shouldSatisfy` isInfixOf "value: x"
          diagnosticMessage d `shouldSatisfy` isInfixOf "type:  A"
        ds  -> expectationFailure ("expected one diagnostic, got " <> show (length ds))

    it "reports the term and expected type for modal introduction" $ do
      case diagnose "#lang rzk-1\n#def bad : Unit := mod _op unit\n" of
        [d] -> do
          diagnosticCode d `shouldBe` "TypeErrorNotModal"
          diagnosticMessage d `shouldSatisfy`
            isInfixOf "cannot check modal introduction against a non-modal type"
          diagnosticMessage d `shouldSatisfy` isInfixOf "term: mod ᵒᵖ unit"
          diagnosticMessage d `shouldSatisfy` isInfixOf "expected type: Unit"
        ds  -> expectationFailure ("expected one diagnostic, got " <> show (length ds))

    it "reports the expected and actual modalities for let-mod elimination" $ do
      let src = "#lang rzk-1\n\
                \#def bad (A :_op U) (x : _op A) : _op A\n\
                \  := let mod _# a := x in mod _op a\n"
      case diagnose src of
        [d] -> do
          diagnosticCode d `shouldBe` "TypeErrorModalityMismatch"
          diagnosticMessage d `shouldSatisfy`
            isInfixOf "modality mismatch in modal elimination"
          diagnosticMessage d `shouldSatisfy`
            isInfixOf "expected by `let mod`: ♯"
          diagnosticMessage d `shouldSatisfy`
            isInfixOf "value's modality:     ᵒᵖ"
        ds  -> expectationFailure ("expected one diagnostic, got " <> show (length ds))

    it "reports the operand and its type when a modal type is ill-formed" $ do
      case diagnose "#lang rzk-1\n#def bad : U := _op unit\n" of
        [d] -> do
          diagnosticCode d `shouldBe` "TypeErrorNotTypeInModal"
          diagnosticMessage d `shouldSatisfy` isInfixOf "cannot form modal type ᵒᵖ unit"
          diagnosticMessage d `shouldSatisfy` isInfixOf "has type\n  Unit"
          diagnosticMessage d `shouldSatisfy`
            isInfixOf "but its type must be U, CUBE, or TOPE"
        ds  -> expectationFailure ("expected one diagnostic, got " <> show (length ds))

    it "uses the structured modality error when unifying function types" $ do
      let src = "#lang rzk-1\n\
                \#postulate f : ((X : U) -> U) -> Unit\n\
                \#postulate g : (X :_op U) -> U\n\
                \#def bad : Unit := f g\n"
      case diagnose src of
        [d] -> do
          diagnosticCode d `shouldBe` "TypeErrorModalityMismatch"
          diagnosticMessage d `shouldSatisfy`
            isInfixOf "cannot unify function types with different parameter modalities"
          diagnosticMessage d `shouldSatisfy` isInfixOf "expected: _id"
          diagnosticMessage d `shouldSatisfy` isInfixOf "actual:   ᵒᵖ"
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
