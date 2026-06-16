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
    it "produces a hint-severity diagnostic carrying the goal" $ do
      case diagnose "#lang rzk-1\n#define g : (A : U) -> A -> A\n  := \\ A a -> ?goal\n" of
        [d] -> do
          diagnosticSeverity d `shouldBe` SeverityHint
          diagnosticCode d `shouldBe` "hole"
          ("goal" `isInfixOf` diagnosticMessage d) `shouldBe` True
        ds  -> expectationFailure ("expected one diagnostic, got " <> show (length ds))

  describe "JSON encoding" $ do
    it "encodes severity, code and message" $ do
      case diagnose "#lang rzk-1\n#check U U : U\n" of
        [d] -> do
          let json = BL8.unpack (encode d)
          ("\"severity\":\"error\"" `isInfixOf` json) `shouldBe` True
          ("\"code\":\"TypeErrorNotFunction\"" `isInfixOf` json) `shouldBe` True
        ds  -> expectationFailure ("expected one diagnostic, got " <> show (length ds))
