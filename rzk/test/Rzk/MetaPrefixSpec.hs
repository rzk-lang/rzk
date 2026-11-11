{-# LANGUAGE OverloadedStrings #-}
-- Meta-prefix fixtures follow https://github.com/rzk-lang/rzk/pull/312.
-- Incomplete-check reporting follows https://github.com/rzk-lang/rzk/pull/364.
module Rzk.MetaPrefixSpec where

import           Free.Scoped (FS (Pure))
import           Language.Rzk.Free.Syntax (TermT', VarIdent)
import qualified Language.Rzk.Syntax as Rzk
import           Rzk.TypeCheck
import           Test.Hspec

type WarningUse = (VarIdent, VarIdent, Int, Int, MetaPrefixRule)

spec :: Spec
spec = describe "Meta-prefix diagnostics" $ do
  it "allows aliases, saturated uses, and schematic arguments to top-level receivers" $
    checkFixture "happy-meta-prefix-plumbing" []
  it "warns when a schema is stored in a pair" $
    checkFixture "warn-meta-prefix-object-position" [("stash", "my-id", 0, 1, MetaPrefixBoth)]
  it "checks source applications before reduction erases the argument" $
    checkFixture "warn-meta-prefix-strict-only" [("local-use", "my-id", 0, 1, MetaPrefixStrictOnly)]
  it "distinguishes structural and strict checking" $
    checkFixture "warn-meta-prefix-option-structural" [("stash", "my-id", 0, 1, MetaPrefixBoth)]
  it "can disable the check" $
    checkFixture "happy-meta-prefix-option-off" []
  it "recomputes prefixes after section generalisation" $
    checkFixture "warn-meta-prefix-section" [("stash", "use-T", 0, 1, MetaPrefixBoth)]
  it "restores strict checking on unset and on leaving a section" $
    checkSource (unlines
      [ "#lang rzk-1"
      , "#def my-id (X : U) (x : X) : X := x"
      , "#section quiet"
      , "#set-option \"warn-meta-prefix\" = \"off\""
      , "#def quiet : Sigma (h : (X : U) -> X -> X), Unit := (my-id, unit)"
      , "#end quiet"
      , "#def outside : Sigma (h : (X : U) -> X -> X), Unit := (my-id, unit)"
      , "#set-option \"warn-meta-prefix\" = \"off\""
      , "#unset-option \"warn-meta-prefix\""
      , "#def reset : Sigma (h : (X : U) -> X -> X), Unit := (my-id, unit)"
      ]) [("outside", "my-id", 0, 1, MetaPrefixBoth), ("reset", "my-id", 0, 1, MetaPrefixBoth)]
  it "checks check and compute commands before reduction" $
    checkSource (unlines
      [ "#lang rzk-1"
      , "#def my-id (X : U) (x : X) : X := x"
      , "#check (my-id, unit) : Sigma (h : (X : U) -> X -> X), Unit"
      , "#compute (\\ (h : (X : U) -> X -> X) -> h Unit unit) my-id"
      , "#compute-nf (\\ (h : (X : U) -> X -> X) -> h Unit unit) my-id"
      ]) [("#check", "my-id", 0, 1, MetaPrefixBoth), ("#compute", "my-id", 0, 1, MetaPrefixStrictOnly), ("#compute-nf", "my-id", 0, 1, MetaPrefixStrictOnly)]
  it "counts ordinary parameters before the last schematic parameter" $
    checkSource (unlines
      [ "#lang rzk-1"
      , "#def my-id (u : Unit) (X : U) (x : X) : X := x"
      , "#def stash : Sigma (h : (X : U) -> X -> X), Unit := (my-id unit, unit)"
      ]) [("stash", "my-id", 1, 2, MetaPrefixBoth)]
  it "checks postulate types and deduplicates repeated endpoint uses" $
    checkSource (unlines
      [ "#lang rzk-1"
      , "#def my-id (X : U) (x : X) : X := x"
      , "#postulate same : my-id =_{((X : U) -> X -> X)} my-id"
      ]) [("same", "my-id", 0, 1, MetaPrefixBoth)]
  it "rejects an invalid sensitivity value" $ do
    let result = defaultTypeCheck (setOption "warn-meta-prefix" "yes" (pure ()) :: TypeCheck VarIdent ())
    case result of
      Left (PlainTypeError err) -> case typeErrorError err of
        TypeErrorOther message -> message `shouldBe`
          "unknown value for \"warn-meta-prefix\" (use \"off\", \"structural\", or \"strict\")"
        _ -> expectationFailure "Wrong error constructor"
      _ -> expectationFailure "Expected an invalid-option error"
  it "reports a failed classification instead of treating it as checked" $ do
    let (result, warnings) = defaultTypeCheckWithWarnings (metaPrefixOf (Pure "unbound" :: TermT'))
    case result of
      Right prefix -> prefix `shouldBe` 0
      Left err -> expectationFailure (ppTypeErrorInScopedContext' BottomUp err)
    case warnings of
      [RSTTIncompleteWarning reason Nothing] -> reason `shouldBe`
        "RSTT meta-prefix classification incomplete: could not inspect a type"
      _ -> expectationFailure "Expected one incomplete-check warning"

checkFixture :: String -> [WarningUse] -> Expectation
checkFixture name expected = readFile ("test/files/" ++ name ++ ".rzk") >>= (`checkSource` expected)

checkSource :: String -> [WarningUse] -> Expectation
checkSource source expected = case Rzk.parseModule source of
  Left err -> expectationFailure ("Parse error: " ++ err)
  Right parsed -> do
    let (result, warnings) = defaultTypeCheckWithWarnings $
          localVerbosity Silent (typecheckModulesWithLocation [("meta-prefix.rzk", parsed)])
    case result of
      Left err -> expectationFailure (ppTypeErrorInScopedContext' BottomUp err)
      Right _ -> case traverse warningUse warnings of
        Left message -> expectationFailure message
        Right uses -> uses `shouldBe` expected
  where
    warningUse (MetaPrefixWarning defName usedName supplied required rule (Just loc))
      | locationFilePath loc == Just "meta-prefix.rzk", Just _ <- locationLine loc =
          Right (defName, usedName, supplied, required, rule)
    warningUse warning = Left ("Unexpected warning or missing location: " ++ ppCheckWarning warning)
