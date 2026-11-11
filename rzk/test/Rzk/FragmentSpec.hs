{-# LANGUAGE OverloadedStrings #-}
-- Fragment fixtures follow https://github.com/rzk-lang/rzk/pull/364.
module Rzk.FragmentSpec where

import           Control.Monad (forM_)
import           Data.List (isInfixOf)
import           Free.Scoped (FS (Pure))
import           Language.Rzk.Free.Syntax (TermT', VarIdent)
import qualified Language.Rzk.Syntax as Rzk
import           Rzk.TypeCheck
import           Test.Hspec

data Finding
  = Restriction VarIdent FragmentUse
  | Binder VarIdent VarIdent
  | ShapeDependency String
  deriving (Eq, Show)

spec :: Spec
spec = describe "Standalone fragment diagnostics" $ do
  it "distinguishes assumed restriction positions" $
    fixture "warn-free-standing-restriction-assumed"
      [ Restriction "at-binder" UseBinder
      , Restriction "at-motive" UseMotive
      , Restriction "at-data" UseData
      , Restriction "at-data" UseBinder
      , Restriction "at-universe" UseData
      , Restriction "at-identity" UseBinder
      , Restriction "at-identity" UseIdentity
      ]
  it "defaults restrictions off and restores that default on unset" $
    fixture "warn-free-standing-restriction-option" [Restriction "reported" UseData]
  it "allows parameter prefixes and reports schematic binders inside terms" $
    fixture "warn-meta-binder-inside-term"
      [Binder "at-universe" "B", Binder "at-cube" "J", Binder "at-tope-family" "φ"]
  it "allows extension types and restrictions along concluded codomains" $
    fixture "happy-ext-style-restrictions" []
  it "allows independent cube domains" $
    fixture "happy-rstt-independent-cubes" []
  it "allows schematic arguments and ordinary functions in pairs" $
    fixture "happy-rstt-schema-parameters" []
  it "reports dependencies in faces, domains, patterns, and inferred lambda types" $
    fixture "warn-rstt-shape-dependency"
      [ ShapeDependency "restriction boundary"
      , ShapeDependency "shape domain"
      , ShapeDependency "restriction boundary"
      , ShapeDependency "restriction boundary"
      , ShapeDependency "shape domain"
      ]
  it "reports a cube domain depending on an outer point" $
    fixture "warn-rstt-dependent-cube" [ShapeDependency "cube domain"]
  it "reports an assumed boundary depending on an outer point" $
    fixture "warn-rstt-shape-boundary" [ShapeDependency "restriction boundary"]
  it "checks binders in faces before beta reduction" $
    fixture "warn-rstt-meta-binder-in-face" [Binder "missed" "B"]
  forM_ [("beta", "beta"), ("pair", "via-pair"), ("tope-family", "family")] $ \(suffix, name) ->
    it ("finds restrictions hidden by " ++ suffix) $
      fixture ("warn-rstt-escape-" ++ suffix) [Restriction name UseData]
  it "checks section assumptions before generalisation" $
    fixture "warn-rstt-escape-section" [Restriction "x" UseBinder]
  it "checks restrictions introduced through type aliases" $
    fixture "warn-rstt-escape-unfolding"
      [Restriction "restricted" UseData, Restriction "use" UseBinder]
  it "checks check and compute commands before reduction" $
    checkSource (unlines
      [ "#lang rzk-1"
      , "#set-option \"warn-free-standing-restriction\" = \"yes\""
      , "#check Unit [TOP |-> unit] : U"
      , "#compute (\\ (X : U) -> Unit) (Unit [TOP |-> unit])"
      , "#compute-nf first ((Unit [TOP |-> unit], unit) as Sigma (X : U), Unit)"
      ]) [Restriction "#check" UseData, Restriction "#compute" UseData, Restriction "#compute-nf" UseData]
  it "restores standalone switches after a section and on unset" $
    checkSource (unlines
      [ "#lang rzk-1"
      , "#def quiet (s : 2) : Unit := (\\ (t : 2 | t <= s) -> unit) 0_2"
      , "#def quiet-meta : Unit := (\\ (A : U) -> unit) Unit"
      , "#section warnings"
      , "#set-option \"warn-shape-dependency\" = \"yes\""
      , "#set-option \"warn-meta-binder\" = \"yes\""
      , "#def warned (s : 2) : Unit := (\\ (t : 2 | t <= s) -> unit) 0_2"
      , "#def warned-meta : Unit := (\\ (A : U) -> unit) Unit"
      , "#end warnings"
      , "#def quiet-again (s : 2) : Unit := (\\ (t : 2 | t <= s) -> unit) 0_2"
      , "#def quiet-meta-again : Unit := (\\ (A : U) -> unit) Unit"
      , "#set-option \"warn-shape-dependency\" = \"yes\""
      , "#set-option \"warn-meta-binder\" = \"yes\""
      , "#unset-option \"warn-shape-dependency\""
      , "#unset-option \"warn-meta-binder\""
      , "#def reset (s : 2) : Unit := (\\ (t : 2 | t <= s) -> unit) 0_2"
      , "#def reset-meta : Unit := (\\ (A : U) -> unit) Unit"
      ]) [ShapeDependency "shape domain", Binder "warned-meta" "A"]
  it "can explicitly disable all three diagnostics" $
    checkSource (unlines
      [ "#lang rzk-1"
      , "#set-option \"warn-free-standing-restriction\" = \"yes\""
      , "#set-option \"warn-shape-dependency\" = \"yes\""
      , "#set-option \"warn-meta-binder\" = \"yes\""
      , "#set-option \"warn-free-standing-restriction\" = \"no\""
      , "#set-option \"warn-shape-dependency\" = \"no\""
      , "#set-option \"warn-meta-binder\" = \"no\""
      , "#def restricted : U := Unit [TOP |-> unit]"
      , "#def dependent (s : 2) : Unit := (\\ (t : 2 | t <= s) -> unit) 0_2"
      , "#def inner-meta : Unit := (\\ (A : U) -> unit) Unit"
      ]) []
  forM_ ["warn-free-standing-restriction", "warn-shape-dependency", "warn-meta-binder"] $ \option ->
    it ("rejects an invalid value for " ++ option) $ do
      let result = defaultTypeCheck (setOption option "invalid" (pure ()) :: TypeCheck VarIdent ())
      case result of
        Left (PlainTypeError err) -> case typeErrorError err of
          TypeErrorOther message -> message `shouldBe`
            "unknown value for \"" ++ option ++ "\" (use \"yes\" or \"no\")"
          _ -> expectationFailure "Wrong error constructor"
        _ -> expectationFailure "Expected an invalid-option error"
  it "reports an incomplete inspection and deduplicates the two views" $ do
    let (result, warnings) = defaultTypeCheckWithWarnings $
          localWarnFreeStandingRestriction True $
            recordFragmentUses "broken" (Pure "unbound" :: TermT') Nothing False
    case result of
      Right () -> pure ()
      Left err -> expectationFailure (ppTypeErrorInScopedContext' BottomUp err)
    case warnings of
      [RSTTIncompleteWarning reason Nothing] -> reason `shouldBe`
        "RSTT fragment check incomplete in broken: could not inspect a type"
      _ -> expectationFailure "Expected one incomplete-check warning"

fixture :: String -> [Finding] -> Expectation
fixture name expected = readFile ("test/files/fragment-" ++ name ++ ".rzk") >>= (`checkSource` expected)

checkSource :: String -> [Finding] -> Expectation
checkSource source expected = case Rzk.parseModule source of
  Left err -> expectationFailure ("Parse error: " ++ err)
  Right parsed -> do
    let (result, warnings) = defaultTypeCheckWithWarnings $
          localVerbosity Silent (typecheckModulesWithLocation [("fragment.rzk", parsed)])
    case result of
      Left err -> expectationFailure (ppTypeErrorInScopedContext' BottomUp err)
      Right _ -> case traverse finding warnings of
        Left message -> expectationFailure message
        Right found -> found `shouldBe` expected
  where
    finding (FreeStandingRestrictionWarning name _ use loc) | located loc = Right (Restriction name use)
    finding (MetaBinderWarning name binder _ loc) | located loc = Right (Binder name binder)
    finding (ShapeDependencyWarning message loc) | located loc =
      case [part | part <- ["cube domain", "shape domain", "restriction boundary"], part `isInfixOf` message] of
        [part] | "outer cube points:" `isInfixOf` message -> Right (ShapeDependency part)
        _ -> Left ("Unexpected shape warning: " ++ message)
    finding warning = Left ("Unexpected warning or missing location: " ++ ppCheckWarning warning)
    located (Just loc) = locationFilePath loc == Just "fragment.rzk" && locationLine loc /= Nothing
    located Nothing = False
