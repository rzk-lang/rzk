{-# LANGUAGE OverloadedStrings #-}
module Rzk.RSTTSafeSpec (spec) where

import qualified Data.Text as T
import qualified Language.Rzk.Syntax as Syntax
import Rzk.Diagnostic (checkWarningTag, typeErrorTagInScopedContext)
import Rzk.TypeCheck
import Test.Hspec

-- Exercise the public driver used by the CLI and incremental editor clients.
-- The source fixtures cover the individual fragment conditions.
run :: Maybe RSTTSafeMode -> [T.Text] -> ([String], [String], Int)
run override sources =
  let (errors, warnings, holes) = runDetails override sources
  in (errors, map checkWarningTag warnings, holes)

runDetails :: Maybe RSTTSafeMode -> [T.Text] -> ([String], [CheckWarning], Int)
runDetails override sources =
  case traverse Syntax.parseModule sources of
    Left err -> error (T.unpack err)
    Right modules -> case checkedModules (zip (map show [1 :: Int ..]) modules)
      ((allowHoles emptyContext) { ctxRSTTSafeOverride = override, ctxVerbosity = Silent }) of
        Left err -> ([typeErrorTagInScopedContext err], [], 0)
        Right (checked, holes) ->
          ( map typeErrorTagInScopedContext (checkedErrors checked)
          , checkedWarnings checked
          , length holes
          )

nested :: T.Text
nested = "#def nested (A : U) (a : A) : A := (\\ (B : U) (b : B) → b) A a\n"

spec :: Spec
spec = describe "RSTT-safe run policy" $ do
  it "keeps a CLI error selection despite source attempts to disable checks" $ do
    let (errors, _, _) = run (Just RSTTSafeError)
          ["#lang rzk-1\n#set-option \"rstt-safe\" = \"off\"\n#set-option \"warn-meta-binder\" = \"no\"\n" <> nested]
    errors `shouldBe` ["TypeErrorRSTT"]
  it "keeps a CLI warning selection despite a source error selection" $ do
    let (errors, warnings, _) = run (Just RSTTSafeWarn)
          ["#lang rzk-1\n#set-option \"rstt-safe\" = \"error\"\n" <> nested]
    errors `shouldBe` []
    warnings `shouldBe` ["MetaBinderWarning"]
  it "applies the CLI selection to subsequent modules" $ do
    let (errors, _, _) = run (Just RSTTSafeError)
          ["#lang rzk-1\n#def id (A : U) (a : A) : A := a\n",
           "#lang rzk-1\n#set-option \"rstt-safe\" = \"off\"\n" <> nested]
    errors `shouldBe` ["TypeErrorRSTT"]
  it "keeps restriction checks enabled by the CLI despite source options" $ do
    let source = "#lang rzk-1\n#set-option \"rstt-safe\" = \"off\"\n#set-option \"warn-free-standing-restriction\" = \"no\"\n#postulate restricted (A : U) (a : A) : A [TOP ↦ a]\n"
        (errors, _, _) = run (Just RSTTSafeError) [source]
        (warnErrors, warnings, _) = run (Just RSTTSafeWarn) [source]
    errors `shouldBe` ["TypeErrorRSTT"]
    warnErrors `shouldBe` []
    warnings `shouldBe` ["FreeStandingRestrictionWarning"]
  it "keeps restriction checks enabled by source safe mode" $ do
    let (errors, _, _) = run Nothing
          ["#lang rzk-1\n#set-option \"rstt-safe\" = \"error\"\n#set-option \"warn-free-standing-restriction\" = \"no\"\n#postulate restricted (A : U) (a : A) : A [TOP ↦ a]\n"]
    errors `shouldBe` ["TypeErrorRSTT"]
  it "restores standalone restriction settings when safe mode is turned off" $ do
    run Nothing
      ["#lang rzk-1\n#set-option \"warn-free-standing-restriction\" = \"no\"\n#set-option \"rstt-safe\" = \"warn\"\n#set-option \"rstt-safe\" = \"off\"\n#postulate restricted (A : U) (a : A) : A [TOP ↦ a]\n"]
      `shouldBe` ([], [], 0)
  it "resets the standalone binder default even while safe mode is active" $ do
    run Nothing
      ["#lang rzk-1\n#set-option \"warn-meta-binder\" = \"yes\"\n#unset-option \"warn-meta-binder\"\n#set-option \"rstt-safe\" = \"off\"\n" <> nested]
      `shouldBe` ([], [], 0)
  it "rejects a hole even when the API allows unfinished terms" $ do
    let (errors, warnings, count) = run (Just RSTTSafeError)
          ["#lang rzk-1\n#def unfinished : Unit := ?\n"]
    errors `shouldBe` ["TypeErrorRSTT"]
    warnings `shouldBe` ["RSTTHoleWarning"]
    count `shouldBe` 1
  it "reports unfinished obligations without failing in warning mode" $ do
    run (Just RSTTSafeWarn) ["#lang rzk-1\n#def unfinished : Unit := ?\n"]
      `shouldBe` ([], ["RSTTHoleWarning"], 1)

  it "locates unsupported syntax below binders before it can reduce away" $ do
    let source = "#lang rzk-1\n#def discard (i : 2) : Unit :=\n  (\\ (j : 2) → unit) (sup i 1_2)\n"
        (errors, warnings, _) = runDetails (Just RSTTSafeWarn) [source]
    errors `shouldBe` []
    map checkWarningTag warnings `shouldBe` ["RSTTSyntaxWarning"]
    map warningLocation warnings `shouldBe`
      [Just (LocationInfo (Just "1") (Just 3) (Just 23))]
  it "checks standalone commands as well as declarations" $ do
    let (errors, warnings, _) = run (Just RSTTSafeError)
          ["#lang rzk-1\n#check (\\ (j : 2) → unit) (sup 0_2 1_2) : Unit\n"]
    errors `shouldBe` ["TypeErrorRSTT"]
    warnings `shouldBe` ["RSTTSyntaxWarning"]
  it "locates modal syntax inside an otherwise allowed term" $ do
    let (errors, warnings, _) = runDetails Nothing
          ["#lang rzk-1\n#def modal : U :=\n  (_# Unit)\n"]
    errors `shouldBe` []
    map checkWarningTag warnings `shouldBe` ["RSTTModalWarning"]
    map warningLocation warnings `shouldBe`
      [Just (LocationInfo (Just "1") (Just 3) (Just 4))]
  it "locates match before elaboration turns it into an eliminator" $ do
    let (errors, warnings, _) = runDetails Nothing
          ["#lang rzk-1\n#data bool := false | true\n#def choose (b : bool) : bool :=\n  match b (false ⇒ true | true ⇒ false)\n"]
    errors `shouldBe` []
    map checkWarningTag warnings `shouldBe`
      ["RSTTInductiveWarning", "RSTTInductiveWarning"]
    map warningLocation warnings `shouldBe`
      [ Just (LocationInfo (Just "1") (Just 2) (Just 1))
      , Just (LocationInfo (Just "1") (Just 4) (Just 3)) ]
  it "still rejects direct constructor uses when safe mode is re-enabled" $ do
    let (errors, warnings, _) = runDetails Nothing
          ["#lang rzk-1\n#set-option \"rstt-safe\" = \"off\"\n#data bool := false | true\n#set-option \"rstt-safe\" = \"warn\"\n#def yes : bool := true\n"]
    errors `shouldBe` []
    map checkWarningTag warnings `shouldBe` ["RSTTInductiveWarning"]
    map warningLocation warnings `shouldBe`
      [Just (LocationInfo (Just "1") (Just 5) (Just 20))]
  it "allows local binders that shadow constructor names" $ do
    run Nothing
      ["#lang rzk-1\n#set-option \"rstt-safe\" = \"off\"\n#data bool := false | true\n#set-option \"rstt-safe\" = \"warn\"\n#def shadow (false : Unit) : Unit := false\n"]
      `shouldBe` ([], [], 0)
