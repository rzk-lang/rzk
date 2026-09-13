{-# LANGUAGE OverloadedStrings #-}
module Rzk.RSTTSafeSpec (spec) where

import qualified Data.Text as T
import qualified Language.Rzk.Syntax as Syntax
import Rzk.Diagnostic (checkWarningTag, typeErrorTagInScopedContext)
import Rzk.TypeCheck
import Rzk.TypeCheck.MetaPrefix (isMetaType, metaPrefixOf)
import Language.Rzk.Foil.Syntax (typeAscT, universeT)
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
  it "reports exhausted meta-prefix classification in warn and error modes" $ do
    -- A valid type ascription needs a head reduction; force that inspection
    -- to reach the judgement limit without an enormous source fixture.
    let inspect mode = runTypeCheckWith
          (emptyContext { ctxRSTTSafeOverride = Just mode
                        , ctxVerbosity = Silent
                        , ctxActionStackDepth = maxActionStackDepth })
          (metaPrefixOf (typeAscT universeT universeT))
        (warnResult, (_, warnWarnings)) = inspect RSTTSafeWarn
        (errorResult, (_, errorWarnings)) = inspect RSTTSafeError
    either (const Nothing) Just warnResult `shouldBe` Just 0
    map checkWarningTag warnWarnings `shouldBe` ["RSTTIncompleteWarning"]
    either (Just . typeErrorTagInScopedContext) (const Nothing) errorResult
      `shouldBe` Just "TypeErrorRSTT"
    map checkWarningTag errorWarnings `shouldBe` ["RSTTIncompleteWarning"]

  it "does not classify an inspection failure as an object type" $ do
    let (result, _) = runTypeCheckWith
          (emptyContext { ctxVerbosity = Silent
                        , ctxActionStackDepth = maxActionStackDepth })
          (isMetaType (typeAscT universeT universeT))
    either (Just . typeErrorTagInScopedContext) (const Nothing) result
      `shouldBe` Just "TypeErrorOther"

  it "classifies a reducible universe when inspection succeeds" $ do
    let (result, (_, warnings)) = runTypeCheckWith
          (emptyContext { ctxVerbosity = Silent })
          (isMetaType (typeAscT universeT universeT))
    either (const Nothing) Just result `shouldBe` Just True
    map checkWarningTag warnings `shouldBe` []

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
  it "keeps outer-point dependency checks enabled by the CLI" $ do
    let source = "#lang rzk-1\n#set-option \"rstt-safe\" = \"off\"\n#set-option \"warn-shape-dependency\" = \"no\"\n#postulate mixed (A : U) (a : A) (s : 2) (f : (t : 2) → A [t === s ↦ a]) : Unit\n"
        (errors, _, _) = run (Just RSTTSafeError) [source]
        (warnErrors, warnings, _) = run (Just RSTTSafeWarn) [source]
    errors `shouldBe` ["TypeErrorRSTT"]
    warnErrors `shouldBe` []
    warnings `shouldBe` ["RSTTShapeDependencyWarning"]
  it "warns about shape dependencies by default with safe mode off" $ do
    run Nothing
      ["#lang rzk-1\n#set-option \"rstt-safe\" = \"off\"\n#postulate mixed (A : U) (a : A) (s : 2) (f : (t : 2) → A [t === s ↦ a]) : Unit\n"]
      `shouldBe` ([], ["RSTTShapeDependencyWarning"], 0)
  it "restores the standalone shape-dependency default on unset" $ do
    run Nothing
      ["#lang rzk-1\n#set-option \"rstt-safe\" = \"off\"\n#set-option \"warn-shape-dependency\" = \"no\"\n#unset-option \"warn-shape-dependency\"\n#postulate mixed (A : U) (a : A) (s : 2) (f : (t : 2) → A [t === s ↦ a]) : Unit\n"]
      `shouldBe` ([], ["RSTTShapeDependencyWarning"], 0)
  it "restores an explicit shape-dependency opt-out after safe mode" $ do
    run Nothing
      ["#lang rzk-1\n#set-option \"warn-shape-dependency\" = \"no\"\n#set-option \"rstt-safe\" = \"error\"\n#set-option \"rstt-safe\" = \"off\"\n#postulate mixed (A : U) (a : A) (s : 2) (f : (t : 2) → A [t === s ↦ a]) : Unit\n"]
      `shouldBe` ([], [], 0)
  it "keeps shape-dependency checks enabled by source safe mode" $ do
    let (errors, _, _) = run Nothing
          ["#lang rzk-1\n#set-option \"rstt-safe\" = \"error\"\n#set-option \"warn-shape-dependency\" = \"no\"\n#postulate mixed (A : U) (a : A) (s : 2) (f : (t : 2) → A [t === s ↦ a]) : Unit\n"]
    errors `shouldBe` ["TypeErrorRSTT"]
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

  it "rejects outer-point capture introduced by an instantiated type family" $ do
    let source = T.unlines
          [ "#lang rzk-1"
          , "#def extension (φ : 2 → TOPE) (A : U) (a : A) : U :="
          , "  (t : 2) → A [φ t ↦ a]"
          , "#def at-point (A : U) (a : A) (s : 2)"
          , "  (f : extension (\\ t → t === s) A a) : A := f s"
          ]
        (errors, warnings, _) = run (Just RSTTSafeError) [source]
    errors `shouldBe` ["TypeErrorRSTT"]
    warnings `shouldBe` ["RSTTShapeDependencyWarning"]

  it "exposes shapes behind lets, both projections and identity elimination" $ do
    let shape = "(t : 2) → A [φ t ↦ a]"
        bodies =
          [ "let X : U := " <> shape <> " in X"
          , "first ((" <> shape <> "), unit)"
          , "second (unit, (" <> shape <> "))"
          , "idJ (Unit, unit, (\\ _ _ → U), (" <> shape <> "), unit, refl)"
          ]
    mapM_ (\body -> do
      let source = T.unlines
            [ "#lang rzk-1"
            , "#def extension (φ : 2 → TOPE) (A : U) (a : A) : U := " <> body
            , "#postulate bad (A : U) (a : A) (s : 2)"
            , "  (f : extension (\\ t → t === s) A a) : Unit"
            ]
          (errors, warnings, _) = run (Just RSTTSafeError) [source]
      errors `shouldBe` ["TypeErrorRSTT"]
      warnings `shouldBe` ["RSTTShapeDependencyWarning"]) bodies

  it "checks argument domains created by earlier schematic arguments" $ do
    let source = T.unlines
          [ "#lang rzk-1"
          , "#postulate schema (φ : 2 → TOPE) (A : U) (a : A)"
          , "  (f : (t : 2) → A [φ t ↦ a]) : Unit"
          , "#def bad (A : U) (a : A) (s : 2) : Unit :="
          , "  schema (\\ t → t === s) A a (\\ t → a)"
          ]
        (errors, warnings, _) = run (Just RSTTSafeError) [source]
    errors `shouldBe` ["TypeErrorRSTT"]
    warnings `shouldBe` ["RSTTShapeDependencyWarning"]

  it "allows independent shapes after unfolding and beta reduction" $ do
    run (Just RSTTSafeError) [T.unlines
      [ "#lang rzk-1"
      , "#def extension (φ : 2 → TOPE) (A : U) (a : A) : U :="
      , "  (t : 2) → A [φ t ↦ a]"
      , "#def at-point (A : U) (a : A) (s : 2)"
      , "  (f : extension (\\ t → t === 0_2) A a) : A := f s"
      ]] `shouldBe` ([], [], 0)

  it "finds contextual domains after instantiation, let reduction and projection" $ do
    let source = T.unlines
          [ "#lang rzk-1"
          , "#def shape (φ : 2 → TOPE) (A : U) : U := (t : 2 | φ t) → A"
          , "#postulate bad (A : U) (s : 2)"
          , "  (f : first ((let X := shape (\\ t → t <= s) A in X), unit)) : Unit"
          ]
        (errors, warnings, _) = run (Just RSTTSafeWarn) [source]
    errors `shouldBe` []
    warnings `shouldSatisfy` elem "RSTTShapeDependencyWarning"

  it "preserves dependencies even when a guard simplifies to top" $ do
    let source = T.unlines
          [ "#lang rzk-1"
          , "#def extension (φ : 2 → TOPE) (A : U) (a : A) : U :="
          , "  (t : 2) → A [φ t ↦ a]"
          , "#postulate bad (A : U) (a : A) (s : 2)"
          , "  (f : extension (\\ t → s === s) A a) : Unit"
          ]
        (errors, warnings, _) = run (Just RSTTSafeError) [source]
    errors `shouldBe` ["TypeErrorRSTT"]
    warnings `shouldBe` ["RSTTShapeDependencyWarning"]

  it "preserves concluded tails after unfolding" $ do
    let source = T.unlines
          [ "#lang rzk-1"
          , "#set-option \"rstt-safe\" = \"off\""
          , "#def restricted (A : U) (a : A) : U := A [TOP ↦ a]"
          , "#set-option \"rstt-safe\" = \"error\""
          , "#def good (A : U) (a : A) : restricted A a := a"
          ]
    run Nothing [source] `shouldBe` ([], [], 0)

  it "reports an incomplete audit instead of silently accepting a reduction limit" $ do
    let source = T.unlines $ ["#lang rzk-1", "#def T0 : U := Unit"] <>
          [ "#def T" <> T.pack (show i) <> " : U := T" <> T.pack (show (i - 1))
          | i <- [1 .. 260 :: Int] ]
        (errors, warnings, _) = run (Just RSTTSafeWarn) [source]
        (strictErrors, _, _) = run (Just RSTTSafeError) [source]
    errors `shouldBe` []
    warnings `shouldSatisfy` elem "RSTTIncompleteWarning"
    strictErrors `shouldSatisfy` elem "TypeErrorRSTT"

  it "audits computed argument types in standalone commands" $ do
    let declarations = T.unlines
          [ "#lang rzk-1"
          , "#postulate schema (φ : 2 → TOPE) (A : U) (a : A)"
          , "  (f : (t : 2) → A [φ t ↦ a]) : Unit"
          ]
        application = "(\\ (s : 2) → schema (\\ t → t === s) Unit unit (\\ t → unit))"
    mapM_ (\command -> do
      let (errors, warnings, _) = run (Just RSTTSafeError) [declarations <> command]
      errors `shouldBe` ["TypeErrorRSTT"]
      warnings `shouldBe` ["RSTTShapeDependencyWarning"])
      [ "#check " <> application <> " : 2 → Unit"
      , "#compute " <> application
      , "#compute-nf " <> application
      ]

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
