{-# LANGUAGE OverloadedStrings #-}
module Rzk.SchematicSpec (spec) where

import qualified Data.Text as T
import qualified Language.Rzk.Syntax as Syntax
import Rzk.Diagnostic (checkWarningTag, typeErrorTagInScopedContext)
import Rzk.TypeCheck
import Test.Hspec

check :: Maybe RSTTSafeMode -> [T.Text] -> ([String], [String])
check mode sources = case traverse Syntax.parseModule sources of
  Left err -> error (T.unpack err)
  Right modules -> case checkedModules (zip (map show [1 :: Int ..]) modules)
    (emptyContext { ctxRSTTSafeOverride = mode, ctxVerbosity = Silent }) of
      Left err -> ([typeErrorTagInScopedContext err], [])
      Right (checked, _) ->
        (map typeErrorTagInScopedContext (checkedErrors checked),
         map checkWarningTag (checkedWarnings checked))

source :: [T.Text] -> T.Text
source declarations = T.unlines ("#lang rzk-1" : declarations)

identity :: T.Text
identity = "#def id (A : U) (a : A) : A := a"

accepts :: [T.Text] -> Expectation
accepts ds = check (Just RSTTSafeError) [source ds] `shouldBe` ([], [])

rejects :: [T.Text] -> Expectation
rejects ds = do
  let (errors, warnings) = check (Just RSTTSafeError) [source ds]
  errors `shouldBe` ["TypeErrorRSTT"]
  warnings `shouldSatisfy` elem "RSTTSchematicWarning"

spec :: Spec
spec = describe "Schematic declarations" $ do
  it "accepts ordinary type and family arguments" $ accepts
    [ identity
    , "#def apply (A : U) (B : A → U) (f : (a : A) → B a) (a : A) : B a := f a"
    , "#def good : Unit := apply Unit (\\ x → Unit) (\\ x → id Unit x) unit"
    ]

  it "keeps schematic statements distinct from proofs" $ accepts
    [ "#def Bottom : U := (A : U) → A"
    , "#postulate trusted : Bottom"
    ]

  it "accepts schematic rule aliases and their instances" $ accepts
    [ "#def Rule : U := (A : U) → A → A"
    , "#def apply-rule (rule : Rule) (A : U) (a : A) : A := rule A a"
    , "#def good : Unit := apply-rule (\\ A a → a) Unit unit"
    ]

  it "checks every universe instantiation before beta reduction" $
    mapM_ (\(arg, value) -> rejects [identity, "#def bad : " <> arg <> " := id " <> arg <> " " <> value])
      [("U", "Unit"), ("CUBE", "2"), ("TOPE", "TOP")]

  it "rejects families and rules passed as ordinary types" $
    mapM_ (\(ty, value) -> rejects [identity, "#def bad : " <> ty <> " := id (" <> ty <> ") (" <> value <> ")"])
      [("U → U", "\\ x → x"), ("(A : U) → A → A", "\\ A a → a")]

  it "rejects the predicate-of-predicates step of Hurkens" $ rejects
    [ "#def P (A : U) : U := A → U"
    , "#def bad (X : U) : U := P (P X)"
    ]

  it "checks family arguments pointwise" $ rejects
    [ "#def receive (F : Unit → U) : U := F unit"
    , "#def bad : U := receive (\\ x → U)"
    ]

  it "checks higher-order family arguments pointwise" $ rejects
    [ "#def receive (F : U → U) : U := F Unit"
    , "#def bad : U := receive (\\ A → U)"
    ]

  it "allows higher-order meta functions with object-valued instances" $ accepts
    [ "#def receive (F : U → U) : U := F Unit"
    , "#def good : U := receive (\\ A → A → A)"
    ]

  it "allows meta computations through pairs" $ accepts
    [ "#def T : U := first (Unit, unit)"
    , "#def good : T := unit"
    ]

  it "rejects a product containing schematic data as an object type" $ rejects
    [ identity
    , "#def bad : Σ (A : U), A := id (Σ (A : U), A) (Unit, unit)"
    ]

  it "rejects object equality between types" $ rejects
    [ "#postulate bad : Unit =_{U} Unit" ]

  it "rejects equality elimination into the universe" $ rejects
    [ "#def bad : U := idJ (Unit, unit, (\\ _ _ → U), Unit, unit, refl)" ]

  it "rechecks used dependencies introduced with safe mode off" $ do
    let ds = [ "#set-option \"rstt-safe\" = \"off\"", identity
             , "#def bad : U := id U Unit"
             , "#set-option \"rstt-safe\" = \"error\""
             , "#def use : U := bad" ]
        (errors, warnings) = check Nothing [source ds]
    errors `shouldBe` ["TypeErrorRSTT"]
    warnings `shouldSatisfy` elem "RSTTSchematicWarning"

  it "does not recheck unused dependencies" $
    check Nothing [source
      [ "#set-option \"rstt-safe\" = \"off\"", identity
      , "#def bad : U := id U Unit"
      , "#set-option \"rstt-safe\" = \"error\""
      , "#def good : Unit := unit" ]] `shouldBe` ([], [])

  it "keeps the additional check off in ordinary mode" $
    check (Just RSTTSafeOff) [source [identity, "#def bad : U := id U Unit"]]
      `shouldBe` ([], [])

  it "reports kind confusion in warning mode" $
    check (Just RSTTSafeWarn) [source [identity, "#def bad : U := id U Unit"]]
      `shouldBe` ([], ["RSTTSchematicWarning"])

  it "checks standalone commands" $
    mapM_ (\command -> rejects [identity, command])
      ["#check id U Unit : U", "#compute id U Unit", "#compute-nf id U Unit"]

  it "preserves valid section abstraction and checks later instances" $ do
    let section = ["#section S", "#assume A : U", "#def local-id (a : A) : A := a", "#end S"]
    accepts (section <> ["#def good : Unit := local-id Unit unit"])
    rejects (section <> ["#def bad : U := local-id U Unit"])

  it "reconstructs applications after section parameters become explicit" $ accepts
    [ "#def product (X Y : U) : U := Σ (x : X), Y"
    , "#section S", "#assume A B : U"
    , "#def F (f : A → B) : U := A"
    , "#def G (f : A → B) : U := product (F f) (F f)"
    , "#end S"
    , "#postulate test (A B : U) (f : A → B) : G A B f"
    ]

  it "rejects a universe hidden in a neutral case split" $ rejects
    [ "#def mixed (i : 2 | i === 0_2 ∨ i === 1_2) : U :="
    , "  recOR (i === 0_2 ↦ Unit, i === 1_2 ↦ U)"
    , "#postulate receive (A : U) : Unit"
    , "#def bad (i : 2 | i === 0_2 ∨ i === 1_2) : Unit := receive (mixed i)"
    ]

  it "accepts object types selected by a neutral case split" $ accepts
    [ "#def mixed (i : 2 | i === 0_2 ∨ i === 1_2) : U :="
    , "  recOR (i === 0_2 ↦ Unit, i === 1_2 ↦ Unit → Unit)"
    , "#postulate receive (A : U) : Unit"
    , "#def good (i : 2 | i === 0_2 ∨ i === 1_2) : Unit := receive (mixed i)"
    ]

  it "accepts ex falso under an empty shape without following its cyclic annotation" $ accepts
    [ "#def empty-shape (A : U) (t : 2 | BOT) : A := recBOT" ]

  it "checks erased type arguments in declaration signatures" $ rejects
    [ identity, "#postulate bad : id U Unit" ]

  it "checks the kinds of boundary values on a universe" $ do
    rejects ["#def bad : U [TOP ↦ U] := U"]
    accepts ["#def good : U [TOP ↦ Unit] := Unit"]
