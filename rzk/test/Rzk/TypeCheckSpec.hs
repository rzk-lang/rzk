{-# LANGUAGE OverloadedStrings #-}
-- Regression examples follow rzk-lang/rzk#359, based on Reviewer A's
-- tope-family counterexample. Restricted endpoint cases follow
-- https://github.com/rzk-lang/rzk/pull/377.
-- Warning cases follow https://github.com/rzk-lang/rzk/pull/362.
-- Lambda-domain cases follow https://github.com/rzk-lang/rzk/pull/347.
-- This runner uses the historical checker API.
module Rzk.TypeCheckSpec where

import           Language.Rzk.Free.Syntax (TermT')
import qualified Language.Rzk.Syntax as Rzk
import           Rzk.TypeCheck
import           Test.Hspec

spec :: Spec
spec = do
  describe "Tope-family domains" $ do
    it "preserves the clipped domain through a wider parameter" $
      checkFixture "happy-tope-family-domain-instance" Accept
    it "rejects the non-instance exploit at its cube argument" $
      checkFixture "ill-tope-family-domain-non-instance" RejectTope
    it "rejects the same-domain control at its cube argument" $
      checkFixture "ill-tope-family-domain-same-domain" RejectTope

  describe "Tope-family warnings" $ do
    it "reports a clipped family once per source location" $
      checkFixtureWarnings "warn-tope-family-domain-non-instance" 2
    it "supports disabling the warning and restoring its default" $
      checkFixtureWarnings "warn-tope-family-domain-option" 1
    it "does not warn for an included family" $
      checkFixtureWarnings "happy-tope-family-domain-contained" 0
    it "rejects an invalid warning option" $
      checkFixture "ill-tope-family-domain-option" RejectOption

  describe "Lambda domains" $ do
    let point = "\\ (t : 2 | t === 0_2) -> unit" :: TermT'
        interval = "\\ (t : 2 | TOP) -> unit" :: TermT'
    it "restricts an interval lambda to a point covariantly" $
      checkComparison Covariant point interval Accept
    it "restricts an interval lambda to a point contravariantly" $
      checkComparison Contravariant interval point Accept
    it "rejects widening a point lambda covariantly" $
      checkComparison Covariant interval point RejectTope
    it "rejects widening a point lambda contravariantly" $
      checkComparison Contravariant point interval RejectTope
    it "rejects different domains in invariant comparison" $
      checkComparison Invariant point interval RejectTope

    it "compares bodies under the common domain in both directions" $ do
      let pointBody = "\\ (t : 2 | t === 0_2) -> Unit [TOP |-> unit]" :: TermT'
          intervalBody = "\\ (t : 2 | TOP) -> Unit [t === 0_2 |-> unit]" :: TermT'
      checkComparison Covariant pointBody intervalBody Accept
      checkComparison Contravariant intervalBody pointBody Accept
    it "rejects bodies that only subtype on the common domain" $ do
      let pointBody = "\\ (t : 2 | t === 0_2) -> Unit" :: TermT'
          intervalBody = "\\ (t : 2 | TOP) -> Unit [TOP |-> unit]" :: TermT'
      checkComparison Covariant pointBody intervalBody RejectConversion
    it "accepts equivalent domains invariantly" $
      checkComparison Invariant point
        "\\ (t : 2 | t === 0_2 /\\ TOP) -> unit" Accept

  describe "Invariant term comparison" $ do
    it "rejects subtyping of identity endpoints" $
      checkFixture "ill-variance-id-endpoint" RejectConversion
    it "rejects subtyping between restricted identity endpoints" $
      checkFixture "ill-variance-restricted-endpoint" RejectTope
    it "rejects subtyping between restricted lambda endpoints" $
      checkFixture "ill-variance-restricted-lambda-endpoint" RejectTope
    it "rejects the equality-collapse exploit" $
      checkFixture "ill-variance-all-equal" RejectConversion
    it "rejects incoherent branches in either order" $ do
      checkFixture "ill-variance-coherence-forward" RejectConversion
      checkFixture "ill-variance-coherence-reverse" RejectConversion
    it "compares type-valued pair boundaries invariantly" $
      checkFixture "ill-variance-pair-boundary" RejectConversion
    it "compares type-valued function boundaries invariantly" $
      checkFixture "ill-variance-lambda-boundary" RejectConversion
    it "preserves type subtyping and coherent branches" $
      checkFixture "happy-variance-conversion" Accept

data Expected = Accept | RejectTope | RejectConversion | RejectOption
  deriving (Eq, Show)

checkFixture :: String -> Expected -> Expectation
checkFixture name expected = do
  let path = "test/files/" ++ name ++ ".rzk"
  source <- readFile path
  case Rzk.parseModule source of
    Left err -> expectationFailure ("Parse error: " ++ err)
    Right parsed ->
      case defaultTypeCheck (localVerbosity Silent (typecheckModulesWithLocation [(path, parsed)])) of
        Right _ -> Accept `shouldBe` expected
        Left err
          | Just expected == rejectionKind err -> pure ()
          | otherwise -> expectationFailure (ppTypeErrorInScopedContext' BottomUp err)

rejectionKind :: TypeErrorInScopedContext var -> Maybe Expected
rejectionKind (ScopedTypeError _ err) = rejectionKind err
rejectionKind (PlainTypeError err) = case typeErrorError err of
  TypeErrorTopeNotSatisfied{} -> Just RejectTope
  TypeErrorTopesNotEquivalent{} -> Just RejectTope
  TypeErrorUnifyTerms{} -> Just RejectConversion
  TypeErrorUnify{} -> Just RejectConversion
  TypeErrorOther message
    | message == "unknown value for \"warn-tope-family-domain\" (use \"yes\" or \"no\")" -> Just RejectOption
  _ -> Nothing

checkFixtureWarnings :: String -> Int -> Expectation
checkFixtureWarnings name expected = do
  let path = "test/files/" ++ name ++ ".rzk"
  source <- readFile path
  case Rzk.parseModule source of
    Left err -> expectationFailure ("Parse error: " ++ err)
    Right parsed -> do
      let (result, warnings) = defaultTypeCheckWithWarnings $
            localVerbosity Silent (typecheckModulesWithLocation [(path, parsed)])
      case result of
        Left err -> expectationFailure (ppTypeErrorInScopedContext' BottomUp err)
        Right _ -> length warnings `shouldBe` expected

checkComparison :: Covariance -> TermT' -> TermT' -> Expected -> Expectation
checkComparison variance expected actual verdict =
  case defaultTypeCheck (localVerbosity Silent (setVariance variance (unify Nothing expected actual))) of
    Right () -> Accept `shouldBe` verdict
    Left err
      | Just verdict == rejectionKind err -> pure ()
      | otherwise -> expectationFailure (ppTypeErrorInScopedContext' BottomUp err)
