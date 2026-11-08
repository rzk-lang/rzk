-- Regression examples follow rzk-lang/rzk#359, based on Reviewer A's
-- tope-family counterexample. Variance cases exercise type-valued terms.
-- This runner uses the historical checker API.
module Rzk.TypeCheckSpec where

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

  describe "Invariant term comparison" $ do
    it "rejects subtyping of identity endpoints" $
      checkFixture "ill-variance-id-endpoint" RejectConversion
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

data Expected = Accept | RejectTope | RejectConversion
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
  TypeErrorUnifyTerms{} -> Just RejectConversion
  TypeErrorUnify{} -> Just RejectConversion
  _ -> Nothing
