-- Regression examples follow rzk-lang/rzk#359, based on Reviewer A's
-- tope-family counterexample. This runner uses the historical checker API.
module Rzk.TypeCheckSpec where

import qualified Language.Rzk.Syntax as Rzk
import           Rzk.TypeCheck
import           Test.Hspec

spec :: Spec
spec = describe "Tope-family domains" $ do
  it "preserves the clipped domain through a wider parameter" $
    checkFixture "happy-tope-family-domain-instance" False
  it "rejects the non-instance exploit at its cube argument" $
    checkFixture "ill-tope-family-domain-non-instance" True
  it "rejects the same-domain control at its cube argument" $
    checkFixture "ill-tope-family-domain-same-domain" True

checkFixture :: String -> Bool -> Expectation
checkFixture name expectRejection = do
  let path = "test/files/" ++ name ++ ".rzk"
  source <- readFile path
  case Rzk.parseModule source of
    Left err -> expectationFailure ("Parse error: " ++ err)
    Right parsed ->
      case defaultTypeCheck (localVerbosity Silent (typecheckModulesWithLocation [(path, parsed)])) of
        Right _ -> expectRejection `shouldBe` False
        Left err
          | expectRejection && isTopeError err -> pure ()
          | otherwise -> expectationFailure (ppTypeErrorInScopedContext' BottomUp err)

isTopeError :: TypeErrorInScopedContext var -> Bool
isTopeError (ScopedTypeError _ err) = isTopeError err
isTopeError (PlainTypeError err) = case typeErrorError err of
  TypeErrorTopeNotSatisfied{} -> True
  _ -> False
