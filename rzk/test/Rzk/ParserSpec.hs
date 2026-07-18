{-# LANGUAGE OverloadedStrings #-}

-- | The surface parser: the deprecated forms removed from the grammar must be
-- rejected, and their modern replacements must still parse.
--
-- The removed forms are the ones deprecated since v0.5.0: braces around
-- parameters (with or without a tope), angle brackets around extension types,
-- and the 4-argument @recOR@.
module Rzk.ParserSpec (spec) where

import           Data.Either         (isLeft, isRight)
import qualified Data.Text           as T
import           Test.Hspec

import qualified Language.Rzk.Syntax as Rzk

rejects :: T.Text -> Expectation
rejects t = Rzk.parseTerm t `shouldSatisfy` isLeft

parses :: T.Text -> Expectation
parses t = Rzk.parseTerm t `shouldSatisfy` isRight

spec :: Spec
spec = do
  describe "removed deprecated forms are parse errors" $ do
    it "4-argument recOR" $
      rejects "recOR(psi, phi, a, b)"
    it "angle brackets around an extension type" $
      rejects "<(t : 2 | TOP) → A>"
    it "angle brackets around an extension type (ASCII)" $
      rejects "<(t : 2 | TOP) -> A>"
    it "braces around a typed parameter" $
      rejects "{x : A} -> B"
    it "braces around a shape parameter" $
      rejects "{t : 2 | TOP} -> A"
    it "braces around a parenthesised shape parameter" $
      rejects "{(t : 2) | TOP} -> A"
    it "braces around a shape parameter of a lambda" $
      rejects "\\ {t : 2 | TOP} -> t"

  describe "the modern replacements still parse" $ do
    it "recOR over restrictions" $
      parses "recOR(psi |-> a, phi |-> b)"
    it "extension type as a shaped function type" $
      parses "(t : 2 | TOP) -> A"
    it "typed parameter in parentheses" $
      parses "(x : A) -> B"
    it "shape parameter of a lambda" $
      parses "\\ (t : 2 | TOP) -> t"
