{-# LANGUAGE OverloadedStrings #-}
-- | Tests for 'declBinderTypes', the elaborated types of local binders that
-- the LSP hover consumes. The interesting cases are the ones the surface
-- annotations cannot provide: bare lambda binders (typed by the domain of
-- the lambda's own Π-type) and pair patterns, whose dependent components are
-- instantiated at the earlier components.
module Rzk.BinderTypesSpec (spec) where

import qualified Data.Text                as T

import           Language.Rzk.Free.Syntax (RzkPosition (RzkPosition),
                                           getVarIdent)
import qualified Language.Rzk.Syntax      as Rzk
import           Language.Rzk.Syntax      (VarIdent' (VarIdent))
import           Rzk.TypeCheck

import           Test.Hspec

-- | Parse and typecheck a module, returning rendered binder types keyed by
-- the binder's (1-based) source position. Errors out loudly so a broken
-- fixture is obvious.
binderTypesOf :: T.Text -> [((Int, Int), String)]
binderTypesOf src =
  case Rzk.parseModule src of
    Left err -> error ("parse error: " <> T.unpack err)
    Right m  -> case defaultTypeCheck (localVerbosity Silent (typecheckModulesWithLocation [("<test>", m)])) of
      Left err    -> error ("typecheck threw: " <> ppTypeErrorInScopedContext' BottomUp err)
      Right decls ->
        [ (pos, show t)
        | d <- concatMap snd decls
        , (v, t) <- declBinderTypes d
        , let VarIdent (RzkPosition _ mpos) _ = getVarIdent v
        , Just pos <- [mpos]
        ]

exampleModule :: T.Text
exampleModule = T.unlines
  [ "#lang rzk-1"                                        -- line 1
  , "#define comp (A : U) (f : A -> A) : A -> A"         -- line 2
  , "  := \\ z -> let w := f z in f w"                   -- line 3
  , "#define dep"                                        -- line 4
  , "  ( A : U)"                                         -- line 5
  , "  ( B : A -> U)"                                    -- line 6
  , "  ( C : (x : A) -> B x -> U)"                       -- line 7
  , "  ( (p , q , r) : Sigma (x : A , y : B x) , C x y)" -- line 8
  , "  : U"                                              -- line 9
  , "  := A"                                             -- line 10
  , "#define uncur (A : U) (B : A -> U)"                 -- line 11
  , "  : (Sigma (x : A) , B x) -> U"                     -- line 12
  , "  := \\ (a , b) -> A"                               -- line 13
  ]

spec :: Spec
spec = describe "declBinderTypes" $ do
  let entries = binderTypesOf exampleModule
      at pos = lookup pos entries

  it "types an annotated parameter" $
    at (2, 15) `shouldBe` Just "U"                    -- (A : U)

  it "types a bare lambda binder from the enclosing Π-type" $
    at (3, 8) `shouldBe` Just "A"                     -- \ z ->

  it "types an unannotated let binder from its value" $
    at (3, 17) `shouldBe` Just "A"                    -- let w := f z

  it "types a Π binder inside a parameter's type" $
    at (7, 10) `shouldBe` Just "A"                    -- C : (x : A) -> ...

  it "instantiates dependent tuple components at earlier components" $ do
    at (8, 6)  `shouldBe` Just "A"                    -- p
    at (8, 10) `shouldBe` Just "B p"                  -- q
    at (8, 14) `shouldBe` Just "C p q"                -- r

  it "types a bare pair lambda against its Σ domain" $ do
    at (13, 9)  `shouldBe` Just "A"                   -- \ (a , b) ->
    at (13, 13) `shouldBe` Just "B a"
