{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Tests for the reference index behind go-to-definition, find-references,
-- and the surface-annotation tier of hover. Everything here is pure: modules
-- are parsed, never typechecked, which is exactly the situation the index is
-- for (mid-edit and ill-typed code).
module Rzk.RefIndexSpec (spec) where

import           Test.Hspec

#ifdef LSP_ENABLED
import qualified Data.Text                          as T
import           Language.Rzk.Syntax                (parseModule)
import qualified Language.Rzk.VSCode.ReferenceIndex as RI

indexOf :: [(FilePath, T.Text)] -> RI.ReferenceIndex
indexOf sources = RI.indexModules
  [ (path, m)
  | (path, src) <- sources
  , Right m <- [parseModule src]
  ]

-- All positions are 0-based, as in the LSP protocol.
lookupAt' :: RI.ReferenceIndex -> FilePath -> (Int, Int) -> Maybe RI.Binding
lookupAt' ri path (l, c) = RI.lookupAt ri (RI.Uri path) (RI.Position l c)

-- | Like 'lookupAt'', erroring out loudly when nothing is there, so a
-- misplaced test position is obvious.
bindingAt :: RI.ReferenceIndex -> FilePath -> (Int, Int) -> RI.Binding
bindingAt ri path pos = case lookupAt' ri path pos of
  Just b  -> b
  Nothing -> error ("no binding at " <> path <> ":" <> show pos)

defPos :: RI.Binding -> (FilePath, Int, Int)
defPos b =
  let RI.Location (RI.Uri p) (RI.Range (RI.Position l c) _) = RI.bindingDef b
  in (p, l, c)

libSrc :: T.Text
libSrc = T.unlines
  [ "#lang rzk-1"                                        -- 0
  , "#define id (A : U) (x : A) : A := x"                -- 1
  ]

mainSrc :: T.Text
mainSrc = T.unlines
  [ "#lang rzk-1"                                                 -- 0
  , "#variable C : U"                                             -- 1
  , "#variables D E : C -> U"                                     -- 2
  , "#define twice (A : U) (f : A -> A) (x : A) : A := f (f x)"   -- 3
  , "#define use-id (A : U) (x : A) : A := id A (twice A (id A) x)" -- 4
  , "#define shadow (id : C) : C := id"                           -- 5
  , "#define pairs"                                               -- 6
  , "  ( A : U)"                                                  -- 7
  , "  ( B : A -> U)"                                             -- 8
  , "  ( (p , q) : Sigma (x : A) , B x)"                          -- 9
  , "  ( (t , s) : 2 * 2)"                                        -- 10
  , "  ( ((a , b) , c) : (2 * 2) * 2)"                            -- 11
  , "  ( (u , v) : 2 * 2 | t <= u /\\ s <= v)"                    -- 12
  , "  ( (h , k , l) : Sigma (x : A , y : B x) , C)"              -- 13
  , "  : U"                                                       -- 14
  , "  := A"                                                      -- 15
  ]

spec :: Spec
spec = describe "reference index" $ do
  let ri = indexOf [("lib.rzk", libSrc), ("main.rzk", mainSrc)]
      annAt path pos = lookupAt' ri path pos >>= RI.bindingType

  describe "definitions and references" $ do
    it "resolves a cross-file use to its definition" $
      (defPos <$> lookupAt' ri "main.rzk" (4, 38)) `shouldBe` Just ("lib.rzk", 1, 8)

    it "lists references without duplicating the definition site" $ do
      let b = bindingAt ri "lib.rzk" (1, 8)
      length (RI.bindingRefs b) `shouldBe` 2                -- two uses in use-id
      RI.bindingDef b `notElem` RI.bindingRefs b `shouldBe` True
      length (RI.bindingSites b) `shouldBe` 3               -- def : refs

    it "resolves a local binder and its use" $ do
      let b = bindingAt ri "main.rzk" (3, 55)               -- x in f (f x)
      defPos b `shouldBe` ("main.rzk", 3, 36)
      length (RI.bindingSites b) `shouldBe` 2

    it "resolves a local that shadows a global to the local" $
      (defPos <$> lookupAt' ri "main.rzk" (5, 31))          -- id in := id
        `shouldBe` Just ("main.rzk", 5, 16)

    it "misses positions outside identifiers" $
      lookupAt' ri "main.rzk" (3, 13) `shouldBe` Nothing    -- whitespace

  describe "surface type annotations" $ do
    it "annotates parameters" $ do
      annAt "main.rzk" (3, 15) `shouldBe` Just "U"          -- (A : U)
      annAt "main.rzk" (3, 23) `shouldBe` Just "A -> A"     -- (f : A -> A)

    it "annotates assumptions (#variable, #variables)" $ do
      annAt "main.rzk" (1, 10) `shouldBe` Just "U"
      annAt "main.rzk" (2, 11) `shouldBe` Just "C -> U"
      annAt "main.rzk" (2, 13) `shouldBe` Just "C -> U"

    it "leaves top-level names to the elaborated tier" $ do
      let b = bindingAt ri "main.rzk" (3, 8)                -- twice
      RI.bindingType b `shouldBe` Nothing

    it "splits a pair against a Σ-type, instantiating the dependency" $ do
      annAt "main.rzk" (9, 5) `shouldBe` Just "A"           -- p
      annAt "main.rzk" (9, 9) `shouldBe` Just "B p"         -- q

    it "splits pairs against cube products, nested too" $ do
      annAt "main.rzk" (10, 5) `shouldBe` Just "2"          -- t
      annAt "main.rzk" (10, 9) `shouldBe` Just "2"          -- s
      annAt "main.rzk" (11, 6) `shouldBe` Just "2"          -- a
      annAt "main.rzk" (11, 10) `shouldBe` Just "2"         -- b
      annAt "main.rzk" (11, 15) `shouldBe` Just "2"         -- c

    it "gives shaped pair components their cube" $ do
      annAt "main.rzk" (12, 5) `shouldBe` Just "2"          -- u
      annAt "main.rzk" (12, 9) `shouldBe` Just "2"          -- v

    it "splits a tuple against a Σ-tuple" $ do
      annAt "main.rzk" (13, 5) `shouldBe` Just "A"          -- h
      annAt "main.rzk" (13, 9) `shouldBe` Just "B h"        -- k
      annAt "main.rzk" (13, 13) `shouldBe` Just "C"         -- l

  describe "#data declarations" $ do
    let dataSrc = T.unlines
          [ "#lang rzk-1"                                        -- 0
          , "#data bool := false | true"                         -- 1
          , "#define not (b : bool) : bool"                      -- 2
          , "  := rec-bool bool true false b"                    -- 3
          ]
        riData = indexOf [("data.rzk", dataSrc)]

    it "resolves the type name at a use after the declaration" $
      defPos (bindingAt riData "data.rzk" (2, 17)) `shouldBe` ("data.rzk", 1, 6)

    it "resolves a constructor at a use after the declaration" $
      -- `true` in the body of `not`
      defPos (bindingAt riData "data.rzk" (3, 19)) `shouldBe` ("data.rzk", 1, 22)

    it "resolves a generated eliminator to the #data name" $
      -- `rec-bool` has no source declaration; it derives from `bool`
      defPos (bindingAt riData "data.rzk" (3, 5)) `shouldBe` ("data.rzk", 1, 6)

    it "does not occlude the type name with the derived entries" $
      -- hovering the declaration's own `bool` must hit `bool`, not ind-bool
      RI.bindingName (bindingAt riData "data.rzk" (1, 6)) `shouldBe` "bool"
#else
spec :: Spec
spec = describe "reference index" (pure ())
#endif
