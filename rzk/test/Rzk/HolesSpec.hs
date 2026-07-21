{-# LANGUAGE OverloadedStrings #-}
-- | Tests for the structured goal/context query (lenient hole mode). The
-- YAML fixtures cover the strict-mode errors (ill-hole-*); here we check the
-- structured 'HoleInfo' that the game and LSP consume, which substring
-- assertions in the fixture harness cannot pin down (especially the split
-- between term variables and cube variables).
module Rzk.HolesSpec (spec) where

import           Data.List           (isInfixOf)
import           Data.Maybe          (fromMaybe)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T (readFile)
import           System.Environment  (lookupEnv)
import           System.FilePath     ((</>))

import qualified Language.Rzk.Syntax as Rzk
import           Language.Rzk.Foil.Names (VarIdent)
import           Rzk.Diagnostic      (typeErrorTagInScopedContext)
import           Rzk.TypeCheck

import           Test.Hspec

-- | Parse and typecheck a module in lenient hole mode, returning the recorded
-- holes. Errors out loudly so a broken fixture is obvious.
holesOf :: T.Text -> [HoleInfo]
holesOf src =
  case Rzk.parseModule src of
    Left err -> error ("parse error: " <> T.unpack err)
    Right m  -> case typecheckModulesWithHoles [("<test>", m)] of
      Left err          -> error ("typecheck threw: " <> ppTypeErrorInScopedContext BottomUp err)
      Right (_, holes)  -> holes

-- | Like 'holesOf', but allow-lists the given named top-level lemmas for the
-- candidate hints (see 'withHintLemmas'\/'typecheckModulesWithHolesAndLemmas').
holesWithLemmas :: [VarIdent] -> T.Text -> [HoleInfo]
holesWithLemmas lemmas src =
  case Rzk.parseModule src of
    Left err -> error ("parse error: " <> T.unpack err)
    Right m  -> case typecheckModulesWithHolesAndLemmas lemmas [("<test>", m)] of
      Left err         -> error ("typecheck threw: " <> ppTypeErrorInScopedContext BottomUp err)
      Right (_, holes) -> holes

names :: [HoleEntry] -> [String]
names = map (show . holeEntryName)

-- | Parse and typecheck a module in lenient hole mode, returning the
-- constructor tags of the collected (non-fatal) type errors. Used to assert
-- which diagnostics fire without depending on their rendered text.
errTagsOf :: T.Text -> [String]
errTagsOf src =
  case Rzk.parseModule src of
    Left err -> error ("parse error: " <> T.unpack err)
    Right m  -> case typecheckModulesWithHoles [("<test>", m)] of
      Left err           -> [typeErrorTagInScopedContext err]
      Right (checked, _) -> map typeErrorTagInScopedContext (checkedErrors checked)

-- | Like 'holesOf'/'errTagsOf', but reads a module from @test/files/@ (so a
-- large example need not be inlined). Honours @RZK_TEST_ROOT@ like the other
-- specs. Returns the recorded holes and the collected error tags.
checkFile :: FilePath -> IO ([HoleInfo], [String])
checkFile name = do
  root <- fromMaybe "." <$> lookupEnv "RZK_TEST_ROOT"
  src  <- T.readFile (root </> "test" </> "files" </> name)
  case Rzk.parseModule src of
    Left err -> error ("parse error: " <> T.unpack err)
    Right m  -> case typecheckModulesWithHoles [(name, m)] of
      Left err            -> error ("typecheck threw: " <> ppTypeErrorInScopedContext BottomUp err)
      Right (checked, hs) ->
        pure (hs, map typeErrorTagInScopedContext (checkedErrors checked))

spec :: Spec
spec = do
  describe "typecheckModulesWithHoles (structured goal/context query)" $ do
    it "records a hole's goal and local term context (no name)" $ do
      case holesOf "#lang rzk-1\n#define f : (A : U) -> A -> A\n  := \\ A a -> ?\n" of
        [h] -> do
          holeName h `shouldBe` Nothing
          show (holeGoal h) `shouldBe` "A"
          names (holeTermVars h) `shouldContain` ["A"]
          names (holeTermVars h) `shouldContain` ["a"]
          holeCubeVars h `shouldBe` []
        hs  -> expectationFailure ("expected exactly one hole, got " <> show (length hs))

    it "keeps the user's hole name and splits cube variables from term variables" $ do
      case holesOf "#lang rzk-1\n#define g : (A : U) -> (t : 2) -> A\n  := \\ A t -> ?goal\n" of
        [h] -> do
          holeName h `shouldBe` Just "goal"
          names (holeCubeVars h) `shouldBe` ["t"]
          names (holeTermVars h) `shouldContain` ["A"]
          names (holeTermVars h) `shouldNotContain` ["t"]
        hs  -> expectationFailure ("expected exactly one hole, got " <> show (length hs))

    -- Rendering: an anonymous binder that the return type does not use is
    -- not shown ("U → U" rather than "(x₁ : U) → U"), while a user-written
    -- name is kept even when unused.
    it "hides unused anonymous binders in rendered types" $ do
      case holesOf "#lang rzk-1\n#define mp : (A : U) -> (B : U) -> A -> (A -> B) -> B := ?\n" of
        [h] -> show (holeGoal h) `shouldBe` "(A : U) → (B : U) → A → (A → B) → B"
        hs  -> expectationFailure ("expected exactly one hole, got " <> show (length hs))
      case holesOf "#lang rzk-1\n#define keep : (x : U) -> U -> U := ?\n" of
        [h] -> show (holeGoal h) `shouldBe` "(x : U) → U → U"
        hs  -> expectationFailure ("expected exactly one hole, got " <> show (length hs))

    it "records every hole in a module" $ do
      let holes = holesOf "#lang rzk-1\n#define p : (A : U) -> (B : U) -> A -> B -> A\n  := \\ A B a b -> ?\n"
      length holes `shouldBe` 1
      let holes2 = holesOf "#lang rzk-1\n#define q : (A : U) -> A -> A\n  := \\ A a -> ?\n#define r : (A : U) -> A -> A\n  := \\ A a -> ?\n"
      length holes2 `shouldBe` 2

    -- A multi-variable binder (x y : A) is parsed as the application spine
    -- `x y`; it must be desugared into nested binders rather than crash
    -- `unsafeTermToPattern` ("expected a pattern but got x y"). The hole query
    -- then sees the hypothesis as the nested function type.
    it "handles a hole whose context has a multi-variable binder" $ do
      case holesOf "#lang rzk-1\n#assume A : U\n#def foo (k : (x y : A) → A) : A := ?\n" of
        [h] -> do
          show (holeGoal h) `shouldBe` "A"
          map (show . holeEntryType) (holeTermVars h)
            `shouldContain` ["(x : A) → (y : A) → A"]
        hs  -> expectationFailure ("expected exactly one hole, got " <> show (length hs))

    -- A hole whose elaborated term reaches unification (here the `refl` endpoint)
    -- must not panic ("unexpected term in UNIFY"); it unifies with anything.
    it "handles a hole that flows into unification" $ do
      case holesOf "#lang rzk-1\n#define t : (A : U) -> (a : A) -> a =_{A} a\n  := \\ A a -> refl_{?}\n" of
        [h] -> show (holeGoal h) `shouldBe` "A"
        hs  -> expectationFailure ("expected exactly one hole, got " <> show (length hs))

    -- A bare hole in each recOR branch: the checking-direction recOR rule pushes
    -- the common type (A) into every branch, so both holes are recorded against A
    -- with their branch tope in context. Without that rule the recOR is elaborated
    -- by inference and the branch holes fail with TypeErrorCannotInferHole.
    it "records a hole in each recOR branch against the common type" $ do
      case holesOf "#lang rzk-1\n#define square : (A : U) -> A -> (2 × 2) -> A\n  := \\ A a (t , s) -> recOR ( t ≤ s ↦ ? , s ≤ t ↦ ? )\n" of
        [h1, h2] -> do
          show (holeGoal h1) `shouldBe` "A"
          show (holeGoal h2) `shouldBe` "A"
          map show (holeTopes h1) `shouldContain` ["t ≤ s"]
          map show (holeTopes h2) `shouldContain` ["s ≤ t"]
        hs  -> expectationFailure ("expected exactly two holes, got " <> show (length hs))

    -- When the recOR is checked against an /extension type/, each branch hole
    -- reports the boundary it must meet under its tope (the restriction is
    -- pushed into the branches), rather than the bare underlying type. So the
    -- player sees, e.g., that the value must equal `f t` on `s ≡ 0₂` and `a` on
    -- `s ≡ 1₂`, not just `A`.
    it "shows the extension-type boundary in a recOR branch hole" $ do
      case holesOf "#lang rzk-1\n#def square (A : U) (a : A) (f : (t : 2) → A)\n  : (t : 2) → (s : 2) → A [ s ≡ 0₂ ↦ f t , s ≡ 1₂ ↦ a ]\n  := \\ t s → recOR ( s ≤ t ↦ ? , t ≤ s ↦ ? )\n" of
        [h1, h2] -> do
          let goal1 = show (holeGoal h1)
          ("A [" `isInfixOf` goal1) `shouldBe` True   -- a restricted type, not bare A
          ("↦ f t" `isInfixOf` goal1) `shouldBe` True -- the s ≡ 0₂ face is shown
          ("↦ a" `isInfixOf` goal1) `shouldBe` True   -- the s ≡ 1₂ face is shown
          map show (holeTopes h1) `shouldContain` ["s ≤ t"]
          show (holeGoal h2) `shouldBe` goal1          -- both branches carry the same boundary
          map show (holeTopes h2) `shouldContain` ["t ≤ s"]
        hs  -> expectationFailure ("expected exactly two holes, got " <> show (length hs))

    -- A hole nested inside a larger term (`f ?`) checked against an
    -- extension-type boundary: the boundary face is unified against `f ?`, which
    -- must be deferred rather than reported as a mismatch.
    it "handles a nested hole under an extension-type boundary" $ do
      case holesOf "#lang rzk-1\n#define t : (A : U) -> (f : A -> A) -> (a : A) -> (t : 2) -> A [ t === 0_2 |-> a ]\n  := \\ A f a t -> f ?\n" of
        [h] -> show (holeGoal h) `shouldBe` "A"
        hs  -> expectationFailure ("expected exactly one hole, got " <> show (length hs))

    -- A hole standing for a /whole/ shape-restricted argument under an enclosing
    -- extension-type boundary. The argument unfolds to a recOR whose faces
    -- mention the hole (e.g. @π₁ ? ≤ π₂ ?@); checking the boundary enters such a
    -- face and a branch reduction then drops the hole from the compared terms
    -- (the triangle here, @const-pt A c@, discards its point). So the mismatch
    -- (@a@ vs @c@) is hole-free in the terms even though the assumed face still
    -- mentions the hole. The per-term deferral cannot see it; deferring on a
    -- hole-bearing tope context can, so the hole is reported with its shape goal
    -- rather than rejected. (Before the fix this raised TypeErrorUnifyTerms.)
    it "handles a hole for a whole shape argument under an extension boundary" $ do
      let src = "#lang rzk-1\n\
                \#def Δ¹ : 2 → TOPE := \\ t → TOP\n\
                \#def Δ¹×Δ¹ : (2 × 2) → TOPE := \\ (t , s) → TOP ∧ TOP\n\
                \#def unfold (A : U) (tri : (2 × 2) → A) : Δ¹×Δ¹ → A\n\
                \  := \\ (t , s) → recOR (t ≤ s ↦ tri (s , t) , s ≤ t ↦ tri (t , s))\n\
                \#def const-pt (A : U) (c : A) : (2 × 2) → A := \\ (t , s) → c\n\
                \#def hom (A : U) (x y : A) : U := (t : Δ¹) → A [ t ≡ 0₂ ↦ x , t ≡ 1₂ ↦ y ]\n\
                \#def test (A : U) (a b c : A) : hom A a b\n\
                \  := \\ t → unfold A (const-pt A c) ?\n"
      case holesOf src of
        [h] -> do
          show (holeGoal h) `shouldBe` "2 × 2"
          case holeGoalShape h of
            Just (s, tope) -> show tope `shouldBe` ("Δ¹×Δ¹ " <> show s)
            Nothing        -> expectationFailure "expected a shape goal (holeGoalShape)"
        hs  -> expectationFailure ("expected exactly one hole, got " <> show (length hs))

    -- A genuinely completable work-in-progress term is tolerated. The example
    -- (the Yoneda game's square-transformation level) feeds an incomplete
    -- `codomain-square A is-segal-A a b (f ?) ? ? ? ?` where a value of
    -- `hom A (f t) a` is wanted. Inferring it records the holes, but the final
    -- check of its type then unifies two extension-type boundaries, and a
    -- *hole-free* face comparison fails — so the existing per-term deferral
    -- cannot see that a hole is involved, and the whole def was rejected with
    -- TypeErrorUnifyTerms. We now tolerate it and keep the recorded holes. The
    -- companion `…-complete` definition fills the holes and typechecks, so the
    -- holes really can be filled to make the same term check (it is not a dead
    -- end). See test/files/holes-wip-square.rzk.
    it "tolerates a completable WIP term whose type only fails around a hole" $ do
      (holes, errs) <- checkFile "holes-wip-square.rzk"
      errs `shouldBe` []          -- the WIP def is tolerated, the complete one checks
      length holes `shouldBe` 5   -- the five holes of square-transformation-wip

    -- A hole used as the argument of a shape-restricted function: the
    -- shape-membership tope (psi ?) mentions the hole and cannot be decided, so
    -- it is deferred rather than reported as TypeErrorTopeNotSatisfied. The goal
    -- is the shape (s : I | psi s), captured in holeGoalShape.
    it "records the shape goal for a hole argument to a shape-restricted function" $ do
      case holesOf "#lang rzk-1\n#define t : (I : CUBE) -> (psi : I -> TOPE) -> (A : U) -> (a : (s : I | psi s) -> A) -> (t : I) -> A\n  := \\ I psi A a t -> a ?\n" of
        [h] -> do
          show (holeGoal h) `shouldBe` "I"
          case holeGoalShape h of
            Just (s, tope) -> do
              show s `shouldBe` "s"
              show tope `shouldBe` "psi s"
            Nothing -> expectationFailure "expected a shape goal (holeGoalShape)"
        hs  -> expectationFailure ("expected exactly one hole, got " <> show (length hs))

    -- An ordinary (non-shape) hole has no shape goal.
    it "leaves holeGoalShape empty for an ordinary hole" $ do
      case holesOf "#lang rzk-1\n#define f : (A : U) -> A -> A\n  := \\ A a -> ?\n" of
        [h] -> holeGoalShape h `shouldBe` Nothing
        hs  -> expectationFailure ("expected exactly one hole, got " <> show (length hs))

    -- A hole checked directly against an extension type shows its boundary in
    -- the goal (the extension type is a real restricted type, carried in
    -- holeGoal — not a shape, so holeGoalShape stays empty).
    it "shows the extension-type boundary in the goal" $ do
      case holesOf "#lang rzk-1\n#define t : (A : U) -> (a : A) -> (t : 2) -> A [ t === 0_2 |-> a ]\n  := \\ A a t -> ?\n" of
        [h] -> do
          holeGoalShape h `shouldBe` Nothing
          let goal = show (holeGoal h)
          ("A [" `isInfixOf` goal) `shouldBe` True   -- a restricted type, not bare A
          ("↦ a" `isInfixOf` goal) `shouldBe` True   -- the boundary face is present
        hs  -> expectationFailure ("expected exactly one hole, got " <> show (length hs))

    -- A pair-pattern binder \ (t , s) -> ? restores the user's component names:
    -- the goal and tope context show t / s, not projections (π₁ / π₂) of a fresh
    -- variable. This is what the game and LSP hole panels display.
    it "restores pair-pattern binder names in the goal and topes" $ do
      case holesOf "#lang rzk-1\n#define test : (A : U) -> (x : A) -> ( (t , s) : 2 * 2 | s <= t ) -> A [ t === s |-> x ]\n  := \\ A x (t , s) -> ?\n" of
        [h] -> do
          let goal = show (holeGoal h)
          ("t ≡ s" `isInfixOf` goal) `shouldBe` True
          ('π' `elem` goal) `shouldBe` False
          map show (holeTopes h) `shouldContain` ["s ≤ t"]
          -- the cube variable is shown as the pattern, not a fresh variable
          names (holeCubeVars h) `shouldBe` ["(t, s)"]
        hs  -> expectationFailure ("expected exactly one hole, got " <> show (length hs))

    -- A nested tuple pattern ((t , s) , r) restores all the component names,
    -- including the doubly-projected one (s = π₂ (π₁ x)).
    it "restores nested tuple binder names" $ do
      case holesOf "#lang rzk-1\n#define test : (A : U) -> (x : A) -> ( ((t , s) , r) : (2 * 2) * 2 | r <= s ) -> A [ r === t |-> x ]\n  := \\ A x ((t , s) , r) -> ?\n" of
        [h] -> do
          let goal = show (holeGoal h)
          ("r ≡ t" `isInfixOf` goal) `shouldBe` True
          ('π' `elem` goal) `shouldBe` False
          map show (holeTopes h) `shouldContain` ["r ≤ s"]
          names (holeCubeVars h) `shouldBe` ["((t, s), r)"]
        hs  -> expectationFailure ("expected exactly one hole, got " <> show (length hs))

    -- A bare use of a pattern binder's point (not a projection) inside a shape
    -- in the /goal type/ must print as the pattern, not a fresh variable: the
    -- membership tope of @((t , s) : Δ²) → A@ reads @Δ² (t , s)@, not @Δ² x@.
    it "restores a pattern point used bare in a goal-type shape tope" $ do
      case holesOf "#lang rzk-1\n#def Δ² : (2 × 2) → TOPE := \\ (t , s) → s ≤ t\n#def f (A : U) : ( ((t , s) : Δ²) → A ) := ?\n" of
        [h] -> do
          let goal = show (holeGoal h)
          ("| Δ² (t, s))" `isInfixOf` goal) `shouldBe` True
          ('π' `elem` goal) `shouldBe` False
        hs  -> expectationFailure ("expected exactly one hole, got " <> show (length hs))

    -- Guardrail: ordinary projections of a variable that is NOT a pattern binder
    -- must still print as π₁ / π₂ (only pattern-binder projections are folded).
    it "leaves ordinary projections of a non-pattern variable as π₁ / π₂" $ do
      case holesOf "#lang rzk-1\n#define test : (A : U) -> (x : A) -> ( p : 2 * 2 | second p <= first p ) -> A [ first p === second p |-> x ]\n  := \\ A x p -> ?\n" of
        [h] -> do
          let goal = show (holeGoal h)
          ("π₁ p ≡ π₂ p" `isInfixOf` goal) `shouldBe` True
          map show (holeTopes h) `shouldContain` ["π₂ p ≤ π₁ p"]
        hs  -> expectationFailure ("expected exactly one hole, got " <> show (length hs))

  describe "holeCandidates (type-directed elimination candidates)" $ do
    let cands = map show . holeCandidates

    -- A hypothesis that already has the goal type is offered as-is.
    it "offers a hypothesis of the goal type directly" $ do
      case holesOf "#lang rzk-1\n#define f : (A : U) -> A -> A\n  := \\ A a -> ?\n" of
        [h] -> cands h `shouldContain` ["a"]
        hs  -> expectationFailure ("expected exactly one hole, got " <> show (length hs))

    -- A function is offered fully applied, with its argument left as a hole;
    -- the under-applied function itself is not a move.
    it "applies a function, leaving the argument as a hole" $ do
      case holesOf "#lang rzk-1\n#define h : (A : U) -> (B : U) -> (A -> B) -> A -> B\n  := \\ A B g a -> ?\n" of
        [h] -> cands h `shouldContain` ["g ?"]
        hs  -> expectationFailure ("expected exactly one hole, got " <> show (length hs))

    -- A Σ-typed hypothesis is eliminated by projection to reach the goal.
    it "projects a Σ-typed hypothesis to reach the goal" $ do
      case holesOf "#lang rzk-1\n#define k : (A : U) -> (s : Σ (a : A) , A) -> A\n  := \\ A s -> ?\n" of
        [h] -> do
          cands h `shouldContain` ["π₁ s"]
          cands h `shouldContain` ["π₂ s"]
        hs  -> expectationFailure ("expected exactly one hole, got " <> show (length hs))

    -- A path is eliminated by path induction. The motive is introduced straight
    -- away as a λ, so over @p : a =_A x@ the spine @idJ (A, a, \ b → \ q → ?, ?,
    -- x, p)@ has a result type that β-reduces to a hole and so fits any goal —
    -- here an ordinary goal @A@ with the path in scope.
    it "eliminates a path by idJ, introducing the motive λ" $ do
      case holesOf "#lang rzk-1\n#def f (A : U) (a x : A) (p : a =_{A} x) : A := ?\n" of
        [h] -> cands h `shouldContain` ["idJ (A, a, \\ b → \\ q → ?, ?, x, p)"]
        hs  -> expectationFailure ("expected exactly one hole, got " <> show (length hs))

    -- With no path in scope there is nothing to eliminate by idJ.
    it "does not offer idJ without a path hypothesis" $ do
      case holesOf "#lang rzk-1\n#def f (A : U) (a : A) : A := ?\n" of
        [h] -> filter (isInfixOf "idJ") (cands h) `shouldBe` []
        hs  -> expectationFailure ("expected exactly one hole, got " <> show (length hs))

    -- A partial application whose result structurally mismatches the goal is not
    -- offered: matching uses structural (not fully lenient) hole unification, so
    -- the hole in @h ?@ : @P ?@ cannot excuse the mismatch with the goal @Q@.
    it "does not offer a structurally mismatched partial application" $ do
      case holesOf "#lang rzk-1\n#define t : (A : U) -> (P : A -> U) -> (Q : U) -> (h : (a : A) -> P a) -> Q\n  := \\ A P Q h -> ?\n" of
        [h] -> cands h `shouldBe` []
        hs  -> expectationFailure ("expected exactly one hole, got " <> show (length hs))

    -- In a contradictory tope context (here the shape @t ≡ 0₂ ∧ t ≡ 1₂@, which
    -- entails ⊥) recBOT inhabits the goal, so it is offered as a candidate.
    it "offers recBOT in a contradictory tope context" $ do
      case holesOf "#lang rzk-1\n#def f (A : U) (a : A) : ( (t : 2 | t ≡ 0₂ ∧ t ≡ 1₂) → A )\n  := \\ t → ?\n" of
        [h] -> cands h `shouldContain` ["recBOT"]
        hs  -> expectationFailure ("expected exactly one hole, got " <> show (length hs))

    -- A consistent tope context does not offer recBOT.
    it "does not offer recBOT in a consistent tope context" $ do
      case holesOf "#lang rzk-1\n#def f (A : U) (a : A) : ( (t : 2 | t ≡ 0₂) → A )\n  := \\ t → ?\n" of
        [h] -> cands h `shouldNotContain` ["recBOT"]
        hs  -> expectationFailure ("expected exactly one hole, got " <> show (length hs))

    -- In a shape setting (a cube variable in scope) the goal can be built by a
    -- tope case split; the generic two-way recOR is always available there.
    it "offers a generic recOR split in a shape setting" $ do
      case holesOf "#lang rzk-1\n#def Δ¹ : 2 → TOPE := \\ t → TOP\n#def hom (A : U) (x y : A) : U\n  := (t : Δ¹) → A [ t ≡ 0₂ ↦ x , t ≡ 1₂ ↦ y ]\n#def mor (A : U) (x y : A) : hom A x y := \\ t → ?\n" of
        hs | (h:_) <- reverse hs -> cands h `shouldContain` ["recOR (? ↦ ?, ? ↦ ?)"]
        _ -> expectationFailure "expected at least one hole"

    -- recOR and recBOT are term-level eliminators, so a hole whose goal is a cube
    -- point or a tope is never offered them — even in a setting (a cube variable
    -- in scope) that does offer a recOR split for a term goal.
    let recCands = filter (\s -> isInfixOf "recOR" s || isInfixOf "recBOT" s) . cands
    it "does not offer recOR/recBOT for a cube goal" $
      -- `?` is a cube point (goal `2`) fed to a shape function, with a cube
      -- variable `s` in scope (which would offer a recOR split for a term goal).
      case holesOf "#lang rzk-1\n#def c (A : U) (s : 2) (g : (t : 2) -> A) : A := g ?\n" of
        [h] -> recCands h `shouldBe` []
        hs  -> expectationFailure ("expected exactly one hole, got " <> show (length hs))
    it "does not offer recOR/recBOT for a tope goal" $
      case holesOf "#lang rzk-1\n#def p (A : U) (t : 2) : TOPE := ?\n" of
        [h] -> recCands h `shouldBe` []
        hs  -> expectationFailure ("expected exactly one hole, got " <> show (length hs))

    -- An ordinary, tope-free goal offers no recOR split.
    it "offers no recOR split for a tope-free goal" $ do
      case holesOf "#lang rzk-1\n#def plain (A : U) (a : A) : A := ?\n" of
        [h] -> filter (isInfixOf "recOR") (cands h) `shouldBe` []
        hs  -> expectationFailure ("expected exactly one hole, got " <> show (length hs))

    -- On the boundary t ≡ 0₂ ∨ t ≡ 1₂ the faces cover the context, so the goal's
    -- restriction faces become a concrete recOR split.
    it "splits on the goal's faces when they cover the context" $ do
      case holesOf "#lang rzk-1\n#def bdry (A : U) (x y : A)\n  : ( (t : 2 | t ≡ 0₂ ∨ t ≡ 1₂) → A [ t ≡ 0₂ ↦ x , t ≡ 1₂ ↦ y ] )\n  := \\ t → ?\n" of
        [h] -> cands h `shouldContain` ["recOR (t ≡ 0₂ ↦ ?, t ≡ 1₂ ↦ ?)"]
        hs  -> expectationFailure ("expected exactly one hole, got " <> show (length hs))

  describe "holeCandidates with allow-listed lemmas (withHintLemmas)" $ do
    let cands = map show . holeCandidates
        -- two top-level lemmas and an off-type one; the goal is the type @B@.
        src = "#lang rzk-1\n#postulate B : U\n#postulate C : U\n"
           <> "#postulate concat : B -> B -> B\n#postulate rev : B -> B\n"
           <> "#postulate elsewhere : C\n#define goal : B := ?\n"
        oneHole f hs = case hs of
          [h] -> f h
          _   -> expectationFailure ("expected exactly one hole, got " <> show (length hs))

    -- A top-level definition is not a hole candidate unless it is allow-listed.
    it "does not offer a top-level lemma that was not allow-listed" $
      flip oneHole (holesWithLemmas [] src) $ \h ->
        filter (isInfixOf "concat") (cands h) `shouldBe` []

    -- An allow-listed lemma is offered, applied to holes (fit to the goal).
    it "offers an allow-listed lemma applied to holes" $
      flip oneHole (holesWithLemmas ["concat"] src) $ \h ->
        cands h `shouldContain` ["concat ? ?"]

    -- Each allow-listed lemma is offered independently.
    it "offers several allow-listed lemmas" $
      flip oneHole (holesWithLemmas ["concat", "rev"] src) $ \h -> do
        cands h `shouldContain` ["concat ? ?"]
        cands h `shouldContain` ["rev ?"]

    -- The allow-list is still fit-filtered: a lemma whose type cannot reach the
    -- goal (here @elsewhere : C@ against goal @B@) is not offered.
    it "does not offer an allow-listed lemma whose type does not fit" $
      flip oneHole (holesWithLemmas ["elsewhere"] src) $ \h ->
        filter (isInfixOf "elsewhere") (cands h) `shouldBe` []

    -- Lemmas feed the candidate list only, not the local context: an allow-listed
    -- global does not appear among the hole's term/cube variables.
    it "keeps allow-listed lemmas out of the local context" $
      flip oneHole (holesWithLemmas ["concat"] src) $ \h ->
        (names (holeTermVars h) <> names (holeCubeVars h)) `shouldNotContain` ["concat"]

    -- A lemma fully applied to many arguments is offered: filling a function's
    -- arguments with holes is a forced spine step that does not spend the search
    -- budget, so the eight-argument `deep` reaches the goal even though its spine
    -- is longer than `maxEliminationDepth`. (Regression: argument count used to be
    -- charged against the bound, silently dropping deep lemmas.)
    it "offers a lemma applied to more arguments than maxEliminationDepth" $
      let deepSrc = "#lang rzk-1\n#postulate A : U\n#postulate B : U\n"
                 <> "#postulate deep : A -> A -> A -> A -> A -> A -> A -> A -> B\n"
                 <> "#define goal : B := ?\n"
      in flip oneHole (holesWithLemmas ["deep"] deepSrc) $ \h ->
           cands h `shouldContain` ["deep ? ? ? ? ? ? ? ?"]

    -- A lemma is never offered below its meta prefix (here X and Y, so two
    -- arguments): an unsaturated schema is not a suggestion, mirroring the
    -- warn-meta-prefix discipline.
    it "does not offer a lemma below its meta prefix" $
      let metaSrc = "#lang rzk-1\n#postulate A : U\n"
                 <> "#postulate pick : (X : U) -> (Y : U) -> X -> X\n"
                 <> "#define goal : A -> A := ?\n"
      in flip oneHole (holesWithLemmas ["pick"] metaSrc) $ \h -> do
           cands h `shouldContain` ["pick ? ?"]
           filter (`elem` ["pick", "pick ?"]) (cands h) `shouldBe` []

    -- Also at the schema's own type: the bare alias is not offered (writing
    -- the alias is the user's call), while the saturated spine still is.
    it "does not offer a bare schema even at its own type" $
      let aliasSrc = "#lang rzk-1\n"
                  <> "#define my-id (X : U) (x : X) : X := x\n"
                  <> "#define goal : (X : U) -> X -> X := ?\n"
      in flip oneHole (holesWithLemmas ["my-id"] aliasSrc) $ \h -> do
           cands h `shouldContain` ["my-id ?"]
           filter (== "my-id") (cands h) `shouldBe` []

  describe "holeCandidates under shadowing" $ do
    let cands = map show . holeCandidates
        -- the motive λ rebinds b, so at the inner hole the telescope's b is
        -- shadowed and the λ's b displays as b₁.
        src = "#lang rzk-1\n"
           <> "#def transport (A : U) (B : A → U) (x y : A) (p : x = y) (b : B x)\n"
           <> "  : B y\n"
           <> "  := (idJ (A, x, \\ b → \\ q → x = ?, ?, y, p))\n"

    -- A candidate is inserted as source text, so it is rendered with source
    -- names where they resolve: the motive's λ-bound b (displayed as b₁
    -- beside the shadowed telescope b) is offered as b, which is what the
    -- written b resolves to at the hole. The undefined b₁ never appears.
    it "renders a shadowed-context candidate by its source name" $ do
      case holesOf src of
        [h1, _h2] -> do
          filter (isInfixOf "b₁") (cands h1) `shouldBe` []
          cands h1 `shouldContain` ["x"]
          cands h1 `shouldContain` ["y"]
          cands h1 `shouldContain` ["b"]
        hs -> expectationFailure ("expected exactly two holes, got " <> show (length hs))

    -- The converse: a hypothesis that fits the goal but is shadowed out of
    -- the source namespace is not offered — writing its name would resolve
    -- to the inner binder, a different term.
    it "does not offer a hypothesis that is shadowed out" $ do
      case holesOf ("#lang rzk-1\n"
                 <> "#def t (A : U) (B : A → U) (x : A) (b : B x) : A → B x\n"
                 <> "  := \\ b → ?\n") of
        [h] -> cands h `shouldBe` []
        hs  -> expectationFailure ("expected exactly one hole, got " <> show (length hs))

    -- The idJ move's own motive binders are freshened against the names in
    -- scope: at transport's initial hole (b : B x in the telescope), the
    -- offered motive binds b₁, not a shadowing b. Accepting the move then
    -- never sets up the shadowing of the previous test in the first place.
    it "freshens the idJ motive binders against the telescope" $ do
      let src0 = "#lang rzk-1\n"
              <> "#def transport (A : U) (B : A → U) (x y : A) (p : x = y) (b : B x)\n"
              <> "  : B y\n"
              <> "  := ?\n"
      case holesOf src0 of
        [h] -> cands h `shouldBe` ["idJ (A, x, \\ b₁ → \\ q → ?, ?, y, p)"]
        hs  -> expectationFailure ("expected exactly one hole, got " <> show (length hs))

    -- A destructuring pattern's hidden scrutinee must not claim a
    -- user-facing name: it used to take x₁ from the default supply, so a
    -- user-written x₁ bound later displayed as x₂ (a context/source
    -- mismatch) and its moves were dropped as unreferable. The scrutinee's
    -- placeholder is now the pattern itself, which cannot collide, so the
    -- context shows x₁ and its candidate survives.
    it "keeps a user x₁ intact beside a destructuring pattern" $ do
      case holesOf ("#lang rzk-1\n#data Void\n"
                 <> "#data coprod (A B : U) := inl (a : A) | inr (b : B)\n"
                 <> "#def prod (A B : U) : U := Σ (a : A) , B\n"
                 <> "#def neg (A : U) : U := A → Void\n"
                 <> "#def t (A B : U) : prod (neg A) (neg B) → coprod A B → coprod A B\n"
                 <> "  := \\ (na , nb) → \\ x₁ → ?\n") of
        [h] -> do
          names (holeTermVars h) `shouldContain` ["x₁"]
          names (holeTermVars h) `shouldNotContain` ["x₂"]
          cands h `shouldContain` ["x₁"]
        hs  -> expectationFailure ("expected exactly one hole, got " <> show (length hs))

    -- The same shape at a Void goal, with the coproduct eliminators: the
    -- moves survive the parse-back check, and every motive binder is named
    -- in the move, freshened past the in-scope x₁ (an accepted eliminator
    -- must not shadow the variable it just eliminated).
    it "offers the coproduct eliminators with freshened motive binders" $ do
      case holesOf ("#lang rzk-1\n#data Void\n"
                 <> "#data coprod (A B : U) := inl (a : A) | inr (b : B)\n"
                 <> "#def prod (A B : U) : U := Σ (a : A) , B\n"
                 <> "#def neg (A : U) : U := A → Void\n"
                 <> "#def t (A B : U) : prod (neg A) (neg B) → coprod A B → Void\n"
                 <> "  := \\ (na , nb) → \\ x₁ → ?\n") of
        [h] -> do
          cands h `shouldContain` ["rec-coprod A B ? ? ? x₁"]
          cands h `shouldContain` ["ind-coprod A B (\\ x₂ → ?) ? ? x₁"]
          cands h `shouldContain` ["ind-Void (\\ x₂ → ?) (na ?)"]
          filter (isInfixOf "\\ x₁") (cands h) `shouldBe` []
        hs  -> expectationFailure ("expected exactly one hole, got " <> show (length hs))

    -- A whole-point use of a pattern-bound variable renders as the pattern
    -- itself, which is also the pair expression a move may insert.
    it "renders a whole-point pattern use as the pattern" $ do
      case holesOf "#lang rzk-1\n#def t (A B : U) : (Σ (a : A) , B) → Σ (a : A) , B\n  := \\ (x , y) → ?\n" of
        [h] -> cands h `shouldContain` ["(x, y)"]
        hs  -> expectationFailure ("expected exactly one hole, got " <> show (length hs))

  describe "holeIntroductions (type-directed introduction forms)" $ do
    let intros = map show . holeIntroductions

    -- A function goal is introduced by a λ over a hole body; the binder is
    -- taken from the type, so a named domain keeps its name.
    it "introduces a function goal as a λ with a hole body" $ do
      case holesOf "#lang rzk-1\n#define f : (A : U) -> ((n : A) -> A)\n  := \\ A -> ?\n" of
        [h] -> intros h `shouldBe` ["\\ n → ?"]
        hs  -> expectationFailure ("expected exactly one hole, got " <> show (length hs))

    -- The λ binder is freshened so it does not shadow a name already in scope.
    -- Here the goal unfolds to `(t : 2) -> A`, whose binder `t` (taken from
    -- `endo`'s definition, mirroring `hom`) clashes with the in-scope cube
    -- variable `t`; the introduction binds `t₁` instead of a shadowing `t`.
    it "freshens the λ binder so it does not shadow an in-scope name" $ do
      case holesOf "#lang rzk-1\n#define endo : U -> U\n  := \\ A -> (t : 2) -> A\n#define f : (A : U) -> (t : 2) -> endo A\n  := \\ A t -> ?\n" of
        [h] -> intros h `shouldBe` ["\\ t₁ → ?"]
        hs  -> expectationFailure ("expected exactly one hole, got " <> show (length hs))

    -- An anonymous Π domain is named inside the move, not by the renderer, so
    -- the freshening sees the choice: a λ binder `x₁` already introduced by
    -- the user (or by a previous introduction tap) is not shadowed — the next
    -- introduction binds `x₂`. (Regression: the anonymous binder used to be
    -- named at render time, so two successive taps on `B → A → C` both bound
    -- `x₁`, making the outer binder inaccessible.)
    it "freshens an anonymous λ binder against an enclosing λ binder" $ do
      case holesOf "#lang rzk-1\n#def f (A B C : U) : B → A → C := \\ x₁ → ?\n" of
        [h] -> intros h `shouldBe` ["\\ x₂ → ?"]
        hs  -> expectationFailure ("expected exactly one hole, got " <> show (length hs))

    it "freshens an anonymous λ binder against a telescope binder" $ do
      case holesOf "#lang rzk-1\n#def g (A B : U) (x₁ : B) : A → B := ?\n" of
        [h] -> intros h `shouldBe` ["\\ x₂ → ?"]
        hs  -> expectationFailure ("expected exactly one hole, got " <> show (length hs))

    -- A named pattern domain (e.g. a cube point) keeps its pattern, so the λ
    -- binds the user's names rather than a projection.
    it "introduces a pattern-domain function with the pattern binder" $ do
      case holesOf "#lang rzk-1\n#define f : (A : U) -> ( ((t , s) : 2 × 2) -> A )\n  := \\ A -> ?\n" of
        [h] -> intros h `shouldBe` ["\\ (t, s) → ?"]
        hs  -> expectationFailure ("expected exactly one hole, got " <> show (length hs))

    -- A nameless product domain is destructured by default, recursively, with
    -- cube-point leaves named tN.
    it "destructures a nameless cube-product domain" $ do
      case holesOf "#lang rzk-1\n#def f (A : U) : ( (2 × 2 × 2) → A ) := ?\n" of
        [h] -> intros h `shouldBe` ["\\ ((t1, t2), t3) → ?"]
        hs  -> expectationFailure ("expected exactly one hole, got " <> show (length hs))

    -- A nameless Σ domain is destructured too, with term leaves named xN.
    it "destructures a nameless Σ domain" $ do
      case holesOf "#lang rzk-1\n#def f (A B C : U) : ( (Σ (a : A) , B) → C ) := ?\n" of
        [h] -> intros h `shouldBe` ["\\ (x1, x2) → ?"]
        hs  -> expectationFailure ("expected exactly one hole, got " <> show (length hs))

    -- A Σ-type goal is introduced by a pair of holes.
    it "introduces a Σ goal as a pair of holes" $ do
      case holesOf "#lang rzk-1\n#define f : (A : U) -> (B : A -> U) -> (a : A) -> (b : B a) -> Σ (w : A) , B w\n  := \\ A B a b -> ?\n" of
        [h] -> intros h `shouldBe` ["(?, ?)"]
        hs  -> expectationFailure ("expected exactly one hole, got " <> show (length hs))

    -- A U-goal is introduced by the type formers: a function type, a Σ-type,
    -- an identity type, the unit type, and every user-declared datatype in
    -- scope, applied to holes through its parameter telescope.
    it "introduces a U goal by the type formers, including datatypes" $ do
      case holesOf ("#lang rzk-1\n#data bool := false | true\n"
                 <> "#data coprod (A B : U) := inl (a : A) | inr (b : B)\n"
                 <> "#def T : U := ?\n") of
        [h] -> intros h `shouldBe`
                 ["? → ?", "Σ (x : ?), ?", "? =_{ ? } ?", "Unit", "bool", "coprod ? ?"]
        hs  -> expectationFailure ("expected exactly one hole, got " <> show (length hs))

    -- The Σ introduction's binder is freshened like a λ's.
    it "freshens the Σ introduction's binder against the context" $ do
      case holesOf "#lang rzk-1\n#def T (x : Unit) : U := ?\n" of
        [h] -> intros h `shouldContain` ["Σ (x₁ : ?), ?"]
        hs  -> expectationFailure ("expected exactly one hole, got " <> show (length hs))

    -- An identity type whose endpoints already agree is introduced by refl.
    it "introduces an identity type with agreeing endpoints by refl" $ do
      case holesOf "#lang rzk-1\n#define f : (A : U) -> (a : A) -> (a =_{A} a)\n  := \\ A a -> ?\n" of
        [h] -> intros h `shouldBe` ["refl"]
        hs  -> expectationFailure ("expected exactly one hole, got " <> show (length hs))

    -- refl is conditional: it is not offered when the endpoints need not agree.
    it "does not offer refl when the endpoints need not agree" $ do
      case holesOf "#lang rzk-1\n#define f : (A : U) -> (a : A) -> (b : A) -> (a =_{A} b)\n  := \\ A a b -> ?\n" of
        [h] -> intros h `shouldBe` []
        hs  -> expectationFailure ("expected exactly one hole, got " <> show (length hs))

    -- The unit type is introduced by unit.
    it "introduces the unit type by unit" $ do
      case holesOf "#lang rzk-1\n#define f : Unit\n  := ?\n" of
        [h] -> intros h `shouldBe` ["unit"]
        hs  -> expectationFailure ("expected exactly one hole, got " <> show (length hs))

    -- A goal whose type has no head constructor to introduce (a neutral
    -- application) offers no introduction.
    it "offers no introduction for a neutral goal" $ do
      case holesOf "#lang rzk-1\n#define f : (A : U) -> (B : A -> U) -> (a : A) -> B a\n  := \\ A B a -> ?\n" of
        [h] -> intros h `shouldBe` []
        hs  -> expectationFailure ("expected exactly one hole, got " <> show (length hs))

    -- A shape goal (a hole of type TOPE) is introduced by every tope
    -- constructor, so a shape can be built up by tapping.
    it "introduces a tope goal by every tope constructor" $ do
      case holesOf "#lang rzk-1\n#define sh : 2 -> TOPE\n  := \\ t -> ?\n" of
        [h] -> do
          show (holeGoal h) `shouldBe` "TOPE"
          intros h `shouldBe` ["⊤", "⊥", "? ≡ ?", "? ≤ ?", "? ∧ ?", "? ∨ ?"]
        hs  -> expectationFailure ("expected exactly one hole, got " <> show (length hs))

  -- In lenient (hole-checking) mode a partial proof should still typecheck even
  -- when it has not yet applied a declared variable: the unfilled hole may be
  -- where that variable will eventually be used. We tolerate the unused-variable
  -- diagnostics only while such a hole is present; strict mode (the YAML
  -- ill-unused-* fixtures) still reports them.
  describe "unused-variable diagnostics tolerated in lenient hole mode" $ do
    let section body = "#lang rzk-1\n#section sec\n#variable A : U\n"
                    <> "#variable unused-var : A\n" <> body <> "\n#end sec\n"

    it "tolerates an unused section assumption when the section has a hole" $
      errTagsOf (section "#define only-type : U\n  := ?")
        `shouldNotContain` ["TypeErrorUnusedVariable"]

    it "still reports an unused section assumption with no hole present" $
      errTagsOf (section "#define only-type : U\n  := A")
        `shouldContain` ["TypeErrorUnusedVariable"]

    let usesSection body = "#lang rzk-1\n#section sec\n#variable A : U\n"
                        <> "#variable x : A\n" <> body <> "\n#end sec\n"

    it "tolerates an unused 'uses' variable when the declaration has a hole" $
      errTagsOf (usesSection "#define f uses (x) : U\n  := ?")
        `shouldNotContain` ["TypeErrorUnusedUsedVariables"]

    it "still reports an unused 'uses' variable with no hole in the declaration" $
      errTagsOf (usesSection "#define f uses (x) : U\n  := A")
        `shouldContain` ["TypeErrorUnusedUsedVariables"]

    -- A hole-free definition whose body refers to an in-progress (hole-bearing)
    -- one declares 'uses (x)' that reads as unused only because the referenced
    -- definition is incomplete. The section has a hole, so it is tolerated (the
    -- check keys off a hole anywhere in the section, not the declaration alone).
    it "tolerates an unused 'uses' on a hole-free wrapper of a hole-bearing def" $
      errTagsOf (usesSection ("#define in-progress uses (x) : U\n  := ?\n"
                           <> "#define wrapper uses (x) : U\n  := in-progress"))
        `shouldNotContain` ["TypeErrorUnusedUsedVariables"]

  -- The goal cell: when the goal is a renderable shape, the hole carries an SVG
  -- of that shape, drawn from an abstract inhabitant with the proof term hidden
  -- (see 'renderHideTerm'). So the picture shows the given boundary edges with a
  -- blank interior, never the term that would fill it.
  describe "holeDiagram (goal-cell SVG)" $ do
    it "renders a shape goal as a labelled, term-free SVG" $ do
      let src = "#lang rzk-1\n\
                \#def sq (A : U) (a : A) : U\n\
                \  := ((t , s) : 2 * 2) -> A [ t === 0_2 |-> a , t === 1_2 |-> a , s === 0_2 |-> a , s === 1_2 |-> a ]\n\
                \#def fill (A : U) (a : A) : sq A a := ?\n"
      case holesOf src of
        [h] -> case holeDiagram h of
          Just svg -> do
            ("<svg" `isInfixOf` svg)            `shouldBe` True
            ("<path" `isInfixOf` svg)           `shouldBe` True  -- a 2-cell (square), not just an arrow
            (">a</text>" `isInfixOf` svg)       `shouldBe` True  -- the given boundary edge `a`
            ("<title></title>" `isInfixOf` svg) `shouldBe` True  -- titles blanked: no proof term
          Nothing -> expectationFailure "expected a goal-cell diagram for a shape goal"
        hs -> expectationFailure ("expected exactly one hole, got " <> show (length hs))

    it "has no diagram for a non-shape goal" $
      case holesOf "#lang rzk-1\n#define f : (A : U) -> A -> A\n  := \\ A a -> ?\n" of
        [h] -> holeDiagram h `shouldBe` Nothing
        hs  -> expectationFailure ("expected exactly one hole, got " <> show (length hs))

  describe "inductive types in the inventory (#data)" $ do
    let intros = map show . holeIntroductions
        cands  = map show . holeCandidates
        natPrelude = "#lang rzk-1\n#data nat := zero | suc (n : nat)\n"
        vecPrelude = natPrelude
          <> "#data vec (A : U) : nat → U :=\n"
          <> "    nil : vec A zero\n"
          <> "  | cons (n : nat) (x : A) (xs : vec A n) : vec A (suc n)\n"

    it "introduces a datatype goal by its constructors, applied to holes" $
      case holesOf (natPrelude <> "#define f (m : nat) : nat := ?\n") of
        [h] -> intros h `shouldBe` ["zero", "suc ?"]
        hs  -> expectationFailure ("expected exactly one hole, got " <> show (length hs))

    it "filters constructors whose indices cannot meet the goal's" $
      -- nil : vec A zero cannot fit a goal at index (suc n)
      case holesOf (vecPrelude
          <> "#define g (A : U) (n : nat) (v : vec A (suc n)) : vec A (suc n) := ?\n") of
        [h] -> intros h `shouldBe` ["cons ? ? ? ?"]
        hs  -> expectationFailure ("expected exactly one hole, got " <> show (length hs))

    it "offers the generated eliminators over a datatype hypothesis" $
      -- the motive is a λ over a hole, like idJ's, so the spine fits any goal
      case holesOf (natPrelude <> "#define f (m : nat) : nat := ?\n") of
        [h] -> do
          cands h `shouldContain` ["rec-nat ? ? ? m"]
          cands h `shouldContain` ["ind-nat (\\ x₁ → ?) ? ? m"]
        hs  -> expectationFailure ("expected exactly one hole, got " <> show (length hs))

    it "instantiates the eliminator at the hypothesis's indices" $
      case holesOf (vecPrelude
          <> "#define g (A : U) (n : nat) (v : vec A (suc n)) : vec A (suc n) := ?\n") of
        [h] ->
          cands h `shouldContain` ["ind-vec A (\\ i → \\ x₁ → ?) ? ? (suc n) v"]
        hs  -> expectationFailure ("expected exactly one hole, got " <> show (length hs))

  describe "holes in match branches" $ do
    let natPrelude = "#lang rzk-1\n#data nat := zero | suc (n : nat)\n"

    it "shows the branch binders as hypotheses, with reduced types" $
      -- the induction hypothesis's type is the motive at the field: the
      -- administrative redex is reduced, so it reads "nat", not "(λ …) k"
      case holesOf (natPrelude
          <> "#define plus (n m : nat) : nat\n"
          <> "  := match n (zero ⇒ m | suc k ih ⇒ ?)\n") of
        [h] -> do
          show (holeGoal h) `shouldBe` "nat"
          names (holeTermVars h) `shouldContain` ["k"]
          names (holeTermVars h) `shouldContain` ["ih"]
          map (show . holeEntryType) (holeTermVars h) `shouldBe`
            ["nat", "nat", "nat", "nat"]
        hs  -> expectationFailure ("expected exactly one hole, got " <> show (length hs))

    it "substitutes the constructor form into a dependent goal" $
      -- the motive is built from the goal by abstracting the scrutinee, so a
      -- branch hole's goal is the goal at that constructor (labelled form)
      case holesOf ("#lang rzk-1\n#data bool := false | true\n"
          <> "#define not (b : bool) : bool := match b (false ⇒ true | true ⇒ false)\n"
          <> "#define not-not (b : bool) : not (not b) =_{bool} b\n"
          <> "  := match b (false ⇒ refl | true ⇒ ?)\n") of
        [h] -> show (holeGoal h) `shouldBe` "not (not true) =_{ bool } true"
        hs  -> expectationFailure ("expected exactly one hole, got " <> show (length hs))

    it "keeps a named into-motive as a named application" $
      -- only the administrative redexes are reduced: a motive that is a named
      -- family stays folded in the branch goal
      case holesOf (natPrelude
          <> "#assume C : nat → U\n"
          <> "#define f (n : nat) (c : (m : nat) → C m) : C n\n"
          <> "  := match n into C (zero ⇒ c zero | suc k ih ⇒ ?)\n") of
        [h] -> show (holeGoal h) `shouldBe` "C (suc k)"
        hs  -> expectationFailure ("expected exactly one hole, got " <> show (length hs))

    it "offers a match with one hole per branch over a datatype hypothesis" $
      -- branch binders reuse the declared field names, plus "ih" for an
      -- induction hypothesis, freshened against the names taken at the hole
      case holesOf (natPrelude <> "#define f (m : nat) : nat := ?\n") of
        [h] -> map show (holeCandidates h) `shouldContain`
                 ["match m (zero ⇒ ? | suc n ih ⇒ ?)"]
        hs  -> expectationFailure ("expected exactly one hole, got " <> show (length hs))

    it "does not offer a match over an empty family" $
      -- there is no branch syntax for zero constructors; the eliminators
      -- (ind-void, rec-void) remain
      case holesOf "#lang rzk-1\n#data void : U\n#define h (x : void) : void := ?\n" of
        [h] -> filter (isInfixOf "match") (map show (holeCandidates h)) `shouldBe` []
        hs  -> expectationFailure ("expected exactly one hole, got " <> show (length hs))

  describe "unit-pattern binders (issue #320)" $ do
    it "binds nothing user-visible: empty context, no spurious hypotheses" $
      case holesOf "#lang rzk-1\n#define f : (x y : Unit) → Unit\n  := \\ unit unit → ?\n" of
        [h] -> do
          holeTermVars h `shouldBe` []
          map show (holeCandidates h) `shouldBe` []
          map show (holeIntroductions h) `shouldBe` ["unit"]
        hs  -> expectationFailure ("expected exactly one hole, got " <> show (length hs))

    it "keeps moves that mention the unit-bound point (rendered as unit)" $
      -- the parse-back guard collapses the point to the constructor, so the
      -- idJ moves of is-set-Unit survive, with unit as the endpoints
      case holesOf ("#lang rzk-1\n"
          <> "#define g : (x y : Unit) → (p : x =_{Unit} y) → (q : x =_{Unit} y) → p =_{x =_{Unit} y} q\n"
          <> "  := \\ unit unit p q → ?\n") of
        [h] -> do
          names (holeTermVars h) `shouldBe` ["p", "q"]
          map show (holeCandidates h) `shouldContain`
            ["idJ (Unit, unit, \\ b → \\ q₁ → ?, ?, unit, p)"]
        hs  -> expectationFailure ("expected exactly one hole, got " <> show (length hs))
