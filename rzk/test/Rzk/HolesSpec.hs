{-# LANGUAGE OverloadedStrings #-}
-- | Tests for the structured goal/context query (lenient hole mode). The
-- YAML fixtures cover the strict-mode errors (ill-hole-*); here we check the
-- structured 'HoleInfo' that the game and LSP consume, which substring
-- assertions in the fixture harness cannot pin down (especially the split
-- between term variables and cube variables).
module Rzk.HolesSpec (spec) where

import           Data.List           (isInfixOf)
import qualified Data.Text           as T

import qualified Language.Rzk.Syntax as Rzk
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
      Left err            -> error ("typecheck threw: " <> ppTypeErrorInScopedContext' BottomUp err)
      Right (_, _, holes) -> holes

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
      Right (_, errs, _) -> map typeErrorTagInScopedContext errs

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

    it "records every hole in a module" $ do
      let holes = holesOf "#lang rzk-1\n#define p : (A : U) -> (B : U) -> A -> B -> A\n  := \\ A B a b -> ?\n"
      length holes `shouldBe` 1
      let holes2 = holesOf "#lang rzk-1\n#define q : (A : U) -> A -> A\n  := \\ A a -> ?\n#define r : (A : U) -> A -> A\n  := \\ A a -> ?\n"
      length holes2 `shouldBe` 2

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

    -- A hole nested inside a larger term (`f ?`) checked against an
    -- extension-type boundary: the boundary face is unified against `f ?`, which
    -- must be deferred rather than reported as a mismatch.
    it "handles a nested hole under an extension-type boundary" $ do
      case holesOf "#lang rzk-1\n#define t : (A : U) -> (f : A -> A) -> (a : A) -> (t : 2) -> A [ t === 0_2 |-> a ]\n  := \\ A f a t -> f ?\n" of
        [h] -> show (holeGoal h) `shouldBe` "A"
        hs  -> expectationFailure ("expected exactly one hole, got " <> show (length hs))

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
