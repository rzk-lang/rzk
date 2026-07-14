{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The free-foil core: the surface conversions, α-equivalence, and the typing
-- context.
--
-- The context tests are the load-bearing ones. Entering a binder sinks the
-- context by coercion (see "Rzk.TypeCheck.Context"), which is sound but unsafe if
-- the argument is ever wrong, so these check the things the argument promises:
-- that an entry stays intact and keeps its meaning however deeply it is sunk.
module Rzk.FoilCoreSpec (spec) where

import qualified Control.Monad.Foil       as Foil
import           Control.Monad.Free.Foil  (AST (Var), ScopedAST (..),
                                           alphaEquiv)
import qualified Data.Set                 as Set
import qualified Data.Text                as T
import           Test.Hspec

import           Language.Rzk.Foil.Convert (toTermClosed)
import           Language.Rzk.Foil.Print   (fromTermClosed)
import           Language.Rzk.Foil.Syntax
import           Language.Rzk.Free.Syntax  (Binder (..), TModality (..),
                                           VarIdent, binderName)
import qualified Language.Rzk.Syntax       as Rzk
import           Rzk.TypeCheck.Context
import           Rzk.TypeCheck.Display (namingOfContext, ppName, ppTerm)
import           Rzk.TypeCheck.Error   (OutputDirection (..),
                                        ppTypeErrorInScopedContext)
import           Rzk.TypeCheck.Eval    (nfT, whnfT)
import           Rzk.TypeCheck.Judgements (infer, typecheck)
import           Rzk.TypeCheck.Monad   (runTypeCheck)

-- | Parse a term of the surface syntax, or fail loudly.
parse :: T.Text -> Rzk.Term
parse t =
  case Rzk.parseTerm t of
    Left err   -> error ("cannot parse " <> show t <> ": " <> T.unpack err)
    Right term -> term

-- | Parse a closed term into the core.
core :: T.Text -> Term Foil.VoidS
core = toTermClosed . parse

-- | Parse, elaborate to the core, and print back as surface syntax.
roundTrip :: T.Text -> T.Text
roundTrip = T.pack . Rzk.printTree . fromTermClosed . core

-- | Are two closed terms α-equivalent?
equivalent :: T.Text -> T.Text -> Bool
equivalent l r = alphaEquiv Foil.emptyScope (core l) (core r)

-- | A hypothesis of the given type, with no value and no modality.
hypothesis :: VarIdent -> TermT n -> VarInfo n
hypothesis name ty = VarInfo
  { varType = ty
  , varValue = Nothing
  , varModality = Id
  , varModAccum = Id
  , varOrig = BinderVar (Just name)
  , varIsAssumption = False
  , varIsTopLevel = False
  , varDeclaredAssumptions = []
  , varLocation = Nothing
  }

spec :: Spec
spec = do
  -- The terms are closed (a free variable has no name to resolve to in an empty
  -- scope), so each type-level example binds its own A and B.
  describe "surface syntax round-trips through the core" $ do
    let it_roundTrips t expected =
          it (T.unpack t) $ roundTrip t `shouldBe` expected

    it_roundTrips "\\ x -> x" "\\ x → x"
    it_roundTrips "\\ (t, s) -> t" "\\ (t, s) → t"
    it_roundTrips "\\ A -> \\ (x : A) -> x" "\\ A → \\ (x : A) → x"
    it_roundTrips "\\ A -> \\ B -> (x : A) -> B" "\\ A → \\ B → (x : A) → B"
    it_roundTrips "\\ A -> \\ B -> A -> B" "\\ A → \\ B → A → B"
    it_roundTrips "\\ A -> \\ B -> Sigma (x : A), B" "\\ A → \\ B → Σ (x : A), B"
    it_roundTrips "\\ (t, s) -> first (t, s)" "\\ (t, s) → π₁ (t, s)"
    it_roundTrips "\\ A -> \\ (x : A) -> refl_{x : A}" "\\ A → \\ (x : A) → refl_{ x : A }"
    it_roundTrips "\\ t -> \\ s -> t === s" "\\ t → \\ s → t ≡ s"

  describe "α-equivalence ignores binder names" $ do
    it "\\ x -> x is \\ y -> y" $
      equivalent "\\ x -> x" "\\ y -> y" `shouldBe` True

    -- The old representation derived Eq structurally, so it compared the binder
    -- names too and called these two unequal. This is the behaviour change.
    it "\\ x -> \\ y -> x is not \\ x -> \\ y -> y" $
      equivalent "\\ x -> \\ y -> x" "\\ x -> \\ y -> y" `shouldBe` False

    it "a pair pattern binds one variable, and its projections are right" $ do
      equivalent "\\ (t, s) -> t" "\\ (a, b) -> a" `shouldBe` True
      equivalent "\\ (t, s) -> t" "\\ (a, b) -> b" `shouldBe` False

    it "two holes with different names are different terms" $
      equivalent "?a" "?b" `shouldBe` False

  describe "the context" $ do
    it "resolves a name bound three binders up" $ do
      let ctx0 = emptyContext
      withFreshBinder ctx0 (hypothesis "A" universeT) $ \bA ctxA ->
        withFreshBinder ctxA (hypothesis "x" (Var (Foil.nameOf bA))) $ \_bx ctxx ->
          withFreshBinder ctxx (hypothesis "y" cubeT) $ \_by ctxy -> do
            -- 'A' was bound at the outermost scope and has been sunk twice.
            case lookupNamed "A" ctxy of
              Nothing -> expectationFailure "A is not in scope"
              Just nameA -> do
                let info = lookupVarInfo nameA ctxy
                binderName (varOrig info) `shouldBe` Just "A"
                alphaEquiv (ctxScope ctxy) (untyped (varType info)) (untyped universeT)
                  `shouldBe` True

    it "keeps a sunk hypothesis pointing at the name it was typed by" $ do
      -- x : A, where A is itself a bound name. After sinking x's entry into a
      -- deeper scope, its type must still be *that* name, not a stale or
      -- renamed one: this is what the coercion in 'enterBinder' promises.
      withFreshBinder emptyContext (hypothesis "A" universeT) $ \bA ctxA -> do
        let nameA = Foil.nameOf bA
        withFreshBinder ctxA (hypothesis "x" (Var nameA)) $ \_bx ctxx ->
          withFreshBinder ctxx (hypothesis "y" cubeT) $ \_by ctxy ->
            case lookupNamed "x" ctxy of
              Nothing -> expectationFailure "x is not in scope"
              Just nameX -> do
                let info = lookupVarInfo nameX ctxy
                case untyped (varType info) of
                  Var name -> Foil.nameId name `shouldBe` Foil.nameId nameA
                  _        -> expectationFailure "the type of x is no longer a variable"

    it "lets the outer binder keep its name when an inner one shadows it" $
      -- Display names are claimed oldest binding first (see 'ctxBound'), so the
      -- inner 'x' is the one refreshed. The order cannot be read off the name
      -- ids: free-foil refreshes a binder only on a clash, so after a
      -- substitution an inner binder may carry a smaller id than an outer one.
      withFreshBinder emptyContext (hypothesis "x" universeT) $ \bOuter ctxOuter ->
        withFreshBinder ctxOuter (hypothesis "x" universeT) $ \bInner ctxInner -> do
          let naming = namingOfContext ctxInner
          ppName naming (Foil.sink (Foil.nameOf bOuter)) `shouldBe` "x"
          ppName naming (Foil.nameOf bInner) `shouldNotBe` "x"

    it "records the names in scope for the shadowing check" $
      withFreshBinder emptyContext (hypothesis "A" universeT) $ \_bA ctxA ->
        Set.toList (ctxNameSet ctxA) `shouldBe` ["A" :: VarIdent]

  describe "evaluation" $ do
    -- The identity on the directed interval, applied to an endpoint. A cube-layer
    -- term, so this drives the whole reduction path: whnfT dispatches on the
    -- layer, nfTope reduces the redex, and the argument is substituted into the
    -- body (invalidating the memo on the way).
    let identityOn2 :: TermT Foil.VoidS
        identityOn2 = Foil.withFresh Foil.emptyScope $ \binder ->
          let t = BinderVar (Just "t")
              ty = typeFunT t Id cube2T Nothing (ScopedAST binder cube2T)
           in lambdaT ty t (Just (LambdaParam Id cube2T Nothing))
                (ScopedAST binder (Var (Foil.nameOf binder)))

        applied = appT cube2T identityOn2 cube2_0T

    it "reduces (\\ t -> t) 0₂ to 0₂" $
      case runTypeCheck (whnfT applied) of
        Left err -> expectationFailure (ppTypeErrorInScopedContext TopDown err)
        Right result ->
          alphaEquiv Foil.emptyScope (untyped result) (untyped cube2_0T)
            `shouldBe` True

    it "does not reduce the unapplied identity" $
      case runTypeCheck (whnfT identityOn2) of
        Left err -> expectationFailure (ppTypeErrorInScopedContext TopDown err)
        Right result ->
          alphaEquiv Foil.emptyScope (untyped result) (untyped cube2_0T)
            `shouldBe` False

  describe "the checker" $ do
    -- Parse, elaborate, infer, normalise, and print back: the whole pipeline of
    -- the new core, end to end.
    let inferAndShow :: T.Text -> Either String String
        inferAndShow t =
          case runTypeCheck (infer (core t) >>= nfT) of
            Left err     -> Left (ppTypeErrorInScopedContext TopDown err)
            Right result -> Right (ppTerm (namingOfContext emptyContext) (untyped result))

        checkAndShow :: T.Text -> T.Text -> Either String String
        checkAndShow t ty =
          case runTypeCheck (infer (core ty) >>= typecheck (core t) >>= nfT) of
            Left err     -> Left (ppTypeErrorInScopedContext TopDown err)
            Right result -> Right (ppTerm (namingOfContext emptyContext) (untyped result))

    it "infers and evaluates (\\ (x : Unit) -> x) unit" $
      inferAndShow "(\\ (x : Unit) -> x) unit" `shouldBe` Right "unit"

    it "infers a dependent function type" $
      inferAndShow "(A : U) -> A -> A" `shouldBe` Right "(A : U) → A → A"

    it "checks the identity against its type" $
      -- the elaborated lambda carries the domain it was checked against
      checkAndShow "\\ A -> \\ (x : A) -> x" "(A : U) -> A -> A"
        `shouldBe` Right "\\ (A : U) → \\ (x : A) → x"

    it "rejects a term that does not have the expected type" $
      case checkAndShow "\\ A -> \\ (x : A) -> A" "(A : U) -> A -> A" of
        Left err -> err `shouldContain` "cannot unify"
        Right t  -> expectationFailure ("expected a type error, got " <> t)

    -- A cube domain with a pair pattern: the binder binds one variable whose
    -- components are projections, and the pattern is shown back.
    it "checks a function from a cube point" $
      checkAndShow "\\ A -> \\ (a : A) -> \\ ((t, s) : 2 * 2) -> a"
                   "(A : U) -> (a : A) -> (t : 2 * 2) -> A"
        `shouldBe` Right "\\ (A : U) → \\ (a : A) → \\ ((t, s) : 2 × 2) → a"
