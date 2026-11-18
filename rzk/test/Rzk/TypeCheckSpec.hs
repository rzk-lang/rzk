{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Rzk.TypeCheckSpec (spec) where

import           Control.Monad            (forM, unless, when)
import qualified Control.Monad.Foil       as Foil
import           Control.Monad.Free.Foil  (AST (..), ScopedAST (..))
import           Data.Aeson               (FromJSON, withObject, (.:), (.:?))
import           Data.Bifunctor           (first)
import           Data.List                (isInfixOf, isPrefixOf)
import qualified Data.Text                as T
import qualified Data.Text.IO             as T
import qualified Data.Yaml                as Yaml
import           System.Directory         (doesFileExist)
import           System.Environment       (lookupEnv)
import           System.FilePath          (dropExtension, (</>), takeDirectory,
                                           takeFileName)
import           System.FilePath.Glob     (compile, globDir1)

import           Data.List                (sort)

import           Language.Rzk.Foil.Names    (TModality (..))
import qualified Language.Rzk.Syntax        as Rzk
import qualified Rzk.Check                as Check
import qualified Rzk.Check.Display        as CheckDisplay
import qualified Rzk.Check.Syntax         as CheckSyntax
import qualified Rzk.Check.Value          as CheckValue
import           Rzk.Diagnostic           (checkWarningTag,
                                           locationOfTypeError,
                                           typeErrorTagInScopedContext)
import           Rzk.TypeCheck            (Checked, Context (..),
                                           LocationInfo (..),
                                           OutputDirection (..),
                                           TypeErrorInScopedContext,
                                           Verbosity (..), checkedErrors,
                                           checkedModules, checkedWarnings,
                                           emptyContext,
                                           ppTypeErrorInScopedContext)

import           Test.Hspec

data Expect = Expect
  { expectStatus          :: String
  , expectErrorTag        :: Maybe String
  , expectMessageContains :: Maybe [String]
  , expectLine            :: Maybe Int
  , expectColumn          :: Maybe Int
  , expectErrorCount      :: Maybe Int
  , expectRegressionFor   :: Maybe [String]
  , expectModules         :: Maybe [FilePath]
  , expectApi             :: Maybe String
  , expectWarnings        :: Maybe [String]
  } deriving (Show, Eq)

instance FromJSON Expect where
  parseJSON = withObject "Expect" $ \o -> Expect
    <$> o .:  "status"
    <*> o .:? "error_tag"
    <*> o .:? "message_contains"
    <*> o .:? "line"
    <*> o .:? "column"
    <*> o .:? "error_count"
    <*> o .:? "regression_for"
    <*> o .:? "modules"
    <*> o .:? "api"
    <*> o .:? "warnings"

-- | The line an error was raised on.
--
-- An error carries the context it was raised in, so there are no binder layers to
-- peel: the old representation nested the error one Inc deeper at every binder, and
-- this had to unsafeCoerce its way back out.
errorLine :: TypeErrorInScopedContext -> Maybe Int
errorLine err = locationOfTypeError err >>= locationLine

-- | The column an error was raised at: the start of the sub-term it is about.
errorColumn :: TypeErrorInScopedContext -> Maybe Int
errorColumn err = locationOfTypeError err >>= locationColumn

-- | The constructor name of a type error, used to match @error_tag@ in fixtures.
-- This is the library's own tag (also used for diagnostic codes), so the two cannot
-- drift apart.
typeErrorConstructorName :: TypeErrorInScopedContext -> String
typeErrorConstructorName = typeErrorTagInScopedContext

casesRoot :: FilePath
casesRoot = "test/typecheck/cases"

-- | When running tests from a different cwd (e.g. some CI), set @RZK_TEST_ROOT@ to the @rzk/@ package directory.
resolveRoot :: IO FilePath
resolveRoot = do
  m <- lookupEnv "RZK_TEST_ROOT"
  case m of
    Just root | not (null root) -> pure root
    _                           -> pure "."

globExpectPaths :: FilePath -> IO [FilePath]
globExpectPaths root =
  globDir1 (compile "**/*.expect.yaml") (root </> casesRoot)

globDirExpectPaths :: FilePath -> IO [FilePath]
globDirExpectPaths root =
  globDir1 (compile "**/expect.yaml") (root </> casesRoot)

readExpect :: FilePath -> IO (Either String Expect)
readExpect path = do
  e <- Yaml.decodeFileEither path
  pure $ first show e

loadModules :: [(FilePath, FilePath)] -> IO (Either String [(FilePath, Rzk.Module)])
loadModules [] = pure (Right [])
loadModules ((relPath, absPath) : rest) = do
  txt <- T.readFile absPath
  case Rzk.parseModule txt of
    Left err -> pure $ Left (T.unpack err)
    Right m  -> fmap (fmap ((relPath, m) :)) $ loadModules rest

-- | Run the checker with @verbosity = Silent@ so @traceTypeCheck Normal@ does not
-- clutter @stack test@ output (the CLI keeps the default @Normal@).
silently :: Context n -> Context n
silently ctx = ctx { ctxVerbosity = Silent }

-- | Strict: the first error stops the run and is returned.
runStrict :: [(FilePath, Rzk.Module)] -> Either TypeErrorInScopedContext Checked
runStrict ms = case checkedModules ms (silently emptyContext) of
  Left err -> Left err
  Right (checked, _holes) -> case checkedErrors checked of
    err : _ -> Left err
    []      -> Right checked

-- | Collect: every error is returned, and the run continues past them.
runCollect :: [(FilePath, Rzk.Module)] -> Either TypeErrorInScopedContext Checked
runCollect ms = fst <$> checkedModules ms (silently emptyContext)

assertExpect :: String -> Expect -> Either TypeErrorInScopedContext Checked -> IO ()
assertExpect _label Expect{..} (Right _) | expectStatus /= "ok" =
  expectationFailure "expected type error (status: error), but typechecking succeeded"
assertExpect label Expect{..} (Left err) | expectStatus == "ok" =
  expectationFailure $ "unexpected type error in " <> label <> ":\n"
    <> ppTypeErrorInScopedContext BottomUp err
assertExpect label Expect{..} (Left err) | expectStatus == "error" = do
  let tag = typeErrorConstructorName err
  case expectErrorTag of
    Nothing -> expectationFailure "expect.yaml missing error_tag for status: error"
    Just want | want /= tag ->
      expectationFailure $ "wrong error constructor in " <> label <> ": wanted "
        <> want <> ", got " <> tag <> "\nFull message:\n"
        <> ppTypeErrorInScopedContext BottomUp err
    Just _ -> pure ()
  case expectMessageContains of
    Nothing -> pure ()
    Just subs -> do
      let msg = ppTypeErrorInScopedContext BottomUp err
      mapM_ (\s -> unless (s `isInfixOf` msg) $
        expectationFailure $ "in " <> label <> ", message missing substring "
          <> show s <> ":\n" <> msg) subs
  case expectLine of
    Nothing -> pure ()
    Just ln -> case errorLine err of
      Nothing -> expectationFailure $ "in " <> label <> ", expected line "
        <> show ln <> " but error has no line"
      Just got | got /= ln ->
        expectationFailure $ "in " <> label <> ", expected line " <> show ln
          <> " got " <> show got
      Just _ -> pure ()
  case expectColumn of
    Nothing -> pure ()
    Just c -> case errorColumn err of
      Nothing -> expectationFailure $ "in " <> label <> ", expected column "
        <> show c <> " but error has no column"
      Just got | got /= c ->
        expectationFailure $ "in " <> label <> ", expected column " <> show c
          <> " got " <> show got
      Just _ -> pure ()
assertExpect label Expect{expectStatus = "ok", expectWarnings = want} (Right checked) =
  assertWarnings label want checked
assertExpect label Expect{expectStatus = st} _ =
  expectationFailure $ "in " <> label <> ", unknown status " <> show st

-- | When the fixture spells a @warnings@ list, the run's warning tags must
-- match it exactly (order-insensitively); an absent field asserts nothing.
assertWarnings :: String -> Maybe [String] -> Checked -> IO ()
assertWarnings _label Nothing _ = pure ()
assertWarnings label (Just want) checked = do
  let got = map checkWarningTag (checkedWarnings checked)
  unless (sort want == sort got) $
    expectationFailure $ "in " <> label <> ", expected warnings "
      <> show want <> " but got " <> show got

assertExpectCollect :: String -> Expect -> Either TypeErrorInScopedContext Checked -> IO ()
assertExpectCollect _label _ (Left err) =
  expectationFailure $ "unexpected fatal error: " <> ppTypeErrorInScopedContext BottomUp err
assertExpectCollect label Expect{..} (Right checked) = case checkedErrors checked of
 errs -> case expectStatus of
  "ok" | not (null errs) ->
    expectationFailure $ "in " <> label <> ", expected ok but got errors: " <> show (length errs)
  "ok" -> assertWarnings label expectWarnings checked
  "error" -> case errs of
    [] ->
      expectationFailure $ "in " <> label <> ", expected type errors but got none"
    err : _ -> do
      case expectErrorCount of
        Nothing -> pure ()
        Just n | length errs /= n ->
          expectationFailure $ "in " <> label <> ", expected " <> show n
            <> " errors, got " <> show (length errs) <> ":\n"
            <> unlines (map (ppTypeErrorInScopedContext BottomUp) errs)
        Just _ -> pure ()
      let tag = typeErrorConstructorName err
      case expectErrorTag of
        Nothing -> expectationFailure "expect.yaml missing error_tag for status: error"
        Just want | want /= tag ->
          expectationFailure $ "wrong error constructor in " <> label <> ": wanted "
            <> want <> ", got " <> tag
        Just _ -> pure ()
      case expectMessageContains of
        Nothing -> pure ()
        Just subs -> do
          let msg = ppTypeErrorInScopedContext BottomUp err
          mapM_ (\s -> unless (s `isInfixOf` msg) $
            expectationFailure $ "in " <> label <> ", message missing substring "
              <> show s) subs
      case expectLine of
        Nothing -> pure ()
        Just ln -> case errorLine err of
          Nothing -> expectationFailure "expected line number on error"
          Just got | got /= ln ->
            expectationFailure $ "line mismatch: wanted " <> show ln <> " got " <> show got
          Just _ -> pure ()
      case expectColumn of
        Nothing -> pure ()
        Just c -> case errorColumn err of
          Nothing -> expectationFailure "expected column on error"
          Just got | got /= c ->
            expectationFailure $ "column mismatch: wanted " <> show c <> " got " <> show got
          Just _ -> pure ()
  st ->
    expectationFailure $ "in " <> label <> ", unknown status " <> show st

data CaseKind
  = PairedCase
      { pcExpectPath :: FilePath
      , pcRzkPath    :: FilePath
      , pcRelTitle   :: String
      }
  | DirCase
      { dcDir        :: FilePath
      , dcExpectPath :: FilePath
      , dcRelTitle   :: String
      }

relTitleFrom :: FilePath -> FilePath -> FilePath
relTitleFrom root absP
  | root == "."                 = absP
  | root `isPrefixOf` absP      = drop (length root + 1) absP
  | ("./" <> root) `isPrefixOf` absP = drop (length root + 3) absP
  | otherwise                   = absP

discoverCases :: FilePath -> IO [CaseKind]
discoverCases root = do
  paired <- globExpectPaths root
  let paired' = [ p | p <- paired, takeFileName p /= "expect.yaml" ]
  pairedKinds <- forM paired' $ \absExpect -> do
    let dir      = takeDirectory absExpect
        fname    = takeFileName absExpect
        baseName = dropExtension (dropExtension fname)
        absRzk   = dir </> baseName <> ".rzk"
        absRzkMd = dir </> baseName <> ".rzk.md"
    absChosen <- do
      rzkOk <- doesFileExist absRzk
      if rzkOk
        then pure absRzk
        else do
          mdOk <- doesFileExist absRzkMd
          if mdOk
            then pure absRzkMd
            else ioError $ userError $
              "Missing twin .rzk or .rzk.md for " <> absExpect
    pure PairedCase
      { pcExpectPath = absExpect
      , pcRzkPath    = absChosen
      , pcRelTitle   = relTitleFrom root absExpect
      }
  dirExpects <- globDirExpectPaths root
  dirKinds <- forM dirExpects $ \absExpect -> do
    let dir = takeDirectory absExpect
    pure DirCase
      { dcDir        = dir
      , dcExpectPath = absExpect
      , dcRelTitle   = relTitleFrom root absExpect
      }
  pure (pairedKinds ++ dirKinds)

runCase :: CaseKind -> IO ()
runCase PairedCase{..} = do
  e <- readExpect pcExpectPath
  expect <- case e of
    Left err -> expectationFailure ("YAML " <> pcExpectPath <> ": " <> err) >> undefined
    Right x  -> pure x
  txt <- T.readFile pcRzkPath
  root <- resolveRoot
  case Rzk.parseModule txt of
    Left perr -> expectationFailure $
      "parse failure in " <> relTitleFrom root pcRzkPath <> ":\n" <> T.unpack perr
    Right m   -> do
      let relModPath = relTitleFrom root pcRzkPath
          mods       = [(relModPath, m)]
      case expectApi expect of
        Just "collect" -> assertExpectCollect pcRelTitle expect (runCollect mods)
        _              -> assertExpect pcRelTitle expect (runStrict mods)
runCase DirCase{..} = do
  e <- readExpect dcExpectPath
  expect <- case e of
    Left err -> expectationFailure ("YAML " <> dcExpectPath <> ": " <> err) >> undefined
    Right x  -> pure x
  root <- resolveRoot
  modRelAbs <- case expectModules expect of
    Nothing -> do
      let inp = dcDir </> "input.rzk"
      ok <- doesFileExist inp
      unless ok $ ioError $ userError $
        "Directory case " <> dcRelTitle <> ": add modules: [...] in expect.yaml or input.rzk"
      pure [(relTitleFrom root inp, inp)]
    Just names -> pure [(relTitleFrom root (dcDir </> n), dcDir </> n) | n <- names]
  modsE <- loadModules modRelAbs
  mods <- either (\msg -> expectationFailure ("load/parse in " <> dcRelTitle <> ": " <> msg) >> undefined) pure modsE
  case expectApi expect of
    Just "collect" -> assertExpectCollect dcRelTitle expect (runCollect mods)
    _              -> assertExpect dcRelTitle expect (runStrict mods)

spec :: Spec
spec = describe "Typecheck fixtures" $ do
  it "discovers at least one case" $ do
    root <- resolveRoot
    ks <- discoverCases root
    (length ks > 0) `shouldBe` True
  describe "cases" $ do
    root <- runIO resolveRoot
    ks <- runIO (discoverCases root)
    mapM_ (\k -> it (caseTitle k) (runCase k)) ks
  -- Every fixture also runs under the new checker ("Rzk.Check"); a fixture
  -- whose constructs the new checker does not cover yet is pending.
  describe "cases under the new checker" $ do
    root <- runIO resolveRoot
    ks <- runIO (discoverCases root)
    mapM_ (\k -> it (caseTitle k) (runCaseNew k)) ks
  it "collects an invalid unset option and a later error" $ do
    let src = T.unlines
          [ "#lang rzk-1"
          , "#unset-option \"frobnitz\""
          , "#def bad (A : U) : A → A := \\ _ → 0₂"
          ]
    case Rzk.parseModule src of
      Left err -> expectationFailure (T.unpack err)
      Right m -> map Check.errorTag (fst (runNew [("<test>", m)]))
        `shouldBe` ["TypeErrorOther", "TypeErrorUnify"]
  it "records simplification hints in the hint log" $ do
    let src = T.unlines
          [ "#lang rzk-1"
          , "#def test : (t : 2 | (\\ s → TOP) t) → TOPE := \\ _ → TOP"
          ]
    case Rzk.parseModule src of
      Left err -> expectationFailure (T.unpack err)
      Right m -> map Check.hintTag (runNewHints [("<test>", m)])
        `shouldBe` ["SimplificationHint"]
  it "warns when an option is ignored" $ do
    let src = T.unlines
          [ "#lang rzk-1"
          , "#set-option \"rstt-safe\" = \"yes\""
          ]
    case Rzk.parseModule src of
      Left err -> expectationFailure (T.unpack err)
      Right m -> map Check.warningTag (snd (runNew [("<test>", m)]))
        `shouldBe` ["UnsupportedOptionWarning"]
  it "reports an invalid pair pattern with its pattern" $ do
    let src = T.unlines
          [ "#lang rzk-1"
          , "#def bad (A : U) : A → A := \\ (x, y) → x"
          ]
    case Rzk.parseModule src of
      Left err -> expectationFailure (T.unpack err)
      Right m -> case fst (runNew [("<test>", m)]) of
        [err] -> do
          Check.errorTag err `shouldBe` "TypeErrorUnexpectedPairPattern"
          Check.ppErrorInContext err `shouldContain` "(x, y)"
        errs -> expectationFailure $
          "expected one pair-pattern error, got " <> show (map Check.errorTag errs)
  it "rejects unit patterns at non-unit types" $ do
    let src = T.unlines
          [ "#lang rzk-1"
          , "#def badLambda (A : U) (x : A) : A → A := \\ unit → x"
          , "#def badLet (A : U) (x : A) : A := let unit := x in x"
          , "#def badNested (A : U) : (Σ (x : A), A) → A := \\ (x, unit) → x"
          ]
    case Rzk.parseModule src of
      Left err -> expectationFailure (T.unpack err)
      Right m -> case fst (runNew [("<test>", m)]) of
        errs@[_, _, _] -> do
          map Check.errorTag errs `shouldBe`
            replicate 3 "TypeErrorUnexpectedUnitPattern"
          mapM_ (\err -> Check.ppErrorInContext err `shouldContain` "unexpected unit pattern") errs
        errs -> expectationFailure $
          "expected three unit-pattern errors, got " <> show (map Check.errorTag errs)
  it "ignores cached types in hole and free-name queries" $ do
    let hole = CheckSyntax.Hole Nothing :: CheckSyntax.Core Foil.VoidS
        ann = CheckSyntax.Ann CheckSyntax.Unit hole
        recOr = CheckSyntax.RecOr hole [(CheckSyntax.TopeTop, CheckSyntax.Unit)]
    CheckSyntax.containsHole ann `shouldBe` False
    CheckSyntax.containsHole recOr `shouldBe` False
    CheckSyntax.freeNamesOf ann `shouldBe` []
    CheckSyntax.freeNamesOf recOr `shouldBe` []
    CheckSyntax.containsHole (CheckSyntax.Ann hole CheckSyntax.Unit) `shouldBe` True
    CheckSyntax.containsHole (CheckSyntax.TypeAsc CheckSyntax.Unit hole) `shouldBe` True
    CheckSyntax.containsHole
      (CheckSyntax.RecOr CheckSyntax.Unit [(CheckSyntax.TopeTop, hole)]) `shouldBe` True
  it "displays a recOR before its type is attached" $ do
    let term = Node (CheckSyntax.RecOrF Nothing [(CheckSyntax.TopeTop, CheckSyntax.Unit)])
          :: CheckSyntax.Core Foil.VoidS
    CheckDisplay.ppCore (CheckDisplay.namingOfContext Check.emptyContext) term
      `shouldContain` "recOR"
  it "does not duplicate a neutral family's domain" $ do
    root <- resolveRoot
    let path = root </> casesRoot </> "ill-tope-family-domain-non-instance.rzk"
    txt <- T.readFile path
    case Rzk.parseModule txt of
      Left err -> expectationFailure (T.unpack err)
      Right m -> case fst (runNew [("<test>", m)]) of
        [err] -> do
          let msg = Check.ppErrorInContext err
          msg `shouldContain` "ψ t ∧ χ t"
          msg `shouldNotContain` "ψ t ∧ ψ t ∧ χ t"
        errs -> expectationFailure $
          "expected one family-domain error, got " <> show (map Check.errorTag errs)
  it "checks restricted codomains against the actual application" $ do
    let src = T.unlines
          [ "#lang rzk-1"
          , "#def const (A : U) (a : A) : (t : 2) → A := \\ t → a"
          , "#def good (A : U) (a : A) : (t : 2) → A [t ≡ 0₂ ↦ a] := const A a"
          , "#def bad (A : U) (a : A) (f : (t : 2) → A)"
          , "  : (t : 2) → A [t ≡ 0₂ ↦ a]"
          , "  := f"
          ]
    case Rzk.parseModule src of
      Left err -> expectationFailure (T.unpack err)
      Right m -> map Check.errorTag (fst (runNew [("<test>", m)]))
        `shouldBe` ["TypeErrorUnify"]
  it "uses the comparison subject in both variances" $ do
    let unit = CheckSyntax.Unit :: CheckSyntax.Core Foil.VoidS
        unitV = CheckValue.evalCore Foil.emptyScope Check.emptyContext unit
        restrictedV = CheckValue.evalCore Foil.emptyScope Check.emptyContext
          (CheckSyntax.TypeRestricted unit [(CheckSyntax.TopeTop, unit)])
    CheckValue.conv Foil.emptyScope
      (Check.withSubject (Just unit) Check.emptyContext) restrictedV unitV `shouldBe` True
    CheckValue.conv Foil.emptyScope
      (Check.withSubject (Just unit) (Check.withVariance Check.Contravariant Check.emptyContext))
      unitV restrictedV `shouldBe` True
  it "treats a missing cube shape as TOP" $ do
    let cubeV = CheckValue.evalCore Foil.emptyScope Check.emptyContext CheckSyntax.Cube2
        top = CheckValue.topShapeValue Foil.emptyScope cubeV
        topC = CheckValue.quoteIn Foil.emptyScope Check.emptyContext top
        family = CheckValue.evalCore Foil.emptyScope Check.emptyContext
          (CheckSyntax.TypeShape Id CheckSyntax.Cube2 Nothing)
        familyTop = CheckValue.evalCore Foil.emptyScope Check.emptyContext
          (CheckSyntax.TypeShape Id CheckSyntax.Cube2 (Just topC))
        piWith mshape = CheckValue.evalCore Foil.emptyScope Check.emptyContext $
          Foil.withFresh Foil.emptyScope $ \binder ->
            CheckSyntax.TypeFun Id CheckSyntax.Cube2 mshape
              (ScopedAST (CheckValue.singlePattern binder) CheckSyntax.Unit)
    CheckValue.conv Foil.emptyScope Check.emptyContext family familyTop `shouldBe` True
    CheckValue.conv Foil.emptyScope Check.emptyContext (piWith Nothing) (piWith (Just topC)) `shouldBe` True
  it "does not invent a shape for an ordinary domain" $ do
    let cubeV = CheckValue.evalCore Foil.emptyScope Check.emptyContext CheckSyntax.Cube2
        topC = CheckValue.quoteIn Foil.emptyScope Check.emptyContext
          (CheckValue.topShapeValue Foil.emptyScope cubeV)
        piWith mshape = CheckValue.evalCore Foil.emptyScope Check.emptyContext $
          Foil.withFresh Foil.emptyScope $ \binder ->
            CheckSyntax.TypeFun Id CheckSyntax.Universe mshape
              (ScopedAST (CheckValue.singlePattern binder) CheckSyntax.Unit)
    CheckValue.conv Foil.emptyScope Check.emptyContext (piWith Nothing) (piWith (Just topC)) `shouldBe` False

-- * The new checker

newOptions :: Check.Options
newOptions = Check.defaultOptions { Check.optVerbosity = Check.Silent }

-- | The outcome of a run under the new checker: the errors it reports, in
-- order, and its warnings.
runNew :: [(FilePath, Rzk.Module)] -> ([Check.ErrorInContext], [Check.Warning])
runNew ms = case Check.checkModules newOptions ms of
  Check.Checked _ _ errs warnings _ _ -> (errs, warnings)

-- | The hints recorded by a run under the new checker.
runNewHints :: [(FilePath, Rzk.Module)] -> [Check.Hint]
runNewHints ms = case Check.checkModules newOptions ms of
  Check.Checked _ _ _ _ hints _ -> hints

-- | Does a fixture exercise the fragment checks (RSTT-safe mode, the
-- meta-prefix, meta-binder, free-standing restriction and shape dependency
-- checks), which the new checker does not run yet?
fragmentFixture :: Expect -> [T.Text] -> Bool
fragmentFixture Expect{..} sources =
  maybe False (== "TypeErrorRSTT") expectErrorTag
    || any (`notElem` ["OverhangWarning", "TopeFamilyDomainWarning"]) (concat expectWarnings)
    || any (\src -> any (`T.isInfixOf` src) fragmentOptions) sources
  where
    fragmentOptions =
      [ "\"rstt-safe\"", "\"warn-meta-prefix\"", "\"warn-meta-binder\""
      , "\"warn-free-standing-restriction\"", "\"warn-shape-dependency\"" ]

assertExpectNew :: String -> Expect -> Bool -> [T.Text] -> ([Check.ErrorInContext], [Check.Warning]) -> IO ()
assertExpectNew label expect@Expect{..} collect sources (errs, warnings) = case errs of
  _ | fragmentFixture expect sources ->
    pendingWith "the fragment checks are not in the new checker yet"
  err : _ | Check.errorTag err == "TypeErrorUnsupported" ->
    pendingWith ("not covered by the new checker yet: " <> Check.ppErrorInContext err)
  _ -> case expectStatus of
    "ok" -> case errs of
      [] -> case expectWarnings of
        Nothing -> pure ()
        Just want -> do
          let got = map Check.warningTag warnings
          unless (sort want == sort got) $
            expectationFailure $ "in " <> label <> ", expected warnings "
              <> show want <> " but got " <> show got
      err : _ -> expectationFailure $ "unexpected type error in " <> label <> ":\n"
        <> Check.ppErrorInContext err
    "error" -> case errs of
      [] -> expectationFailure "expected type error (status: error), but typechecking succeeded"
      err : _ -> do
        let tag = Check.errorTag err
        case expectErrorTag of
          Nothing -> expectationFailure "expect.yaml missing error_tag for status: error"
          Just want | want /= tag ->
            expectationFailure $ "wrong error constructor in " <> label <> ": wanted "
              <> want <> ", got " <> tag <> "\nFull message:\n" <> Check.ppErrorInContext err
          Just _ -> pure ()
        when collect $ case expectErrorCount of
          Just n | length errs /= n ->
            expectationFailure $ "in " <> label <> ", expected " <> show n
              <> " errors, got " <> show (length errs)
          _ -> pure ()
        case expectLine of
          Nothing -> pure ()
          Just ln -> case Check.errorLocation err >>= Check.locationLine of
            Just got | got /= ln ->
              expectationFailure $ "in " <> label <> ", expected line " <> show ln <> " got " <> show got
            _ -> pure ()
        case expectColumn of
          Nothing -> pure ()
          Just c -> case Check.errorLocation err >>= Check.locationColumn of
            Just got | got /= c ->
              expectationFailure $ "in " <> label <> ", expected column " <> show c <> " got " <> show got
            _ -> pure ()
    st -> expectationFailure $ "in " <> label <> ", unknown status " <> show st

runCaseNew :: CaseKind -> IO ()
runCaseNew PairedCase{..} = do
  e <- readExpect pcExpectPath
  expect <- case e of
    Left err -> expectationFailure ("YAML " <> pcExpectPath <> ": " <> err) >> undefined
    Right x  -> pure x
  txt <- T.readFile pcRzkPath
  root <- resolveRoot
  case Rzk.parseModule txt of
    Left perr -> expectationFailure $
      "parse failure in " <> relTitleFrom root pcRzkPath <> ":\n" <> T.unpack perr
    Right m   -> do
      let mods = [(relTitleFrom root pcRzkPath, m)]
      assertExpectNew pcRelTitle expect (expectApi expect == Just "collect") [txt] (runNew mods)
runCaseNew DirCase{..} = do
  e <- readExpect dcExpectPath
  expect <- case e of
    Left err -> expectationFailure ("YAML " <> dcExpectPath <> ": " <> err) >> undefined
    Right x  -> pure x
  root <- resolveRoot
  modRelAbs <- case expectModules expect of
    Nothing -> pure [(relTitleFrom root (dcDir </> "input.rzk"), dcDir </> "input.rzk")]
    Just names -> pure [(relTitleFrom root (dcDir </> n), dcDir </> n) | n <- names]
  modsE <- loadModules modRelAbs
  mods <- either (\msg -> expectationFailure ("load/parse in " <> dcRelTitle <> ": " <> msg) >> undefined) pure modsE
  sources <- mapM (T.readFile . snd) modRelAbs
  assertExpectNew dcRelTitle expect (expectApi expect == Just "collect") sources (runNew mods)

caseTitle :: CaseKind -> String
caseTitle = \case
  PairedCase{..} -> pcRelTitle
  DirCase{..}    -> dcRelTitle
