{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Rzk.TypeCheckSpec (spec) where

import           Control.Monad            (forM, unless)
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

import qualified Language.Rzk.Syntax        as Rzk
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

caseTitle :: CaseKind -> String
caseTitle = \case
  PairedCase{..} -> pcRelTitle
  DirCase{..}    -> dcRelTitle
