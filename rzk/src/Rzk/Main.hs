{-# LANGUAGE CPP               #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeApplications  #-}

module Rzk.Main where

import           Control.Monad           (forM, forM_, unless, void, when,
                                          (>=>))
import           Data.List               (sort)
import           Data.Version            (showVersion)

#ifdef LSP
import           Language.Rzk.VSCode.Lsp (runLsp)
#endif

import qualified Data.Yaml               as Yaml
import           Options.Generic
import           System.Directory        (doesPathExist)
import           System.Exit             (exitFailure, exitSuccess)
import           System.FilePath.Glob    (glob)

import           Data.Functor            ((<&>))
import qualified Language.Rzk.Syntax     as Rzk
import           Paths_rzk               (version)
import           Rzk.Format              (formatFile, formatFileWrite,
                                          isWellFormattedFile)
import           Rzk.Project.Config
import           Rzk.TypeCheck

data FormatOptions = FormatOptions
  { check :: Bool
  , write :: Bool
  } deriving (Generic, Show, ParseRecord, Read, ParseField)

instance ParseFields FormatOptions where
  parseFields _ _ _ _ = FormatOptions
    <$> parseFields (Just "Check if the files are correctly formatted") (Just "check") (Just 'c') Nothing
    <*> parseFields (Just "Write formatted file to disk") (Just "write") (Just 'w') Nothing

data Command
  = Typecheck [FilePath]
  | Lsp
  | Format FormatOptions [FilePath]
  | Version
  deriving (Generic, Show, ParseRecord)

main :: IO ()
main = getRecord "rzk: an experimental proof assistant for synthetic ∞-categories" >>= \case
  Typecheck paths -> do
    modules <- parseRzkFilesOrStdin paths
    case defaultTypeCheck (typecheckModulesWithLocation modules) of
      Left err -> do
        putStrLn "An error occurred when typechecking!"
        putStrLn $ unlines
          [ "Type Error:"
          , ppTypeErrorInScopedContext' BottomUp err
          ]
        exitFailure
      Right _decls -> putStrLn "Everything is ok!"

  Lsp ->
#ifdef LSP
    void runLsp
#else
    error "rzk lsp is not supported with this build"
#endif

  Format (FormatOptions check write) paths -> do
    when (check && write) (error "Options --check and --write are mutually exclusive")
    expandedPaths <- expandRzkPathsOrYaml paths
    case expandedPaths of
      [] -> error "No files found"
      filePaths -> do
        when (not check && not write) $ forM_ filePaths (formatFile >=> putStrLn)
        when write $ forM_ filePaths formatFileWrite
        when check $ do
          results <- forM filePaths $ \path -> isWellFormattedFile path <&> (path,)
          let notFormatted = map fst $ filter (not . snd) results
          unless (null notFormatted) $ do
            putStrLn "Some files are not well formatted:"
            forM_ notFormatted $ \path -> putStrLn ("  " <> path)
            exitFailure
          exitSuccess

  Version -> putStrLn (showVersion version)

parseStdin :: IO Rzk.Module
parseStdin = do
  result <- Rzk.parseModule <$> getContents
  case result of
    Left err -> do
      putStrLn ("An error occurred when parsing stdin")
      error err
    Right rzkModule -> return rzkModule

-- | Finds matches to the given pattern in the current working directory.
-- **NOTE:** throws exception when 'glob' returns an empty list.
globNonEmpty :: FilePath -> IO [FilePath]
globNonEmpty path = do
  glob path >>= \case
    []    -> error ("File(s) not found at " <> path)
    paths -> return (sort paths)

extractFilesFromRzkYaml :: FilePath -> IO [FilePath]
extractFilesFromRzkYaml rzkYamlPath = do
  eitherConfig <- Yaml.decodeFileEither @ProjectConfig rzkYamlPath
  case eitherConfig of
    Left err -> do
      error ("Invalid or missing rzk.yaml: " ++ Yaml.prettyPrintParseException err)
    Right ProjectConfig{..} -> do
      return include

-- | Given a list of file paths (possibly including globs), expands the globs (if any)
-- or tries to read the list of files from the rzk.yaml file (if no paths are given).
-- Glob patterns in rzk.yaml are also expanded.
expandRzkPathsOrYaml :: [FilePath] -> IO [FilePath]
expandRzkPathsOrYaml = \case
    [] -> do
      let rzkYamlPath = "rzk.yaml"
      rzkYamlExists <- doesPathExist rzkYamlPath
      if rzkYamlExists
        then do
          paths <- extractFilesFromRzkYaml rzkYamlPath
          when (null paths) (error $ "No files found in " <> rzkYamlPath)
          expandRzkPathsOrYaml paths
        else error ("No paths given and no " <> rzkYamlPath <> " found")
    paths -> foldMap globNonEmpty paths

parseRzkFilesOrStdin :: [FilePath] -> IO [(FilePath, Rzk.Module)]
parseRzkFilesOrStdin = \case
  -- if no paths are given — read from stdin
  -- TODO: reuse the `expandRzkPathsOrYaml` function
  [] -> do
    let rzkYamlPath = "rzk.yaml"
    rzkYamlExists <- doesPathExist rzkYamlPath
    if rzkYamlExists
      then do
        putStrLn ("Using Rzk project stucture specified in " <> rzkYamlPath)
        paths <- extractFilesFromRzkYaml rzkYamlPath
        parseRzkFilesOrStdin paths
      else do
        rzkModule <- parseStdin
        return [("<stdin>", rzkModule)]
  -- otherwise — parse all given files in given order
  paths -> do
    expandedPaths <- foldMap globNonEmpty paths
    forM expandedPaths $ \path -> do
      putStrLn ("Loading file " <> path)
      result <- Rzk.parseModule <$> readFile path
      case result of
        Left err -> do
          putStrLn ("An error occurred when parsing file " <> path)
          error err
        Right rzkModule -> return (path, rzkModule)

typecheckString :: String -> Either String String
typecheckString moduleString = do
  rzkModule <- Rzk.parseModule moduleString
  case defaultTypeCheck (typecheckModules [rzkModule]) of
    Left err -> Left $ unlines
      [ "An error occurred when typechecking!"
      , "Rendering type error... (this may take a few seconds)"
      , unlines
        [ "Type Error:"
        , ppTypeErrorInScopedContext' BottomUp err
        ]
      ]
    Right _ -> pure "Everything is ok!"
