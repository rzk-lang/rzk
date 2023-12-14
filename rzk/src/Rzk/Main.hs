{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}

module Rzk.Main where

import           Control.Monad        (forM, when)
import           Data.List            (sort)
import qualified Data.Yaml            as Yaml
import           System.Directory     (doesPathExist)
import           System.FilePath.Glob (glob)

import qualified Language.Rzk.Syntax  as Rzk
import           Rzk.Project.Config
import           Rzk.TypeCheck



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
        when (null paths) (error $ "No Rzk files specified in the config file at " <> rzkYamlPath)
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
