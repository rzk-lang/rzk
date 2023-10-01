{-# LANGUAGE CPP               #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}

module Rzk.Main where

import           Control.Monad           (forM, void)
import           Data.List               (sort)
import           Data.Version            (showVersion)

#ifdef LSP
import           Language.Rzk.VSCode.Lsp (runLsp)
#endif

import qualified Data.Yaml               as Yaml
import           Options.Generic
import           System.Directory        (doesPathExist)
import           System.Exit             (exitFailure)
import           System.FilePath.Glob    (glob)

import qualified Language.Rzk.Syntax     as Rzk
import           Paths_rzk               (version)
import           Rzk.Project.Config
import           Rzk.TypeCheck

data Command
  = Typecheck [FilePath]
  | Lsp
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

parseRzkFilesOrStdin :: [FilePath] -> IO [(FilePath, Rzk.Module)]
parseRzkFilesOrStdin = \case
  -- if no paths are given — read from stdin
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
