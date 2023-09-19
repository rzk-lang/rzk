{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Rzk.Main where

import           Control.Monad                (forM)
import qualified Data.Aeson                   as JSON
import qualified Data.ByteString.Lazy.Char8   as ByteString
import           Data.Version                 (showVersion)
import           Options.Generic
import           System.Exit                  (exitFailure)
import           System.FilePath.Glob         (glob)

import qualified Language.Rzk.Syntax          as Rzk
import           Language.Rzk.VSCode.Tokenize (tokenizeModule)
import           Paths_rzk                    (version)
import           Rzk.TypeCheck

data Command
  = Typecheck [FilePath]
  | Tokenize
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
          , ppTypeErrorInScopedContext' err
          ]
        exitFailure
      Right () -> putStrLn "Everything is ok!"

  Tokenize -> do
    rzkModule <- parseStdin
    ByteString.putStrLn (JSON.encode (tokenizeModule rzkModule))

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
    paths -> return paths

parseRzkFilesOrStdin :: [FilePath] -> IO [(FilePath, Rzk.Module)]
parseRzkFilesOrStdin = \case
  -- if no paths are given — read from stdin
  [] -> do
    rzkModule <- parseStdin
    return [("<stdin>", rzkModule)]
  -- otherwise — parse all given files in given order
  paths -> do
    expandedPaths <- foldMap globNonEmpty paths
    forM (reverse expandedPaths) $ \path -> do
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
  case defaultTypeCheck (typecheckModule Nothing rzkModule) of
    Left err -> Left $ unlines
      [ "An error occurred when typechecking!"
      , "Rendering type error... (this may take a few seconds)"
      , unlines
        [ "Type Error:"
        , ppTypeErrorInScopedContext' err
        ]
      ]
    Right _ -> pure "Everything is ok!"
