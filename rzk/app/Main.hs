{-# LANGUAGE CPP               #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

#ifndef __GHCJS__
import           Main.Utf8 (withUtf8)
#endif

import           Control.Monad           (forM, forM_, unless, when,
                                          (>=>))
import           Data.Version            (showVersion)

#ifdef LSP
import           Language.Rzk.VSCode.Lsp (runLsp)
#endif

import           Options.Generic
import           System.Exit             (exitFailure, exitSuccess)

import           Data.Functor            ((<&>))
import           Paths_rzk               (version)
import           Rzk.Format              (formatFile, formatFileWrite,
                                          isWellFormattedFile)
import           Rzk.TypeCheck
import           Rzk.Main

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
main = do
#ifndef __GHCJS__
  withUtf8 $
#endif
    getRecord "rzk: an experimental proof assistant for synthetic ∞-categories" >>= \case
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

    Format (FormatOptions {check, write}) paths -> do
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

