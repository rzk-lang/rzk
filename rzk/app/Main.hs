{-# LANGUAGE CPP               #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Main (main) where

#ifndef __GHCJS__
import           Main.Utf8               (withUtf8)
#endif

import           Control.Monad           (forM, forM_, unless, when, (>=>))
import           Data.Version            (showVersion)

#ifdef LSP_ENABLED
import           Language.Rzk.VSCode.Lsp (runLsp)
#endif

import           Options.Generic
import           System.Exit             (exitFailure, exitSuccess)

import           Data.Aeson              (encode)
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Functor            (void, (<&>))
import qualified Data.Text.IO            as T

import           Paths_rzk               (version)
import           Rzk.Diagnostic          (Diagnostic (..), Severity (..),
                                          diagnoseHole, diagnoseTypeError,
                                          ppHoleInfo)
import           Rzk.Format              (formatFile, formatFileWrite,
                                          isWellFormattedFile)
import           Rzk.Main
import           Rzk.TypeCheck

data FormatOptions = FormatOptions
  { check :: Bool
  , write :: Bool
  } deriving (Generic, Show, ParseRecord, Read, ParseField)

instance ParseFields FormatOptions where
  parseFields _ _ _ _ = FormatOptions
    <$> parseFields (Just "Check if the files are correctly formatted") (Just "check") (Just 'c') Nothing
    <*> parseFields (Just "Write formatted file to disk") (Just "write") (Just 'w') Nothing

data TypecheckOptions = TypecheckOptions
  { typecheckAllowHoles :: Bool
  , typecheckJson       :: Bool
  } deriving (Generic, Show, ParseRecord, Read, ParseField)

instance ParseFields TypecheckOptions where
  parseFields _ _ _ _ = TypecheckOptions
    <$> parseFields (Just "Allow unsolved holes: report each hole's goal and local context instead of failing") (Just "allow-holes") (Just 'H') Nothing
    <*> parseFields (Just "Output diagnostics (type errors and holes) as JSON on stdout") (Just "json") (Just 'j') Nothing

data Command
  = Typecheck TypecheckOptions [FilePath]
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
    Typecheck (TypecheckOptions {typecheckAllowHoles = allowHolesFlag, typecheckJson = jsonFlag}) paths -> do
      modules <- parseRzkFilesOrStdin paths
      let reportError err = do
            putStrLn "An error occurred when typechecking!"
            putStrLn $ unlines
              [ "Type Error:"
              , ppTypeErrorInScopedContext' BottomUp err
              ]
      if jsonFlag
        -- Machine-readable mode: emit every diagnostic (type errors as errors,
        -- holes as hints) as a JSON array on stdout; progress goes to stderr.
        -- Exit non-zero iff there is an error-severity diagnostic.
        then do
          let diagnostics = case typecheckModulesWithHoles modules of
                Left err -> [diagnoseTypeError BottomUp err]
                Right (_decls, errors, holes) ->
                  map (diagnoseTypeError BottomUp) errors ++ map diagnoseHole holes
          BL8.putStrLn (encode diagnostics)
          when (any ((== SeverityError) . diagnosticSeverity) diagnostics) exitFailure
        else if allowHolesFlag
          then case typecheckModulesWithHoles modules of
            Left err -> reportError err >> exitFailure
            Right (_decls, errors, holes) -> do
              forM_ holes (putStr . ppHoleInfo)
              case errors of
                [] -> putStrLn ("Everything is ok! (" <> show (length holes) <> " hole(s))")
                _  -> do forM_ errors reportError; exitFailure
          else case defaultTypeCheck (typecheckModulesWithLocation modules) of
            Left err -> reportError err >> exitFailure
            Right _decls -> putStrLn "Everything is ok!"

    Lsp ->
#ifdef LSP_ENABLED
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
          when (not check && not write) $ forM_ filePaths (formatFile >=> T.putStrLn)
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
