module Rzk.Main where

import           Control.Monad       (forM)
import           System.Environment  (getArgs)
import           System.Exit         (exitFailure)

import qualified Language.Rzk.Syntax as Rzk
import           Rzk.TypeCheck

main :: IO ()
main = do
  args <- getArgs
  case args of
    "typecheck" : paths -> do
      modules <- case paths of
        -- if no paths are given — read from stdin
        [] -> do
          result <- Rzk.parseModule <$> getContents
          case result of
            Left err -> do
              putStrLn ("An error occurred when parsing stdin")
              error err
            Right rzkModule -> return [("<stdin>", rzkModule)]
        -- otherwise — parse all given files in given order
        _ -> forM paths $ \path -> do
          putStrLn ("Loading file " <> path)
          result <- Rzk.parseModule <$> readFile path
          case result of
            Left err -> do
              putStrLn ("An error occurred when parsing file " <> path)
              error err
            Right rzkModule -> return (path, rzkModule)
      case defaultTypeCheck (typecheckModulesWithLocation modules) of
        Left err -> do
          putStrLn "An error occurred when typechecking!"
          putStrLn $ unlines
            [ "Type Error:"
            , ppTypeErrorInScopedContext' err
            ]
          exitFailure
        Right () -> putStrLn "Everything is ok!"
    _ -> ppUsage

ppUsage :: IO ()
ppUsage = putStrLn "rzk typecheck FILE"

typecheckString :: String -> Either String String
typecheckString moduleString = do
  rzkModule <- Rzk.parseModule moduleString
  case defaultTypeCheck (typecheckModule rzkModule) of
    Left err -> Left $ unlines
      [ "An error occurred when typechecking!"
      , "Rendering type error... (this may take a few seconds)"
      , unlines
        [ "Type Error:"
        , ppTypeErrorInScopedContext' err
        ]
      ]
    Right _ -> pure "Everything is ok!"

