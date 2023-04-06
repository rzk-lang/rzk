module Main (main) where

import System.Environment (getArgs)
import Control.Monad (forM)

import qualified Language.Rzk.Syntax as Rzk
import Rzk.TypeCheck

main :: IO ()
main = do
  args <- getArgs
  case args of
    "typecheck" : paths -> do
      modules <- forM paths $ \path -> do
        putStrLn ("Loading file " <> path)
        result <- Rzk.parseModule <$> readFile path
        case result of
          Left err -> do
            putStrLn ("An error occurred when parsing file " <> path)
            error err
          Right rzkModule -> return rzkModule
      let rzkModule = Rzk.Module (Rzk.LanguageDecl Rzk.Rzk1) 
            [ command | Rzk.Module _ commands <- modules, command <- commands ]
      case defaultTypeCheck (typecheckModule rzkModule) of
        Left err -> error $ unlines
          [ "Type Error:"
          , ppTypeErrorInScopedContext' err
          ]
        Right () -> putStrLn "Everything is ok!"
    _ -> ppUsage

ppUsage :: IO ()
ppUsage = putStrLn "rzk typecheck FILE"
