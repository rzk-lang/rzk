{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Main where

import           System.Environment (getArgs)
import Control.Monad

import           Rzk.Polylingual

main :: IO ()
main = do
  args <- getArgs
  case args of
    "typecheck" : paths -> do
      modules <- forM paths $ \path -> do
        result <- safeParseSomeModule <$> readFile path
        case result of
          Left err -> do
            putStrLn ("An error occurred when parsing file " <> path)
            error err
          Right m  -> return m
      case combineModules modules of
        Left err -> do
          error err
        Right m -> putStrLn (compileSomeModule m)
    _ -> ppUsage

ppUsage :: IO ()
ppUsage = putStrLn "rzk typecheck FILE"
