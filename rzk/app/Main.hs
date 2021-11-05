{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Main where

import           System.Environment (getArgs)

import           Rzk.Polylingual

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["typecheck", path] -> do
      result <- safeParseSomeModule <$> readFile path
      case result of
        Left err -> putStrLn err
        Right m  -> putStrLn (compileSomeModule m)
    _ -> ppUsage

ppUsage :: IO ()
ppUsage = putStrLn "rzk typecheck FILE"
