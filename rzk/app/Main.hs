{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Main where

import           System.Environment (getArgs)

import           Rzk.Parser.Text
import           Rzk.Syntax.Var
import           Rzk.TypeChecker

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["typecheck", path] -> do
      m <- loadModuleFromMarkdownFile path
      print (typecheckModule @Var ["{H}"] m)
    _ -> ppUsage

ppUsage :: IO ()
ppUsage = putStrLn "rzk typecheck FILE"
