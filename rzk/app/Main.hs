module Main (main) where

import qualified Language.Rzk.Syntax as Rzk
import Rzk

main :: IO ()
main = do
  input <- getContents
  case Rzk.parseModule input of
    Left err -> do
      putStrLn "Parse Error:"
      putStrLn err
    Right rzkModule -> do
      putStrLn (Rzk.printTree rzkModule)
