module Main (main) where

import           Main.Utf8 (withUtf8)
import qualified Rzk.Main

main :: IO ()
main = withUtf8 Rzk.Main.main
