{-# LANGUAGE CPP #-}
module Main (main) where

#ifndef __GHCJS__
import           Main.Utf8 (withUtf8)
#endif
import qualified Rzk.Main

main :: IO ()
main =
#ifndef __GHCJS__
  withUtf8
#endif
    Rzk.Main.main
