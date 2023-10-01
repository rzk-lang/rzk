{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified GHCJS.Foreign.Callback as GHCJS
import GHCJS.Marshal (fromJSVal, toJSVal)
import GHCJS.Prim (JSVal)
import Data.JSString(JSString, pack)
import JavaScript.Object
import JavaScript.Object.Internal (Object (..), create)
import qualified Rzk.Main as Rzk
import System.IO

main :: IO ()
main = do
  putStr "Haskell logic core starting"
  hFlush stdout

  -- https://discourse.haskell.org/t/compile-library-with-ghcjs/4727
  callback <- GHCJS.syncCallback1 GHCJS.ThrowWouldBlock $ \jsval -> do
    let o = Object jsval
    rawInput <- getProp "input" o
    input <- maybe (Left "Could not turn JSRef to a String") Right <$> fromJSVal rawInput

    case Rzk.typecheckString =<< input of
      Left err -> setStringProp "status" "error" o >> setStringProp "result" (pack err) o
      Right ok -> setStringProp "status" "ok" o >> setStringProp "result" (pack ok) o

  set_rzk_typecheck_callback callback

  putStr "Haskell logic core callbacks initialized"

setStringProp :: JSString -> JSString -> Object -> IO ()
setStringProp name valueRaw object = do
  value <- toJSVal valueRaw
  setProp name value object

foreign import javascript unsafe "rzkTypecheck_ = $1"
  set_rzk_typecheck_callback :: GHCJS.Callback (JSVal -> IO ()) -> IO ()
