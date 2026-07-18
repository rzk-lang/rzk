{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings        #-}

-- | The rzk logic core for the browser playground, compiled to WebAssembly
-- with the GHC wasm backend.
--
-- It exports a single JavaScript-callable function, @rzkTypecheck@, over the
-- wasm JSFFI. The function takes the module source as a string and returns a
-- JSON string @{"status": "ok"|"error", "result": <text>}@, which the
-- playground parses. The previous GHCJS build instead set a global callback
-- that mutated a JS object in place; the playground loader now adapts this
-- return value back to that shape, so the rest of the app is unchanged.
module Main (main, rzkTypecheck) where

import           Data.Aeson           (encode, object, (.=))
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as T
import           GHC.Wasm.Prim        (JSString (..), fromJSString, toJSString)
import qualified Rzk.Main             as Rzk

foreign export javascript "rzkTypecheck"
  rzkTypecheck :: JSString -> IO JSString

rzkTypecheck :: JSString -> IO JSString
rzkTypecheck jsInput = do
  let input = T.pack (fromJSString jsInput)
      (status, result) = case Rzk.typecheckString input of
        Left err -> ("error" :: T.Text, err)
        Right ok -> ("ok" :: T.Text, ok)
      json = encode (object ["status" .= status, "result" .= result])
  pure (toJSString (T.unpack (T.decodeUtf8 (BL.toStrict json))))

-- | Unused: this is a reactor module (see @-optl-mexec-model=reactor@), so no
-- @main@ is run; JavaScript drives it through the exported function.
main :: IO ()
main = pure ()
