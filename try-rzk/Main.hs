-- | Haskell language pragma
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}

-- | Haskell module declaration
module Main where

-- | Miso framework import
import           Miso
import           Miso.String
import qualified GHCJS.Foreign.Callback as GHCJS
import           GHCJS.Marshal (fromJSVal)
import           GHCJS.Prim (JSVal)

import qualified Rzk.Main as Rzk

-- | Type synonym for an application model
data Model = Model
  { response :: MisoString }
  deriving (Show, Eq)

-- | Sum type for application events
data Action
  = Reload
  | NoOp
  | Check MisoString
  deriving (Show, Eq)

-- | Entry point for a miso application
main :: IO ()
main = startApp App {..}
  where
    initialAction = Reload -- initial action to be executed on application load
    model  = initModel            -- initial model
    update = updateModel          -- update function
    view   = viewModel            -- view function
    events = defaultEvents        -- default delegated events
    subs   = [ ctrlEnterSub ]
    mountPoint = Just "__app__"   -- mount point for application (Nothing defaults to 'body')
    logLevel = Off                -- used during prerendering to see if the VDOM and DOM are in sync (only used with `miso` function)

ctrlEnterSub :: Sub Action
ctrlEnterSub sink = do
  callback <- GHCJS.asyncCallback1 $ \inputVal -> do
    Just input <- fromJSVal inputVal
    sink (Check input)
  set__rzk__trigger_Check_callback callback

initModel :: Model
initModel = Model
  { response = "loading..." }

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel NoOp m = noEff m
updateModel Reload m = initModel <# do
  Check <$> codemirrorGetValue
updateModel (Check input) m = noEff m
  { response = responseStr }
  where
    responseStr = ms $
      case Rzk.typecheckString (fromMisoString input) of
        Left err -> err
        Right ok -> ok

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel Model{..} = div_ [] [
   button_ [ onClick Reload ] [ text "Typecheck (Ctrl + Enter)" ]
 , br_ []
 , br_ []
 , pre_ [] [ text (ms response) ]
 ]

foreign import javascript unsafe "$r = myCodeMirror.getValue();"
  codemirrorGetValue :: IO MisoString

foreign import javascript unsafe "__rzk__trigger_Check = $1"
  set__rzk__trigger_Check_callback :: (GHCJS.Callback (JSVal -> IO ())) -> IO ()
