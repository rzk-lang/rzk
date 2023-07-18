{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Rzk.VSCode.Lsp where

import Control.Lens (to, (^.))
import Control.Monad.IO.Class
import qualified Data.Text as T
import Language.LSP.Protocol.Lens (HasParams (params), HasTextDocument (textDocument), HasUri (uri))
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server
import Language.LSP.VFS (virtualFileText)

handlers :: Handlers (LspM ())
handlers =
  mconcat
    [ notificationHandler SMethod_Initialized $ \_not -> pure (),
      requestHandler SMethod_TextDocumentSemanticTokensFull $ \req responder -> do
        let doc = req ^. params . textDocument . uri . to toNormalizedUri
        mdoc <- getVirtualFile doc
        -- Why is mdoc Nothing???
        case virtualFileText <$> mdoc of
          Nothing -> do
            let (NormalizedUri _ path) = doc
            _ <- sendNotification SMethod_WindowShowMessage (ShowMessageParams MessageType_Error (T.concat ["Couldn't open file: ", path]))
            return ()
          Just sourceCode -> do
            _ <- sendNotification SMethod_WindowShowMessage (ShowMessageParams MessageType_Info "File opened")
            -- TODO: tokenize `sourceCode` and send with `responder`
            return ()
    ]

runLsp :: IO Int
runLsp =
  runServer $
    ServerDefinition
      { onConfigurationChange = const $ pure $ Right (),
        doInitialize = const . pure . Right,
        staticHandlers = const handlers,
        interpretHandler = \env -> Iso (runLspT env) liftIO,
        options = defaultOptions,
        defaultConfig = ()
      }
