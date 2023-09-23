{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Language.Rzk.VSCode.Lsp where

import           Control.Lens                  (_Just, to, (^.), (^..))
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import qualified Data.Text                     as T
import           Language.LSP.Protocol.Lens    (HasParams (params),
                                                HasTextDocument (textDocument),
                                                HasUri (uri), changes, uri)
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types
import           Language.LSP.Server
import           Language.LSP.VFS              (virtualFileText)

import           Language.Rzk.Syntax           (parseModule)
import           Language.Rzk.VSCode.Env
import           Language.Rzk.VSCode.Handlers
import           Language.Rzk.VSCode.Tokenize  (tokenizeModule)

-- | The maximum number of diagnostic messages to send to the client
maxDiagnosticCount :: Int
maxDiagnosticCount = 100

handlers :: Handlers LSP
handlers =
  mconcat
    [ notificationHandler SMethod_Initialized $ const typecheckFromConfigFile
    -- TODO: add logging
    -- Empty handlers to silence the errors
    , notificationHandler SMethod_TextDocumentDidOpen $ \_msg -> pure ()
    -- , requestHandler SMethod_TextDocumentFormatting $ \_req _res -> pure ()
    , notificationHandler SMethod_TextDocumentDidChange $ \_msg -> pure ()
    , notificationHandler SMethod_TextDocumentDidClose $ \_msg -> pure ()
    , notificationHandler SMethod_WorkspaceDidChangeWatchedFiles $ \msg -> do
        let modifiedPaths = msg ^.. params . changes . traverse . uri . to uriToFilePath . _Just
        resetCacheForFiles modifiedPaths
        -- TODO: see what files changed and typecheck them again
        --  Need to handle 3 events: added, changed, and deleted

        -- Currently, this is only sent for changes in `rzk.yaml`, so it makes sense to typecheck again (unconditionally)
        typecheckFromConfigFile
    , notificationHandler SMethod_TextDocumentDidSave $ \_msg -> do
        -- TODO: check if the file is included in the config's `include` list.
        --       If not (and not in `exclude`) either, issue a warning.
        typecheckFromConfigFile
    -- , requestHandler SMethod_TextDocumentHover $ \req responder -> do
    --    TODO: Read from the list of symbols that is supposed to be cached by the typechecker
    --     let TRequestMessage _ _ _ (HoverParams _doc pos _workDone) = req
    --         Position _l _c' = pos
    --         rsp = Hover (InL ms) (Just range')
    --         ms = mkMarkdown "Hello world"
    --         range' = Range pos pos
    --     responder (Right $ InL rsp)
    , requestHandler SMethod_TextDocumentSemanticTokensFull $ \req responder -> do
        let doc = req ^. params . textDocument . uri . to toNormalizedUri
        mdoc <- getVirtualFile doc
        let possibleTokens = case virtualFileText <$> mdoc of
              Nothing -> Left "Failed to get file content"
              Just sourceCode -> tokenizeModule <$> parseModule (T.unpack sourceCode)
        case possibleTokens of
          Left _err -> do
            -- Failed to open the file or to tokenize
            return ()
          Right tokens -> do
            let encoded = encodeTokens defaultSemanticTokensLegend $ relativizeTokens tokens
            case encoded of
              Left _err -> do
                -- Failed to encode the tokens
                return ()
              Right list ->
                responder (Right (InL SemanticTokens { _resultId = Nothing, _data_ = list }))
    ]


syncOptions :: TextDocumentSyncOptions
syncOptions = TextDocumentSyncOptions
  { _openClose         = Just True
  , _change            = Just TextDocumentSyncKind_Full
  , _willSave          = Just False
  , _willSaveWaitUntil = Just False
  , _save              = Just $ InR $ SaveOptions { _includeText = Just True }
  }

runLsp :: IO Int
runLsp = do
  rzkEnv <- defaultRzkEnv
  runServer $
    ServerDefinition
      { onConfigurationChange = const $ pure $ Right (),
        doInitialize = const . pure . Right,
        staticHandlers = const handlers,
        interpretHandler = \env -> Iso (flip runReaderT rzkEnv . runLspT env) liftIO,
        options = defaultOptions { optTextDocumentSync = Just syncOptions },
        defaultConfig = ()
      }
