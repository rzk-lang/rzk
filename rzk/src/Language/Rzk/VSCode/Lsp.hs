{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Language.Rzk.VSCode.Lsp where

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Default.Class            (Default (def))
import qualified Data.Text                     as T
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types
import           Language.LSP.Server

import           Data.Aeson                    (Result (Error, Success),
                                                fromJSON)
import           Language.Rzk.VSCode.Config    (ServerConfig (..))
import           Language.Rzk.VSCode.Env
import           Language.Rzk.VSCode.Handlers

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
    , notificationHandler SMethod_TextDocumentDidChange $ \_msg -> pure ()
    , notificationHandler SMethod_TextDocumentDidClose $ \_msg -> pure ()
    , notificationHandler SMethod_WorkspaceDidChangeWatchedFiles handleFilesChanged
    , notificationHandler SMethod_TextDocumentDidSave $ \_msg -> do
        -- TODO: check if the file is included in the config's `include` list.
        --       If not (and not in `exclude`) either, issue a warning.
        return () -- FIXME: typecheck standalone files (if they are not a part of the project)
    -- An empty hadler is needed to silence the error since it is already handled by the LSP package
    , notificationHandler SMethod_WorkspaceDidChangeConfiguration $ const $ pure ()
    -- , requestHandler SMethod_TextDocumentHover $ \req responder -> do
    --    TODO: Read from the list of symbols that is supposed to be cached by the typechecker
    --     let TRequestMessage _ _ _ (HoverParams _doc pos _workDone) = req
    --         Position _l _c' = pos
    --         rsp = Hover (InL ms) (Just range')
    --         ms = mkMarkdown "Hello world"
    --         range' = Range pos pos
    --     responder (Right $ InL rsp)
    , requestHandler SMethod_TextDocumentCompletion provideCompletions
    , requestHandler SMethod_TextDocumentSemanticTokensFull provideSemanticTokens
    , requestHandler SMethod_TextDocumentFormatting formatDocument
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
      { configSection = "rzk"
      , parseConfig = \_oldConfig newObject -> case fromJSON newObject of
          -- TODO: handle partial config updates from VS Code by updating oldConfig rather than parsing from scratch
          Error err         -> Left $ T.pack err
          Success rzkConfig -> Right rzkConfig
      , onConfigChange = const $ pure ()
      , doInitialize = const . pure . Right
      , staticHandlers = const handlers
      , interpretHandler = \env -> Iso (flip runReaderT rzkEnv . runLspT env) liftIO
      , options = defaultOptions { optTextDocumentSync = Just syncOptions }
      , defaultConfig = def :: ServerConfig
      }
