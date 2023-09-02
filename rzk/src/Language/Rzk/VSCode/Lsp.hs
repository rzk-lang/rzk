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
import Language.Rzk.VSCode.Tokenize (tokenizeModule)
import Language.Rzk.Syntax (parseModule)
import Language.LSP.Diagnostics (partitionBySource)
import Rzk.TypeCheck (typecheckModule, defaultTypeCheck, ppTypeErrorInScopedContext')

-- | The maximum number of diagnostic messages to send to the client
maxDiagnosticCount :: Int
maxDiagnosticCount = 100

handlers :: Handlers (LspM ())
handlers =
  mconcat
    [ notificationHandler SMethod_Initialized $ \_not -> pure ()
    -- TODO: add logging
    -- Empty handlers to silence the errors
    , notificationHandler SMethod_TextDocumentDidOpen $ \_msg -> pure ()
    , notificationHandler SMethod_TextDocumentDidChange $ \_msg -> pure ()
    , notificationHandler SMethod_TextDocumentDidClose $ \_msg -> pure ()
    , notificationHandler SMethod_TextDocumentDidSave $ \msg -> do
        -- TODO: do the same thing in DidOpen handler
        let normUri = toNormalizedUri $ msg ^. params . textDocument . uri
        mdoc <- getVirtualFile normUri
        let typeErrors =
              case virtualFileText <$> mdoc of
                Nothing -> Left "error getting text"
                Just text -> typecheckModule Nothing <$> parseModule (T.unpack text)
        let errorMessage = do
              res <- typeErrors
              case defaultTypeCheck res of
                Left err -> Left $ ppTypeErrorInScopedContext' err
                Right _decls -> Right ()
        case errorMessage of
          Left err -> do
            let diags =
                  [ Diagnostic
                      (Range (Position 0 0) (Position 0 0))
                      (Just DiagnosticSeverity_Error)
                      Nothing -- diagnostic code
                      Nothing -- diagonstic description
                      (Just "rzk") -- A human-readable string describing the source of this diagnostic
                      (T.pack err)
                      Nothing -- tags
                      (Just []) -- related information
                      Nothing -- data that is preserved between different calls
                  ]
            publishDiagnostics maxDiagnosticCount normUri Nothing (partitionBySource diags)
          Right () -> do
            publishDiagnostics 0 normUri Nothing (partitionBySource [])
    -- , requestHandler SMethod_TextDocumentHover $ \req responder -> do
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
  , _save              = Just $ InR $ SaveOptions $ Just False
  }

runLsp :: IO Int
runLsp =
  runServer $
    ServerDefinition
      { onConfigurationChange = const $ pure $ Right (),
        doInitialize = const . pure . Right,
        staticHandlers = const handlers,
        interpretHandler = \env -> Iso (runLspT env) liftIO,
        options = defaultOptions { optTextDocumentSync = Just syncOptions },
        defaultConfig = ()
      }
