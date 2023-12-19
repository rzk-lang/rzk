{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}

module Language.Rzk.VSCode.Lsp where

import           Control.Lens                  (_Just, to, (^..))
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Default.Class            (Default (def))
import           Data.List                     (isSuffixOf)
import qualified Data.Text                     as T
import           Language.LSP.Protocol.Lens    (HasParams (params),
                                                HasUri (uri), changes, uri)
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types
import           Language.LSP.Server

import           Control.Exception             (SomeException, evaluate, try)
import           Control.Monad.Except          (ExceptT (ExceptT),
                                                MonadError (throwError),
                                                modifyError, runExceptT)
import           Data.Aeson                    (Result (Error, Success),
                                                fromJSON)
import           Language.Rzk.Syntax           (parseModuleFile)
import           Language.Rzk.VSCode.Config    (ServerConfig (..))
import           Language.Rzk.VSCode.Env
import           Language.Rzk.VSCode.Handlers
import           Language.Rzk.VSCode.Logging
import           Rzk.TypeCheck                 (defaultTypeCheck,
                                                typecheckModulesWithLocationIncremental)

-- | The maximum number of diagnostic messages to send to the client
maxDiagnosticCount :: Int
maxDiagnosticCount = 100

data IsChanged
  = HasChanged
  | NotChanged

-- | Detects if the given path has changes in its declaration compared to what's in the cache
isChanged :: RzkTypecheckCache -> FilePath -> LSP IsChanged
isChanged cache path = toIsChanged $ do
  cachedDecls <- maybeToEitherLSP $ lookup path cache
  module' <- toExceptTLifted $ parseModuleFile path
  e <- toExceptTLifted $ try @SomeException $ evaluate $
    defaultTypeCheck (typecheckModulesWithLocationIncremental (takeWhile ((/= path) . fst) cache) [(path, module')])
  (checkedModules, _errors) <- toExceptT $ return e
  decls' <- maybeToEitherLSP $ lookup path checkedModules
  return $ if decls' == cachedDecls
    then NotChanged
    else HasChanged
  where
    toExceptT = modifyError (const ()) . ExceptT
    toExceptTLifted = toExceptT . liftIO
    maybeToEitherLSP = \case
      Nothing -> throwError ()
      Just x -> return x
    toIsChanged m = runExceptT m >>= \case
      Left _ -> return HasChanged -- in case of error consider the file has changed
      Right x -> return x

hasNotChanged :: RzkTypecheckCache -> FilePath -> LSP Bool
hasNotChanged cache path = isChanged cache path >>= \case
  HasChanged -> return False
  NotChanged -> return True

-- | Monadic 'dropWhile'
dropWhileM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
dropWhileM _ []     = return []
dropWhileM p (x:xs) = do
  q <- p x
  if q
    then dropWhileM p xs
    else return (x:xs)

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
        if any ("rzk.yaml" `isSuffixOf`) modifiedPaths
          then do
            logDebug "rzk.yaml modified. Clearing module cache"
            resetCacheForAllFiles
          else do
            cache <- getCachedTypecheckedModules
            actualModified <- dropWhileM (hasNotChanged cache) modifiedPaths
            resetCacheForFiles actualModified
        typecheckFromConfigFile
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
