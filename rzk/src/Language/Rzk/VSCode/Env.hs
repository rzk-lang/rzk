{-# LANGUAGE ScopedTypeVariables #-}
module Language.Rzk.VSCode.Env where

import           Control.Concurrent.Async   (Async, async, cancel)
import           Control.Concurrent.STM
import           Control.Exception          (catch)
import           Control.Monad.Reader
import qualified Data.Map.Strict            as Map
import qualified Data.Text                  as T
import           Language.LSP.Server
import           Language.Rzk.Syntax        (Module)
import qualified Language.Rzk.VSCode.Config as RzkConfig
import           Language.Rzk.VSCode.Logging
import qualified Language.Rzk.VSCode.ReferenceIndex as RefInd
import           Rzk.TypeCheck              (Checked, DeclView,
                                             TypeErrorInScopedContext)

-- | What checking one module produced.
--
-- 'cachedModuleChecked' is the state of the whole run /after/ this module: the
-- top-level scope and every declaration elaborated so far. That is what a resume
-- starts from — a cached elaborated term names the definitions it uses by their
-- foil name, which only means anything in the scope that produced it, so the scope
-- has to be cached with them.
--
-- 'cachedModuleDecls' is the rendered view of this module's own declarations,
-- which is all that completion, symbols and hover need.
data RzkCachedModule = RzkCachedModule
  { cachedModuleChecked :: Checked
  , cachedModuleDecls   :: [DeclView]
  , cachedModuleErrors  :: [TypeErrorInScopedContext]
  }

type RzkTypecheckCache = [(FilePath, RzkCachedModule)]

-- | Where a parse came from, deciding whether it may be reused.
data ParseSource
  = ParsedFromBuffer T.Text
    -- ^ Parsed from the editor buffer with this text; reusable for the
    -- current file while the buffer text is unchanged.
  | ParsedFromDisk
    -- ^ Parsed from the file on disk; trusted until invalidated by a
    -- file-change notification.
  | ParseInvalidated
    -- ^ The file changed; must be re-parsed. The module is kept as the last
    -- good parse, which hover falls back to while the file fails to parse.

-- | A parse result for the reference index. A failed parse is a 'Nothing'
-- module and is not retried until the source changes.
data ParsedModule = ParsedModule
  { parsedSource :: ParseSource
  , parsedModule :: Maybe Module
  }

data ReferenceIndexCache = ReferenceIndexCache
  { indexCacheModules :: Map.Map FilePath ParsedModule
    -- ^ Per-file parse cache. Entries parsed from disk are trusted until a
    -- file-change notification evicts them ('resetCacheForFiles'); the entry
    -- for the file being edited is revalidated against the editor buffer.
  , indexCacheResult  :: Maybe ([FilePath], RefInd.ReferenceIndex)
    -- ^ The index last built, with the file set it was built from.
  }

emptyReferenceIndexCache :: ReferenceIndexCache
emptyReferenceIndexCache = ReferenceIndexCache Map.empty Nothing

data RzkEnv = RzkEnv
  { rzkEnvTypecheckCache      :: TVar RzkTypecheckCache
  , rzkEnvReferenceIndexCache :: TVar ReferenceIndexCache
  , rzkEnvTypecheckWorker     :: TVar (Maybe (Async ()))
    -- ^ The thread running the current project typecheck, if any.
  }

defaultRzkEnv :: IO RzkEnv
defaultRzkEnv = do
  typecheckCache <- newTVarIO []
  referenceIndexCache <- newTVarIO emptyReferenceIndexCache
  typecheckWorker <- newTVarIO Nothing
  return RzkEnv
    { rzkEnvTypecheckCache = typecheckCache
    , rzkEnvReferenceIndexCache = referenceIndexCache
    , rzkEnvTypecheckWorker = typecheckWorker
    }

type LSP = LspT RzkConfig.ServerConfig (ReaderT RzkEnv IO)

-- | Run the given action (a project typecheck) on a fresh worker thread,
-- cancelling the previous worker first. Cancellation waits for the old
-- worker to stop, so it can no longer write to the caches once the new
-- one starts. Typechecking runs on a worker so that the handler dispatch
-- thread stays responsive: 'lsp' dispatches messages sequentially, so a
-- multi-second re-check run directly in a notification handler would
-- block every later request (e.g. formatting on save).
spawnTypecheckWorker :: LSP () -> LSP ()
spawnTypecheckWorker action = do
  lspEnv <- getLspEnv
  rzkEnv <- lift ask
  let run act = runReaderT (runLspT lspEnv act) rzkEnv
  liftIO $ do
    oldWorker <- atomically $ swapTVar (rzkEnvTypecheckWorker rzkEnv) Nothing
    mapM_ cancel oldWorker
    worker <- async $
      run action `catch` \(_ :: ProgressCancelledException) ->
        run (logInfo (T.pack "Typechecking was cancelled by the client"))
    atomically $ writeTVar (rzkEnvTypecheckWorker rzkEnv) (Just worker)

cacheTypecheckedModules :: RzkTypecheckCache -> LSP ()
cacheTypecheckedModules cache = lift $ do
  typecheckCache <- asks rzkEnvTypecheckCache
  referenceIndexCache <- asks rzkEnvReferenceIndexCache
  liftIO $ atomically $ do
    writeTVar typecheckCache cache
    -- Drop the built index (the declaration set may have changed), but keep
    -- the parse entries: files changed on disk are already evicted per path
    -- by 'resetCacheForFiles' on file-change notifications, and a kept entry
    -- may hold the last good parse of a file that currently has a syntax
    -- error, which hover falls back to.
    modifyTVar referenceIndexCache $ \c -> c { indexCacheResult = Nothing }

resetCacheForAllFiles :: LSP ()
resetCacheForAllFiles = cacheTypecheckedModules []

resetCacheForFiles :: [FilePath] -> LSP ()
resetCacheForFiles paths = lift $ do
  typecheckCache <- asks rzkEnvTypecheckCache
  referenceIndexCache <- asks rzkEnvReferenceIndexCache
  liftIO $ atomically $ do
    modifyTVar typecheckCache (takeWhile ((`notElem` paths) . fst))
    modifyTVar referenceIndexCache $ \c -> ReferenceIndexCache
      { indexCacheModules =
          foldr (Map.adjust (\pm -> pm { parsedSource = ParseInvalidated }))
                (indexCacheModules c) paths
      , indexCacheResult  = Nothing
      }

getCachedTypecheckedModules :: LSP RzkTypecheckCache
getCachedTypecheckedModules = lift $ do
  typecheckCache <- asks rzkEnvTypecheckCache
  liftIO $ readTVarIO typecheckCache

cacheReferenceIndex :: ReferenceIndexCache -> LSP ()
cacheReferenceIndex cache = lift $ do
  referenceIndexCache <- asks rzkEnvReferenceIndexCache
  liftIO $ atomically $ writeTVar referenceIndexCache cache

getCachedReferenceIndex :: LSP ReferenceIndexCache
getCachedReferenceIndex = lift $ do
  referenceIndexCache <- asks rzkEnvReferenceIndexCache
  liftIO $ readTVarIO referenceIndexCache
