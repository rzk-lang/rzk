module Language.Rzk.VSCode.Env where

import           Control.Concurrent.STM
import           Control.Monad.Reader
import qualified Data.Text                  as T
import           Language.LSP.Server
import           Language.Rzk.Free.Syntax   (VarIdent)
import qualified Language.Rzk.VSCode.Config as RzkConfig
import qualified Language.Rzk.VSCode.ReferenceIndex as RefInd
import           Rzk.TypeCheck              (Decl', TypeErrorInScopedContext)

data RzkCachedModule = RzkCachedModule
  { cachedModuleDecls  :: [Decl']
  , cachedModuleErrors :: [TypeErrorInScopedContext VarIdent]
  }

type RzkTypecheckCache = [(FilePath, RzkCachedModule)]

data ReferenceIndexCache = ReferenceIndexCache
  { indexCachePaths   :: [FilePath]
  , indexCacheCurrent :: FilePath
  , indexCacheSource  :: Maybe T.Text
  , indexCache        :: RefInd.ReferenceIndex
  }

data RzkEnv = RzkEnv
  { rzkEnvTypecheckCache      :: TVar RzkTypecheckCache
  , rzkEnvReferenceIndexCache :: TVar (Maybe ReferenceIndexCache)
  }

defaultRzkEnv :: IO RzkEnv
defaultRzkEnv = do
  typecheckCache <- newTVarIO []
  referenceIndexCache <- newTVarIO Nothing
  return RzkEnv
    { rzkEnvTypecheckCache = typecheckCache
    , rzkEnvReferenceIndexCache = referenceIndexCache
    }

type LSP = LspT RzkConfig.ServerConfig (ReaderT RzkEnv IO)

cacheTypecheckedModules :: RzkTypecheckCache -> LSP ()
cacheTypecheckedModules cache = lift $ do
  typecheckCache <- asks rzkEnvTypecheckCache
  referenceIndexCache <- asks rzkEnvReferenceIndexCache
  liftIO $ atomically $ do
    writeTVar typecheckCache cache
    writeTVar referenceIndexCache Nothing

resetCacheForAllFiles :: LSP ()
resetCacheForAllFiles = cacheTypecheckedModules []

resetCacheForFiles :: [FilePath] -> LSP ()
resetCacheForFiles paths = lift $ do
  typecheckCache <- asks rzkEnvTypecheckCache
  referenceIndexCache <- asks rzkEnvReferenceIndexCache
  liftIO $ atomically $ do
    modifyTVar typecheckCache (takeWhile ((`notElem` paths) . fst))
    writeTVar referenceIndexCache Nothing

getCachedTypecheckedModules :: LSP RzkTypecheckCache
getCachedTypecheckedModules = lift $ do
  typecheckCache <- asks rzkEnvTypecheckCache
  liftIO $ readTVarIO typecheckCache

cacheReferenceIndex :: ReferenceIndexCache -> LSP ()
cacheReferenceIndex cache = lift $ do
  referenceIndexCache <- asks rzkEnvReferenceIndexCache
  liftIO $ atomically $ writeTVar referenceIndexCache (Just cache)

getCachedReferenceIndex :: LSP (Maybe ReferenceIndexCache)
getCachedReferenceIndex = lift $ do
  referenceIndexCache <- asks rzkEnvReferenceIndexCache
  liftIO $ readTVarIO referenceIndexCache
