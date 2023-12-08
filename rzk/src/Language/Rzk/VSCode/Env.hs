module Language.Rzk.VSCode.Env where

import           Control.Concurrent.STM
import           Control.Monad.Reader
import           Language.LSP.Server
import           Language.Rzk.VSCode.Config (ServerConfig)
import           Rzk.TypeCheck              (Decl')

type RzkTypecheckCache = [(FilePath, [Decl'])]

data RzkEnv = RzkEnv
  { rzkEnvTypecheckCache :: TVar RzkTypecheckCache
  }

defaultRzkEnv :: IO RzkEnv
defaultRzkEnv = do
  typecheckCache <- newTVarIO []
  return RzkEnv
    { rzkEnvTypecheckCache = typecheckCache }


type LSP = LspT ServerConfig (ReaderT RzkEnv IO)

-- | Override the cache with given typechecked modules.
cacheTypecheckedModules :: RzkTypecheckCache -> LSP ()
cacheTypecheckedModules cache = lift $ do
  typecheckCache <- asks rzkEnvTypecheckCache
  liftIO $ atomically $ do
    writeTVar typecheckCache cache

-- | Completely invalidate the cache of typechecked files.
resetCacheForAllFiles :: LSP ()
resetCacheForAllFiles = cacheTypecheckedModules []

-- | Invalidate the cache for a list of file paths.
resetCacheForFiles :: [FilePath] -> LSP ()
resetCacheForFiles paths = lift $ do
  typecheckCache <- asks rzkEnvTypecheckCache
  liftIO $ atomically $ do
    modifyTVar typecheckCache (takeWhile ((`notElem` paths) . fst))

-- | Get the current state of the cache.
getCachedTypecheckedModules :: LSP RzkTypecheckCache
getCachedTypecheckedModules = lift $ do
  typecheckCache <- asks rzkEnvTypecheckCache
  liftIO $ readTVarIO typecheckCache
