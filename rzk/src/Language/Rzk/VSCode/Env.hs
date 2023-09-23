module Language.Rzk.VSCode.Env where

import           Control.Concurrent.STM
import           Control.Monad.Reader
import           Language.LSP.Server
import           Rzk.TypeCheck          (Decl')

type RzkTypecheckCache = [(FilePath, [Decl'])]

data RzkEnv = RzkEnv
  { rzkEnvTypecheckCache :: TVar RzkTypecheckCache
  }

defaultRzkEnv :: IO RzkEnv
defaultRzkEnv = do
  typecheckCache <- newTVarIO []
  return RzkEnv
    { rzkEnvTypecheckCache = typecheckCache }


type LSP = LspT () (ReaderT RzkEnv IO)

cacheTypecheckedModules :: RzkTypecheckCache -> LSP ()
cacheTypecheckedModules cache = lift $ do
  typecheckCache <- asks rzkEnvTypecheckCache
  liftIO $ atomically $ do
    writeTVar typecheckCache cache

resetCacheForFiles :: [FilePath] -> LSP ()
resetCacheForFiles paths = lift $ do
  typecheckCache <- asks rzkEnvTypecheckCache
  liftIO $ atomically $ do
    modifyTVar typecheckCache (takeWhile ((`notElem` paths) . fst))

getCachedTypecheckedModules :: LSP RzkTypecheckCache
getCachedTypecheckedModules = lift $ do
  typecheckCache <- asks rzkEnvTypecheckCache
  liftIO $ readTVarIO typecheckCache
