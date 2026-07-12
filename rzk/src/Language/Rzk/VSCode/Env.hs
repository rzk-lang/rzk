module Language.Rzk.VSCode.Env where

import           Control.Concurrent.STM
import           Control.Monad.Reader
import qualified Data.Map.Strict            as Map
import qualified Data.Text                  as T
import           Language.LSP.Server
import           Language.Rzk.Free.Syntax   (VarIdent)
import           Language.Rzk.Syntax        (Module)
import qualified Language.Rzk.VSCode.Config as RzkConfig
import qualified Language.Rzk.VSCode.ReferenceIndex as RefInd
import           Rzk.TypeCheck              (Decl', TypeErrorInScopedContext)

data RzkCachedModule = RzkCachedModule
  { cachedModuleDecls  :: [Decl']
  , cachedModuleErrors :: [TypeErrorInScopedContext VarIdent]
  }

type RzkTypecheckCache = [(FilePath, RzkCachedModule)]

-- | A parse result for the reference index, together with the source text it
-- was parsed from ('Nothing' when parsed from disk rather than the editor
-- buffer; a failed parse is a 'Nothing' module and is not retried until the
-- source changes).
data ParsedModule = ParsedModule
  { parsedSource :: Maybe T.Text
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
  }

defaultRzkEnv :: IO RzkEnv
defaultRzkEnv = do
  typecheckCache <- newTVarIO []
  referenceIndexCache <- newTVarIO emptyReferenceIndexCache
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
    -- A full typecheck follows files changing on disk; drop the parses too.
    writeTVar referenceIndexCache emptyReferenceIndexCache

resetCacheForAllFiles :: LSP ()
resetCacheForAllFiles = cacheTypecheckedModules []

resetCacheForFiles :: [FilePath] -> LSP ()
resetCacheForFiles paths = lift $ do
  typecheckCache <- asks rzkEnvTypecheckCache
  referenceIndexCache <- asks rzkEnvReferenceIndexCache
  liftIO $ atomically $ do
    modifyTVar typecheckCache (takeWhile ((`notElem` paths) . fst))
    modifyTVar referenceIndexCache $ \c -> ReferenceIndexCache
      { indexCacheModules = foldr Map.delete (indexCacheModules c) paths
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
