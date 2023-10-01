module Language.Rzk.VSCode.Logging where

import           Colog.Core           (Severity (..), WithSeverity (..), (<&))
import qualified Data.Text            as T
import           Language.LSP.Logging (defaultClientLogger)
import           Language.LSP.Server  (MonadLsp)


logDebug :: MonadLsp c m => String -> m ()
logDebug msg = defaultClientLogger <& T.pack msg `WithSeverity` Debug

logInfo :: MonadLsp c m => String -> m ()
logInfo msg = defaultClientLogger <& T.pack msg `WithSeverity` Info

logWarning :: MonadLsp c m => String -> m ()
logWarning msg = defaultClientLogger <& T.pack msg `WithSeverity` Warning

-- | Error logs will also be shown to the user via `window/showMessage`
logError :: MonadLsp c m => String -> m ()
logError msg = defaultClientLogger <& T.pack msg `WithSeverity` Error
