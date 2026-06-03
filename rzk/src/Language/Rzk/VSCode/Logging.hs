module Language.Rzk.VSCode.Logging where

import           Colog.Core           (Severity (..), WithSeverity (..), (<&))
import qualified Data.Text            as T
import           Language.LSP.Logging (defaultClientLogger)
import           Language.LSP.Server  (MonadLsp)


logDebug :: MonadLsp c m => T.Text -> m ()
logDebug msg = defaultClientLogger <& msg `WithSeverity` Debug

logInfo :: MonadLsp c m => T.Text -> m ()
logInfo msg = defaultClientLogger <& msg `WithSeverity` Info

logWarning :: MonadLsp c m => T.Text -> m ()
logWarning msg = defaultClientLogger <& msg `WithSeverity` Warning

-- | Error logs will also be shown to the user via `window/showMessage`
logError :: MonadLsp c m => T.Text -> m ()
logError msg = defaultClientLogger <& msg `WithSeverity` Error
