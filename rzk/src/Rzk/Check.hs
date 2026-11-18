{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | The new checker's entry point: check modules, and report the errors,
-- warnings, hints, and holes with their diagnostics.
module Rzk.Check
  ( Checked (..)
  , checkModules
  , module Rzk.Check.Context
  , module Rzk.Check.Error
  ) where

import           Control.Monad.Foil       (Distinct)
import qualified Control.Monad.Foil       as Foil

import qualified Language.Rzk.Syntax      as Rzk
import           Rzk.Check.Context
import qualified Rzk.Check.Decl           as Decl
import           Rzk.Check.Decl           (Decl)
import           Rzk.Check.Error
import           Rzk.Check.Monad

-- | What a run produced: the final scope's context, the declarations by
-- file, and the errors, warnings, hints, and holes found.
data Checked where
  Checked
    :: Distinct n
    => ContextWithScope n
    -> [(FilePath, [Decl n])]
    -> [ErrorInContext]
    -> [Warning]
    -> [Hint]
    -> [HoleInfo]
    -> Checked

-- | Check the modules.
checkModules :: Options -> [(FilePath, Rzk.Module)] -> Checked
checkModules options modules =
  case runCheck ctx0 (Decl.checkModules modules $ \decls errs -> do
                        ctx <- ContextWithScope <$> askScope <*> askContext
                        pure (Checked ctx decls errs [] [] [])) of
    (Right (Checked ctx decls errs _ _ _), CheckLog holes warnings hints) ->
      Checked ctx decls errs (reverse warnings) (reverse hints) (reverse holes)
    (Left err, CheckLog holes warnings hints) ->
      Checked ctx0 [] [err] (reverse warnings) (reverse hints) (reverse holes)
  where
    ctx0 = ContextWithScope Foil.emptyScope (emptyContext { ctxOptions = options })
