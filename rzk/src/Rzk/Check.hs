{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | The new checker's entry point: check modules, and report the errors,
-- warnings and holes with their diagnostics.
module Rzk.Check
  ( Checked (..)
  , checkModules
  , module Rzk.Check.Context
  , module Rzk.Check.Error
  ) where

import           Control.Monad.Foil       (Distinct)

import qualified Language.Rzk.Syntax      as Rzk
import           Rzk.Check.Context
import qualified Rzk.Check.Decl           as Decl
import           Rzk.Check.Decl           (Decl)
import           Rzk.Check.Error
import           Rzk.Check.Monad

-- | What a run produced: the final scope's context, the declarations by
-- file, and the errors, warnings and holes found.
data Checked where
  Checked
    :: Distinct n
    => Context n
    -> [(FilePath, [Decl n])]
    -> [ErrorInContext]
    -> [Warning]
    -> [HoleInfo]
    -> Checked

-- | Check the modules.
checkModules :: Options -> [(FilePath, Rzk.Module)] -> Checked
checkModules options modules =
  case runCheck ctx0 (Decl.checkModules modules $ \decls errs -> do
                        ctx <- askContext
                        pure (Checked ctx decls errs [] [])) of
    (Right (Checked ctx decls errs _ _), CheckLog holes warnings) ->
      Checked ctx decls errs (reverse warnings) (reverse holes)
    (Left err, CheckLog holes warnings) ->
      Checked ctx0 [] [err] (reverse warnings) (reverse holes)
  where
    ctx0 = emptyContext { ctxOptions = options }
