{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | The new checker's entry points: a full run, which reports errors,
-- warnings and holes with their diagnostics, and a fast run, which reports
-- what happened.
module Rzk.Check
  ( Checked (..)
  , checkModulesFull
  , Status (..)
  , checkModulesFast
  , module Rzk.Check.Context
  , module Rzk.Check.Error
  ) where

import           Control.Monad.Foil       (Distinct)

import qualified Language.Rzk.Syntax      as Rzk
import           Rzk.Check.Context
import           Rzk.Check.Decl
import           Rzk.Check.Error
import           Rzk.Check.Monad

-- | What a full run produced: the final scope's context, the declarations
-- by file, and the errors, warnings and holes found.
data Checked where
  Checked
    :: Distinct n
    => Context Diag n
    -> [(FilePath, [Decl n])]
    -> [ErrorInContext]
    -> [Warning]
    -> [HoleInfo]
    -> Checked

-- | Check the modules with full diagnostics.
checkModulesFull :: Options -> [(FilePath, Rzk.Module)] -> Checked
checkModulesFull options modules =
  case runFull ctx0 (checkModules modules $ \decls errs -> do
                       ctx <- askContext
                       pure (Checked ctx decls errs [] [])) of
    (Right (Checked ctx decls errs _ _), CheckLog holes warnings) ->
      Checked ctx decls errs (reverse warnings) (reverse holes)
    (Left err, CheckLog holes warnings) ->
      Checked ctx0 [] [err] (reverse warnings) (reverse holes)
  where
    ctx0 = (emptyContext emptyDiag) { ctxOptions = options }

-- | What a fast run produced: the tags of the errors, and the findings.
data Status = Status
  { statusErrors   :: [String]
  , statusFindings :: Findings
  }

-- | Check the modules for their status only.
checkModulesFast :: Options -> [(FilePath, Rzk.Module)] -> Status
checkModulesFast options modules =
  case runFast ctx0 (checkModules modules $ \_ errs -> pure errs) of
    (Right errs, findings) -> Status errs findings
    (Left err, findings)   -> Status [err] findings
  where
    ctx0 = (emptyContext NoDiag) { ctxOptions = options }

