{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE LambdaCase #-}
-- | The judgements a run uses, chosen by the context's 'ctxChecker': the ones
-- on terms ("Rzk.TypeCheck.Judgements"), the ones over values
-- ("Rzk.TypeCheck.Judgements.Values"), or both compared on every
-- declaration, the differential test. "Rzk.TypeCheck.Decl" calls these.
module Rzk.TypeCheck.Checker
  ( typecheck
  , infer
  , reportCheckerDifferential
  ) where

import           Control.Monad.Except              (catchError, throwError)
import           Control.Monad.Reader              (asks)
import           Data.List                         (sortOn, (\\))

import           Control.Monad.Foil                (Distinct)
import qualified Control.Monad.Foil                as Foil
import           Control.Monad.Free.Foil           (AST (Var))

import           Language.Rzk.Foil.Syntax
import           Rzk.TypeCheck.Context
import           Rzk.TypeCheck.Eval                (freeVarsDeep, nubNames,
                                                    typeOfUncomputed)
import qualified Rzk.TypeCheck.Judgements          as Terms
import qualified Rzk.TypeCheck.Judgements.Values   as Values
import           Rzk.TypeCheck.Monad
import           Rzk.TypeCheck.NbE                 (evalTerm)
import           Rzk.TypeCheck.Render              (ppInContext)
import           Rzk.TypeCheck.Unify               (unifyTerms)

-- | Check a term against a type, by the judgements the context names.
typecheck :: Distinct n => Term n -> TermT n -> TypeCheck n (TermT n)
typecheck term ty = asks ctxChecker >>= \case
  CheckerTerms  -> Terms.typecheck term ty
  CheckerValues -> byValues
  CheckerCheck  -> compared (Terms.typecheck term ty) byValues
  where
    byValues = do
      tyV <- asks (`evalTerm` ty)
      Values.check term tyV

-- | Infer the type of a term, by the judgements the context names.
infer :: Distinct n => Term n -> TypeCheck n (TermT n)
infer term = asks ctxChecker >>= \case
  CheckerTerms  -> Terms.infer term
  CheckerValues -> fst <$> Values.infer term
  CheckerCheck  -> compared (Terms.infer term) (fst <$> Values.infer term)

-- | The differential test on one judgement: the judgements on terms decide
-- it, and what they recorded stands; the judgements over values run on the
-- side, recording nothing, and a disagreement is a warning. Two results
-- agree when they are α-equal without their annotations, or else
-- convertible, and so are their types, and when they depend on the same
-- names (through their annotations, as a declaration's assumptions are
-- found).
compared :: Distinct n => TypeCheck n (TermT n) -> TypeCheck n (TermT n) -> TypeCheck n (TermT n)
compared reference candidate = do
  outcome <- attempt reference
  outcome' <- suppressing (attempt candidate)
  verdict <- case (outcome, outcome') of
    (Right r, Right c) -> do
      scope <- asks ctxScope
      let alike l r' = alphaEqT scope l r'
      same <- if alike r c then pure (Right ()) else
        suppressing (attempt (unifyTerms r c >> unifyTerms c r))
      sameType <- suppressing $ attempt $ do
        rTy <- typeOfUncomputed r
        cTy <- typeOfUncomputed c
        if alike rTy cTy then pure () else unifyTerms rTy cTy >> unifyTerms cTy rTy
      depsR <- nubNames <$> freeVarsDeep r
      depsC <- nubNames <$> freeVarsDeep c
      let byId = sortOn Foil.nameId
          extraC = byId depsC \\ byId depsR
          extraR = byId depsR \\ byId depsC
      case (same, sameType) of
        (Right (), Right ())
          | null extraC && null extraR -> pure Agreement
          | otherwise -> do
              extraCStr <- mapM (ppInContext . Var) extraC
              extraRStr <- mapM (ppInContext . Var) extraR
              pure $ Disagreement $ "dependencies: only over values " <> show extraCStr
                <> ", only on terms " <> show extraRStr
        _ -> do
          rStr <- ppInContext r
          cStr <- ppInContext c
          pure $ Disagreement $ "on terms " <> rStr <> ", over values " <> cStr
    (Left _, Left _) -> pure BothFailed
    (Right r, Left _) -> do
      rStr <- ppInContext r
      pure $ Disagreement $ "over values rejected what the judgements on terms accept: " <> rStr
    (Left _, Right c) -> do
      cStr <- ppInContext c
      pure $ Disagreement $ "over values accepted what the judgements on terms reject: " <> cStr
  tally verdict
  either throwError pure outcome
  where
    attempt action = (Right <$> action) `catchError` (pure . Left)

data Verdict = Agreement | BothFailed | Disagreement String

tally :: Distinct n => Verdict -> TypeCheck n ()
tally verdict = do
  modifyLog $ \l -> l { logCheckerStats = count (logCheckerStats l) }
  case verdict of
    Disagreement how -> do
      loc <- asks ctxLocation
      recordCheckWarning (CheckerDisagreementWarning "a declaration" how loc)
    _ -> pure ()
  where
    count stats = (stats { checkerQueries = checkerQueries stats + 1 }) `with` verdict
    with stats = \case
      Agreement      -> stats { checkerAgreements = checkerAgreements stats + 1 }
      BothFailed     -> stats { checkerBothFailed = checkerBothFailed stats + 1 }
      Disagreement _ -> stats { checkerDisagreements = checkerDisagreements stats + 1 }

-- | Under 'CheckerCheck', the tallies of the differential test, as a warning
-- at the end of the run.
reportCheckerDifferential :: Distinct n => TypeCheck n ()
reportCheckerDifferential = asks ctxChecker >>= \case
  CheckerCheck -> do
    stats <- getsLog logCheckerStats
    recordCheckWarning (CheckerDifferentialReport stats)
  _ -> pure ()
