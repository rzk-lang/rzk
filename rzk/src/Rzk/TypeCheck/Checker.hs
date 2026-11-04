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
import           Control.Monad.Free.Foil           (AST (Node, Var),
                                                    ScopedAST (..))
import           Control.Monad.Free.Foil.Annotated (AnnSig (..))
import           Data.Bifoldable                   (bifold)
import           Data.Bitraversable                (bitraverse)
import           Data.Bifunctor                    (first)
import qualified FreeFoil.NbE.Conversion           as Conv

import           Language.Rzk.Foil.Syntax
import           Rzk.TypeCheck.Context
import           Rzk.TypeCheck.Eval                (freeVarsDeep, inScope,
                                                    nubNames, typeOfUncomputed)
import qualified Rzk.TypeCheck.Judgements          as Terms
import qualified Rzk.TypeCheck.Judgements.Values   as Values
import           Rzk.TypeCheck.Monad
import           Rzk.TypeCheck.NbE                 (evalTerm, rootCtx)
import           Rzk.TypeCheck.Render              (ppInContext)
import           Language.Rzk.Foil.Names           (Binder (..), TModality (..),
                                                    TypeInfo (..))

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
-- agree when they are α-equal without their annotations (and without the
-- domain annotations of their lambdas, which the two judgements spell
-- differently), or else convertible by the library's conversion, and so
-- are their types, and when they depend on the same names (through their
-- annotations, as a declaration's assumptions are found).
compared :: Distinct n => TypeCheck n (TermT n) -> TypeCheck n (TermT n) -> TypeCheck n (TermT n)
compared reference candidate = do
  outcome <- attempt reference
  outcome' <- suppressing (attempt candidate)
  verdict <- case (outcome, outcome') of
    (Right r, Right c) -> do
      ctx <- asks id
      let scope = ctxScope ctx
          alike l r' = alphaEqT scope (withoutLambdaDomains l) (withoutLambdaDomains r')
            || Conv.convertible scope (rootCtx ctx) l r'
      rTy <- typeOfUncomputed r
      cTy <- typeOfUncomputed c
      let same = alike r c
          sameType = alike rTy cTy
      depsR <- nubNames <$> freeVarsDeep r
      depsC <- nubNames <$> freeVarsDeep c
      let byId = sortOn Foil.nameId
          extraC = byId depsC \\ byId depsR
          extraR = byId depsR \\ byId depsC
      case (same, sameType) of
        (True, True)
          | null extraC && null extraR -> pure Agreement
          | otherwise -> do
              extraCStr <- mapM (ppInContext . Var) extraC
              extraRStr <- mapM (ppInContext . Var) extraR
              throughC <- annotationsMentioning extraC c
              throughR <- annotationsMentioning extraR r
              rStr <- ppInContext r
              cStr <- ppInContext c
              pure $ Disagreement $ "dependencies: only over values " <> show extraCStr
                <> ", only on terms " <> show extraRStr
                <> concat [ "\n  over values, " <> d | d <- throughC ]
                <> concat [ "\n  on terms, " <> d | d <- throughR ]
                <> "\n  on terms: " <> rStr <> "\n  over values: " <> cStr
        _ -> do
          rStr <- ppInContext (if same then rTy else r)
          cStr <- ppInContext (if same then cTy else c)
          pure $ Disagreement $ (if same then "the types: " else "the terms: ")
            <> "on terms " <> rStr <> ", over values " <> cStr
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

-- | The nodes of a term whose annotation mentions one of the names, with
-- the annotation, each printed in its own scope.
annotationsMentioning :: Distinct n => [Foil.Name n] -> TermT n -> TypeCheck n [String]
annotationsMentioning names = go
  where
    ids = map Foil.nameId names
    go :: Distinct l => TermT l -> TypeCheck l [String]
    go (Var _) = pure []
    go t@(Node (AnnSig info node)) = do
      here <- if any ((`elem` ids) . Foil.nameId) (freeVarsOfTermT (infoType info))
        then do
          nodeStr <- ppInContext t
          annStr <- ppInContext (infoType info)
          pure ["the annotation of " <> nodeStr <> " is " <> annStr]
        else pure []
      below <- bitraverse (\scoped -> inScope (BinderVar Nothing) Id universeT scoped go) go node
      pure (here <> bifold below)

-- | A term with the domain annotations of its lambdas dropped.
withoutLambdaDomains :: TermT n -> TermT n
withoutLambdaDomains = \case
  Var x -> Var x
  Node (AnnSig info node) -> Node $ AnnSig info $ case node of
    LambdaF orig _ body -> LambdaF orig Nothing (scoped body)
    _ -> bimapNode node
  where
    scoped (ScopedAST b body) = ScopedAST b (withoutLambdaDomains body)
    bimapNode = first scoped . fmap withoutLambdaDomains


tally :: Distinct n => Verdict -> TypeCheck n ()
tally verdict = do
  modifyLog $ \l -> l { logCheckerStats = count (logCheckerStats l) }
  case verdict of
    Disagreement how -> do
      loc <- asks ctxLocation
      let what = "a declaration" <> maybe "" (\l -> " at " <> show (locationLine l)) loc
      recordCheckWarning (CheckerDisagreementWarning what how loc)
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
