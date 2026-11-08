{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE QuantifiedConstraints  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}
-- | The checker's monads: what the typing rules need from their monad, as
-- a class, and its two instances.
--
-- The rules are written once against 'Checking'. The full instance keeps
-- the context's diagnostics (the stack of judgements, the narrowed
-- location), raises errors with their context, and records warnings and
-- holes rendered. The fast instance keeps none of that: an error is its
-- tag, a warning its kind, a hole a count, so that a status-only run pays
-- for nothing it does not report. The rules are polymorphic in the
-- instance, so both runs share them.
module Rzk.Check.Monad
  ( Checking (..)
  , CheckM
  , Full
  , Fast
  , CheckLog (..)
  , emptyCheckLog
  , Findings (..)
  , emptyFindings
  , runFull
  , runFast
    -- * Helpers
  , asksContext
  , withLocation
  , withOptions
  , issueTypeError
  , underTope
  , withVarianceM
  ) where

import           Control.Monad.Foil       (Distinct, S)
import           Data.Kind                (Type)
import           Debug.Trace              (trace)

import           Rzk.Check.Context
import           Rzk.Check.Error
import           Rzk.Check.Syntax
import           Rzk.Check.Topes

-- | What the typing rules need from their monad.
class (forall n. Monad (m n)) => Checking (m :: S -> Type -> Type) where
  -- | What the context keeps for diagnostics.
  type DiagOf m :: S -> Type
  -- | What a failed attempt returns.
  type FailureOf m :: Type
  askContext :: m n (Context (DiagOf m) n)
  -- | Run a judgement in another scope's context.
  inContext :: Context (DiagOf m) l -> m l a -> m n a
  localContext :: (Context (DiagOf m) n -> Context (DiagOf m) n) -> m n a -> m n a
  -- | Raise an error. The argument is a thunk: the fast instance never
  -- forces its fields.
  failWith :: Distinct n => TypeError n -> m n a
  -- | Run a judgement for its outcome, keeping what it recorded.
  attempt :: m n a -> m n (Either (FailureOf m) a)
  -- | Run a judgement as an action on the stack, with the location narrowed
  -- to the term it is about.
  performing :: Distinct n => Action n -> m n a -> m n a
  -- | Record a warning: its kind, and its rendering from the context.
  recordWarning :: Distinct n => WarningKind -> (Context (DiagOf m) n -> Warning) -> m n ()
  -- | Record a hole: its rendering from the context.
  recordHole :: Distinct n => (Context (DiagOf m) n -> HoleInfo) -> m n ()

-- | The one shape of both instances: a reader of the context and a state
-- of what the run records, with an error channel.
newtype CheckM (d :: S -> Type) e log n a = CheckM
  { unCheckM :: Context d n -> log -> (Either e a, log) }

instance Functor (CheckM d e log n) where
  fmap f (CheckM m) = CheckM $ \ctx l -> case m ctx l of
    (Left e, l')  -> (Left e, l')
    (Right a, l') -> (Right (f a), l')
  {-# INLINE fmap #-}

instance Applicative (CheckM d e log n) where
  pure a = CheckM $ \_ l -> (Right a, l)
  {-# INLINE pure #-}
  CheckM mf <*> CheckM ma = CheckM $ \ctx l -> case mf ctx l of
    (Left e, l')  -> (Left e, l')
    (Right f, l') -> case ma ctx l' of
      (Left e, l'')  -> (Left e, l'')
      (Right a, l'') -> (Right (f a), l'')
  {-# INLINE (<*>) #-}

instance Monad (CheckM d e log n) where
  CheckM m >>= k = CheckM $ \ctx l -> case m ctx l of
    (Left e, l')  -> (Left e, l')
    (Right a, l') -> unCheckM (k a) ctx l'
  {-# INLINE (>>=) #-}

-- | What a full run records: the holes it found and the warnings it made,
-- newest first.
data CheckLog = CheckLog
  { logHoles    :: [HoleInfo]
  , logWarnings :: [Warning]
  }

emptyCheckLog :: CheckLog
emptyCheckLog = CheckLog [] []

-- | What a fast run records: which kinds of warning fired, and how many
-- holes it met.
data Findings = Findings
  { findingsWarnings :: [WarningKind]
  , findingsHoles    :: !Int
  }

emptyFindings :: Findings
emptyFindings = Findings [] 0

-- | The full instance.
type Full = CheckM Diag ErrorInContext CheckLog

-- | The fast instance.
type Fast = CheckM NoDiag String Findings

runFull :: Context Diag n -> Full n a -> (Either ErrorInContext a, CheckLog)
runFull ctx (CheckM m) = m ctx emptyCheckLog

runFast :: Context NoDiag n -> Fast n a -> (Either String a, Findings)
runFast ctx (CheckM m) = m ctx emptyFindings

-- | The depth of nested judgements at which a full run gives up: the cap
-- catches a non-terminating search. The fast run keeps no depth.
maxDepth :: Int
maxDepth = 1000

instance Checking Full where
  type DiagOf Full = Diag
  type FailureOf Full = ErrorInContext
  askContext = CheckM $ \ctx l -> (Right ctx, l)
  {-# INLINE askContext #-}
  inContext ctx (CheckM m) = CheckM $ \_ l -> m ctx l
  {-# INLINE inContext #-}
  localContext f (CheckM m) = CheckM $ \ctx l -> m (f ctx) l
  {-# INLINE localContext #-}
  failWith err = CheckM $ \ctx l -> (Left (ErrorInContext ctx err), l)
  {-# INLINE failWith #-}
  attempt (CheckM m) = CheckM $ \ctx l -> case m ctx l of
    (Left e, l')  -> (Right (Left e), l')
    (Right a, l') -> (Right (Right a), l')
  {-# INLINE attempt #-}
  performing action (CheckM m) = CheckM $ \ctx l ->
    let diag = ctxDiag ctx
        depth = diagDepth diag
        ctx' = ctx
          { ctxDiag = Diag (action : diagActions diag) (depth + 1)
          , ctxLocation = narrowLocation action (ctxLocation ctx)
          }
    in if depth >= maxDepth
         then (Left (ErrorInContext ctx (TypeErrorOther "maximum depth reached")), l)
         else if optVerbosity (ctxOptions ctx) <= Debug
           then trace (unlines (map (replicate (2 * depth) ' ' <>) (lines (ppAction ctx action)))) (m ctx' l)
           else m ctx' l
  {-# INLINE performing #-}
  recordWarning _ render = CheckM $ \ctx l ->
    let w = render ctx
    in (Right (), if w `elem` logWarnings l then l else l { logWarnings = w : logWarnings l })
  {-# INLINE recordWarning #-}
  recordHole render = CheckM $ \ctx l -> (Right (), l { logHoles = render ctx : logHoles l })
  {-# INLINE recordHole #-}

instance Checking Fast where
  type DiagOf Fast = NoDiag
  type FailureOf Fast = String
  askContext = CheckM $ \ctx l -> (Right ctx, l)
  {-# INLINE askContext #-}
  inContext ctx (CheckM m) = CheckM $ \_ l -> m ctx l
  {-# INLINE inContext #-}
  localContext f (CheckM m) = CheckM $ \ctx l -> m (f ctx) l
  {-# INLINE localContext #-}
  failWith err = CheckM $ \_ l -> (Left (typeErrorTag err), l)
  {-# INLINE failWith #-}
  attempt (CheckM m) = CheckM $ \ctx l -> case m ctx l of
    (Left e, l')  -> (Right (Left e), l')
    (Right a, l') -> (Right (Right a), l')
  {-# INLINE attempt #-}
  performing action (CheckM m) = CheckM $ \ctx l ->
    m ctx { ctxLocation = narrowLocation action (ctxLocation ctx) } l
  {-# INLINE performing #-}
  recordWarning kind _ = CheckM $ \_ l ->
    (Right (), if kind `elem` findingsWarnings l then l else l { findingsWarnings = kind : findingsWarnings l })
  {-# INLINE recordWarning #-}
  recordHole _ = CheckM $ \_ l -> (Right (), l { findingsHoles = findingsHoles l + 1 })
  {-# INLINE recordHole #-}

-- | Point the location at the sub-term an action is about, when the
-- action's term carries a position.
narrowLocation :: Action n -> Maybe LocationInfo -> Maybe LocationInfo
narrowLocation action loc = case action of
  ActionCheck term _ | Just pos <- positionOfRaw term -> atPosition pos <$> loc
  ActionInfer term   | Just pos <- positionOfRaw term -> atPosition pos <$> loc
  _ -> loc

-- * Helpers

asksContext :: Checking m => (Context (DiagOf m) n -> a) -> m n a
asksContext f = f <$> askContext
{-# INLINE asksContext #-}

withLocation :: Checking m => LocationInfo -> m n a -> m n a
withLocation loc = localContext $ \ctx -> ctx { ctxLocation = Just loc }

withOptions :: Checking m => (Options -> Options) -> m n a -> m n a
withOptions f = localContext $ \ctx -> ctx { ctxOptions = f (ctxOptions ctx) }

issueTypeError :: (Checking m, Distinct n) => TypeError n -> m n a
issueTypeError = failWith
{-# INLINE issueTypeError #-}

-- | Run a judgement with a tope assumed.
underTope :: Checking m => Tope -> m n a -> m n a
underTope tope = localContext $ \ctx -> ctx { ctxTopes = assume tope (ctxTopes ctx) }
{-# INLINE underTope #-}

withVarianceM :: Checking m => Variance -> m n a -> m n a
withVarianceM v = localContext (withVariance v)
{-# INLINE withVarianceM #-}
