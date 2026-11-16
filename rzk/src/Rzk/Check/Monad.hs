{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes     #-}
-- | The checker's monad: a reader of the context and a state of what the
-- run records (holes and warnings), with errors raised in their context.
-- The context keeps the diagnostics: the stack of judgements and the
-- location narrowed to the term a judgement is about.
module Rzk.Check.Monad
  ( Check
  , CheckLog (..)
  , emptyCheckLog
  , runCheck
  , askContext
  , inContext
  , localContext
  , failWith
  , attempt
  , performing
  , recordWarning
  , recordHole
    -- * Helpers
  , asksContext
  , withLocation
  , withOptions
  , issueTypeError
  , underTope
  , withVarianceM
  ) where

import           Control.Monad.Foil       (Distinct)
import           Debug.Trace              (trace)

import           Rzk.Check.Context
import           Rzk.Check.Error
import           Rzk.Check.Syntax
import           Rzk.Check.Topes

-- | The checker's monad.
newtype Check n a = Check
  { unCheck :: Context n -> CheckLog -> (Either ErrorInContext a, CheckLog) }

instance Functor (Check n) where
  fmap f (Check m) = Check $ \ctx l -> case m ctx l of
    (Left e, l')  -> (Left e, l')
    (Right a, l') -> (Right (f a), l')
  {-# INLINE fmap #-}

instance Applicative (Check n) where
  pure a = Check $ \_ l -> (Right a, l)
  {-# INLINE pure #-}
  Check mf <*> Check ma = Check $ \ctx l -> case mf ctx l of
    (Left e, l')  -> (Left e, l')
    (Right f, l') -> case ma ctx l' of
      (Left e, l'')  -> (Left e, l'')
      (Right a, l'') -> (Right (f a), l'')
  {-# INLINE (<*>) #-}

instance Monad (Check n) where
  Check m >>= k = Check $ \ctx l -> case m ctx l of
    (Left e, l')  -> (Left e, l')
    (Right a, l') -> unCheck (k a) ctx l'
  {-# INLINE (>>=) #-}

-- | What a run records: the holes it found and the warnings it made,
-- newest first.
data CheckLog = CheckLog
  { logHoles    :: [HoleInfo]
  , logWarnings :: [Warning]
  }

emptyCheckLog :: CheckLog
emptyCheckLog = CheckLog [] []

runCheck :: Context n -> Check n a -> (Either ErrorInContext a, CheckLog)
runCheck ctx (Check m) = m ctx emptyCheckLog

-- | The depth of nested judgements at which checking gives up: the cap
-- catches a non-terminating search.
maxDepth :: Int
maxDepth = 1000

askContext :: Check n (Context n)
askContext = Check $ \ctx l -> (Right ctx, l)
{-# INLINE askContext #-}

-- | Run a judgement in another scope's context.
inContext :: Context l -> Check l a -> Check n a
inContext ctx (Check m) = Check $ \_ l -> m ctx l
{-# INLINE inContext #-}

localContext :: (Context n -> Context n) -> Check n a -> Check n a
localContext f (Check m) = Check $ \ctx l -> m (f ctx) l
{-# INLINE localContext #-}

-- | Raise an error, in the current context.
failWith :: Distinct n => TypeError n -> Check n a
failWith err = Check $ \ctx l -> (Left (ErrorInContext ctx err), l)
{-# INLINE failWith #-}

-- | Run a judgement for its outcome, keeping what it recorded.
attempt :: Check n a -> Check n (Either ErrorInContext a)
attempt (Check m) = Check $ \ctx l -> case m ctx l of
  (Left e, l')  -> (Right (Left e), l')
  (Right a, l') -> (Right (Right a), l')
{-# INLINE attempt #-}

-- | Run a judgement as an action on the stack, with the location narrowed
-- to the term it is about.
performing :: Distinct n => Action n -> Check n a -> Check n a
performing action (Check m) = Check $ \ctx l ->
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

-- | Record a warning, rendered from the context, once.
recordWarning :: (Context n -> Warning) -> Check n ()
recordWarning render = Check $ \ctx l ->
  let w = render ctx
  in (Right (), if w `elem` logWarnings l then l else l { logWarnings = w : logWarnings l })
{-# INLINE recordWarning #-}

-- | Record a hole, rendered from the context.
recordHole :: (Context n -> HoleInfo) -> Check n ()
recordHole render = Check $ \ctx l -> (Right (), l { logHoles = render ctx : logHoles l })
{-# INLINE recordHole #-}

-- | Point the location at the sub-term an action is about, when the
-- action's term carries a position.
narrowLocation :: Action n -> Maybe LocationInfo -> Maybe LocationInfo
narrowLocation action loc = case action of
  ActionCheck term _ | Just pos <- positionOfRaw term -> atPosition pos <$> loc
  ActionInfer term   | Just pos <- positionOfRaw term -> atPosition pos <$> loc
  _ -> loc

-- * Helpers

asksContext :: (Context n -> a) -> Check n a
asksContext f = f <$> askContext
{-# INLINE asksContext #-}

withLocation :: LocationInfo -> Check n a -> Check n a
withLocation loc = localContext $ \ctx -> ctx { ctxLocation = Just loc }

withOptions :: (Options -> Options) -> Check n a -> Check n a
withOptions f = localContext $ \ctx -> ctx { ctxOptions = f (ctxOptions ctx) }

issueTypeError :: Distinct n => TypeError n -> Check n a
issueTypeError = failWith
{-# INLINE issueTypeError #-}

-- | Run a judgement with a tope assumed.
underTope :: Tope -> Check n a -> Check n a
underTope tope = localContext $ \ctx -> ctx { ctxTopes = assume tope (ctxTopes ctx) }
{-# INLINE underTope #-}

withVarianceM :: Variance -> Check n a -> Check n a
withVarianceM v = localContext (withVariance v)
{-# INLINE withVarianceM #-}
