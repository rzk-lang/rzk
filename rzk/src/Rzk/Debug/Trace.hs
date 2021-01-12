{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Rzk.Debug.Trace where

import           Data.IORef
import           System.IO.Unsafe

import           Data.Typeable
import qualified Debug.Trace
import           System.Console.ANSI
import           Unsafe.Coerce       (unsafeCoerce)

import           Rzk.Pretty.Text     ()
import           Rzk.Syntax.Term
import           Rzk.Syntax.Var

-- trace :: String -> b -> b
-- trace = const id
--
-- traceShow :: Show a => a -> b -> b
-- traceShow = const id

__GLOBAL_DEBUG_SCOPE_LEVEL :: IORef Int
__GLOBAL_DEBUG_SCOPE_LEVEL = unsafePerformIO (newIORef (-1))

incrementScope :: IO Int
incrementScope = do
  modifyIORef __GLOBAL_DEBUG_SCOPE_LEVEL (+1)
  readIORef __GLOBAL_DEBUG_SCOPE_LEVEL

decrementScope :: IO Int
decrementScope = do
  modifyIORef __GLOBAL_DEBUG_SCOPE_LEVEL (subtract 1)
  readIORef __GLOBAL_DEBUG_SCOPE_LEVEL

getScope :: IO Int
getScope = readIORef __GLOBAL_DEBUG_SCOPE_LEVEL

scoped :: a -> a
scoped x = unsafePerformIO incrementScope `seq` x `seq` unsafePerformIO decrementScope `seq` x
{-# NOINLINE scoped #-}

indentWithScope :: String -> String
indentWithScope s = replicate (unsafePerformIO getScope) '-' <> s
{-# NOINLINE indentWithScope #-}

trace :: String -> a -> a
trace s x = scoped (Debug.Trace.trace (indentWithScope s) x)
{-# NOINLINE trace #-}

traceShow :: Show a => a -> b -> b
traceShow x = trace (show x)
{-# NOINLINE traceShow #-}

unsafeTraceShowCoerce :: forall b a c. Show b => a -> c -> c
unsafeTraceShowCoerce = traceShow . (unsafeCoerce :: a -> b)

traceShowCast :: forall b a c. (Typeable a, Typeable b, Show b) => String -> a -> c -> c
traceShowCast fallback = trace . maybe fallback show . (cast :: a -> Maybe b)

traceShowCast' :: forall b a c. (Typeable a, Typeable b, Show b) => b -> a -> c -> c
traceShowCast' fallback = traceShowCast @b (show fallback)

traceShowCast_ :: forall b a c. (Typeable a, Typeable b, Show b) => a -> c -> c
traceShowCast_ = maybe id traceShow . (cast :: a -> Maybe b)

traceShowTag :: Show a => String -> a -> b -> b
traceShowTag tag x = trace (colored [SetColor Foreground Dull White] ("[" <> tag <> "] ") <> show x)

traceTerm :: String -> Term Var -> a -> a
traceTerm = traceShowTag

unsafeTraceTerm :: String -> Term var -> a -> a
unsafeTraceTerm tag = traceTerm tag . unsafeCoerce

traceTyping :: String -> Term Var -> Term Var -> a -> a
traceTyping tag term ty = traceTerm tag (TypedTerm term ty)

unsafeTraceTyping :: String -> Term var -> Term var -> a -> a
unsafeTraceTyping tag term ty
  = traceTyping tag (unsafeCoerce term) (unsafeCoerce ty)

colored :: [SGR] -> String -> String
colored sgrs t = prefix <> t <> suffix
  where
    prefix = setSGRCode sgrs
    suffix = setSGRCode [Reset]
