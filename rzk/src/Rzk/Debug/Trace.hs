{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Rzk.Debug.Trace where

import           Data.Typeable
import qualified Debug.Trace
import           System.Console.ANSI
import           Unsafe.Coerce       (unsafeCoerce)

import           Rzk.Pretty.Text     ()
import           Rzk.Syntax.Term
import           Rzk.Syntax.Var

unsafeTraceShowCoerce :: forall b a c. Show b => a -> c -> c
unsafeTraceShowCoerce = Debug.Trace.traceShow . (unsafeCoerce :: a -> b)

traceShowCast :: forall b a c. (Typeable a, Typeable b, Show b) => String -> a -> c -> c
traceShowCast fallback = Debug.Trace.trace . maybe fallback show . (cast :: a -> Maybe b)

traceShowCast' :: forall b a c. (Typeable a, Typeable b, Show b) => b -> a -> c -> c
traceShowCast' fallback = traceShowCast @b (show fallback)

traceShowCast_ :: forall b a c. (Typeable a, Typeable b, Show b) => a -> c -> c
traceShowCast_ = maybe id Debug.Trace.traceShow . (cast :: a -> Maybe b)

traceShowTag :: Show a => String -> a -> b -> b
traceShowTag tag x = Debug.Trace.trace (colored [SetColor Foreground Dull White] ("[" <> tag <> "] ") <> show x)

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
