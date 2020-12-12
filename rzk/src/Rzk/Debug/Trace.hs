module Rzk.Debug.Trace where

import           Debug.Trace     (trace)
import           Unsafe.Coerce   (unsafeCoerce)

import           Rzk.Pretty.Text ()
import           Rzk.Syntax.Term
import           Rzk.Syntax.Var

traceShow' :: Show a => String -> a -> b -> b
traceShow' tag x = trace ("[" <> tag <> "] " <> show x)

traceTerm :: String -> Term Var -> a -> a
traceTerm = traceShow'

unsafeTraceTerm :: String -> Term var -> a -> a
unsafeTraceTerm tag = traceTerm tag . unsafeCoerce

traceTyping :: String -> Term Var -> Term Var -> a -> a
traceTyping tag term ty = traceTerm tag (TypedTerm term ty)

unsafeTraceTyping :: String -> Term var -> Term var -> a -> a
unsafeTraceTyping tag term ty
  = traceTyping tag (unsafeCoerce term) (unsafeCoerce ty)
