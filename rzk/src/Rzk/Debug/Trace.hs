module Rzk.Debug.Trace where

import           Debug.Trace     (trace)
import           Unsafe.Coerce   (unsafeCoerce)

import           Rzk.Pretty.Text ()
import           Rzk.Syntax.Term
import           Rzk.Syntax.Var

traceShow' :: Show a => a -> b -> b
traceShow' x = trace ("[debug] " <> show x)

traceTerm :: Term Var -> a -> a
traceTerm = traceShow'

unsafeTraceTerm :: Term var -> a -> a
unsafeTraceTerm = traceTerm . unsafeCoerce

traceTyping :: Term Var -> Term Var -> a -> a
traceTyping term ty = traceTerm (TypedTerm term ty)

unsafeTraceTyping :: Term var -> Term var -> a -> a
unsafeTraceTyping term ty
  = traceTyping (unsafeCoerce term) (unsafeCoerce ty)
