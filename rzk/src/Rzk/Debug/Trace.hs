module Rzk.Debug.Trace where

import           Debug.Trace     (trace, traceShow)
import           Unsafe.Coerce   (unsafeCoerce)

import           Rzk.Pretty.Text ()
import           Rzk.Syntax.Term
import           Rzk.Syntax.Var

traceTerm :: Term Var -> a -> a
traceTerm = traceShow

unsafeTraceTerm :: Term var -> a -> a
unsafeTraceTerm = traceTerm . unsafeCoerce

traceTyping :: Term Var -> Term Var -> a -> a
traceTyping term ty = trace (show term <> "  :  " <> show ty)

unsafeTraceTyping :: Term var -> Term var -> a -> a
unsafeTraceTyping term ty
  = traceTyping (unsafeCoerce term) (unsafeCoerce ty)
