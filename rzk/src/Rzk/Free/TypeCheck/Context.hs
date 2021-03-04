module Rzk.Free.TypeCheck.Context where

import           Bound

import           Rzk.Free.Bound.Name
import           Rzk.Free.Syntax.Term

data Context b a = Context
  { contextKnownTypes :: a -> Maybe (TypedTerm b a)
  -- ^ Types for free variables.
  }

withBound :: TypedTerm b a -> Context b a -> Context b (Var (Name b ()) a)
withBound typeOfBoundVar context = context
  { contextKnownTypes = contextKnownTypes' }
    where
      contextKnownTypes' (B _) = Just (F <$> typeOfBoundVar)
      contextKnownTypes' (F x) = fmap F <$> contextKnownTypes context x

emptyContext :: Context b a
emptyContext = Context
  { contextKnownTypes = const Nothing
  }
