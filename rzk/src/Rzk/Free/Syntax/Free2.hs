module Rzk.Free.Syntax.Free2 where

import           Control.Monad  (ap)
import           Data.Bifunctor

data Free s f a
  = Pure a
  | Free (f (Free s f (s (Free s f a))) (Free s f a))

instance (Functor s, Bifunctor f) => Functor (Free s f) where
  fmap f (Pure x) = Pure (f x)
  fmap f (Free k) = Free (bimap (fmap (fmap (fmap f))) (fmap f) k)

instance (Functor s, Bifunctor f) => Applicative (Free s f) where
  pure = return
  (<*>) = ap

instance (Functor s, Bifunctor f) => Monad (Free s f) where
  return = Pure
  Pure x >>= f = f x
  Free k >>= f = Free (bimap (fmap (fmap (>>= f))) (>>= f) k)
