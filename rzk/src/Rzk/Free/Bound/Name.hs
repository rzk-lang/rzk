{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell   #-}
module Rzk.Free.Bound.Name where

import           Bound.Scope
import           Bound.Var
import           Data.Bifunctor.TH

data Name n b = Name (Maybe n) b
  deriving (Show, Functor, Foldable, Traversable)

instance Eq b => Eq (Name n b) where
  Name _ x == Name _ y = x == y
instance Ord b => Ord (Name n b) where
  Name _ x `compare` Name _ y = x `compare` y

deriveBifunctor ''Name
deriveBifoldable ''Name
deriveBitraversable ''Name

abstractName :: (Monad f) => (a -> Maybe b) -> f a -> Scope (Name a b) f a
abstractName f t = Scope (fmap g t)
  where
    g y = case f y of
            Just x  -> B (Name (Just y) x)
            Nothing -> F (return y)

abstract1Name :: (Monad f, Eq a) => a -> f a -> Scope (Name a ()) f a
abstract1Name x = abstractName (\y -> if x == y then Just () else Nothing)

abstract1Unnamed :: Monad f => f (Maybe a) -> Scope (Name b ()) f a
abstract1Unnamed t = Scope (fmap g t)
  where
    g Nothing  = B (Name Nothing ())
    g (Just y) = F (return y)

instantiateName :: Monad f => (b -> f a) -> Scope (Name n b) f a -> f a
instantiateName f (Scope t) = t >>= g
  where
    g (B (Name _ b)) = f b
    g (F x)          = x

instantiate1Name :: Monad f => f a -> Scope (Name n ()) f a -> f a
instantiate1Name f = instantiateName (const f)
