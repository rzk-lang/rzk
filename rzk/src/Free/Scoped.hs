{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE QuantifiedConstraints    #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
module Free.Scoped where

import           Control.Monad      (ap)
import           Data.Bifoldable
import           Data.Bifunctor
import           Data.Bifunctor.TH
import           Data.Bitraversable
import qualified GHC.Generics       as GHC

data Inc var = Z | S var
  deriving (Eq, Show, Functor, Foldable, Traversable)

type Scope term var = term (Inc var)

instantiate :: Monad f => f a -> f (Inc a) -> f a
instantiate e f = f >>= g
  where
    g Z     = e
    g (S x) = return x

data FS t a
  = Pure a
  | Free (t (Scope (FS t) a) (FS t a))

deriving instance (Eq a, forall x y. (Eq x, Eq y) => Eq (t x y)) => Eq (FS t a)

instance Bifunctor t => Functor (FS t) where
  fmap f (Pure x) = Pure (f x)
  fmap f (Free t) = Free
    (bimap (fmap (fmap f)) (fmap f) t)

instance Bifoldable t => Foldable (FS t) where
  foldMap f (Pure x) = f x
  foldMap f (Free t) = bifoldMap (foldMap (foldMap f)) (foldMap f) t

instance Bitraversable t => Traversable (FS t) where
  traverse f (Pure x) = Pure <$> f x
  traverse f (Free t) = Free <$>
    bitraverse (traverse (traverse f)) (traverse f) t

instance Bifunctor t => Applicative (FS t) where
  pure = Pure
  (<*>) = ap

instance Bifunctor t => Monad (FS t) where
  return = pure
  Pure x >>= f = f x
  Free t >>= f = Free
    (bimap ((>>= traverse f)) (>>= f) t)

data Sum f g scope term
  = InL (f scope term)
  | InR (g scope term)
  deriving (Functor, Foldable, Traversable, GHC.Generic)
deriveBifunctor ''Sum
deriveBifoldable ''Sum
deriveBitraversable ''Sum

type (:+:) = Sum

data Empty scope term
  deriving (Functor, Foldable, Traversable)
deriveBifunctor ''Empty
deriveBifoldable ''Empty
deriveBitraversable ''Empty

data TypedF term scope typedTerm = TypedF
  { typeF :: typedTerm
  , termF :: term scope typedTerm
  } deriving (Eq, Show, Functor)

instance Bifunctor term => Bifunctor (TypedF term) where
  bimap f g (TypedF t x) = TypedF (g t) (bimap f g x)

-- | Important: does not fold over the 'typeF' component!
instance Bifoldable term => Bifoldable (TypedF term) where
  bifoldMap f g (TypedF _ty x) = {- g ty <> -} bifoldMap f g x

instance Bitraversable term => Bitraversable (TypedF term) where
  bitraverse f g (TypedF t x) = TypedF <$> g t <*> bitraverse f g x

transFS
  :: (Bifunctor term)
  => (forall s t. term s t -> term' s t)
  -> FS term a -> FS term' a
transFS phi = \case
  Pure x -> Pure x
  Free t -> Free (phi (bimap (transFS phi) (transFS phi) t))

untyped :: Bifunctor term => FS (TypedF term) a -> FS term a
untyped = transFS termF

pattern ExtE :: ext (Scope (FS (t :+: ext)) a) (FS (t :+: ext) a) -> FS (t :+: ext) a
pattern ExtE t = Free (InR t)

substitute :: Bifunctor t => FS t a -> Scope (FS t) a -> FS t a
substitute t = (>>= f)
  where
    f Z = t
    f (S y) = Pure y
