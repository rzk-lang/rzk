{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module Free.Scoped where

import           Control.Monad      (ap)
import           Data.Bifoldable
import           Data.Bifunctor
import           Data.Bifunctor.TH
import           Data.Bitraversable
import           Data.Function      (on)
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

data AnnF ann term scope typedTerm = AnnF
  { annF  :: ann typedTerm
  , termF :: term scope typedTerm
  } deriving (Show, Functor)

-- | Important: does not compare the `annF` component!
instance Eq (term scope typedTerm) => Eq (AnnF ann term scope typedTerm) where
  (==) = (==) `on` termF

instance (Functor ann, Bifunctor term) => Bifunctor (AnnF ann term) where
  bimap f g (AnnF t x) = AnnF (fmap g t) (bimap f g x)

-- | Important: does not fold over the 'annF' component!
instance Bifoldable term => Bifoldable (AnnF ann term) where
  bifoldMap f g (AnnF _ty x) = {- g ty <> -} bifoldMap f g x

instance (Traversable ann, Bitraversable term) => Bitraversable (AnnF ann term) where
  bitraverse f g (AnnF t x) = AnnF <$> traverse g t <*> bitraverse f g x

transFS
  :: (Bifunctor term)
  => (forall s t. term s t -> term' s t)
  -> FS term a -> FS term' a
transFS phi = \case
  Pure x -> Pure x
  Free t -> Free (phi (bimap (transFS phi) (transFS phi) t))

untyped :: (Functor ann, Bifunctor term) => FS (AnnF ann term) a -> FS term a
untyped = transFS termF

pattern ExtE :: ext (Scope (FS (t :+: ext)) a) (FS (t :+: ext) a) -> FS (t :+: ext) a
pattern ExtE t = Free (InR t)

substitute :: Bifunctor t => FS t a -> Scope (FS t) a -> FS t a
substitute t = (>>= f)
  where
    f Z     = t
    f (S y) = Pure y
