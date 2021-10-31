{-# LANGUAGE DeriveFoldable       #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module Rzk.Free.Syntax.FreeScoped where

import           Bound
import qualified Bound.Scope            as Bound
import           Control.Monad          (ap, liftM)
import           Control.Monad.Identity (Identity (..))
import           Data.Bifoldable
import           Data.Bifunctor
import           Data.Bifunctor.TH
import           Data.Bitraversable

-- * Free monad transformer with scoping

data FreeScopedF term scope subterm a
  = PureScopedF a
  | FreeScopedF (term scope subterm)
  deriving (Eq, Show, Functor, Foldable, Traversable)

newtype FreeScopedT b term m a = FreeScopedT
  { runFreeScopedT :: m (FreeScopedF term (Scope b (FreeScopedT b term m) a) (FreeScopedT b term m a) a)
  }

instance (Bifoldable term, Foldable m) => Foldable (FreeScopedT b term m) where
  foldMap f (FreeScopedT m) = foldMap f' m
    where
      f' (PureScopedF x) = f x
      f' (FreeScopedF t) = bifoldMap (foldMap f) (foldMap f) t

instance (Bitraversable term, Traversable m, Monad m)
  => Traversable (FreeScopedT b term m) where
  traverse f (FreeScopedT m) = FreeScopedT <$> traverse f' m
    where
      f' (PureScopedF x) = PureScopedF <$> f x
      f' (FreeScopedF t) = FreeScopedF <$> bitraverse (traverse f) (traverse f) t

instance (Bifunctor term, Monad m) => Functor (FreeScopedT b term m) where
  fmap f (FreeScopedT m) = FreeScopedT (liftM f' m)
    where
      f' (PureScopedF x) = PureScopedF (f x)
      f' (FreeScopedF t) = FreeScopedF (bimap (fmap f) (fmap f) t)

instance (Bifunctor term, Monad m) => Applicative (FreeScopedT b term m) where
  pure = return
  (<*>) = ap

instance (Bifunctor term, Monad m) => Monad (FreeScopedT b term m) where
  return = FreeScopedT . return . PureScopedF
  FreeScopedT m >>= f = FreeScopedT (m >>= f')
    where
      f' (PureScopedF x) = runFreeScopedT (f x)
      f' (FreeScopedF t) = return (FreeScopedF (bimap (>>>= f) (>>= f) t))

hoistFreeScopedT
  :: (Monad m, Bifunctor term)
  => (forall x. m x -> n x) -> FreeScopedT b term m a -> FreeScopedT b term n a
hoistFreeScopedT phi (FreeScopedT m) = FreeScopedT (phi (liftM f' m))
  where
    f' (PureScopedF x) = PureScopedF x
    f' (FreeScopedF t) = FreeScopedF (bimap hoistFreeScopedT' (hoistFreeScopedT phi) t)

    hoistFreeScopedT' = Scope . hoistFreeScopedT phi . fmap (fmap (hoistFreeScopedT phi)) . unscope

transFreeScopedT
  :: (Monad m, Bifunctor term)
  => (forall x y. term x y -> term' x y)
  -> FreeScopedT b term m a -> FreeScopedT b term' m a
transFreeScopedT phi (FreeScopedT m) = FreeScopedT (liftM f' m)
  where
    f' (PureScopedF x) = PureScopedF x
    f' (FreeScopedF t) = FreeScopedF (phi (bimap transFreeScopedT' (transFreeScopedT phi) t))

    transFreeScopedT' = Scope . transFreeScopedT phi . fmap (fmap (transFreeScopedT phi)) . unscope

wrapFreeScopedT
  :: Monad m
  => term (Scope b (FreeScopedT b term m) a) (FreeScopedT b term m a)
  -> FreeScopedT b term m a
wrapFreeScopedT = FreeScopedT . return . FreeScopedF

iterFreeScopedT
  :: (Monad m, Bifunctor term)
  => (forall x. term (Scope b m x) (m x) -> m x)
  -> FreeScopedT b term m a
  -> m a
iterFreeScopedT phi (FreeScopedT m) = m >>= \case
  PureScopedF x -> return x
  FreeScopedF t -> phi (bimap (Bound.hoistScope (iterFreeScopedT phi)) (iterFreeScopedT phi) t)

-- * Free monad with scoping

type FreeScoped b term = FreeScopedT b term Identity

pattern PureScoped :: a -> FreeScoped b term a
pattern PureScoped x = FreeScopedT (Identity (PureScopedF x))

pattern FreeScoped
  :: term (Scope b (FreeScoped b term) a) (FreeScoped b term a)
  -> FreeScoped b term a
pattern FreeScoped t = FreeScopedT (Identity (FreeScopedF t))

iterFreeScopedM
  :: (Monad m, Bifunctor term)
  => (forall x. term (Scope b m x) (m x) -> m x)
  -> FreeScoped b term a
  -> m a
iterFreeScopedM phi = iterFreeScopedT phi . hoistFreeScopedT (return . runIdentity)

{-# COMPLETE PureScoped, FreeScoped #-}

data Sum f g scope term
  = InL (f scope term)
  | InR (g scope term)
  deriving (Functor, Foldable, Traversable)

type (:+:) = Sum

data Const a scope term = Const a
  deriving (Functor, Foldable, Traversable)

-- stratSum
--   :: (Bifunctor f, Bifunctor g)
--   => FreeScoped b (f :+: g) a
--   -> FreeScoped b f (FreeScoped b (f :+: g) a)
-- stratSum = \case
--   PureScoped x -> pure (pure x)
--   FreeScoped (InL f) -> FreeScoped (bimap stratSumScoped stratSum f)
--   fg -> pure fg
--
-- stratSumScoped
--   :: (Bifunctor f, Bifunctor g)
--   => Scope b (FreeScoped b (f :+: g)) a
--   -> Scope b (FreeScoped b f) (FreeScoped b (f :+: g) a)
-- stratSumScoped = toScope . _ . stratSum . fromScope

-- viewL
--   :: (Monad m, Bifunctor f)
--   => FreeScopedT b (f :+: g) m a
--   -> FreeScopedT b f m (FreeScopedT b (f :+: g) m a)
-- viewL t@(FreeScopedT m) = FreeScopedT $ m <&> \case
--   FreeScopedF (InL f) -> FreeScopedF $
--     bimap (Bound.toScope . error "impossible" . viewL . Bound.fromScope) viewL f
--   _ -> PureScopedF t

deriveBifunctor ''Sum
deriveBifoldable ''Sum
deriveBitraversable ''Sum

deriveBifunctor ''Const
deriveBifoldable ''Const
deriveBitraversable ''Const
