{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Rzk.Simple.Evaluator where

import           Bound.Scope
import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.Reader   (ReaderT, asks, runReaderT)
import           Control.Monad.State
import           Data.Hashable          (Hashable)
import           Data.HashMap.Strict    (HashMap)
import qualified Data.HashMap.Strict    as HashMap
import           Rzk.Simple.Syntax.Term
import           Rzk.Simple.Syntax.Var

import           Text.Trifecta          (Span, Spanned (..))

class Monad m => MonadLookup k v m where
  lookupM :: k -> m (Maybe v)

instance MonadLookup k v Identity where
  lookupM = const (return Nothing)

newtype ForgetLookupT m a = ForgetLookupT { runForgetLookupT :: m a }
  deriving (Functor, Applicative, Monad)

instance Monad m => MonadLookup k v (ForgetLookupT m) where
  lookupM = const (return Nothing)

newtype HashMapLookupT k v m a
  = HashMapLookupT { runHashMapLookupT :: ReaderT (HashMap k v) m a }
  deriving (Functor, Applicative, Monad)

instance (Eq k, Hashable k, Monad m) => MonadLookup k v (HashMapLookupT k v m) where
  lookupM = HashMapLookupT . asks . HashMap.lookup

data Thunk a
  = Unevaluated a
  | Evaluating
  | Evaluated a

newtype WhnfHashMapLookupT ann var a m b
  = WhnfHashMapLookupT { runWhnfHashMapLookupT :: StateT (HashMap a (Thunk (Term ann var a))) m b }
  deriving (Functor, Applicative, Monad)

instance (Eq a, Hashable a, Monad m) => MonadLookup a (Term ann var a) (WhnfHashMapLookupT ann var a m) where
  lookupM x = WhnfHashMapLookupT $ do
    gets (HashMap.lookup x) >>= \case
      Nothing -> return Nothing
      Just (Unevaluated t) -> do
        modify (HashMap.insert x Evaluating)
        t' <- runWhnfHashMapLookupT (whnfM t)
        modify (HashMap.insert x (Evaluated t'))
        return (Just t')
      Just (Evaluated t) -> return (Just t)
      Just Evaluating -> error "unable to evaluate self-referencing value"

data UndefinedError k = UndefinedError k
  deriving (Show)

newtype EnsureDefinedT k m a = EnsureDefinedT { runEnsureDefinedT :: ExceptT (UndefinedError k) m a }
  deriving (Functor, Applicative, Monad)

instance (MonadLookup k v m) => MonadLookup k v (EnsureDefinedT k m) where
  lookupM x = EnsureDefinedT $ do
    lift (lookupM x) >>= \case
      Nothing -> throwError (UndefinedError x)
      Just t -> return (Just t)

nfM
  :: MonadLookup a (Term ann var a) m
  => Term ann var a -> m (Term ann var a)
nfM = nf'
  where
    nfScoped
      :: MonadLookup a (Term ann var a) m
      => Scope b (Term ann var) a -> m (Scope b (Term ann var) a)
    nfScoped = fmap Scope . nfScoped' . unscope
      where
        nfScoped' f = runForgetLookupT (nfM f) >>= traverse (traverse nfM)

    nf' :: MonadLookup a (Term ann var a) m => Term ann var a -> m (Term ann var a)
    nf' = \case
      t@(Variable x) -> lookupM x >>= \case
        Nothing -> return t
        Just t' -> nf' t'
      Annotated ann t -> Annotated ann <$> nf' t
      App t1 t2   -> whnfM t1 >>= \case
        Lambda _a _phi body -> nf' (instantiate1 t2 body)
        t1'                 -> App <$> nf' t1' <*> nf' t2

      First t -> whnfM t >>= \case
        Pair t1 _t2 -> nf' t1
        t'          -> nf' t'
      Second t -> whnfM t >>= \case
        Pair _t1 t2 -> nf' t2
        t'          -> nf' t'

      IdJ tA a tC d x p -> whnfM p >>= \case
        Refl{} -> nf' d
        p'     -> IdJ <$> nf' tA <*> nf' a <*> nf' tC <*> nf' d <*> nf' x <*> nf' p'

      e@Universe -> return e

      ExtensionType tC psi tA phi a -> ExtensionType
        <$> nf' tC
        <*> nfScoped psi
        <*> nfScoped tA
        <*> nfScoped phi
        <*> nfScoped a

      Pi t'       -> Pi <$> nf' t'
      Lambda a phi body -> Lambda
        <$> traverse nf' a
        <*> traverse nfScoped phi
        <*> nfScoped body

      Sigma t'    -> Sigma <$> nf' t'
      Pair t1 t2  -> Pair <$> nf' t1 <*> nf' t2

      IdType a x y -> IdType <$> nf' a <*> nf' x <*> nf' y
      Refl a x -> Refl <$> traverse nf' a <*> nf' x

      Cube -> return Cube
      CubeUnit -> return CubeUnit
      CubeUnitStar -> return CubeUnitStar

      CubeProd t1 t2 -> CubeProd <$> nf' t1 <*> nf' t2

      Tope -> return Tope
      TopeTop -> return TopeTop
      TopeBottom -> return TopeBottom
      TopeOr  t1 t2 -> TopeOr  <$> nf' t1 <*> nf' t2
      TopeAnd t1 t2 -> TopeAnd <$> nf' t1 <*> nf' t2
      TopeEQ  t1 t2 -> TopeEQ  <$> nf' t1 <*> nf' t2

      RecBottom -> return RecBottom
      RecOr psi phi t1 t2 -> RecOr <$> nf' psi <*> nf' phi <*> nf' t1 <*> nf' t2

      Cube2 -> return Cube2
      Cube2_0 -> return Cube2_0
      Cube2_1 -> return Cube2_1

      TopeLEQ t1 t2 -> TopeLEQ <$> nf' t1 <*> nf' t2

whnfM
  :: MonadLookup a (Term ann var a) m
  => Term ann var a -> m (Term ann var a)
whnfM = \case
  t@(Variable x) -> lookupM x >>= \case
    Nothing -> return t
    Just t' -> whnfM t'
  Annotated ann t -> Annotated ann <$> whnfM t
  App t1 t2   -> whnfM t1 >>= \case
    Lambda _a _phi body -> whnfM (instantiate1 t2 body)
    t1'                 -> return (App t1' t2)

  First t -> whnfM t >>= \case
    Pair t1 _t2 -> whnfM t1
    t'          -> return t'
  Second t -> whnfM t >>= \case
    Pair _t1 t2 -> whnfM t2
    t'          -> return t'

  IdJ tA a tC d x p -> whnfM p >>= \case
    Refl{} -> whnfM d
    p'     -> return (IdJ tA a tC d x p')

  t@Universe -> return t
  t@Lambda{} -> return t
  t@ExtensionType{} -> return t
  t@Pi{} -> return t
  t@Sigma{} -> return t
  t@Pair{} -> return t

  t@IdType{} -> return t
  t@Refl{} -> return t

  t@Cube -> return t
  t@CubeUnit -> return t
  t@CubeUnitStar -> return t
  t@CubeProd{} -> return t

  t@Tope -> return t
  t@TopeTop -> return t
  t@TopeBottom -> return t

  t@TopeOr{} -> return t
  t@TopeAnd{} -> return t
  t@TopeEQ{} -> return t

  t@RecBottom -> return t
  t@RecOr{} -> return t

  t@Cube2 -> return t
  t@Cube2_0 -> return t
  t@Cube2_1 -> return t
  t@TopeLEQ{} -> return t

nf :: Term ann var a -> Term ann var a
nf = runIdentity . nfM

whnf :: Term ann var a -> Term ann var a
whnf = runIdentity . whnfM

nfWith :: (Eq a, Hashable a) => [(a, Term ann var a)] -> Term ann var a -> Term ann var a
nfWith definitions = runIdentity . flip runReaderT defs . runHashMapLookupT . nfM
  where
    defs = HashMap.fromList definitions

whnfWith :: (Eq a, Hashable a) => [(a, Term ann var a)] -> Term ann var a -> Term ann var a
whnfWith definitions = runIdentity . flip runReaderT defs . runHashMapLookupT . whnfM
  where
    defs = HashMap.fromList definitions

nfWith' :: (Eq a, Hashable a) => [(a, Term ann var a)] -> Term ann var a -> Term ann var a
nfWith' definitions = runIdentity . flip evalStateT defs . runWhnfHashMapLookupT . nfM
  where
    defs = Unevaluated <$> HashMap.fromList definitions

whnfWith' :: (Eq a, Hashable a) => [(a, Term ann var a)] -> Term ann var a -> Either (UndefinedError a) (Term ann var a)
whnfWith' definitions = runIdentity . flip evalStateT defs . runWhnfHashMapLookupT . runExceptT . runEnsureDefinedT . whnfM
  where
    defs = Unevaluated <$> HashMap.fromList definitions
