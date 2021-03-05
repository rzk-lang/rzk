{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
module Rzk.Free.TypeCheck.Trans where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Functor.Identity
import           Data.String                  (IsString)
import           Data.Text.Prettyprint.Doc    (Pretty)

import           Bound                        (Var (..))
import           Bound.Scope

import           Rzk.Free.Bound.Name
import           Rzk.Free.Syntax.Term
import           Rzk.Free.TypeCheck.Context
import           Rzk.Free.TypeCheck.TypeError

newtype TypeCheckT b a m x = TypeCheckT
  { runTypeCheckT :: ReaderT (Context b a) (ExceptT (TypeError b a) m) x }
  deriving (Functor, Applicative, Monad, MonadError (TypeError b a), MonadReader (Context b a))

type TypeCheck b a = TypeCheckT b a Identity

enterScope
  :: Functor m
  => TypedTerm b a
  -> TypeCheckT b (Var (Name b ()) a) m x
  -> TypeCheckT b a m x
enterScope typeOfBoundVar m = TypeCheckT $ ReaderT $ \context ->
  withExceptT TypeErrorScope $
    runReaderT (runTypeCheckT m) (withBound typeOfBoundVar context)

typeOfFreeVar :: Monad m => a -> TypeCheckT b a m (TypedTerm b a)
typeOfFreeVar x = do
  f <- asks contextKnownTypes
  case f x of
    Nothing -> error "unknown type for variable"
    Just t  -> return t

typeOf :: Monad m => TypedTerm b a -> TypeCheckT b a m (TypedTerm b a)
typeOf (Typed t _)   = return t
typeOf (VariableT x) = typeOfFreeVar x

typeOfScoped
  :: (Eq a, Monad m)
  => TypedTerm b a
  -> Scope1TypedTerm b a
  -> TypeCheckT b a m (Scope1TypedTerm b a)
typeOfScoped typeOfBoundVar term =
  enterScope typeOfBoundVar $ do
    toScope <$> typeOf (fromScope term)

unsafeRunTypeCheckT :: (Functor m, IsString a, Pretty a, Pretty b) => TypeCheckT b a m x -> m x
unsafeRunTypeCheckT
  = fmap fromRight
  . runExceptT
  . flip runReaderT emptyContext
  . runTypeCheckT
  where
    fromRight (Right x)  = x
    fromRight (Left err) = error (show err)

unsafeRunTypeCheck :: (IsString a, Pretty a, Pretty b) => TypeCheck b a x -> x
unsafeRunTypeCheck = runIdentity . unsafeRunTypeCheckT
