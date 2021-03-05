{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Rzk.Free.TypeCheck where

import           Bound
import           Control.Monad.Except
import           Data.String                  (IsString)
import           Data.Text.Prettyprint.Doc    (Pretty)

import           Rzk.Free.Bound.Name
import           Rzk.Free.Eval
import           Rzk.Free.Syntax.Term
import           Rzk.Free.TypeCheck.Trans
import           Rzk.Free.TypeCheck.TypeError

-- * Typecheck

typecheckScope
  :: Eq a
  => TypedTerm b a
  -> Scope1Term b a
  -> Scope1TypedTerm b a
  -> TypeCheck b a (Scope1TypedTerm b a)
typecheckScope typeOfBoundVar term expectedType =
  enterScope typeOfBoundVar $ do
    toScope <$> typecheck (fromScope term) (fromScope expectedType)

inferScoped
  :: Eq a
  => TypedTerm b a
  -> Scope1Term b a
  -> TypeCheck b a (Scope1TypedTerm b a)
inferScoped typeOfBoundVar term =
  enterScope typeOfBoundVar $ do
    toScope <$> infer (fromScope term)

infer :: Eq a => Term b a -> TypeCheck b a (TypedTerm b a)
infer = \case
  Universe -> return universeT
  Pi a b -> do
    a' <- typecheck a universeT
    b' <- typecheckScope a' b (toScope universeT)
    return (PiT universeT a' b')

  App (Lambda body) arg -> do
    arg' <- infer arg
    typeOfArg <- typeOf arg'
    body' <- inferScoped typeOfArg body
    typeOfBody <- typeOfScoped arg' body'
    let typeOfResult = instantiate1 arg' typeOfBody
    return (AppT typeOfResult (LambdaT (PiT universeT typeOfArg typeOfBody) body') arg')

  App t1 t2 -> do
    infer t1 >>= \case
      t1'@(Typed (PiT _ a b) _) -> do
        t2' <- typecheck t2 a
        return (AppT (instantiate1 t2' b) t1' t2')
      t1'@(VariableT x) -> do
        typeOfFreeVar x >>= \case
          PiT _ a b -> do
            t2' <- typecheck t2 a
            return (AppT (instantiate1 t2' b) t1' t2')
          _ -> throwError (TypeErrorNotAFunction t1')
      t1' -> throwError (TypeErrorNotAFunction t1')

  t@Lambda{} -> throwError (TypeErrorCannotInferLambda t)
  Unit -> return (UnitT (UnitTypeT universeT))
  UnitType -> return (UnitTypeT universeT)
  Variable x -> return (VariableT x)
  Refl t x -> do
    t' <- typecheck t universeT
    x' <- typecheck x t'
    return (ReflT (IdTypeT universeT t' x' x') t' x')
  IdType t x y -> do
    t' <- typecheck t universeT
    x' <- typecheck x t'
    y' <- typecheck y t'
    return (IdTypeT universeT t' x' y')

  IdJ tA a tC d x p -> do
    tA' <- typecheck tA universeT
    a'  <- typecheck a tA'
    tC' <- typecheck tC
                (piT tA' (abstract1Unnamed
                  (piT (idTypeT (Just <$> tA') (Just <$> a') (VariableT Nothing)) (toScope universeT))))
    let tC'a' = piT (idTypeT tA' a' a') (toScope universeT)
    d'  <- typecheck d
            (AppT universeT (AppT tC'a' tC' a') (ReflT (idTypeT tA' a' a') tA' a'))
    x' <- typecheck x tA'
    p' <- typecheck p (idTypeT tA' a' x')
    let tC'x' = piT (idTypeT tA' a' x') (toScope universeT)
    return (IdJT (AppT universeT (AppT tC'x' tC' x') p')
              tA' a' tC' d' x' p')

typecheck :: Eq a => Term b a -> TypedTerm b a -> TypeCheck b a (TypedTerm b a)
typecheck term expectedType = case (term, expectedType) of
  (Lambda body, PiT _ a b) ->
     LambdaT expectedType <$> typecheckScope a body b
  (Variable x, _) -> return (VariableT x)
  _ -> infer term >>= \case
         Typed ty x          -> do
           tt <- unify ty expectedType
           return (Typed tt x)
         term'@(VariableT x) -> do
           tx <- typeOfFreeVar x
           _tt <- unify tx expectedType
           return term'

unifyScoped
  :: Eq a
  => TypedTerm b a -> Scope1TypedTerm b a -> Scope1TypedTerm b a -> TypeCheck b a (Scope1TypedTerm b a)
unifyScoped typeOfBoundVar l r = do
  enterScope typeOfBoundVar $ do
    toScope <$> unify (fromScope l) (fromScope r)

unify :: Eq a => TypedTerm b a -> TypedTerm b a -> TypeCheck b a (TypedTerm b a)
unify = go
  where
    go l r = do
      l' <- whnfT l
      r' <- whnfT r
      go' l' r'

    go' (UniverseT _) (UniverseT _) = return universeT
    go' (PiT _ a b) (PiT _ c d)     = do
      ac <- go a c
      bd <- unifyScoped ac b d
      return (PiT universeT ac bd)
    go' (AppT t a b) (AppT t' c d) =
      AppT <$> go t t' <*> go a c <*> go b d
    go' (LambdaT (PiT _ a b) x) (LambdaT (PiT _ c d) y) = do
      ac <- go a c
      bd <- unifyScoped ac b d
      xy <- unifyScoped ac x y
      return (LambdaT (PiT universeT ac bd) xy)
    go' inferred@(VariableT x) expected@(VariableT y)
      | x == y = return (VariableT x)
      | otherwise = throwError (TypeErrorUnexpected inferred expected)
    go' (UnitTypeT _) (UnitTypeT _) = return (UnitTypeT universeT)
    go' (UnitT _) (UnitT _) = return (UnitT (UnitTypeT universeT))
    go' (IdTypeT _ t1 x1 y1) (IdTypeT _ t2 x2 y2) = do
      t <- go t1 t2
      x <- go x1 x2
      y <- go y1 y2
      return (IdTypeT universeT t x y)
    go' (ReflT _ t1 x1) (ReflT _ t2 x2) = do
      t <- go t1 t2
      x <- go x1 x2
      return (ReflT (IdTypeT universeT t x x) t x)
    go' (IdJT tt1 tA1 a1 tC1 d1 x1 p1) (IdJT tt2 tA2 a2 tC2 d2 x2 p2) = do
      tt <- go tt1 tt2
      tA <- go tA1 tA2
      a <- go a1 a2
      tC <- go tC1 tC2
      d <- go d1 d2
      x <- go x1 x2
      p <- go p1 p2
      return (IdJT tt tA a tC d x p)
    go' inferred expected = throwError (TypeErrorUnexpected inferred expected)

unsafeTypecheckClosed :: (Eq a, IsString a, Pretty a, Pretty b) => Term b a -> TypedTerm b a -> TypedTerm b a
unsafeTypecheckClosed term = unsafeRunTypeCheck . typecheck term

unsafeInferClosed :: (Eq a, IsString a, Pretty a, Pretty b) => Term b a -> TypedTerm b a
unsafeInferClosed = unsafeRunTypeCheck . infer
