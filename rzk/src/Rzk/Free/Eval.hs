{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
module Rzk.Free.Eval where

import           Bound
import           Data.String               (IsString)
import           Data.Text.Prettyprint.Doc (Pretty)

import           Rzk.Free.Syntax.Term
import           Rzk.Free.TypeCheck.Trans

-- * Evaluation

whnf :: Term b a -> Term b a
whnf = \case
  App t1 t2 ->
    case whnf t1 of
      Lambda body -> instantiate1 t2 body
      t1'         -> App t1' t2
  IdJ tA a tC d x p ->
    case whnf p of
      Refl{} -> whnf d
      p'     -> IdJ tA a tC d x p'
  t@Variable{} -> t
  t@Lambda{} -> t
  t@Universe{} -> t
  t@Pi{} -> t
  t@Unit{} -> t
  t@UnitType{} -> t
  t@IdType{} -> t
  t@Refl{} -> t

nf :: Term b a -> Term b a
nf = \case
  App t1 t2 ->
    case whnf t1 of
      Lambda body -> nf (instantiate1 t2 body)
      t1'         -> App (nf t1') (nf t2)
  IdJ tA a tC d x p ->
    case whnf p of
      Refl{} -> nf d
      p'     -> IdJ (nf tA) (nf a) (nf tC) (nf d) (nf x) (nf p')
  Lambda body -> Lambda (nfScope body)
  Pi a b -> Pi (nf a) (nfScope b)
  IdType t x y -> IdType (nf t) (nf x) (nf y)
  Refl t x -> Refl (nf t) (nf x)
  t@Variable{} -> t
  t@Universe{} -> t
  t@Unit{} -> t
  t@UnitType{} -> t
  where
    nfScope = toScope . nf . fromScope

whnfT :: Monad m => TypedTerm b a -> TypeCheckT b a m (TypedTerm b a)
whnfT = \case
  AppT t t1 t2 -> do
    typeOf_t1 <- typeOf t1 >>= whnfT
    whnfT t1 >>= \case
      _ | PiT _ _ b <- typeOf_t1, UnitTypeT _ <- fromScope b
        -> return (UnitT (UnitTypeT universeT))
      LambdaT _ body -> return (instantiate1 t2 body)
      t1'            -> return (AppT t t1' t2)
  IdJT t tA a tC d x p ->
    whnfT p >>= \case
      ReflT{} -> whnfT d
      p'      -> return (IdJT t tA a tC d x p')
  t@VariableT{} -> return t
  t@LambdaT{} -> return t
  t@UniverseT{} -> return t
  t@PiT{} -> return t
  t@UnitT{} -> return t
  t@UnitTypeT{} -> return t
  t@IdTypeT{} -> return t
  t@ReflT{} -> return t

nfT :: Monad m => TypedTerm b a -> TypeCheckT b a m (TypedTerm b a)
nfT = nfT'
  where
    nfT' = \case
      AppT t t1 t2 -> do
        typeOf_t1 <- typeOf t1 >>= whnfT
        whnfT t1 >>= \case
          _ | PiT _ _ b <- typeOf_t1, UnitTypeT _ <- fromScope b
            -> return (UnitT (UnitTypeT universeT))
          LambdaT _ body -> nfT' (instantiate1 t2 body)
          t1'            -> AppT <$> nfT' t <*> nfT' t1' <*> nfT' t2
      IdJT t tA a tC d x p ->
        whnfT p >>= \case
          ReflT{} -> nfT' d
          p'     -> IdJT <$> nfT' t <*> nfT' tA <*> nfT' a <*> nfT' tC <*> nfT' d <*> nfT' x <*> nfT' p'
      LambdaT t@(PiT _ a _) body -> LambdaT <$> nfT' t <*> nfScopeT a body
      LambdaT _ _ -> error "impossible type of Lambda"
      PiT _ a b      -> PiT universeT <$> nfT' a <*> nfScopeT a b
      IdTypeT _ t x y -> IdTypeT universeT <$> nfT' t <*> nfT' x <*> nfT' y
      ReflT _ t x -> do
        t' <- nfT' t
        x' <- nfT' x
        return (ReflT (IdTypeT universeT t' x' x') t' x')
      t@VariableT{} -> return t
      t@UniverseT{} -> return t
      t@UnitT{} -> return t
      t@UnitTypeT{} -> return t
      where
        nfScopeT typeOfBoundVar term = do
          enterScope typeOfBoundVar $ do
            toScope <$> nfT (fromScope term)

unsafeWHNF :: (IsString a, Pretty a, Pretty b) => TypedTerm b a -> TypedTerm b a
unsafeWHNF = unsafeRunTypeCheck . whnfT

unsafeNF :: (IsString a, Pretty a, Pretty b) => TypedTerm b a -> TypedTerm b a
unsafeNF = unsafeRunTypeCheck . nfT
