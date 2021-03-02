{-# LANGUAGE LambdaCase #-}
module Rzk.Free.Eval where

import           Bound

import           Rzk.Free.Bound.Name
import           Rzk.Free.Syntax.Term

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

whnfT :: (a -> TypedTerm b a) -> TypedTerm b a -> TypedTerm b a
whnfT typeOfFreeVar = \case
  AppT t t1 t2 ->
    case whnfT typeOfFreeVar t1 of
      _ | PiT _ _ b <- whnfT typeOfFreeVar (typeOf typeOfFreeVar t1), UnitTypeT _ <- fromScope b
        -> UnitT (UnitTypeT universeT)
      LambdaT _ body -> instantiate1 t2 body
      t1'            -> AppT t t1' t2
  IdJT t tA a tC d x p ->
    case whnfT typeOfFreeVar p of
      ReflT{} -> whnfT typeOfFreeVar d
      p'      -> IdJT t tA a tC d x p'
  t@VariableT{} -> t
  t@LambdaT{} -> t
  t@UniverseT{} -> t
  t@PiT{} -> t
  t@UnitT{} -> t
  t@UnitTypeT{} -> t
  t@IdTypeT{} -> t
  t@ReflT{} -> t

whnfTClosed :: TypedTerm b a -> TypedTerm b a
whnfTClosed = whnfT (error "a free variable in a closed term!")

nfT :: (a -> TypedTerm b a) -> TypedTerm b a -> TypedTerm b a
nfT typeOfFreeVar = nfT'
  where
    nfT' = \case
      AppT t t1 t2 ->
        case whnfT typeOfFreeVar t1 of
          _ | PiT _ _ b <- whnfT typeOfFreeVar (typeOf typeOfFreeVar t1), UnitTypeT _ <- fromScope b
            -> UnitT (UnitTypeT universeT)
          LambdaT _ body -> nfT' (instantiate1 t2 body)
          t1'            -> AppT (nfT' t) (nfT' t1') (nfT' t2)
      IdJT t tA a tC d x p ->
        case whnfT typeOfFreeVar p of
          ReflT{} -> nfT' d
          p'     -> IdJT (nfT' t) (nfT' tA) (nfT' a) (nfT' tC) (nfT' d) (nfT' x) (nfT' p')
      LambdaT t@(PiT _ a _) body -> LambdaT (nfT' t) (nfScopeT a body)
      LambdaT _ _ -> error "impossible type of Lambda"
      PiT _ a b      -> PiT universeT (nfT' a) (nfScopeT a b)
      IdTypeT _ t x y -> IdTypeT universeT (nfT' t) (nfT' x) (nfT' y)
      ReflT _ t x ->
        let t' = nfT' t
            x' = nfT' x
         in ReflT (IdTypeT universeT t' x' x') t' x'
      t@VariableT{} -> t
      t@UniverseT{} -> t
      t@UnitT{} -> t
      t@UnitTypeT{} -> t
      where
        nfScopeT typeOfBoundVar = toScope . nfT typeOfVar . fromScope
          where
            typeOfVar (B (Name _ ())) = F <$> typeOfBoundVar
            typeOfVar (F x)           = F <$> typeOfFreeVar x

nfTClosed :: TypedTerm b a -> TypedTerm b a
nfTClosed = nfT (error "a free variable in a closed term!")
