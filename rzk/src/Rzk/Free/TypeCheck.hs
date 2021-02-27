{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Rzk.Free.TypeCheck where

import           Bound
import           Bound.Name
import           Data.Text            (Text)
import qualified Data.Text            as Text

import           Rzk.Free.Eval
import           Rzk.Free.Pretty
import           Rzk.Free.Syntax.Term
import           Unsafe.Coerce        (unsafeCoerce)

-- * Typecheck

typecheckScope
  :: Eq a
  => TypedTerm b a -> (a -> TypedTerm b a) -> Scope1Term b a -> Scope1TypedTerm b a -> Scope1TypedTerm b a
typecheckScope typeOfBoundVar typeOfFreeVar term expectedType
  = toScope (typecheck typeOfVar (fromScope term) (fromScope expectedType))
    where
      typeOfVar (B (Name _ ())) = F <$> typeOfBoundVar
      typeOfVar (F x)           = F <$> typeOfFreeVar x

inferScoped :: Eq a => TypedTerm b a -> (a -> TypedTerm b a) -> Scope1Term b a -> Scope1TypedTerm b a
inferScoped typeOfBoundVar typeOfFreeVar = toScope . infer typeOfVar . fromScope
    where
      typeOfVar (B (Name _ ())) = F <$> typeOfBoundVar
      typeOfVar (F x)           = F <$> typeOfFreeVar x

infer :: Eq a => (a -> TypedTerm b a) -> Term b a -> TypedTerm b a
infer typeOfFreeVar = \case
  Universe -> universeT
  Pi a b ->
    let a' = typecheck typeOfFreeVar a universeT
        b' = typecheckScope a' typeOfFreeVar b (toScope universeT)
     in PiT universeT a' b'
  App (Lambda body) arg ->
    case infer typeOfFreeVar arg of
      arg' ->
        let typeOfArg = typeOf typeOfFreeVar arg'
            body' = inferScoped typeOfArg typeOfFreeVar body
            typeOfBody = typeOfScoped arg' typeOfFreeVar body'
            typeOfResult = instantiate1 arg' typeOfBody
        in AppT typeOfResult (LambdaT (PiT universeT typeOfArg typeOfBody) body') arg'
  App t1 t2 ->
    case infer typeOfFreeVar t1 of
      t1'@(Typed (PiT _ a b) _) ->
        let t2' = typecheck typeOfFreeVar t2 a
         in AppT (instantiate1 t2' b) t1' t2'
      t1'@(VariableT x) ->
        case typeOfFreeVar x of
          PiT _ a b ->
            let t2' = typecheck typeOfFreeVar t2 a
             in AppT (instantiate1 t2' b) t1' t2'
          _ -> error "not a function!"
      _ -> error "not a function!"
  term@(Lambda _body) -> errorText $ "can't infer Lambda: " <> ppTerm ["x", "y", "z"] (unsafeCoerce term)
  Unit -> UnitT (UnitTypeT universeT)
  UnitType -> UnitTypeT universeT
  Variable x -> VariableT x
  Refl t x ->
    let t' = typecheck typeOfFreeVar t universeT
        x' = typecheck typeOfFreeVar x t'
     in ReflT (IdTypeT universeT t' x' x') t' x'
  IdType t x y ->
    let t' = typecheck typeOfFreeVar t universeT
        x' = typecheck typeOfFreeVar x t'
        y' = typecheck typeOfFreeVar y t'
     in IdTypeT universeT t' x' y'

typecheck :: Eq a => (a -> TypedTerm b a) -> Term b a -> TypedTerm b a -> TypedTerm b a
typecheck typeOfFreeVar term expectedType = case (term, expectedType) of
  (Lambda body, PiT _ a b) ->
     LambdaT expectedType (typecheckScope a typeOfFreeVar body b)
  (Lambda _, _) -> error "lambda is expected to be a non-function type?!"
  (Variable x, _) -> VariableT x
  _ ->
    case infer typeOfFreeVar term of
      Typed ty x          -> (unify typeOfFreeVar ty expectedType) `seq` Typed (unify typeOfFreeVar ty expectedType) x
      term'@(VariableT x) -> unify typeOfFreeVar (typeOfFreeVar x) expectedType `seq` term'

unifyScoped
  :: Eq a
  => TypedTerm b a -> (a -> TypedTerm b a) -> Scope1TypedTerm b a -> Scope1TypedTerm b a -> Scope1TypedTerm b a
unifyScoped typeOfBoundVar typeOfFreeVar l r
  = toScope (unify typeOfVar (fromScope l) (fromScope r))
    where
      typeOfVar (B (Name _ ())) = F <$> typeOfBoundVar
      typeOfVar (F x)           = F <$> typeOfFreeVar x

unify :: Eq a => (a -> TypedTerm b a) -> TypedTerm b a -> TypedTerm b a -> TypedTerm b a
unify typeOfFreeVar = go
  where
    go l r = go' (whnfT typeOfFreeVar l) (whnfT typeOfFreeVar r)

    go' (UniverseT _) (UniverseT _) = universeT
    go' (PiT _ a b) (PiT _ c d)     =
      let ac = go a c
       in PiT universeT ac (unifyScoped ac typeOfFreeVar b d)
    go' (AppT t a b) (AppT t' c d)  = AppT (go t t') (go a c) (go b d)
    go' (LambdaT (PiT _ a b) x) (LambdaT (PiT _ c d) y) =
      let ac = go a c
       in LambdaT (PiT universeT ac (unifyScoped ac typeOfFreeVar b d)) (unifyScoped ac typeOfFreeVar x y)
    go' (VariableT x) (VariableT y)
      | x == y = VariableT x
      | otherwise = error "can't unify different variables"
    go' (UnitTypeT _) (UnitTypeT _) = UnitTypeT universeT
    go' (UnitT _) (UnitT _) = UnitT (UnitTypeT universeT)
    go' (IdTypeT _ t1 x1 y1) (IdTypeT _ t2 x2 y2) =
      let t = go t1 t2
          x = go x1 x2
          y = go y1 y2
       in t `seq` x `seq` y `seq` IdTypeT universeT t x y   -- FIXME: instead of seq, use typechecking monad
    go' (ReflT _ t1 x1) (ReflT _ t2 x2) =
      let t = go t1 t2
          x = go x1 x2
       in ReflT (IdTypeT universeT t x x) t x
    go' l r = errorText ("can't unify terms:\n" <> ppTypedTerm ["x", "y", "z"] (unsafeCoerce l) <> "\nand\n" <> ppTypedTerm ["x", "y", "z"] (unsafeCoerce r))

typecheckClosed :: Eq a => Term b a -> TypedTerm b a -> TypedTerm b a
typecheckClosed = typecheck (error "expected closed term, but free vars found!")

inferClosed :: Eq a => Term b a -> TypedTerm b a
inferClosed = infer (error "expected closed term, but free vars found!")

errorText :: Text -> a
errorText = error . Text.unpack
