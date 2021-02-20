{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Rzk.Simple.Evaluator where

import qualified Data.MemoTrie          as MemoTrie

import           Bound
import           Control.Monad.Trans    (lift)
import           Data.Hashable          (Hashable)
import qualified Data.HashMap.Strict    as HashMap
import           Rzk.Simple.Syntax.Term

data EvalContext ann var a b = EvalContext
  { definitionOf :: a -> Either b (Term ann var a)
  }

emptyEvalContext :: EvalContext ann var a a
emptyEvalContext = EvalContext
  { definitionOf = Left
  }

mkEvalContext
  :: (Eq a, Hashable a)
  => (a -> b)
  -> [(a, Term ann var a)]
  -> EvalContext ann var a b
mkEvalContext convert dict = EvalContext
  { definitionOf = \x ->
      case HashMap.lookup x definitions of
        Nothing -> Left (convert x)
        Just t  -> Right t
  }
  where
    definitions = HashMap.fromList dict

nfWith :: forall ann var a. EvalContext ann var a a -> Term ann var a -> Term ann var a
nfWith context@EvalContext{..} = nf'
  where
    whnf' = whnfWith context

    -- definitionOf'
    --   :: Var x (Term ann var a)
    --   -> Either (Var x (Term ann var a)) (Term ann var (Var x (Term ann var a)))
    definitionOf' = \case
      B b -> Left (B b)
      F (Variable y) ->
        case definitionOf y of
          Left z  -> Left (F (pure z))
          Right t -> Right (pure (F t))
      F t -> Right (F . pure <$> t)

    -- nfScoped :: Scope x (Term ann var) a -> Scope x (Term ann var) b
    nfScoped = Scope . nfWith context' . unscope
      where
        -- context' :: EvalContext ann var (Var x (Term ann var a)) (Var x (Term ann var b))
        context' = context { definitionOf = definitionOf' }

    nf' = \case
      t@(Variable x) ->
        case definitionOf x of
          Left  y  -> Variable y
          Right t' -> nf' t'
      Annotated ann t -> Annotated ann (nf' t)
      App t1 t2   ->
        case whnf' t1 of
          Lambda _a _phi body -> nf' (instantiate1 t2 body)
          t1'                 -> App (nf' t1') (nf' t2)

      First t ->
        case whnf' t of
          Pair t1 _t2 -> nf' t1
          t'          -> nf' t'
      Second t ->
        case whnf' t of
          Pair _t1 t2 -> nf' t2
          t'          -> nf' t'

      IdJ tA a tC d x p ->
        case whnf' p of
          Refl{} -> nf' d
          p'     -> IdJ (nf' tA) (nf' a) (nf' tC) (nf' d) (nf' x) (nf' p')

      e@Universe -> e

      ExtensionType tC psi tA phi a ->
        ExtensionType (nf' tC) (nfScoped psi) (nfScoped tA) (nfScoped phi) (nfScoped a)
      Pi t'       -> Pi (nf' t')
      Lambda a phi body
        -> Lambda (fmap nf' a) (fmap nfScoped phi) (nfScoped body)

      Sigma t'    -> Sigma (nf' t')
      Pair t1 t2  -> Pair (nf' t1) (nf' t2)

      IdType a x y -> IdType (nf' a) (nf' x) (nf' y)
      Refl a x -> Refl (fmap nf' a) (nf' x)

      Cube -> Cube
      CubeUnit -> CubeUnit
      CubeUnitStar -> CubeUnitStar

      CubeProd t1 t2 -> CubeProd (nf' t1) (nf' t2)

      Tope -> Tope
      TopeTop -> TopeTop
      TopeBottom -> TopeBottom
      TopeOr t1 t2 -> TopeOr (nf' t1) (nf' t2)
      TopeAnd t1 t2 -> TopeAnd (nf' t1) (nf' t2)
      TopeEQ t1 t2 -> TopeEQ (nf' t1) (nf' t2)

      RecBottom -> RecBottom
      RecOr psi phi t1 t2 -> RecOr (nf' psi) (nf' phi) (nf' t1) (nf' t2)

      Cube2 -> Cube2
      Cube2_0 -> Cube2_0
      Cube2_1 -> Cube2_1

      TopeLEQ t1 t2 -> TopeLEQ (nf' t1) (nf' t2)

whnfWith :: EvalContext ann var a a -> Term ann var a -> Term ann var a
whnfWith EvalContext{..} = whnf'
  where
    whnf' = \case
      t@(Variable x) ->
        case definitionOf x of
          Left z   -> pure z
          Right t' -> whnf' t'
      Annotated ann t -> Annotated ann (whnf' t)
      App t1 t2   ->
        case whnf' t1 of
          Lambda _a _phi body -> whnf' (instantiate1 t2 body)
          t1'                 -> App t1' t2

      First t ->
        case whnf' t of
          Pair t1 _t2 -> whnf' t1
          t'          -> t'
      Second t ->
        case whnf' t of
          Pair _t1 t2 -> whnf' t2
          t'          -> t'

      IdJ tA a tC d x p ->
        case whnf' p of
          Refl{} -> whnf' d
          p'     -> IdJ tA a tC d x p'

      t@Universe -> t
      t@Lambda{} -> t
      t@ExtensionType{} -> t
      t@Pi{} -> t
      t@Sigma{} -> t
      t@Pair{} -> t

      t@IdType{} -> t
      t@Refl{} -> t

      t@Cube -> t
      t@CubeUnit -> t
      t@CubeUnitStar -> t
      t@CubeProd{} -> t

      t@Tope -> t
      t@TopeTop -> t
      t@TopeBottom -> t

      t@TopeOr{} -> t
      t@TopeAnd{} -> t
      t@TopeEQ{} -> t

      t@RecBottom -> t
      t@RecOr{} -> t

      t@Cube2 -> t
      t@Cube2_0 -> t
      t@Cube2_1 -> t
      t@TopeLEQ{} -> t

nf :: Term ann var a -> Term ann var a
nf = nfWith emptyEvalContext

whnf :: Term ann var a -> Term ann var a
whnf = whnfWith emptyEvalContext
