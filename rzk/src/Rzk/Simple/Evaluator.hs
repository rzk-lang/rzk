{-# LANGUAGE LambdaCase #-}
module Rzk.Simple.Evaluator where

import           Bound
import           Rzk.Simple.Syntax.Term

nfScoped :: Scope b (Term var) a -> Scope b (Term var) a
nfScoped = Scope . nf . unscope

nf :: Term var a -> Term var a
nf = \case
  App t1 t2   ->
    case whnf t1 of
      Lambda _a _phi body -> nf (instantiate1 t2 body)
      t1'                 -> App (nf t1') (nf t2)

  First t ->
    case whnf t of
      Pair t1 _t2 -> nf t1
      t'          -> nf t'
  Second t ->
    case whnf t of
      Pair _t1 t2 -> nf t2
      t'          -> nf t'

  IdJ tA a tC d x p ->
    case whnf p of
      Refl{} -> nf d
      p'     -> IdJ (nf tA) (nf a) (nf tC) (nf d) (nf x) (nf p')

  e@Variable{} -> e
  e@Universe -> e

  ExtensionType tC psi tA phi a ->
    ExtensionType (nf tC) (nfScoped psi) (nfScoped tA) (nfScoped phi) (nfScoped a)
  Pi t'       -> Pi (nf t')
  Lambda a phi body
    -> Lambda (fmap nf a) (fmap nfScoped phi) (nfScoped body)

  Sigma t'    -> Sigma (nf t')
  Pair t1 t2  -> Pair (nf t1) (nf t2)

  IdType a x y -> IdType (nf a) (nf x) (nf y)
  Refl a x -> Refl (fmap nf a) (nf x)

  Cube -> Cube
  CubeUnit -> CubeUnit
  CubeUnitStar -> CubeUnitStar

  CubeProd t1 t2 -> CubeProd (nf t1) (nf t2)

  Tope -> Tope
  TopeTop -> TopeTop
  TopeBottom -> TopeBottom
  TopeOr t1 t2 -> TopeOr (nf t1) (nf t2)
  TopeAnd t1 t2 -> TopeAnd (nf t1) (nf t2)
  TopeEQ t1 t2 -> TopeEQ (nf t1) (nf t2)

  RecBottom -> RecBottom
  RecOr psi phi t1 t2 -> RecOr (nf psi) (nf phi) (nf t1) (nf t2)

  Cube2 -> Cube2
  Cube2_0 -> Cube2_0
  Cube2_1 -> Cube2_1

  TopeLEQ t1 t2 -> TopeLEQ (nf t1) (nf t2)

whnf :: Term var a -> Term var a
whnf = \case
  App t1 t2   ->
    case whnf t1 of
      Lambda _a _phi body -> whnf (instantiate1 t2 body)
      t1'                 -> App t1' t2

  First t ->
    case whnf t of
      Pair t1 _t2 -> nf t1
      t'          -> t'
  Second t ->
    case whnf t of
      Pair _t1 t2 -> nf t2
      t'          -> t'

  IdJ tA a tC d x p ->
    case whnf p of
      Refl{} -> whnf d
      p'     -> IdJ tA a tC d x p'

  t@Variable{} -> t
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

