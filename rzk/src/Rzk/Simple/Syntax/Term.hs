{-# LANGUAGE DeriveFoldable  #-}
{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE TemplateHaskell #-}
module Rzk.Simple.Syntax.Term where

import           Bound
import           Bound.Name
import           Control.Monad        (ap)
import           Data.Deriving        (deriveEq1, deriveOrd1, deriveRead1,
                                       deriveShow1)
import           Data.Functor.Classes

type Scope1 var = Scope (Name var ())

type Scope1Term var a = Scope1 var (Term var) a

-- | This is a (probably unevaluated) term out of context.
data Term var a
  = Variable a
  -- ^ Term variable \(x_i\) or type variable \(A_i\) or cube variable \(t_i\) or tope variable \(\phi_i\).

  | Universe
  -- ^ Universe type \(\mathcal{U}\).
  -- Saying \(A \mathbf{type}\) is equivalent to saying \(A : \mathcal{U}\).

  | Pi (Term var a)
  -- ^ Dependent product type former \(\prod_{x : A} B(x)\).
  -- The term argument represents type family \(B : A \to \mathcal{U}\).
  | Lambda (Maybe (Term var a)) (Maybe (Scope1Term var a)) (Scope1Term var a)
  -- ^ \(\lambda\)-abstraction ("lambda abstraction").
  -- @Lambda x a Nothing m@ represents a term of the form \(\lambda (x : A). M\)
  -- while @Lambda t i (Just phi) m@ represents \(\lambda \{t : I | \phi\}. M\)
  | App (Term var a) (Term var a)
  -- ^ Application of one term to another \((M N)\).

  | Sigma (Term var a)
  -- ^ Dependent sum type former \(\sum_{x : A} B(x)\).
  -- The term argument represents type family \(B : A \to \mathcal{U}\).
  | Pair (Term var a) (Term var a)
  -- ^ A (dependent) pair of terms.
  -- @Pair x y@ represents a term of the form \((x, y)\).
  | First (Term var a)
  -- ^ Project the first element of a pair: \(\pi_1 p\).
  | Second (Term var a)
  -- ^ Project the second element of a pair: \(\pi_2 p\).

  | IdType (Term var a) (Term var a) (Term var a)
  -- ^ Identity type former \(x =_A y\) (corresponding to term @IdType a x y@).
  | Refl (Maybe (Term var a)) (Term var a)
  -- ^ Trivial inhabitant of \(x =_A x\) for any type \(A\) and \(x : A\).
  -- @Refl a x@ corresponds to \(x =_a x\).
  | IdJ (Term var a) (Term var a) (Term var a) (Term var a) (Term var a) (Term var a)
  -- ^ Path induction (for identity types).
  -- For any type \(A\) and \(a : A\), type family
  -- \(C : \prod_{x : A} ((a =_A x) \to \mathcal{U})\)
  -- and \(d : C(a,\mathsf{refl}_a)\)
  -- and \(x : A\)
  -- and \(p : a =_A x\)
  -- we have \(\mathcal{J}(A, a, C, d, x, p) : C(x, p)\).

  | Cube
  -- ^ Cube "universe". Technically, it is not a type, but it is treated as such syntactically.

  | CubeUnit
  -- ^ Unit cube: \(\mathbf{1}\;\mathsf{cube}\).
  | CubeUnitStar
  -- ^ The only point in the unit cube: \(\star : \mathbf{1}\).

  | CubeProd (Term var a) (Term var a)
  -- ^ Product of cubes: \(I \times J\).

  | Tope
  -- ^ Tope "universe". Like cubes, this is not a type.
  | TopeTop
  -- ^ Top tope (no constraints on cube): \(\top\;\mathsf{tope}\)
  | TopeBottom
  -- ^ Bottom tope (cube contrained to empty space): \(\bot\;\mathsf{tope}\)
  | TopeOr (Term var a) (Term var a)
  -- ^ Tope disjuction (union of shapes): \(\psi \lor \phi\;\mathsf{tope}\)
  | TopeAnd (Term var a) (Term var a)
  -- ^ Tope conjunction (intersection of shapes): \(\psi \land \phi\;\mathsf{tope}\)
  | TopeEQ (Term var a) (Term var a)
  -- ^ Equality tope (diagonals): \(t \equiv s \;\mathsf{tope}\).
  -- Note that it can involve projections as well, e.g. \(\pi_1 t \equiv \pi_2 t\).

  | RecBottom
  -- ^ Bottom tope eliminator: \(\mathsf{rec}_\bot : A\).
  | RecOr (Term var a) (Term var a) (Term var a) (Term var a)
  -- ^ Tope disjunction eliminator: \(\mathsf{rec}^{\psi,\phi}_\lor(a_\psi, a_\phi)\).

  | ExtensionType
      (Term var a) (Scope1Term var a) (Scope1Term var a) (Scope1Term var a) (Scope1Term var a)
  -- ^ Extension type \( \left\langle \prod_{t : I | psi} A(t) \rvert^{\phi}_{a(t)} \right\rangle \) corresponding to @ExtensionType t cI psi tA phi a@.

  | Cube2
  -- ^ Directed interval cube: \(\mathbb{2}\).
  | Cube2_0
  -- ^ Start of directed interval: \(0 : \mathbb{2}\).
  | Cube2_1
  -- ^ End of directed interval: \(1 : \mathbb{2}\).

  | TopeLEQ (Term var a) (Term var a)
  -- ^ Inequality tope: \(t \leq s\).

  deriving (Functor, Foldable)

instance Applicative (Term var) where
  pure = Variable
  (<*>) = ap

instance Monad (Term var) where
  return = Variable
  t >>= f =
    case t of
      Variable x  -> f x
      Universe    -> Universe

      ExtensionType tC psi tA phi a ->
        ExtensionType (tC >>= f) (psi >>>= f) (tA >>>= f) (phi >>>= f) (a >>>= f)
      Pi t'       -> Pi (t' >>= f)
      Lambda a phi body
        -> Lambda (fmap (>>= f) a) (fmap (>>>= f) phi) (body >>>= f)
      App t1 t2   -> App (t1 >>= f) (t2 >>= f)

      Sigma t'    -> Sigma (t' >>= f)
      Pair t1 t2  -> Pair (t1 >>= f) (t2 >>= f)
      First t'    -> First (t' >>= f)
      Second t'   -> Second (t' >>= f)

      IdType a x y -> IdType (a >>= f) (x >>= f) (y >>= f)
      Refl a x -> Refl (fmap (>>= f) a) (x >>= f)
      IdJ tA a tC d x p -> IdJ (tA >>= f) (a >>= f) (tC >>= f) (d >>= f) (x >>= f) (p >>= f)

      Cube -> Cube
      CubeUnit -> CubeUnit
      CubeUnitStar -> CubeUnitStar

      CubeProd t1 t2 -> CubeProd (t1 >>= f) (t2 >>= f)

      Tope -> Tope
      TopeTop -> TopeTop
      TopeBottom -> TopeBottom
      TopeOr t1 t2 -> TopeOr (t1 >>= f) (t2 >>= f)
      TopeAnd t1 t2 -> TopeAnd (t1 >>= f) (t2 >>= f)
      TopeEQ t1 t2 -> TopeEQ (t1 >>= f) (t2 >>= f)

      RecBottom -> RecBottom
      RecOr psi phi t1 t2 -> RecOr (psi >>= f) (phi >>= f) (t1 >>= f) (t2 >>= f)


      Cube2 -> Cube2
      Cube2_0 -> Cube2_0
      Cube2_1 -> Cube2_1

      TopeLEQ t1 t2 -> TopeLEQ (t1 >>= f) (t2 >>= f)

deriveEq1   ''Term
deriveOrd1  ''Term
deriveRead1 ''Term
deriveShow1 ''Term

instance (Eq var, Eq a) => Eq (Term var a) where (==) = eq1
instance (Ord var, Ord a) => Ord (Term var a) where compare = compare1
instance (Show var, Show a) => Show (Term var a) where showsPrec = showsPrec1
instance (Read var, Read a) => Read (Term var a) where readsPrec = readsPrec1
