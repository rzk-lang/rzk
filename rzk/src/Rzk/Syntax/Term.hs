{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor  #-}
module Rzk.Syntax.Term where

-- | This is a (probably unevaluated) term out of context.
data Term var
  = Variable var                  -- ^ Term variable \(x_i\) or type variable \(A_i\) or cube variable \(t_i\).
  | Hole var                      -- ^ Term or type hole (to be filled by the typechecker).
  | Universe  -- ^ Universe \(\mathcal{U}\).

  | Pi (Term var)
  -- ^ Dependent product type former \(\prod_{x : A} B(x)\).
  -- The term argument represents type family \(B : A \to \mathcal{U}\).
  | Lambda var (Term var) (Term var)
  -- ^ \(\lambda\)-abstraction ("lambda abstraction").
  -- @Lambda x a m@ represents a term of the form \(\lambda (x : A). M\).
  | App (Term var) (Term var)
  -- ^ Application of one term to another \((M N)\).

  | Sigma (Term var)
  -- ^ Dependent sum type former \(\sum_{x : A} B(x)\).
  -- The term argument represents type family \(B : A \to \mathcal{U}\).
  | Pair (Term var) (Term var)
  -- ^ A (dependent) pair of terms.
  -- @Pair x y@ represents a term of the form \((x, y)\).
  | First (Term var)
  -- ^ Project the first element of a pair: \(\pi_1 p\).
  | Second (Term var)
  -- ^ Project the second element of a pair: \(\pi_2 p\).

  | IdType (Term var) (Term var) (Term var)
  -- ^ Identity type former \(x =_A y\) (corresponding to term @IdType a x y@).
  | Refl (Term var) (Term var)
  -- ^ Trivial inhabitant of \(x =_A x\) for any type \(A\) and \(x : A\).
  -- @Refl a x@ corresponds to \(x =_a x\).
  | IdJ (Term var) (Term var) (Term var) (Term var) (Term var) (Term var)
  -- ^ Path induction (for identity types).
  -- For any type \(A\) and \(a : A\), type family
  -- \(C : \prod_{x : A} ((a =_A x) \to \mathcal{U})\)
  -- and \(d : C(a,\mathsf{refl}_a)\)
  -- and \(x : A\)
  -- and \(p : a =_A x\)
  -- we have \(\mathcal{J}(A, a, C, d, x, p) : C(x, p)\).
  deriving (Eq, Functor, Foldable)
