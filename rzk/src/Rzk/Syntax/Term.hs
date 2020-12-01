{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor  #-}
module Rzk.Syntax.Term where

-- | This is a (probably unevaluated) term out of context.
data Term var
  = Variable var                  -- ^ Term variable \(x_i\) or type variable \(A_i\) or cube variable \(t_i\).
  | Hole var                      -- ^ Term or type hole (to be filled by the typechecker).
  | Universe  -- ^ Universe \(\mathcal{U}\).

  | Pi (Term var)
  -- ^ Dependent function type former \(\prod_{x : A} B(x)\).
  -- The term argument represents type family \(B : A \to \mathcal{U}\).
  | Lambda var (Term var) (Term var)
  -- ^ \(\lambda\)-abstraction ("lambda abstraction").
  -- @Lambda x a m@ represents a term of the form \(\lambda (x : A). M\).
  | App (Term var) (Term var)
  -- ^ Application of one term to another \((M N)\).
  deriving (Eq, Functor, Foldable)
