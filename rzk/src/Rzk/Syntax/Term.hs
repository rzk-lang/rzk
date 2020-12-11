{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor  #-}
module Rzk.Syntax.Term where

-- | This is a (probably unevaluated) term out of context.
data Term var
  = Variable var
  -- ^ Term variable \(x_i\) or type variable \(A_i\) or cube variable \(t_i\).

  | TypedTerm (Term var) (Term var)
  -- ^ Explicitly typed term \(M : A\).

  | Hole var
  -- ^ Term or type hole (to be filled by the typechecker).

  | Universe
  -- ^ Universe type \(\mathcal{U}\).
  -- Saying \(A \mathbf{type}\) is equivalent to saying \(A : \mathcal{U}\).

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

  | Cube
  -- ^ Cube "universe". Technically, it is not a type, but it is treated as such syntactically.

  | CubeUnit
  -- ^ Unit cube: \(\mathbf{1}\;\mathsf{cube}\).
  | CubeUnitStar
  -- ^ The only point in the unit cube: \(\star : \mathbf{1}\).

  | CubeProd (Term var) (Term var)
  -- ^ Product of cubes: \(I \times J\).

  | Tope
  -- ^ Tope "universe". Like cubes, this is not a type.
  | TopeTop
  -- ^ Top tope (no constraints on cube): \(\top\;\mathsf{tope}\)
  | TopeBottom
  -- ^ Bottom tope (cube contrained to empty space): \(\bot\;\mathsf{tope}\)
  | TopeOr (Term var) (Term var)
  -- ^ Tope disjuction (union of shapes): \(\psi \lor \phi\;\mathsf{tope}\)
  | TopeAnd (Term var) (Term var)
  -- ^ Tope conjunction (intersection of shapes): \(\psi \land \phi\;\mathsf{tope}\)
  | TopeEQ (Term var) (Term var)
  -- ^ Equality tope (diagonals): \(t \equiv s \;\mathsf{tope}\).
  -- Note that it can involve projections as well, e.g. \(\pi_1 t \equiv \pi_2 t\).

  | RecBottom
  -- ^ Bottom tope eliminator: \(\mathsf{rec}_\bot : A\).
  | RecOr (Term var) (Term var) (Term var) (Term var)
  -- ^ Tope disjunction eliminator: \(\mathsf{rec}^{\psi,\phi}_\lor(a_\psi, a_\phi)\).

  | ExtensionType var (Term var) (Term var) (Term var) (Term var) (Term var)
  -- ^ Extension type \( \left\langle \prod_{t : I | psi} A(t) \rvert^{\phi}_{a(t)} \right\rangle \) corresponding to @ExtensionType t cI psi tA phi a@.
  deriving (Eq, Functor, Foldable)

