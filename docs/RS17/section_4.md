# RS17, Section 4

## Prerequisites

```rzk
prod : (A : U) -> (B : U) -> U
  := \A -> \B -> ∑ (x : A), B

isweq : (A : U) -> (B : U) -> (f : (_ : A) -> B) -> U
  := \A -> \B -> \f -> ∑ (g : (_ : B) -> A), prod ((x : A) -> g (f x) =_{A} x) ((y : B) -> f (g y) =_{B} y)

weq : (A : U) -> (B : U) -> U
  := \A -> \B -> ∑ (f : (_ : A) -> B), isweq A B f
```

## Theorem 4.1 (commutation of Pi and extension types)

```rzk
Theorem-4.1 : (I : CUBE) -> (psi : (t : I) -> TOPE) -> (phi : {(t : I) | psi t} -> TOPE) -> (X : U) -> (Y : <{t : I | psi t} -> (x : X) -> U >) -> (f : <{t : I | phi t} -> (x : X) -> Y t x >) -> weq <{t : I | psi t} -> (x : X) -> Y t x [phi t |-> f t]> ((x : X) -> <{t : I | psi t} -> Y t x [phi t |-> f t x]>)
  := \I -> \psi -> \phi -> \X -> \Y -> \f -> (\k -> \x -> \t -> k t x, (\k -> \{t : I | psi t} -> \x -> (k x) t, (\k -> refl_{k}, \k -> refl_{k})))
```

## Theorem 4.2 (currying and commutation of arguments for extension types)

```rzk
uncurry_ext : (I : CUBE) -> (J : CUBE) -> (psi : (t : I) -> TOPE) -> (zeta : (s : J) -> TOPE) -> (X : <{t : I | psi t} -> <{s : J | zeta s} -> U> >) -> (chi : {(t : I) | psi t} -> TOPE) -> (phi : {(s : J) | zeta s} -> TOPE) -> (f : <{(t, s) : I * J | psi t /\ zeta s} -> X t s >) -> (_ : <{t : I | psi t} -> <{s : J | zeta s} -> X t s [chi s |-> f (t, s)]> [phi t |-> \s -> f (t, s)]>) -> <{(t, s) : I * J | psi t /\ zeta s} -> X t s [(phi t /\ zeta s) \/ (psi t /\ chi s) |-> f (t, s)]>
  := \I -> \J -> \psi -> \zeta -> \X -> \chi -> \phi -> \f -> \k -> \(t, s) -> k t s
```
