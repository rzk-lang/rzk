# RS17, Section 4

```rzk
prod : (A : U) -> (B : U) -> U
  := \(A : U) -> \(B : U) -> ∑ (x : A), B

isweq : (A : U) -> (B : U) -> (f : (_ : A) -> B) -> U
  := \(A : U) -> \(B : U) -> \(f : (_ : A) -> B) -> ∑ (g : (_ : B) -> A), (prod ((x : A) -> (g (f x)) =_{A} x)) ((y : B) -> (f (g y)) =_{B} y)

weq : (A : U) -> (B : U) -> U
  := \(A : U) -> \(B : U) -> ∑ (f : (_ : A) -> B), ((isweq A) B) f
```

## Theorem 4.1

```rzk
RS17:Thm:4_1 : (I : CUBE) -> (psi : (t : I) -> TOPE) -> (phi : {t : I | psi t} -> TOPE) -> (X : U) -> (Y : <{t : I | psi t} -> (x : X) -> U [BOT |-> recBOT]>) -> (f : <{t : I | phi t} -> (x : X) -> Y t x [BOT |-> recBOT]>) -> weq (<{t : I | psi t} -> (x : X) -> Y t x [phi t |-> f t]>) ((x : X) -> <{t : I | psi t} -> Y t x [phi t |-> f t x]>)
  := \(I : CUBE) -> \(psi : (t : I) -> TOPE) -> \(phi : {t : I | psi t} -> TOPE) -> \(X : U) -> \(Y : <{t : I | psi t} -> (x : X) -> U [BOT |-> recBOT]>) -> \(f : <{t : I | phi t} -> (x : X) -> Y t x [BOT |-> recBOT]>) -> (\(g : <{t : I | psi t} -> (x : X) -> Y t x [phi t |-> f t]>) -> \(x : X) -> \{t : I | psi t} -> g t x, (\(g : (x : X) -> <{t : I | psi t} -> Y t x [phi t |-> f t x]>) -> \{t : I | psi t} -> \(x : X) -> g x t, (\(g : <{t : I | psi t} -> (x : X) -> Y t x [phi t |-> f t]>) -> refl_{g : <{t : I | psi t} -> (x : X) -> Y t x [phi t |-> f t]>}, \(g : (x : X) -> <{t : I | psi t} -> Y t x [phi t |-> f t x]>) -> refl_{g : (x : X) -> <{t : I | psi t} -> Y t x [phi t |-> f t x]>})))
```

