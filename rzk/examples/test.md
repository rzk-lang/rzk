# Examples

Here are some super basic examples of declarations embedded in Markdown file.

Identity function:

```rzk
id : (A : U) -> (_ : A) -> A
  := \(B : U) -> \(x : B) -> x
```

Church-encoded booleans with `id` used to make type look more complicated:

```rzk
false : (id U) ((A : U) -> (_x : A) -> (_y : A) -> A)
  := \(F : U) -> \(x : F) -> \(_ : F) -> x

true : (A : U) -> (_ : A) -> (_ : A) -> A
  := \(C : U) -> \(_ : C) -> \(y : C) -> y
```

Dependent sums:

```rzk
prod : (A : U) -> (B : U) -> U
  := \(A : U) -> \(B : U) -> ∑ (x : A), B

pair : (A : U) -> U
  := \(A : U) -> (prod A) A

ex1 : pair ((A : U) -> U)
  := ( id U, (id ((B : U) -> U)) (id U) )

ex2 : (A : U) -> U
  := first ex1

ex3 : (A : U) -> U
  := second ex1

ex4 : U
  := ∑ (A : U), pair A

ex5 : ex4
  := (U, (U, U))

ex6 : ex4
  := ((B : U) -> (x : B) -> B, (id, id))
```

Identity types:

```rzk
iscontr : (A : U) -> U
  := \(A : U) -> ∑ (x : A), (y : A) -> x =_{A} y

isaprop : (A : U) -> U
  := \(A : U) -> (x : A) -> (y : A) -> x =_{A} y

invpath : (A : U) -> (x : A) -> (y : A) -> (p : x =_{A} y) -> y =_{A} x
  := \(A : U) -> \(x : A) -> \(y : A) -> \(p : x =_{A} y) -> idJ(A, x, \(z : A) -> \(_ : x =_{A} z) -> z =_{A} x, refl_{x : A}, y, p)

ex7 : (A : U) -> (x : A) -> refl_{x : A} =_{x =_{A} x} ((((invpath A) x) x) refl_{x : A})
  := \(A : U) -> \(x : A) -> refl_{refl_{x : A} : x =_{A} x}
```

Equivalence:

```rzk
isweq : (A : U) -> (B : U) -> (f : (_ : A) -> B) -> U
  := \(A : U) -> \(B : U) -> \(f : (_ : A) -> B) -> ∑ (g : (_ : B) -> A), (prod ((x : A) -> (g (f x)) =_{A} x)) ((y : B) -> (f (g y)) =_{B} y)

weq : (A : U) -> (B : U) -> U
  := \(A : U) -> \(B : U) -> ∑ (f : (_ : A) -> B), ((isweq A) B) f

idweq : (A : U) -> (weq A) A
  := \(A : U) -> ( id A , ( id A, ( \(x : A) -> refl_{x : A}, \(x : A) -> refl_{x : A} ) ) )
```

Cubes and topes:

```rzk
ex8 : CUBE
  := 1

ex9 : CUBE
  := 1 * 1

ex10 : (I : CUBE) -> (t : I * I) -> I * I
  := \(I : CUBE) -> \(t : I * I) -> (second t, first t)

ex11 : (t : 1 * 1) -> TOPE
  := \(t : 1 * 1) -> (second t) === (first t)
```

Constraints:

```rzk
ex12 : (I : CUBE) -> <{t : I | BOT} -> U[BOT |-> recBOT]>
  := \(I : CUBE) -> \{t : I | BOT} -> recBOT

ex13 : (I : CUBE) -> (A : U) -> <{t : I | BOT} -> A[BOT |-> recBOT]>
  := \(I : CUBE) -> \(A : U) -> \{t : I | BOT} -> recBOT

ex14 : (I : CUBE) -> (phi : (t : I) -> TOPE) -> (psi : (t : I) -> TOPE) -> (A : U) -> (a : <{t : I | phi t \/ psi t} -> A[BOT |-> recBOT]>) -> <{t : I | psi t \/ phi t} -> A[BOT |-> recBOT]>
  := \(I : CUBE) -> \(phi : (t : I) -> TOPE) -> \(psi : (t : I) -> TOPE) -> \(A : U) -> \(a : <{t : I | phi t \/ psi t} -> A[BOT |-> recBOT]>) -> \{t : I | phi t \/ psi t} -> a t
```

### RS17, Section 4

#### Theorem 4.1

```rzk
RS17:Thm:4_1 : (I : CUBE) -> (psi : (t : I) -> TOPE) -> (phi : {t : I | psi t} -> TOPE) -> (X : U) -> (Y : <{t : I | psi t} -> (x : X) -> U [BOT |-> recBOT]>) -> (f : <{t : I | phi t} -> (x : X) -> Y t x [BOT |-> recBOT]>) -> weq (<{t : I | psi t} -> (x : X) -> Y t x [phi t |-> f t]>) ((x : X) -> <{t : I | psi t} -> Y t x [phi t |-> f t x]>)
  := \(I : CUBE) -> \(psi : (t : I) -> TOPE) -> \(phi : {t : I | psi t} -> TOPE) -> \(X : U) -> \(Y : <{t : I | psi t} -> (x : X) -> U [BOT |-> recBOT]>) -> \(f : <{t : I | phi t} -> (x : X) -> Y t x [BOT |-> recBOT]>) -> (\(g : <{t : I | psi t} -> (x : X) -> Y t x [phi t |-> f t]>) -> \(x : X) -> \{t : I | psi t} -> g t x, (\(g : (x : X) -> <{t : I | psi t} -> Y t x [phi t |-> f t x]>) -> \{t : I | psi t} -> \(x : X) -> g x t, (\(g : <{t : I | psi t} -> (x : X) -> Y t x [phi t |-> f t]>) -> refl_{g : <{t : I | psi t} -> (x : X) -> Y t x [phi t |-> f t]>}, \(g : (x : X) -> <{t : I | psi t} -> Y t x [phi t |-> f t x]>) -> refl_{g : (x : X) -> <{t : I | psi t} -> Y t x [phi t |-> f t x]>})))
```

Here's a version that's a bit nicer to read, but is not supported at the moment:

```
RS17:Thm:4_1
  :  (I : CUBE)
  -> (psi : I -> TOPE)
  -> (phi : {t : I | psi t} -> TOPE)
  -> (Y : <{t : I | psi t} -> (x : X) -> U>)
  -> (f : <{t : I | phi t} -> (x : X) -> Y t x>)
  -> weq (<{t : I | psi t} -> (x : X) -> Y t x [phi t |-> f t]>)
         ((x : X) -> <{t : I | psi t} -> Y t x [phi t |-> f t x]>)
  := \ I psi phi Y f -> 
    (g, (h, (\_ -> refl, \_ -> refl)))
  where
    g = \k x t -> k t x
    h = \k t x -> k x t
```

#### Theorem 4.2

```rzk
RS17:Thm:4_2a : (I : CUBE) -> (J : CUBE) -> (psi : (t : I) -> TOPE) -> (zeta : (s : J) -> TOPE) -> (phi : {t : I | psi t} -> TOPE) -> (chi : {s : J | zeta s} -> TOPE) -> (X : <{t : I | psi t} -> <{s : J | zeta s} -> U[BOT |-> recBOT]> [BOT |-> recBOT]>) -> (f : <{ts : I * J | (phi (first ts) /\ zeta (second ts)) \/ (psi (first ts) /\ chi (second ts))} -> X (first ts) (second ts) [BOT |-> recBOT]>) -> weq <{t : I | psi t} -> <{s : J | zeta s} -> X t s [chi s |-> f (t, s)]> [phi t |-> \{s : J | zeta s} -> f (t, s)]> <{ts : I * J | psi (first ts) /\ zeta (second ts)} -> X (first ts) (second ts) [ (phi (first ts) /\ zeta (second ts)) \/ (psi (first ts) /\ chi (second ts)) |-> f ts]>
  := \(I : CUBE) -> \(J : CUBE) -> \(psi : (t : I) -> TOPE) -> \(zeta : (s : J) -> TOPE) -> \(phi : {t : I | psi t} -> TOPE) -> \(chi : {s : J | zeta s} -> TOPE) -> \(X : <{t : I | psi t} -> <{s : J | zeta s} -> U[BOT |-> recBOT]> [BOT |-> recBOT]>) -> \(f : <{ts : I * J | (phi (first ts) /\ zeta (second ts)) \/ (psi (first ts) /\ chi (second ts))} -> X (first ts) (second ts) [BOT |-> recBOT]>) -> (\(k : <{t : I | psi t} -> <{s : J | zeta s} -> (X t) s[ chi s |-> f (t, s) ]> [ phi t |-> \{s : J | zeta s} -> f (t, s) ]>) -> \{ts : I * J | psi (first ts) /\ zeta (second ts)} -> k (first ts) (second ts), (\(k : <{ts : I * J | psi (first ts) /\ zeta (second ts)} -> X (first ts) (second ts)[ (phi (first ts) /\ zeta (second ts)) \/ (psi (first ts) /\ chi (second ts)) |-> f ts ]>) -> \{t : I | psi t} -> \{s : J | zeta s} -> k (t, s), (\(k : <{t : I | psi t} -> <{s : J | zeta s} -> X t s[ chi s |-> f (t, s) ]>[ phi t |-> \{s : J | zeta s} → f (t, s) ]>) -> refl_{k : <{t : I | psi t} -> <{s : J | zeta s} -> X t s[ chi s |-> f (t, s) ]>[ phi t |-> \{s : J | zeta s} → f (t, s) ]>}, \(k : <{ts : I * J | psi (first ts) /\ zeta (second ts)} -> (X (first ts)) (second ts)[ (phi (first ts) /\ zeta (second ts)) \/ (psi (first ts) /\ chi (second ts)) |-> f ts ]>) -> refl_{k : <{ts : I * J | psi (first ts) /\ zeta (second ts)} -> (X (first ts)) (second ts)[ (phi (first ts) /\ zeta (second ts)) \/ (psi (first ts) /\ chi (second ts)) |-> f ts ]>})))
```

#### Theorem 4.3

```rzk
RS17:Thm:4_3 : (I : CUBE) -> (psi : (t : I) -> TOPE) -> (phi : {t : I | psi t} -> TOPE) -> (X : <{t : I | psi t} -> U [BOT |-> recBOT]>) -> (Y : <{t : I | psi t} -> (x : X t) -> U [BOT |-> recBOT]>) -> (a : <{t : I | phi t} -> X t [BOT |-> recBOT]>) -> (b : <{t : I | phi t} -> Y t (a t) [BOT |-> recBOT]>) -> weq <{t : I | psi t} -> ∑ (x : X t), Y t x [phi t |-> (a t, b t)]> (∑ (f : <{t : I | psi t} -> X t [phi t |-> a t]>), <{t : I | psi t} -> Y t (f t) [phi t |-> b t]>)
  := \(I : CUBE) -> \(psi : (t : I) -> TOPE) -> \(phi : {t : I | psi t} -> TOPE) -> \(X : <{t : I | psi t} -> U [BOT |-> recBOT]>) -> \(Y : <{t : I | psi t} -> (x : X t) -> U [BOT |-> recBOT]>) -> \(a : <{t : I | phi t} -> X t [BOT |-> recBOT]>) -> \(b : <{t : I | phi t} -> Y t (a t) [BOT |-> recBOT]>) -> (\(k : <{t : I | psi t} -> ∑ (x : X t), Y t x[ phi t |-> (a t, b t) ]>) -> (\{t : I | psi t} -> first (k t), \{t : I | psi t} -> second (k t)), (\(k : ∑ (f : <{t : I | psi t} -> X t[ phi t |-> a t ]>), <{t : I | psi t} -> Y t (f t)[ phi t |-> b t ]>) -> \{t : I | psi t} -> ((first k) t, (second k) t), (\(k : <{t : I | psi t} -> ∑ (x : X t), Y t x[ phi t |-> (a t, b t) ]>) -> refl_{k : <{t : I | psi t} -> ∑ (x : X t), Y t x[ phi t |-> (a t, b t) ]>}, \(k : ∑ (f : <{t : I | psi t} -> X t[ phi t |-> a t ]>), <{t : I | psi t} -> Y t (f t)[ phi t |-> b t ]>) -> refl_{k : ∑ (f : <{t : I | psi t} -> X t[ phi t |-> a t ]>), <{t : I | psi t} -> Y t (f t)[ phi t |-> b t ]>})))
```

### Typechecking Markdown files

You can typecheck this file directly:

```
rzk typecheck examples/test.md
```

The result should look something like this

```
Everything is ok!

Free variables and their known types:
  true : ( A : 𝒰 ) → ( _ : A ) → ( _₁ : A ) → A
  false : ( A : 𝒰 ) → ( _x : A ) → ( _y : A ) → A
  id : ( A : 𝒰 ) → ( _ : A ) → A
Type holes and their instantiations:
  ?A₂ := _
  ?(H)₃ := 𝒰
  ?A₁ := _
  ?(H)₂ := 𝒰
  ?U₁ := 𝒰
  ?(H)₁ := 𝒰
```
