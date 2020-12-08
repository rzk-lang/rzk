# Examples

```
-- [RS17, Theorem 4.5]
f :
     (I : Cube)
  -> (phi : I -> Tope)
  -> (psi : I -> Tope)
  -> (X : {t : I | phi t \/ psi t} -> U)
  -> (a : {t : I | phi t \/ psi t} -> X t)
  -> ({t : I | phi t \/ psi t} -> X(t) [psi t -> a t])
```

Here are some super basic examples of declarations embedded in Markdown file.

Identity function:

```rzk
id : {A : U} -> ({_ : A} -> A)
  := \(B : U) -> \(x : B) -> x
```

Church-encoded booleans with `id` used to make type look more complicated:

```rzk
false : (id ?UN) ({A : U} -> {_x : A} -> {_y : A} -> A)
  := \(F : U) -> \(x : F) -> \(_ : F) -> x

true : {A : U} -> {_ : A} -> {_ : A} -> A
  := \(C : U) -> \(_ : C) -> \(y : C) -> y
```

Dependent sums:

```rzk
prod : {A : U} -> {B : U} -> U
  := \(A : U) -> \(B : U) -> ∑ (x : A), B

pair : {A : U} -> U
  := \(A : U) -> (prod A) A

ex1 : pair ({A : U} -> U)
  := ( id U, (id ({B : U} -> U)) (id U) )

ex2 : {A : U} -> U
  := first ex1

ex3 : {A : U} -> U
  := second ex1

ex4 : U
  := ∑ (A : U), pair A

ex5 : ex4
  := (U, (U, U))

ex6 : ex4
  := ({B : U} -> {x : B} -> B, (id, id))
```

Identity types:

```rzk
iscontr : {A : U} -> U
  := \(A : U) -> ∑ (x : A), {y : A} -> x =_{A} y

isaprop : {A : U} -> U
  := \(A : U) -> {x : A} -> {y : A} -> x =_{A} y

invpath : {A : U} -> {x : A} -> {y : A} -> {p : x =_{A} y} -> y =_{A} x
  := \(A : U) -> \(x : A) -> \(y : A) -> \(p : x =_{A} y) -> idJ(A, x, \(z : A) -> \(_ : x =_{A} z) -> z =_{A} x, refl_{x : A}, y, p)

ex7 : {A : U} -> {x : A} -> refl_{x : A} =_{x =_{A} x} ((((invpath A) x) x) refl_{x : A})
  := \(A : U) -> \(x : A) -> refl_{refl_{x : A} : x =_{A} x}
```

Equivalence:

```rzk
isweq : {A : U} -> {B : U} -> {f : {_ : A} -> B} -> U
  := \(A : U) -> \(B : U) -> \(f : {_ : A} -> B) -> ∑ (g : {_ : B} -> A), (prod ({x : A} -> (g (f x)) =_{A} x)) ({y : B} -> (f (g y)) =_{B} y)

weq : {A : U} -> {B : U} -> U
  := \(A : U) -> \(B : U) -> ∑ (f : {_ : A} -> B), ((isweq A) B) f

idweq : {A : U} -> (weq A) A
  := \(A : U) -> ( id A , ( id A, ( \(x : A) -> refl_{x : A}, \(x : A) -> refl_{x : A} ) ) )
```

Cubes and topes:

```rzk
ex8 : CUBE
  := 1

ex9 : CUBE
  := 1 * 1

ex10 : {I : CUBE} -> {t : I * I} -> I * I
  := \(I : CUBE) -> \(t : I * I) -> (second t, first t)

ex11 : {t : 1 * 1} -> TOPE
  := \(t : 1 * 1) -> (second t) === (first t)
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
  true : { A : 𝒰 } → { _ : A } → { _₁ : A } → A
  false : { A : 𝒰 } → { _x : A } → { _y : A } → A
  id : { A : 𝒰 } → { _ : A } → A
Type holes and their instantiations:
  ?A₂ := _
  ?{H}₃ := 𝒰
  ?A₁ := _
  ?{H}₂ := 𝒰
  ?U₁ := 𝒰
  ?{H}₁ := 𝒰
```
