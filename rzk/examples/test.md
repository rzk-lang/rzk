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
  := \(F : ?U1) -> \(x : F) -> \(_ : F) -> x

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
