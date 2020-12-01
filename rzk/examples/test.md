# Examples

Here are some super basic examples of declarations embedded in Markdown file.

Identity function:

```rzk
id : {A : U} -> ({_ : A} -> A)
  := \(B : U) -> \(x : B) -> x
```

Church-encoded booleans with `id` used to make type look more complicated:

```rzk
false : (id U) ({A : U} -> {_x : A} -> {_y : A} -> A)
  := \(_ : ?U1) -> \(x : ?A1) -> \(_ : ?A2) -> x

true : {A : U} -> {_ : A} -> {_ : A} -> A
  := \(C : U) -> \(_ : C) -> \(y : C) -> y
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
