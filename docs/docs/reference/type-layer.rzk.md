# Types and terms

```rzk
#lang rzk-1
```

## Functions (dependent products)

Function (dependent product) types \(\prod_{x : A} B\) are written `#!rzk (x : A) -> B x`. Values of function types are \(\lambda\)-abstractions written in one of the following ways:

  - `#!rzk \x -> <body>` — this is usually fine;
  - `#!rzk \(x : A) -> <body>` — this sometimes helps the typechecker.

## Dependent sums

Dependent sum type \(\sum_{x : A} B\) is written `#!rzk ∑ (x : A), B` or `#!rzk Sigma (x : A), B`. Values of dependent sum types are pairs written as `#!rzk (x, y)`.

To access components of a dependent pair `#!rzk p`, use `#!rzk first p` and `#!rzk second p`.

!!! warning
    `#!rzk first` and `#!rzk second` are not valid syntax without an argument!

## Identity types

Identity (path) type \(x =_A y\) is written `#!rzk x =_{A} y`.

!!! tip
    Specifying the type `#!rzk A` is optional: `#!rzk x = y` is valid syntax!

Any identity type has value `#!rzk refl_{x : A}` whose type is `#!rzk x =_{A} x` whenever `#!rzk x : A`

!!! tip
    Specifying term and type of `#!rzk refl_{x : A}` is optional: `#!rzk refl_{x}` and `#!rzk refl` are both valid syntax.

Path induction is done using \(\mathcal{J}\) path eliminator:

- for
    - any type \(A\) and \(a : A\),
    - type family \(C : \prod_{x : A} ((a =_A x) \to \mathcal{U})\) and
    - \(d : C(a,\mathsf{refl}_a)\) and
    - \(x : A\) and \(p : a =_A x\)
- we have \(\mathcal{J}(A, a, C, d, x, p) : C(x, p)\)

In `#!rzk rzk-1` we write `#!rzk idJ(A, a, C, d, x, p)`

!!! warning
    `#!rzk idJ` is not valid syntax without exactly 6-tuple provided as an argument!

