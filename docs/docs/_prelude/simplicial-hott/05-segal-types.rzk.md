# Segal Types

These formalisations correspond to Section 5 of the RS17 paper.

This is a literate `rzk` file:

```rzk
#lang rzk-1

#require-file "docs/docs/_prelude/hott/06-contractible.rzk.md"

#require-file "docs/docs/_prelude/simplicial-hott/03-simplicial-type-theory.rzk.md"
```

## Hom types

Extension types are used to define the type of arrows between fixed terms:

```rzk title="RS17, Definition 5.1"
-- The type of arrows in A from x to y.
#def hom
  (A : U)
  (x y : A)
  : U
  := (t : Δ¹) -> A [
    t === 0_2 |-> x ,    -- * the left endpoint is exactly x
    t === 1_2 |-> y     -- * the right endpoint is exactly y
  ]
```

Extension types are also used to define the type of commutative triangles:

```rzk title="RS17, Definition 5.2"
-- the type of commutative triangles in A
#def hom2
  (A : U)
  (x y z : A)
  (f : hom A x y)
  (g : hom A y z)
  (h : hom A x z)
  : U
  := { (t1 , t2) : Δ² } -> A [
    t2 === 0_2 |-> f t1 ,        -- * the top edge is exactly `f`,
    t1 === 1_2 |-> g t2 ,        -- * the right edge is exactly `g`, and
    t2 === t1 |-> h t2         -- * the diagonal is exactly `h`
  ]
```

## The Segal condition

A type is Segal if every composable pair of arrows has a unique composite. Note
this is a considerable simplification of the usual Segal condition, which also
requires homotopical uniqueness of higher-order composites.

```rzk title="RS17, Definition 5.3"
#def is-segal
  (A : U)
  : U
  := (x : A) -> (y : A) -> (z : A) ->
      (f : hom A x y) -> (g : hom A y z) ->
      is-contr ( Σ (h : hom A x z) , hom2 A x y z f g h)
```
