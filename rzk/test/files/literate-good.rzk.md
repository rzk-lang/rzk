# 1.4 Dependent function types ($\Pi$-types)

This is a literate Rzk file:

```rzk
#lang rzk-1
```

A polymorphic function is one which takes a type as one of its arguments,
and then acts on elements of that type (or of other types constructed from it).
An example is the polymorphic identity function:

```rzk
#define id
  ( A : U)
  : A → A
  := \ x → x
```

Another, less trivial, example of a polymorphic function is the "swap" operation
that switches the order of the arguments of a (curried) two-argument function:

```rzk
#define swap
  ( A B C : U)
  : ( A → B → C) → (B → A → C)
  := \ f y x → f x y
```
