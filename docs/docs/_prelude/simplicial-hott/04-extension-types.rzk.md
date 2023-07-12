# Extension types

!!! info "Incomplete file"

    This file contains minimal definitions required in the examples used in
    the documentation. Refer to <https://emilyriehl.github.io/yoneda/simplicial-hott/04-extension-types.rzk/>
    for a more complete file on equivalences.

```rzk
#lang rzk-1

#require-file "docs/docs/_prelude/hott/00-common.rzk.md"
#require-file "docs/docs/_prelude/hott/03-equivalences.rzk.md"

#require-file "docs/docs/_prelude/simplicial-hott/03-simplicial-type-theory.rzk.md"
```

## Relative function extensionality

There are various equivalent forms of the relative function extensionality
axiom. Here we state the one that will be most useful and derive an application.

```rzk
#def ext-htpy-eq
  ( I : CUBE)
  ( ψ : I -> TOPE)
  ( ϕ : ψ -> TOPE)
  ( A : ψ -> U)
  ( a : (t : ϕ) -> A t)
  ( f g : (t : ψ) -> A t [ ϕ t |-> a t ])
  ( p : f = g)
  : (t : ψ) -> (f t = g t) [ ϕ t |-> refl ]
  := idJ
    ( ( (t : ψ) -> A t [ ϕ t |-> a t ]) ,
      ( f) ,
      ( \ g' p' -> (t : ψ) -> (f t = g' t) [ ϕ t |-> refl ]) ,
      ( \ t -> refl) ,
      ( g) ,
      ( p))
```

The type that encodes the extension extensionality axiom. As suggested by
footnote 8, we assert this as an "extension extensionality" axiom

```rzk title="RS17, Proposition 4.8(ii)"

#def ExtExt
  : U
  :=
    ( I : CUBE) ->
    ( ψ : I -> TOPE) ->
    ( ϕ : ψ -> TOPE) ->
    ( A : ψ -> U) ->
    ( a : (t : ϕ) -> A t) ->
    ( f : (t : ψ) -> A t [ ϕ t |-> a t ]) ->
    ( g : (t : ψ) -> A t [ ϕ t |-> a t ]) ->
    is-equiv
      ( f = g)
      ( (t : ψ) -> (f t = g t) [ ϕ t |-> refl ])
      ( ext-htpy-eq I ψ ϕ A a f g)

#def eq-ext-htpy
  ( extext : ExtExt)
  ( I : CUBE)
  ( ψ : I -> TOPE)
  ( ϕ : ψ -> TOPE)
  ( A : ψ -> U)
  ( a : (t : ϕ) -> A t)
  ( f g : (t : ψ) -> A t [ ϕ t |-> a t ])
  : ((t : ψ) -> (f t = g t) [ ϕ t |-> refl ]) -> (f = g)
  := first (first (extext I ψ ϕ A a f g))
```
