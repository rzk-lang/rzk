# 0. Common

This is a literate `rzk` file:

```rzk
#lang rzk-1
```

## products of types

```rzk
#def product
  ( A B : U)
  : U
  := Σ (x : A) , B
```

The following demonstrates the syntax for constructing terms in Sigma types:

```rzk
#def diagonal
  ( A : U)
  ( a : A)
  : product A A
  := (a , a)
```

## The type of logical equivalences between types

```rzk
#def iff
  ( A B : U)
  : U
  := product (A -> B) (B -> A)
```

## Basic function definitions

```rzk
#section basic-functions

#variables A B C D : U

#def composition
  ( g : B -> C)
  ( f : A -> B)
  : A -> C
  := \ z -> g (f z)

#def triple-composition
  ( h : C -> D)
  ( g : B -> C)
  ( f : A -> B)
  : A -> D
  := \ z -> h (g (f z))

#def identity
  : A -> A
  := \ a -> a

#def constant
  ( b : B)
  : A -> B
  := \ a -> b

#end basic-functions
```

## Substitution

### Reindexing a type family along a function into the base type

```rzk
#def reindex
  ( A B : U)
  ( f : B -> A)
  ( C : A -> U)
  : B -> U
  := \ b -> C (f b)
```
