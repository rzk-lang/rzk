# Tope disjuction elimination along identity paths

\(\mathsf{rec}_{\lor}^{\ψ,\φ}(a_\ψ, a*\φ)\) (written `recOR(ψ, φ, a_psi, a_phi)` in the code)
is well-typed when \(a*\ψ\) and \(a*\φ\) are \_definitionally equal* on \(\ψ \land \φ\).
Sometimes this is too strong since many terms are not _definitionally_ equal, but only equal up to a path.
Luckily, assuming relative function extensionality, we can define a weaker version of \(rec*{\lor}\) (`recOR`), which we call `recId`, that can work in presence of a witness of type \(\prod*{t : I \mid \ψ \land \φ} a*\ψ = a*\φ\).

## Prerequisites

This file relies on some definitions, defined in

- [Getting Started > Dependent Types](../getting-started/dependent-types.rzk.md)

We begin by introducing common HoTT definitions:

```rzk
#lang rzk-1

-- A is contractible there exists x : A such that for any y : A we have x = y.
#define iscontr (A : U)
  : U
  := Σ ( a : A) , (x : A) → a =_{A} x

-- A is a proposition if for any x, y : A we have x = y
#define isaprop (A : U)
  : U
  := ( x : A) → (y : A) → x =_{A} y

-- A is a set if for any x, y : A the type x =_{A} y is a proposition
#define isaset (A : U)
  : U
  := ( x : A) → (y : A) → isaprop (x =_{A} y)

-- A function f : A → B is an equivalence
-- if there exists g : B → A
-- such that for all x : A we have g (f x) = x
-- and for all y : B we have f (g y) = y
#define isweq (A : U) (B : U) (f : A → B)
  : U
  := Σ ( g : B → A)
  , prod
    ( ( x : A) → g (f x) =_{A} x)
    ( ( y : B) → f (g y) =_{B} y)

-- Equivalence of types A and B
#define weq (A : U) (B : U)
  : U
  := Σ ( f : A → B)
  , isweq A B f

-- Transport along a path
#define transport
  ( A : U)
  ( C : A → U)
  ( x y : A)
  ( p : x =_{A} y)
  : C x → C y
  := \ cx → idJ(A , x , (\ z q → C z) , cx , y , p)
```

## Relative function extensionality

We can now define relative function extensionality. There are several formulations, we provide two, following Riehl and Shulman:

```rzk
-- [RS17, Axiom 4.6] Relative function extensionality.
#define relfunext
  : U
  := ( I : CUBE)
  → ( ψ : I → TOPE)
  → ( φ : ψ → TOPE)
  → ( A : ψ → U)
  → ( ( t : ψ) → iscontr (A t))
  → ( a : ( t : φ) → A t)
  → ( t : ψ) → A t [ φ t ↦ a t]

-- [RS17, Proposition 4.8] A (weaker) formulation of function extensionality.
#define relfunext2
  : U
  :=
    ( I : CUBE)
  → ( ψ : I → TOPE)
  → ( φ : ψ → TOPE)
  → ( A : ψ → U)
  → ( a : ( t : φ) → A t)
  → ( f : (t : ψ) → A t [ φ t ↦ a t ])
  → ( g : ( t : ψ) → A t [ φ t ↦ a t ])
  → weq
    ( f = g)
    ( ( t : ψ) → (f t =_{A t} g t) [ φ t ↦ refl ])
```

## Construction of `recId`

The idea is straightforward. We ask for a proof that `a = b` for all points in `ψ ∧ φ`. Then, by relative function extensionality (`relfunext2`), we can show that restrictions of `a` and `b` to `ψ ∧ φ` are equal. If we reformulate `a` as extension of its restriction, then we can `transport` such reformulation along the path connecting two restrictions and apply `recOR`.

First, we define how to restrict an extension type to a subshape:

```rzk
#section construction-of-recId

#variable r : relfunext2
#variable I : CUBE
#variables ψ φ : I → TOPE
#variable A : (t : I | ψ t ∨ φ t) → U

-- Restrict extension type to a subshape.
#define restrict_phi
  ( a : ( t : φ) → A t)
  : ( t : I | ψ t ∧ φ t) → A t
  := \ t → a t

-- Restrict extension type to a subshape.
#define restrict_psi
  ( a : ( t : ψ) → A t)
  : ( t : I | ψ t ∧ φ t) → A t
  := \ t → a t
```

Then, how to reformulate an `a` (or `b`) as an extension of its restriction:

```rzk
-- Reformulate extension type as an extension of a restriction.
#define ext-of-restrict_psi
  ( a : ( t : ψ) → A t)
  : ( t : ψ)
  → A t [ ψ t ∧ φ t ↦ restrict_psi a t ]
  := a  -- type is coerced automatically here

-- Reformulate extension type as an extension of a restriction.
#define ext-of-restrict_phi
  ( a : ( t : φ) → A t)
  : ( t : φ)
  → A t [ ψ t ∧ φ t ↦ restrict_phi a t ]
  := a  -- type is coerced automatically here
```

Now, assuming relative function extensionality, we construct a path between restrictions:

```rzk
-- Transform extension of an identity into an identity of restrictions.
#define restricts-path
  ( a_psi : (t : ψ) → A t)
  ( a_phi : (t : φ) → A t)
  : ( e : (t : I | ψ t ∧ φ t) → a_psi t = a_phi t)
  → restrict_psi a_psi = restrict_phi a_phi
  :=
  first
  ( second
    ( r I
      ( \ t → ψ t ∧ φ t)
      ( \ t → BOT)
      ( \ t → A t)
      ( \ t → recBOT)
      ( \ t → a_psi t)
      ( \ t → a_phi t)))
```

Finally, we bring everything together into `recId`:

```rzk
-- A weaker version of recOR, demanding only a path between a and b:
-- recOR(ψ, φ, a, b) demands that for ψ ∧ φ we have a == b (definitionally)
-- (recId ψ φ a b e) demands that e is the proof that a = b (intensionally) for ψ ∧ φ
#define recId uses (r) -- we declare that recId is using r on purpose
  ( a_psi : (t : ψ) → A t)
  ( a_phi : (t : φ) → A t)
  ( e : (t : I | ψ t ∧ φ t) → a_psi t = a_phi t)
  : ( t : I | ψ t ∨ φ t) → A t
  := \ t → recOR(
        ψ t ↦
          transport
          ( ( s : I | ψ s ∧ φ s) → A s)
          ( \ ra → (s : ψ) → A s [ ψ s ∧ φ s ↦ ra s ])
          ( restrict_psi a_psi)
          ( restrict_phi a_phi)
          ( restricts-path a_psi a_phi e)
          ( ext-of-restrict_psi a_psi)
          ( t)
      , φ t ↦
          ext-of-restrict_phi a_phi t
      )

#end construction-of-recId
```

## Gluing extension types

An application of of `recId` is gluing together extension types,
whenever we can show that they are equal on the intersection of shapes:

```rzk
-- If two extension types are equal along two subshapes,
-- then they are also equal along their union.
#define id-along-border
  ( r : relfunext2)
  ( I : CUBE)
  ( ψ : I → TOPE)
  ( φ : I → TOPE)
  ( A : (t : I | ψ t ∨ φ t) → U)
  ( a b : (t : I | ψ t ∨ φ t) → A t)
  ( e_psi : (t : ψ) → a t = b t)
  ( e_phi : (t : φ) → a t = b t)
  ( border-is-a-set : (t : I | ψ t ∧ φ t) → isaset (A t))
  : ( t : I | ψ t ∨ φ t) → a t = b t
  :=
  recId r I ψ φ
  ( \ t → a t = b t)
  ( e_psi)
  ( e_phi)
  ( \ t → border-is-a-set t (a t) (b t) (e_psi t) (e_phi t))
```
