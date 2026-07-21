# `#data` command

The `#data` command declares an inductive type: the type former, its constructors, and the generated eliminators.

## Syntax

```{.rzk}
#data <name> [uses (<vars>)] (<param>)* := <constructor> [| <constructor>]* (<elim-clause>)*
#data <name> [uses (<vars>)] (<param>)*
```

where a constructor is

```{.rzk}
<name> (<field>)*
```

and an optional eliminator clause is

```{.rzk}
eliminator <name> : <type>
```

## Description

The declaration introduces the type former, one ordinary top-level name per constructor, and two generated eliminators: the induction principle `ind-<name>` and its non-dependent version `rec-<name>`. All of them live in the same namespace as every other top-level name, so a clash with an existing name is an error. The second form (no `:=`) declares the empty family.

For example:

```rzk
#lang rzk-1

#data bool := false | true

#data coprod
  ( A B : U)
  :=
    inl (a : A)
  | inr (b : B)
```

generates

```rzk
#check ind-bool : (C : bool → U) → C false → C true → (b : bool) → C b
#check rec-coprod
  : ( A : U) → (B : U) → (C : U)
  → ( ( a : A) → C) → ((b : B) → C)
  → coprod A B → C
```

The eliminator arguments come in the order: parameters, motive, one method per constructor (in declaration order), scrutinee. Computation is definitional: an eliminator applied to a constructor reduces to the corresponding method applied to the constructor's fields. The [`match` expression](../match.rzk.md) is notation for the induction principle, with one branch per constructor.

!!! warning "Large inductive types"

    A constructor field whose type is or quantifies over a universe (e.g. `#!rzk box (X : U)`) makes the type _large_. Since Rzk currently has `#!rzk U : U`, a large inductive type is a known shortcut to inconsistency, so the declaration is accepted with a warning.

Recursion is supported for _directly_ recursive fields, i.e. fields whose type is the declared type applied to its parameters, such as `#!rzk suc (n : nat)`. Each recursive field contributes an induction hypothesis to the eliminator's method, right after the field:

```rzk
#data nat := zero | suc (n : nat)

#check ind-nat
  : ( C : nat → U)
  → C zero
  → ( ( n : nat) → C n → C (suc n))
  → ( x : nat) → C x
```

Constructor fields must be strictly positive in the declared type.

Indexed families spell their index telescope in the sort. A constructor of an indexed family must spell out its return type, which instantiates the indices; a directly recursive field does the same, and its indices instantiate the induction hypothesis:

```rzk
#data vec
  ( A : U)
  : nat → U
  :=
    nil : vec A zero
  | cons (n : nat) (x : A) (xs : vec A n) : vec A (suc n)

#check ind-vec
  : ( A : U)
  → ( C : (n : nat) → vec A n → U)
  → C zero (nil A)
  → ( ( n : nat) → (x : A) → (xs : vec A n) → C n xs → C (suc n) (cons A n x xs))
  → ( n : nat) → (xs : vec A n) → C n xs
```

The parameters (before the sort) are uniform: every constructor returns the declared type applied to exactly the parameter variables, followed by its index terms.

## The `eliminator` clause

A declaration may end with `eliminator` clauses, at most one per generated eliminator. A clause re-ascribes the named eliminator's type with the user's own spelling. The checker verifies that the spelling is definitionally equal to the canonical generated type, with the type former and the constructors in scope. Since definitionally equal types are interchangeable, the values and the computation rules are untouched; only the stored spelling changes, and it propagates wherever the type is displayed, e.g. to hover and to goals.

For example, with an unfolding synonym defined beforehand:

```rzk
#define branch (C : U)
  : U
  := C

#data direction := down | up
  eliminator rec-direction : (C : U) → branch C → branch C → direction → C

#check rec-direction : (C : U) → C → C → direction → C
```

A clause must name one of the two generated eliminators of the declaration. If the given spelling is not definitionally equal to the canonical type, the error prints the canonical type.

## Current restrictions

At the moment:

- recursive fields must be direct: a positive function-typed field such as `#!rzk node (f : A → tree)` (the W-type shape) is not supported yet;
- indices must be plain types (no cube or shape indices), and constructors may not take cube or shape arguments (over the directed interval they would declare directed cells).

Note also that an inductive type comes with exactly its induction principle; how the type interacts with the simplicial structure is a separate matter. See the discreteness caveat in [Dependent types](../../getting-started/dependent-types.rzk.md#booleans).
