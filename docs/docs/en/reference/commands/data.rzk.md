# `#data` command

The `#data` command declares an inductive type: the type former, its constructors, and the generated eliminators.

## Syntax

```{.rzk}
#data <name> [uses (<vars>)] (<param>)* := <constructor> [| <constructor>]*
#data <name> [uses (<vars>)] (<param>)*
```

where a constructor is

```{.rzk}
<name> (<field>)*
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

The eliminator arguments come in the order: parameters, motive, one method per constructor (in declaration order), scrutinee. Computation is definitional: an eliminator applied to a constructor reduces to the corresponding method applied to the constructor's fields.

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

## Current restrictions

At the moment:

- recursive fields must be direct: a positive function-typed field such as `#!rzk node (f : A → tree)` (the W-type shape) is not supported yet;
- the sort must be `#!rzk U` and a constructor's return type, when spelled out, must be the declared type applied to its parameters (indexed families are planned);
- constructors may not take cube or shape arguments (over the directed interval they would declare directed cells);
- the `eliminator` re-ascription clause is parsed but not yet supported.

Note also that an inductive type comes with exactly its induction principle; how the type interacts with the simplicial structure is a separate matter. See the discreteness caveat in [Dependent types](../../getting-started/dependent-types.rzk.md#booleans).
