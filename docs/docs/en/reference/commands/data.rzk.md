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

and an optional re-ascription clause is

```{.rzk}
eliminate with <name> : <type>
compute with <name> : <type>
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

The eliminator arguments come in the order: parameters, motive, one method per constructor (in declaration order), scrutinee. Computation is definitional: an eliminator applied to a point constructor reduces to the corresponding method applied to the constructor's fields. The [`match` expression](../match.rzk.md) is notation for the eliminators, with one branch per constructor.

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

## Path constructors

A constructor whose return type is an identity type over the declared type, spelled `#!rzk l =_{D} r`, declares a _path_: an identification between the two endpoint terms. This makes the declaration a higher inductive type in the style of the HoTT book. For example, the circle:

```rzk
#data S¹
  :=
    base
  | loop : base =_{S¹} base
```

Each path constructor contributes a method to the generated eliminators: the equation between the images of its endpoints under the section being defined, over the path for `ind-S¹` (spelled with transport through `#!rzk idJ`, since Rzk has no primitive transport). Since a path method refers to the point methods, a declaration with path constructors binds its method arguments by name (`m-base`, `m-loop`):

```rzk
#check ind-S¹
  : ( C : S¹ → U)
  → ( b : C base)
  → ( ℓ : idJ (S¹ , base , \ y _ → C base → C y , \ u → u , base , loop) b = b)
  → ( x : S¹)
  → C x

#check rec-S¹ : (C : U) → (b : C) → (ℓ : b = b) → S¹ → C
```

Computation follows the HoTT book: β stays definitional on point constructors and is _propositional_ on path constructors. Nothing reduces when an eliminator meets a path; instead the declaration generates one computation rule per path constructor and eliminator, named `compute-ind-<name>-<con>` and `compute-rec-<name>-<con>`, stating that `ap`/`apd` of the eliminator on the path equals the method.

An endpoint must be built from the declaration's constructors and the constructor's own fields (a directly recursive field is fine, and its image is the induction hypothesis). For example, the propositional truncation:

```rzk
#data trunc
  ( A : U)
  :=
    in-trunc (a : A)
  | squash (x : trunc A) (y : trunc A) : x =_{trunc A} y
```

## The re-ascription clauses

A declaration may end with re-ascription clauses: `eliminate with` names one of the two generated eliminators, `compute with` names a generated computation rule, and either gives the entry's type in the user's own spelling. The checker verifies that the spelling is definitionally equal to the canonical generated type, with the type former and the constructors in scope. Since definitionally equal types are interchangeable, the values and the computation rules are untouched; only the stored spelling changes, and it propagates wherever the type is displayed, e.g. to hover and to goals. If the given spelling is not definitionally equal to the canonical type, the error prints the canonical type.

This matters most for path constructors, whose canonical types inline transport and `ap`/`apd` through `#!rzk idJ`. With a library `transport`, `ap` and `apd` (each definable from `#!rzk idJ` before any declaration), the circle reads:

```rzk
#define transport
  ( A : U) (C : A → U) (x y : A) (p : x =_{A} y) (u : C x)
  : C y
  := idJ (A , x , \ y' _ → C x → C y' , \ v → v , y , p) u

#define ap
  ( A B : U) (f : A → B) (x y : A) (p : x =_{A} y)
  : f x =_{B} f y
  := idJ (A , x , \ y' _ → f x =_{B} f y' , refl , y , p)

#define apd
  ( A : U) (C : A → U) (f : (a : A) → C a) (x y : A) (p : x =_{A} y)
  : transport A C x y p (f x) =_{C y} f y
  := idJ (A , x , \ y' q → transport A C x y' q (f x) =_{C y'} f y' , refl , y , p)

#data circle
  :=
    pt
  | turn : pt =_{circle} pt
  eliminate with ind-circle
    : ( C : circle → U)
    → ( b : C pt)
    → ( ℓ : transport circle C pt pt turn b = b)
    → ( x : circle)
    → C x
  compute with compute-rec-circle-turn
    : ( C : U)
    → ( b : C)
    → ( ℓ : b = b)
    → ap circle C (rec-circle C b ℓ) pt pt turn = ℓ
  compute with compute-ind-circle-turn
    : ( C : circle → U)
    → ( b : C pt)
    → ( ℓ : transport circle C pt pt turn b = b)
    → apd circle C (ind-circle C b ℓ) pt pt turn = ℓ
```

The clauses are equally available on declarations without path constructors, e.g. to spell an eliminator's type through an unfolding synonym.

## Current restrictions

At the moment:

- recursive fields must be direct: a positive function-typed field such as `#!rzk node (f : A → tree)` (the W-type shape) is not supported yet;
- parameters and indices must be types: a cube point (`#!rzk (t : I)`) or a shape point, in any spelling, is rejected. The rule is checked on the elaborated sort, so the named spelling `#!rzk (t : Δ¹)` meets it too. Parameters of type `#!rzk CUBE` or `#!rzk I → TOPE` range over cubes and shapes themselves, not over their points, and are fine — see the generic realisation below. A family varying over a shape is what extension types are for;
- a modal *type* field (`#!rzk (x :_b A)`) is not supported yet: a type field can be recursive, and the recursion and positivity bookkeeping does not see through a modal binder. A modal *shape* field (`#!rzk (t :_b I | φ)`) is fine — a shape field cannot be recursive — and the eliminator binds it with its modality, so the lock discipline of [modalities](../modalities.rzk.md) applies inside a branch;
- path constructors are not supported in indexed families, and only paths between points are supported: an identity carrier or an identity-typed field (a higher path, as in the 0-truncation) is rejected.

## Shape fields

A constructor may take a *shape* point. The declared type is then the shape realisation of that shape: it is freely generated by a Φ-indexed family of points, with no identifications between them.

A shape is a predicate on a cube, so it is spelled `#!rzk I → TOPE`, just as `#!rzk Δ¹` and `#!rzk Λ²₁` are. The general realisation is therefore

```rzk
#data Shape (I : CUBE) (phi : I → TOPE) := point (t : phi)
```

and mapping out of it is giving an extension-type section:

```{.rzk}
rec-Shape : (I : CUBE) → (phi : I → TOPE) → (C : U)
          → ((t : I | phi t) → C) → Shape I phi → C
```

Computation is strict on a syntactic point, `#!rzk rec-Shape I phi C m (point I phi p) ≡ m p`.

A field may equally be written with the shape inline, `#!rzk (t : I | φ)`, or with a bare cube, `#!rzk (t : I)`, which is the shape whose tope is `#!rzk ⊤`:

```rzk
#data Arr := seg (t : 2 | ⊤)
```

Matching binds the cube variable and brings the tope into the branch, so a shape field may be followed by fields that mention it:

```rzk
#data Total (A : (t : 2) → U) := tot (t : 2 | ⊤) (a : A t)
```

which is the total space of a family over the shape, and has no other spelling.

The construction and its metatheory are due to Kudasov, [*Booleans, coproducts and shape types in type theory for synthetic ∞-categories*](https://fizruk.github.io/files/%5Bnotes%5D%20N.Kudasov.%20Booleans%2C%20coproducts%20and%20shape%20types%20in%20type%20theory%20for%20synthetic%20%E2%88%9E-categories.pdf), §4 "Shapes as types".

One thing this deliberately does **not** provide: there is no way to give a field *face equations*, which is what would declare a directed cell such as the directed circle. The eliminator for those needs a Segal or covariant target, and the metatheory is open.

Note also that an inductive type comes with exactly its induction principle; how the type interacts with the simplicial structure is a separate matter. See the discreteness caveat in [Dependent types](../../getting-started/dependent-types.rzk.md#booleans).
