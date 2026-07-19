# Pattern matching

The `match` expression eliminates a value of an inductive type declared with [`#data`](commands/data.rzk.md), with one branch per constructor. It is notation for the generated induction principle: typechecking elaborates every `match` into an application of `ind-<name>`, so computation and termination behave exactly as they do for the eliminator.

```rzk
#lang rzk-1

#data nat := zero | suc (n : nat)
```

A branch names a constructor and binds one variable per *method argument*: the constructor's fields, then one induction hypothesis per recursive field, in order. The branch arrow is `⇒` (ASCII `=>`).

```rzk
#define plus
  ( n m : nat)
  : nat
  := match n
      ( zero ⇒ m
      | suc k ih ⇒ suc ih)
```

Here `ih` stands for `plus k m`, the result of the recursion on `k`. Recursion happens only through the induction hypotheses, so every `match` terminates by construction. Computation is definitional:

```rzk
#define plus-two-two
  : plus (suc (suc zero)) (suc (suc zero)) =_{nat} suc (suc (suc (suc zero)))
  := refl
```

Branches must be in bijection with the constructors: every constructor appears exactly once (in any order), with exactly as many binders as its method takes. There are no nested patterns and no wildcards.

## The motive

A `match` used in checking position takes its motive from the expected type: when the scrutinee is a variable, that variable is abstracted out of the goal, so the branches see the goal at each constructor. This gives dependent matching by substitution:

```rzk
#define plus-zero
  ( n : nat)
  : plus zero n =_{nat} n
  := refl

#define zero-plus
  ( n : nat)
  : plus n zero =_{nat} n
  := match n
      ( zero ⇒ refl
      | suc k ih ⇒
          idJ
            ( nat , plus k zero
            , \ z q → suc (plus k zero) =_{nat} suc z
            , refl , k , ih))
```

In the `suc` branch the goal is `plus (suc k) zero =_{nat} suc k` and the hypothesis `ih : plus k zero =_{nat} k` is available; path induction over `ih` turns it into the congruence for `suc` that finishes the proof.

When the scrutinee is not a variable, the motive is constant: the goal must not depend on the scrutinee. Finally, an explicit motive can be written after `into`; it is the *family* the match eliminates into, applied to the indices and the scrutinee:

```rzk
#define plus'
  ( n m : nat)
  : nat
  := match n into (\ _ → nat)
      ( zero ⇒ m
      | suc k ih ⇒ suc ih)
```

A `match` without `into` is only accepted where its type is already known; in inference position (e.g. under `#compute`), write the motive explicitly.

## The convoy pattern

To let a branch use a hypothesis *at the refined scrutinee*, make the motive a function type and apply the match to the hypothesis afterwards (the convoy pattern of Coq folklore). For example, with

```rzk
#data bool := false | true

#define not
  ( b : bool)
  : bool
  := match b
      ( false ⇒ true
      | true ⇒ false)
```

a proof of `C (not (not b))` from `h : C b` cannot match on `b` directly: in the `true` branch the goal becomes `C (not (not true))`, but `h` still has type `C b` for the un-refined `b`. Threading `h` through the motive refines both at once:

```rzk
#define convoy
  ( C : bool → U)
  ( b : bool)
  ( h : C b)
  : C (not (not b))
  := (match b into (\ b' → C b' → C (not (not b')))
        ( false ⇒ \ h' → h'
        | true ⇒ \ h' → h')) h
```

In each branch the argument `h'` has type `C` at the constructor, and `C (not (not true))` computes to `C true`, so the identity suffices.

## Indexed families

Matching on a value of an indexed family works the same way; an `into` motive then abstracts the indices before the scrutinee:

```rzk
#data vec
  ( A : U)
  : nat → U
  :=
    nil : vec A zero
  | cons (n : nat) (x : A) (xs : vec A n) : vec A (suc n)

#define vlen
  ( A : U)
  ( n : nat)
  ( xs : vec A n)
  : nat
  := match xs
      ( nil ⇒ zero
      | cons k x tail ih ⇒ suc ih)
```

The motive may use the indices. For instance, the safe head on `vec A (suc n)` computes its motive by a nested match on the index, so the `nil` branch is asked for a `Unit` and the `cons` branch for an `A`:

```rzk
#define vhead
  ( A : U)
  ( n : nat)
  ( xs : vec A (suc n))
  : A
  := match xs into (\ k v → match k (zero ⇒ Unit | suc j jh ⇒ A))
      ( nil ⇒ unit
      | cons k x tail ih ⇒ x)
```

When the goal depends on the indices of a variable scrutinee, the built motive keeps the indices fixed, which is usually not what the induction needs; write the dependent motive with `into` in that case.
