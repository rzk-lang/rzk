# Hurkens's paradox

This example documents a known consistency limitation of Rzk. It constructs a term of `(A : U) → A` using type-in-type (`U : U`), without postulates, holes or additional type formers. The argument is due to Hurkens[^hurkens]; this version adapts the Agda formalisation[^agda] with eta-expansions to satisfy Rzk's schematic saturation checks.

## Reproducing the limitation

Run this file explicitly from the repository root:

```sh
rzk typecheck --rstt-safe=error --json docs/docs/en/reference/limitations/hurkens.rzk.md
```

At [revision `e7190253`](https://github.com/rzk-lang/rzk/commit/e7190253), the command exits successfully with no diagnostics:

```json
[]
```

The file is excluded from the default `rzk.yaml` include lists. Its successful checking records a limitation of that revision; future versions may reject the proof as their consistency checks improve. Typechecking is enough to reproduce the limitation. There is no need to evaluate an application of `absurd`.

## The construction

We encode falsehood as the ability to produce an element of every type. `P A` is the type of predicates on `A`. The polymorphic type `V` corresponds to the universe constructed in §3 of Hurkens's paper.

```rzk
#lang rzk-1

#def Bottom : U := (A : U) → A
#def Not (A : U) : U := A → Bottom
#def P (A : U) : U := A → U
#def V : U := (X : U) → (P (P X) → X) → P (P X)
```

The maps `tau` and `sigma` connect `V` with its double predicate type. In particular, `sigma` instantiates an element `s : V` at `V` itself.

```rzk
#def tau (t : P (P V)) : V :=
  \ X f p → t (\ x → p (f (x X f)))

#def sigma (s : V) : P (P V) :=
  s V (\ t X f q → tau t X f q)
```

Hurkens's diagonal construction gives a predicate `delta`, an element `omega`, and a proposition `D` for which we will obtain both a proof and a refutation.

```rzk
#def delta : P V :=
  \ y → Not ((p : P V) → sigma y p → p (\ X f q → tau (sigma y) X f q))

#def omega : V :=
  tau (\ p → (x : V) → sigma x p → p x)

#def D : U :=
  (p : P V) → sigma omega p → p (\ X f q → tau (sigma omega) X f q)
```

The first lemma supplies the common argument used to construct `lemma2 : Not D` and `lemma3 : D`. Applying the refutation to the proof gives the contradiction.

```rzk
#def lemma1 (p : P V) (h : (x : V) → sigma x p → p x)
  : p (\ X f q → omega X f q) :=
  h (\ X f q → omega X f q) (\ x → h (\ X f q → tau (sigma x) X f q))

#def lemma2 : Not D :=
  lemma1 delta
    (\ x h2 h3 → h3 (\ y → delta y) h2
      (\ p → h3 (\ y → p (\ X f q → tau (sigma y) X f q))))

#def lemma3 : D :=
  \ p → lemma1 (\ y → p (\ X f q → tau (sigma y) X f q))

#def absurd (A : U) : A := lemma2 lemma3 A
```

## Why the fragment checks accept it

The construction uses only universes and dependent functions. There are no shape dependencies, free-standing restrictions, or excluded extensions. Eta-expanding the schematic uses of `tau`, `omega` and `delta` satisfies the saturation check.

The remaining problem is the universe discipline of the meta-theoretic parameter layer. Type-in-type permits the self-instantiation used by the construction, while the fragment checks do not enforce universe levels. Passing [RSTT-safe checks](../commands/options.rzk.md#rstt-safe) therefore does not establish consistency.

[^hurkens]: Antonius J. C. Hurkens. *A simplification of Girard's paradox.* TLCA 1995, LNCS 902, pp. 266–278. 1995. <https://doi.org/10.1007/BFb0014058> — §3 gives the proof term; §7 analyses its reduction behaviour.
[^agda]: The Agda developers. *Hurkens.agda*. <https://github.com/agda/agda/blob/cf715b08903b750e4bd13ba272a1aa5ab4747af8/test/Succeed/Hurkens.agda> — source of this adaptation; the applicable [Agda licence notice](hurkens-agda-LICENSE.txt) is retained alongside this file.
