# Changelog for `rzk`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

This version was a complete rewrite of the proof assistant, using a new parser, a new internal representation, and a rewrite of the typechecking logic. This is still a prototype, but, arguably, more stable and manageable than version 0.1.0.

### Changes in syntax

Syntax is almost entirely backwards compatible with version 0.1.0.

The only known breaking changes are:

1. Terms like `second x y` which previous have been parsed as `second (x y)`
  now are properly parsed as `(second x) y`.
2. It is now necessary to have at least a minimal indentation in the definition of a term after a newline.
3. Unicode syntax is temporarily disabled, except for dependent sums and arrows in function types.

Otherwise, syntax is now made more flexible:

1. Function parameters can be unnamed: `A -> B` is the same as `(_ : A) -> B`.
2. Angle brackets are now optional: `{t : I | psi t} -> A t [ phi t |-> a t ]`
3. Nullary extension types are possible: `A t [ phi t |-> a t ]`
4. Lambda abstractions can introduce multiple arguments:

```rzk
#def hom : (A : U) -> A -> A -> U
  := \A x y ->
    (t : Δ¹) -> A [ ∂Δ¹ t |-> recOR(t === 0_2, t === 1_2, x, y) ]
```

5. There are now 3 syntactic versions of `refl` with different amount of explicit annotations:
  `refl`, `refl_{x}` and `refl_{x : A}`

6. There are now 2 syntactic versions of identity types (`=`): `x = y` and `x =_{A} y`.

7. `recOR` now supports alternative syntax with an arbitrary number of subshapes:
  `recOR( tope1 |-> term1, tope2 |-> term2, ..., topeN |-> termN )`

8. Now it is possible to have type ascriptions: `t as T`. This can help with ensuring types of subexpressions in parts of formalisations.

9. 

### Simple shape coercions

In some places, shapes (cube indexed tope families) can be used directly:

1. In function parameters: `(Λ -> A) -> (Δ² -> A)` is the same as `({(t, s) : 2 * 2 | Λ (t, s)} -> A) -> ({(t, s) : 2 * 2 | Δ²} -> A)`

2. In parameter types of lambda abstractions: `\((t, s) : Δ²) -> ...` is the same as `\{(t, s) : 2 * 2 | Δ² (t, s)} -> ...`

### Better type inference

1. It is now not required to annotate point variables with tope restrictions, the typechecker is finally smart enough to figure them out from the context.

2. It is now possible to simply write `refl` in most situations.

3. It is now possible to omit the index type in an identity type: `x = y`

### Better output and error message

The output and error messages have been slightly improved, but not in a major way.

### Better internal representation

A new internal representation (a version of second-order abstract syntax)
allows to stop worrying about name captures in substitutions,
so the implementation is much more trustworthy.
The new representation will also allow to bring in higher-order unification in the future, for better type inference, matching, etc.

New representation also allowed annotating each (sub)term with its type to avoid recomputations and some other minor speedups. There are still some performance issues, which need to be debugged, but overall it is much faster than version 0.1.0 already.

## 0.1.0 - YYYY-MM-DD
