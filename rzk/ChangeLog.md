# Changelog for `rzk`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## v0.4.0 — 2022-05-18

This version introduces sections and variables. The feature is similar to <a href="https://coq.inria.fr/refman/language/core/assumptions.html#coq:cmd.Variable" target="_blank">`Variable` command in Coq</a>. An important difference, however, is that `rzk` does not allow definitions to use variables implicitly and adds `uses (...)` annotations to ensure such dependencies are not accidental.

- Variables and sections (Coq-style) (see [#38]( https://github.com/fizruk/rzk/pull/38 ));

Minor improvements:

- Add flake, set up nix and cabal builds, cache nix store on CI (see [#39]( https://github.com/fizruk/rzk/pull/39 ));
- Apply stylish-haskell (see [7d42ef62]( https://github.com/fizruk/rzk/commit/7d42ef62 ));

## v0.3.0 — 2022-04-28

This version introduces an experimental feature for generating visualisations for simplicial terms in SVG.
To enable rendering, enable option `"render" = "svg"` (to disable, `"render" = "none"`):

```rzk
#set-option "render" = "svg"  -- enable rendering in SVG
```

Minor changes:

- Exit with non-zero code upon a type error (see b135c4fb)
- Fix external links and some typos in the documentation

Fixes:

- Fixed an issue with tope solver when context was empty (see 6196af9e);
- Fixed #33 (missing coherence check for restricted types).

## v0.2.0 - 2022-04-20

This version was a complete rewrite of the proof assistant, using a new parser, a new internal representation, and a rewrite of the typechecking logic. This is still a prototype, but, arguably, significantly more stable and manageable than version 0.1.0.

### Language

Syntax is almost entirely backwards compatible with version 0.1.0.
Typechecking has been fixed and improved.

#### Breaking Changes

The only known breaking changes are:

1. Terms like `second x y` which previous have been parsed as `second (x y)`
  now are properly parsed as `(second x) y`.
2. It is now necessary to have at least a minimal indentation in the definition of a term after a newline.
3. Unicode syntax is temporarily disabled, except for dependent sums and arrows in function types.
4. The restriction syntax `[ ... ]` now has a slightly different precedence, so some parentheses are required, e.g. in `(A -> B) [ phi |-> f]` or `(f t = g t) [ phi |-> f]`.
5. Duplicate top-level definitions are no longer allowed.

#### Deprecated Syntax

The angle brackets for extension types are supported, but deprecated,
as they are completely unnecessary now: `<{t : I | psi t} -> A t [ phi t |-> a t ]>` can now be written as `{t : I | psi t} -> A t [ phi t |-> a t]`
or even `(t : psi) -> A t [ phi t |-> a t ]`.

#### Syntax Relaxation

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

5. Parameters can be introduced simultaneously for the type and body. Moreover, multiple parameters can be introduced with the same type:

    ```rzk
    #def hom (A : U) (x y : A) : U
      := (t : Δ¹) -> A [ ∂Δ¹ t |-> recOR(t === 0_2, t === 1_2, x, y) ]
    ```

6. Restrictions can now support multiple subshapes, effectively internalising `recOR`:

    ```rzk
    #def hom (A : U) (x y : A) : U
      := (t : Δ¹) -> A [ t === 0_2 |-> x, t === 1_2 |-> y ]
    ```

7. There are now 3 syntactic versions of `refl` with different amount of explicit annotations:
  `refl`, `refl_{x}` and `refl_{x : A}`

8. There are now 2 syntactic versions of identity types (`=`): `x = y` and `x =_{A} y`.

9. `recOR` now supports alternative syntax with an arbitrary number of subshapes:
  `recOR( tope1 |-> term1, tope2 |-> term2, ..., topeN |-> termN )`

10. Now it is possible to have type ascriptions: `t as T`. This can help with ensuring types of subexpressions in parts of formalisations, or to upcast types.

11. New (better) commands are now supported:

    1. `#define <name> (<param>)* : <type> := <term>` — same as `#def`, but with full spelling of the word
    2. `#postulate <name> (<param>)* : <type>` — postulate an axiom
    3. `#check <term> : <type>` — typecheck an expression against a given type
    4. `#compute-whnf <term>` — compute (WHNF) of a term
    5. `#compute-nf <term>` — compute normal form of a term
    6. `#compute <term>` — alias for `#compute-whnf`
    7. `#set-option <option> = <value>` — set a (typechecker) option:
    
        - `#set-option "verbosity" = "silent"` — no log printing
        - `#set-option "verbosity" = "normal"` — log typechecking progress
        - `#set-option "verbosity" = "debug"` — log every intermediate action
          (may be useful to debug when some definition does not typecheck)

    8. `#unset-option <option>` — revert option's value to its default

#### Simple Shape Coercions

In some places, shapes (cube indexed tope families) can be used directly:

1. In function parameters: `(Λ -> A) -> (Δ² -> A)` is the same as `({(t, s) : 2 * 2 | Λ (t, s)} -> A) -> ({(t, s) : 2 * 2 | Δ²} -> A)`

2. In parameter types of lambda abstractions: `\((t, s) : Δ²) -> ...` is the same as `\{(t, s) : 2 * 2 | Δ² (t, s)} -> ...`

#### Better Type Inference

1. It is now not required to annotate point variables with tope restrictions, the typechecker is finally smart enough to figure them out from the context.

2. It is now possible to simply write `refl` in most situations.

3. It is now possible to omit the index type in an identity type: `x = y`

### Better output and error message

The output and error messages have been slightly improved, but not in a major way.

### Internal representation

A new internal representation (a version of second-order abstract syntax)
allows to stop worrying about name captures in substitutions,
so the implementation is much more trustworthy.
The new representation will also allow to bring in higher-order unification in the future, for better type inference, matching, etc.

New representation also allowed annotating each (sub)term with its type to avoid recomputations and some other minor speedups. There are still some performance issues, which need to be debugged, but overall it is much faster than version 0.1.0 already.
