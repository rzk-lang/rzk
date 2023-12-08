# Changelog for `rzk`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## v0.7.1 — 2023-12-08

- Fix default build to include Rzk Language Server (`rzk lsp`) (see [`9b78a15`](https://github.com/rzk-lang/rzk/commit/9b78a15c750699afa93c4dab3735c2aa31e6faac));
- Apply formatting to `recId.rzk.md` example (see [`4032724`](https://github.com/rzk-lang/rzk/commit/40327246954332f40cd82c48d102bf4257ad719e));

## v0.7.0 — 2023-12-08

Major changes:

- Add an experimental `rzk format` command (by [Abdelrahman Abounegm](https://github.com/aabounegm), with feedback by [Fredrik Bakke](https://github.com/fredrik-bakke) (see [sHoTT#142](https://github.com/rzk-lang/sHoTT/pull/142)) and [Nikolai Kudasov](https://github.com/fizruk)):
  - Automatically format files, partially automating the [Code Style of the sHoTT project](https://rzk-lang.github.io/sHoTT/STYLEGUIDE/)
  - Notable features:
    - Adds a space after the opening parenthesis to help with the [code tree structure](https://rzk-lang.github.io/sHoTT/STYLEGUIDE/#the-tree-structure-of-constructions)
    - Puts the definition conclusion (type, starting with `:`) and construction (body, starting with `:=`) on new lines
    - Adds a space after the `\` of a lambda term and around binary operators (like `,`)
    - Moves binary operators to the beginning of the next line if they appear at the end of the previous one.
    - Replaces [common ASCII sequences](https://rzk-lang.github.io/sHoTT/STYLEGUIDE/#use-of-unicode-characters) with their Unicode equivalent
    - A CLI subcommand (`rzk format`) with `--check` and `--write` options
  - Known limitations
    - The 80 character line limit is currently not enforced due to the difficulty of determining where to add line breaks (for reference, check out [this post](https://journal.stuffwithstuff.com/2015/09/08/the-hardest-program-ive-ever-written/) by a Dart `fmt` engineer)
    - Fixing indentation is not yet implemented due to the need for more semantics than the lexer provides to determine indentation level.
    - There may be rare edge cases in which applying the formatter twice could result in additional edits that were not detected the first time.

Minor changes:

- Fix "latest" Rzk Playground link (see [#137](https://github.com/rzk-lang/rzk/pull/137));
- Add more badges to README (see [#136](https://github.com/rzk-lang/rzk/pull/136));

## v0.6.7 — 2023-10-07

- Fix build on some systems (fix `BNFC:bnfc` executable dependency, see [#125](https://github.com/rzk-lang/rzk/pull/125))
- Improve Rzk Playground (see [#124](https://github.com/rzk-lang/rzk/pull/124) by @deemp):
  - Add `snippet_url` parameter
  - Migrated from NextJS to Vite
  - Use `setText` on `params` attribute
- Slightly improve documentation:
  - Add links to Rzk Zulip (see [#131](https://github.com/rzk-lang/rzk/pull/131))
  - Add `cabal update` to instructions (see [`3aa8fd3`](https://github.com/rzk-lang/rzk/commit/3aa8fd38902fc8cbb29f86122410d24398a15b0b))

## v0.6.6 — 2023-10-02

- Fix builds on Windows (and macOS) (see [#121](https://github.com/rzk-lang/rzk/pull/121))

## v0.6.5 — 2023-10-01

This version contains mostly intrastructure improvements:

- Typecheck using `rzk.yaml` if it exists (see [#119](https://github.com/rzk-lang/rzk/pull/119))
- Update Rzk Playground and Nix Flake (see [#84](https://github.com/rzk-lang/rzk/pull/84))
  - Rzk Playground now uses CodeMirror 6 and NextJS
  - `miso` dependency is dropped
  - GHCJS 9.6 is now used
  - Support `snippet={code}` or `code={code}` param (see [#118](https://github.com/rzk-lang/rzk/pull/118))
    - Support for `snippet_url={URL}` is temporarily dropped
- Update to GHC 9.6, latest Stackage Nightly, improve Rzk setup, and GitHub Actions (see [#116](https://github.com/rzk-lang/rzk/pull/116))
- Add logging for Rzk Language Server (see [#114](https://github.com/rzk-lang/rzk/pull/114))

Fixes:

- Fix error messages in Rzk Playground (see [#115](https://github.com/rzk-lang/rzk/pull/115))

## v0.6.4 — 2023-09-27

This version improves the stucture of the project, in particular w.r.t dependencies:

- Add custom snapshot and explicit lower bounds (see [#108](https://github.com/rzk-lang/rzk/pull/108))

## v0.6.3 — 2023-09-27

This version contains a fix for the command line interface of `rzk`:

- Fix command line `rzk typecheck` (see [#106](https://github.com/rzk-lang/rzk/pull/106))

  - Previous version ignored failures in the command line
    (the bug was introced when allowing better autocompletion in LSP).

## v0.6.2 — 2023-09-26

This version contains some improvements in efficiency and also to the language server:

- Improve efficiency of the tope solver, applying LEM for directed interval only as a fallback option (see [#102](https://github.com/rzk-lang/rzk/pull/102))
- Support autocompleting definitions from previous modules (see [#102](https://github.com/rzk-lang/rzk/pull/103))
  - Well-typed definitions from the same module also work if the module is only partially well-typed!
- Improve information order in the error messages given in LSP diagnostics (see [#104](https://github.com/rzk-lang/rzk/pull/104))

## v0.6.1 — 2023-09-24

This version contains a minor fix:

- Catch exceptions in the parser, fixing LSP for files where layout resolver fails (see [#99](https://github.com/rzk-lang/rzk/pull/99)).

## v0.6.0 — 2023-09-23

This version introduces a proper LSP server with basic support for incremental typechecking
and some minor improvements:

1. LSP server with incremental typechecking (see [#95](https://github.com/rzk-lang/rzk/pull/95));
2. Improve error messages for unclosed `#section` and extra `#end` (see [#91](https://github.com/rzk-lang/rzk/pull/91)).

## v0.5.7 — 2023-09-21

This version contains two fixes (see [#88](https://github.com/rzk-lang/rzk/pull/88)) for issues discovered in [rzk-lang/sHoTT#30](https://github.com/rzk-lang/sHoTT/pull/30#issuecomment-1729212862):

1. We now only generate well-typed LEM instances in the tope solver, speeding up significantly.
2. We fix $\eta$-rule for product cubes, to not get stopped by reflexive equality topes like $\langle \langle \pi_1 (t_{12}), \pi_2 (t_{12}) \rangle, t_3 \rangle \equiv \langle t_{12}, t_3 \rangle$.

## v0.5.6 — 2023-09-19

This version fixes the behaviour of glob (see [`77b7cc0`](https://github.com/rzk-lang/rzk/commit/77b7cc0494fe0bfd1c9f1aa83db20f42cfda5794)).

## v0.5.5 — 2023-09-19

This version contains Unicode and tope logic-related fixes:

1. Fix (add missing checks) for subshapes (see [#85](https://github.com/rzk-lang/rzk/pull/85));
2. Allow to handle wildcards in `rzk` itself (see [#83](https://github.com/rzk-lang/rzk/pull/83));
3. Fix Unicode on machines with non-standard locales (see [#82](https://github.com/rzk-lang/rzk/pull/82));
4. Specify `happy` and `alex` as build tools to fix cabal build from Hackage (see [#80](https://github.com/rzk-lang/rzk/pull/80)).
5. Add configuration for MkDocs plugin for Rzk (see [#79](https://github.com/rzk-lang/rzk/pull/79)).

## v0.5.4 — 2023-08-19

This version contains minor improvements:

1. Improve typechecking by trying an easier unification strategy first (see [#76](https://github.com/rzk-lang/rzk/pull/76));
2. Update GitHub Action for Nix (see [#74](https://github.com/rzk-lang/rzk/pull/74)).

## v0.5.3 — 2023-07-12

This version contains a few minor improvements:

1. Allow patterns in dependent function types (see [#67](https://github.com/rzk-lang/rzk/pull/67));
2. Hint about possible shape coercions (see [#67](https://github.com/rzk-lang/rzk/pull/67));
3. Enable doctests (see [#68](https://github.com/rzk-lang/rzk/pull/68));
4. Improve documentation (add recommended installation instructions via VS Code)
5. Migrate from `fizruk` to `rzk-lang` organisation on GitHub (see [`ee0d063`](https://github.com/rzk-lang/rzk/commit/ee0d0638283232c99003a83fdf41eb109ae78983));
6. Speed up GHCJS build with Nix (see [#66](https://github.com/rzk-lang/rzk/pull/66));

## v0.5.2 — 2023-07-05

This version introduces support for Unicode syntax, better recognition of Markdown code blocks and improves documentation a bit:

- Support some Unicode syntax (see [#61](https://github.com/rzk-lang/rzk/pull/61));
- Support curly braces syntax for code blocks (see [#64](https://github.com/rzk-lang/rzk/pull/64));
- Update documentation a bit (see [07b520a6](https://github.com/rzk-lang/rzk/commit/07b520a67eb432105fad908202949c93a1639ca8) and [7cc7f383](https://github.com/rzk-lang/rzk/commit/7cc7f383b1785130277ed79d123c1dd357162d9d));
- Factor out Pygments highlighting to https://github.com/rzk-lang/pygments-rzk;
- Use new cache action for Nix (see [#60](https://github.com/rzk-lang/rzk/pull/60)).

## v0.5.1 — 2023-06-29

This version fixes `Unit` type and makes some changes to documentation:

- Fix computation for `Unit` (see [2f7d6295](https://github.com/rzk-lang/rzk/commit/2f7d6295));
- Update documentation (see [ea2d176b](https://github.com/rzk-lang/rzk/commit/ea2d176b));
- Use mike to deploy versioned docs (see [99cf721a](https://github.com/rzk-lang/rzk/commit/99cf721a));
- Replace MkDocs hook with the published plugin (see [#58](https://github.com/rzk-lang/rzk/pull/58));
- Switch to Material theme for MkDocs (see [#57](https://github.com/rzk-lang/rzk/pull/57));
- Fix links to `*.rzk.md` in `mkdocs.yml` (see [8ba1c55b](https://github.com/rzk-lang/rzk/commit/8ba1c55b));

## v0.5 — 2023-06-20

This version contains the following changes:

- `Unit` type (with `unit` value) (see [ede02611](https://github.com/rzk-lang/rzk/commit/ede02611) and [bf9d6cd9](https://github.com/rzk-lang/rzk/commit/bf9d6cd9);
- Add basic tokenizer support via `rzk tokenize` (see [#53](https://github.com/rzk-lang/rzk/pull/53));
- Add location information for shadowing warnings and duplicate definition errors (see [bf9d6cd9](https://github.com/rzk-lang/rzk/commit/bf9d6cd9)).

## v0.4.1 — 2023-06-16

This is version contains minor changes, primarily in tools around rzk:

- Add `rzk version` command (see [f1709dc7](https://github.com/rzk-lang/rzk/commit/f1709dc7));
- Add action to release binaries (see [9286afae](https://github.com/rzk-lang/rzk/commit/9286afae));
- Automate SVG rendering in MkDocs (see [#49](https://github.com/rzk-lang/rzk/pull/49));
- Read `stdin` when no filepaths are given (see [936c15a3](https://github.com/rzk-lang/rzk/commit/936c15a3));
- Add Pygments highlighting (see [01c2a017](https://github.com/rzk-lang/rzk/commit/01c2a017), [cbd656cc](https://github.com/rzk-lang/rzk/commit/cbd656cc), [5220ddf9](https://github.com/rzk-lang/rzk/commit/5220ddf9), [142ec003](https://github.com/rzk-lang/rzk/commit/142ec003), [5c7425f2](https://github.com/rzk-lang/rzk/commit/5c7425f2));
- Update HighlightJS config for rzk v0.4.0 (see [171ee63f](https://github.com/rzk-lang/rzk/commit/171ee63f));

## v0.4.0 — 2023-05-18

This version introduces sections and variables. The feature is similar to <a href="https://coq.inria.fr/refman/language/core/assumptions.html#coq:cmd.Variable" target="_blank">`Variable` command in Coq</a>. An important difference, however, is that `rzk` does not allow definitions to use variables implicitly and adds `uses (...)` annotations to ensure such dependencies are not accidental.

- Variables and sections (Coq-style) (see [#38](https://github.com/rzk-lang/rzk/pull/38));

Minor improvements:

- Add flake, set up nix and cabal builds, cache nix store on CI (see [#39](https://github.com/rzk-lang/rzk/pull/39));
- Apply stylish-haskell (see [7d42ef62](https://github.com/rzk-lang/rzk/commit/7d42ef62));

## v0.3.0 — 2023-04-28

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

## v0.2.0 - 2023-04-20

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
