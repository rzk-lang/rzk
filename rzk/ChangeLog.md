# Changelog for `rzk`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

Added:

- **Language server: cross-file references, hover, symbols, and better highlighting** (see [#282](https://github.com/rzk-lang/rzk/pull/282)). Go-to-definition and find-references work across the files of a project. Hover shows a definition's elaborated type (falling back to the written annotation), rendered as an rzk code block; long signatures are split across lines, pair binders are split through weak head normalisation, and shaped binders are shown with their tope. Document symbols populate the outline view. Semantic tokens are computed from the lexer token stream, and a per-file parse cache keeps the last good parse across syntax errors, so highlighting, hover, and references keep working while a file is temporarily broken. No grammar changes are involved. A matching `vscode-rzk` release should be pinned to this version, so editor users actually receive the new UX.

- **Language server: typecheck progress reporting and a responsive server** (see [#283](https://github.com/rzk-lang/rzk/pull/283)). Typechecking now reports its progress to the editor (via `$/progress`), showing the file being checked and the fraction of modules done. The project re-check runs on a worker thread, so requests such as formatting on save are answered while the re-check is still running; a newer change cancels and restarts the re-check, keeping the modules it has finished. Checking stops at the first module with errors; the modules a run never reaches are now marked with a warning naming the blocker (e.g. `Not checked: blocked by an error in …`) instead of keeping stale diagnostics.

- **Language server: workspace symbol search** (see [#285](https://github.com/rzk-lang/rzk/pull/285)). Every definition of the project is searchable by name. In VS Code, press `Ctrl+T` (`⌘T` on macOS, "Go to Symbol in Workspace…"), or type `#` followed by the name in Quick Open, e.g. `#yoneda`; Enter jumps to the definition. Matching is case-insensitive and by infix, and the results span all files of the project, not just the open one (that remains the job of document symbols, `Ctrl+Shift+O`). Definitions are served from the typecheck cache, so a module becomes searchable once it has been checked.

- **Language server: holes are highlighted** (see [#284](https://github.com/rzk-lang/rzk/pull/284)). A hole (`?` or `?name`) now gets its own semantic token. It is computed from the lexer token stream, so holes stay highlighted while the file temporarily fails to parse.

Changed:

- Function types no longer show unused anonymous binders (see [#286](https://github.com/rzk-lang/rzk/pull/286)). A goal `#!rzk U → U` used to print as `#!rzk (x₁ : U) → U`, with a machine-generated name; the domain is now rendered as a plain parameter everywhere a type is shown (hover, hole goals, error messages). A user-written name is kept even when unused.

Fixed:

- Subtype checks now respect variance. Three asymmetric checks (tope-family domains, the domain topes of Π-types, and restriction faces) ran in a fixed direction regardless of the ambient variance, so in negative positions they ran backwards. For example, `#!rzk k : (f : (t : 2 | ⊤) → A) → A` was accepted where `#!rzk (f : (t : 2 | t ≡ 0₂) → A) → A` is expected, letting `#!rzk k` apply a partial `#!rzk f` outside its domain. Each check now consults the variance flag; six regression fixtures are added, and the sHoTT corpus is unaffected (see [#269](https://github.com/rzk-lang/rzk/pull/269)).

- Unification no longer equates identity types over different types. Identity types were equated whenever their endpoints unified, accepting e.g. a free homotopy (a path in `#!rzk Δ¹ → A`) where an endpoint-fixing one (a path in `#!rzk hom A x y`) is expected. The underlying types are now compared, invariantly, so subtyping does not leak into equality of identity types (see [#270](https://github.com/rzk-lang/rzk/pull/270)).

Performance:

- A performance pass over the typechecker roughly halves the wall-clock time of a full sHoTT check (26–28 s down to ~13.5 s) and cuts its maximum residency fourfold (8 GB down to 2.1 GB):

  - the tope solver short-circuits its Boolean search, maintains the flat-cube discreteness axioms incrementally, and builds its debug traces only at `Debug` verbosity (see [#273](https://github.com/rzk-lang/rzk/pull/273));
  - variable lookups no longer project the whole context (see [#276](https://github.com/rzk-lang/rzk/pull/276));
  - top-level entries are kept unshifted and embedded on lookup (see [#277](https://github.com/rzk-lang/rzk/pull/277));
  - the name-shadowing check no longer embeds global payloads (see [#279](https://github.com/rzk-lang/rzk/pull/279));
  - the saturated tope context is cached lazily, recomputed only when the context changes (see [#280](https://github.com/rzk-lang/rzk/pull/280)).

  A corpus benchmark harness under `bench/` records the measurements (see [#274](https://github.com/rzk-lang/rzk/pull/274)).

CI / infrastructure:

- The parser-drift workflow now also fails on LR conflicts and unused rules in the grammar (`make -C rzk check-parser-conflicts`), so a grammar change cannot silently introduce ambiguity (see [#281](https://github.com/rzk-lang/rzk/pull/281)).

## v0.9.1 — 2026-06-26

Fixed:

- In lenient (`--allow-holes`) mode, an unused `#!rzk uses` variable is now tolerated whenever the section contains a hole, not only when the hole sits in the declaration itself. For example, with `#!rzk #def in-progress uses (P) : U := ?` and a hole-free wrapper `#!rzk #def check uses (P) : U := in-progress`, the wrapper's `#!rzk uses (P)` reads as unused only because `#!rzk in-progress` is incomplete; it no longer masks the hole the player is meant to see. Strict mode (the default, and CI) still reports a genuinely unused `#!rzk uses` (see [#262](https://github.com/rzk-lang/rzk/pull/262)).

- A multi-variable binder in a function type, such as `#!rzk (x y : A) → A`, no longer crashes the checker with `expected a pattern but got x y`. It is now desugared into nested one-variable binders `#!rzk (x : A) → (y : A) → A`, as one would expect. The crash was an uncaught `error` reached during translation, so it took down the playground, the game, and the LSP rather than surfacing as a diagnostic; both `rzk typecheck` and the hole query (`--allow-holes`) are fixed (see [#263](https://github.com/rzk-lang/rzk/pull/263)).

- A hole's hint inventory no longer drops a hypothesis or allow-listed lemma whose elimination spine is long. For a goal `#!rzk B` and a lemma `#!rzk deep : A -> A -> A -> A -> A -> A -> A -> A -> B`, the candidate `#!rzk deep ? ? ? ? ? ? ? ?` was silently omitted, because filling each of the eight arguments with a hole spent one unit of the search-depth bound. Filling a function's arguments is a forced spine step, so it no longer counts against the bound; only the genuinely branching eliminators (Σ/cube projections and `#!rzk idJ`) do. This affects only the candidate hints surfaced to the game and the LSP, not the strict `rzk typecheck` path (see [#261](https://github.com/rzk-lang/rzk/pull/261)).

- In lenient (`--allow-holes`) mode, a hole standing for an entire shape-restricted argument is now reported with its goal instead of failing the enclosing extension-type boundary check. When the hole is the whole point of a shape (e.g. a `#!rzk 2 × 2 | Δ¹×Δ¹` point), the surrounding term can unfold to a `#!rzk recOR` whose faces mention the hole (such as `#!rzk π₁ ? ≤ π₂ ?`). Checking an extension-type boundary then enters one of those faces, and a reduction there can drop the hole from the compared terms while the assumed face still depends on it. The boundary mismatch was reported as `cannot unify term`, even though filling the hole makes the term typecheck. A mismatch found under a tope context that still mentions an unfilled hole is now deferred, just like one whose terms still contain a hole. Strict mode (the default, and CI) still reports the hole as unsolved (see [#267](https://github.com/rzk-lang/rzk/pull/267)).

Packaging:

- Add upper bounds on all dependencies, as Hackage asks for. The bounds are at the next super-major version, so a new GHC's boot libraries (`array`, `bytestring`, `directory`, `mtl`, `template-haskell`, `text`) and the pinned Nix/GHCJS package set both stay in range without a Hackage revision on every minor dependency bump. The loose lower bounds are unchanged, so the GHCJS path still resolves (see [#259](https://github.com/rzk-lang/rzk/issues/259), [#134](https://github.com/rzk-lang/rzk/issues/134)).

CI / infrastructure:

- Upgrade the playground's build toolchain to Vite 7 and `tsx` 4.20, picking up the upstream security fixes, and build it with Node 22 under Nix to match (see [#265](https://github.com/rzk-lang/rzk/pull/265)).

- Fix the playground's `ASSET_URL` so the root (`main`) deployment loads its assets from the correct path.

## v0.9.0 — 2026-06-24

Added:

- **Typed holes and a structured goal/context query.** Write `?` (or a named `?goal`) for an unfilled subterm. By default `rzk typecheck` reports any hole as an error, so finished work and CI never admit holes. With `rzk typecheck --allow-holes` it instead prints, for each hole, its expected type (the goal) and its local context. The local context lists the term variables, cube variables, and tope assumptions in scope; the global environment is excluded. The same data is available to library consumers (the playground/game and the LSP) through `typecheckModulesWithHoles`, which returns structured `HoleInfo` values. Holes are checked only where their type is known, that is, in checking position. A hole in inference position is reported rather than guessed. When a hole is the argument of a shape-restricted function, its goal is shown as the shape `(s : I | ψ s ∧ φ s)` (the membership tope), and extension-type goals show their boundary `A [φ ↦ a]` (see [#237](https://github.com/rzk-lang/rzk/pull/237), [#240](https://github.com/rzk-lang/rzk/pull/240)).

- **Hint inventory for a hole.** Alongside the goal and context, the query records the moves that fit. It offers three kinds of move. The first is type-directed *elimination candidates* over the hypotheses (a function applied to holes, a projection of a Σ, or path induction by `idJ`, see [#244](https://github.com/rzk-lang/rzk/pull/244)). The second is *introduction forms* that build the goal from its head (a λ, a pair, `refl`, or a tope constructor). The third, in a shape setting, is the context-driven `recBOT` (ex falso) and `recOR` case-splits (see [#245](https://github.com/rzk-lang/rzk/pull/245)). A caller may additionally allow-list named lemmas in scope, so they surface as candidates applied to holes (see [#256](https://github.com/rzk-lang/rzk/pull/256)). Pattern binders are restored throughout goals, errors, and candidates, so a goal reads `\ (t , s) → ?` rather than in terms of projections (see [#242](https://github.com/rzk-lang/rzk/pull/242), [#246](https://github.com/rzk-lang/rzk/pull/246)).

- **Structured diagnostics.** Type errors and holes are now available as structured data, not only as a formatted string. A diagnostic in `Rzk.Diagnostic` carries a severity, a stable code, a source location, and a message. `rzk typecheck --json` emits all diagnostics (both type errors and holes) as JSON on stdout, for editor-agnostic tooling and CI (see [#238](https://github.com/rzk-lang/rzk/pull/238), [#243](https://github.com/rzk-lang/rzk/pull/243)). The language server consumes the same structured diagnostics. It shows each hole's goal and context as a warning rather than a hard error, in the spirit of Agda's yellow "unsolved" highlight, and it now reports all diagnostics for a file together. Source spans remain line-level.

- **Goal-cell diagrams.** When a hole's goal is a shape (an arrow, triangle, or square), the query exposes an SVG of the cell the hole must inhabit, drawn from an abstract inhabitant of the goal. A new `render-hide-term` option draws the shape with the proof term hidden, keeping only the given boundary labels (see [#253](https://github.com/rzk-lang/rzk/pull/253)).

- Experimental **cubical interval** `#!rzk II` (`#!rzk 𝕀`), a lattice cube added on top of the modal type system. This is the intended sound home for an amazing right adjoint `#!rzk √`, as flagged in the v0.8.0 release notes (see [#228](https://github.com/rzk-lang/rzk/pull/228)).

Changed:

- Experimental **modal syntax** is less noisy. A modal type is now written `#!rzk m A` instead of `#!rzk <| m | A |>`, and a modal binding is `#!rzk (x :m A)` (see [#227](https://github.com/rzk-lang/rzk/pull/227)). Modal bindings on Π-, Σ-, and λ-types are now primitive rather than sugar for `#!rzk let mod`, and modal extraction is restricted to the right-adjoint modalities (`#!rzk ♯`, `#!rzk ᵒᵖ`, `#!rzk _id`) (see [#248](https://github.com/rzk-lang/rzk/pull/248)).

- Experimental: modalities are tracked through the tope layer, so the tope solver knows which topes are accessible under which modality (see [#230](https://github.com/rzk-lang/rzk/pull/230)).

- The `#!rzk uses`/unused-variable check now keys off the definition as written rather than its weak head normal form. Reduction can drop or expose a variable occurrence, so a genuine use could be reported as unused, or a transitive dependency on an assumption hidden. Both are now classified from the written term (see [#255](https://github.com/rzk-lang/rzk/pull/255)).

Other changes:

- In lenient (`--allow-holes`) mode, we tolerate diagnostics that only a finished term must satisfy, so an in-progress sketch still typechecks. These are an unused `#!rzk uses`-variable (see [#249](https://github.com/rzk-lang/rzk/pull/249)), a hole-introduction binder that would shadow a name in scope (see [#250](https://github.com/rzk-lang/rzk/pull/250)), and a term whose type fails only around a hole (see [#251](https://github.com/rzk-lang/rzk/pull/251)). We also show the boundary in a `#!rzk recOR` branch goal (see [#252](https://github.com/rzk-lang/rzk/pull/252)) and accept holes in more positions (see [#239](https://github.com/rzk-lang/rzk/pull/239)).

- Build `rzk` with `build-type: Simple` instead of `Custom`. Thus the package now builds under the **GHC WebAssembly backend** (a `Custom` `Setup.hs` cannot run there) and no longer requires BNFC, `alex`, or `happy` to install (e.g. from Hackage). The parser and lexer are still generated from `grammar/Syntax.cf`, but out of band: run `make -C rzk regen-parser` after editing the grammar. A new `Parser up-to-date` CI workflow regenerates from the grammar and fails on drift, and the WASM/GHCJS CI lanes no longer patch the package before building (see [#236](https://github.com/rzk-lang/rzk/pull/236)).

Fixes:

- A `#!rzk recOR` in the cube or tope layer is now a type error instead of a panic (see [#254](https://github.com/rzk-lang/rzk/pull/254)).
- Check each `#!rzk recOR` branch against the expected type (see [#247](https://github.com/rzk-lang/rzk/pull/247)).
- Warn about overhanging `#!rzk recOR` faces and reject disjoint (vacuous) ones, replacing two checks that were silently vacuous (see [#233](https://github.com/rzk-lang/rzk/pull/233)).
- Check a term even under an absurd (`#!rzk recBOT`) context, so an ill-formed body is no longer admitted under a false hypothesis (see [#234](https://github.com/rzk-lang/rzk/pull/234)).
- Experimental modal layer: several corrections (see [#258](https://github.com/rzk-lang/rzk/pull/258)). The coercion relation is completed with `#!rzk ♭ ⇒ ᵒᵖ` and `#!rzk ᵒᵖ ⇒ ♯`, so a flat variable is now accessible under `#!rzk ᵒᵖ`. The normal form of `#!rzk let mod` extracts only for the right-adjoint modalities, matching the typechecker. And `#!rzk unflipᵒᵖ` now expects an interval cube under `#!rzk ᵒᵖ` (i.e. `#!rzk ᵒᵖ 2` or `#!rzk ᵒᵖ 𝕀`) rather than a bare cube, mirroring `#!rzk flipᵒᵖ`.

Documentation:

- Remove a malicious `polyfill.io` dependency from the docs (see [#231](https://github.com/rzk-lang/rzk/pull/231)).

CI / infrastructure:

- Add a CI job that builds the `rzk` library with the GHC WebAssembly backend (see [#235](https://github.com/rzk-lang/rzk/pull/235)).
- Import `RzkConfig` qualified (see [#232](https://github.com/rzk-lang/rzk/pull/232)).
- Deploy the docs and the playground reliably on tag pushes, so a release publishes both (past releases sometimes skipped them).
- Move the MkDocs typecheck and deploy into the GHC workflow and reuse the `rzk` binary built in the same run, so the docs are checked with the version they document rather than with an older release.
- Build all four OS only on pull requests, tags, and `main`. A `develop` push, already validated by its pull request, builds Linux only. A per-ref concurrency group cancels a superseded pull-request run, but never a push or tag run that may be deploying.

## v0.8.0 — 2026-06-04

Major changes:

- Experimental **modal extension** by [Islam Talipov](https://github.com/LIshy2) for reasoning in the style of Triangulated Type Theory (see [#225](https://github.com/rzk-lang/rzk/pull/225) and [#224](https://github.com/rzk-lang/rzk/pull/224)):
  - Four modalities: discretization `#!rzk _b` (`#!rzk ♭`), codiscretization `#!rzk _#` (`#!rzk ♯`), orientation reversal `#!rzk _op` (`#!rzk ᵒᵖ`), and identity `#!rzk _id`, composed by a fixed mode theory.
  - Modal types `#!rzk <| m | A |>`, introduction form `#!rzk mod m x`, and the eliminator `#!rzk let mod m x := … in …`.
  - Modal tope axioms (`#!rzk inv`/`#!rzk uninv`, `#!rzk flip`/`#!rzk unflip`) with η-rules and reductions for the cube/tope layer under `#!rzk _op`.
  - New documentation page [Modalities (experimental)](https://rzk-lang.github.io/rzk/en/v0.8.0/reference/modalities.rzk/) (also [in Russian](https://rzk-lang.github.io/rzk/ru/v0.8.0/reference/modalities.rzk/)).

  !!! warning "Known unsoundness footgun"
      Users formalizing Triangulated Type Theory must **not** postulate the amazing right adjoint `√` to `#!rzk (2 → -)` (or any "tininess of `2`") on the existing cube `#!rzk 2` — this is unsound in the standard RS17 model because `#!rzk 2` carries a totally-ordered tope layer (GWB §1.3). A sound place to assert `√` (a lattice cube `𝕀`) is planned for the next release.

- Experimental **`let`-bindings** at the MLTT level (see [#222](https://github.com/rzk-lang/rzk/pull/222)), with formatter support and tests. Documented in [Let bindings](https://rzk-lang.github.io/rzk/en/v0.8.0/reference/let-bindings.rzk/).

- Data-driven **typechecking test suite** (see [#223](https://github.com/rzk-lang/rzk/pull/223)): YAML-described fixtures under `rzk/test/typecheck/cases/`, with paired (`foo.rzk` + `foo.expect.yaml`) and directory layouts, regression-traceability metadata, and a schema (`rzk/test/typecheck/SCHEMA.md`).

Other changes:

- Run BNFC with `--text-token` and bump the Stack resolver to `lts-24.34` (see [#221](https://github.com/rzk-lang/rzk/pull/221)).
- Add an agda-input-like extension to input Unicode symbols in the Rzk Playground, including superscript and subscript symbols (see [#210](https://github.com/rzk-lang/rzk/pull/210)).

Fixes:

- Apply a prettier formatter pass (see [#214](https://github.com/rzk-lang/rzk/pull/214)).
- Fix formatting for colon in parameters/parens (see [#216](https://github.com/rzk-lang/rzk/pull/216), fixes [#215](https://github.com/rzk-lang/rzk/issues/215)).
- Normalize tabs to spaces in the formatter; use whole-document formatting with LSP (see [#217](https://github.com/rzk-lang/rzk/pull/217)).
- Insert newline after `=_{…}` if it was on its own line; avoid an extra trailing newline (see [#218](https://github.com/rzk-lang/rzk/pull/218)).
- Fix `#unset-option` for `render`.

Documentation:

- Add commands documentation and complete the Russian translation of the Reference section (see [#211](https://github.com/rzk-lang/rzk/pull/211)).
- Translate documentation for modalities and let-bindings into Russian.

CI / infrastructure:

- Bump GitHub Actions versions and switch to better-maintained actions (see [#212](https://github.com/rzk-lang/rzk/pull/212)).

## v0.7.7 — 2025-11-04

Important fixes:

- Fix subtyping (automatic coercion for extension types and tope disjunction elimination) (see [#207](https://github.com/rzk-lang/rzk/pull/207))
  1. Do not assume variance for the argument of function/type family application (fixes #206).
  2. Separately check tope families and extension types (and $\Pi$-types), since, for tope families, variance of the argument is different.
  3. Fix `refl` to check for equality, not subtyping.
  4. Fix `inferAs` to perform `typecheck` instead (where appropriate).

Minor fix:

- Add version bounds for successful Hackage upload (see [`9aba39c`](https://github.com/rzk-lang/rzk/commit/9aba39c9d63799a00438774e947a986fe8bb9d69))

## v0.7.6 — 2025-08-14

Minor fixes:

- Support BNFC 2.9.6 (see [#201](https://github.com/rzk-lang/rzk/pull/201))
- Fix typo in the sHoTT link (see [#194](https://github.com/rzk-lang/rzk/pull/194))
- Fix tuple pattern syntax (see [#193](https://github.com/rzk-lang/rzk/pull/193) and [#191](https://github.com/rzk-lang/rzk/pull/191))

## v0.7.5 — 2024-08-18

Minor changes:

- Support syntax sugar for nested Σ-types (see [#183](https://github.com/rzk-lang/rzk/pull/183))
- Improve error reporting (see [#176](https://github.com/rzk-lang/rzk/pull/176) and [#179](https://github.com/rzk-lang/rzk/pull/179))

Fixes:

- Support newer `lsp` (specifically, `lsp-2.7.0.0`, see [#188](https://github.com/rzk-lang/rzk/pull/188))
- Fix CI (see [#184](https://github.com/rzk-lang/rzk/pull/184))
- Fix build of nix flake on aarch64-darwin (see [#181](https://github.com/rzk-lang/rzk/pull/181))
- Small documentation fixes (see [#178](https://github.com/rzk-lang/rzk/pull/178))

## v0.7.4 — 2024-04-01

Fixes:

- Fix caching in Rzk Language Server, especially in presence of errors (see [#167](https://github.com/rzk-lang/rzk/pull/167))
- Fix CI for the Rzk Playground (see [#174](https://github.com/rzk-lang/rzk/pull/174))

This release also contains minor refactoring (see [#165](https://github.com/rzk-lang/rzk/pull/165))
and a typo fix (see [#171](https://github.com/rzk-lang/rzk/pull/171)).

## v0.7.3 — 2023-12-16

Fixes:

- Fix overlapping edits in the formatter, hopefully making it idempotent (see [#160](https://github.com/rzk-lang/rzk/pull/160));
- Fix formatter crashing the language server (see [#161](https://github.com/rzk-lang/rzk/pull/161));
- Avoid unnecessary typechecking when semantics of a file has not changed (see [#159](https://github.com/rzk-lang/rzk/pull/159));
- Stop typechecking after the first parse error (avoid invalid cache) (see [`68ab0b4`](https://github.com/rzk-lang/rzk/commit/68ab0b4dd3d627756e10adb55cb16845b08d09d9));

Other:

- Add unit tests for the formatter (see [#157](https://github.com/rzk-lang/rzk/pull/157));

## v0.7.2 — 2023-12-12

Fixes:

- Fixes for `rzk format`:
  - Fix extra space after open parens in formatter (see [#155](https://github.com/rzk-lang/rzk/pull/155));
  - Replace line string content with tokens when checking open parens (see [#156](https://github.com/rzk-lang/rzk/pull/156));
- Throw an error when `rzk.yaml`'s `include` is empty (see [#154](https://github.com/rzk-lang/rzk/pull/154));

Changes to the Rzk website:

- Support multiple languages in the documentation (see [#150](https://github.com/rzk-lang/rzk/pull/150));
  - English is the default;
  - Russian documentation is partially translated and is available at <http://rzk-lang.github.io/rzk/ru/>;
- Add a blog (see [#153](https://github.com/rzk-lang/rzk/pull/153) and [`e438820`](https://github.com/rzk-lang/rzk/commit/e4388202cea59531903c4c24b939841b2771ceb7));
  - The blog is not versioned and is always available at <https://rzk-lang.github.io/rzk/en/blog/>;
- Add a new [Other proof assistants for HoTT](https://rzk-lang.github.io/rzk/en/v0.7.2/related/) page (also [in Russian](https://rzk-lang.github.io/rzk/ru/v0.7.2/related/));
- Add a new [Introduction to Dependent Types](https://rzk-lang.github.io/rzk/en/v0.7.2/getting-started/dependent-types.rzk/) page (also [in Russian](https://rzk-lang.github.io/rzk/ru/v0.7.2/getting-started/dependent-types.rzk/))
- Add (default) social cards
- Integrate ToC on the left
- Use Inria Sans for English, PT Sans for Russian

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

This version contains mostly infrastructure improvements:

- Typecheck using `rzk.yaml` if it exists (see [#119](https://github.com/rzk-lang/rzk/pull/119))
- Update Rzk Playground and Nix Flake (see [#84](https://github.com/rzk-lang/rzk/pull/84))
  - Rzk Playground now uses CodeMirror 6 and NextJS
  - `miso` dependency is dropped
  - GHCJS 9.6 is now used
  - Support `snippet={code}` or `code={code}` param (see [#118](https://github.com/rzk-lang/rzk/pull/118))
    - Support for `snippet_url={URL}` is temporarily dropped
- Update to GHC 9.6, the latest Stackage Nightly, improve Rzk setup, and GitHub Actions (see [#116](https://github.com/rzk-lang/rzk/pull/116))
- Add logging for Rzk Language Server (see [#114](https://github.com/rzk-lang/rzk/pull/114))

Fixes:

- Fix error messages in Rzk Playground (see [#115](https://github.com/rzk-lang/rzk/pull/115))

## v0.6.4 — 2023-09-27

This version improves the structure of the project, in particular w.r.t dependencies:

- Add custom snapshot and explicit lower bounds (see [#108](https://github.com/rzk-lang/rzk/pull/108))

## v0.6.3 — 2023-09-27

This version contains a fix for the command line interface of `rzk`:

- Fix command line `rzk typecheck` (see [#106](https://github.com/rzk-lang/rzk/pull/106))
  - Previous version ignored failures in the command line
    (the bug was introduced when allowing better autocompletion in LSP).

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

This version fixes the behavior of glob (see [`77b7cc0`](https://github.com/rzk-lang/rzk/commit/77b7cc0494fe0bfd1c9f1aa83db20f42cfda5794)).

## v0.5.5 — 2023-09-19

This version contains Unicode and tope logic-related fixes:

1. Fix (add missing checks) for subshapes (see [#85](https://github.com/rzk-lang/rzk/pull/85));
2. Allow handling wildcards in `rzk` itself (see [#83](https://github.com/rzk-lang/rzk/pull/83));
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
5. Migrate from `fizruk` to `rzk-lang` organization on GitHub (see [`ee0d063`](https://github.com/rzk-lang/rzk/commit/ee0d0638283232c99003a83fdf41eb109ae78983));
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

This version introduces an experimental feature for generating visualizations for simplicial terms in SVG.
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
Type checking has been fixed and improved.

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

6. Restrictions can now support multiple subshapes, effectively internalizing `recOR`:

   ```rzk
   #def hom (A : U) (x y : A) : U
     := (t : Δ¹) -> A [ t === 0_2 |-> x, t === 1_2 |-> y ]
   ```

7. There are now 3 syntactic versions of `refl` with different amount of explicit annotations:
   `refl`, `refl_{x}` and `refl_{x : A}`

8. There are now 2 syntactic versions of identity types (`=`): `x = y` and `x =_{A} y`.

9. `recOR` now supports alternative syntax with an arbitrary number of subshapes:
   `recOR( tope1 |-> term1, tope2 |-> term2, ..., topeN |-> termN )`

10. Now it is possible to have type ascriptions: `t as T`. This can help with ensuring types of subexpressions in parts of formalizations, or to upcast types.

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
allows stopping worrying about name captures in substitutions,
so the implementation is much more trustworthy.
The new representation will also allow bringing in higher-order unification in the future, for better type inference, matching, etc.

New representation also allowed annotating each (sub)term with its type to avoid recomputations and some other minor speedups. There are still some performance issues, which need to be debugged, but overall it is much faster than version 0.1.0 already.
