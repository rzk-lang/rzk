These tests do not cover everything, but 

Paired `*.rzk` / `*.rzk.md` + `*.expect.yaml` (or dir `expect.yaml`). `Rzk.TypeCheckSpec` runs checks at **Silent** verbosity so traces do not fill `stack test` output.

- **Well-typed tests:** `happy-check`, `happy-refl-path`, `happy-shott-simplicial-subcomplexes`, tope/cubes (`happy-tope-shapes`, `happy-tope-high-dim-cubes`), `recOR`/restrictions (`happy-tope-rec-and-restrict`, `happy-restrict-face-not-contained`, `happy-recor-guard-exceeds-context`, `happy-recor-split-simplex-overhang`), nested `recOR` (`happy-tope-nested-rec-or`, `happy-tope-nested-rec-or-d{4,5,6}`), recBOT body well-formedness (`happy-recbot-term-wellformed`).
- **Ill—typed tests:** unify (`ill-unify*`), pairs/projections/functions (`ill-not-*`, `ill-unexpected-*`, `ill-bare-*`), env (`ill-implicit`, `ill-duplicate`, `ill-undefined`, `ill-unused-assumption`, `ill-param-untyped-pattern`), invalid domains (`ill-invalid-arg-type-bot`, `ill-tope-param-*`).
- **Bad commands/sections/render:** options (`ill-set-option-*`, `ill-unset-option-unknown`), sections (`ill-section-*`), LaTeX define (`ill-render-latex-define`).
- **Options:** `happy-set-option-warn-overhang` (the opt-in overhang hint: the option name is recognised by `#set-option`/`#unset-option`, and an overhanging face still typechecks with it on).
- **UNSAT topes/shapes/`recOR`:** `ill-tope-not-satisfied-*`, `ill-tope-subtle-*`, `ill-rec-or-overlap-incoherent`, `ill-recor-coverage-required`, `ill-restrict-face-disjoint`, `ill-recor-guard-disjoint`, nested `recOR` (`ill-tope-nested-rec-or-inner-singleton`; `*-inner-singleton-d{4,5,6}`) (exhibit exponential slowdown).
- **recBOT body well-formedness:** `ill-recbot-term-not-function`, `ill-recbot-term-undefined` (ill-typed bodies must not be admitted under an absurd hypothesis).
- **Holes (strict mode):** `ill-hole-unsolved` (a hole is an error by default), `ill-hole-infer` (a hole in inference position cannot be guessed). The lenient mode and the structured goal/context query are covered by `Rzk.HolesSpec`, not by YAML fixtures.
- **NbE conversion fast path:** `happy-nbe-church-conversion` (βδ-equal but structurally different Church-numeral applications, including inline endpoints that must not decompose into false subgoals), `ill-nbe-church-unequal` (a wrong equation still fails through the ordinary unification — the fast path never refutes).
- **Inductive types (`#data`, stage 1):** well-typed declarations and
  computation through the generated eliminators (`happy-data-bool`,
  `happy-data-coprod`, `happy-data-empty`), uniform section closure
  (`happy-data-section`), the largeness warning (`happy-data-large-warning`,
  asserted via the `warnings` field); stage-1 rejections
  (`ill-data-recursive`, `ill-data-non-u-sort`, `ill-data-return-type`,
  `ill-data-shape-field`, `ill-data-eliminator-clause`) and name clashes
  (`ill-data-duplicate-constructor`, `ill-data-clash-generated`).
- **Other layouts:** `multimodule-*`, `literate-fence/`.

# Regression tests

Fixture comments and `regression_for` use stable prose (which judgment fails, which helper such as `contextEntailsUnion` / `checkCoherence`) — not `TypeCheck.hs` line numbers. See `SCHEMA.md`.

| Pointer | Fixture(s) | Notes |
|---------|------------|--------|
| Issue [#206](https://github.com/rzk-lang/rzk/issues/206) / PR [#207](https://github.com/rzk-lang/rzk/pull/207) extension `App` subtyping | `ill-issue-206-families-of-extension` | `families-of A` vs `families-of (A [ … ])` must fail (`TypeErrorUnifyTerms`) |
| Issue [#9](https://github.com/rzk-lang/rzk/issues/9) type expansion / shadowing | `happy-issue-9-relfunext2-id` | `relfunext2` + `(w : relfunext2) → relfunext2 := w` |
| Issue [#13](https://github.com/rzk-lang/rzk/issues/13) insufficient `flip` checking | `ill-issue-13-flip-flip-wrong` | Wrong `flip A B (flip A B f)` vs `f` path type |
| Issue [#33](https://github.com/rzk-lang/rzk/issues/33) restriction coherence | `ill-issue-33-restriction-coherence` | Overlapping restriction faces on `2 × 2` |
| Tope context folding: faces/guards need not be contained in the context | `happy-restrict-face-not-contained`, `happy-recor-guard-exceeds-context`, `ill-recor-coverage-required` | `recOR` requires only *coverage* (`context ⊢ ⋁ guards`, via `contextEntailsUnion`); restriction faces need no containment. The removed `contextEntailedBy` guard and the dropped reverse check of the former `contextEquiv` (now `contextEntailsUnion`) both tested `tope ⊢ ⋁ context`, which is vacuous (`⊤ ∈ localTopesNF`, so `⋁ context` reduces to `⊤`). |
| Overhang vs disjoint faces/guards | `happy-restrict-face-not-contained`, `happy-recor-guard-exceeds-context`, `ill-restrict-face-disjoint`, `ill-recor-guard-disjoint` | A face/guard that *overhangs* a consistent context (not entailed by it, but overlapping) is allowed and only warned about (`checkTopeAgainstContext`, gated at `Normal`); one *disjoint* from the context (overlap `⊥`) is vacuous and is a hard error (`TypeErrorTopeContextDisjoint`). |
| Useful overhang: reusing named shapes | `happy-recor-split-simplex-overhang` | Splitting the 2-simplex `Δ²` by the global total-order topes `t ≤ s` / `s ≤ t` (à la RS17 Prop 3.5); the `t ≤ s` guard overhangs `Δ²` yet the split is well-typed. Motivates why overhang must be allowed. |
| recBOT term well-formedness | `ill-recbot-term-not-function`, `ill-recbot-term-undefined`, `happy-recbot-term-wellformed` | A term checked against recBOT (expected type collapsed under an absurd context) must still be well-formed in its own right: `typecheck` runs `infer term` before returning `recBottomT`, so `a a` and undefined variables are rejected while valid bodies still pass. |
| PR [#167](https://github.com/rzk-lang/rzk/pull/167) multi-file / cache | `multimodule-first-error/`, `multimodule-two-ok/` | Strict pipeline: error in module B stops after failing file |
| PR [#179](https://github.com/rzk-lang/rzk/pull/179) unused variable errors | `ill-unused-assumption` | Section assumption unused at `#end` |
| PR [#115](https://github.com/rzk-lang/rzk/pull/115) `typecheckModules` | `multimodule-two-ok` | Ordered modules share context |
| Commit ac9b6d89 refl / check | `happy-refl-path` | `refl_{x}` and identity `#check` |
| Docs: sections / implicit | `ill-implicit` | `TypeErrorImplicitAssumption` |
| Coverage matrix | `ill-unify`, `ill-undefined`, `ill-duplicate`, `ill-not-function`, `happy-check` | Core `TypeError` constructors |
| Typed holes (strict mode) | `ill-hole-unsolved`, `ill-hole-infer` | A hole is `TypeErrorUnsolvedHole` by default (finished work/CI reject holes); a hole in inference position is `TypeErrorCannotInferHole`. Lenient mode + structured goal/context in `Rzk.HolesSpec`. |
| Pattern-binder name restoration | `ill-hole-pattern-binder-names` | A pair-pattern lambda `\ (a , b) -> ?` renders its components by name in the strict-mode error: the goal `B (first p)` folds to `B a` and the binder shows as `(a , b)`, not `π₁ x` of a fresh variable. Lenient-mode goals/context covered by `Rzk.HolesSpec`. |
| Bare pattern point in tope | `ill-tope-pattern-binder-bare` | A pattern-bound point used bare (not projected) in a shape's membership tope renders as the pattern: a type error's local tope context shows `Δ² (t , s)`, not `Δ² x₁`. Complements the projection-folding restoration above. |
| `#data` stage 1 (M3, design/inductive-types.md) | `happy-data-*`, `ill-data-*` | The declaration registers the type former before checking constructors (the prototype's ordering bug); the ι-rule fires in WHNF and NF (refl on computed equalities); section closure abstracts the whole family uniformly (`makeAssumptionExplicit` forces the type former). Stage-1 rejections are `TypeErrorOther` with distinguishing `message_contains`. |

# Test schema

## Paired file case

- `something.rzk` — Rzk source (must parse).
- `something.expect.yaml` — expectations for typechecking that file alone with `typecheckModulesWithLocation`.

## Directory case (multiple modules)

- `my-case/expect.yaml` — same fields as below, plus **`modules`** (non-empty list of filenames relative to that directory, typechecked in order).
- `*.rzk` files in that directory.

## YAML fields

| Field | Required | Description |
|--------|----------|-------------|
| `status` | yes | `ok` if typechecking must succeed; `error` if it must fail with a type error. |
| `error_tag` | if `status: error` | Name of the `TypeError` constructor after stripping `ScopedTypeError` wrappers, e.g. `TypeErrorUnify`, `TypeErrorUndefined`. |
| `message_contains` | no | Substrings that must all appear in the rendered error (`ppTypeErrorInScopedContext'`). |
| `line` | no | 1-based line number (approximate) in the Rzk file where the error is reported. |
| `regression_for` | no | Traceability strings (PR numbers, issue ids, commit themes). |
| `modules` | no | If set (directory case), ordered list of module files for one `typecheckModulesWithLocation` run. |
| `api` | no | Omit or `strict` (default): `typecheckModulesWithLocation` (throws on first error). `collect`: `typecheckModulesWithLocation'` — returns a list of errors without using `throwError`; note that the typechecker still stops per-module chaining when a module reports errors (see implementation in `Rzk.TypeCheck`). |

Parse failures are always test failures; they are not expressed in this schema.
