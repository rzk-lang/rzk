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
- **NbE shared-head alignment:** `happy-nbe-wrapper-alignment` (a term against itself under a thin alias, with the alias on the left, on the right, and on both sides) and `happy-nbe-transport-refl-spine` (`transport … refl t` against `t`, the shape that made a definition in Benno Lossin's sHoTT fork take five minutes). Both carry a term whose normal form has 2^22 applications. They are therefore cheap only if the fast path answers from the two spines rather than normalising. Each takes about 0.05 s with the alignment search, and about 12 s without it. The runner has no timing field, so that gap is the assertion: a regression shows up as a suite that takes half a minute, rather than as a failing example. The sizes are chosen so that a regression is unmistakable while the run still terminates.
- **Inductive types (`#data`, stages 1–2):** well-typed declarations and
  computation through the generated eliminators (`happy-data-bool`,
  `happy-data-coprod`, `happy-data-empty`), recursion with induction
  hypotheses (`happy-data-nat` with a genuine induction proof,
  `happy-data-list`), uniform section closure with and without recursion
  (`happy-data-section`, `happy-data-rec-section`), the largeness warning
  (`happy-data-large-warning`, asserted via the `warnings` field);
  indexed families (`happy-data-vec` with the proposal's generated
  eliminator types and index-aware computation, `happy-data-indices-edge`
  with a two-index family and an indexed family in a section,
  `happy-data-hottbook-nat` — the §2.13 encode–decode story with
  successor injectivity and `zero ≠ suc`, round-tripping by `refl`);
  rejections (`ill-data-negative` for strict positivity,
  `ill-data-fun-field` for function-typed recursive fields,
  `ill-data-non-u-sort` for a malformed sort, `ill-data-missing-return`
  and `ill-data-index-mismatch` for index errors, `ill-data-return-type`,
  `ill-data-shape-index` and `ill-data-shape-index-named` for the two
  spellings of a shape index, `ill-data-cube-index` for a cube index,
  `ill-data-shape-param` and `ill-data-cube-param` for shape and cube
  parameters — the last four enforced on the elaborated sort, so the
  named spelling meets the same rule as the inline one,
  `ill-data-modal-type-field` for a modal type field) and name clashes
  (`ill-data-duplicate-constructor`, `ill-data-clash-generated`);
  shape fields (`happy-data-shape-realisation` — the realisation ⌈Φ⌉,
  its recursor as the representability rule, strict β on a syntactic
  point, and the cube-generic form; `happy-data-shape-tope` — the tope
  reaching a `match` branch, a field mentioning the cube variable an
  earlier shape field binds, and a subshape of a product cube;
  `happy-data-modal-shape-field` — a shape field under a modality, with
  strict β through the lock, and `ill-data-modal-shape-field-lock` — the
  lock discipline enforced on the bound cube variable);
  re-ascription clauses (`eliminate with`:
  `happy-data-eliminator-reascription` — a definitionally equal
  spelling is stored and stays interchangeable with the canonical type,
  ι untouched; `ill-data-eliminator-mismatch` for the dedicated
  convertibility error printing the canonical type,
  `ill-data-eliminator-unknown`, `ill-data-eliminator-duplicate`).
- **Path constructors (`#data`, stage 3):** the circle
  (`happy-data-circle` — named method binders, idJ-spelled transport in
  the dependent path method, definitional ι on the point constructor,
  the identity map through `rec` and through `match`, both generated
  `compute-` rules), the propositional truncation
  (`happy-data-prop-trunc` — recursive fields as endpoints, collapse to
  `Unit`), the pushout (`happy-data-pushout` — endpoints applying
  constructors to datatype-free terms), function extensionality proved
  from the interval per HoTT book Lemma 6.3.2, plus its relative form
  for functions out of a shape (RS17 Axiom 4.6, assumed as `extext` in
  sHoTT) by the same argument
  (`happy-data-interval-funext` — a `#data` that changes the ambient
  theory); re-ascription of eliminators
  and computation rules through a library transport/ap/apd
  (`happy-data-hit-reascription`, `compute with` clauses); rejections
  (`ill-data-path-indexed`, `ill-data-path-unannotated` for a bare
  `l = r` return, `ill-data-path-endpoint` for a non-constructor-built
  endpoint, `ill-data-path-higher` and `ill-data-path-higher-field` for
  higher paths, `ill-data-compute-unknown`, `ill-data-compute-kind`,
  `ill-data-compute-mismatch`) and HIT match coverage
  (`ill-match-missing-path-branch`).
- **`match` expressions (M3, PR 3):** elaboration into the generated
  induction eliminator (`happy-match-basics`: plain and recursive
  matches, definitional computation, an `into` motive, a dependent
  motive built from the goal, a non-variable scrutinee, branch order,
  a nested match), indexed families (`happy-match-indexed`, with an
  `into` motive over the index telescope), the equation convoy
  (`happy-match-case-vec` — Coq's `case_vec`: the motive returns a
  function out of the index equation, the match is applied to `refl`,
  and the unused induction hypothesis is bound as `_`); rejections for
  the branch
  bijection and arity (`ill-match-missing-branch`,
  `ill-match-duplicate-branch`, `ill-match-unknown-branch`,
  `ill-match-branch-arity`), a scrutinee that is not of a `#data` type
  (`ill-match-not-data`), and a motive-less match in inference position
  (`ill-match-cannot-infer`). Holes in branches (binder hypotheses,
  labelled goals) are covered by `Rzk.HolesSpec`.
- **Modal `let mod` with an explicit motive:** `happy-modal-let-into`
  (constant motives agreeing with the motive-free form, and the
  dependent elimination the motive exists for: the goal is `C x` while
  the body only proves `C (mod ♭ a)`, which flat admits only through
  the motive since it has no η-rule), `ill-modal-let-into-body` (a body
  that misses the motive at the introduction form).
- **Meta-parameter layer check:** the object-position
  warning (`warn-meta-prefix-object-position`), the strict-only marking
  (`warn-meta-prefix-strict-only`), warning-free plumbing — aliasing,
  saturation, passing a schema to a meta-prefix parameter
  (`happy-meta-prefix-plumbing`), the recomputation of the prefix at
  section close (`warn-meta-prefix-section`), and the sensitivity option
  (`happy-meta-prefix-option-off`, `warn-meta-prefix-option-structural`);
  all asserted via the `warnings` field.
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
| `match` elaboration (M3 PR 3) | `happy-match-*`, `ill-match-*` | A match elaborates into `ind-D params motive methods indices scrutinee` (`checkMatch`); branches are checked against the method Π-types one arm at a time (`checkMatchArms`, the λ rule's mirror); the motive comes from `into` or from goal abstraction (`motiveFromGoal`); administrative motive redexes are β-reduced before branch goals and hypothesis types are shown (`betaMotiveApps`). |
| PR [#327](https://github.com/rzk-lang/rzk/pull/327) `let mod` motive | `happy-modal-let-into`, `ill-modal-let-into-body` | MTT's dependent modal elimination: the `into` motive is checked at `(z :^ν ⟨μ\|A⟩) → U`, the body against `C (mod_μ x)`, and the let itself gets `C M`. Without a motive the body is checked against the goal as written, which suffices only when the goal need not vary with the scrutinee. |
| Meta-parameter layer check | `warn-meta-prefix-*`, `happy-meta-prefix-plumbing` | An unsaturated use of a declaration below its meta prefix warns at object-level positions (`Rzk.TypeCheck.MetaPrefix`); aliasing at a definition root and meta-shaped argument domains stay silent (the sHoTT `weakextext-extext` composition pattern); `endSection` recomputes `varMetaPrefix` after abstracting assumptions. |

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
| `line` | no | 1-based line number in the Rzk file where the error is reported: the start of the sub-term the error is about, not of the declaration around it. |
| `column` | no | 1-based column on that line, likewise the start of the sub-term. |
| `error_count` | no | With `api: collect`, the exact number of errors the run must report. |
| `regression_for` | no | Traceability strings (PR numbers, issue ids, commit themes). |
| `modules` | no | If set (directory case), ordered list of module files for one `typecheckModulesWithLocation` run. |
| `api` | no | Omit or `strict` (default): `typecheckModulesWithLocation` (throws on first error). `collect`: `typecheckModulesWithLocation'` — returns a list of errors without using `throwError`; note that the typechecker still stops per-module chaining when a module reports errors (see implementation in `Rzk.TypeCheck`). |

Parse failures are always test failures; they are not expressed in this schema.
