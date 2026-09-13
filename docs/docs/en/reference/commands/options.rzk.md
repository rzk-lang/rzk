# Options

The `#set-option` and `#unset-option` commands control typechecker options.

## Syntax

```{.rzk}
#set-option "<option-name>" = "<value>"
#unset-option "<option-name>"
```

## Available Options

### `verbosity`

Controls the verbosity of typechecking output.

- `"silent"` — no log printing
- `"normal"` — log typechecking progress (default)
- `"debug"` — log every intermediate action (useful for debugging)

### `render`

Controls the rendering backend for diagrams.

- `"svg"` — enable rendering in SVG format
- `"latex"` — enable rendering in LaTeX format
- `"none"` — disable rendering (default)

### `warn-meta-prefix`

Controls the sensitivity of the meta-parameter layer check. The type theory implemented in rzk separates a _meta-theoretic parameter layer_ from the object theory (see Section 3.2 of the Rzk paper[^1]), where a statement is abstracted over a context of schematic cube, tope, and type parameters. Per declaration, the _meta prefix_ is the parameter prefix up to and including the last parameter whose type lives outside the object theory proper: a universe, `CUBE`, `TOPE`, or a function type quantifying over or landing in one of those. The check warns when a declaration is used with fewer arguments than its meta prefix at an object-level position (for example, storing it in a pair component); unsaturated use at a meta-typed position, such as aliasing a definition or passing it to a parameter with a matching schematic type, stays allowed. Thus a development reads as a family of object-theory definitions, one per meta instantiation.

- `"strict"` — additionally require an unsaturated argument to sit within a top-level receiver's meta prefix (default); the extra warnings carry the distinct code `MetaPrefixWarningStrictOnly`
- `"structural"` — warn only at structurally object-level positions
- `"off"` — disable the check

Note that the check is syntactic and has a known blind spot: with type-in-type, instantiating an ordinary object parameter with a large type can make a position look meta-typed (for example, `g ((X : U) → X → X) h` where `g` expects `(X : U) (x : X)`). The strict sensitivity flags such a use when it falls outside the receiver's meta prefix, but a forgery landing within the prefix, or behind a λ-bound receiver, is not detected; recognising genuinely impredicative instantiations requires universe level inference, which rzk does not implement at the moment.

### `rstt-safe`

Controls the checks for the RSTT fragment:

- `"warn"` — report detected violations (default).
- `"error"` — reject a declaration when a violation is detected.
- `"off"` — leave each standalone warning at its own setting.

The bundle enables strict meta-prefix checking, checks for restrictions in assumed positions, and checks for schematic binders inside terms. It also reports modal constructs, the auxiliary interval and involutions, inductive declarations and their eliminations or constructors, and unfinished obligations. Individual warning options cannot disable these checks while the mode is active. In particular, `warn-free-standing-restriction` and `warn-meta-binder` are off by default with safe mode off; the bundle enables both.

The bundle also enforces `warn-shape-dependency`, which remains enabled by default when safe mode is off.

Syntax is checked against an explicit allow-list before typechecking. Constructs outside it, including cube `sup` and `inf`, are reported even if computation would remove them. Known extensions receive specific diagnostic codes; other unsupported syntax receives `RSTTSyntaxWarning`. Diagnostics point to the outermost forbidden node on each branch, using its source position when available.

The whole-context `warn-overhang` advisory and the family-clipping warning `warn-tope-family-domain` are separate. They do not fail `rstt-safe = "error"`. A free-standing restriction along a concluded codomain remains allowed.

Use `rzk typecheck --rstt-safe=warn` or `--rstt-safe=error` to enforce the selected mode throughout the run, including all input modules. Source options cannot override this CLI selection. `--rstt-safe=off` instead selects standalone warnings throughout. Warning mode preserves a successful exit status for a well-typed input; error mode exits unsuccessfully on a fragment violation, including with `--json` or `--allow-holes`.

Types are also inspected after unfolding definitions and reducing applications, lets, projections and identity elimination. This includes argument domains instantiated by earlier arguments. Restriction guards are preserved, so simplification cannot erase an outer-point dependency. An inspection failure or the limit of 256 nested head-reduction steps produces `RSTTIncompleteWarning`, which is an error in safe error mode.

A source `#set-option` affects only its scope. Enabling it after definitions were checked with the mode off does not check those dependencies retrospectively. These checks do not enforce a consistent universe discipline for the meta-theoretic parameter layer. Rzk accepts `U : U`, and the [Hurkens counterexample](../limitations/hurkens.rzk.md) proves `(A : U) → A` while passing even safe error mode.

### `warn-overhang`

Controls the non-fatal hint printed when a restriction face or a `recOR` guard overhangs the local tope context (is not entailed by it, while still overlapping it). Overhang is legitimate — for example, restricting with an already-defined shape whose faces live on the whole cube — so the hint is informational only. Deciding whether a face overhangs costs a solver query per face, so the hint is off by default.

- `"yes"` — print the hint for overhanging faces and guards
- `"no"` — do not check for overhang (default)

The warning has diagnostic code `OverhangWarning` and is also reported in JSON and by the language server, including at silent verbosity. It tests whether the face or guard entails the whole local tope context. This advisory is independent of RSTT-safe mode.

### `warn-tope-family-domain`

Controls the warning for a tope family that is not included in its declared domain. A family `χ : ψ → TOPE` stands for a tope within `ψ`, and the checker reads it as its intersection with the domain (see Section 6.2 of the Rzk paper[^1]): a concrete family `\ t → φ` checked against `{t : I | ψ} → TOPE` is elaborated to `\ t → ψ ∧ φ`, unless `φ` already entails `ψ`. When the conjunct is inserted, the family checked differs from the family written, and the warning says so. For example, an inner horn declared over `Δ³` with its faces written on the whole cube is read as the horn within `Δ³`, which is usually what is meant. The check costs nothing beyond the entailment that decides the conjunct.

- `"yes"` — warn about a family not included in its declared domain (default); the code is `TopeFamilyDomainWarning`
- `"no"` — do not warn (the family is still read as its intersection with the domain)

### `warn-free-standing-restriction`

Controls the warning for a free-standing restriction in an assumed position. The check accepts restrictions directly on the codomain of a shape-Π, including boundaries that exceed its domain: boundary obligations are checked under the domain assumption. Other restrictions are treated as _free-standing_. Conservativity over RSTT is proved for the derivations that conclude a free-standing restriction but never assume one (Section 5 of the Rzk paper[^1]), so the checker reports the assumed positions: the type of a binder, the motive of an eliminator, the type of an identity type, and a type passed as data, which includes the body of a `U`-valued definition and an argument at a universe-typed parameter. A type passed as data matters because it is substituted into binder and motive positions later. Restrictions along concluded codomains are exempt, including a restriction under an ordinary `Π`. A postulate supplies an assumption, so its type is checked as assumed.

- `"yes"` — warn about an assumed free-standing restriction; the code is `FreeStandingRestrictionWarning`
- `"no"` — do not warn (standalone default; active RSTT-safe mode enables the check)

The check also inspects computed types and instantiated argument domains, so unfolding a definition or reducing an application does not hide an assumed restriction.

### `warn-shape-dependency`

Extension domains and boundaries must be independent of outer cube points (RS17, Appendix A.2[^rs17]); violations have code `RSTTShapeDependencyWarning`. Coordinates bound together in a product cube are allowed, as are boundary values that depend on outer points. The check follows aliases and variable types. Boundary inclusion itself is not required: a boundary can be intersected with the enclosing shape when translated to RSTT.

- `"yes"` — warn about outer-point dependencies (standalone default).
- `"no"` — suppress the warning when RSTT-safe mode is off.

Active RSTT-safe mode forces the check on and treats violations as errors in `"error"` mode. `#unset-option "warn-shape-dependency"` restores `"yes"`.

This check enforces RS17’s requirement that extension domains and boundaries be independent of outer cube points. It does not determine whether a declaration admits an equivalent reformulation within RSTT.

### `warn-meta-binder`

Controls the warning for a schematic variable bound inside a term. A declaration's parameter prefix is its meta-theoretic parameter context, so that the declaration is a family of object-theory statements, one per instantiation (see `warn-meta-prefix`). A λ _inside_ a term that binds a variable at a universe, `CUBE`, `TOPE`, or a function into one of those is outside that reading. The leading λs of a definition's body are its own parameters and stay silent.

Two positions still count as the parameter prefix. The branches of a case split reached while peeling the prefix, because a schema defined by recursion on a schematic index has to split before binding its remaining parameters, and both the index and those parameters end up in the meta prefix, so every use supplies them. And an argument at a schematic parameter, which is the plumbing `warn-meta-prefix` already allows.

- `"yes"` — warn about a schematic binder inside a term; the code is `MetaBinderWarning`
- `"no"` — do not warn (default)

The check is off by default because a development may package a statement quantified over a universe as a type and then prove it by a λ, which is idiomatic although it is outside the correspondence with the parameter layer.

## Examples

```rzk
#lang rzk-1

-- Set verbosity to silent mode
#set-option "verbosity" = "silent"

-- Enable SVG rendering
#set-option "render" = "svg"

-- Later, disable rendering
#unset-option "render"

-- Set verbosity back to normal
#set-option "verbosity" = "normal"
```

## Notes

- Options are set for the remainder of the file (or until unset)
- `#unset-option` reverts an option to its default value
- Unknown option names or invalid values will result in a typechecking error

[^1]:
    Nikolai Kudasov, Violetta Sim, Benedikt Ahrens.
    _Rzk: a Proof Assistant for Synthetic ∞-Categories_. 2026. <https://arxiv.org/abs/2607.12207>

[^rs17]: Emily Riehl and Michael Shulman. _A type theory for synthetic ∞-categories._ 2017. <https://arxiv.org/abs/1705.07442> — Appendix A.2 explains why extension shapes must be independent of the ambient cube context.
