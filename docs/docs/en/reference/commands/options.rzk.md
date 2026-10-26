# Options

The `#set-option` and `#unset-option` commands control typechecker options.

## Syntax

```{.rzk}
#set-option "<option-name>" = "<value>"
#unset-option "<option-name>"
```

## Available options

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

A declaration's _meta prefix_ ends at its last schematic parameter: a universe, `CUBE`, `TOPE`, or a function quantifying over or into one of them. These parameters represent a family of object-theory statements (Section 3.2 of the Rzk paper[^1]). The check reports uses that supply too few schematic arguments, for example when storing a declaration in a pair.

- `"strict"` — also require unsaturated schematic arguments to occur within another declaration's meta prefix (default).
- `"structural"` — allow unsaturated arguments at any schematic parameter.
- `"off"` — disable the check when RSTT-safe mode is off.

The diagnostic codes are `MetaPrefixWarning` and `MetaPrefixWarningStrictOnly`. Aliases and arguments supplied to schematic parameters remain allowed. Saturation does not enforce universe levels; see the [consistency limitation](#rstt-safe).

### `rstt-safe`

Checks declarations against the RSTT fragment used in the conservativity result.[^1]

- `"warn"` — report detected violations (default).
- `"error"` — reject declarations with violations or incomplete checks.
- `"off"` — use the standalone warning settings.

Active RSTT-safe mode enforces these settings:

| Option | RSTT-safe `"warn"` / `"error"` | Standalone default |
|---|---|---|
| `warn-meta-prefix` | `"strict"` | `"strict"` |
| `warn-free-standing-restriction` | `"yes"` | `"no"` |
| `warn-meta-binder` | `"yes"` | `"no"` |
| `warn-shape-dependency` | `"yes"` | `"yes"` |

The mode also reports unfinished obligations and unsupported constructs, including modal and inductive extensions, auxiliary intervals, involutions, and cube `sup` and `inf`. These checks have no standalone option. Violations in source expressions are reported even when computation would remove them. Types exposed by computation are also checked; an unfinished inspection produces `RSTTIncompleteWarning`.

`warn-overhang` and `warn-tope-family-domain` remain independent advisories. They do not cause errors in RSTT-safe error mode.

Use `rzk typecheck --rstt-safe=warn`, `--rstt-safe=error`, or `--rstt-safe=off` to fix the mode for the whole run, including all input modules. Source options cannot override that selection. Error mode gives an unsuccessful exit status on violations, including with `--json` or `--allow-holes`. A source `#set-option` affects only its scope; enabling safe mode does not audit all earlier declarations.

Schematic parameters are checked separately from ordinary types. An `(A : U)` parameter requires an ordinary type; passing `U`, a family kind such as `X → U`, or a schematic rule produces `RSTTSchematicWarning`. The check follows aliases, computations and proof dependencies, including used definitions checked with safe mode off. Such a dependency must pass independently of the caller’s tope assumptions.

Assumptions and postulates remain trusted: their statements are checked, but their consistency is not. This check rejects the [Hurkens counterexample](../limitations/hurkens.rzk.md) without introducing universe levels. It supplements Rzk’s typechecker; it is not an independent RSTT kernel. The check is conservative: using ordinary polymorphic combinators on schematic data may require a direct schematic definition instead.

### `warn-overhang`

Warns when a restriction face or `recOR` guard overlaps the local tope context but is not contained in it. The check asks whether the face or guard entails the whole context. Such overhang is allowed.

- `"yes"` — report overhang as `OverhangWarning`.
- `"no"` — do not check for overhang (default).

The warning appears in text, JSON, and language-server diagnostics, including at silent verbosity. It remains an advisory in RSTT-safe error mode.

### `warn-tope-family-domain`

Controls the warning for a tope family that is not included in its declared domain. A family `χ : ψ → TOPE` stands for a tope within `ψ`, and the checker reads it as its intersection with the domain (see Section 6.2 of the Rzk paper[^1]): a concrete family `\ t → φ` checked against `{t : I | ψ} → TOPE` is elaborated to `\ t → ψ ∧ φ`, unless `φ` already entails `ψ`. When the conjunct is inserted, the family checked differs from the family written, and the warning says so. For example, an inner horn declared over `Δ³` with its faces written on the whole cube is read as the horn within `Δ³`, which is usually what is meant. The check costs nothing beyond the entailment that decides the conjunct.

- `"yes"` — warn about a family not included in its declared domain (default); the code is `TopeFamilyDomainWarning`
- `"no"` — do not warn (the family is still read as its intersection with the domain)

### `warn-free-standing-restriction`

Warns about assumed free-standing restrictions, which are outside the proved conservative fragment.[^1] A restriction is _free-standing_ unless it occurs directly on the codomain of a shape-Π. Boundary obligations are checked within that shape, so a boundary may extend beyond its domain.

The check covers binder types, eliminator motives, the underlying types of identity types, and types passed as data. It also checks types exposed by computation. Restrictions along concluded codomains remain allowed, including under an ordinary `Π`. A postulate supplies an assumption, so its type must satisfy the same restriction check as other assumptions.

- `"yes"` — report assumed free-standing restrictions as `FreeStandingRestrictionWarning`.
- `"no"` — suppress the warning (standalone default).

Active RSTT-safe mode enables the check.

### `warn-shape-dependency`

Warns about outer cube points in cube domains, shape conditions, and assumed extension boundaries, following RS17, Appendix A.2.[^rs17] This includes dependencies revealed by aliases and computation. Coordinates bound together in a product cube are allowed, as are boundary values that depend on outer points. A boundary may extend beyond its domain: it can be intersected with that domain when translated to RSTT.

- `"yes"` — report dependencies as `RSTTShapeDependencyWarning` (standalone default).
- `"no"` — suppress the warning when RSTT-safe mode is off.

Active RSTT-safe mode enables the check; `#unset-option` restores `"yes"`. A warning does not rule out an equivalent reformulation within RSTT.

### `warn-meta-binder`

Warns about schematic parameters bound inside terms, including type-valued terms. For example, storing `\ (A : U) → A` in a pair reports `MetaBinderWarning`. A schematic parameter has type `U`, `CUBE`, `TOPE`, or a function quantifying over or into one of them.

The leading parameters of a definition and schematic arguments supplied to other declarations remain allowed (see `warn-meta-prefix`).

- `"yes"` — report schematic binders inside terms.
- `"no"` — suppress the warning (standalone default).

Active RSTT-safe mode enables the check.

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
