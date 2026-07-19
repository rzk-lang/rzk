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

### `warn-overhang`

Controls the non-fatal hint printed when a restriction face or a `recOR` guard overhangs the local tope context (is not entailed by it, while still overlapping it). Overhang is legitimate — for example, restricting with an already-defined shape whose faces live on the whole cube — so the hint is informational only. Deciding whether a face overhangs costs a solver query per face, so the hint is off by default.

- `"yes"` — print the hint for overhanging faces and guards
- `"no"` — do not check for overhang (default)

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
