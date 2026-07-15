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
