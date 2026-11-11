# Options

## Tope-family domain warning

By default, the checker warns when a concrete tope family is not included in its declared domain. It reads the family as its intersection with the domain.

Use `#set-option "warn-tope-family-domain" = "no"` to disable the warning. The value `"yes"` enables it. `#unset-option "warn-tope-family-domain"` restores the default. These options affect the warning only. The checker still intersects the family with its domain.

## Meta-prefix warnings

A declaration's meta prefix ends at its last schematic parameter: a universe, `CUBE`, `TOPE`, or a function quantifying over or into one of these. The checker warns when a declaration is used at an object-level position before all parameters in this prefix have been supplied.

The option `warn-meta-prefix` accepts `"strict"` (the default), `"structural"`, or `"off"`. Strict checking also warns when an unsaturated schema is passed to a local function. Passing it to a top-level declaration's schematic parameter within that declaration's meta prefix is allowed. Structural checking omits this additional condition. Use `#set-option "warn-meta-prefix" = "off"` to disable these warnings and `#unset-option "warn-meta-prefix"` to restore the default.

The check inspects definitions, postulates, assumptions, `#check`, and computation commands. It reports an incomplete check if it cannot classify a type. These diagnostics do not establish universe levels or validate schematic instantiations.

## RSTT fragment warnings

Three optional diagnostics report declarations outside the RSTT fragment:

- `warn-free-standing-restriction` reports a restriction assumed outside the immediate codomain of a shape function. This includes binder types, eliminator motives, identity types, and types passed as data. Restrictions along concluded codomains are allowed.
- `warn-shape-dependency` reports cube domains, shape conditions, and assumed restriction boundaries that depend on outer cube points.
- `warn-meta-binder` reports schematic binders inside terms. A declaration's parameter prefix and schema arguments may bind schematic parameters.

Each option accepts `"yes"` or `"no"` and defaults to `"no"`. For example, `#set-option "warn-shape-dependency" = "yes"` enables shape diagnostics. `#unset-option "warn-shape-dependency"` restores the default. Settings inside a section do not escape it.

These diagnostics inspect source expressions and unfolded types. An inspection that cannot finish produces an incomplete-check warning. They do not change which declarations are accepted or establish universe levels or valid schematic instantiations.
