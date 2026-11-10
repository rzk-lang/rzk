# Options

## Tope-family domain warning

By default, the checker warns when a concrete tope family is not included in its declared domain. It reads the family as its intersection with the domain.

Use `#set-option "warn-tope-family-domain" = "no"` to disable the warning. The value `"yes"` enables it. `#unset-option "warn-tope-family-domain"` restores the default. These options affect the warning only. The checker still intersects the family with its domain.
