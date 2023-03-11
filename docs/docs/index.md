# rzk — an experimental proof assistant for synthetic ∞-categories

`rzk` is an early prototype of a proof assistant for a family of type systems, including Riehl and Shulman's «Type Theory for Synthetic ∞-categories» ([https://arxiv.org/abs/1705.07442](https://arxiv.org/abs/1705.07442)).

The assistant is polylingual, supporting several language modes. The language mode is specified in the first line of the source module code:

```
#lang <mode>
```

Currently the online assistant supports the following `#lang` modes:

- `#lang rzk-1` ([try online](https://fizruk.github.io/rzk/?lang=rzk-1)) —
  the early prototype of the proof assistant based on Riehl and Shulman's type theory for synthetic ∞-categories.

- `#lang sltc` ([try online](https://fizruk.github.io/rzk/?lang=stlc)) —
  a variant of simply typed lambda calculus (STLC) with computation available at the level of types;

- `#lang pcf` ([try online](https://fizruk.github.io/rzk/?lang=pcf)) —
  programming with computable functions (PCF), an extension of simply typed lambda calculus with natural numbers and booleans;

- `#lang mltt` ([try online](https://fizruk.github.io/rzk/?lang=mltt)) —
  Martin-Löf Type Theory, a dependent type theory with [intensional identity types](https://ncatlab.org/nlab/show/identity+type).
