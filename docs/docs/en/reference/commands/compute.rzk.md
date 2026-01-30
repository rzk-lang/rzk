# `#compute` commands

The compute commands evaluate terms to their normal forms or weak head normal forms.

## Syntax

```{.rzk}
#compute <term>
#compute-whnf <term>
#compute-nf <term>
```

## Description

- `#compute <term>` — alias for `#compute-whnf`, computes the weak head normal form (WHNF) of a term
- `#compute-whnf <term>` — computes the weak head normal form (WHNF) of a term
- `#compute-nf <term>` — computes the full normal form (NF) of a term

These commands are useful for debugging and understanding how terms evaluate. The computed form is printed to the output during typechecking.

## Example

```rzk
#lang rzk-1

#define add (x y : Unit)
  : Unit
  := unit

#define double (x : Unit)
  : Unit
  := add x x

-- Compute weak head normal form
#compute-whnf double unit
-- Output: unit

-- Compute full normal form
#compute-nf double unit
-- Output: unit
```
