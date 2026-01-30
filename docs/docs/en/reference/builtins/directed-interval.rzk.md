# Directed interval cube

The `2` cube is the directed interval, which is a fundamental building block for synthetic ∞-category theory.

```rzk
#lang rzk-1
```

## Points

The directed interval `#!rzk 2` has two points:

- `#!rzk 0_2` — the initial point (source)
- `#!rzk 1_2` — the terminal point (target)

## Inequality

The directed interval supports an inequality tope `#!rzk <=` (or `#!rzk ≤`):

- `#!rzk t <= s` is a tope that is satisfied when `#!rzk t : 2` and `#!rzk s : 2` and `#!rzk t` is less than or equal to `#!rzk s` in the directed ordering

This inequality is used to define shapes like simplices and to express directedness in synthetic ∞-category theory.

## Example

```rzk
-- A 1-simplex (directed path) is defined using the inequality
#define Δ¹
  : 2 → TOPE
  := \ t → TOP

-- A 2-simplex uses the inequality to express ordering
#define Δ²
  : ( 2 × 2) → TOPE
  := \ (t , s) → s ≤ t
```

## Notes

- The directed interval `#!rzk 2` is distinct from the unit cube `#!rzk 1`
- Products of `#!rzk 2` cubes (like `#!rzk 2 × 2`) are used to define higher-dimensional shapes
- The inequality `#!rzk ≤` is only valid for points of cube `#!rzk 2`
