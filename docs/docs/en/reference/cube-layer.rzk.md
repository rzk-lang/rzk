# Cube layer

```rzk
#lang rzk-1
```

All cubes live in `#!rzk CUBE` universe.

There are three built-in cubes:

1. `#!rzk 1` cube is a unit cube with a single point `#!rzk *_1`
2. `#!rzk 2` cube is a [directed interval](builtins/directed-interval.rzk.md) cube with points `#!rzk 0_2` and `#!rzk 1_2`, equipped with a linear order
3. `#!rzk II` (or `#!rzk I`) is a cubical interval with points `#!rzk 0_I` and `#!rzk 1_I`, without a linear order

`#!rzk 2` is a subtype of `#!rzk II`: any point of `#!rzk 2` can be used where `#!rzk II` is expected. The cubical interval `#!rzk II` can be used similarly to `#!rzk 2`, but without a total order:

```rzk
-- A cubical hom-type in A between x and y
#define cub-hom
  (A : U) (x y : A)
  : U
  := (t : II) -> A [ t === 0_I |-> x , t === 1_I |-> y ]
```

It is also possible to have `#!rzk CUBE` variables and make products of cubes:

1. `#!rzk I * J` is a product of cubes `#!rzk I` and `#!rzk J`
2. `#!rzk (t, s)` is a point in `#!rzk I * J` if `#!rzk t : I` and `#!rzk s : J`
3. if `#!rzk ts : I * J`, then `#!rzk first ts : I` and `#!rzk second ts : J`

You can usually use `#!rzk (t, s)` both as a pattern, and a construction of a pair of points:

```rzk
-- Swap point components of a point in a cube I × I
#define swap
    ( I : CUBE)
  : ( I × I) → I × I
  := \ ( t , s) → (s , t)
```
