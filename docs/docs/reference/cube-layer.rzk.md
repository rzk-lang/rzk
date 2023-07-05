# Cube layer

```rzk
#lang rzk-1
```

All cubes live in `#!rzk CUBE` universe.

There are two built-in cubes:

1. `#!rzk 1` cube is a unit cube with a single point `#!rzk *_1`
2. `#!rzk 2` cube is a [directed interval](../builtins/directed-interval.rzk.md) cube with points `#!rzk 0_2` and `#!rzk 1_2`

It is also possible to have `#!rzk CUBE` variables and make products of cubes:

1. `#!rzk I * J`  is a product of cubes `#!rzk I` and `#!rzk J`
2. `#!rzk (t, s)` is a point in `#!rzk I * J` if `#!rzk t : I` and `#!rzk s : J`
3. if `#!rzk ts : I * J`, then `#!rzk first ts : I` and `#!rzk second ts : J`

You can usually use `#!rzk (t, s)` both as a pattern, and a construction of a pair of points:

```rzk
-- Swap point components of a point in a cube I × I
#define swap
    (I : CUBE)
  : (I * I) -> I * I
  := \(t, s) -> (s, t)
```

