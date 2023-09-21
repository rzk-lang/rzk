# 3. Simplicial Type Theory

These formalisations correspond in part to Section 3 of the RS17 paper.

This is a literate `rzk` file:

```rzk
#lang rzk-1
```

## Simplices and their subshapes

### Simplices

```rzk
-- the 1-simplex
#def Δ¹ : 2 -> TOPE
  := \ t -> TOP

-- the 2-simplex
#def Δ² : (2 * 2) -> TOPE
  := \ (t , s) -> s <= t
```
