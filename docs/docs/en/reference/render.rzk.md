# Rendering Diagrams

Starting from version `0.3.0`, `rzk` supports rendering of topes, types, and terms as diagrams.

This is a literate `rzk` file:

```rzk
#lang rzk-1
```

To enable rendering, enable option `"render" = "svg"` (to disable, `"render" = "none"`):

```rzk
#set-option "render" = "svg"  -- enable rendering in SVG
```

Rendering is completely automatic, and works in the following situations:

1. Mapping from a shape (including curried mappings), up to 3 dimensions, only in products of `2` cubes;
2. Type of mapping from a shape (including curried mappings), up to 3 dimensions, only in products of `2` cubes.
3. Mappings from a shape that is a section of an existing shape.

The rendering assigns the following colors:

- purple is assigned for parameters (context) variables;
- blue is used for fillings for types (e.g. for `hom` and `hom2`);
- red is used for terms (e.g. `Segal-comp-witness`);
- orange is used for shapes in the tope layer;
- grey is used for discarded parts of a (larger) mapping (e.g. when extracting a diagonal/face from a larger shape).

The SVG pictures can be inserted directly into `.md` files before a corresponding `rzk` code block. At the bottom of a markdown file, you might want to add stylization, e.g.:

```html
<style>
.rzk-render { transition: transform .2s; /* Animation */ }
.rzk-render:hover { transform: scale(1.5); /* (150% zoom - Note: if the zoom is too large, it will go outside of the viewport) */ }
</style>

<!-- Definitions for the SVG images above -->
<svg width="0" height="0">
  <defs>
    <style data-bx-fonts="Noto Serif">@import url(https://fonts.googleapis.com/css2?family=Noto+Serif&display=swap);</style>
    <marker id="arrow" viewBox="0 0 10 10" refX="5" refY="5"
      markerWidth="5" markerHeight="5" orient="auto-start-reverse">
      <path d="M 0 2 L 5 5 L 0 8 z" stroke="purple" fill="purple" />
    </marker>
  </defs>
  <style>
    text, textPath {
      font-family: Noto Serif;
      font-size: 28px;
      dominant-baseline: middle;
      text-anchor: middle;
    }
  </style>
</svg>
```

## Examples

### Visualising Simplicial Topes

Topes are visualised with <span style="color: orange">**orange**</span> color:

```rzk
-- 2-simplex
#define Δ² : (2 * 2) -> TOPE
  := \(t, s) -> s <= t
```
<br><br>
Boundary of a tope:

```rzk
-- boundary of a 2-simplex
#define ∂Δ² : Δ² -> TOPE
  := \(t, s) -> s === 0_2 \/ t === 1_2 \/ s === t
```

The busiest tope diagram involves the entire 3D cube:
<br><br>

```rzk
-- 3-dim cube
#define 2³ : (2 * 2 * 2) -> TOPE
  := \_ -> TOP
```
<br><br><br>

```rzk
-- 3-simplex
#define Δ³ : (2 * 2 * 2) -> TOPE
  := \((t1, t2), t3) -> t3 <= t2 /\ t2 <= t1
```

<br><br>
### Visualising Simplicial Types

Types are visualised with <span style="color: blue">**blue**</span> color. Recognised parameter part (e.g. fixed endpoints, edges, faces with clear labels) are visualised with <span style="color: purple">**purple**</span> color. When a type is constructed by taking a part of another shape, the rest of the larger shape is colored using <span style="color: gray">**gray**</span> color.

```rzk
-- [RS17, Definition 5.1]
-- The type of arrows in A from x to y.
#define hom
  (A : U)   -- A type.
  (x y : A) -- Two points in A.
  : U                   -- (hom A x y) is a 1-simplex (an arrow)
  := (t : 2) -> A [    -- in A where
    t === 0_2 |-> x,    -- * the left endpoint is exactly x
    t === 1_2 |-> y     -- * the right endpoint is exactly y
  ]
```

```rzk
-- [RS17, Definition 5.2]
-- the type of commutative triangles in A
#define hom2
  (A : U)           -- A type.
  (x y z : A)       -- Three points in A.
  (f : hom A x y)   -- An arrow in A from x to y.
  (g : hom A y z)   -- An arrow in A from y to z.
  (h : hom A x z)   -- An arrow in A from x to z.
  : U                           -- (hom2 A x y z f g h) is a 2-simplex (triangle)
  := { (t1, t2) : Δ² } -> A [   -- in A where
    t2 === 0_2 |-> f t1,        -- * the top edge is exactly f,
    t1 === 1_2 |-> g t2,        -- * the right edge is exactly g, and
    t2 === t1  |-> h t2         -- * the diagonal is exactly h
  ]
```

### Visualising Terms of Simplicial Types

Terms (with non-trivial labels) are visualised with <span style="color: red">**red**</span> color (you can see a detailed label on hover). Recognised parameter part (e.g. fixed endpoints, edges, faces with clear labels) are visualised with <span style="color: purple">**purple**</span> color. When a term is constructed by taking a part of another shape, the rest of the larger shape is colored using <span style="color: gray">**gray**</span> color.

We can visualise terms that fill a shape:

```rzk
#define square
  (A : U)
  (x y z : A)
  (f : hom A x y)
  (g : hom A y z)
  (h : hom A x z)
  (a : Sigma (h' : hom A x z), hom2 A x y z f g h')
  : (2 * 2) -> A
  := \(t, s) -> recOR( s <= t |-> second a (t, s) , t <= s |-> second a (s, t))
```

If a term is extracted as a part of a larger shape, generally, the whole shape will be shown (in gray):

```rzk
#define face
  (A : U)
  (x y z : A)
  (f : hom A x y)
  (a : Sigma (g : hom A y z), {((t1, t2), t3) : 2 * 2 * 2 | t3 <= t1 \/ t2 <= t1} -> A [ t1 === 0_2 |-> f t2, t1 === 1_2 |-> g t3 ])
  : Δ² -> A
  := \(t, s) -> second a ((t, t), s)
```

<!-- Style for the SVG images above -->
<style>
.rzk-render { transition: transform .2s; /* Animation */ }
.rzk-render:hover { transform: scale(1.5); /* (150% zoom - Note: if the zoom is too large, it will go outside of the viewport) */ }
</style>

<!-- Definitions for the SVG images above -->
<svg width="0" height="0">
  <defs>
    <style data-bx-fonts="Noto Serif">@import url(https://fonts.googleapis.com/css2?family=Noto+Serif&display=swap);</style>
    <marker id="arrow" viewBox="0 0 10 10" refX="5" refY="5"
      markerWidth="5" markerHeight="5" orient="auto-start-reverse">
      <path d="M 0 2 L 5 5 L 0 8 z" stroke="black" fill="black" />
    </marker>
  </defs>
  <style>
    text, textPath {
      font-family: Noto Serif;
      font-size: 28px;
      dominant-baseline: middle;
      text-anchor: middle;
    }
  </style>
</svg>
