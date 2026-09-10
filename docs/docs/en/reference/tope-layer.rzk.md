# Tope layer

All topes live in `#!rzk TOPE` universe.

Here are all the ways to build a tope:

1. Introduce a tope family parameter, a "function" from some cube to `#!rzk TOPE`: `#!rzk (psi : I -> TOPE) -> ...`. Topes usually depend on point variables from some cube(s), and the family records that dependency; a bare tope parameter is the special case of a family over the unit cube, `#!rzk (psi : 1 -> TOPE) -> ...`, applied as `#!rzk psi *_1`. (A binder of `#!rzk TOPE` itself is not allowed.)

2. Use a constant:
   - top tope \(\top\) is written `#!rzk TOP`
   - bottom tope \(\bot\) is written `#!rzk BOT`

3. Use a tope connective:
   - tope conjunction \(\psi \land \phi\) is written `#!rzk psi /\ phi`
   - tope disjunction \(\psi \lor \phi\) is written `#!rzk psi \/ phi`
   - equality tope \(t \equiv s\) is written `#!rzk t === s`, whenever `#!rzk t` and `#!rzk s` are points of the same cube
   - inequality tope \(t \leq s\) is written `#!rzk t <= s` whenever `#!rzk t : 2` and `#!rzk s : 2`
