# Tope layer

All topes live in `#!rzk TOPE` universe.

Here are all the ways to build a tope:

1. Introduce a variable, e.g. `#!rzk (psi : TOPE) -> ...`;

    - Usually, topes depend on point variables from some cube(s). To indicate that, we usually introduce topes as "functions" from some cube to `#!rzk TOPE`. For example, `#!rzk (psi : I -> TOPE) -> ...`.

2. Use a constant:

    - top tope \(\top\) is written `#!rzk TOP`
    - bottom tope \(\bot\) is written `#!rzk BOT`

3. Usa a tope connective:
    - tope conjunction \(\psi \land \phi\) is written `#!rzk psi /\ phi`
    - tope disjunction \(\psi \lor \phi\) is written `#!rzk psi \/ phi`
    - equality tope \(t \equiv s\) is written `#!rzk t === s`, whenever `#!rzk t` and `#!rzk s` are points of the same cube
    - inequality tope \(t \leq s\) is written `#!rzk t <= s` whenever `#!rzk t : 2` and `#!rzk s : 2`

