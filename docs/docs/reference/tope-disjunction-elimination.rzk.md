# Tope disjuction elimination

```rzk
#lang rzk-1
```

Following Riehl and Shulman's type theory[^1], `#!rzk rzk-1` introduces two primitive terms for disjunction elimination:

1. `#!rzk recBOT` corresponds to \(\mathsf{rec}\_\bot\), has any type, and is valid whenever tope context is included in `#!rzk BOT`;

2. `#!rzk recOR(«tope_1» |-> «term_1», ..., «tope_n» |-> «term_n»)` defines a term for a disjunction of topes `#!rzk «tope_1» \/ ... \/ «tope_n»`. This is well-typed when for an intersection of any two topes `#!rzk «tope_i» /\ «tope_j»` the corresponding terms `#!rzk «term_i»` and `#!rzk «term_j»` are judgementally equal. In particular, `#!rzk recOR(psi |-> a_psi, phi |-> a_phi)` corresponds to \(\mathsf{rec}_\lor^{\psi, \phi}(a_\psi, a\_\phi)\).

!!! warning "Deprecated syntax"
`#!rzk recOR(psi, phi, a_psi, a_phi)` corresponds to \(\mathsf{rec}_\lor^{\psi, \phi}(a_\psi, a\_\phi)\), is well-typed when `#!rzk a_psi` is definitionally equal to `#!rzk a_phi` under `#!rzk psi /\ phi`. However, this syntax is deprecated since it is easy to confuse which tope relates to which term.

[^1]: Emily Riehl & Michael Shulman. _A type theory for synthetic ∞-categories._ Higher Structures 1(1), 147-224. 2017. <https://arxiv.org/abs/1705.07442>
