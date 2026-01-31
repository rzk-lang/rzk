# Устранение дизъюнкции топов

Следуя теории типов Рил и Шульмана[^1], `#!rzk rzk-1` вводит два примитивных терма для исключения дизъюнкции:

1. `#!rzk recBOT` соответствует \(\mathsf{rec}\_\bot\), имеет любой тип и валиден, когда контекст топов включён в `#!rzk BOT`;

2. `#!rzk recOR(«tope_1» |-> «term_1», ..., «tope_n» |-> «term_n»)` определяет терм для дизъюнкции топов `#!rzk «tope_1» \/ ... \/ «tope_n»`. Это хорошо типизировано, когда для пересечения любых двух топов `#!rzk «tope_i» /\ «tope_j»` соответствующие термы `#!rzk «term_i»` и `#!rzk «term_j»` сужденчески равны. В частности, `#!rzk recOR(psi |-> a_psi, phi |-> a_phi)` соответствует \(\mathsf{rec}_\lor^{\psi, \phi}(a_\psi, a\_\phi)\).

!!! warning "Устаревший синтаксис"
`#!rzk recOR(psi, phi, a_psi, a_phi)` соответствует \(\mathsf{rec}_\lor^{\psi, \phi}(a_\psi, a\_\phi)\), хорошо типизирован, когда `#!rzk a_psi` определительно равен `#!rzk a_phi` при `#!rzk psi /\ phi`. Однако этот синтаксис устарел, так как легко перепутать, какой топ относится к какому терму.

[^1]: Emily Riehl & Michael Shulman. _A type theory for synthetic ∞-categories._ Higher Structures 1(1), 147-224. 2017. <https://arxiv.org/abs/1705.07442>
