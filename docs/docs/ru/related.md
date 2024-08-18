# Другие решатели

Rzk — не первый и не единственный решатель для (варианта) гомотопической теории типов.
Ниже следует неполный список существующих решателей и библиотек формализаций на них.

## Agda

[Agda](https://agda.readthedocs.io/en/latest/) — это язык программирования с зависимыми типами, а также решатель теорем.

Хотя по умолчанию Agda не совместима с гомотопической теории типов из-за встроенной аксиомы K,
решатель поддерживает [опцию `--without-K`](https://agda.readthedocs.io/en/v2.6.1/language/without-k.html#without-k),
позволяющей восстановить совместимость с унивалентностью.
Стоит отметить основные формализации гомотопической теории типов на Agde:
[`agda-unimath`](https://unimath.github.io/agda-unimath/) и
[`HoTT-Agda`](https://github.com/hott/hott-agda/).

Синтаксис выражений в Rzk частично вдохновлён синтаксисом Agda.
Экспериментальная поддержка автоматического форматирования кода Rzk
основана на стиле принятом в проектах [emilyriehl/yoneda](https://github.com/emilyriehl/yoneda) и [rzk-lang/sHoTT](https://github.com/rzk-lang/sHoTT),
который в свою очередь вдохновлён [стилем `agda-unimath`](https://unimath.github.io/agda-unimath/CODINGSTYLE.html).

Agda реализована на языке программирования Haskell.

## Arend

[Arend](https://arend-lang.github.io) — это решатель теорем для гомотопической теории типов со встроенным типом (ненаправленного) интервала,
что делает его схожим с кубическими системами типов.
Стандартная библиотека формализаций Arend находится в [JetBrains/arend-lib](https://github.com/JetBrains/arend-lib).

Arend разрабатывается JetBrains и реализован на языке Java.
Также существует [плагин для IntelliJ IDEA](https://arend-lang.github.io/about/intellij-features),
превращающий её в полноценную среду разработки для Arend.

## Aya

[Aya](https://www.aya-prover.org) — экспериментальный кубический решатель теорем,
с системой типов, схожей с Cubical Agda, <b><span style="color: red">red</span>tt</b>, и Arend.
Также в Aya есть поддержка сопоставления с образцом, допускающая пересекающиеся и неупорядоченные образцы,
а также возможности для функционального программирования, аналогичные Haskell и Idris.

Aya реализована на Java.

!!! question "Формализации на Aya?"

    Мне неизвестны библиотеки формализации на Aya.

## Семейство <b><span style="color: red">red</span>\*</b>

[<b><span style="color: #135cb7;">cool</span>tt</b>](https://github.com/redprl/cooltt),
[<b><span style="color: red">red</span>tt</b>](https://github.com/redprl/redtt),
и [<b><span style="color: red">Red</span>PRL</b>](https://redprl.readthedocs.io/en/latest/)
представляют семейство экспериментальных решателей теорем,
разработанных [командой <b><span style="color: red">Red</span>PRL</b>](https://redprl.org).

Существует формализация [математической библиотеки <b><span style="color: red">red</span>tt</b>](https://github.com/RedPRL/redtt/tree/master/library)

Семейство решателей <b><span style="color: red">red</span>\*</b> реализовано на языке программирования OCaml.
Создатели семества решателей также работают над [проектом <b><span style="color: rgb(133, 177, 96);">A.L.G.A.E.<span></b>](https://redprl.org/#algae),
который нацелен на реализацию ряда удобных библиотек (для OCaml) для помощи в реализации
решателей теорем на основе проверки типов для ядра решателя.

## Coq

Coq — это зрелый решатель теорем, основанный на исчислении индуктивных конструкций.
Первая формализация гомотопической теории типов и унивалентных оснований, [UniMath](https://github.com/UniMath/UniMath),
начатая Владимиром Воеводским, была создана с использованием Coq и до сих пор развивается и поддерживается
[координационным комитетом UniMath](https://github.com/UniMath/UniMath#the-unimath-coordinating-committee).
Ещё одна важная формализация гомотопической теории типов — это [Coq-HoTT](https://github.com/HoTT/Coq-HoTT)[^3].

Coq реализован на OCaml.

## Cubical Agda

[Cubical Agda](https://agda.readthedocs.io/en/latest/language/cubical.html) (кубическая Агда)
— это расширение Agda набором возможностей кубической теории типов[^1] [^2].

Хотя технически это расширение Agda, лучше рассматривать его как отдельный язык.
Кубическая Agda (как и другие кубические решатели) поддерживает вариацию типов-расширений (аналогично Rzk),
но только для кубических интервалов.

Важными проектами формализации на кубической Agda являются библиотека [`cubical`](https://github.com/agda/cubical)
и проект [1lab](https://1lab.dev).

Кубическая Agda является частью Agda и также реализована на Haskell.

## `cubicaltt`

`cubicaltt` — это экспериментальный кубический решатель[^1] и предшественник Cubical Agda.

Некоторые формализации на `cubicaltt` находятся в <https://github.com/mortberg/cubicaltt/tree/master/examples>.

`cubicaltt` реализован на языке Haskell.

## Lean

[Lean](https://lean-lang.org) — это решатель теорем и язык программирования с зависимыми типами,
основанный на исчислении индуктивных конструкций.
Аналогично Coq, Lean способствует широкому использованию тактик и автоматизации.

Lean 2, как и Agda, поддерживал режим без уникальности доказательств тождеств (Uniqueness of Identity Proofs, UIP),
который допускал совместимость с гомотопической теорией типов.
Поэтому, существует формализация [HoTT на Lean 2](https://github.com/leanprover/lean2/tree/master/hott)[^4].
Однако, Lean 2 больше не поддерживается и формализация, соответственно, тоже.

Lean 3 и 4 убрали режим, допускающий (прямую) формализацию гомотопической теории типов.
Несмотря на это проект [Ground Zero](https://github.com/forked-from-1kasper/ground_zero)
продолжает формализацию HoTT на Lean 4. Проект использует [проверку элиминации](https://github.com/forked-from-1kasper/ground_zero/blob/d8c41ea2910d81d3c1bf6c2762663473368016ab/GroundZero/Meta/HottTheory.lean),
портированную из неподдерживаемого [проекта по переносу библиотеки HoTT с Lean 2 на Lean 3](https://github.com/gebner/hott3).
Эта проверка позволяет поддерживать окружение HoTT, совместимое со стандартным окружением Lean (по крайней мере по убеждениям авторов проекта).
В частности, попытка (напрямую) доказать принцип неразличимости доказательств равенства (Uniqueness of Identity Proofs)
приводит к ошибки типизации в рамках проекта Ground Zero.

Lean 2 и 3 реализованы на C++.
Lean 4 реализован в основном на самом себе (и немного на C++)!

[^1]:
    Cyril Cohen, Thierry Coquand, Simon Huber, and Anders Mörtberg.
    _Cubical Type Theory: A Constructive Interpretation of the Univalence Axiom_.
    In 21st International Conference on Types for Proofs and Programs (TYPES 2015).
    Leibniz International Proceedings in Informatics (LIPIcs), Volume 69, pp. 5:1-5:34, Schloss Dagstuhl - Leibniz-Zentrum für Informatik (2018)
    <https://doi.org/10.4230/LIPIcs.TYPES.2015.5>

[^2]:
    Thierry Coquand, Simon Huber, and Anders Mörtberg. 2018. _On Higher Inductive Types in Cubical Type Theory_.
    In Proceedings of the 33rd Annual ACM/IEEE Symposium on Logic in Computer Science (LICS '18).
    Association for Computing Machinery, New York, NY, USA, 255–264.
    <https://doi.org/10.1145/3209108.3209197>

[^3]:
    Andrej Bauer, Jason Gross, Peter LeFanu Lumsdaine, Michael Shulman, Matthieu Sozeau, and Bas Spitters. 2017. _The HoTT library: a formalization of homotopy type theory in Coq_.
    In Proceedings of the 6th ACM SIGPLAN Conference on Certified Programs and Proofs (CPP 2017).
    Association for Computing Machinery, New York, NY, USA, 164–172.
    <https://doi.org/10.1145/3018610.3018615>

[^4]:
    Floris van Doorn, Jakob von Raumer & Ulrik Buchholtz. 2017. _Homotopy Type Theory in Lean_.
    In: Ayala-Rincón, M., Muñoz, C.A. (eds) Interactive Theorem Proving. ITP 2017.
    Lecture Notes in Computer Science(), vol 10499. Springer, Cham.
    <https://doi.org/10.1007/978-3-319-66107-0_30>
