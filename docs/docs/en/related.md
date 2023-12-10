# Other proof assistants for HoTT

Rzk is not the first or the only proof assistant where it's possible to do (a variant of) homotopy type theory.
Here is an incomplete list of such proof assistants and corresponding formalization libraries.

## Agda

[Agda](https://agda.readthedocs.io/en/latest/) is a dependently typed programming language (and also a proof assistant).

While by default Agda is not compatible with HoTT because of built-in Axiom K,
it supports [`--without-K` option](https://agda.readthedocs.io/en/v2.6.1/language/without-k.html#without-k), allowing to restore the compatibility with univalence.
Some notable HoTT libraries in Agda include [`agda-unimath`](https://unimath.github.io/agda-unimath/),
[`HoTT-Agda`](https://github.com/hott/hott-agda/).

Rzk's syntax for expressions is partially inspired by Agda.
Rzk's (experimental) formatter is based on the code style accepted in [emilyriehl/yoneda](https://github.com/emilyriehl/yoneda) and [rzk-lang/sHoTT](https://github.com/rzk-lang/sHoTT) projects,
which comes largely from the [code style of `agda-unimath`](https://unimath.github.io/agda-unimath/CODINGSTYLE.html).

Agda is implemented in Haskell.

## Arend

[Arend](https://arend-lang.github.io) is a theorem prover based on homotopy type theory with an interval type,
making it similar to cubical type theories. Arend features a standard library at [JetBrains/arend-lib](https://github.com/JetBrains/arend-lib).

Arend is developed by JetBrains, and is implemented in Java.
It also features a [plugin for IntelliJ IDEA](https://arend-lang.github.io/about/intellij-features) turning it into an IDE for Arend.

## Aya

[Aya](https://www.aya-prover.org) is an experimental cubical proof assistant,
featuring type system features similar to Cubical Agda, <b><span style="color: red">red</span>tt</b>, and Arend.
It also features overlapping and order-independent pattern matching, as well as
some functional programming features similar to Haskell and Idris.

Aya is implemented in Java.

!!! question "Formalizations in Aya?"

    I do not know of existing formalization libraries in Aya.

## The <b><span style="color: red">red</span>*</b> family

[<b><span style="color: #135cb7;">cool</span>tt</b>](https://github.com/redprl/cooltt), [<b><span style="color: red">red</span>tt</b>](https://github.com/redprl/redtt), and [<b><span style="color: red">Red</span>PRL</b>](https://redprl.readthedocs.io/en/latest/) are a family of experimental proof assistants developed by the [<b><span style="color: red">Red</span>PRL</b> Development Team](https://redprl.org).

There is a [<b><span style="color: red">red</span>tt</b> mathematical library](https://github.com/RedPRL/redtt/tree/master/library)

The <b><span style="color: red">red</span>*</b> family of proof assistants is implemented in OCaml.
The developers also have a related [<b><span style="color: rgb(133, 177, 96);">A.L.G.A.E.<span></b> project](https://redprl.org/#algae),
which aims to provide composable (OCaml) libraries that facilitate the construction of a usable proof assistant from a core type checker.

## Coq

Coq is a mature proof assistant, based on the Calculus of Inductive Constructions.
The original HoTT formalization, [UniMath](https://github.com/UniMath/UniMath),
initiated by Vladimir Voevodsky, is done in Coq and is maintained to this day by
[The UniMath Coordinating Committee](https://github.com/UniMath/UniMath#the-unimath-coordinating-committee).
Other notable formalizations of HoTT in Coq include [Coq-HoTT](https://github.com/HoTT/Coq-HoTT)[^3]

Coq is implemented in OCaml.

## Cubical Agda
[Cubical Agda](https://agda.readthedocs.io/en/latest/language/cubical.html) is a mode extending Agda with a variety of features from Cubical Type Theory[^1] [^2].

Although technical a mode within Agda, it is probably best seen as a separate language.
Cubical Agda (as well as other cubical proof assistants) supports a variant of extension types found in Rzk,
albeit restricted to the use with cubical intervals.

Some notable formalizations in Cubical Agda include [`cubical`](https://github.com/agda/cubical) and [1lab](https://1lab.dev).

Cubical Agda as part of Agda is implemented in Haskell.

## `cubicaltt`

`cubicaltt` is an experimental cubical proof assistant[^1] and a precursor to Cubical Agda.

Several formalizations in `cubicaltt` can be found at <https://github.com/mortberg/cubicaltt/tree/master/examples>.

`cubicaltt` is implemented in Haskell.

## Lean

[Lean](https://lean-lang.org) is a teorem prover and a dependently typed programming language, based on the Calculus of Inductive Constructions.
Similarly to Coq, Lean encourages heavy use of tactics and automation.

Lean 2, similarly to Agda, supported a mode without uniqueness of identity proofs (UIP),
which allowed for HoTT formalizations.
Hence, a formalization of [HoTT in Lean 2](https://github.com/leanprover/lean2/tree/master/hott)[^4] exists.
However, since Lean 2 is not supported anymore, the formalization is also unmantained.

Lean 3 and 4 has dropped the mode that allowed (direct) HoTT formalization.
There is, however, an unmaintained [port of Lean 2 HoTT Library to Lean 3](https://github.com/gebner/hott3).

Lean 2 and 3 are implemented in C++.
Lean 4 is implemented in itself (and a bit of C++)!

[^1]:
    Cyril Cohen, Thierry Coquand, Simon Huber, and Anders Mörtberg.
    _Cubical Type Theory: A Constructive Interpretation of the Univalence Axiom_.
    In 21st International Conference on Types for Proofs and Programs (TYPES 2015).
    Leibniz International Proceedings in Informatics (LIPIcs), Volume 69, pp. 5:1-5:34, Schloss Dagstuhl - Leibniz-Zentrum für Informatik (2018)
    <https://doi.org/10.4230/LIPIcs.TYPES.2015.5>

[^2]:
    Thierry Coquand, Simon Huber, and Anders Mörtberg.
    2018. _On Higher Inductive Types in Cubical Type Theory_.
    In Proceedings of the 33rd Annual ACM/IEEE Symposium on Logic in Computer Science (LICS '18).
    Association for Computing Machinery, New York, NY, USA, 255–264.
    <https://doi.org/10.1145/3209108.3209197>

[^3]:
    Andrej Bauer, Jason Gross, Peter LeFanu Lumsdaine, Michael Shulman, Matthieu Sozeau, and Bas Spitters.
    2017. _The HoTT library: a formalization of homotopy type theory in Coq_.
    In Proceedings of the 6th ACM SIGPLAN Conference on Certified Programs and Proofs (CPP 2017).
    Association for Computing Machinery, New York, NY, USA, 164–172.
    <https://doi.org/10.1145/3018610.3018615>

[^4]:
    Floris van Doorn, Jakob von Raumer & Ulrik Buchholtz.
    2017. _Homotopy Type Theory in Lean_.
    In: Ayala-Rincón, M., Muñoz, C.A. (eds) Interactive Theorem Proving. ITP 2017.
    Lecture Notes in Computer Science(), vol 10499. Springer, Cham.
    <https://doi.org/10.1007/978-3-319-66107-0_30>
