# Rzk proof assistant

Rzk is an experimental proof assistant,
based on [«Type Theory for Synthetic ∞-categories»](https://arxiv.org/abs/1705.07442)[^1].

[Get started with Rzk](getting-started/install.md){ .md-button .md-button--primary }
[Try Rzk Playground](playground/index.html){ .md-button }

## About this project

This project has started with the idea of bringing Riehl and Shulman's 2017 paper[^1]
to "life" by implementing a proof assistant based on their type theory with shapes.
At the moment, Rzk provides a language that is close to the original paper,
as well as some tooling around it (such as a VS Code extension and a language server with syntax highlighting and formatting support).

### Formalizing ∞-category theory

A big portion of the original paper (up to the ∞-categorical Yoneda lemma) has been formalized in Rzk (see [Yoneda for ∞-categories](https://emilyriehl.github.io/yoneda/)[^2]).
More formalization results are under way (see [sHoTT](https://rzk-lang.github.io/sHoTT/)).
There are also some efforts to formalize the HoTT Book in Rzk (see [hottbook](https://rzk-lang.github.io/hottbook/)).

### Using Rzk

The recommended way of interacting with Rzk is via VS Code (see [Getting Started](getting-started/install.md)),
but you can also download binaries from [GitHub Releases](https://github.com/rzk-lang/rzk/releases), build [from sources](getting-started/install.md#install-from-sources),
or try setting up the Rzk Language Server with your editor of choice.
Additionally, for "throwaway" single-file formalizations, you can use [Rzk Online Playground](playground/index.html).

### Implementation

Rzk serves also as a playground for some techniques of developing proof assistants in Haskell.
In particular, it features a version of second-order abstract syntax for handling binders and,
in the future, dependent type inference through higher-order unification[^3] [^4].
The idea is ultimately, to provide higher-order unification and/or dependent type inference "as a library",
keeping the implementation of Rzk (at least its core language) small, maintainable, and safe.

Another important part of Rzk is the tope layer solver[^5],
which is a built-in intuitionistic theorem prover required for a part of the type theory.
Although its implementation is fairly simple,
it is sufficient to check existing proofs in synthetic ∞-categories
without requiring any explicit proofs for the tope layer.

Rzk and the tooling around it is developed by just a couple of people.
See the list of contributors at [CONTRIBUTORS.md](CONTRIBUTORS.md).

### Discussing Rzk and getting help

A Zulip chat is available for all to join and chat about Rzk, including formalization projects, development of Rzk, and related projects:

[Join Rzk Zulip](https://rzk-lang.zulipchat.com/register/){ .md-button .md-button--primary }

[^1]:
    Emily Riehl & Michael Shulman. _A type theory for synthetic ∞-categories_.
    Higher Structures 1(1), 147-224. 2017. <https://arxiv.org/abs/1705.07442>

[^2]:
    Nikolai Kudasov, Emily Riehl, Jonathan Weinberger.
    _Formalizing the ∞-categorical Yoneda lemma_. 2023. <https://arxiv.org/abs/2309.08340>

[^3]:
    Nikolai Kudasov. _Functional Pearl: Dependent type inference via free higher-order unification_. 2022.
    <https://arxiv.org/abs/2204.05653>

[^4]:
    Nikolai Kudasov. _E-Unification for Second-Order Abstract Syntax_. In 8th International Conference on Formal Structures for Computation and Deduction (FSCD 2023). Leibniz International Proceedings in Informatics (LIPIcs), Volume 260, pp. 10:1-10:22, Schloss Dagstuhl - Leibniz-Zentrum für Informatik (2023)
    <https://doi.org/10.4230/LIPIcs.FSCD.2023.10>

[^5]:
    Nikolai Kudasov. Experimental prover for Tope logic. SCAN 2023, pages 37–39. 2023.
    <https://www.mathnet.ru/ConfLogos/2220/abstracts.pdf#page=38>
