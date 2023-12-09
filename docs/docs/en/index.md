# Rzk proof assistant

`rzk` is an experimental proof assistant for ∞-categories,
based on Riehl and Shulman's «Type Theory for Synthetic ∞-categories»[^1].

[^1]:
    Emily Riehl & Michael Shulman. A type theory for synthetic ∞-categories.
    Higher Structures 1(1), 147-224. 2017. <https://arxiv.org/abs/1705.07442>

[Get started with Rzk](getting-started/install.md){ .md-button .md-button--primary }
[Try Rzk Playground](playground/){ .md-button }

## About this project

This project has started with the idea of bringing Riehl and Shulman's 2017 paper [1] to "life" by implementing a proof assistant based on their type theory with shapes. Currently an early prototype with an [online playground](https://rzk-lang.github.io/rzk/develop/playground/) is available. The current implementation is capable of checking various formalisations. Perhaps, the largest formalisations are available in two related projects: <https://rzk-lang.github.com/sHoTT> and <https://github.com/emilyriehl/yoneda>. `sHoTT` project (originally a fork of the yoneda project) aims to cover more formalisations in simplicial HoTT and ∞-categories, while `yoneda` project aims to compare different formalisations of the Yoneda lemma.

Internally, `rzk` uses a version of second-order abstract syntax allowing relatively straightforward handling of binders (such as lambda abstraction). In the future, `rzk` aims to support dependent type inference relying on E-unification for second-order abstract syntax [2].
Using such representation is motivated by automatic handling of binders and easily automated boilerplate code. The idea is that this should keep the implementation of `rzk` relatively small and less error-prone than some of the existing approaches to implementation of dependent type checkers.

An important part of `rzk` is a tope layer solver, which is essentially a theorem prover for a part of the type theory. A related project, dedicated just to that part is available at <https://github.com/fizruk/simple-topes>. `simple-topes` supports used-defined cubes, topes, and tope layer axioms. Once stable, `simple-topes` will be merged into `rzk`, expanding the proof assistant to the type theory with shapes, allowing formalisations for (variants of) cubical, globular, and other geometric versions of HoTT.

See the list of contributors at [CONTRIBUTORS.md](CONTRIBUTORS.md).

## Discussing Rzk and getting help

A Zulip chat is available for all to join and chat about Rzk, including formalization projects, development of Rzk, and related projects:

[Join Rzk Zulip](https://rzk-lang.zulipchat.com/register/){ .md-button .md-button--primary }
