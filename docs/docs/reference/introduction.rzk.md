# Introduction

`rzk` is an experimental proof assistant for synthetic ∞-categories.
`rzk-1` is an early version of the language supported by `rzk`.
The language is based on Riehl and Shulman's «Type Theory for Synthetic ∞-categories»[^1]. In this section, we introduce syntax, discuss features and some of the current limitations of the proof assistant.

Overall, a program in `rzk-1` consists of a language pragma (specifying that we use `rzk-1` and not one of the other languages[^2]) followed by a sequence of commands. For now, we will only use `#define` command.

Here is a small formalisation in an MLTT subset of `rzk-1`:

```rzk
#lang rzk-1

-- Flipping the arguments of a function.
#define flip
    (A B : U)                         -- For any types A and B
    (C : (x : A) -> (y : B) -> U)     -- and a type family C
    (f : (x : A) -> (y : B) -> C x y) -- given a function f : A -> B -> C
  : (y : B) -> (x : A) -> C x y       -- we construct a function of type B -> A -> C
  := \y x -> f x y    -- by swapping the arguments

-- Flipping a function twice is the same as not doing anything
#define flip-flip-is-id
    (A B : U)                         -- For any types A and B
    (C : (x : A) -> (y : B) -> U)     -- and a type family C
    (f : (x : A) -> (y : B) -> C x y) -- given a function f : A -> B -> C
  : f = flip B A (\y x -> C x y)
          (flip A B C f)              -- flipping f twice is the same as f
  := refl                             -- proof by reflexivity
```

Let us explain parts of this code:

1. `#!rzk #lang rzk-1` specifies that we are in using `#!rzk rzk-1` language;
2. `#!rzk --` starts a comment line (until the end of the line);
3. `#!rzk #define «name» : «type» := «term»` defines a name `«name»` to be equal to `«term»`; the proof assistant will typecheck `«term»` against type `«type»`;
4. We define two terms here — `flip` and `flip-flip-is-id`;
5. `flip` is a function that takes 4 arguments and returns a function of two arguments.
6. `flip-flip-is-id` is a function that takes two types, a type family, and a function `f` and returns a value of an identity type `flip ... (flip ... f) = f`, indicating that flipping a function `f` twice gets us back to `f`.

Similarly to the three layers in Riehl and Shulman's type theory, `rzk-1` has 3 universes:

- `CUBE` is the universe of cubes, corresponding to the cube layer;
- `TOPE` is the universe of topes, corresponding to the tope layer;
- `U` is the universe of types, corresponding to the types and terms layer.

These are explained in the following sections.

## Soundness

`rzk-1` assumes "type-in-type", that is `U` has type `U`.
This is known to make the type system unsound (due to Russell and Curry-style paradoxes), however,
it is sometimes considered acceptable in proof assistants.
And, since it simplifies implementation, `rzk-1` embraces this assumption, at least for now.

Moreover, `rzk-1` does not prevent cubes or topes to depend on types and terms. For example, the following definition typechecks:

```rzk
#define weird
    (A : U)
    (I : A -> CUBE)
    (x y : A)
  : CUBE
  := I x * I y
```

This likely leads to another inconsistency, but it will probably not lead to bugs in actual proofs of interest,
so current version embraces this lax treatment of universes.

[^1]: Emily Riehl & Michael Shulman. _A type theory for synthetic ∞-categories._ Higher Structures 1(1), 147-224. 2017. <https://arxiv.org/abs/1705.07442>

[^2]: In version [:octicons-tag-24: v0.1.0](https://github.com/fizruk/rzk/releases/tag/v0.1.0), `rzk` has supported simply typed lambda calculus, PCF, and MLTT. However, those languages have been removed.
