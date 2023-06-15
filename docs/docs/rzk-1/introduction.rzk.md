# Introduction to `rzk-1`

!!! warning "Work-in-progress"
    The documentation is not yet up-to-date with all
    the changes introduced in `rzk-0.2.0`. <br>
    See [`rzk` changelog](https://github.com/fizruk/rzk/blob/release-v0.3.0/rzk/ChangeLog.md#v020---2022-04-20) for more details.

`rzk` is an experimental proof assistant for synthetic ∞-categories.
`rzk-1` is an early version of the language supported by `rzk`.
The language is based on Riehl and Shulman's «Type Theory for Synthetic ∞-categories» ([https://arxiv.org/abs/1705.07442](https://arxiv.org/abs/1705.07442)). We will refer to Riehl and Shulman's Type Theory as RSTT. In this section, we introduce syntax, discuss features and some of the current limitations of the proof assistant.

Overall, a program in `rzk-1` consists of a language pragma (specifying that we use `rzk-1` and not one of the other languages) followed by a sequence of commands. For now, we will only use `#def` command.

Here is a small formalisation in an MLTT subset of `rzk-1`:

```rzk
#lang rzk-1

-- Flipping the arguments of a function.
#def flip
    (A B : U)                         -- For any types A and B
    (C : (x : A) -> (y : B) -> U)     -- and a type family C
    (f : (x : A) -> (y : B) -> C x y) -- given a function f : A -> B -> C
  : (y : B) -> (x : A) -> C x y       -- we construct a function of type B -> A -> C
  := \y x -> f x y    -- by swapping the arguments

-- Flipping a function twice is the same as not doing anything
#def flip-flip-is-id
    (A B : U)                         -- For any types A and B
    (C : (x : A) -> (y : B) -> U)     -- and a type family C
    (f : (x : A) -> (y : B) -> C x y) -- given a function f : A -> B -> C
  : f = flip B A (\y x -> C x y)
          (flip A B C f)              -- flipping f twice is the same as f
  := refl                             -- proof by reflexivity
```

Let us explain parts of this code:

1. `#lang rzk-1` specifies that we are in using `rzk-1` language;
2. `--` starts a comment line (until the end of the line);
3. `#def <name> : <type> := <term>` defines a name `<name>` to be equal to `<term>`; the proof assistant will typecheck `<term>` against type `<type>`;
4. We define two terms here — `flip` and `flip-flip-is-id`;
5. `flip` is a function that takes 4 arguments and returns a function of two arguments.
6. `flip-flip-is-id` is a function that takes two types, a type family, and a function `f` and returns a value of an identity type `flip ... (flip ... f) = f`, indicating that flipping a function `f` twice gets us back to `f`.

## Syntax

Similarly to the three layers in RSTT, `rzk-1` has 3 universes:

- `CUBE` is the universe of cubes, corresponding to the cube layer;
- `TOPE` is the universe of topes, corresponding to the tope layer;
- `U` is the universe of types, corresponding to the types and terms layer.

### Cube layer

All cubes live in `CUBE` universe.

There are two built-in cubes:

1. `1` cube is a unit cube with a single point `*_1`
2. `2` cube is a directed interval cube with points `0_2` and `1_2`

It is also possible to have `CUBE` variables and make products of cubes:

1. `I * J`  is a product of cubes `I` and `J`
2. `(t, s)` is a point in `I * J` if `t : I` and `s : J`
3. if `ts : I * J`, then `first ts : I` and `second ts : J`

You can usually use `(t, s)` both as a pattern, and a construction of a pair of points:

```rzk
-- Swap point components of a point in a cube I × I
#def swap
    (I : CUBE)
  : (I * I) -> I * I
  := \(t, s) -> (s, t)
```

### Tope layer

All topes live in `TOPE` universe.

Here are all the ways to build a tope:

1. Introduce a variable, e.g. `(psi : TOPE) -> ...`;

    - Usually, topes depend on point variables from some cube(s). To indicate that, we usually introduce topes as "functions" from some cube to `TOPE`. For example, `(psi : I -> TOPE) -> ...`.

2. Use a constant:

    - top tope \(\top\) is written `TOP`;
    - bottom tope \(\bot\) is written `BOT`;
    - tope conjunction \(\psi \land \phi\) is written `psi /\ phi`;
    - tope disjunction \(\psi \lor \phi\) is written `psi \/ phi`;
    - equality tope \(t \equiv s\) is written `t === s`, whenever `t` and `s` are points of the same cube;
    - inequality tope \(t \leq s\) is written `t <= s` whenever `t : 2` and `s : 2`.

### Types and terms

1. Function (dependent product) types \(\prod_{x : A} B\) are written `(x : A) -> B x`
    - values of function types are \(\lambda\)-abstractions written in one of the following ways:
        - `\x -> <body>` — this is usually fine;
        - `\(x : A) -> <body>` — this sometimes helps the typechecker.

2. Dependent sum type \(\sum_{x : A} B\) is written `∑ (x : A), B` or `Sigma (x : A), B`
    - values of dependent sum types are pairs written as `(x, y)`;
    - to access components of a dependent pair `p`, use `first p` and `second p`;
    - `first` and `second` are not valid syntax without an argument!

3. Identity (path) type \(x =_A y\) is written `x =_{A} y`
    - specifying the type `A` is optional: `x = y` is valid syntax!
    - the only value of an identity type is `refl_{x : A}` whose type is `x =_{A} x` whenever `x : A`
    - specifying term and type is optional: `refl_{x}` and `refl` are both valid syntax;
    - path induction is done using \(J\) path eliminator; for any type \(A\) and \(a : A\), type family
      \(C : \prod_{x : A} ((a =_A x) \to \mathcal{U})\)
      and \(d : C(a,\mathsf{refl}_a)\)
      and \(x : A\)
      and \(p : a =_A x\)
      we have \(\mathcal{J}(A, a, C, d, x, p) : C(x, p)\); in `rzk-1` we write
      `idJ(A, a, C, d, x, p)`;
    - `idJ` is not valid syntax without exactly 6-tuple provided as an argument!

4. Extension types \(\left\langle \prod_{t : I \mid \psi} A \vert ^{\phi} _{a} \right\rangle\) are written as `{t : I | psi t} -> A [ phi |-> a ]`
    - specifying `[ phi |-> a ]` is optional, semantically defaults to `[ BOT |-> recBOT ]` (like in RSTT);
    - specifying `psi` in `{t : I | psi}` is mandatory;
    - values of function types are \(\lambda\)-abstractions written in one of the following ways:
        - `\t -> <body>` or `λt → <body>` — this is usually fine;
        - `\{t : I | psi} -> <body>` or `λ{t : I | psi} -> <body>` — this sometimes helps the typechecker;

5. Types of functions from a shape \(\prod_{t : I \mid \psi} A\) are a specialised variant of extension types and are written `{t : I | psi} -> A`
    - specifying the name of the argument is mandatory; i.e. `{I | psi} -> A` is invalid syntax!
    - values of function types are \(\lambda\)-abstractions written in one of the following ways:
        - `\t -> <body>` or `λt → <body>` — this is usually fine;
        - `\{t : I | psi} -> <body>` or `λ{t : I | psi} -> <body>` — this sometimes helps the typechecker;

### Tope disjuction elimination

Following RSTT, `rzk-1` introduces two primitive terms for disjunction elimination:

1. `recBOT` (also written `rec⊥`) corresponds to \(\mathsf{rec}_\bot\), has any type, and is valid whenever tope context is included in `BOT`;
2. `recOR(psi, phi, a_psi, a_phi)` (also written `rec∨(psi, phi, a_psi, a_phi)`) corresponds to \(\mathsf{rec}_\lor^{\psi, \phi}(a_\psi, a_\phi)\), is well-typed when `a_psi` is definitionally equal to `a_phi` under `psi /\ phi`.

## Soundness

First of all, in `rzk-1` we have "type-in-type", that is `U` has type `U`.
This is known to make the type system unsound, however,
it is usually considered acceptable in proof assistants.
And, since it simplifies implementation, `rzk-1` follows this convention.

Additionally, unlike RSTT, `rzk-1` does not prevent cubes or topes to depend on types and terms. For example, the following definition typechecks:

```rzk
#def weird
	  (A : U)
    (I : A -> CUBE)
    (x y : A)
  : CUBE
  := I x * I y
```

This likely leads to another inconsistency, but it will hardly lead to bugs in actual proofs of interest,
so current version embraces this treatment of universes.

