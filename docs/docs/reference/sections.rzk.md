# Sections and Variables

Sections and variables allow to simplify definitions by factoring out common assumptions.

!!! info "Coq-style variables"

    `rzk` implements variables similarly to
    <a href="https://coq.inria.fr/refman/language/core/assumptions.html#coq:cmd.Variable" target="_blank">`Variable` command in Coq</a>.
    An important difference is that `rzk` does not allow definitions to use variables implicitly and adds `uses (...)` annotations to ensure such dependencies are not accidental.
    This is, perhaps, somewhat related to <a href="https://coq.inria.fr/refman/proofs/writing-proofs/equality.html#coq:exn.Section-variable-‘ident’-occurs-implicitly-in-global-declaration-‘qualid’-present-in-hypothesis-‘ident’" target="_blank">this error message in Coq</a>.

This is a literate `rzk` file:

```rzk
#lang rzk-1
```

## Variables

Consider the following definitions:

```rzk
#define compose₁
  (A B C : U)
  (g : B -> C)
  (f : A -> B)
  : A -> C
  := \x -> g (f x)

#define twice₁
  (A : U)
  (h : A -> A)
  : A -> A
  := \x -> h (h x)
```

Since it might be common to introduce types `A`, `B`, and `C`, we can declare these are variables:

```rzk
#variables A B C : U

#define compose₂
  (g : B -> C)
  (f : A -> B)
  : A -> C
  := \x -> g (f x)

#define twice₂
  (h : A -> A)
  : A -> A
  := \x -> h (h x)
```

The `#variables` command here introduces assumptions, which can be used in the following definitions. Importantly, after checking a file (module), all definitions will have the assumptions used (explicitly or implicitly) attached as bound variables.

### Implicitly used variables (and `uses`)

We can try going even further and declare variables `f`, `g`, `h`, and `x`:

```rzk
#variable g : B -> C
#variable f : A -> B
#variable h : A -> A
#variable x : A

-- #define bad-compose₃ : C := g (f x)  -- ERROR: implicit assumptions A and B
#define twice₃ : A := h (h x)
```

Note how this definition of `bad-compose₃` is implicitly dependent on the types `A` and `B`, which is promptly noted by `rzk`, which issues an error (if we uncomment the corresponding line):

```text
implicit assumption
  B : U
used in definition of
  bad-compose₃
```

To let `rzk` know that this is not accidental, we can add `uses (...)` annotation to specify a list of variables implicitly used in the definition:

```rzk
#define compose₃ uses (A B) : C := g (f x)
```

## Sections

To introduce assumption variables temporarily inside of one file, you can use sections:

```rzk
#section example-1

#variables X Y Z : U
#variable k : X -> X
#variable x' : X

#define compose₄
  (g : Y -> Z)
  (f : X -> Y)
  : X -> Z
  := \x -> g (f x)

#define twice₄ : X := k (k x')

#end example-1
```

Now, once outside of the section, `compose₄` and `twice₄` obtain corresponding parameters
(only those used, explicitly or implicitly):

```rzk
-- compose₄ : (X : U) -> (Y : U) -> (Z : U) -> (g : Y -> Z) -> (f : X -> Y) -> (X -> Z)
-- twice₄ : (X : U) -> (k : X -> X) -> (x' : X) -> X

#define twice₅
  (T : U)
  (e : T -> T)
  : T -> T
  := compose₄ T T T e e

#define identity
  (T : U)
  : T -> T
  := twice₄ T (\t -> t)
```

!!! warning "Lack of indentation"

    `rzk` currently does not support indentation, so all definitions and commands inside a section (including nested sections) have to start at the beginning of a line.
