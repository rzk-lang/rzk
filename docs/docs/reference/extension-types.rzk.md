# Extension types

```rzk
#lang rzk-1
```

4. Extension types \(\left\langle \prod*{t : I \mid \psi} A \vert ^{\phi} *{a} \right\rangle\) are written as `#!rzk {t : I | psi t} -> A [ phi |-> a ]`

   - specifying `#!rzk [ phi |-> a ]` is optional, semantically defaults to `#!rzk [ BOT |-> recBOT ]` (like in RSTT);
   - specifying `#!rzk psi` in `#!rzk {t : I | psi}` is mandatory;
   - values of function types are \(\lambda\)-abstractions written in one of the following ways:
     - `#!rzk \t -> <body>` — this is usually fine;
     - `#!rzk \{t : I | psi} -> <body>` — this sometimes helps the typechecker;

5. Types of functions from a shape \(\prod\_{t : I \mid \psi} A\) are a specialised variant of extension types and are written `#!rzk {t : I | psi} -> A`
   - specifying the name of the argument is mandatory; i.e. `#!rzk {I | psi} -> A` is invalid syntax!
   - values of function types are \(\lambda\)-abstractions written in one of the following ways:
     - `#!rzk \t -> <body>` — this is usually fine;
     - `#!rzk \{t : I | psi} -> <body>` — this sometimes helps the typechecker;

[^1]: Emily Riehl & Michael Shulman. _A type theory for synthetic ∞-categories._ Higher Structures 1(1), 147-224. 2017. <https://arxiv.org/abs/1705.07442>
