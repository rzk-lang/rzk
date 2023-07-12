# 6. Contractible

This is a literate `rzk` file:

```rzk
#lang rzk-1

#require-file "docs/docs/_prelude/hott/00-common.rzk.md"
#require-file "docs/docs/_prelude/hott/01-paths.rzk.md"
#require-file "docs/docs/_prelude/hott/02-homotopies.rzk.md"
#require-file "docs/docs/_prelude/hott/03-equivalences.rzk.md"
```

## Contractible types

```rzk
-- contractible types
#def is-contr (A : U) : U
  := Σ (x : A) , ((y : A) -> x = y)
```

## Contractible type data

```rzk
#section contractible-data

#variable A : U
#variable is-contr-A : is-contr A

#def contraction-center
  : A
  := (first is-contr-A)

-- The path from the contraction center to any point.
#def contracting-htpy
  : ( z : A) -> contraction-center = z
  := second is-contr-A

#def contracting-htpy-realigned uses (is-contr-A)
  : ( z : A) -> contraction-center = z
  :=
    \ z ->
      ( concat A contraction-center contraction-center z
          (rev A contraction-center contraction-center
            ( contracting-htpy contraction-center))
          (contracting-htpy z))

#def contracting-htpy-realigned-path uses (is-contr-A)
  : ( contracting-htpy-realigned contraction-center) = refl
  :=
    ( left-inverse-concat A contraction-center contraction-center
      ( contracting-htpy contraction-center))

-- A path between an arbitrary pair of types in a contractible type.
#def contractible-connecting-htpy uses (is-contr-A)
  (x y : A)
  : x = y
  :=
    zag-zig-concat A x contraction-center y
    ( contracting-htpy x) (contracting-htpy y)

#end contractible-data
```

## Unit type

The prototypical contractible type is the unit type, which is built-in to rzk.

```rzk
#def ind-unit
  ( C : Unit -> U)
  ( C-unit : C unit)
  ( x : Unit)
  : C x
  := C-unit

#def is-prop-unit
  ( x y : Unit)
  : x = y
  := refl

-- Terminal projection as a constant map
#def terminal-map
  ( A : U)
  : A -> Unit
  := constant A Unit unit
```

## Identity types of unit types

```rzk
#def terminal-map-of-path-types-of-Unit-has-retr
  ( x y : Unit)
  : has-retraction (x = y) Unit (terminal-map (x = y))
  :=
    ( \ a -> refl ,
      \ p -> idJ (Unit , x , \ y' p' -> refl =_{x = y'} p' , refl , y , p))

#def terminal-map-of-path-types-of-Unit-has-sec
  ( x y : Unit)
  : has-section (x = y) Unit (terminal-map (x = y))
  := ( \ a -> refl , \ a -> refl)

#def terminal-map-of-path-types-of-Unit-is-equiv
  ( x y : Unit)
  : is-equiv (x = y) Unit (terminal-map (x = y))
  :=
    ( terminal-map-of-path-types-of-Unit-has-retr x y ,
      terminal-map-of-path-types-of-Unit-has-sec x y)
```

## Characterization of contractibility

A type is contractible if and only if its terminal map is an equivalence.

```rzk
#def terminal-map-is-equiv
  ( A : U)
  : U
  := is-equiv A Unit (terminal-map A)

#def contr-implies-terminal-map-is-equiv-retr
  ( A : U)
  ( is-contr-A : is-contr A)
  : has-retraction A Unit (terminal-map A)
  :=
    ( constant Unit A (contraction-center A is-contr-A) ,
      \ y -> (contracting-htpy A is-contr-A) y)

#def contr-implies-terminal-map-is-equiv-sec
  ( A : U)
  ( is-contr-A : is-contr A)
  : has-section A Unit (terminal-map A)
  := ( constant Unit A (contraction-center A is-contr-A) , \ z -> refl)

#def contr-implies-terminal-map-is-equiv
  ( A : U)
  ( is-contr-A : is-contr A)
  : is-equiv A Unit (terminal-map A)
  :=
    ( contr-implies-terminal-map-is-equiv-retr A is-contr-A ,
      contr-implies-terminal-map-is-equiv-sec A is-contr-A)

#def terminal-map-is-equiv-implies-contr
  ( A : U)
  (e : terminal-map-is-equiv A)
  : is-contr A
  := ( (first (first e)) unit ,
       (second (first e)))

#def contr-iff-terminal-map-is-equiv
  ( A : U)
  : iff (is-contr A) (terminal-map-is-equiv A)
  :=
    ( ( contr-implies-terminal-map-is-equiv A) ,
      ( terminal-map-is-equiv-implies-contr A))

#def equiv-with-contractible-domain-implies-contractible-codomain
  ( A B : U)
  ( f : Equiv A B)
  ( is-contr-A : is-contr A)
  : is-contr B
  := ( terminal-map-is-equiv-implies-contr B
      ( second
        ( comp-equiv B A Unit
          ( inv-equiv A B f)
          ( (terminal-map A) ,
            ( contr-implies-terminal-map-is-equiv A is-contr-A)))))

#def equiv-with-contractible-codomain-implies-contractible-domain
  ( A B : U)
  ( f : Equiv A B)
  ( is-contr-B : is-contr B)
  : is-contr A
  :=
    ( equiv-with-contractible-domain-implies-contractible-codomain B A
      ( inv-equiv A B f) is-contr-B)

#def equiv-then-domain-contractible-iff-codomain-contractible
  ( A B : U)
  ( f : Equiv A B)
  : ( iff (is-contr A) (is-contr B))
  :=
    ( \ is-contr-A ->
      ( equiv-with-contractible-domain-implies-contractible-codomain
        A B f is-contr-A) ,
      \ is-contr-B ->
      ( equiv-with-contractible-codomain-implies-contractible-domain
        A B f is-contr-B))

#def path-types-of-Unit-are-contractible
  ( x y : Unit)
  : is-contr (x = y)
  :=
    ( terminal-map-is-equiv-implies-contr
      ( x = y) (terminal-map-of-path-types-of-Unit-is-equiv x y))
```

## Retracts of contractible types

A retract of contractible types is contractible.

```rzk
-- A type that records a proof that A is a retract of B.
-- Very similar to has-retraction.
#def is-retract-of
  ( A B : U)
  : U
  := Σ ( s : A -> B) , has-retraction A B s

#section retraction-data

#variables A B : U
#variable AretractB : is-retract-of A B

#def is-retract-of-section
  : A -> B
  := first AretractB

#def is-retract-of-retraction
  : B -> A
  := first (second AretractB)

#def is-retract-of-homotopy
  : homotopy A A (composition A B A is-retract-of-retraction is-retract-of-section) (identity A)
  := second (second AretractB)

-- If A is a retract of a contractible type it has a term.
#def is-retract-of-is-contr-isInhabited uses (AretractB)
  ( is-contr-B : is-contr B)
  : A
  := is-retract-of-retraction (contraction-center B is-contr-B)

-- If A is a retract of a contractible type it has a contracting homotopy.
#def is-retract-of-is-contr-hasHtpy uses (AretractB)
  ( is-contr-B : is-contr B)
  ( a : A)
  : ( is-retract-of-is-contr-isInhabited is-contr-B) = a
  := concat
      A
      ( is-retract-of-is-contr-isInhabited is-contr-B)
      ( (composition A B A is-retract-of-retraction is-retract-of-section) a)
      a
      ( ap B A (contraction-center B is-contr-B) (is-retract-of-section a)
        ( is-retract-of-retraction)
        ( contracting-htpy B is-contr-B (is-retract-of-section a)))
      ( is-retract-of-homotopy a)

-- If A is a retract of a contractible type it is contractible.
#def is-retract-of-is-contr-is-contr uses (AretractB)
  ( is-contr-B : is-contr B)
  : is-contr A
  :=
    ( is-retract-of-is-contr-isInhabited is-contr-B ,
      is-retract-of-is-contr-hasHtpy is-contr-B)

#end retraction-data
```

## Functions between contractible types

A function between contractible types is an equivalence

```rzk
#def is-equiv-are-contr
  ( A B : U)
  ( is-contr-A : is-contr A)
  ( is-contr-B : is-contr B)
  ( f : A -> B)
  : is-equiv A B f
  :=
    ( ( \ b -> contraction-center A is-contr-A ,
        \ a -> contracting-htpy A is-contr-A a) ,
      ( \ b -> contraction-center A is-contr-A ,
        \ b -> contractible-connecting-htpy B is-contr-B
                (f (contraction-center A is-contr-A)) b))
```

A type equivalent to a contractible type is contractible.

```rzk
#def is-contr-is-equiv-to-contr
  ( A B : U)
  ( e : Equiv A B)
  ( is-contr-B : is-contr B)
  : is-contr A
  :=
    is-retract-of-is-contr-is-contr A B (first e , first (second e)) is-contr-B

#def is-contr-is-equiv-from-contr
  ( A B : U)
  ( e : Equiv A B)
  ( is-contr-A : is-contr A)
  : is-contr B
  := is-retract-of-is-contr-is-contr B A
      ( first (second (second e)) , (first e , second (second (second e))))
      ( is-contr-A)
```
