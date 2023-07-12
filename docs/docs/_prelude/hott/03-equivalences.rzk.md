# 3. Equivalences

This is a literate `rzk` file:

```rzk
#lang rzk-1

#require-file "docs/docs/_prelude/hott/00-common.rzk.md"
#require-file "docs/docs/_prelude/hott/01-paths.rzk.md"
#require-file "docs/docs/_prelude/hott/02-homotopies.rzk.md"
```

## Sections, retractions, and equivalences

```rzk
#section is-equiv

#variables A B : U

#def has-section
  ( f : A -> B)
  : U
  := Σ (s : B -> A) , (homotopy B B (composition B A B f s) (identity B))

#def has-retraction
  ( f : A -> B)
  : U
  := Σ (r : B -> A) , (homotopy A A (composition A B A r f) (identity A))

-- equivalences are bi-invertible maps
#def is-equiv
  ( f : A -> B)
  : U
  := product (has-retraction f) (has-section f)

#end is-equiv
```

## Equivalence data

```rzk
#section equivalence-data

#variables A B : U
#variable f : A -> B
#variable is-equiv-f : is-equiv A B f

#def is-equiv-section uses (f)
  : B -> A
  := first (second is-equiv-f)

#def is-equiv-retraction uses (f)
  : B -> A
  := first (first is-equiv-f)

-- the homotopy between the section and retraction of an equivalence
#def homotopic-inverses-is-equiv uses (f)
  : homotopy B A is-equiv-section is-equiv-retraction
  :=
    homotopy-composition B A
      ( is-equiv-section)
      ( triple-composition B A B A is-equiv-retraction f is-equiv-section)
      ( is-equiv-retraction)
      ( homotopy-rev B A
        ( triple-composition B A B A is-equiv-retraction f is-equiv-section)
        ( is-equiv-section)
        ( homotopy-prewhisker B A A
          ( composition A B A is-equiv-retraction f)
          ( identity A)
          ( second (first is-equiv-f))
          ( is-equiv-section)))
      ( homotopy-postwhisker B B A
        ( composition B A B f is-equiv-section)
        ( identity B)
        ( second (second is-equiv-f))
        ( is-equiv-retraction))

#end equivalence-data
```

## Invertible maps

```rzk
-- the following type of more coherent equivalences is not a proposition
#def has-inverse
  ( A B : U)
  ( f : A -> B)
  : U
  :=
    Σ ( g : B -> A) ,
      ( product
        ( homotopy A A (composition A B A g f) (identity A))
        -- The retracting homotopy
        ( homotopy B B (composition B A B f g) (identity B)))
        -- The section homotopy
```

## Equivalences are invertible maps

```rzk
-- invertible maps are equivalences
#def is-equiv-has-inverse
  ( A B : U)
  ( f : A -> B)
  ( has-inverse-f : has-inverse A B f)
  : is-equiv A B f
  :=
    ( ( first has-inverse-f , first (second has-inverse-f)) ,
      ( first has-inverse-f , second (second has-inverse-f)))

-- equivalences are invertible
#def has-inverse-is-equiv
  ( A B : U)
  ( f : A -> B)
  ( is-equiv-f : is-equiv A B f)
  : has-inverse A B f
  :=
    ( is-equiv-section A B f is-equiv-f ,
      ( homotopy-composition A A
        ( composition A B A (is-equiv-section A B f is-equiv-f) f)
        ( composition A B A (is-equiv-retraction A B f is-equiv-f) f)
        ( identity A)
        ( homotopy-prewhisker A B A
          ( is-equiv-section A B f is-equiv-f)
          ( is-equiv-retraction A B f is-equiv-f)
          ( homotopic-inverses-is-equiv A B f is-equiv-f)
          ( f))
        ( second (first is-equiv-f)) ,
      ( second (second is-equiv-f))))
```

## Invertible map data

```rzk
#section has-inverse-data

#variables A B : U
#variable f : A -> B
#variable has-inverse-f : has-inverse A B f

-- The inverse of a map with an inverse
#def has-inverse-inverse uses (f)
  : B -> A
  := first (has-inverse-f)

-- Some iterated composites associated to a pair of invertible maps.
#def has-inverse-retraction-composite uses (B has-inverse-f)
  : A -> A
  := composition A B A has-inverse-inverse f

#def has-inverse-section-composite uses (A has-inverse-f)
  : B -> B
  := composition B A B f has-inverse-inverse

-- This composite is parallel to f; we won't need the dual notion.
#def has-inverse-triple-composite uses (has-inverse-f)
  : A -> B
  := triple-composition A B A B f has-inverse-inverse f

-- This composite is also parallel to f; again we won't need the dual notion.
#def has-inverse-quintuple-composite uses (has-inverse-f)
  : A -> B
  := \ a -> f (has-inverse-inverse (f (has-inverse-inverse (f a))))
#end has-inverse-data
```

## Composing equivalences

The type of equivalences between types uses is-equiv rather than has-inverse.

```rzk
#def Equiv
  ( A B : U)
  : U
  := Σ (f : A -> B) , (is-equiv A B f)
```

The data of an equivalence is not symmetric so we promote an equivalence to an
invertible map to prove symmetry.

```rzk
#def inv-equiv
  ( A B : U)
  ( e : Equiv A B)
  : Equiv B A
  :=
    ( first (has-inverse-is-equiv A B (first e) (second e)) ,
      ( ( first e ,
          second (second (has-inverse-is-equiv A B (first e) (second e)))) ,
        ( first e ,
        first (second (has-inverse-is-equiv A B (first e) (second e))))))
```

Composition of equivalences in diagrammatic order.

```rzk
#def comp-equiv
  ( A B C : U)
  ( A≃B : Equiv A B)
  ( B≃C : Equiv B C)
  : Equiv A C
  :=
    ( \ a -> (first B≃C) ((first A≃B) a) , -- the composite equivalence
      ( ( \ c ->
          ( first (first (second A≃B))) ((first (first (second (B≃C)))) c) ,
          ( \ a ->
            concat A
              ( (first (first (second A≃B)))
                ((first (first (second B≃C)))
                ((first B≃C) ((first A≃B) a))))
              ( (first (first (second A≃B))) ((first A≃B) a))
              ( a)
              ( ap B A
                ( (first (first (second B≃C))) ((first B≃C) ((first A≃B) a)))
                  -- should be inferred
                ( (first A≃B) a) -- should be inferred
                ( first (first (second A≃B)))
                ( (second (first (second B≃C))) ((first A≃B) a)))
              ( (second (first (second A≃B))) a))) ,
                ( \ c ->
                  ( first (second (second A≃B)))
                  ( (first (second (second (B≃C)))) c) ,
          ( \ c ->
            concat C
              ( (first B≃C) ((first A≃B) ((first (second (second A≃B)))
                ((first (second (second B≃C))) c))))
              ( (first B≃C) ((first (second (second B≃C))) c))
              ( c)
              ( ap B C
                ( (first A≃B) ((first (second (second A≃B)))
                  ((first (second (second B≃C))) c))) -- should be inferred
                ( (first (second (second B≃C))) c) -- should be inferred
                ( first B≃C)
                ( (second (second (second A≃B)))
                  ((first (second (second B≃C))) c)))
              ( (second (second (second B≃C))) c)))))

-- now we compose the functions that are equivalences
#def compose-is-equiv
  ( A B C : U)
  ( f : A -> B)
  ( is-equiv-f : is-equiv A B f)
  ( g : B -> C)
  ( is-equiv-g : is-equiv B C g)
  : is-equiv A C (composition A B C g f)
  :=
    ( ( composition C B A
      ( is-equiv-retraction A B f is-equiv-f)
      ( is-equiv-retraction B C g is-equiv-g) ,
      ( \ a ->
        concat A
          ( (is-equiv-retraction A B f is-equiv-f)
            ((is-equiv-retraction B C g is-equiv-g) (g (f a))))
          ( (is-equiv-retraction A B f is-equiv-f) (f a))
          ( a)
          ( ap B A
            ( (is-equiv-retraction B C g is-equiv-g) (g (f a))) -- should be inferred
            ( f a) -- should be inferred
            ( is-equiv-retraction A B f is-equiv-f)
            ( (second (first is-equiv-g)) (f a)))
          ( (second (first is-equiv-f)) a))) ,
      ( composition C B A
        ( is-equiv-section A B f is-equiv-f)
        ( is-equiv-section B C g is-equiv-g) ,
        ( \ c ->
          concat C
            ( g (f ((first (second is-equiv-f)) ((first (second is-equiv-g)) c))))
            ( g ((first (second is-equiv-g)) c))
            ( c)
            ( ap B C
              ( f ((first (second is-equiv-f)) ((first (second is-equiv-g)) c)))
                                -- should be inferred
              ( (first (second is-equiv-g)) c) -- should be inferred
              ( g)
              ( (second (second is-equiv-f)) ((first (second is-equiv-g)) c)))
                ((second (second is-equiv-g)) c))))

-- Right cancellation of equivalences in diagrammatic order.
#def right-cancel-equiv
  ( A B C : U)
  ( A≃C : Equiv A C)
  ( B≃C : Equiv B C)
  : Equiv A B
  := comp-equiv A C B (A≃C) (inv-equiv B C B≃C)

-- Left cancellation of equivalences in diagrammatic order.
#def left-cancel-equiv
  ( A B C : U)
  ( A≃B : Equiv A B)
  ( A≃C : Equiv A C)
  : Equiv B C
  := comp-equiv B A C (inv-equiv A B A≃B) (A≃C)

-- a composition of three equivalences
#def triple-comp-equiv
  ( A B C D : U)
  ( A≃B : Equiv A B)
  ( B≃C : Equiv B C)
  ( C≃D : Equiv C D)
  : Equiv A D
  := comp-equiv A B D (A≃B) (comp-equiv B C D B≃C C≃D)

#def triple-compose-is-equiv
  ( A B C D : U)
  ( f : A -> B)
  ( is-equiv-f : is-equiv A B f)
  ( g : B -> C)
  ( is-equiv-g : is-equiv B C g)
  ( h : C -> D)
  ( is-equiv-h : is-equiv C D h)
  : is-equiv A D (triple-composition A B C D h g f)
  :=
    compose-is-equiv A B D
      ( f)
      ( is-equiv-f)
      ( composition B C D h g)
      ( compose-is-equiv B C D g is-equiv-g h is-equiv-h)
```

## Equivalences and homotopy

If a map is homotopic to an equivalence it is an equivalence.

```rzk
#def is-equiv-homotopic-is-equiv
  ( A B : U)
  ( f g : A -> B)
  ( H : homotopy A B f g)
  ( is-equiv-g : is-equiv A B g)
  : is-equiv A B f
  :=
    ( ( ( first (first is-equiv-g)) ,
        ( \ a ->
          concat A
            ( (first (first is-equiv-g)) (f a))
            ( (first (first is-equiv-g)) (g a))
            ( a)
            ( ap B A (f a) (g a) (first (first is-equiv-g)) (H a))
            ( (second (first is-equiv-g)) a))) ,
      ( ( first (second is-equiv-g)) ,
        ( \ b ->
          concat B
            ( f ((first (second is-equiv-g)) b))
            ( g ((first (second is-equiv-g)) b))
            ( b)
            ( H ((first (second is-equiv-g)) b))
            ( (second (second is-equiv-g)) b))))

#def is-equiv-rev-homotopic-is-equiv
  ( A B : U)
  ( f g : A -> B)
  ( H : homotopy A B f g)
  ( is-equiv-f : is-equiv A B f)
  : is-equiv A B g
  := is-equiv-homotopic-is-equiv A B g f (homotopy-rev A B f g H) is-equiv-f
```

## Function extensionality

By path induction, an identification between functions defines a homotopy

```rzk
#def htpy-eq
  ( X : U)
  ( A : X -> U)
  ( f g : (x : X) -> A x)
  ( p : f = g)
  : (x : X) -> (f x = g x)
  :=
    idJ
    ( ( (x : X) -> A x) ,
      ( f) ,
      ( \ g' p' -> (x : X) -> (f x = g' x)) ,
      ( \ x -> refl) ,
      ( g) ,
      ( p))
```

The function extensionality axiom asserts that this map defines a family of
equivalences.

```rzk
-- The type that encodes the function extensionality axiom.
#def FunExt : U
  :=
    ( X : U) ->
    ( A : X -> U) ->
    ( f : (x : X) -> A x) ->
    ( g : (x : X) -> A x) ->
    is-equiv (f = g) ((x : X) -> f x = g x) (htpy-eq X A f g)

-- The equivalence provided by function extensionality.
#def FunExt-equiv
  ( funext : FunExt)
  ( X : U)
  ( A : X -> U)
  ( f g : (x : X) -> A x)
  : Equiv (f = g) ((x : X) -> f x = g x)
  := (htpy-eq X A f g , funext X A f g)

-- In particular, function extensionality implies that homotopies give rise to identifications. This defines eq-htpy to be the retraction to htpy-eq.
#def eq-htpy
  ( funext : FunExt)
  ( X : U)
  ( A : X -> U)
  ( f g : (x : X) -> A x)
  : ((x : X) -> f x = g x) -> (f = g)
  := first (first (funext X A f g))

-- Using function extensionality, a fiberwise equivalence defines an equivalence of dependent function types
#def equiv-function-equiv-fibered
  ( funext : FunExt)
  ( X : U)
  ( A B : X -> U)
  ( fibequiv : (x : X) -> Equiv (A x) (B x))
  : Equiv ((x : X) -> A x) ((x : X) -> B x)
  :=
    ( ( \ a x -> (first (fibequiv x)) (a x)) ,
      ( ( ( \ b x -> (first (first (second (fibequiv x)))) (b x)) ,
          ( \ a ->
            eq-htpy
              funext X A
              ( \ x ->
                (first (first (second (fibequiv x))))
                  ((first (fibequiv x)) (a x)))
              ( a)
              ( \ x -> (second (first (second (fibequiv x)))) (a x)))) ,
        ( ( \ b x -> (first (second (second (fibequiv x)))) (b x)) ,
          ( \ b ->
            eq-htpy
              funext X B
              ( \ x ->
                (first (fibequiv x))
                  ((first (second (second (fibequiv x)))) (b x)))
              ( b)
              ( \ x -> (second (second (second (fibequiv x)))) (b x))))))
```

## Embeddings

```rzk
#def is-emb
  ( A B : U)
  ( f : A -> B)
  : U
  := (x : A) -> (y : A) -> is-equiv (x = y) (f x = f y) (ap A B x y f)

#def Emb
  ( A B : U)
  : U
  := (Σ (f : A -> B) , is-emb A B f)

#def is-emb-is-inhabited-emb
  ( A B : U)
  ( f : A -> B)
  ( e : A -> is-emb A B f)
  : is-emb A B f
  := \ x y -> e x x y

#def inv-ap-is-emb
  ( A B : U)
  ( f : A -> B)
  ( is-emb-f : is-emb A B f)
  ( x y : A)
  ( p : f x = f y)
  : (x = y)
  := first (first (is-emb-f x y)) p
```

## Reversal is an equivalence

```rzk
#def has-retraction-rev
  ( A : U)
  ( y : A)
  : (x : A) -> has-retraction (x = y) (y = x) ((\ p -> ((rev A x y) p)))
  :=
    \ x ->
    ( ( rev A y x) ,
      ( \ p ->
        idJ
        ( A ,
          x ,
          ( \ y' p' ->
            ( composition
              ( x = y') (y' = x) (x = y') (rev A y' x) (rev A x y') (p'))
            =_{x = y'}
            ( p')) ,
          ( refl) ,
          ( y) ,
          ( p))))

#def has-section-rev
  ( A : U)
  ( y : A)
  : (x : A) -> has-section (x = y) (y = x) ((\ p -> ((rev A x y) p)))
  :=
    \ x ->
    ( ( rev A y x) ,
      ( \ p ->
        idJ
        ( A ,
          y ,
          ( \ x' p' ->
            ( composition
              ( y = x') (x' = y) (y = x') (rev A x' y) (rev A y x') (p'))
            =_{y = x'}
            ( p')) ,
          ( refl) ,
          ( x) ,
          ( p))))

#def is-equiv-rev
  ( A : U)
  ( x y : A)
  : is-equiv (x = y) (y = x) (rev A x y)
  := ((has-retraction-rev A y x) , (has-section-rev A y x))
```
