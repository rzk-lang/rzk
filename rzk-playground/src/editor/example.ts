export const example = `#lang rzk-1

-- A is contractible there exists x : A such that for any y : A we have x = y.
#def iscontr (A : U) : U
  := ∑ (a : A), (x : A) -> a =_{A} x

-- A is a proposition if for any x, y : A we have x = y
#def isaprop (A : U) : U
  := (x : A) -> (y : A) -> x =_{A} y

-- A is a set if for any x, y : A the type x =_{A} y is a proposition
#def isaset (A : U) : U
  := (x : A) -> (y : A) -> isaprop (x =_{A} y)

-- Non-dependent product of A and B
#def prod (A : U) (B : U) : U
  := ∑ (x : A), B

-- A function f : A -> B is an equivalence
-- if there exists g : B -> A
-- such that for all x : A we have g (f x) = x
-- and for all y : B we have f (g y) = y
#def isweq (A : U) (B : U) (f : A -> B) : U
  := ∑ (g : B -> A), prod ((x : A) -> g (f x) =_{A} x) ((y : B) -> f (g y) =_{B} y)

-- Equivalence of types A and B
#def weq (A : U) (B : U) : U
  := ∑ (f : A -> B), isweq A B f

-- Transport along a path
#def transport
    (A : U)
    (C : A -> U)
    (x y : A)
    (p : x =_{A} y)
    : C x -> C y
  := \\cx -> idJ(A, x, (\\z q -> C z), cx, y, p)

-- [RS17, Axiom 4.6] Relative function extensionality.
#def relfunext : U
  := (I : CUBE)
  -> (psi : I -> TOPE)
  -> (phi : psi -> TOPE)
  -> (A : psi -> U)
  -> ((t : psi) -> iscontr (A t))
  -> (a : (t : phi) -> A t)
  -> (t : psi) -> A t [ phi t |-> a t]

-- [RS17, Proposition 4.8] A (weaker) formulation of function extensionality.
#def relfunext2 : U
  := (I : CUBE)
  -> (psi : I -> TOPE)
  -> (phi : psi -> TOPE)
  -> (A : psi -> U)
  -> (a : (t : phi) -> A t)
  -> (f : (t : psi) -> A t [ phi t |-> a t ])
  -> (g : (t : psi) -> A t [ phi t |-> a t ])
  -> weq (f = g)
         ((t : psi) -> (f t =_{A t} g t) [ phi t |-> refl ])

-- Restrict extension type to a subshape.
#def restrict
    (I : CUBE)
    (psi : I -> TOPE)
    (phi : I -> TOPE)
    (A : {t : I | psi t \\/ phi t} -> U)
    (a : {t : I | psi t} -> A t)
  : {t : I | psi t /\\ phi t} -> A t
  := \\t -> a t

-- Reformulate extension type as an extension of a restriction.
#def ext-of-restrict
    (I : CUBE)
    (psi : I -> TOPE)
    (phi : I -> TOPE)
    (A : {t : I | psi t \\/ phi t} -> U)
    (a : {t : I | psi t} -> A t)
  : (t : psi) -> A t [ psi t /\\ phi t |-> restrict I psi phi A a t ]
  := a

-- Transform extension of an identity into an identity of restrictions.
#def restricts-path
    (r : relfunext2)
    (I : CUBE)
    (psi : I -> TOPE)
    (phi : I -> TOPE)
    (A : {t : I | psi t \\/ phi t} -> U)
    (a_psi : (t : psi) -> A t)
    (a_phi : (t : phi) -> A t)
    (e : {t : I | psi t /\\ phi t} -> a_psi t = a_phi t)
  : restrict I psi phi A a_psi = restrict I phi psi A a_phi
  := (first (second (r I
      (\\t -> psi t /\\ phi t)
      (\\t -> BOT)
      (\\t -> A t)
      (\\t -> recBOT)
      (\\t -> a_psi t)
      (\\t -> a_phi t)))) e

-- A weaker version of recOR, demanding only a path between a and b:
-- recOR(psi, phi, a, b) demands that for psi /\\ phi we have a == b (definitionally)
-- (recId psi phi a b e) demands that e is the proof that a = b (intensionally) for psi /\\ phi
#def recId
    (r : relfunext2)
    (I : CUBE)
    (psi : I -> TOPE)
    (phi : I -> TOPE)
    (A : {t : I | psi t \\/ phi t} -> U)
    (a_psi : (t : psi) -> A t)
    (a_phi : (t : phi) -> A t)
    (e : {t : I | psi t /\\ phi t} -> a_psi t = a_phi t)
  : {t : I | psi t \\/ phi t} -> A t
  := \\t -> recOR(
        psi t |-> transport
          ({t : I | psi t /\\ phi t} -> A t)
          (\\ra -> (t : psi) -> A t [ psi t /\\ phi t |-> ra t])
          (restrict I psi phi A a_psi)
          (restrict I phi psi A a_phi)
          (restricts-path r I psi phi A a_psi a_phi e)
          (ext-of-restrict I psi phi A a_psi)
          t,
        phi t |-> ext-of-restrict I phi psi A a_phi t
      )

-- If two extension types are equal along two subshapes,
-- then they are also equal along their union.
#def id-along-border
    (r : relfunext2)
    (I : CUBE)
    (psi : I -> TOPE)
    (phi : I -> TOPE)
    (A : {t : I | psi t \\/ phi t} -> U)
    (a b : {t : I | psi t \\/ phi t} -> A t)
    (e_psi : (t : psi) -> a t = b t)
    (e_phi : (t : phi) -> a t = b t)
    (border-is-a-set : {t : I | psi t /\\ phi t} -> isaset (A t))
  : {t : I | psi t \\/ phi t} -> a t = b t
  := recId r I psi phi
        (\\t -> a t = b t)
        e_psi e_phi
        (\\t -> border-is-a-set t (a t) (b t) (e_psi t) (e_phi t))
`