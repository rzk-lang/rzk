# Hom-type

```rzk
hom : (A : U) -> (x : A) -> (y : A) -> U
  := \A -> \x -> \y -> <{t : 2 | TOP} -> A [ t === 0_2 \/ t === 1_2 |-> recOR(t === 0_2, t === 1_2, x, y) ]>
```

```rzk
RS17:Prop:3.5a : (A : U) -> (f : <{t : 2 | TOP} -> A[BOT |-> recBOT]>) -> <{ts : 2 * 2 | TOP} -> A [BOT |-> recBOT]>
  := \(A : U) -> \(f : <{t : 2 | TOP} -> A[BOT |-> recBOT]>) -> \{ts : 2 * 2 | TOP} -> recOR ((first ts) <= (second ts), (second ts) <= (first ts), f (second ts), f (first ts))

RS17:Prop:3.5b : (A : U) -> (f : <{t : 2 | TOP} -> A[BOT |-> recBOT]>) -> <{ts : 2 * 2 | TOP} -> A [BOT |-> recBOT]>
  := \(A : U) -> \(f : <{t : 2 | TOP} -> A[BOT |-> recBOT]>) -> \{ts : 2 * 2 | TOP} -> recOR ((first ts) <= (second ts), (second ts) <= (first ts), f (first ts), f (second ts))
```

```rzk
isShapeRetraction : (I : CUBE) -> (A : U) -> (phi : (t : I) -> TOPE) -> (psi : (t : I) -> TOPE) -> (f : (k : <{t : I | psi t} -> A [BOT |-> recBOT]>) -> <{t : I | phi t} -> A [BOT |-> recBOT]>) -> U
  := \(I : CUBE) -> \(A : U) -> \(phi : (t : I) -> TOPE) -> \(psi : (t : I) -> TOPE) -> \(f : (k : <{t : I | psi t} -> A [BOT |-> recBOT]>) -> <{t : I | phi t} -> A [BOT |-> recBOT]>) -> ∑ (g : (k : <{t : I | phi t} -> A [BOT |-> recBOT]>) -> <{t : I | psi t} -> A [BOT |-> recBOT]>), (k : <{t : I | phi t} -> A [BOT |-> recBOT]>) -> f (g k) =_{<{t : I | phi t} -> A [BOT |-> recBOT]>} k

shapeRetract : (I : CUBE) -> (phi : (t : I) -> TOPE) -> (psi : (t : I) -> TOPE) -> U
  := \(I : CUBE) -> \(phi : (t : I) -> TOPE) -> \(psi : (t : I) -> TOPE) -> (A : U) -> ∑ (f : (k : <{t : I | psi t} -> A [BOT |-> recBOT]>) -> <{t : I | phi t} -> A [BOT |-> recBOT]>), isShapeRetraction I A phi psi f
```

```rzk
Δ¹ : (t : 2) -> TOPE
  := \(t : 2) -> TOP

Δ² : (t : 2 * 2) -> TOPE
  := \(t, s) -> s <= t

Δ³ : (t : 2 * 2 * 2) -> TOPE
  := \((t1, t2), t3) -> t3 <= t2 /\ t2 <= t1

shapeProd : (I : CUBE) -> (J : CUBE) -> (psi : (t : I) -> TOPE) -> (chi : (s : J) -> TOPE) -> (ts : I * J) -> TOPE
  := \I -> \J -> \psi -> \chi -> \(t, s) -> psi t /\ chi s

Δ¹×Δ¹ : (t : 2 * 2) -> TOPE
  := shapeProd 2 2 Δ¹ Δ¹

Δ²×Δ¹ : (t : 2 * 2 * 2) -> TOPE
  := shapeProd (2 * 2) 2 Δ² Δ¹
```

```rzk
Δ²-is-retract-of-Δ¹×Δ¹ : shapeRetract (2 * 2) Δ² Δ¹×Δ¹
  := \A -> (\k -> \ts -> k ts, (\k -> \(t, s) -> recOR(t <= s, s <= t, k (t, s), k (t, s)), \k -> refl_{k}))
```

```rzk
Δ³-is-retract-of-Δ²×Δ¹ : shapeRetract (2 * 2 * 2) Δ³ Δ²×Δ¹
  := \A -> (\k -> \ts -> k ((first (first ts), second ts), second (first ts)), (\k -> \ts -> recOR((second ts) <= (second (first ts)), (second (first ts)) <= (second ts) /\ (second ts) <= (first (first ts)) \/ (first (first ts)) <= second ts, k ((first (first ts), second ts), second (first ts)), recOR((second (first ts)) <= (second ts) /\ (second ts) <= (first (first ts)), (first (first ts)) <= second ts, k ((first (first ts), second ts), second (first ts)), k ((first (first ts), first (first ts)), second (first ts)))), \k -> refl_{k}))
```
