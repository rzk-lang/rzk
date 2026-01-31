# Тип `Unit`

Поддерживается с версии [:octicons-tag-24: v0.5][Unit support]

```rzk
#lang rzk-1
```

В синтаксисе предоставляются только `Unit` (тип) и `unit` (единственное значение).
`rzk` принимает свойство единственности типа `Unit` (см. раздел 1.5 книги HoTT[^1]) как правило вычисления,
что означает, что любой (хорошо типизированный) терм типа `Unit` сводится к `unit`.
Это означает, в частности, что индукцию и единственность можно определить очень легко:

```rzk
#define ind-Unit
  ( C : Unit → U)
  ( C-unit : C unit)
  ( x : Unit)
  : C x
  := C-unit

#define uniq-Unit
  ( x : Unit)
  : x = unit
  := refl

#define isProp-Unit
  ( x y : Unit)
  : x = y
  := refl
```

В качестве нетривиального примера, вот доказательство того, что `Unit` является типом Сегала (т.е. пред-∞-категорией):

```rzk
#section isSegal-Unit

#variable extext : ExtExt

#define iscontr-Unit
  : isContr Unit
  := (unit , \ _ → refl)

#define isContr-Δ²→Unit uses (extext)
  : isContr (Δ² → Unit)
  := (\ _ → unit , \ k → eq-ext-htpy extext
    ( 2 × 2) Δ² (\ _ → BOT)
    ( \ _ → Unit) (\ _ → recBOT)
    ( \ _ → unit) k
    ( \ _ → refl)
    )

#define isSegal-Unit uses (extext)
  : isSegal Unit
  := \ x y z f g → isRetract-ofContr-isContr
    ( Σ ( h : hom Unit x z) , hom2 Unit x y z f g h)
    ( Δ² → Unit)
    ( \ (_ , k) → k , (\ k → (\ t → k (t , t) , k) , \ _ → refl))
    isContr-Δ²→Unit

#end isSegal-Unit
```

[Unit support]: https://github.com/rzk-lang/rzk/releases/tag/v0.5

[^1]: The Univalent Foundations Program (2013). _Homotopy Type Theory: Univalent Foundations of Mathematics._ <https://homotopytypetheory.org/book>
