# Сопоставление с образцом

Выражение `match` разбирает значение индуктивного типа, объявленного командой [`#data`](commands/data.rzk.md), по одной ветви на конструктор. Это нотация для порождённого принципа индукции, в духе дисциплины Epigram[^epigram], недавно возрождённой проектом Pterodactyl[^ptero]: при проверке типов каждое `match` элаборируется в применение `ind-<имя>`, поэтому вычисление и завершимость устроены в точности как у элиминатора.

```rzk
#lang rzk-1

#data nat := zero | suc (n : nat)
```

Ветвь называет конструктор и связывает по одной переменной на каждый *аргумент метода*: поля конструктора, затем по одной индуктивной гипотезе на каждое рекурсивное поле, по порядку. Стрелка ветви — `⇒` (ASCII `=>`).

```rzk
#define plus
  ( n m : nat)
  : nat
  := match n
      ( zero ⇒ m
      | suc k ih ⇒ suc ih)
```

Здесь `ih` обозначает `plus k m`, результат рекурсии по `k`. Рекурсия возможна только через индуктивные гипотезы, поэтому всякое `match` завершается по построению. Вычисление дефинициональное:

```rzk
#define plus-two-two
  : plus (suc (suc zero)) (suc (suc zero)) =_{nat} suc (suc (suc (suc zero)))
  := refl
```

Ветви должны находиться в биекции с конструкторами: каждый конструктор встречается ровно один раз (в любом порядке), и число связываемых переменных совпадает с арностью его метода. Вложенных образцов и подстановочных знаков нет.

## Мотив

`match` в позиции проверки берёт мотив из ожидаемого типа: если разбираемое выражение — переменная, она абстрагируется из цели, и ветви видят цель на каждом конструкторе. Так получается зависимое сопоставление через подстановку:

```rzk
#define plus-zero
  ( n : nat)
  : plus zero n =_{nat} n
  := refl

#define zero-plus
  ( n : nat)
  : plus n zero =_{nat} n
  := match n
      ( zero ⇒ refl
      | suc k ih ⇒
          idJ
            ( nat , plus k zero
            , \ z q → suc (plus k zero) =_{nat} suc z
            , refl , k , ih))
```

В ветви `suc` цель — `plus (suc k) zero =_{nat} suc k`, и доступна гипотеза `ih : plus k zero =_{nat} k`; индукция по путям над `ih` даёт конгруэнтность для `suc`, завершающую доказательство.

Если разбираемое выражение — не переменная, мотив постоянен: цель не должна от него зависеть. Наконец, явный мотив записывается после `into`. Это *семейство*, в которое ведёт сопоставление, применяемое к индексам и разбираемому значению:

```rzk
#define plus'
  ( n m : nat)
  : nat
  := match n into (\ _ → nat)
      ( zero ⇒ m
      | suc k ih ⇒ suc ih)
```

`match` без `into` принимается только там, где его тип уже известен. В позиции вывода (например, под `#compute`) мотив нужно указать явно.

## Приём «конвой»

Чтобы ветвь могла использовать гипотезу *при уточнённом разбираемом значении*, сделайте мотив функциональным типом и примените `match` к гипотезе снаружи (приём «конвой» из фольклора Coq[^cpdt]). Например, при

```rzk
#data bool := false | true

#define not
  ( b : bool)
  : bool
  := match b
      ( false ⇒ true
      | true ⇒ false)
```

доказательство `C (not (not b))` из `h : C b` нельзя получить прямым разбором `b`: в ветви `true` цель становится `C (not (not true))`, но `h` по-прежнему имеет тип `C b` для неуточнённого `b`. Протаскивание `h` через мотив уточняет обе стороны сразу:

```rzk
#define convoy
  ( C : bool → U)
  ( b : bool)
  ( h : C b)
  : C (not (not b))
  := (match b into (\ b' → C b' → C (not (not b')))
        ( false ⇒ \ h' → h'
        | true ⇒ \ h' → h')) h
```

В каждой ветви аргумент `h'` имеет тип `C` на конструкторе, а `C (not (not true))` вычисляется в `C true`, так что достаточно тождественной функции.

## Индексированные семейства

Разбор значения индексированного семейства устроен так же. Мотив после `into` абстрагирует индексы перед разбираемым значением:

```rzk
#data vec
  ( A : U)
  : nat → U
  :=
    nil : vec A zero
  | cons (n : nat) (x : A) (xs : vec A n) : vec A (suc n)

#define vlen
  ( A : U)
  ( n : nat)
  ( xs : vec A n)
  : nat
  := match xs
      ( nil ⇒ zero
      | cons k x tail ih ⇒ suc ih)
```

Мотив может использовать индексы. Например, безопасная голова на `vec A (suc n)` вычисляет свой мотив вложенным разбором индекса, так что ветвь `nil` должна дать `Unit`, а ветвь `cons` — элемент `A`:

```rzk
#define vhead
  ( A : U)
  ( n : nat)
  ( xs : vec A (suc n))
  : A
  := match xs into (\ k v → match k (zero ⇒ Unit | suc j jh ⇒ A))
      ( nil ⇒ unit
      | cons k x tail ih ⇒ x)
```

Если цель зависит от индексов переменной, по которой идёт разбор, построенный мотив оставляет индексы фиксированными, что для индукции обычно не годится. В этом случае запишите зависимый мотив через `into`.

[^epigram]: Conor McBride and James McKinna. _The view from the left._ Journal of Functional Programming 14(1), pp. 69–111, 2004. <https://doi.org/10.1017/S0956796803004829>

[^ptero]: Jon Sterling. _Is it time for a new proof assistant?_ Talk at the Homotopy Type Theory Electronic Seminar Talks (HoTTEST), 25 September 2025. <https://www.youtube.com/watch?v=7oBkEbKJvnE>

[^cpdt]: Adam Chlipala. _Certified Programming with Dependent Types._ MIT Press, 2013. <http://adam.chlipala.net/cpdt/> — приём «конвой» назван в главе MoreDep, <http://adam.chlipala.net/cpdt/html/MoreDep.html>.
