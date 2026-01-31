# Разделы и переменные

Разделы и переменные позволяют упростить определения, вынося общие предположения.

!!! info "Переменные в стиле Coq"
`rzk` реализует переменные аналогично
<a href="https://coq.inria.fr/refman/language/core/assumptions.html#coq:cmd.Variable" target="_blank">команде `Variable` в Coq</a>.
Важное отличие заключается в том, что `rzk` не позволяет определениям использовать переменные неявно и добавляет аннотации `uses (...)` для обеспечения того, чтобы такие зависимости не были случайными.
Это, возможно, несколько связано с <a href="https://coq.inria.fr/refman/proofs/writing-proofs/equality.html#coq:exn.Section-variable-‘ident’-occurs-implicitly-in-global-declaration-‘qualid’-present-in-hypothesis-‘ident’" target="_blank">этим сообщением об ошибке в Coq</a>.

Это литературный файл `rzk`:

```rzk
#lang rzk-1
```

## Переменные

Рассмотрим следующие определения:

```rzk
#define compose₁
  ( A B C : U)
  ( g : B → C)
  ( f : A → B)
  : A → C
  := \ x → g (f x)

#define twice₁
  ( A : U)
  ( h : A → A)
  : A → A
  := \ x → h (h x)
```

Поскольку типы `A`, `B` и `C` будут использоваться в нескольких определениях, мы можем объявить их как переменные:

```rzk
#variables A B C : U

#define compose₂
  ( g : B → C)
  ( f : A → B)
  : A → C
  := \ x → g (f x)

#define twice₂
  ( h : A → A)
  : A → A
  := \ x → h (h x)
```

Команда `#variables` здесь вводит предположения, которые могут использоваться в следующих определениях. Важно, что после проверки файла (модуля) все определения будут иметь прикреплённые предположения, использованные (явно или неявно), как связанные переменные.

### Неявно используемые переменные (и `uses`)

Мы можем попытаться пойти ещё дальше и объявить переменные `f`, `g`, `h` и `x`:

```rzk
#variable g : B → C
#variable f : A → B
#variable h : A → A
#variable x : A

-- #define bad-compose₃ : C := g (f x)  -- ОШИБКА: неявные предположения A и B
#define twice₃
  : A
  := h (h x)
```

Обратите внимание, что `bad-compose₃` неявно зависит от типов `A` и `B`, что сразу отмечается `rzk`, который выдаёт ошибку (если мы раскомментируем соответствующую строку):

```text
implicit assumption
  B : U
used in definition of
  bad-compose₃
```

Чтобы дать `rzk` знать, что это не случайно, мы можем добавить аннотацию `uses (...)` для указания списка переменных, неявно используемых в определении:

```rzk
#define compose₃ uses (A B)
  : C
  := g (f x)
```

## Разделы

Чтобы временно ввести переменные предположений внутри одного файла, вы можете использовать разделы:

```rzk
#section example-1

#variables X Y Z : U
#variable k : X → X
#variable x' : X

#define compose₄
  ( g : Y → Z)
  ( f : X → Y)
  : X → Z
  := \ x → g (f x)

#define twice₄
  : X
  := k (k x')

#end example-1
```

Теперь, вне раздела, `compose₄` и `twice₄` получают соответствующие параметры
(только те, которые использованы, явно или неявно):

```rzk
-- compose₄ : (X : U) -> (Y : U) -> (Z : U) -> (g : Y -> Z) -> (f : X -> Y) -> (X -> Z)
-- twice₄ : (X : U) -> (k : X -> X) -> (x' : X) -> X

#define twice₅
  ( T : U)
  ( e : T → T)
  : T → T
  := compose₄ T T T e e

#define identity
  ( T : U)
  : T → T
  := twice₄ T (\ t → t)
```

!!! warning "Отсутствие отступов"
`rzk` в настоящее время не поддерживает отступы, поэтому все определения и команды внутри раздела (включая вложенные разделы) должны начинаться с начала строки.
