# `#check` command

The `#check` command typechecks a term against a given type.

## Syntax

```{.rzk}
#check <term> : <type>
```

## Description

The `#check` command verifies that a term has the specified type. If the typechecking succeeds, the command completes without error. If it fails, an error message is displayed.

## Example

```rzk
#lang rzk-1

#define id (A : U)
  : A → A
  := \ x → x

-- Check that id has the correct type
#check id : (A : U) → A → A

-- Check that applying id to a type works
#check id U : U → U

-- Check that applying id to a value works
#define unit-type
  : U
  := Unit
#check id unit-type unit : Unit
```
