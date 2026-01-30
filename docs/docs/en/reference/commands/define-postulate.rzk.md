# `#define` and `#postulate` commands

The `#define` and `#postulate` commands are used to introduce definitions and axioms.

## Syntax

```{.rzk}
#define <name> [uses (<vars>)] (<param>)* : <type> := <term>
#define <name> [uses (<vars>)] : <type> := <term>

#postulate <name> [uses (<vars>)] (<param>)* : <type>
#postulate <name> [uses (<vars>)] : <type>
```

## Description

### `#define`

The `#define` command introduces a definition, associating a name with a term of a given type. The typechecker verifies that the term has the specified type.

The `uses (<vars>)` annotation explicitly specifies variables that are implicitly used in the type or the definition (see [Sections and Variables](../sections.rzk.md) for more details).
If there no such variables, the annotation is omitted.

### `#postulate`

The `#postulate` command introduces an axiom (a postulate) without providing a definition.
This is useful when you want to assume the existence of something without proving it.

Like `#define`, the `uses (<vars>)` annotation specifies implicit variables (used in the type of the postulated axiom), if any.

## Example

```rzk
#lang rzk-1

-- Define a function
#define id (A : U)
  : A → A
  := \ x → x

-- Define a constant
#define unit-value
  : Unit
  := unit

-- Postulate an a type
#postulate Void
  : U

-- Postulate an axiom (e.g., double negation elimination)
#postulate DNE (A : U)
  : ( ( ( A → Void) → Void) → A)

-- Postulate with implicit assumptions
#variables A B : U
#variable f : B → A
#variable g : A → B

-- note that B is used implicitly in this postulate, hence uses (B)
#postulate f_f_is_id uses (B)
  ( x : A)
  : f (g x) = id A x
```

## Notes

- `#def` is an alias for `#define`
- Both commands support parameters, which can be used to create functions
- The typechecker verifies that `#define` terms have the correct types
- `#postulate` does not require a term, as it introduces an unproven assumption
