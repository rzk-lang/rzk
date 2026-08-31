# Family of families: featherweight defunctionalization

## Motivation

This package transforms regular type families into
["first-class families"](https://hackage.haskell.org/package/first-class-families).
It does so without declaring dummy data types as defunctionalization symbols,
avoiding namespace pollution.

### Featherweight defunctionalization

The idea is to use strings. There is an unambiguous way to refer to an existing type family:
by its fully qualified name---a triple of a package name, module name, and base name.

```haskell
GHC.TypeNats.+
-- as a Name --
MkName "base" "GHC.TypeNats" "+"
```

All type families can be promoted to first-class using a single symbol indexed by
qualified names. That is the `Family` of type families.

- No namespace pollution: Promoting a type family requires no new data type,
  only an `Eval` instance for its name.
- Decentralization: Every type family induces a unique `Eval` instance,
  and multiple packages may declare the same instance without conflicts.

```haskell
type instance Eval (Family_ (MkName "base" "GHC.TypeNats" "+") _ '(x, '(y, '()))) = x + y
```

For more details on this encoding, see the module `Fcf.Family`.

## Usage

### Promote a type family

To promote a type family, the necessary boilerplate
can be generated with Template Haskell, using the function `Fcf.Family.TH.fcfify`.

```haskell
fcfify ''MyTypeFamily

-- Examples
fcfify ''(GHC.TypeNats.+)
fcfify ''Data.Type.Bool.If
```

This generates instances of `Eval` and auxiliary type families for that former
instance to be well-defined.

### Use the promoted type family

This allows using the defunctionalization symbol `Family` indexed by the
type family's name---a tuple of strings constructed with `MkName`.

```haskell
  Eval (Family (MkName "base" "GHC.TypeNats" "+") P0 '(1, '(2, '())))
= 1 + 2
= 3

  Eval (Family (MkName "base" "GHC.Type.Bool" "If") P1 '( 'True, '("yes", '("no", '()))))
= If 'True "yes" "no"
= "yes"
```

The `familyName` function converts the name of an actual family to an *fcf-family* name in Template Haskell.

```haskell
  Eval (Family $(familyName ''+) P0 '(1, '(2, '())))
= 3

  Eval (Family $(familyName ''If) P1 '( 'True, '("yes", '("no", '()))))
= "yes"
```

## Existing instances

- The library [`fcf-base`](https://hackage.haskell.org/package/fcf-base) contains instances for type families defined in *base*.
- The module `Fcf.Family.Meta` contains the instance for `Eval`.

## Limitations

- There is partial support for dependently typed type families.
  The kind of the result may depend on its arguments.
  This package does not yet support type families where the kind of arguments
  depends on other visible arguments.
- Invisible arguments are currently all assumed to be of kind `Type`.
  Lifting this restriction requires either access to more typing information
  from the compiler via TH, or some implementation of kind inference in TH.
