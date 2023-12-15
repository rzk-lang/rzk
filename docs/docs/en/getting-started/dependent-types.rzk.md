# Dependent types

In this file we will look at Rzk primitives to work with dependent types.

!!! info "Reference material"

    This page is mostly based on the introduction of dependent types in the HoTT Book (Sections 1.2–1.6),
    immediately introducing corresponding formalizations in Rzk and noting some differences.

This is a literate Rzk file:

```rzk
#lang rzk-1
```

## Functions

The type `#!rzk (x : A) → B x` is the type of (dependent)
functions with an argument of type `A` and, for each input `x`,
the output type `B x`.

As a simple example of a dependent function,
consider the identity function:

```rzk
#define identity
  : ( A : U) → (x : A) → A
  := \ A x → x
```

Since we are not using `x` in the type of `identity`,
we can simply write the type of the argument,
without providing its name:

```rzk
#define identity₁
  : ( A : U) → A → A
  := \ A x → x
```

We can write this definition differently,
by putting `#!rzk (A : U)` into parameters (before `:`),
and omitting it in the lambda abstraction:

```rzk
#define identity₂
  ( A : U)
  : A → A
  := \ x → x
```

We could also move `x` into parameters as well,
although this probably does not increase readability anymore:

```rzk
#define identity₃
  ( A : U)
  ( x : A)
  : A
  := x
```

Another, less trivial example of a dependent function is
the one that swaps the arguments of another function:

```rzk
#define swap
  ( A B C : U)
  : ( A → B → C) → (B → A → C)
  := \ f → \ b a → f a b
```

## Product types

Rzk does not have built-in product types, since they are a special case of Σ-types,
which we will discuss soon. For now, we give definition of product types:

```rzk
#define prod
  ( A B : U)
  : U
  := Σ (_ : A) , B
```

The type `#!rzk prod A B` corresponds to the product type $A \times B$.
The `#!rzk Unit` type corresponds to the type $\mathbf{1}$.

The intended elements of `#!rzk prod A B` are only pairs `#!rzk (a, b) : prod A B`
where `#!rzk a : A` and `#!rzk b : B`. Similarly, intended element of `#!rzk Unit`
is only `#!rzk unit`. However, formally, this is not immediately true and instead
is a theorem that we can prove.

### Remark on type formers

Formally, we have the following constituents of the definition for product types and function types
(for comparison):

1. **Type formation**:

    - `#!rzk prod A B` is a type whenever `A` and `B` are types
    - `#!rzk A → B` is a type whenever `A` and `B` are types

2. **Constructors (introduction rules)**:

    - `#!rzk (x , y)` is a term of type `#!rzk prod A B` whenever `#!rzk x : A` and `#!rzk y : B`
    - `#!rzk \ x → y` is a term of type `#!rzk A → B` whenever for any `#!rzk x : A` we have `#!rzk y : B`

3. **Eliminators (elimination rules)**:

    - Given `#!rzk z : prod A B`, we can _project_ the first and second components:
        - `#!rzk first z : A` and `#!rzk second z : B`
        - it is also possible to pattern match (deconstruct) in a function argument or when introducing a parameter, e.g.

            ```rzk
            #define swap-prod₁
              ( A B : U)
              : prod A B → prod B A
              := \ (x , y) → (y , x)

            #define swap-prod₂
              ( A B : U)
              ( (x , y) : prod A B)
              : prod B A
              := ( y , x)
            ```

        - more generally, eliminators come in a form of an _induction principle_, which we will discuss below
          and can be defined in Rzk in terms of pattern matching or `#!rzk first` and `#!rzk second`:

            ```rzk
            #define ind-prod
              ( A B : U)
              ( C : prod A B → U)
              ( f : (a : A) → (b : B) → C (a , b))
              : (z : prod A B) → C z
              := \ (a , b) → f a b
            ```

    - Given `#!rzk f : A → B`, we can _apply_ it to an argument of type `#!rzk a : A`:
        - `#!rzk f a : B`

    !!! warning "Built-in eliminators in Rzk"

        Built-in eliminators in Rzk need to be **always** fully applied (e.g. `#!rzk first` without an argument is invalid syntax!).
        Technically, this corresponds with the "second presentation" of type theory in Appendix A.2 of the HoTT Book.
        In practice, this is not always convenient for users, as we often want to curry some of these built-ins,
        so wrapper functions are introduced (by users), for example:

        ```rzk
        #define pr₁
          ( A B : U)
          : prod A B → A
          := \ p → first p
        ```

4. **Computation rules**:

    - Projecting from a pair is computed as follows for any `#!rzk x : A` and `#!rzk y : B`:
        - `#!rzk first (x , y) ≡ x`
        - `#!rzk second (x , y) ≡ y`
    - Applying an lambda abstraction is computed by substituting the argument into a body:
      - `#!rzk (\ x → y) a ≡ y{x ↦ a}` when `#!rzk a : A` and for all `#!rzk x : A`, `#!rzk y : B`.

5. **Uniqueness principle (optional)**:

    - For any `#!rzk z : prod A B`, we have `#!rzk z ≡ (first z, second z)`
      - This holds definitionally for product types and Σ-types in Rzk, but is provable in a weaker (propositional) form in HoTT Book
    - For any function `#!rzk f : A → B`, we have `#!rzk f ≡ \ x → f x`

### Recursion principle

Following the HoTT Book, for each type former we can formalize its _recursion principle_.
A recursion principle for type `#!rzk T` is a function that allows to produce
a result of arbitrary type `#!rzk C` from a value of type `#!rzk T`:

```{unchecked .rzk}
#define rec-T
  ( C : U)
  -- ... (parameters to the recursion principle)
  : T → C
```

For example, for the product type `#!rzk prod A B`, recursion principle looks like this:

```rzk
#define rec-prod
  ( A B : U)
  ( C : U)
  ( f : A → B → C)
  : prod A B → C
  := \ (a , b) → f a b
```

For the `#!rzk Unit` type, recursion principle is trivial:

```rzk
#define rec-Unit
  ( C : U)
  ( c : C)
  : Unit → C
  := \ unit → c
```

### Induction principle

To define a _dependent_ function out of a type, we use its _induction principle_,
which can be seen as a dependent version of the recursion principle.
An induction principle for type `#!rzk T` is a function that allows to produce
a result of arbitrary type `#!rzk C z` from a value `#!rzk z : T`:

```{unchecked .rzk}
#define ind-T
  ( C : T → U)
  -- ... (parameters to the induction principle)
  : (z : T) → C z
```

For example, for the product type `#!rzk prod A B`, induction principle looks like this:

```rzk
#define ind-prod
  ( A B : U)
  ( C : prod A B → U)
  ( f : (a : A) → (b : B) → C (a , b))
  : ( z : prod A B) → C z
  := \ (a , b) → f a b
```

We can use `#!rzk ind-prod` to prove the uniqueness principle for products.
Here we use the identity type, which we will cover later, but for now it is
sufficient to know that there is always an element `#!rzk refl_{x} : x =_{A} x`
for any `#!rzk x : A`.

```rzk
#define uniq-prod
  ( A B : U)
  ( z : prod A B)
  : ( first z , second z) =_{prod A B} z
  := ind-prod A B
      ( \ z' → (first z' , second z') =_{prod A B} z') -- C
      ( \ a b → refl_{(a , b)})
        -- C (a, b)
        -- ≡ ( \ z' → (first z', second z') =_{prod A B} z') (a, b)
        -- ≡ (first (a, b), second (a, b)) =_{prod A B} (a, b)
        -- ≡ (a, second (a, b)) =_{prod A B} (a, b)
        -- ≡ (a, b) =_{prod A B} (a, b)
      z
```

Since in Rzk the uniqueness principle is builtin, a simpler proof also works:

```rzk
#define uniq-prod'
  ( A B : U)
  ( z : prod A B)
  : ( first z , second z) =_{prod A B} z
  := refl_{z} -- works in Rzk, not in HoTT Book, since in Rzk we have (first z, second z) ≡ z
```

For the `#!rzk Unit` type, induction principle is trivial:

```rzk
#define ind-Unit
  ( C : Unit → U)
  ( c : C unit)
  : ( z : Unit) → C z
  := \ unit → c
```

Unlike `#!rzk rec-Unit`, induction principle for `#!rzk Unit` is not useless,
since it allows, for example, to prove the uniqueness principle:

```rzk
#define uniq-Unit
  ( z : Unit)
  : unit =_{Unit} z
  := ind-Unit
      ( \ z' → unit =_{Unit} z')
      ( refl_{unit})
      z
```

Again, since Rzk has a builtin uniqueness principle for `#!rzk Unit`, a simpler proof also works:

```rzk
#define uniq-Unit'
  ( z : Unit)
  : unit =_{Unit} z
  := refl_{z} -- works in Rzk, not in HoTT Book, since in Rzk we have unit ≡ z
```

## Dependent pair types (Σ-types)

A straightforward generalization of product types to dependent pairs `#!rzk Σ (a : A), B a`
where `#!rzk A` is a type and `#!rzk B : A → U` is a type family indexed in `#!rzk A`.

The indended values of `#!rzk Σ (a : A), B a` are pairs `#!rzk (a , b)` of
terms `#!rzk a : A` and `#!rzk b : B a`. Note that the type of the second component
may depend on the value of the first component.
When the type family `#!rzk B` is constant, e.g. `#!rzk (\ _ → C)`,
then `#!rzk Σ (a : A), B a` becomes exactly the product type `#!rzk prod A C`.

To eliminate dependent pairs, we use `#!rzk first`, `#!rzk second`, or pattern
matching on pairs. However, the types of projections are less obvious compared
to the case of product types.

### Projections

The first projection can be easily defined in terms of pattern matching:

```rzk
#define pr₁
  ( A : U)
  ( B : A → U)
  : ( Σ ( a : A) , B a) → A
  := \ (a , _) → a
```

However, second projection requires some care. For instance, we might try this:

```{unchecked .rzk}
-- NOTE: incorrect definition
#define pr₂
  ( A : U)
  ( B : A → U)
  : (Σ (a : A), B a) → B a  -- ERROR!
  := \ (_ , b) → b
```

```
undefined variable: a
```

We get the `undefined variable` error since `a` is not visible outside of Σ-type definition.
To access it, we need a dependent function:

```rzk
#define pr₂
  ( A : U)
  ( B : A → U)
  : ( z : Σ (a : A) , B a) → B (pr₁ A B z)
  := \ (_ , b) → b
```

In Rzk, it is sometimes more convenient to talk about Σ-types as "total" types (as in "total spaces"):

```rzk
#define total-type
  ( A : U)
  ( B : A → U)
  : U
  := Σ (a : A) , B a
```

We can use pattern matching in the function type and this new definition to write
second projection slightly differently:

```rzk
#define pr₂'
  ( A : U)
  ( B : A → U)
  ( ( a , b) : total-type A B)
  : B a
  := b
```

### Recursion and induction principles

The recursion principle for Σ-types is a simple generalization of
the recursion principle for product types:

```rzk
#define rec-Σ
  ( A : U)
  ( B : A → U)
  ( C : U)
  ( f : (a : A) → B a → C)
  : total-type A B → C
  := \ (a , b) → f a b
```

The induction principle is, again, a generalization of the recursion
principle to dependent types:

```rzk
#define ind-Σ
  ( A : U)
  ( B : A → U)
  ( C : total-type A B → U)
  ( f : (a : A) → (b : B a) → C (a , b))
  : ( z : total-type A B) → C z
  := \ (a , b) → f a b
```

As before, using `#!rzk ind-Σ` we may prove the uniqueness principle, now for Σ-types:

```rzk
#define uniq-Σ
  ( A : U)
  ( B : A → U)
  ( z : total-type A B)
  : ( pr₁ A B z , pr₂ A B z) =_{total-type A B} z
  := ind-Σ A B
      ( \ z → (pr₁ A B z , pr₂ A B z) =_{total-type A B} z)
      ( \ a b → refl_{(a , b)})
      z
```

And again, Rzk can accept a simpler proof, since uniqueness for Σ-types is already built into it:

```rzk
#define uniq-Σ'
  ( A : U)
  ( B : A → U)
  ( z : total-type A B)
  : ( pr₁ A B z , pr₂ A B z) =_{total-type A B} z
  := refl_{z} -- works in Rzk, but not in HoTT Book
```

### Type-theoretic "axiom" of choice

Using `#!rzk ind-Σ` we can also prove a type-theoretic axiom of choice:

```rzk
#define AxiomOfChoice
  : U
  := (A : U)
    → ( B : U)
    → ( R : A → B → U)
    → ( ( x : A) → Σ (y : B) , R x y)
    → ( Σ ( f : A → B) , (x : A) → R x (f x))
```

You are encouraged to try proving this yourself first.

If you encounter problems, try looking for the proof in the HoTT Book Section 1.6 (page 32).

If you still have issues formalizing it in Rzk, you may peek here:

??? abstract "Proof of the type theoretic axiom of choice"

    ```rzk
    #define ac : AxiomOfChoice
      := \ A B R g → ( \ a → first (g a) , \ x → second (g x))
      -- g    : (x : A) → Σ (y : B), R x y
      -- x    : A
      -- g x  : Σ (y : B), R x y
      -- second (g x) : R x (first (g x))

      -- f : A → B
      -- f := \ a → first (g a)
      --
      -- R x (f x)
      -- == R x ((\ a → first (g a)) x)
      -- == R x (first (g x))
    ```

## Coproducts

Given types $A$ and $B$ a coproduct type $A + B$ corresponds intuitively
to a disjoint union of $A$ and $B$ (in set theory). We also have a nullary
version: $\mathbf{0}$ (empty type).

In Rzk, empty type and coproduct types do not exist, but a weaker version can be postulated.

### Postulating the empty type

For example, an empty type can be postulated as follows:

```rzk
#postulate Void
  : U
#postulate ind-Void
  ( C : Void → U)
  : ( z : Void) → C z
```

Since there should be no values of type `#!rzk Void`,
the induction principle corresponds to the principle that from falsehood anything follows.
A non-dependent version of that corresponds to the recursion principle,
which we can define in terms of `#!rzk ind-Void`:

```rzk
#define rec-Void
  ( C : U)
  : Void → C
  := ind-Void (\ _ → C)
```

### Postulating the coproduct type

!!! warning "Postulating coproducts"

    This subsection currently provides postulates with little explanation.
    Once Rzk has support for user-defined (higher) inductive types or built-in coproducts,
    this section will be updated.

Similarly, we can postulate the coproduct:

```rzk
#postulate coprod
  ( A B : U)
  : U
```

There are two ways to create a term of type `#!rzk coprod A B` —
inject a term from `#!rzk A` or a term of `#!rzk B`:

```rzk
#postulate inl
  ( A B : U)
  : A → coprod A B
#postulate inr
  ( A B : U)
  : B → coprod A B
```

To eliminate a coproduct, we have to provide two handlers —
one for the left case and one for the right:

```rzk
#postulate ind-coprod
  ( A B : U)
  ( C : coprod A B → U)
  ( l : (a : A) → C (inl A B a))
  ( r : (b : B) → C (inr A B b))
  : ( z : coprod A B) → C z
```

Since we are postulating the induction principle,
we also have to provide the computational rules explicitly.
However, in Rzk, we can only postulate _propositional_ computational rules:

```rzk
#postulate ind-coprod-inl
  ( A B : U)
  ( C : coprod A B → U)
  ( l : (a : A) → C (inl A B a))
  ( r : (b : B) → C (inr A B b))
  ( a : A)
  : ind-coprod A B C l r (inl A B a) = l a

#postulate ind-coprod-inr
  ( A B : U)
  ( C : coprod A B → U)
  ( l : (a : A) → C (inl A B a))
  ( r : (b : B) → C (inr A B b))
  ( b : B)
  : ind-coprod A B C l r (inr A B b) = r b
```

We can now define recursion for coproducts
as a special case of induction:

```rzk
#define rec-coprod
  ( A B : U)
  ( C : U)
  ( l : A → C)
  ( r : B → C)
  : coprod A B → C
  := ind-coprod A B (\ _ → C) l r
```

The uniqueness principle for coproducts says
that any coproduct is either an `#!rzk inl` or an `#!rzk inr`.
Proving the uniqueness is fairly straightforward, except
we have to provide some intermediate types explicitly:

```rzk
#define uniq-coprod
  ( A B : U)
  ( z : coprod A B)
  : coprod
      ( Σ ( a : A) , inl A B a = z)
      ( Σ ( b : B) , inr A B b = z)
  := ind-coprod A B
      ( \ z' → coprod
          ( Σ ( a : A) , inl A B a = z')
          ( Σ ( b : B) , inr A B b = z'))
      ( \ a' → inl
          ( Σ ( a : A) , (inl A B a = inl A B a'))
          ( Σ ( b : B) , (inr A B b = inl A B a'))
          ( a' , refl))
      ( \ b' → inr
          ( Σ ( a : A) , (inl A B a = inr A B b'))
          ( Σ ( b : B) , (inr A B b = inr A B b'))
          ( b' , refl))
      z
```

## Booleans

!!! warning "Postulating booleans"

    This subsection currently provides postulates with little explanation.
    Once Rzk has support for user-defined (higher) inductive types or built-in booleans,
    this section will be updated.

```rzk
#postulate Bool
  : U
#postulate false
  : Bool
#postulate true
  : Bool
```

```rzk
#postulate ind-Bool
  ( C : Bool → U)
  ( f : C false)
  ( t : C true)
  : ( z : Bool) → C z
```

```rzk
#postulate ind-Bool-false
  ( C : Bool → U)
  ( f : C false)
  ( t : C true)
  : ind-Bool C f t false = f
#postulate ind-Bool-true
  ( C : Bool → U)
  ( f : C false)
  ( t : C true)
  : ind-Bool C f t true = t
```

```rzk
#define rec-Bool
  ( C : U)
  ( f t : C)
  : Bool → C
  := ind-Bool (\ _ → C) f t
```

```rzk
#define uniq-Bool
  ( z : Bool)
  : coprod (z = false) (z = true)
  := ind-Bool
      ( \ z' → coprod (z' = false) (z' = true))
      ( inl (false = false) (false = true) refl)
      ( inr (true = false) (true = true) refl)
      z
```

```rzk
#define not
  : Bool → Bool
  := rec-Bool Bool true false
```

Unfortunately, because computation rules are postulated
in a weak form, they do not compute automatically and have to be used explicitly,
so the following proof does not work:

```{unchecked .rzk}
#define not-not-is-identity
  : (z : Bool) → not (not z) = z
  := ind-Bool
      ( \ z → not (not z) = z)
      ( refl)
      ( refl)
```

There is a way to fix the proof, but we'll need to learn more about
the identity types before we can do that.

## Natural numbers

!!! warning "Postulating natural numbers"

    This subsection currently provides postulates without explanations.
    Once Rzk has support for user-defined (higher) inductive types or built-in natural numbers,
    this section will be updated.

```rzk
#postulate ℕ
  : U
#postulate zero
  : ℕ
#postulate succ (n : ℕ)
  : ℕ

#postulate ind-ℕ
  ( C : ℕ → U)
  ( base : C zero)
  ( step : (n : ℕ) → C n → C (succ n))
  : ( n : ℕ) → C n

#postulate ind-ℕ-zero
  ( C : ℕ → U)
  ( base : C zero)
  ( step : (n : ℕ) → C n → C (succ n))
  : ind-ℕ C base step zero = base
#postulate ind-ℕ-succ
  ( C : ℕ → U)
  ( base : C zero)
  ( step : (n : ℕ) → C n → C (succ n))
  ( n : ℕ)
  : ind-ℕ C base step (succ n) = step n (ind-ℕ C base step n)
```

```rzk
#define rec-ℕ
  ( C : U)
  ( base : C)
  ( step : (n : ℕ) → C → C)
  : ℕ → C
  := ind-ℕ (\ _ → C) base step
```

```rzk
#define double-ℕ
  : ℕ → ℕ
  := rec-ℕ ℕ zero (\ _ m → succ (succ m))
```

```rzk
#define compute-ind-ℕ-zero
  ( C : ℕ → U)
  ( base : C zero)
  ( step : (n : ℕ) → C n → C (succ n))
  : C zero
  := base

#define compute-ind-ℕ-one
  ( C : ℕ → U)
  ( base : C zero)
  ( step : (n : ℕ) → C n → C (succ n))
  : C (succ zero)
  := step zero (compute-ind-ℕ-zero C base step)

#define compute-ind-ℕ-two
  ( C : ℕ → U)
  ( base : C zero)
  ( step : (n : ℕ) → C n → C (succ n))
  : C (succ (succ zero))
  := step (succ zero) (compute-ind-ℕ-one C base step)

#compute compute-ind-ℕ-two (\ _ → ℕ) zero (\ _ m → succ (succ m))
```
