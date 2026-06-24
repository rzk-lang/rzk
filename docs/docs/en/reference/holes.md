# Holes

!!! info "Since version `0.9.0`"

A _hole_ is a part of a term that has been left unwritten. Holes support
incremental proof development. We leave a `#!rzk ?` where a subterm should go,
ask Rzk what is expected there, and fill it in once we know what to write. This
is the term-mode analogue of the interactive, hole-driven development of Agda.

A hole is written as a question mark. For example, the body of the following
definition is a single hole:

```rzk
#define swap
  : (A : U) -> (B : U) -> (Σ (_ : A) , B) -> Σ (_ : B) , A
  := \ A B p -> ?
```

A hole may be named by appending an identifier to the question mark. This is
convenient when a term has several of them:

```rzk
#define swap
  : (A : U) -> (B : U) -> (Σ (_ : A) , B) -> Σ (_ : B) , A
  := \ A B p -> (?b , ?a)
```

## Where holes are allowed

A hole is allowed only where its type is already known, that is, in _checking
position_, where Rzk checks a term against an expected type. The body of a
`#!rzk #define` with a type annotation is such a position. So is each component
of a pair, each branch of a `#!rzk recOR`, and the like.

A hole in _inference position_, where Rzk would have to guess its type, is
rejected. For example, a hole used as a function has no known type:

```rzk
-- error: cannot infer the type of a hole
#define bad
  : (A : U) -> A -> A
  := \ A x -> ? x
```

Rzk does not introduce metavariables, so it never guesses the type of a hole.
This keeps the meaning of a partial term predictable. A hole stands for a term
of exactly the expected type, and nothing more.

## Holes are errors by default

By default, `#!sh rzk typecheck` reports every hole as an error. Finished work
should not contain holes. This is the behaviour we want for a completed
formalisation and for continuous integration, where a file with an unfilled
hole must not check.

```sh
$ rzk typecheck swap.rzk
...
found an unsolved hole
expected type (goal):
  Σ (x₁ : B), A
```

## Inspecting a hole

To work _with_ holes, we pass `#!sh --allow-holes`. In this mode Rzk accepts the
holes and prints, for each one, its expected type (the _goal_) together with the
local context:

```sh
$ rzk typecheck --allow-holes swap.rzk
Hole ?b at swap.rzk:2
  goal:
    B
  context:
    p : Σ (x₁ : A), B
    B : U
    A : U
Hole ?a at swap.rzk:2
  goal:
    A
  context:
    p : Σ (x₁ : A), B
    B : U
    A : U
Everything is ok! (2 hole(s))
```

For a hole in the cube or tope layer the context is split into three sections,
following the layers of the type theory:

- **context** lists the local term variables and their types;
- **cube variables** lists the local variables ranging over cubes (such as
  `#!rzk t : 2`);
- **tope context** lists the tope assumptions in force at the hole.

Only _local_ hypotheses are shown. The global environment, that is every
previous definition, is deliberately excluded, so that the goal stays readable
even inside a large development.

The goal is kept symbolic. Composite definitions are shown as written, not
unfolded. For an extension type the goal carries its boundary, so the conditions
the term must satisfy are visible.

When a binder uses a pair pattern, for example `#!rzk \ (t , s) -> ...`, the
hypothesis is shown by its pattern, as `#!rzk (t, s) : ...`, rather than through
projections. The component names we wrote are thus kept in the goal and context.

## JSON diagnostics

For tooling that is not tied to a particular editor, and for continuous
integration, `#!sh rzk typecheck --json` emits every diagnostic, both type errors
and holes, as a JSON array on stdout. Progress messages go to stderr. The command
exits non-zero only when there is an error-severity diagnostic. A hole has
severity `#!json "warning"`, so under `#!sh --json` a file with holes still exits
zero.

Each diagnostic carries a severity, a stable `#!json "code"`, a source
`#!json "location"`, and a rendered `#!json "message"`. A hole additionally
carries its structured goal and context under `#!json "hole"`:

```json
{
  "code": "hole",
  "severity": "warning",
  "location": { "file": "swap.rzk", "line": 2 },
  "message": "Hole ?b at swap.rzk:2\n  goal:\n    B\n  ...",
  "hole": {
    "name": "b",
    "goal": "B",
    "termVars": [ { "name": "p", "type": "Σ (x₁ : A), B" }, ... ],
    "cubeVars": [],
    "topes": [],
    "shape": null
  }
}
```

## Tooling

The same goal-and-context information is available to tools as structured data,
not only as text. Editors and the Rzk playground can therefore show the goal and
context at a hole, and update them as the term is edited. The language server
reports each hole as a warning carrying its goal and context, following the
yellow "unsolved" highlight of Agda.

## Limitations

- A hole takes the type expected of it. It cannot yet be _refined_ by writing a
  partial term inside it, as the `#!agda {! ... !}` of Agda allows. We plan to
  add this.
