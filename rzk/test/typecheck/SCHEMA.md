# Typecheck Tests Schema

Test cases live under `test/typecheck/cases/`.

## Paired file case

- `something.rzk` — Rzk source (must parse).
- `something.expect.yaml` — expectations for typechecking that file alone with `typecheckModulesWithLocation`.

## Directory case (multiple modules)

- `my-case/expect.yaml` — same fields as below, plus **`modules`** (non-empty list of filenames relative to that directory, typechecked in order).
- `*.rzk` files in that directory.

## YAML fields

| Field | Required | Description |
|--------|----------|-------------|
| `status` | yes | `ok` if typechecking must succeed; `error` if it must fail with a type error. |
| `error_tag` | if `status: error` | Name of the `TypeError` constructor after stripping `ScopedTypeError` wrappers, e.g. `TypeErrorUnify`, `TypeErrorUndefined`. |
| `message_contains` | no | Substrings that must all appear in the rendered error (`ppTypeErrorInScopedContext'`). |
| `line` | no | 1-based line number (approximate) where the error is reported. |
| `regression_for` | no | Traceability strings (PR numbers, issue ids, commit themes). |
| `modules` | no | If set (directory case), ordered list of module files for one `typecheckModulesWithLocation` run. |
| `api` | no | Omit or `strict` (default): `typecheckModulesWithLocation` (throws on first error). `collect`: `typecheckModulesWithLocation'` — returns a list of errors without using `throwError`; note that the typechecker still stops per-module chaining when a module reports errors (see implementation in `Rzk.TypeCheck`). |

Parse failures are always test failures; they are not expressed in this schema.
