# rzk benchmark harness

Measures typechecking performance on pinned real-world corpora, so that
optimisation work and releases carry before/after numbers. The harness is a
single Haskell Stack script (`bench/bench.hs`, run on the workspace
resolver); it needs only `stack`, `git`, and a built `rzk`.

## Usage

```sh
bench/bench.hs run                  # 3 timed full-sHoTT runs, appended to the CSV
bench/bench.hs run --quick          # prefix subset up to 05-segal-types
bench/bench.hs run --runs 5 --rts "-N1" --label my-experiment
bench/bench.hs profile              # profiled build + run, .prof summary
bench/bench.hs summary FILE.prof    # summarise an existing profile
bench/bench.hs setup                # just clone/pin the corpus
```

The rzk binary is the current Stack build by default (`stack build` first);
set `RZK_BIN=/path/to/rzk` to benchmark any other binary. `profile` builds
and uses the Stack profiling build (`--profile --ghc-options=-fprof-auto`)
unless `RZK_BIN` points at one. Note that the `rzk_commit` CSV column
records the repository `HEAD`; when `RZK_BIN` points at a binary built from
elsewhere, use `--label` to say what was actually measured.

## What is measured

Each run is a fresh process; metrics come from the GHC RTS one-shot summary
(`+RTS -t --machine-readable`, which is itself a `Read`-able Haskell
association list): wall clock, mutator and GC wall time, total allocation,
maximum live bytes (residency), memory in use, GC count, and wall-clock
productivity. One CSV row per run is appended to `bench/results/results.csv`
together with the rzk commit (with a `-dirty` marker for uncommitted source
changes), rzk version, corpus commit, mode, and RTS options; the median wall
time is printed.

`profile` runs once under the cost-centre profiler, keeps the `.prof` under
`bench/results/`, and prints the totals, the top flat cost centres, and the
tope solver's inherited time share (summing flat times instead would badly
under-count the solver, whose work is mostly attributed to the term-equality
and substitution helpers it calls).

## Corpora

Corpora are declared in the `corpora` list at the top of `bench/bench.hs`
(URL, pinned commit, quick-mode file selection) and cloned on demand into
`bench/corpora/` (gitignored, as is `bench/results/`). The pin is checked
out detached and must be clean. `--quick` typechecks an in-order prefix of
the project instead of the whole corpus; for sHoTT it ends at
`05-segal-types.rzk.md`, the file with the hardest tope-solver queries. To
bump a pin, edit the `corpora` list.
