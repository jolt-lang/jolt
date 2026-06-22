# Chez test harness

The correctness gate for jolt. Pure Chez (+ Clojure for the JVM oracle).
Correctness is judged against the JVM-sourced conformance spec; the spec itself
lives in `test/conformance/` (see its `SPEC.md`). Run the whole gate with `make
test` from the repo root.

## The spec corpus

`corpus.edn` is the contract: ~2920 rows `{:suite :label :expected :actual}`, with
`:expected` sourced from reference JVM Clojure by `test/conformance/regen-corpus.clj`.
It is frozen (the canonical source) — add or change cases here, then re-source the
answers with `regen-corpus.clj` and re-certify with `test/conformance/certify.clj`.

## The gate runners (`host/chez/`)

- `run-corpus.ss` — runs every corpus case through the spine (read → analyze → IR →
  emit → eval, all on Chez), comparing each result by value-equality against the JVM
  `:expected`. A `known-fail` allowlist covers cases jolt can't match because Chez has
  no JVM host (Java classes, arrays, `BigDecimal`, opaque host-object printers, …);
  the gate fails only on a NEW divergence or if the pass count drops below the floor.

      chez --script host/chez/run-corpus.ss
      JOLT_CORPUS_LIMIT=200 …            # every-Nth stride, fast iteration
      JOLT_CHEZ_ZJ_FLOOR=N …            # override the floor (default 2678)

- `run-unit.ss` — host-specific unit cases (`test/chez/unit.edn`) that aren't in the
  JVM-portable corpus: dot-forms, java statics, io, reader, walk, vars/namespaces,
  refs. Each `:expr` is evaluated in-process and its printed value compared to a baked
  `:expected` (`:throws` asserts a raise).

- `selfcheck.sh` — self-host fixpoint: `bootstrap.ss` rebuild byte-equals the
  checked-in seed (`host/chez/seed/`).
- `smoke.sh` — real `bin/joltc -e` CLI smoke.

## Other Chez tests

- `values-test.ss` — the value model (nil/truthiness/collections). `make values`.
- `bench-chez.ss` — compute bench through the pipeline (opt-in; not in the gate).

All runners assume `chez` on PATH.
