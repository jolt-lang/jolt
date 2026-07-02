# Chez test harness

The correctness gate for jolt. Pure Chez (+ Clojure for the JVM oracle).
Correctness is judged against the JVM-sourced conformance spec; the spec itself
lives in `test/conformance/` (see its `SPEC.md`). Run the whole gate with `make
test` from the repo root.

## The spec corpus

`corpus.edn` is the contract: ~3570 rows `{:suite :label :expected :actual :portability}`, with
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
      JOLT_CHEZ_ZJ_FLOOR=N …            # override the floor (see run-corpus.ss)

- `run-unit.ss` — host-specific unit cases (`test/chez/unit.edn`) that aren't in the
  JVM-portable corpus: dot-forms, java statics, io, reader, walk, vars/namespaces,
  refs. Each `:expr` is evaluated in-process and its printed value compared to a baked
  `:expected` (`:throws` asserts a raise).

- `selfcheck.sh` — self-host fixpoint: `bootstrap.ss` rebuild byte-equals the
  checked-in seed (`host/chez/seed/`).
- `smoke.sh` — real `bin/joltc -e` CLI smoke.
- `cts.sh` — the vendored [jank-lang/clojure-test-suite](https://github.com/jank-lang/clojure-test-suite)
  (`vendor/clojure-test-suite`, a per-core-fn clojure.test suite shared across
  Clojure dialects), run one namespace per `joltc` process (a hang or crash is
  contained) through the `test/chez/cts-app` project and `cts-run` runner.
  Per-namespace fail/error counts must exactly match the checked-in baseline
  `test/chez/cts-known-failures.txt` — a namespace doing worse fails the gate,
  and one doing better fails as stale until the baseline is updated in the same
  change. `make cts`;
  `JOLT_CTS_NS=ns1,ns2` runs a subset verbosely,
  `JOLT_CTS_WRITE_BASELINE=1` regenerates the baseline.

## Other Chez tests

- `values-test.ss` — the value model (nil/truthiness/collections). `make values`.
- `bench-chez.ss` — compute bench through the pipeline (opt-in; not in the gate).

All runners assume `chez` on PATH.
