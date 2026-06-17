# Chez port — Phase 0 test contract harness

The host-neutral correctness gate for the Chez re-host (epic jolt-cf1q). The
spec corpus is data, so the SAME contract validates every host.

## Files
- `extract-corpus.janet` — parses `test/spec/*.janet` `(defspec …)` tables as
  data and writes `corpus.edn` (2655 `[label expected actual]` cases). The file
  is valid as BOTH EDN (a future Chez-jolt runner) and Janet data (the runner
  below). Regenerate: `janet test/chez/extract-corpus.janet`.
- `corpus.edn` — the extracted contract (generated; checked in for convenience).
- `run-corpus.janet` — drives a TARGET jolt binary, one fresh subprocess per case
  (fresh ctx = per-case isolation), checking `(= expected actual)` prints `true`
  at the CLI, or that a `:throws` case exits non-zero. Pluggable target:
  - `janet test/chez/run-corpus.janet`                    # default build/jolt
  - `JOLT_BIN=build/jolt-chez janet test/chez/run-corpus.janet`   # Phase 1+
  - `JOLT_CORPUS_LIMIT=400 …`                              # every-Nth stride, fast
- `known-divergences.edn` — allowlist of cases that diverge at the CLI boundary.
  The gate fails only on a NEW divergence; known ones are reported but tolerated.
- `values-test.ss` / `../../host/chez/values.ss` — Phase 0a value model + tests.

## The reference baseline (2026-06-17, Janet `build/jolt`, compile mode)
2641/2655 pass; 14 known divergences. They split into:
- **interpret-vs-compile leniency** — `:throws` cases where interpret mode raises
  but compile mode returns (`< nil`, `> with nil`, `neg? keyword`, `max`/`min-key`
  on non-numbers). Several are also non-canonical vs JVM Clojure.
- **invoke-collection-as-fn** — the `transient / invokable lookup` suite invokes
  transients/collections as fns (`((transient {:x 7}) :x)`); compile mode (and
  JVM Clojure) reject it.
- **`xml-seq walks`** — one structural case.

The compile-only Chez host (JVM-canonical oracle) should MATCH OR FIX these. The
gate's job is to catch *regressions* the port introduces, not to bless these.

## Why the CLI boundary
The runner tests through `jolt -e`, exactly how the Chez host will be exercised —
not the in-process `eval-string` the Janet `defspec` harness uses. The two differ
on a handful of cases (the allowlist), and the CLI boundary is the portable one.

## Phase 1 — first parity number (subset probe)
The full `run-corpus.janet` gate drives an `-e`-capable jolt binary; the Chez
host can't answer arbitrary `-e` until all of clojure.core is bootstrapped onto
Chez (Phase 2). Until then, `run-corpus-chez.janet` reports parity for the subset
the Phase-1 back end (`host/chez/emit.janet`) can already compile: each case is
run through the live analyzer → Scheme emitter → Chez via `host/chez/driver`.
Cases that reference unimplemented stdlib/host fns fail to EMIT (a clean
compile-time signal) and are counted "out of subset", not as divergences.

    JOLT_CHEZ_CORPUS=1 janet test/chez/run-corpus-chez.janet

Baseline after inc 3a (persistent collections, jolt-wgbz): **433/436 compiled
cases pass**, 3 known divergences, 0 NEW; 2219/2655 out of subset (await the seq
tier + core on Chez). The 3 known divergences are dynamic IFn dispatch — a
keyword/vector held in a LOCAL and called as a fn (`(let [k :a] (k m))`); the
STATIC literal forms (`(:a m)`, `({:a 1} :a)`) are supported. They're
allowlisted in the probe; it exits non-zero on a NEW divergence.

(Prior, inc 2 baseline: 182/182 compiled, 0 divergences, 2473 out of subset.)

It's a slow report (a Chez subprocess per case), so it's gated behind
`JOLT_CHEZ_CORPUS` out of the default suite, like the benches.
`test/chez/emit-test.janet` is the fast Phase-1 unit gate (real analyzer → Chez
parity for fib/mandelbrot + collections + regressions); both skip cleanly when
`chez` isn't on PATH.
