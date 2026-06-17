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

Baseline after inc 3g (letfn + declare): **672/672 compiled cases pass**, 0
divergences; 1986/2658 out of subset (await clojure.core on Chez). Inc 3e
(throw/try + ex-info) was 632/632; inc 3f's quote support + a seq.ss fix (empty
`map`/`filter` results are `()` not nil, matching Clojure) reached 664/664; inc 3g
(letfn -> Scheme `letrec*`, declare/def-no-init -> a reserved var cell) pulled 8
more corpus cases into the subset. `emit-fn` lowers multi-arity fns to a Scheme
`case-lambda` and variadic fns to a rest-arg lambda (rest list coerced to a jolt
seq, nil when empty).

## Phase 1 — clojure.core prelude emission (inc 3d, jolt-ocvi)
The `-e`-capable jolt-chez path: emit the clojure.core tiers
(`jolt-core/clojure/core/NN-*.clj`) through the same analyzer → emit pipeline as a
Scheme PRELUDE of `def-var!` forms, so user code's `(var-deref "clojure.core" …)`
resolves the fn at runtime. `emit/set-prelude-mode!` flips a switch: in the default
(subset) mode a non-native `clojure.core` ref is rejected ("out of subset"); in
prelude mode it lowers to a runtime `var-deref` so core fns chain through each
other. Host interop (`:host`) and unhandled IR ops still error in both modes —
those are the real gaps that need a hand-written RT shim or new emit support.

`core-prelude-probe.janet` (gated behind `JOLT_CHEZ_PRELUDE=1`) measures reach and
catalogs the gaps; macros are skipped (analyze-time only, not a runtime value):

    JOLT_CHEZ_PRELUDE=1 janet test/chez/core-prelude-probe.janet

Baseline after inc 3h (host-interop method calls): **354/355 non-macro core forms
emit** to Scheme (was 348 at inc 3g, 342 at inc 3f). A `.method` call now analyzes
to a `:host-call` IR node; the Chez emitter lowers it to a `jolt-host-call`
dispatch for the methods the RT shims — `.write` → port `display`, `.isDirectory`
→ `file-directory?`, `.listFiles` → `directory-list` — closing the io tier's
print-method defmethods and `file-seq` (now 20/20). Any other method is out of
subset (a clean emit-time reject, so it can't masquerade as a compiled-but-broken
divergence); the Janet back end punts ALL `:host-call` to the interpreter. Prior
incs: `:quote` reconstructs the raw reader form as RT constructors; `:throw` →
`jolt-throw`, `:try` → `guard` + `dynamic-wind`, `ex-info` native-op; `letfn` →
`letrec*`; `declare`/def-no-init → a reserved var cell. Remaining 1 gap: the regex
literal in `parse-uuid` (needs a regex engine on Chez — see jolt issue). The probe
has a regression floor (354).

Prior, inc 3b (seq tier + dynamic IFn, jolt-5pso): 595/595 compiled, 0 divergences,
2060/2655 out of subset. The seq tier brought up a list/lazy-seq type with
first/rest/next/seq/cons/list, map/filter/reduce/into/remove,
range/take/drop/concat/apply, keys/vals, and nth/peek/pop over seqs; dynamic IFn
dispatch (a keyword/vector/coll held in a local and called as a fn) routes through
the `jolt-invoke` fallback, closing the 3 ex-known divergences. The probe exits
non-zero on any NEW divergence.

(Prior, inc 3a: 433/436 compiled, 3 known IFn divergences, 2219 out of subset.
Inc 2: 182/182 compiled, 0 divergences, 2473 out of subset.)

It's a slow report (a Chez subprocess per case), so it's gated behind
`JOLT_CHEZ_CORPUS` out of the default suite, like the benches.
`test/chez/emit-test.janet` is the fast Phase-1 unit gate (real analyzer → Chez
parity for fib/mandelbrot + collections + regressions); both skip cleanly when
`chez` isn't on PATH.
