# Chez port — Phase 0 results (jolt-cf1q.1)

De-risk + contract harness. Done; all gates green. Decisions feed Phases 1–3.

## 0a — value model (`host/chez/values.ss`, `test/chez/values-test.ss`)
Jolt value layer on Chez: nil sentinel (distinct from `#f`/`'()`), interned
keywords (NUL-separated intern key, no ns/name collision), ns+meta symbols,
exactness-aware `jolt=` ((= 1 1.0) is false), and a `jolt-hash` consistent with
it (non-finite-float safe). Chez's numeric tower IS Clojure's — ratios + bignums
come free. **37/37 tests.**

## 0b — host-neutral contract gate (`test/chez/`)
The spec corpus is data, so one contract gates every host. Extracted 2655
`[label expected actual]` cases from `test/spec/*.janet` into `corpus.edn` (valid
as BOTH EDN and Janet data). `run-corpus.janet` drives ANY jolt binary (pluggable
`JOLT_BIN`) at the CLI boundary, one fresh subprocess per case. Baseline vs Janet
`build/jolt` (compile mode, the port's target mode): **2641/2655**, 14 known CLI
divergences allowlisted (interpret-vs-compile leniency + invoke-collection-as-fn,
several non-canonical vs JVM anyway). **The gate fails only on NEW divergences** —
exactly what we want pointed at the Chez host in Phase 1+.

## 0c — persistent-collection perf (`spike/chez/collections-experiment.ss`)
The shim-vs-self-hosted decision for collections. Map-churn workload from
`bench/collections.clj` (30000 assoc/get over 4096 keys), correct result (30000):

| | mean | vs Janet | vs native ceiling |
|---|--:|--:|--:|
| Janet jolt HAMT | 258.6 ms | 1× | — |
| Chez persistent HAMT (hand-Scheme) | 6.3 ms (opt3) | **~41×** | ~15× |
| Chez native hashtable (mutable) | 0.43 ms | ~600× | 1× |

**Decision: self-host the persistent collections in Clojure (jolt-core).** A
persistent HAMT on the Chez substrate is ~41× faster than Janet's, so the
substrate is not the bottleneck; a compiled-Clojure HAMT should land near the
hand-Scheme one (cf. the mandelbrot finding that Chez compiles emitted code to
the native ceiling). The ~15× gap to mutable-native is the inherent persistence
cost (node-copy per assoc), identical in kind to JVM Clojure, and closes with
transients/editable nodes when needed. Keep a Scheme-shim HAMT as fallback ONLY
if Phase 2 shows the compiled-Clojure version underperforms.

Caveats (spike scope): the experiment uses integer-key-as-hash (shallow,
collision-free trie) and `merge-leaves` lacks real collision nodes — fine for the
substrate-speed question; the real RT needs `jolt-hash` + collision handling.

## Net
Substrate speed (compute + collections), value model, and the parity gate are all
validated and green. Phase 1 can bootstrap the real pipeline against a known,
enforceable contract.
