# Re-hosting Jolt on Chez Scheme — phased plan

Decision (2026-06-17): port jolt's runtime substrate from Janet to Chez Scheme
(cisco/ChezScheme). The spike (`spike/chez/RESULTS.md`) validated the thesis:
the compute-substrate ceiling is ~12-47x faster than Janet (mandelbrot
166->13.4ms matching jolt's own C-codegen result; fib 246->5.2ms), Chez's
native compiler reaches that at runtime with the REPL intact, size stays
single-digit MB, and the one regression (memory baseline ~2.5x) is opt-in
tunable via WPO + stripped boot + AOT-under-petite.

This plan is built around two north stars beyond raw speed:

1. **Minimal host shim.** Every line that stays in Scheme is a line we failed to
   self-host. The port is the forcing function for jolt-tzo/uqi/lcn: shrink the
   Scheme seed to the irreducible primitive set, push everything else into
   portable Clojure (`jolt-core/`).
2. **Tests are the contract.** The spec/conformance corpus is host-neutral data
   (`[name expected-clj actual-clj]` triples compared via jolt's own `=`). It is
   the acceptance gate for "the port is correct" — Chez-jolt must pass the same
   corpus the Janet host passes, with no regression to the clojure-test-suite
   baseline.

## The minimal host shim (target end-state)

What MUST be Scheme (the irreducible primitive layer the self-hosted core rests
on) vs what MOVES into portable Clojure:

### Stays in Scheme (the shim)
- **Value primitives that can't bottom out in Clojure without circularity:**
  the `nil` sentinel (distinct from `#f` and `'()` — the classic Lisp-on-Lisp
  trap), keyword/symbol records (Clojure symbols carry ns + meta), char/string
  bridging. NOTE: Chez's **numeric tower is a windfall** — int/float/ratio/
  bignum are native, so jolt gets exact ratios + bignums for free (Janet lacked
  them).
- **Mutable cell / box + array primitives** the self-hosted RT builds on (var
  roots, transients, HAMT node arrays).
- **A type-tag / record primitive** for deftype/protocol dispatch (Chez records).
- **`host/compile`** — eval a backend-emitted Scheme form to a procedure. On Chez
  this is literally `eval`/`compile`. Trivial. This is the whole backend
  host-dependency.
- **FFI / interop bridge** (foreign-procedure) for host interop calls.
- **Persistent-collection hot nodes** — ONLY IF the Clojure-on-Chez version
  (Phase 0c) doesn't hold perf. Open question, decided by measurement.

### Moves into portable Clojure (`jolt-core/`)
- **The reader** (text -> forms). CLJS self-hosts its reader; ours can too. ~33KB
  of Janet leaves the host. Not hot.
- **Analyzer + IR + passes** — already portable Clojure. No change.
- **The backend emitter** — its LOGIC becomes Clojure that emits Scheme forms as
  data; only `host/compile` crosses the seam.
- **macros + clojure.core** — finish the jolt-uqi/tzo migration (most already
  Clojure).
- **Protocol/multimethod dispatch logic** — over the host tag primitive.
- **Persistent collections** — candidate (Phase 0c), perf-permitting.

### Dropped entirely
- **The tree-walking interpreter** (`eval_base/eval_runtime/eval_special/
  eval_resolve`, ~140KB Janet). On Chez, native `compile` is always present and
  cheap, so the compile-only path can cover every form — no `jolt/uncompilable`
  fallback needed. The interpreter's role as the correctness *oracle* transfers
  to the spec corpus + JVM Clojure (the real reference), which is strictly
  better. This is the single largest shim reduction and the biggest open risk;
  Phase 1 validates that compile-only is total before we commit to the drop.

## Test contract strategy

- **Corpus is the contract.** The 44 `test/spec/*.janet` files, the conformance
  cases, `clojure-test-suite`, and `clojure-stdlib-suite` are host-neutral: pure
  Clojure source + expected values. Extract the triples into a runner that can
  target an arbitrary `jolt` binary (subprocess at the Clojure boundary).
- **Parity gate.** Chez-jolt must pass the same corpus as Janet-jolt; the
  clojure-test-suite baseline is the bar (raise it when it rises, never lower).
- **Oracle shift.** Today's 3-mode conformance (interpret/compile/self-host)
  loses the "interpret" leg when the interpreter is dropped; the golden expected
  values + JVM Clojure become the oracle. Keep the frozen expected values.
- **Dual-run during migration.** Run BOTH hosts against the corpus until Chez
  reaches parity, then retire the Janet host.

## Phases (-> beads epic)

**Phase 0 — Foundations & contract harness** (de-risk; no jolt pipeline yet)
- 0a. Chez RT value model: nil sentinel, keyword/symbol records, numeric-tower
  mapping, `=`/hashing. Resolve the nil/`'()`/`#f` representation up front.
- 0b. Host-neutral test-contract runner: extract the spec/conformance corpus to
  drive an arbitrary jolt binary; stand up the parity-gate machinery.
- 0c. Persistent-collection perf experiment: HAMT/PV in Clojure-on-Chez vs
  Scheme-native — the data that decides what stays shim vs self-hosted.

**Phase 1 — Minimal Chez kernel + real-pipeline bootstrap**
- Scheme shim (value layer, var/ns cells, `host/compile`, cenv impl).
- `jolt.backend` Scheme-emit target for the IR the analyzer already produces.
- Bootstrap jolt-core (ir/analyzer) on Chez; compile + run `(+ 1 2)` -> fib ->
  mandelbrot through the REAL pipeline. Gate: compute benches run end-to-end and
  hit ~the spike ceiling; confirm compile-only is total (no fallback needed).

**Phase 2 — clojure.core to spec parity**
- Bring up persistent collections (per 0c) + seq/coll/print/refs/io tiers over
  the Chez RT. Gate: spec + conformance + clojure-test-suite parity with the
  Janet baseline.

**Phase 3 — Self-host expansion (shrink the shim)**
- Move the reader into jolt-core. Continue core-* leaf migration (jolt-uqi/ded/
  tzo, now targeting Chez). Drop the tree-walking interpreter. Gate: shim equals
  the documented minimal set; parity holds.

**Phase 4 — Deployment & optimization modes** (the "optimize specific cases" lever)
- Wire `JOLT_WHOLE_PROGRAM`/direct-link to emit specialized Scheme (fl*/fx*),
  feeding Chez `compile-whole-program`. `jolt build`: WPO + strip-fasl +
  AOT-under-petite + heap tuning -> small fast binary (jolt-0w9u reframed).
  Rebuild fibers/async on call/cc + threads. Gate: full bench suite incl.
  collections/binary-trees (the GC axes); size + memory measured vs spike
  baseline.

**Phase 5 — Retire the Janet host**
- Chez parity + perf confirmed -> remove `src/jolt/*.janet` seed. jolt-core
  unchanged. Janet becomes a historical/alternate host proving portability.

## Beads reconciliation

- **Closed (obsolete — Janet bytecode-VM / cgen / Janet-dispatch mechanisms Chez
  replaces wholesale):** jolt-ffn (epic, already concluded flat), jolt-5vsp.1,
  jolt-qx70, jolt-l1l4 (cgen), jolt-cm7t, jolt-fw2 (Janet dispatch substrate),
  jolt-pria (Janet ctx cold-build startup).
- **Recommend close (confirm) — Janet constant-factor passes Chez's JIT/GC/WPO
  subsume:** jolt-826, jolt-27w, jolt-t6r, jolt-8flj, jolt-3ko, jolt-t34,
  jolt-u1f. (jolt-ffn's own STATUS says the gap is "generic-runtime overhead…
  the JVM erases via JIT + inline caching + unboxing" — exactly Chez's job.)
- **Reframed under the Chez epic:** jolt-0w9u + .1 -> Phase 4 deploy mode +
  closed-world audit; jolt-1r86 -> Phase 0b/4 bench validation; jolt-lcn /
  jolt-uqi / jolt-tzo / jolt-ded / jolt-brh / jolt-7dl -> Phase 2/3 self-hosting,
  now targeting Chez; jolt-5vsp (foundational-runtime epic) -> parent, the Chez
  port is its realization.
- **Keep host-agnostic:** deps beads (jolt-x4o, jolt-xkd, jolt-pnje, jolt-vley);
  correctness bug jolt-jk23.
