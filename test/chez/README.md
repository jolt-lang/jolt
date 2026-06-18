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

Baseline after inc 3i (regex): **355/355 non-macro core forms emit** to Scheme —
the whole non-macro clojure.core now lowers. inc 3i closed the last gap, the regex
literal in `parse-uuid`: a `#"…"` literal lowers to a `:regex` IR node and the Chez
emitter emits a `jolt-regex` value over **vendored irregex** (Alex Shinn, BSD,
`vendor/irregex` submodule) — a portable Scheme regex with PCRE/Java-style string
patterns. `re-pattern`/`re-matches`/`re-find`/`re-seq`/`regex?` are `def-var!`'d
into clojure.core (`host/chez/regex.ss`); they stay OUT of the subset native-ops
(irregex's Unicode/property-class semantics differ from the seed's byte-PEG
approximation), so they resolve in prelude mode — the path the assembled prelude
takes — without dragging engine-difference divergences into the subset corpus. The
Janet back end punts `:regex` to the interpreter (the seed compiles `#"…"` to a
Janet PEG). Prior incs: inc 3h `.method` → `:host-call` (`jolt-host-call` for
`.write`/`.isDirectory`/`.listFiles`); `:quote`, `:throw`, `:try`, `ex-info`,
`letfn` → `letrec*`, `declare`/def-no-init → reserved var cell. The probe has a
regression floor (355) — every non-macro core form must keep emitting.

## Phase 1 — the assembled prelude: -e-capable jolt-chez (inc 3j, jolt-9ziu)
Once the whole non-macro clojure.core emits (inc 3i), the milestone is to ASSEMBLE
it: `driver/emit-core-prelude` emits every non-macro core form across the
dependency-ordered tiers as a `def-var!` (prelude mode), concatenated into a
Scheme prelude. `bin/jolt-chez -e EXPR` loads `rt.ss` + that prelude + a
post-prelude override, emits the user expression in prelude mode, and runs it on
Chez — an `-e`-capable jolt-chez (analysis on Janet, execution on Chez). The
prelude is cached on disk keyed by a fingerprint of the core sources + the RT.

`run-corpus-prelude.janet` is the full parity gate this opens (the prelude-backed
sibling of `run-corpus-chez.janet`): it assembles the prelude once, then runs every
corpus case with all of core present, bucketing the result —

    JOLT_CHEZ_PRELUDE_CORPUS=1 janet test/chez/run-corpus-prelude.janet
    JOLT_CORPUS_LIMIT=200 …    (every-Nth stride, fast)

Parity baseline: inc 3j **1220/2497**; 3k (converters, jolt-t6cr) **1326**;
3l (transients, jolt-kl2l) **1382**; 3m (numeric-edge emit + variadic assoc!,
jolt-q3w8) **1407/2497**, 0 NEW divergences (14 allowlisted:
dynamic vars `*ns*`/`*clojure-version*`/`*unchecked-math*`, var/`*ns*` rendering,
class names, eval-order, with-open — all deferred Phase-2 / dynamic-var gaps).
- inc 3k `host/chez/converters.ss`: `str`/`subs`/`vec`/`keyword`/`symbol`/`compare`/
  `int`/`double`/`gensym` (seed natives — `str` reuses the printer, `compare` is the
  3-way port, the symbol no-ns sentinel is `#f` to match emit's quoted-symbol
  lowering so `(= 'x (symbol "x"))` holds).
- inc 3l `host/chez/transients.ss`: `transient`/`persistent!`/`conj!`/`assoc!`/
  `dissoc!`/`disj!`/`pop!` as copy-on-write over the persistent collections (correct
  semantics, no in-place perf), plus persistent `disj`. `get`/`count`/`contains?`
  are redefined to see THROUGH a transient (frequencies/group-by do `(get tm k)` on a
  transient map); `vector?` on a transient vector is false, which group-by relies on.

- inc 3m: `##Inf`/`##-Inf`/`##NaN` emitted to bare `inf`/`nan` (unbound in Chez);
  emit-const now lowers them to `+inf.0`/`-inf.0`/`+nan.0`, the `-e` printer renders
  `inf`/`-inf`/`nan` and `str` renders `Infinity`/`-Infinity`/`NaN` (Clojure). Plus
  variadic `assoc!`. (`str` of inf *inside a collection* still wants the long form —
  the Phase-2 recursive str renderer — so `[inf inside coll]` is allowlisted.)
- inc 3n `host/chez/natives-seq.ss` (jolt-y6mv): the dominant prelude-parity crash
  bucket was `apply non-procedure jolt-nil` — core fns calling seed-native seq fns
  (`src/jolt/core_coll.janet`) that have no Chez shim, so `var-deref` yields
  jolt-nil. A static scan of the assembled prelude found 52 referenced-but-undefined
  `clojure.core` names; this increment shims the safe, high-value seq fns: `mapcat`/
  `take-while`/`drop-while`/`partition` (collection arities — the 1-arg transducer
  forms are jolt-kxsr), `sort` (compare default; a comparator may return a 3-way
  number or a boolean less-than), and `reduced`/`reduced?` — a `jolt-reduced` record
  in `seq.ss` that `reduce` short-circuits on and `deref` unwraps (so `unreduced`
  works). `identical?` = `jolt=` (the seed's definition). `list?` was deferred: a
  Chez lazy seq and a list are both `cseq`, so it can't be told apart without a
  distinct list type (a real divergence risk).

The remaining buckets are the punch-list the next increments chase (at 1467/2497):
~361 emit-fail (genuine host interop — qualified Java/Janet refs, runtime
`defmacro`/`eval`, out of the analyzer's subset) and ~655 runtime crashes — still
~590 `apply jolt-nil` (more host-coupled natives without a shim: `meta`/`with-meta`,
`format`, the `clojure.string` natives, bit ops, `var`/`volatile`/`future`/
thread-binding ops), the transducer arities (jolt-kxsr — 1-arg `filter`/`map`/`take`/
`take-while` + 3-arg `into`), `cdr`-on-`()` and `\p{}` regex classes (jolt-y1zq), and
multimethod dispatch (Phase 2 jolt-9ls5).

Two host shims landed with the prelude. `host/chez/atoms.ss`: atom/deref/swap!/
reset! (+ compare-and-set!/swap-vals!/reset-vals!) — host-coupled mutable cells the
overlay assumes are native; needed at the prelude's LOAD time
(`global-hierarchy = (atom (make-hierarchy))`). `host/chez/predicates.ss`: the type
predicates + `name`/`namespace`/`boolean` the overlay assumes are seed natives
(`nil?`/`number?`/`string?`/`map?`/`vector?`/`set?`/`seq?`/`coll?`/`fn?`/…), matching
the seed's strict semantics. `host/chez/post-prelude.ss` re-asserts `char?`/`atom?`
AFTER the prelude — the overlay implements those two by reading a value's
`:jolt/type` key (a Janet-host assumption that's false for Chez-native chars/atoms),
and its `def-var!` would otherwise clobber the correct native shims.

The 8 print-method/print-dup `defmulti`/`defmethod` forms (50-io) can't LOAD yet
(no multimethod runtime on Chez — Phase 2); a silent load guard in the assembled
prelude lets the rest load and turns them into lazy gaps.

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
