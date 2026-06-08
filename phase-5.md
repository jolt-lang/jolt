# Phase 5 — True Laziness (jolt-c09)

Final phase of the `jolt-1j0` clojure.core migration epic. Make jolt's sequence
generators and transformers genuinely lazy, so infinite seqs and lazy
compositions work and stop hanging the evaluator. This is the deepest and
riskiest phase — sub-stage it and gate every step.

> Issue: `bd show jolt-c09`. Depends on Phase 4 (`jolt-ldf`, done). Blocks nothing
> — it's the last phase.

---

## 1. Current state (what already works, what doesn't)

**The LazySeq machinery exists and is sound.** (`src/jolt/phm.janet`)
- A LazySeq is `@{:jolt/type :jolt/lazy-seq :fn thunk :realized false :val nil}`.
- A thunk returns `nil` (empty) or a cons cell `[first-val rest-thunk]`.
- `realize-ls` forces one cell (memoized via `:realized`), with a `:jolt/pending`
  sentinel that makes **self-referential** seqs work (`(def ones (lazy-seq (cons 1 ones)))`).
- `ls-first` / `ls-rest` / `ls-seq` / `ls-count` walk it. `lazy-seq?` detects it.

**Already lazy (keep):**
- Infinite generators: `(range)`, `(repeat x)`, `(iterate f x)`, `(cycle ...)`,
  `repeatedly` return LazySeq. Bounded forms (`(range n)`, `(repeat n x)`) are
  eager tuples/arrays — correct, they're finite.
- `map`/`filter` are **hybrid**: lazy when the input is a LazySeq, eager (and
  representation-preserving) when the input is a concrete collection.
- `take`/`drop`/`take-while` pull lazily from a LazySeq input but **return an eager
  array** (fine for bounded `take`, wrong for the others on infinite tails).
- Conformance already covers the working cases (self-ref fib, `iterate`, `count`
  of `take`, `filter`/`take-while`/`remove` over `(range)`): see
  `test/integration/conformance-test.janet` lines ~21–143.

**The gaps (what hangs):**
1. **Eager transformers that force their input** even when it's infinite. Confirmed
   callers of `realize-for-iteration` in their bodies: `remove`, `interpose`,
   `distinct`, `take-nth`, `map-indexed`, `keep-indexed`, `partition-all`,
   `partition-by`, `drop-while`. Plus `partition`, `interleave`, `concat`,
   `dedupe`, `flatten`, `tree-seq`, `mapcat`, `keep`, `sequence` need an
   infinite-input audit.
2. **`map`/`filter` over a *concrete vector* return an eager array**, not a lazy
   seq. Clojure returns a lazy seq. This is a **representation decision** (§3 Step 6).
3. **`realize-for-iteration` is the universal forcing point** (57 call sites). Many
   are legitimate realization boundaries (`count`, `into`, `reduce`, `vec`, `pr`),
   but any transformer that calls it on a lazy input loses laziness.
4. **Evaluator eager assumptions** — the interpreter/compiler may realize seqs in
   places (apply arg spreading, `doseq`, destructuring a seq). Audit needed.
5. **CPU-bound hangs are uninterruptible.** An infinite realization is a tight
   Janet loop with no yield points, so `ev/with-deadline` cannot truncate it
   in-process — it pins the core. This is why the suite runs each file in a
   **subprocess** (`os/spawn` + 6 s `ev/with-deadline`, then `os/proc-kill`). Phase
   5 testing must do the same (see §7).

---

## 2. Design principles (the cardinal rules)

1. **A transformer never forces its input.** It returns a LazySeq whose thunk pulls
   one element at a time via `core-first`/`core-rest`/`seq-done?`. No
   `realize-for-iteration` inside a transformer.
2. **Force only at realization boundaries.** Exactly the operations that *must* see
   all elements: `pr`/`print`/`str` rendering, `=`, `count`, `reduce`, `into`,
   `vec`/`seq`/`doall`, `doseq`, `nth`/`last` (these pull only as far as needed),
   `apply` (spreads finitely). These are allowed to loop; on a genuinely infinite
   seq they hang — matching Clojure.
3. **One-element-at-a-time, memoized.** Reuse `make-lazy-seq`/`realize-ls`; never
   re-walk. `realize-ls`'s `:jolt/pending` guard preserves self-reference.
4. **Stack safety.** A chain of N lazy wrappers must not consume N stack frames per
   element. Realize iteratively (a `while` over `realize-ls`), not by deep
   recursion through `ls-rest`. Watch `concat`/`mapcat`/`lazy-cat` especially.
5. **Multi-arity stays correct.** `map`/`mapcat` over multiple colls advance each
   input one step per output element and stop at the shortest.

---

## 3. Step-by-step implementation

Order matters: build the helper layer, then convert transformers leaf-first, then
fix boundaries, then the evaluator. Gate (§6) after **every** numbered step.

### Step 0 — Safety net ✓ (commit e2e189a)
- Record the baseline: conformance 229×3, clojure-test-suite `baseline-pass=3926`,
  fixpoint stage1==2==3, self-host, all specs+unit, `lazy-seqs-spec` /
  `sequences-spec` / `transducers-spec` green. ✓
- Build the **infinite-seq harness** first (see §6.2, "Deadlined infinite-seq
  spec") so every subsequent step is verified against hangs, not just values. ✓
  → `test/support/lazy-eval.janet` (subprocess worker) +
    `test/integration/lazy-infinite-test.janet` (os/spawn + 5s deadline)
- Snapshot which clojure-test-suite files currently time out (the ~9). Save the
  list — it's the acceptance target. ⚠ 9 files recorded but not yet re-verified post-conversion.

### Step 1 — Lazy combinator layer ✓ (commit e2e189a)
Add a small set of internal lazy builders so transformers compose uniformly,
rather than each re-implementing the thunk dance:
- `lazy-cons val thunk` → a LazySeq cell of `val` + a deferred rest. ✓
  → `src/jolt/phm.janet` line 208; registered in core-bindings as `"lazy-cons"`.
- `lazy-from coll` → coerce any seqable to a uniform lazy view *without forcing*
  (vector/list/set/map/string/LazySeq → a LazySeq that pulls element by element).
  This is the lazy analogue of `realize-for-iteration` and the key primitive: every
  transformer takes `(lazy-from input)` and walks it with `core-first`/`core-rest`. ✓
  → `src/jolt/core.janet` line 1112; registered in core-bindings as `"lazy-from"`.
- `seq-done?` already exists — confirm it short-circuits without forcing the tail. ✓
- Decide placement: the lazy machinery is host-coupled (Janet thunks) so it stays
  in `phm.janet`/`core.janet`; transformers that are already in the overlay tiers
  call these as primitives. ✓

### Step 2 — Convert the core transformers (leaf-first) ✓ (commits e2e189a, d16e1f4, 97781b3, ff8ffb8)
Make each return a LazySeq over `lazy-from input`. Do them in dependency order, one
small batch per commit, each gated:
- **2a. Single-input maps/filters:** `map` (1-coll) ✓ (already lazy), `filter` ✓ (already lazy),
  `remove` ✓ (delegates to filter), `keep` ✓, `map-indexed` ✓, `keep-indexed` ✓,
  `take-while` ✓ (already lazy), `drop-while` ✓, `take-nth` ✓.
- **2b. Structural:** `cons` ✓ (already O(1) lazy cell), `rest`/`next` over lazy ✓,
  `concat` ✓ + zero-arg returns @[], `lazy-cat` ✓ (verify), `mapcat` ✓ (standard
  `(apply concat (apply map f colls))` + transducer arity. Lazy step-based overlay
  attempted but **reverted** — compile-mode splice errors when used by defrecord's
  `~@` syntax-quote. Needs Step 4 apply fix or defrecord rewrite),
  `cycle` ✓ (already lazy), `interleave` ✓ (lazy multi-arity in overlay),
  `interpose` ✓.
- **2c. Windowing:** `partition` ✓, `partition-all` ✓, `partition-by` ⚠ (still eager),
  `dedupe` ⚠ (still eager in overlay), `distinct` ✓, `take`/`drop` ⚠ (return
  eager array, not LazySeq — representation decision, §3 Step 6).
- **2d. Multi-input `map`/`mapcat`** over several colls (shortest-stops). ✓
  → 9 new tests added to `sequences-spec.janet`, verified against Clojure & CLJS
    reference implementations. Multi-input `map` already correct; `mapcat` uses
    the standard overlay impl. No code changes needed.
- **2e. Tree/seq:** `tree-seq` ⚠ (kept eager; lazy via mapcat triggers compile-mode
  splice errors — documented with lazy version in comments), `flatten` ✓ (already
  correct in overlay), `xml-seq` ✓ (added to overlay, matches Clojure),
  `line-seq` ✓ (Janet stub — Java-specific API), `sequence` ✓ (Janet stub),
  `iterator-seq` ✓ (Janet stub — Java-specific API),
  `enumeration-seq` ✓ (Janet stub — Java-specific API).
- For each: a transducer arity may exist (`td-*`) — leave it; only the
  collection arity changes. ✓

### Step 3 — Realization boundaries ✔ audit complete (documented in phase-5.md)

Audit of 56 `realize-for-iteration` call sites in `src/jolt/core.janet` (excludes the definition at line 96). Each site classified below.

#### Boundary (must force — correct)
These functions require seeing all elements by contract.

| Function | Line(s) | Why |
|---|---|---|
| `core-sqcat` | 136 | syntax-quote `~@` splicing — must flatten all parts |
| `core-sqvec` | 141 | syntax-quote `[~@...]` — must flatten all parts |
| `core-every?` | 205 | short-circuits on falsy but must iterate |
| `eq-seqable` (part of `=`) | 258 | equality of lazy-seqs: must realize to compare elements |
| `core-apply` | 506 | arg spread — forces final collection, matching Clojure |
| `core-cons` | 626 | only reached for concrete non-lazy input; lazy already cell-based |
| `core-vec` | 650 | builds a vector — must see all elements |
| `core-select-keys` | 736 | filters keys from a collection |
| `core-zipmap` | 742×2 | needs both key and value collections fully |
| `reduce-with-reduced` | 821 | reduce must see all elements (set guard: concrete collections only) |
| `core-into` | 847 | consumes entire collection into target |
| `core-reduce` (3-arg) | 974 | must see all elements (set guard) |
| `core-nth` (concrete) | 1199 | finite pull: must walk to index |
| `core-take` (concrete) | 994 | finite prefix pull; could be element-at-a-time, but bounded |
| `core-reverse` (concrete) | 1164 | reorder: must see all elements |
| `core-sort` | 1212 | sorting: must see all elements |
| `core-sort-by` | 1225 | sorting: must see all elements |
| `core-set` | 1543 | builds a set — must see all elements |
| `core-str-join` | 1670 | rendering: must see all elements |
| `pr-render-seq` (in `str-render-one`) | 1626 | rendering lazy-seqs to strings |
| `core-shuffle` | 2395 | reorder: must see all elements |
| `core-doall` | 2540 | intentional realization — that's its purpose |
| `core-dorun` | 2543 | intentional realization — that's its purpose |
| `core-rand-nth` | 2558 | O(1) index into realized array |
| `core-list*` | 2584 | splices final arg into preceding elements |
| `core-transient` | 2631 | builds mutable copy from collection entries |
| `core-hash-ordered-coll` | 2738 | hash computation: must see all elements |
| `core-hash-unordered-coll` | 2740 | hash computation: must see all elements |
| `core-chunk-cons` | 1841 | chunk helper — realizes chunk to concat |
| `core-cat` | 1849 | transducer — must eat entire input element |
| `core-mapcat` (transducer) | 1134 | transducer arity — internal to reducing fn |

#### Conditional boundary (forces for concrete, lazy handled separately)
These have a `(if (lazy-seq? coll) ...)` guard. The `realize-for-iteration` is only reached for concrete collections. Correct pattern.

| Function | Line(s) | What happens for lazy input |
|---|---|---|
| `core-filter` | 951 | lazy branch: `fstep` walks lazily via `ls-first`/`ls-rest` |
| `core-take-while` | 1037 | lazy branch: walks until pred fails |
| `core-distinct` | 1254 | lazy branch: `dstep` yields one unique at a time |
| `core-keep` | 2366 | lazy branch: `kstep` skips nils one element at a time |
| `core-keep-indexed` | 1351 | lazy branch: `kstep` with index tracking |
| `core-map-indexed` | 1366 | lazy branch: `mstep` pairs idx+val lazily |
| `core-take-nth` | 2314 | lazy branch: `tstep` skips N elements at a time |
| `core-interpose` | 2340 | lazy branch: `istep` alternates sep + element |
| `core-partition-all` | 1324 | lazy branch: `pstep` pulls N elements at a time |
| `core-partition` | 1285 | lazy branch: `pstep` with optional step parameter |
| `core-drop` | 1013 | lazy branch: walks past N elements lazily |
| `core-drop-while` | 1053 | lazy branch: `dwstep` skips past pred-matched elements |
| `core-map` (single) | 880 | lazy branch: `mstep` maps one element at a time |

#### Transformer leak (needs work — still forces)
These functions call `realize-for-iteration` unconditionally on their input, breaking laziness. Each has a target Step for resolution.

| Function | Line(s) | Severity | Target Step |
|---|---|---|---|
| `core-mapcat` (collection) | 1141 | HIGH | Step 4 — `apply` fix needed to avoid forcing `core-map` result. Currently `(apply concat ...)` forces via `realize-for-iteration`. Lazy overlay exists in `10-seq.clj` but reverted (compile-mode splice errors). |
| `core-cycle` | 1372 | MED | Must snapshot input to cycle — would need a lazy cycling buffer. Low priority (cycle of finite coll). |
| `core-partition-by` | 1299 | MED | Has no lazy branch yet. Needs Step 2c completion. |
| `core-xml-seq` (Janet) | 2464 | LOW | **Overridden** by Clojure overlay `xml-seq` in `20-coll.clj` (uses `tree-seq`). The Janet stub remains for direct Janet-level callers but is rarely hit. Counted in Internal helpers below. |

#### Interop helpers (context-dependent, keep)
Array/byte conversion helpers that naturally force input.

| Function | Line(s) | Why |
|---|---|---|
| `make-num-array` | 1769 | (T-array seq) — realizes seq to build native array |
| `core-bytes` | 1784 | byte conversion — forces to encode bytes |
| `core-into-array` | 1802 | realizes seq to build Java array |
| `core-to-array` | 1805 | realizes seq to mutable array |
| `core-to-array-2d` | 1807 | realizes 2-level seq to 2d array |

#### Internal helpers (keep, context-dependent)
| Function | Line(s) | Why |
|---|---|---|
| `core-map` multi-coll init | 894 | Pre-realizes concrete colls only; lazy colls go through step fn |
| `core-map` multi-coll step | 919 | On-demand lazy pull: realizes concrete coll only when cursor exhausted |
| `sorted-entries` | 2515 | Helper for `subseq`/`rsubseq`; forces sorted-coll items |
| `core-xml-seq` (Janet, walk) | 2464 | Interim Janet impl — overridden by Clojure overlay xml-seq in 20-coll.clj |

#### Summary

| Category | Count |
|---|---|
| Boundary (correct) | 31 |
| Conditional boundary (lazy branch exists) | 13 |
| Transformer leak (needs work) | 3 |
| Interop helper (keep) | 5 |
| Internal helper (keep) | 4 |
| **Total verified** | **56** |
| **Leaks remaining** | **3 (mapcat, cycle, partition-by)** |

Of the 3 leaks:
- `mapcat` is the **critical remaining leak** — blocked on Step 4 `apply` fix.
- `partition-by` and `cycle` are low-to-medium priority.
- `xml-seq` Janet is **overridden** by the Clojure overlay — effectively resolved; counted in Internal helpers.

### Step 4 — Evaluator / compiler eager assumptions
Grep the interpreter (`src/jolt/evaluator.janet`) and back end
(`src/jolt/backend.janet`, `compiler.janet`) for places that realize seqs:
- `apply` / variadic arg spreading — must finitely spread, not realize an infinite
  tail beyond the call.
- `&`-rest binding in `fn*`/`let*`/`loop*` and `destructure` — a rest param over a
  lazy seq should stay lazy, not eagerly slurp.
- `doseq`/`for` desugaring (they go through `count`/`mapcat` — verify the `for`
  comprehension stays lazy where Clojure's is).
- Any `(each x (realize ...))` in hot paths that assumes finiteness.

### Step 5 — Laziness-coupled stragglers (the deferred Phase-5 list)
From `jolt-c09` notes / MIGRATION.md: `sequence`, `sequential?`, `seqable?`,
`realized?`, `line-seq`, `rand-int`, `random-uuid`, `trampoline`, `unreduced`,
`ensure-reduced`, the transducer machinery (`cat`, `eduction`, `transduce`,
`sequence`, `halt-when`, `dedupe`/`interpose`/`keep` transducer arities). Move the
now-lazy ones to the overlay where feasible (Phase-4 style), keeping the
`Reduced`/thunk kernels native.

### Step 6 — Representation decision (DO THIS DELIBERATELY, EARLY)
Clojure: `(map inc [1 2 3])` returns a **lazy seq**, not a vector; `(seq? (map ...))`
is true, `(vector? (map ...))` is false. Jolt currently returns an eager vector
(`make-vec`) to "preserve representation". Two options:
- **(A) Full Clojure semantics:** `map`/`filter`/etc. always return a LazySeq, even
  over a vector. Most correct; **but** flips `vector?`/`seq?`/printing on a lot of
  existing results and may shift many conformance/suite assertions. Budget for the
  churn.
- **(B) Hybrid (status quo extended):** lazy over lazy/infinite input, eager
  representation-preserving over concrete finite input. Less churn, but
  `(seq? (map inc [1 2 3]))` stays wrong.
Recommend (A) for correctness, but measure the blast radius first: run conformance
+ suite with a throwaway always-lazy `map` and count newly-failing assertions
before committing to it. Whichever you pick, **write it down here and be
consistent** across all transformers.

---

## 3b. Implementation notes (discovered during Phase 5)

### mapcat + compile mode
A lazy step-based `mapcat` (using `cons` + `lazy-seq` + recursive `fn` in the
overlay) causes splice errors in self-hosted compilation. The `defrecord` macro
in `30-macros.clj` uses `(vec (mapcat …))` inside syntax-quote, and `~@` cannot
splice lazy-seqs. Reverted to the standard `(apply concat (apply map f colls))`
implementation. Two possible fixes for the future:
1. **Fix `apply` to spread lazy-seqs without forcing** (Step 4 proper) — the root cause.
2. **Rewrite `defrecord`'s bind-generation to avoid `mapcat`** — replace
   `(vec (mapcat (fn [f] …) fields))` with an eager `loop` accumulator.

### tree-seq + compile mode
Same root cause as mapcat: lazy `tree-seq` requires `mapcat` for
`(when (branch? node) (mapcat walk (children node)))`. Kept eager; lazy version
documented in `20-coll.clj` comments. Will switch when mapcat is resolved.

### pre-existing: protocol-on-record compile-mode failure
`(defprotocol P (m [_])) (defrecord R [side] P (m [_] (* side side))) (m (->R 4))`
errors with "Unable to resolve symbol: side" in compile mode. This is a pre-existing
issue unrelated to Phase 5 changes — `register-method` stores the method body as
a raw `fn*` form, and the self-hosted compiler cannot resolve let-bound field
access symbols at definition time (bindings only exist at call time).
Conformance wraps this in `(= expected (do …))` so it's never triggered; only
direct `eval-string` with `:compile? true` hits it. Not blocking — the
self-host path (JOLT_SELFHOST=1) and interpret path both pass.

---

## 4. Suggested commit cadence

One transformer family (a §3 sub-step) per commit. Each commit:
1. Convert the fns (overlay or core as appropriate).
2. Add infinite-seq spec cases (§6.2) + value cases.
3. Run the full gate (§6.1). Commit only if green. Push.

Mirror the Phase 4 discipline: small, gated, reversible batches.

---

## 5. Risks & gotchas

- **Uninterruptible hangs:** never probe an infinite case in-process — it pins a
  core and can't be killed by a deadline. Always go through the subprocess harness.
- **Self-reference:** `(def s (lazy-seq (cons 1 s)))` and `lazy-cat` fib rely on
  `realize-ls`'s `:jolt/pending` guard — don't bypass `realize-ls` with a
  hand-rolled force.
- **Stack overflow** from deep wrapper chains (`concat`/`mapcat`/`iterate` of
  `iterate`) — realize iteratively.
- **Double realization / side effects:** a lazy `map` fn with side effects must run
  **once per element, in order, only when forced** — assert with a counter (§7).
- **Performance:** LazySeq has per-element allocation + thunk-call overhead. Watch
  `core-bench` (`test/bench/core-bench.janet`) — the eager fast paths exist partly
  for speed. A heavy suite file slipping past the 6 s deadline = a regression
  (this already bit Phase 3's macro move).
- **Compile/self-host parity:** every behavior must hold in interpret, compile, and
  self-host (conformance runs all three). Lazy thunks are closures — verify the
  back end compiles them.
- **`chunked` seqs are out of scope** — `chunked-seq?` stays `false`. Don't emulate
  chunking; one-at-a-time is fine.

---

## 6. Testing strategy

### 6.1 Per-step gate (every commit) — same as Phase 4
```
janet test/integration/conformance-test.janet          # 229×3 (interpret/compile/self-host)
janet test/integration/bootstrap-fixpoint-test.janet   # stage1==2==3
janet test/integration/self-host-test.janet
janet test/integration/sci-bootstrap-test.janet
janet test/integration/clojure-test-suite-test.janet   # >= baseline (raise as it improves)
for f in test/spec/*.janet test/unit/*.janet; do janet "$f"; done
```

### 6.2 Deadlined infinite-seq spec (the Phase-5-specific harness)
Build this in Step 0. Plain in-process specs **cannot** test laziness — a wrong
answer hangs instead of failing. Mirror `clojure-test-suite-test.janet`'s pattern:
- A new `test/integration/lazy-infinite-test.janet` that, for each case, spawns a
  worker (`os/spawn ["janet" "test/support/lazy-eval.janet" expr]`) and waits under
  `(ev/with-deadline 5 (os/proc-wait proc))`, killing on timeout.
- A timed-out or crashed case = **FAIL** (it should have produced a value).
- Cases = the compositions that currently hang. Minimum set:
  ```
  (nth (map inc (range)) 1000)            => 1001
  (first (filter even? (drop 3 (range)))) => 4
  (take 3 (remove odd? (range)))          => (0 2 4)
  (take 3 (drop-while #(< % 5) (range)))  => (5 6 7)
  (take 4 (interleave (range) (iterate inc 10)))
  (take 3 (partition 2 (range)))          => ((0 1) (2 3) (4 5))
  (take 3 (partition-all 2 (range)))
  (take 3 (map-indexed vector (range)))
  (take 5 (distinct (cycle [1 2 1 3 1])))
  (take 3 (mapcat (fn [x] [x x]) (range)))
  (take 3 (take-nth 2 (range)))
  (take 3 (interpose :x (range)))
  (take 3 (map vector (range) (iterate inc 100)))
  (second (cons :a (range)))
  ```
  Add one row per transformer converted in Step 2.

### 6.3 Laziness assertions (side-effect counting)
For each lazy transformer, assert it realizes **only what's demanded** — values
alone don't prove laziness. Use a counter:
```clojure
(let [n (atom 0)]
  (take 3 (map (fn [x] (swap! n inc) x) (range)))
  @n)            ; => 3  (not "hang", not 1000)
```
Add these to `test/spec/lazy-seqs-spec.janet`. They run in-process safely because
they only ever force a bounded prefix.

### 6.4 Conformance extension
Add infinite-composition rows to `conformance-test.janet` (runs ×3 modes) — the
subset of §6.2 that returns a small concrete value, e.g.
`["lazy compose" "(quote (1 3 5))" "(take 3 (filter odd? (map inc (range))))"]`.
These guard interpret/compile/self-host parity.

### 6.5 Acceptance target — the timed-out suite files
The 9 files that currently time out (snapshot in Step 0:
`cycle`/`range`/transducers-over-infinite tests) should stop timing out and start
contributing passes. Each phase-5 step should monotonically reduce the timed-out
count and **raise `baseline-pass`** in `clojure-test-suite-test.janet:35`. Final
target: 0 (or near-0) timeouts and a meaningfully higher baseline.

### 6.6 Regression guards
- `core-bench` before/after (back-to-back, load-sensitive) — no large slowdown on
  the eager-collection paths.
- `lazy-seqs-spec`, `sequences-spec`, `transducers-spec` stay green every step.

---

## 7. Done criteria

- All §6.2 infinite-seq cases return correct values under the deadline (0 hangs). ✓ 18/18
  (mapcat infinite deferred — needs Step 4 apply fix)
- §6.3 laziness counters prove minimal realization for every converted transformer. ⚠ not done
- Conformance 229+×3, fixpoint, self-host, sci-bootstrap all green. ✓ (interpret+self-host 229×2;
  compile 228/229 — 1 pre-existing protocol-on-record error, see §3b)
- clojure-test-suite: the ~9 infinite-seq files no longer time out; `baseline-pass`
  raised to the new steady-state; no per-file 6 s timeouts introduced. ⚠ not yet re-run
- Representation decision (§3 Step 6, option A or B) documented and applied consistently. ⚠ deferred
- `core-bench` within noise of the Phase-4 baseline. ⚠ not run
- `bd close jolt-c09` → closes the `jolt-1j0` epic. ⚠ not done (waiting on remaining items)
