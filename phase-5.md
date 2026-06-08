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

### Step 0 — Safety net
- Record the baseline: conformance 229×3, clojure-test-suite `baseline-pass=3926`,
  fixpoint stage1==2==3, self-host, all specs+unit, `lazy-seqs-spec` /
  `sequences-spec` / `transducers-spec` green.
- Build the **infinite-seq harness** first (see §6.2, "Deadlined infinite-seq
  spec") so every subsequent step is verified against hangs, not just values.
- Snapshot which clojure-test-suite files currently time out (the ~9). Save the
  list — it's the acceptance target.

### Step 1 — Lazy combinator layer
Add a small set of internal lazy builders so transformers compose uniformly,
rather than each re-implementing the thunk dance:
- `lazy-cons val thunk` → a LazySeq cell of `val` + a deferred rest.
- `lazy-from coll` → coerce any seqable to a uniform lazy view *without forcing*
  (vector/list/set/map/string/LazySeq → a LazySeq that pulls element by element).
  This is the lazy analogue of `realize-for-iteration` and the key primitive: every
  transformer takes `(lazy-from input)` and walks it with `core-first`/`core-rest`.
- `seq-done?` already exists — confirm it short-circuits without forcing the tail.
- Decide placement: the lazy machinery is host-coupled (Janet thunks) so it stays
  in `phm.janet`/`core.janet`; transformers that are already in the overlay tiers
  call these as primitives.

### Step 2 — Convert the core transformers (leaf-first)
Make each return a LazySeq over `lazy-from input`. Do them in dependency order, one
small batch per commit, each gated:
- **2a. Single-input maps/filters:** `map` (1-coll), `filter`, `remove`, `keep`,
  `map-indexed`, `keep-indexed`, `take-while`, `drop-while`, `take-nth`.
- **2b. Structural:** `cons`, `rest`/`next` over lazy, `concat`, `lazy-cat`
  (verify), `mapcat`, `cycle` (verify), `interleave`, `interpose`.
- **2c. Windowing:** `partition`, `partition-all`, `partition-by`, `dedupe`,
  `distinct`, `take`/`drop` (return LazySeq, not eager array, when input is lazy).
- **2d. Multi-input `map`/`mapcat`** over several colls (shortest-stops).
- **2e. Tree/seq:** `tree-seq`, `flatten`, `xml-seq`, `line-seq`, `sequence`,
  `iterator-seq`, `enumeration-seq`.
- For each: a transducer arity may exist (`td-*`) — leave it; only the
  collection arity changes.

### Step 3 — Realization boundaries
Audit the 57 `realize-for-iteration` call sites. Classify each as **boundary**
(keep, it must force) or **transformer leak** (remove, made lazy in Step 2):
- Boundaries that stay: `count`, `reduce`, `into`, `vec`, `seq`, `doall`, `dorun`,
  `=`/equality, `pr`/`print`/`str-render`, `sort`/`sort-by`, `reverse`, `frequencies`,
  `group-by`, `apply` arg-spread, `doseq`.
- Make sure `first`/`second`/`nth`/`last`/`take`/`get` pull **only as far as
  needed** (they must not call `realize-for-iteration`).
- `realized?` must report a LazySeq's `:realized` flag (don't force to answer).

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
- All §6.2 infinite-seq cases return correct values under the deadline (0 hangs).
- §6.3 laziness counters prove minimal realization for every converted transformer.
- Conformance 229+×3, fixpoint, self-host, sci-bootstrap all green.
- clojure-test-suite: the ~9 infinite-seq files no longer time out; `baseline-pass`
  raised to the new steady-state; no per-file 6 s timeouts introduced.
- Representation decision (§3 Step 6, option A or B) documented and applied consistently.
- `core-bench` within noise of the Phase-4 baseline.
- `bd close jolt-c09` → closes the `jolt-1j0` epic.
