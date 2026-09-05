# jolt benchmark suite

Run it after any change to the compiler passes, the emitter, or the runtime's
hot paths. `make test` and `make libconformance` check that answers are right and
neither notices when they get slower: `arrays` went 229.7 -> 1272.6ms on a
codegen change with all 88 ci targets and all 47 libraries green, because every
answer was still correct. A release compares this suite against the previous
release and blocks `publish` on it (`ci/bench-gate.sh`, wired into
`.github/workflows/release.yml`).

One suite run is not evidence. Per-benchmark noise is around 1.07x on a quiet
machine and the first benchmark of a run can be much further out — the run that
caught the 5.4x also reported `mandelbrot` 1.63x faster and `nth-access` 1.07x
slower, and both were noise. Re-measure anything that moved with
`bench/run.sh <name>`, on each side, before believing it.

Benchmarks that isolate the workload axes jolt's optimizing passes target. The
ray tracer (`examples/ray-tracer`) is **float-compute-bound** — its time is
irreducible algorithmic math (hit-testing + transcendentals), and devirt,
allocation removal, and type-proving all measured **flat** on it. So it can't
tell us whether those passes work. These benchmarks make each pass's target
workload the *dominant* cost.

Reference: the cross-language suites these draw from —
[Are We Fast Yet?](https://github.com/smarr/are-we-fast-yet) (Marr et al., DLS '16)
and the [Computer Language Benchmarks Game](https://benchmarksgame-team.pages.debian.net/benchmarksgame/).
The benchmarks are portable Clojure, so they also run on JVM Clojure for an
absolute reference.

## Benchmarks

| Benchmark | Axis | Pass it exercises | Source |
|---|---|---|---|
| `binary-trees` | allocation / GC pressure (escaping short-lived records) | scalar-replace, escape analysis | CLBG |
| `dispatch` | polymorphic (**megamorphic**) protocol dispatch | devirt, inline-cache | AWFY-style |
| `mono-dispatch` | **monomorphic** protocol dispatch (devirt/inline-cache *can* fire) | devirt, inline-cache | AWFY-style |
| `collections` | persistent map/vector churn (HAMT / 32-way tries) + map/filter/take/reduce over the built vector | persistent structures, transients | CLBG k-nucleotide-style |
| `vecops` | vector-of-vectors: pairwise `into` concatenation, `subvec` windows + reduce, split-at/rejoin loop | vector concat + slice (the RRB axis; `into`/`subvec` are wired through the RRB ops, so concat is O(log n) here and linear in core Clojure) | RRB workload |
| `mandelbrot` | pure float compute (tight arith loops, no alloc/dispatch) | native arith, loop codegen | CLBG |
| `arrays` | primitive `double-array` throughput (unboxed `aget`/`aset`, no boxing/collections) | unboxed primitive-array codegen (flvector read/write) | CLBG-style |
| `mathfns` | transcendental math (`java.lang.Math` sqrt/sin/cos/log/pow/atan2 over doubles) | native `Math` op lowering (`flsqrt`/`flsin`/… vs generic host-static dispatch) | CLBG-style |
| `fib` | recursion: function-call + integer-arith overhead | native arith, small-fn inlining | CLBG |
| `tak` | deep three-way self-recursion + integer arith | direct-linked self-calls, proven fixnum arith | CLBG/AWFY |
| `loop-recur` | tight `loop`/`recur` + per-iteration integer arith (`mod`, `quot`, `bit-xor`) | numeric pass (primitive long loop counters), loop codegen | CLBG-style |
| `seqs` | lazy-seq + HOF pipelines (`map`/`filter`/`reduce`, `every?`, `iterate`/`take`, `mapcat`) | lazy-seq cell allocation, per-element call overhead | CLBG-style |
| `transducers` | transducer pipelines (`comp` of `map`/`filter`/`take`) | transducer machinery, `reduce` fast paths | CLBG-style |
| `transients` | bulk map/set building through the transient write path (`into`, `assoc!`/`conj!`, `dissoc!`/`disj!`, `zipmap`/`frequencies`/`group-by`), with a transient vector build as the control | editable-HAMT transient nodes, `persistent!` spine freeze | — |
| `keyed-lookup` | scalar KEYS: hashing/comparing keywords, symbols and strings, and looking them up in SMALL maps; a symbol built per lookup, and a collection or keyword-local in head position | hash engine fast paths (`jolt-hasheq`/`jolt=2`), `symbol-t` khash, `jolt-invokeN` lookup shapes | honeysql `format-dsl` |
| `hash-eq` | composite KEYS AND VALUES: repeat-hashing vectors/maps/sets/records/seqs, vector- record- and fn-keyed map and set lookups, and `=` on equal and unequal collections | per-instance hasheq caches, collection/record probes ahead of the eq and hash arm walks, hash fast-reject in `jolt-coll=?`, procedure identity hash | instaparse GLL msg-cache, honeysql |
| `literals` | fixed per-call overhead in a library's inner fn: constant map/vector/set literals in the body (incl. quoted symbols), and `true?`/`false?`/`boolean?`/`identical?` | per-site constant hoisting (`hoist-const-per-site`), identity-based boolean predicates, inlined `identical?` | honeysql `format` loop |
| `string-build` | `StringBuilder` appended to in a loop, and the transducer-over-`join` shape libraries render text with | proven-StringBuilder direct emission vs jhost method-table dispatch | honeysql `format-entity` |
| `string-ops` | the ordinary String surface — `.indexOf`/`.startsWith`/`.substring`/`.toLowerCase` on hinted and inference-proven targets, `clojure.string` over already-string arguments, `.getName`/`.getNamespace` on a keyword | direct emission for proven-string and proven-keyword interop targets, `clojure.string/to-str` string fast path | honeysql, clojure.string |
| `char-scan` | walking a string one code point at a time via `.charAt`, with the `int`/`long`/`unchecked-*` casts hinted Clojure puts around it, incl. a `case`-dispatched character state machine | numeric cast fast paths, `.charAt` on a proven string, `case` over small ints | honeysql `alphanumeric?` |
| `typed-records` | DECLARED field types on a record: `^double`/`^long`/`^String` at construction and at every read | inlined per-arity record constructor, declared-field-tag typing | — |
| `sorted-access` | reads a collection's structure can answer without walking: `count`/`drop` on a vector seq, `rseq`, `first` on a sorted map/set | shape-answered reads (Counted / IDrop / leftmost-node), not traversal | — |
| `nth-access` | `nth` on a vector, small and large, and with a default — the constant cost of an indexed read | `Indexed`-first ordering in `jolt-nth` ahead of the extension-type probes | — |
| `executors` | `java.util.concurrent` dispatch: fire-and-forget at a cached pool faster than it can drain, submit/get round trips, growth to 64 blocking tasks, and four producers on one pool | the RUNTIME rather than a pass — the executor's queue, its wake rule and its growth rule (`host/chez/java/concurrency.ss`) | — |

`executors` is the one row that is not about a compiler pass. It is here because
nothing else in the suite measures concurrency throughput, and the cost it watches
is real: an enqueue that woke every idle worker instead of one took a no-op task
from 8.9µs to 152µs and the pool from 7 threads to 134, with every correctness
gate still green. Concurrency numbers are noisier than the rest — the four-producer
phase is a fight over one mutex on however many cores the machine has — so read a
move here with more suspicion than usual, and re-run it alone on both sides.

## Scorecard

**vs JVM** is jolt ÷ JVM Clojure on the same source — **lower is better, and
under 1.0× means jolt is faster**. Two build modes: **opt** is
`jolt build --direct-link --opt`, **release** is a plain `jolt build` (what a
default build ships). Times are the mean of 3 runs after warmup, in ms. Every
row below is from ONE `MODE_A=1 bench/run.sh` on one machine in one sitting,
which is the only way the ratios mean anything.

Refreshed 2026-09-02 on an Apple Silicon host (MacBook Pro, M1 Pro, macOS 26.3,
OpenJDK 20.0.1, Chez 10.4.1), after the 2026-09 audit rounds landed. This is a
full same-sitting re-run of every row with one binary, not a patch of
individual rows, so it supersedes the previous (x86_64 Linux) table wholesale:
absolute ms are not comparable across the two machines, and several ratios
shift with them (`collections` most visibly — see the machine note kept below
from the previous refresh — and `nth-access`, where the JVM figure moves more
run to run than jolt's). The rows the audit targeted, measured on THIS machine
before and after in the same session: `char-scan` 24.2× → 6.7×, `literals`
7.5× → 2.1×, `string-ops` 6.9× → 3.7×, `arrays` 6.4× → 3.9×, `keyed-lookup`
5.7× → 3.5×, `string-build` 5.3× → 4.5×, `hash-eq` 3.0× → 2.6×; `seqs`,
`transducers`, `transients` and `sorted-access` each lost 10–15% of their
absolute time, and no row got slower. The six rows the flat array-map round
touched (`keyed-lookup`, `collections`, `transients`, `hash-eq`, `literals`,
`seqs`) were re-run the same day after it landed, both columns in one sitting,
and are the figures shown; that round's own A/B/A is in its section below.

The previous refresh (2026-08-24, x86_64 Linux, Intel i5-4278U, OpenJDK
21.0.11) was a weaker, older machine than this one; its `collections` row read
0.8× where this one reads 2.0×, a JVM warmup/JIT effect of that host rather
than a jolt-side change.

> **Four rows below are stale as of 2026-09-05 and are marked ⚠.** The type-hint
> codegen round changed the SOURCE of `mathfns`, `arrays`, `string-ops` and
> `char-scan` (each grew a phase for an axis it did not previously cover), so
> their absolute ms and their vs-JVM ratios no longer describe what the benchmark
> now runs, and `typed-records` is new and has no row at all. They are marked
> rather than patched on purpose: this table is one same-sitting run on one
> machine, and dropping four freshly-measured numbers from a different machine
> into it is precisely how the stale-scorecard incident below happened. The
> evidence for that round is the before/after A/B above, which is a ratio between
> two compilers on one machine and does not depend on this table. A full
> same-sitting refresh is owed and is the way these rows come back.

| Benchmark | vs JVM | vs JVM (release) | jolt (ms) | JVM (ms) | Axis |
|---|---:|---:|---:|---:|---|
| `vecops` | **0.3×** | 0.3× | 4.5 | 15.5 | vector concat + slice (beats the JVM) |
| `tak` | **0.4×** | 0.4× | 7.1 | 18.4 | deep three-way self-recursion + integer arith (beats the JVM) |
| `dispatch` | **1.2×** | 1.2× | 62.2 | 52.3 | megamorphic protocol dispatch |
| `fib` | **1.3×** | 1.3× | 9.3 | 6.9 | recursion: call + integer arith |
| `loop-recur` | **1.5×** | 1.5× | 27.9 | 18.6 | tight loop/recur + per-iteration integer arith |
| ⚠ `mathfns` | **1.5×** | 1.5× | 24.0 | 16.5 | transcendental math (`Math` sqrt/sin/cos/log/pow/atan2) |
| `mandelbrot` | **1.6×** | 1.5× | 22.6 | 14.5 | pure float compute |
| `collections` | **1.6×** | 1.6× | 17.0 | 10.7 | persistent map/vector churn |
| `binary-trees` | **2.0×** | 1.9× | 73.2 | 37.1 | escaping short-lived records (allocation/GC) |
| `literals` | **2.1×** | 2.1× | 52 | 25 | constant literals + boolean predicates, per call |
| `sorted-access` | **2.4×** | 2.4× | 31.3 | 13.0 | shape-answered collection reads |
| `seqs` | **2.4×** | 2.5× | 338.7 | 140.1 | lazy-seq + HOF pipelines |
| `hash-eq` | **2.5×** | 2.4× | 448 | 182 | composite-value hashing + collection `=` |
| `transients` | **2.6×** | 2.6× | 147 | 56 | transient map/set bulk build |
| `mono-dispatch` | **2.7×** | 2.5× | 36.1 | 13.6 | monomorphic protocol dispatch |
| `nth-access` | **2.9×** | 2.9× | 62.9 | 21.5 | `nth` on a vector, small and large |
| `executors` | **3.4×** | 3.4× | 1427.9 | 417.5 | `java.util.concurrent` dispatch: enqueue, submit/get, growth, contention |
| `keyed-lookup` | **3.5×** | 3.5× | 91 | 26 | scalar-key hashing + small-map lookup |
| ⚠ `string-ops` | **3.7×** | 3.7× | 250 | 68 | String/Keyword interop + `clojure.string` |
| ⚠ `arrays` | **3.9×** | 3.9× | 141.1 | 35.9 | primitive `double-array` throughput |
| `transducers` | **4.2×** | 4.3× | 128.2 | 30.4 | transducer pipelines |
| `string-build` | **4.5×** | 4.6× | 170 | 38 | `StringBuilder` assembly + `join` |
| ⚠ `char-scan` | **6.7×** | 6.6× | 94 | 14 | per-character `.charAt` + numeric casts |

`opt` and `release` track each other closely across the whole suite — the plain
`jolt build` picks up essentially all of the win. Every row is within 0.2 of a
ratio point; the widest gaps are `mono-dispatch` (2.7× vs 2.5×) and `hash-eq`
(467 vs 457 ms), both of which read level on repeat runs.

### The type-hint codegen round (2026-09-05)

Widening the bridges from jolt's Java-type hints into codegen. Measured with
`ci/bench-gate.sh <main-jolt> <branch-jolt>`, which builds THIS checkout's bench
sources with both compilers and alternates timed runs, so the ratio is codegen and
nothing else. Same machine, one sitting, min of 3 runs per side; every row below
was reproduced in a second independent round.

| Benchmark | before (ms) | after (ms) | ratio | what moved |
|---|---:|---:|---:|---|
| `mathfns` | 1165.0 | 179.4 | **0.15×** | `Math/abs`/`min`/`max`/`floorDiv`/`floorMod` on proven longs stopped taking `host-static-call`'s per-invocation method lookup |
| `arrays` | 2631.2 | 949.8 | **0.36×** | `aget`/`aset` on `^objects`/`^longs` stopped taking the generic `nth` dispatch walk |
| `string-ops` | 720.0 | 581.0 | **0.81×** | interop results carry a type, so chains and let-bound results prove; 20 more methods emit directly; `count`/`str` over proven strings lower to primitives |
| `typed-records` | 56.4 | 50.4 | **0.89×** | the inlined record constructor stopped refusing `^double` fields |

Two isolating micro-benchmarks, written to separate the axes inside those rows and
then deleted (they are reproduced here rather than kept, because each is a single
shape and the suite's rows are meant to be workloads): an `^objects` `aget` loop
alone went **0.32×**, and an escaping `^double`-field record constructor alone went
**0.60×**.

**What did NOT move, measured and reported rather than assumed.** `char-scan` came
back 0.97× — inside noise. Isolated micro-benchmarks for the `^int` parameter hint
and for the record field tags (`^long`/`^String`) both came back 1.00–1.03×. The
codegen change is real and visible in the emitted Scheme — a hinted record's field
reads dropped two `record-method-dispatch` sites and turned a `jolt-count` into a
`jolt-str-count` — but in every shape benchmarked, something else dominated the
time: a `reduce` closure, an allocating `.toUpperCase`, or a literal-argument call
that the inliner had already specialized. They are correctness-and-codegen wins
with no measured speed claim attached.

**Two measurement traps this round walked into**, both worth knowing about before
writing a benchmark here:

*A record that never escapes is deleted.* The first draft of `typed-records`
allocated a `Vec3` per iteration and ran at 3ns an iteration — scalar-replace had
removed the record entirely, so the constructor benchmark was timing an empty
loop. `make-vecs` now stores into an `object-array` to force the escape.

*A hint on a callee is erased by inlining.* The first draft of the boxed-array work
narrowed the inline pass's array-hint refusal, on the reasoning that the boxed
kinds have no unboxed path to lose. They lose `jolt-vaget`: the emitted Scheme
showed the standalone `osum` with `jolt-vaget` and all three spliced copies — every
call on the hot path — with `jolt-nth`. The A/B read 1.02×, i.e. the entire win
cancelled. Restoring the refusal took the same benchmark to 0.32×. A ratio near
1.00 on a change you expected to matter is worth reading the emitted code over.

### A stale scorecard hid a 1.7× regression for three weeks (now fixed)

The rows above are a full re-measure. The previous table's `loop-recur` (30.7),
`mandelbrot` (23.3) and `fib` (7.0) figures dated from the 2026-07-26 refresh and
were carried forward unchanged through three later README edits that added rows
and prose without re-running the suite. They were wrong by then: bisecting the
published release binaries puts `loop-recur` at 30.3ms on v0.5.12 (2026-07-29)
and **51.7ms on v0.5.13** (2026-08-01), flat at ~52ms in every release from
there through v0.7.17. The JVM column was unchanged across the same span and
Chez has been 10.4.1 since June, so it was neither the machine nor the
substrate.

The cause was in v0.5.13's bit-op change. `bit-and`/`bit-or`/`bit-xor`/`bit-not`
moved off the raw Chez primitives — which Chez inlines to native code — onto
`jolt-bit-*` helpers, so that a bad operand raises a catchable
`IllegalArgumentException` in call position as well as value position. Correct,
but the helpers coerce through `->int`, which range-checks against ±2^63. Those
bounds are **bignums** on Chez's 61-bit fixnum tower, so every `(bit-xor a b)`
on ordinary integers paid four fixnum-vs-bignum compares — the identical
pathology the 2026-08-17 numeric-cast round fixed for `int`, `long` and the
`unchecked-*` forms, which `->int` was not given.

Two fixes took it back. **First, `->int` tests `fixnum?` first.** That is not a
semantic narrowing: a Chez fixnum here is 61-bit, so it is always an exact
integer inside signed 64-bit range and always took the slow path's accept
branch. That alone was `loop-recur` 51.8 → 34.4ms.

**Second, the back end open-codes the fixnum case.** Even with a cheap `->int`,
`jolt-bit-xor` is a cross-unit call, and cp0 does not inline it — measured, it
declines even from a direct same-unit call site. A call-position `bit-and`/`or`/
`xor`/`not` now emits its operands into a `let*`, tests them, and takes
`fxand`/`fxior`/`fxxor`/`fxnot` when both are fixnums, with the helper as the
slow arm. `loop-recur` 34.4 → **30.8ms**, which is v0.5.12's 30.3.

Isolated in Chez, 1.28M `bit-xor`s: 26ms through the old `->int`, 8ms through
the new one, 4ms as a raw inlined `bitwise-xor`, and 4ms through the open-coded
form — the inline recovers the call overhead completely.

The catchable-exception semantics the helpers exist for are untouched, because
the fast arm only short-circuits operands that provably cannot raise. `->int`
accepts exactly the exact integers in signed 64-bit range; a fixnum is always
one of those; and on fixnums the `fx*` ops agree with the generic `bitwise-*`
ops and always yield a fixnum, since the result's bit pattern is bounded by the
operands'. Everything that CAN raise — a non-integer, a ratio, an out-of-range
bignum — still reaches the helper. An above-fixnum but in-range value like 2^62
also reaches it, and still computes rather than raising. Ten corpus rows pin
that contract against JVM Clojure, covering call position, value position, the
variadic arity, the boundaries, and `Long/MIN_VALUE`/`Long/MAX_VALUE`.

A proven `^long` operand does NOT license dropping the test: jolt's `^long` is
64-bit and a Chez fixnum is 61, so a proven-`:long` value can be a bignum at
runtime. That is why this is a runtime guard in the emitter and not a `:lng`
registry entry. The shifts get no fast path either — `bit-shift-left` overflows
fixnum range and `jolt-bit-shift-left` wraps to 64 bits, so no `fx` op agrees
with them.

**The float side was the same bug, and took the same fix.** A `^double`
param/return coercion emits `jolt->fl` where it used to emit a bare
`exact->inexact`, so every parameter entry, return and contagion site became a
procedure call. `jolt->fl` returns its argument unchanged when it is already a
flonum — it tests `flonum?` first — so the emitter now hoists that test to the
call site (`(let ((t X)) (if (flonum? t) t (jolt->fl t)))`) and only a value
that genuinely needs converting, or that must raise, reaches the helper.
`jolt->fx` gets the same treatment on the `^long` side. `mandelbrot` 31.7 →
**23.7ms**, against v0.5.12's 23.0.

Both halves of the v0.5.13 window are closed now, and the shape of the fix was
identical each time: a Chez primitive that inlines was replaced by a helper call
for its JVM semantics, and the semantics only matter on the operands the fast
path does not take. Open-code the no-op case, keep the helper as the slow arm.

The moral is procedural, and is why this section exists: **the numeric rows were
carried forward instead of re-measured, and that is what made a 1.7× regression
invisible for three weeks.** A README edit that touches the scorecard should
re-run the suite or say plainly which rows it did not.

### Where the rest of the suite stands

The arithmetic/loop half sits at ~1.2–1.9× the JVM. The 2026-07 numeric-pass
round did most of that: mixed long×double contagion (a `:long` operand beside a
proven double widens via `fixnum->flonum`, so `Math` calls over it lower to
native flonum ops instead of generic host dispatch — `mathfns` ~22.7×→~1.6×) and
JVM literal-init loop semantics (a `(loop [i 0] …)` counter is a primitive long,
so its `inc`/compare/`mod`/`quot` run as fixnum ops — `loop-recur` ~8.3×→~1.7×,
and `mandelbrot`'s grid counters took it ~2.0×→~1.6×). `tak` and `vecops` beat
the JVM outright; `fib` is close to level with it.

`vecops` is the row the RRB work moved. Its comment used to say every operation
in it was linear per op on jolt; wiring `into` and `subvec` through the RRB ops
made concat O(log n) while core Clojure's `into` stays linear, so jolt now runs
it at 0.3× — the one place in the suite where jolt has a better ALGORITHM than
the reference rather than a better or worse constant.

`sorted-access` exists because every operation in it used to be a full traversal
here while the reference answers it from the collection's shape — and none of
that is visible to a value test, since the answers were correct all along. Over
200k elements `(count (seq v))` was 18.8ms against 166ns, `(rseq v)` 19.8ms
against 209ns, and `(first sorted-map)` 190ms against 416ns. Its collections are
built OUTSIDE the timed region: constructing a sorted map is a separate and much
larger gap (~126×, bead jolt-r8tz.7) that otherwise drowns out the reads this
measures. `test/complexity_test.clj` gates the same properties pass/fail.

`keyed-lookup` and `string-build` came out of profiling honeysql's test suite,
where `honey.sql/format` was 11× the reference and neither the existing
`collections` nor `transducers` benchmark could see why. `collections` churns large
HAMTs, so a per-key hash is amortised across 32-way nodes; at 3-8 entries the hash
IS the cost, and a symbol key built for one lookup and discarded pays a full
Murmur3 every time. `transducers` measures its machinery with an arithmetic
reducing function; `string-build`'s rf calls a host method instead, which was the
most expensive interop shape jolt had. Both are sensitive to the fixes that
followed: against the binary from before them, `keyed-lookup` ran 444ms (1.9×
slower) and `string-build` 1322ms (6.4× slower).

`char-scan` came out of the same profiling and is still the worst ratio in the
suite, at 6.7× down from 24.9×. It exists because the cost was in the *casts*,
not the loop or the string: `int`, `long` and the `unchecked-*` forms all fell
through a generic `cond` to a `truncate` call and generic bitwise masking, and
`long` additionally compared against ±2^63, which are BIGNUMS on Chez's 61-bit
fixnum tower, so every `(long x)` on an ordinary integer paid two fixnum-vs-
bignum compares. Giving each cast a fixnum fast path took it 466 → 343 ms on the
x86 host; what the 2026-09 audit then found was that the `unchecked-*` family
was never lowered at all — `unchecked-int`, `unchecked-long` and `unchecked-inc`
were var calls, so a loop written in exactly the hinted idiom this benchmark
copies from honeysql paid five generic invokes per character and kept its
counters untyped. They are native ops and `:long`-typed casts now, and the
proven-string `.charAt` index skips `jolt->idx`'s generic path: 363 → 94 ms on
this machine. What is left is the `case` chain (`identical?` per clause where
the JVM hash-jumps) and the per-character call sequence against a JIT that
inlines all of it.

### The hash, equality and per-call-overhead axes

Four benchmarks cover the 2026-08 rounds that came out of profiling honeysql and
instaparse. None of the older benchmarks could see any of them: the work they
measure is per-CALL and per-KEY, and every other benchmark amortises it across a
data structure or a loop trip count.

- **`hash-eq` 2.6×** is the composite-value half of the hash engine, where
  `keyed-lookup` is the scalar half. Vectors, maps and sets fell through to a
  linear walk of the equality and hash arm REGISTRIES before reaching their base
  cases, so loading an unrelated library made `(hash {:a 1 :b 2})` 8.4× slower;
  they are answered ahead of that walk now, and `reject-fast-type-claim!`
  refuses an arm that would claim one. On top of that every collection kind got
  a hasheq cache it was missing: a pvec had the field since `chez-pvec-v3` but
  nothing ever wrote it (repeat hash of a 1k vector 131µs → 191ns), a defrecord
  caches in an instance slot (4-field record 215 → 34ns, record-keyed map get
  216 → 58ns), a seq caches on its head. `jolt-coll=?` then rejects on differing
  cached hashes without a structural walk (27× on unequal 1k vectors). The fn
  row is a bug in its own right: Chez's `equal-hash` returns ONE constant for
  every procedure, so an fn-keyed map degenerated to a single bucket and
  instaparse's `[listener index]` cache went quadratic — procedures get an
  identity hasheq from a weak side table now, which took test.chuck's grammar
  require 3.57s → 1.66s.
- **`literals` 2.1×** (was 10.5× on the x86 host, 7.5× here before the audit rounds) is the purest row in the suite: it does no work
  at all beyond constructing the literals in a function body and calling three
  predicates. A literal collection is a compile-time constant the reference
  emits into the class constant pool; rebuilding one per call made `pset-conj`
  11% of samples in the honeysql profile, all of it set literals. Hoisting has
  to be PER SITE (two textually identical literals are distinct objects in the
  reference, and `=` short-circuits on identity), and quoted forms have to hoist
  too or `#{:for 'for}` stays dynamic. The predicate half is separate: `true?`
  and `false?` were `(= true x)`, and a mixed-type `=` misses every fast clause
  and walks the equality arm registry, so `boolean?` on a keyword cost 801ns
  against 194 now.
- **`transients` 3.5×** is the bulk-build write path. A transient map or set was
  a Chez hashtable, so `persistent!` folded every entry back through the
  ordinary insert and rebuilt the trie from scratch — the transient did not
  avoid the path-copying build, it deferred it and added a hashtable on top.
  Writes now claim each node on their path into an editable copy. The transient
  VECTOR build in the same benchmark is the control: it was always a tail-array
  append, so if the map/set rows move and it does not, the change is in the trie
  edit path.
- **`string-ops` 3.7×** (was 7.2×) is the ordinary String surface, which `string-build`
  (StringBuilder) and `char-scan` (`.charAt` plus casts) both miss. An interop
  call on an unproven target finds its method table by hashing the target's tag
  string, finds the handler by hashing the method name, and passes arguments as
  a vector converted back to a list for apply. A target proven a string — by a
  `^String` hint or by inference — emits the operation directly instead:
  `(.indexOf ^String s 46)` 140 → 23ns, `(.startsWith ^String s "so")` 142 → 17,
  `(.substring ^String s 1 4)` 236 → 23. The benchmark carries both proof seams
  and the `clojure.string` layer over them, where `to-str` used to take
  `.toString` unconditionally so a plain string paid the full dispatch chain.

### Five general defects behind the worst rows (2026-09)

An audit that measured the worst rows per OPERATION — each shape timed once and
four times per iteration inside one `--opt` binary, so the loop floor cancels,
then decomposed at the Scheme level inside the real runtime — found that none
of `char-scan`, `literals`, `string-ops`, `keyed-lookup` or `arrays` was about
the pass its row is named for. Five mechanisms, each general, explained them:

- **A compare of two base scalars of different kinds walked the eq arm
  registry.** `jolt=2` answered same-kind pairs fast but every other pair —
  keyword vs symbol, keyword vs string, long vs double, boolean vs keyword, and
  anything vs `nil` — ran every registered arm predicate before `jolt=2-base`
  saw the nil clause: 145–240 ns per miss with the 17 arms a bare runtime
  registers, more per library loaded. `case` lowers to a chain of exactly those
  compares, so a keyword `case` fed a symbol paid ~200 ns per clause where the
  JVM hash-jumps. Answered ahead of the walk now (the JVM's `Util.equiv` has no
  extension point for such a pair either), the cross-kind pairs joined the fast
  probes so the registry refuses an arm that would claim one, and `case`
  compares keyword/nil/boolean constants with `identical?`. `(= :a 'a)` 217 → ~6
  ns.
- **The `unchecked-*` family was not lowered.** `unchecked-int`, `unchecked-
  long` and `unchecked-inc` were plain var calls; `unchecked-add` only reached
  `jolt-uncadd2` when both operands were already proven long; the helpers were
  variadic rest-list folds; and `jolt-wrap64` compared a fixnum against BIGNUM
  bounds. `unchecked-add` 29 ns against 3.6 for `+`, `unchecked-int` 13 against
  5 for `int`, and every loop counter written in the hinted idiom stayed
  untyped. They are native ops now, `unchecked-long`/`unchecked-int` are casts
  that type their result `:long` (the `jolt-l*` guards already admit a wrap
  result past the 61-bit fixnum), and the helpers test `fixnum?` first
  (`unchecked-add` 3 ns). This is `char-scan`'s whole 24× and most of
  `literals`' — whose literals hoist correctly; the cost was the eleven
  `unchecked-add`s and `true?`/`boolean?` calls per iteration.
- **An interop call on an unproven receiver cost 60–135 ns.** `(.length s)` with
  no hint walked the method-dispatch arms, converted its `jolt-vector` rest args
  back to a list through the seq machinery, and matched the method name down a
  `string=?` chain — where the proven-string direct form is 3–11 ns. A method
  with a string or keyword direct form now tests the receiver at the site and
  takes it, with `record-method-dispatch` as the slow arm (receiver and args
  bound once, in order), and the generic path reads the rest args off the
  vector's tail.
- **Calling a clojure.core fn from a built binary cost ~12 ns of dispatch.** An
  app fn is a direct Scheme call under direct-link, but every seed fn call was
  `(jolt-invokeN (var-cell-deref cell) …)`: 7 ns for the dynamic-binding check
  and root read, 5 for the `procedure?` and arity-mask tests, on top of the
  callee — `true?` 16 ns, `name` 22. A `jolt build` now direct-links seed vars
  under the same closed-world rule an app def gets (a procedure root whose
  arity admits the call; not `^:dynamic`, `^:redef` or redefined by the app):
  the def binds the root once at load and calls it. `jolt run` never
  direct-links, so `with-redefs` in tests is unaffected.
- **Collection dispatch overhead.** `nth` on a 4-element vector was 19 ns
  (`pv-leaf-for`'s two-value return through `let-values` was 9 of it; every
  vector of 32 or fewer elements is all tail, read directly now), `first` on a
  vector 45–62 ns where the cell allocation was 4 (`jolt-seq` was `set!`-wrapped
  by nio-file.ss and `jolt-first` by host-table.ss; both are registry arms now,
  and `first` answers a cell or a vector from its backing store with no cell),
  and `str` over two strings 56 ns where `string-append` is 1 (fixed 2/3-arg
  entries, strings/keywords/fixnums rendered without the printer). `(:k m)` on a
  5-entry map was still 38 ns and `assoc` 201 against ~5 and ~20 on the JVM —
  the HAMT standing in for a flat array map, fixed next.

The measured table is in the memory note `perf-audit-2026-09-marginal-costs`;
the scorecard below is the same-sitting re-run after the rounds landed.

### A small map is one flat slot vector (2026-09)

An array-mode map — a literal of up to 8 entries, up to 64 keyword-keyed, and
whatever `assoc` keeps below those limits — was a hash trie with an
insertion-order list beside it: a record, a node, a leaf pair per entry and an
order pair per entry, every lookup hashing the key and descending the trie. It
is now what `PersistentArrayMap` is, a record over `#(k0 v0 k1 v1 …)`: lookup
scans the slots with a compare chosen once from the probe key's kind (a keyword
by identity, a fixnum, string or symbol by its own equality, the rest by `=`);
`assoc` and `dissoc` copy the slots; the transient is a slot buffer with the
reference's swap-on-remove; `seq`, `keys` and `vals` are vector-backed cells
(O(1) `count`, `reduce` in the chunk loop) instead of a cons chain built before
the first element is read; `reduce-kv` is native and folds in place; `into {}`
from a map and `zipmap` over vectors never build a seq.

Two general things sit under it. The runtime half of a built binary compiles at
Chez optimize-level 2 (level 3 is unsafe mode, deliberately not used), where a
Scheme loop pays a type and bounds check per element: a checked scan of a
64-keyword map cost 92 ns against 33 unchecked, and a 64-slot copy loop 390 ns
against 62. The hot slot loops now run on an adapter tier of unchecked
primitives (`sa-ufx+`, `sa-uvector-ref` … in `host/scheme-adapter/CONTRACT.txt`;
Chez's `#3%` forms, the checked ones on Gambit) where the bound is proven by
construction, and every vector copy under a trie node update, a tail append or
a transient's growth is a bulk move — which is why `collections` and
`transients`, all hash-mode maps, moved too.

Per operation, same binary shape as the audit table (before → after, ns):
`(:k m)` on 3 entries 34 → 12, on 8 entries 25 → 15, a miss 17 → 16, `(get m
20)` 43 → 16, `contains?` 31 → 11, `find` 40 → 23, `assoc` replacing a value
116 → 64 and of the value already held 117 → 35 (the map itself, as on the
JVM), `dissoc` 111 → 59, `{:a x :b y}` built at run time 100 → 17, `(= m m')`
on 5 entries 230 → 84, `(into {} m)` 1045 → 205, `zipmap` of three 400 → 201,
`(count (keys m))` on 8 entries 209 → 99, `reduce-kv` on 8 entries 583 → 50 and
on a 64-keyword map 4259 → 324, `{:keys [a b]}` destructuring 96 → 48. What got
slower, and stays slower on purpose: the far end of a large keyword array map,
where the scan is linear exactly as `PersistentArrayMap.indexOf` is — the 64th
key of a 64-keyword map 38 → 53 (the 30th 38 → 29, the average over the map
better than the trie) — and a symbol probed against SYMBOL keys and missing,
20 → 38, each slot paying a checked record predicate and accessor at
optimize-level 2 (a symbol probed against keyword keys, honeysql's shape, is
faster than before). Hash-mode maps' lookups are unchanged.

Rows, A/B/A in one sitting against a worktree of the commit before (a jolt
binary assembles a built app's runtime from the CHECKOUT's sources, so a
"before" binary run from this tree measures nothing — the worktree is what
makes the A side real): `keyed-lookup` 97 / 91 / 98 ms, `collections` 19.3 /
17.0 / 19.3, `transients` 205 / 147 / 203, `hash-eq` 461 / 443 / 458, `literals`
53 / 54 / 53, `seqs` 358 / 340 / 357. Nothing slower; the scorecard rows below
carry the refreshed figures.

### The allocation-bound axes

- **`arrays` 3.9×** (was ~18.6×, then ~6.4×): three rounds took it there. The fixnum-first
  index path in `jolt-flaget`/`jolt-flaset` removed the per-access index
  coercion (~18.6×→~9.5×), then emit-side inlining removed the procedure
  boundary itself — on a site where the pass has proven a `^doubles` array and
  a `:long` index, the back end now emits `(flvector-ref (jolt-array-vec a) i)`
  directly, so the flonum stays unboxed through the surrounding `fl+` chain
  instead of being boxed at the wrapper's return (~9.5×→~6.4×). An earlier
  version of this paragraph blamed the residual on Chez boxing the loop-carried
  flonum accumulator; that was wrong. A Chez probe of the exact emitted loop
  (`(let loop ((i 0) (acc 0.0)) … (#3%fl+ acc (#3%fl* …)))`) allocates 0.09
  bytes per iteration — the accumulator is unboxed. What the probe DID show is
  the checked `jolt-array-vec` record accessor being re-read twice per
  iteration: hoisting it took the loop 225 → 145 ms in isolation, and the back
  end now binds a `^doubles` parameter's backing flvector once at the arity's
  entry (`_av$N`) and indexes that (a non-array argument raises the JVM's
  checkcast `ClassCastException` on entry). What remains is memory-bound
  `flvector-ref` against a JVM dot loop the JIT SIMD-vectorizes.
- **`seqs` ~2.6×** (was ~6.3×): the allocation axis idiomatic Clojure hits most
  — range/map/filter/reduce chains, short-circuiting `every?`, `iterate`/`take`,
  and `mapcat` all build lazy-seq cells and call a closure per element. Two
  rounds took it here, both the same bug in different clothing: a lazy seq
  assembled out of COMPOSED lazy primitives pays for every layer. `iterate`
  spelled as `(cons x (lazy-seq …))` allocated a lazyseq node + closure and then
  a cseq cell + closure to keep the tail unforced — two of each per element
  where one suffices (374ms → 83ms over 800k elements). `lazy-concat-seq`, which
  `mapcat` and `(apply concat …)` both route through, built each inner
  collection via variadic `jolt-concat`: ~3 lazy nodes and ~5 closures per
  boundary, which swamps the per-element work when inner colls are small
  (302ms → 122ms). Both now emit exactly one cell per element.
- **`transducers` ~4.2×** (was ~7.0×): `eduction` was a plain lazy seq, so
  reducing one allocated a cell per element instead of driving the transducer
  into the accumulator. It is now a real `Eduction` implementing `IReduceInit`,
  as on the JVM — 152ms→61ms, in line with `transduce`. The remainder is the
  per-element reducing-fn call chain, not the pipeline shape.
- **`binary-trees` ~2.0×** (was ~7.2×): two rounds. Each node walk read a field
  through a keyword RE-INTERNED at every use site; keyword literals are now
  hoisted to a per-def constant (277ms→171ms). Then the read itself stopped
  being a generic `jolt-get`: typing the walker's parameter needs a record
  tracked through a NILABLE RECURSIVE position, which took four fixes to the
  whole-program pass — a `defn`'s self-recursive call now carries the fn's own
  return type (it resolves through the fn's name as a `:local`, so it used to
  read `:any` and a recursive constructor poisoned its own field types), the
  param fixpoint primes without back edges before iterating with them, joining
  two views of the same record keeps a one-sided field instead of widening it,
  and a field read off a record-or-nil keeps the field type joined with `:nil`.
  `(:left node)` now emits `jrec-field-at` at a static slot (165ms→67ms). What
  is left is allocation and GC — the nodes escape into the tree, so
  scalar-replace can't remove them.
- **`mono-dispatch` ~2.7×**:
  collapsed from two orders of magnitude by the type-proving / inline-field /
  bare-read work (`binary-trees` ~140×→~1.8×, `mono-dispatch` ~330×→~2.5×). On a
  statically proven monomorphic receiver, devirt resolves the impl and a
  per-site inline cache holds it. A NILABLE receiver deliberately does not
  devirtualize: the site caches its first resolution, so serving that impl to a
  later nil receiver would return a wrong value where Clojure raises
  `IllegalArgumentException` — a `some?`/`nil?` guard narrows it back and devirt
  fires again.
- **`dispatch` ~1.2×**: a megamorphic site runs a per-site polymorphic inline
  cache (4-slot descriptor scan, `#3%` reads over the proven cache shape), so
  it no longer pays a registry lookup per call.
- **`nth-access` ~2.9×** (jolt 63 ms on this machine before and after the audit; the ratio moves with the JVM column): `jolt-nth` is `set!`-wrapped several times and the
  array shim was outermost, so a plain vector read walked a chain of
  extension-type probes before reaching the arm that answers it. `RT.nth` tests
  `Indexed` first; so does this now (small vector 34.3 → 16.0ns). The residual
  is a constant factor, which is why it lives here and not in
  `test/complexity_test.clj` — two CI runners measured 2.79× and 5.14× for the
  same commit, and the regression worth watching for lands inside that spread.
- **`collections` ~2.0×** (0.8× on the x86 host): JVM-exact Murmur3
  hashing plus the array-map `(k . v)` fold. It read ~1.8× on the previous
  (Apple Silicon) host; the sign flip on this machine is a JVM warmup/JIT
  effect, not a jolt-side change — see the machine note above the scorecard.


## 64-bit integer arithmetic & generators (test.check)

The AOT suite above is float-compute / dispatch / allocation bound; none of it
exercises **64-bit integer arithmetic**, which Chez can't hold in a fixnum
(61-bit), so genuine 64-bit values are heap bignums. The SplitMix PRNG behind
`clojure.test.check` is the worst case — every `rand-long` is ~8 bignum ops.

`bench/testcheck.sh` runs these in **run mode** (`jolt run`, the normal require
path a test suite reaches library code through), against JVM Clojure on the same
source, with the same warmup-and-mean convention as the suite above. The first
two rows isolate one cost each; the rest are real test.check entry points and
carry both plus the rose-tree machinery.

| Workload | ×N | vs JVM | jolt (ms) | JVM (ms) | Bound by |
|---|---:|---:|---:|---:|---|
| SplitMix `mix-64` | 100k | **9.3×** | 77.1 | 8.3 | 64-bit integer arithmetic |
| deftype alloc + protocol dispatch | 100k | **3.7×** | 49.8 | 13.5 | open-world dispatch |
| raw `split` + `rand-long` | 20k | **12.3×** | 146.9 | 11.9 | bignum 64-bit + dispatch |
| `gen/large-integer` | 2k | **4.2×** | 163.2 | 38.8 | arithmetic + rose-tree machinery |
| `(gen/vector gen/large-integer)` | 500 | **17.5×** | 1762.4 | 100.8 | element gen + gen machinery |

Both columns are a fresh `bench/testcheck.sh` pair on one machine in one sitting,
which is the only way the ratio means anything. This pair was measured 2026-08-24
on the same x86_64 Linux host as the scorecard above (Intel i5-4278U, 4 threads),
not the Apple Silicon host the previous pair used, so neither column is
comparable to the previously published ones — every absolute roughly doubled.
The ratio shift is not uniform, though: the allocation/dispatch-heavy rows
(`split`+`rand-long` 21.3×→12.3×, `gen/large-integer` 7.4×→4.2×, the `gen/vector`
row 21.4×→17.5×) narrowed, while the purely-arithmetic `mix-64` row widened
slightly (8.2×→9.3×). Consistent with the JVM's JIT and GC paying more on an
older, weaker CPU than Chez's ahead-of-time compile does — but that is a
plausible reading of one pair of runs, not a re-derivation; treat the ratio
shift as machine-driven, and compare ratios across revisions of this table
measured on the SAME host, not across a host change like this one.

Two no-C codegen levers collapsed the **arithmetic** half: emitting `bit-and`/
`bit-or`/`bit-xor`/`bit-not` as inlined Chez `bitwise-*` primitives (they had gone
through a var-deref'd variadic overlay), and caching the resolved var cell per
reference site (a name lookup was ~45ns/access).

These rows were re-measured with `bench/testcheck.sh` and are **not comparable to
the numbers published before it existed** — that harness wasn't kept, and this one
warms up before timing, which warms the JVM's JIT far more than a single-shot
measurement did. jolt's own times improved (`mix-64` 45→35ms,
`(gen/vector gen/large-integer)` 1289→695ms); the JVM's improved more, so the
ratios read higher than they used to.

The residual gap is **machinery, not arithmetic**: the open-world generator
deftype/protocol dispatch + rose-tree allocation can't be devirtualized without
static types, and the raw 64-bit ops bottom out at the Chez bignum floor
(~20× a native long, substrate-inherent). A native SplitMix C/FFI shim would give
the PRNG ~27× but is the only path that needs C.

## Running

```sh
bench/run.sh                 # full suite + JVM scorecard
bench/run.sh fib             # one benchmark, default size
bench/run.sh fib 32          # one benchmark, custom size
NO_JVM=1 bench/run.sh        # jolt only (skip the JVM reference)
MODE_A=1 bench/run.sh        # also time each bench as a plain release build

bench/testcheck.sh           # 64-bit arithmetic + test.check generators (run mode)
NO_JVM=1 bench/testcheck.sh  # jolt only
```

Two build modes matter: **optimized** (`--direct-link --opt`, the default
scorecard — inlining, scalar replacement, closed-world direct linking) and
**release** (plain `jolt build`, what a default build ships — inference passes
but no direct-link/inlining). `MODE_A=1` adds the release column so a
release-mode win or regression is visible; it roughly doubles build time, so
it's on demand.

Needs Chez's kernel dev files (`libkernel.a` + `scheme.h`) and `cc` for the build,
like `jolt build`; set `JOLT_CHEZ_CSV` to override the detected csv dir.

## Startup / small-program latency

`bench/run.sh` builds each benchmark to a binary and times the compute *inside*
it, so it deliberately excludes `jolt`'s own startup. That fixed floor — boot the
runtime + compiler image, then compile the program — is what dominates ys-style
workloads: many short `jolt prog.clj` runs where the program itself runs for
milliseconds. `bench/startup.sh` measures it, whole-process wall clock (best of N)
for a built jolt against babashka on the same sources:

```sh
bench/startup.sh                          # default 7 reps
REPS=15 bench/startup.sh                   # more reps
JOLT_BIN=/path/to/jolt bench/startup.sh   # pick the binary
```

Three sizes: `version` (pure boot floor, no program), `trivial` (boot + compile +
run a one-liner), `script` (a small lazy-seq pipeline). Use a BUILT jolt
(`target/release/jolt` or an installed one), not the dev `bin/jolt` source
launcher — the dev script boots from source and opts out of the AOT cache, so it
is not representative. Indicative (2026-08-24, x86_64 Linux, Intel i5-4278U):
~276ms boot floor; babashka wasn't installed on this host to compare against.
The previous M-series figure (~117ms vs babashka ~18ms, ~6.5×) is not
comparable — different, weaker machine, and no babashka reference here. The
floor is runtime + compiler image instantiation that re-runs each boot (Chez
has no heap snapshot); see the CLI-closure AOT work that removed the per-boot
recompile of `jolt.main`.

`startup.sh` tells you the floor is there but not where it goes. `bench/startup-phases.sh`
attributes a `jolt prog.clj` run to four phases so a change shows which one it moved:

```sh
bench/startup-phases.sh                              # 7 reps, 400 defns, 30M-iter loop
REPS=15 bench/startup-phases.sh                      # more reps
DEFNS=800 LOOP=60000000 bench/startup-phases.sh      # heavier compile / run
JOLT_BIN=/path/to/jolt bench/startup-phases.sh      # pick the binary
```

`boot` is `jolt --version` (runtime + image load, `jolt.main` recompile).
`dispatch` is the deps/project resolve + load-file setup a file run adds on top,
measured against a `nil` file. `compile` is the delta of a compile-heavy,
run-trivial program (many defns) over the `nil` file, and `run` is the delta of a
run-heavy, compile-trivial program (one long loop). The phases are external
subtractions, each isolating one cost by construction — honest approximations,
not a strict partition, but directional: speed up the compiler and `compile`
drops, speed up the runtime and `run` drops. Indicative (2026-08-24, x86_64
Linux, Intel i5-4278U): boot ~274ms, dispatch ~2ms, compile ~971ms for 400
defns, run ~62ms for a 30M-iter loop — compilation is the dominant per-program
cost, more so than on the previous M-series host (boot ~110ms, dispatch ~1ms,
compile ~400ms, run ~120ms), which is not directly comparable to this row.

## A/B against a change

To measure a pass, run the suite on `main`, then on the branch, back to back
(same machine, quiet). Each benchmark prints `runs: [...]` and `mean: N ms`;
compare the means. A pass is worth landing when it moves a benchmark whose axis it
targets, even if the ray tracer stays flat.

`bench/aba.sh` automates an A1/B/A2 over the six benches: it checks out the
parent's compiler files (`host/chez/seed/image.ss` +
`jolt-core/jolt/passes/types.clj`), builds and times each bench against `HEAD`,
then restores the working tree. A1≈A2 rules out drift; B vs A is the change.

### Dev mode, and why `aba.sh` cannot see it

`bench/aba.sh` compiles each benchmark with `jolt build`, and a built binary's
prologues are baked with tracing OFF. So it is **structurally blind** to anything
that only exists on the `jolt run` / `-M:alias` path — which is where the
tail-frame history lives, and where tracing is on by default.

That blindness is not hypothetical. A per-call ring save/restore landed in v0.5.20
costing up to **19×** on proven-numeric code, shipped in v0.5.20 and v0.6.0, and
every AOT number above stayed flat throughout because built binaries never carried
it. What surfaced it was an application getting slower, not a benchmark.

`bench/aba-trace.sh` is the dev-mode A/B/A. Give it two already-built binaries
(a compiler change needs `make remint`, so reminting between phases would dominate
the wall clock):

    bench/aba-trace.sh /tmp/jolt-A /tmp/jolt-B

Its bench set deliberately spans BOTH shapes, because the original set was
call-heavy only and that is precisely why the regression was invisible:

| shape | benches | what it shows |
|---|---|---|
| call-heavy | `fib` `tak` `binary-trees` | the per-entry ring push — what tracing fundamentally costs |
| numeric loops | `arrays` `mathfns` `loop-recur` `mandelbrot` | per-call-SITE work landing on code that had none, so it reads as a multiple rather than a percentage |

Tracing is **not** free, and the cost is very uneven. Against `JOLT_TRACE=0` on the
same binary: `fib` ~10× (6.9 vs 0.7 ms), `binary-trees` ~1.6×, while the numeric
benches are within noise. Reach for `JOLT_TRACE=0` when timing a dev-mode run —
and note it changes the emitted code, so give it its own `JOLT_CACHE_DIR` or you
will time a mix of both modes.
