# clojure.core migration worklist (jolt-1j0)

Tracking the move of clojure.core from native Janet (`src/jolt/core.janet`,
4145 lines / 421 `core-*` fns) into the self-hosted Clojure overlay
(`jolt-core/clojure/core/`). Goal: shrink the Janet seed to `core-renames` +
genuinely host-coupled fns.

## Phase 0 classification (heuristic — validate per batch)

| Bucket | Count | Disposition |
|---|---|---|
| SEED (in `compiler/core-renames`) | 73 | stay in Janet (compiler emits `core-X` directly) |
| MACRO (in `core-macro-names`) | 44 | Phase 3 |
| HOST-coupled (atoms/vars/meta/proxy/transient/arrays/futures/ns/io) | 80 | Phase 4 (where feasible) / stay |
| LAZY-coupled | 28 | Phase 5 |
| MOVABLE pure-eager (candidates) | 193 | **Phase 2** |

Counts are heuristic (name + body markers); the MOVABLE list still has some
host/lazy leakage (e.g. transient `assoc!`/`conj!`, `doall`/`dorun`,
`chunk-*`, `deliver`) to filter out as each batch is actually moved.

**Key finding:** after removing SEED + HOST, the self-hosted compiler
(`jolt-core/jolt/{ir,analyzer}.clj`) uses **no** additional clojure.core fns
beyond the kernel tier (`second`/`peek`/`subvec`/`mapv`/`update`) plus host
primitives (`atom`/`swap!`/`reset!`). So **Phase 1 (compiler-dep kernel tier)
is essentially already complete** — to verify, not build.

## Performance baseline (test/bench/core-bench.janet, compile mode, min of 5, ms)

| bench | ms |
|---|---|
| fib | 128 |
| seq-pipe | 88 |
| reduce | 391 |
| into-vec | 194 |
| map-build | 681 |
| map-read | 6 |
| str-join | 244 |
| hof | 604 |
| **TOTAL** | **2336** |

Re-run after each phase; watch for regressions as fns move from native Janet to
self-hosted Clojure (interpreted/compiled, slower than native primitives).

## Per-batch workflow + gate (every migration step)
1. Canonical Clojure def in the overlay tier; remove the Janet `core-X` defn +
   its `core-bindings` entry (confirm leaf first: only defn+binding refs).
2. **Add regression tests** for each moved fn — spec cases (test/spec/*-spec.janet,
   interpret) and, for any fn whose behavior is subtle or was buggy, a case in the
   3-mode conformance set (test/integration/conformance-test.janet).
3. Gate: conformance ×3 modes · clojure-test-suite ≥ baseline · stage2==stage3
   fixpoint · fib compiled-fast · core-bench A/B under identical load (the
   absolute number is load-sensitive — compare batch-vs-prior back to back).

If a moved fn surfaces a latent bug (e.g. nthrest's nil-vs-() result, the
if-let/when-let else-scope leak), fix it to match Clojure and add a regression
test, rather than preserving the bug.

## Phase 2 batches landed

- **Sorted collections (jolt-0lj)** — sorted-map / sorted-map-by / sorted-set /
  sorted-set-by / sorted? / sorted-map? / sorted-set? / subseq / rsubseq are
  pure Clojure in their own tier (`25-sorted.clj`). Representation: tagged host
  table with a comparator-ordered :entries vector and the ops ATTACHED to the
  value (:ops map) — the seed dispatch branches call through (coll :ops), so no
  module-level hooks and the ops survive forks/AOT images. Host surface grew by
  two minimal value primitives: jolt.host/tagged-table and jolt.host/ref-get
  (raw field read — plain `get` on a sorted coll is the comparator lookup and
  would recurse). GOTCHA for future attached-ops ports: inside the overlay,
  NEVER read your own wrapper's fields with `get`.

- **Leaf batch (jolt-ded)** — complement, fnil (canonical 2/3/4-arity: patch
  the first 1-3 args only, unlike the old patch-everything kernel fn),
  clojure-version, bigdec, numerator, denominator, supers, munge, test moved
  to 20-coll.clj.
- ***in* reader family (jolt-0d9)** — *in*, read-line, read, with-in-str,
  line-seq in a new `50-io.clj` tier over two seed seams (__stdin-read-line,
  __parse-next). GOTCHAS: a map LITERAL with :jolt/type as a key parses as a
  tagged form (don't tag overlay value maps); a leftover seed stub holding the
  same name breaks direct-linked overlay self-recursion (line-seq bound to the
  stub's root and truncated after one element) — delete the stub first.

## MOVABLE candidates (Phase 2 worklist, 193)
>Eduction NaN? abs aclone alength ancestors array-map array-seq assoc! associative? bean bigdec bigint biginteger boolean boolean? booleans byte bytes bytes? cat char char-escape-string char-name-string char? chars chunk chunk-append chunk-buffer chunk-cons chunk-first chunk-next chunk-rest chunked-seq? class clojure-version comparator compare-and-set! completing conj! counted? decimal? deliver denominator derive descendants destructure disj disj! dissoc! distinct? doall dorun double? doubles drop-last eduction empty ensure-reduced enumeration-seq ex-cause ex-data ex-info ex-info? ex-message find float? floats force halt-when hash-combine hash-map hash-ordered-coll hash-set hash-unordered-coll ident? ifn? indexed? infinite? inst-ms inst? integer? ints isa? iterator-seq key keyword keyword-identical? list* list? longs macrofy map-entry? memfn munge nat-int? neg-int? not-any? not-every? nthnext nthrest numerator numeric= object? parents persistent! pop pop! pos-int? pr prefers println-str prn-str promise qualified-ident? qualified-keyword? qualified-symbol? rand rand-nth random-sample ratio? rational? rationalize re-groups re-matcher record? reduce-kv reduced reduced? reductions replace replicate resolve reversible? rseq rsubseq run! seq-to-map-for-destructuring seque set set? short shorts shuffle simple-ident? simple-keyword? simple-symbol? some-search sort sort-by sorted-map sorted-map-by sorted-map? sorted-set sorted-set-by sorted-set? special-symbol? split-at split-with str-join str-replace-all str-replace-first str-split subseq supers symbol tagged-literal tagged-literal? take-last test transduce unchecked-add unchecked-byte unchecked-char unchecked-dec unchecked-divide-int unchecked-double unchecked-float unchecked-inc unchecked-int unchecked-multiply unchecked-negate unchecked-remainder-int unchecked-short unchecked-subtract undefined? underive uri? uuid? val vector volatile! volatile? xml-seq

## HOST-coupled (Phase 4 / stay, 80)
add-watch aget alter-meta! alter-var-root aset aset-boolean aset-byte aset-char aset-double aset-float aset-int aset-long aset-short atom atom? avoid-method-too-large boolean-array bounded-count byte-array char-array construct-proxy copy-core-var copy-var delay? deref double-array float-array future-call future-cancel future-cancelled? future-done? future? get-proxy-class get-validator init-proxy int-array intern into-array long-array make-array make-delay meta namespace namespace-munge new-var ns-name object-array pop-thread-bindings prefer-method print-dup print-method print-str proxy-call-with-super proxy-mappings proxy-super push-thread-bindings reader-conditional reader-conditional? remove-watch reset! reset-meta! reset-vals! set-validator! short-array swap! swap-vals! thread-first thread-last to-array to-array-2d transient transient? update-proxy var-dynamic? var-get var-set var? vary-meta vreset! vswap! with-meta

## LAZY-coupled (Phase 5, 28)
concat cycle dedupe distinct flatten interleave interpose iterate keep keep-indexed line-seq macro-names map-indexed mapcat partition partition-all partition-by rand-int random-uuid realized? repeat repeatedly seqable? sequence sequential? take-nth trampoline tree-seq unreduced

## Phase 3 (macros) — status & findings

20 macros moved to the overlay: 19 user-facing in `30-macros.clj`, plus `when`
in a new `00-syntax.clj` tier loaded **before** the kernel (interpreted defmacros,
so the macros exist before any code that uses them compiles).

Macro-authoring toolkit for jolt (learned the hard way):
- single-template hygiene: auto-gensym `foo#`
- shared explicit fresh symbol: `(symbol (str (gensym)))` — a bare `(gensym)` in a
  macro body returns a *Janet* symbol the destructurer rejects
- let-rebinding: splice binding *pairs* into a TEMPLATE vector (`[~a ~b ~@pairs]`),
  not a pre-built pvec value — `core-let` wants a tuple form
- build sub-forms via templates, never `cons`/`list` (those make plists the
  evaluator can't run as a form)
- Jolt `defmacro` is **single-arity** — use `& rest`/destructuring
- syntax-tier macros may use only special forms + core-renames seed primitives

**Performance wall (the hot macros stay in Janet for now):** the load-order story
works, but moving the *hot* fundamental control macros (`and`/`or`/`cond`/
`when-not`) regressed the battery — as interpreted overlay defmacros they expand
slower than native Janet, and since they appear in nearly every form the
cumulative overhead tipped a heavy suite file over the 6 s per-file timeout
(3930 -> 3911, +1 timeout). They are correct (conformance 228×3, all edge cases),
but reverted. Moving `and/or/cond/when-not/case/doseq/declare/cond->/->/->>`
needs a **fast (compiled) macro-expansion path**, not interpreted defmacros.

Deferred: `defn/defn-/fn/let/loop` (fundamental + same speed concern), the type
machinery (`defrecord/defprotocol/extend-*/reify/proxy/definterface` → Phase 4),
`lazy-seq/lazy-cat` (→ Phase 5).
