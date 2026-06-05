# Jolt — Complete Implementation Plan

## Architecture Goal

Minimal Janet bootstrap → SCI/CLJS Clojure source runs on Jolt.

Three layers:
1. **Janet runtime**: types.janet, reader.janet, evaluator.janet, compiler.janet (~4,200 lines)
2. **Clojure core**: core.janet (~1,400 lines), phm.janet (~200 lines)
3. **Clojure source** (.clj files loadable at runtime): stdlib modules, SCI

## Current State

Tests are organized in three layers (see the README "Test" section):
`test/spec/` (the black-box contract), `test/integration/` (cross-cutting and
external-suite regression batteries), and `test/unit/` (white-box component
tests). `jpm test` is green.

| Metric | Value |
|--------|-------|
| Spec contract | ~1,000 cases across 24 `test/spec/` files |
| Integration batteries | 12 (conformance, SCI bootstrap/runtime, jank, clojure-test-suite, compile-mode, API, systematic-coverage) |
| Unit tests | 9 files (reader, evaluator, types, collections, regex, compiler) |
| Clojure conformance | 218/218 |
| clojure-test-suite | ~3,920 assertions pass (remainder are documented platform/design divergences) |
| Source lines | ~8,600 (`src/jolt/*.janet`; core.janet ~4,000) |
| SCI source files loading | 9/9 |
| Reader literals | full numeric syntax (`N`/`M`/ratio/radix/exponent/`##Inf`); see `doc/grammar.ebnf` |

## Phase Plan

### Phase 0-10: Foundation ✓

| Phase | Description | Status |
|-------|-------------|--------|
| 0 | `defn` bug fix, bare symbol resolution | ✓ |
| 1 | Var/Namespace system, ns form extensions | ✓ |
| 2 | PersistentHashMap implementation | ✓ |
| 3 | Var system: var-get/set/?, alter-var-root, intern, binding | ✓ |
| 4 | deftype/defrecord completion | ✓ |
| 5 | Multimethods + Hierarchy | ✓ |
| 6 | Reader extensions: tagged literals, :jolt/tagged handler | ✓ |
| 7 | LazySeq + PersistentHashSet | ✓ |
| 8 | Protocol system: defprotocol, extend-type, extend-protocol, reify, satisfies? | ✓ |
| 9 | REPL fixes: buffer-based output, collection rendering, cond fix | ✓ |
| 10 | Standard Library: clojure.string, clojure.set, clojure.walk, clojure.zip, clojure.edn, clojure.java_io, jolt.interop, jolt.shell, jolt.http | ✓ |

### Phase 11: Fix Pre-existing Failures ✓

- `types.janet`: `ns?` now accepts both structs and tables
- `core.janet`: `comment` macro wired into core-bindings
- `sci/lang_stubs.clj`: minimal SCI type stubs for bootstrap
- `test-load-sci.janet`: load stubs before SCI source files
- **Result: SciVar fixed. 1 remaining (deftype with `#?@` — Phase 15)**

### Phase 12: Core Feature Completion ✓

- `apply` support in evaluator + compiler
- `str` handles nil correctly
- 6 CLJS test files created (~120 assertions)
- `#()` anonymous fn reader with `%`, `%1`, `%2` arg handling

### Phase 13: Protocol Completion ✓

- reify dispatch: protocol methods work on reified objects
- `#()` reader macro with gensym-based `%` arg handling
- IFn protocol support in default invocation arm
- clojure.walk loads and `keywordize-keys` works
- 4 test sections: reify dispatch, anon fn, extend-type, walk loading

### Phase 14: Extend CLJS Ported Tests ✓

- `cljs-port-2.janet` expanded: 10 sections (12-21), 35→60 assertions
- `cljs-port-5.janet` created: sections 22-24, destructuring, metadata, fn composition
- `pr-str` compiler fix: maps to new `core-pr-str` (not `core-str`)
- `every-pred` added to core.janet
- `var-dynamic?` and `with-meta` tests restored

### Phase 15: SCI Bootstrap ✓

- ✅ `sci.lang` namespace loads completely (all 10 forms, including Var, Type, Namespace deftypes)
- ✅ 9 SCI source files load without errors (impl/macros, impl/protocols, impl/types, impl/unrestrict, impl/vars, lang, impl/utils, impl/namespaces, core)
- ✅ `prefer-method`/`remove-method`/`remove-all-methods` promoted to special forms (fix: auto-deref gave functions to `get`/`put`)
- ✅ All 5 pre-existing test failures fixed:
  - `cljs-port-1.janet` — `#{}` Janet comment issue replaced with count-based comparisons
  - `cljs-port-2.janet` — `with-meta` added as core binding with table/setproto
  - `cljs-port-3b.janet` — `load-string` multi-form loader for string.clj and set.clj
  - `cljs-port-5.janet` — `var-dynamic?` core binding + `^:dynamic` def handler fix
  - `phase5-test.janet` — `remove-method` special form fixed to eval-form first arg
- New core infrastructure: `core-with-meta` (supports structs/tables via prototype), `core-var-dynamic?`, `load-string` API, `^:dynamic` propagation in `def` handler
- `core-str` now returns `"nil"` for nil (Clojure-compatible)
- `core-meta` checks `:jolt/meta` for with-meta'd values
- Test suite: **317/317 pass, 0 fail**

### Phase 16: Remaining Core Library + Tests ✅

- ✅ `eval` implemented as special form (interpreter + compiler), tested in `eval-test.janet` (4 assertions)
- ✅ `&` rest destructuring, `seq` nil handling, `vector`/`list` equality verified working
- ✅ `syntax-quote` confirmed working with unquote
- ✅ 5 new CLJS ported test files (cljs-port-6 through -10): anon fns, symbols/keywords/lists, destructuring, range/concat/partition/sort, seq predicates/complement, when/if-let/doto
- ✅ 16 total CLJS ported test files, 440 assertions across 31 test files
- ✅ 317/317 tests pass, 0 failing scripts

### Phase 19: Clojure Conformance Hardening ✅

Added `test/conformance.janet` — a harness that evaluates `(= expected actual)`
inside jolt (using jolt's own `=`, i.e. real Clojure semantics), seeded from
`~/src/clojure/test`. Drove fixes for a large class of real-Clojure idioms the
bespoke `.janet` suite never exercised. **80/80 conformance, 46/46 legacy suite,
0 SCI-load failures.**

CRITICAL (core semantics):
- Collections as IFn (`([v] i)`, `({m} k)`, `(#{s} x)`) via unified `jolt-invoke`
- Unified vector representation on Janet tuples (PersistentVector was broken/disconnected)
- `=`/`reduce`/`vec`/`into` now realize lazy-seqs; `into {}`/`into ()` fixed
- Lazy `iterate`/`range`(infinite)/single-coll `map` (over lazy input)
- Truly lazy `concat` → self-referential `lazy-cat`/`lazy-seq` work (`fib-seq`)

HIGH:
- Unified recursive destructuring (nested seq/map, `:strs`/`:syms`/`:or`/`:as`, fn-params)
- `defn` docstring/attr-map handling (docstring'd fns were silently param-less)
- Clojure `str`/`pr-str` (nil→"", keywords/symbols, collections render readably)
- `update`/`update-in`/`assoc-in` on immutable maps; multimethod `:default` key dispatch
- defrecord inline protocol methods with field scope
- `require` loads stdlib namespaces from disk + aliased-ns resolution (`s/join`)

MED:
- New: peek/pop/subvec/reduce-kv/cycle/partition-all/reductions/dedupe/
  keep-indexed/map-indexed/trampoline/format/read-string/fnil + letfn/doseq macros
- Recursive `jolt-equal?` (nested sequential + order-independent map equality)
- Lazy `filter`/`take-while`/`remove` over lazy input
- REPL: lazy-seq printer walks full chain (capped); `def`/`defn` print `#'ns/name`

**Gap-closing batch** (conformance grew to 157 cases): full atoms; typed
volatiles + delays (deref/force/realized?); quot/rem/mod sign semantics; a regex
subsystem (`src/jolt/regex.janet`, regex→PEG, `#"..."` literals, re-find/matches/
seq, string split/replace); sorted-map/sorted-set; `Math/*` statics; ex-info/
ex-data/ex-message; namespaced keywords; ~40 added core fns (split-at, take-nth,
butlast, filterv/mapv, reduced, min/max-key, find, some-fn, sequential?/ifn?/…)
plus defonce/macroexpand-1/letfn/doseq; and fixes to doto, assoc/update-on-vector,
frequencies, coll?, sort, partition.

**Feature batch** (conformance now 184): proper **char type** (`\a` reads to a
char value, char?/char/int/str, string seq/first/nth yield chars, `\a` printing);
**transducers** (map/filter/take/… no-coll arity returns a transducer; transduce/
into-xform/sequence/eduction/reduced; reduce honors reduced); **compiler-path
IFn dispatch** (jolt-call makes collections callable in `:compile?` mode; compiled
set/char literals); and a **multi-line REPL** (accumulates until the form is
balanced).

Deferred: full regex (lookaround/backrefs — the PEG-backed engine covers the
common subset). STM (refs/agents/dosync) intentionally skipped.

### Phase 17: Optimization

- Compiler improvements: inline small core functions
- PersistentHashMap dynamic bucket growth
- Benchmarks

### Phase 18: Standard Library Completion

- Complete EDN reader/writer
- Complete java.io wrappers
- clojure.zip tests

## Implementation Order

1. ✅ Phases 0-16 (completed)
2. Phase 17 (optimization: compiler inlining, PHM bucket growth, benchmarks)
3. Phase 18 (stdlib: EDN reader/writer, java.io wrappers, clojure.zip tests)
