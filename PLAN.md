# Jolt — Complete Implementation Plan

## Architecture Goal

Minimal Janet bootstrap → SCI/CLJS Clojure source runs on Jolt.

Three layers:
1. **Janet runtime**: types.janet, reader.janet, evaluator.janet, compiler.janet (~4,200 lines)
2. **Clojure core**: core.janet (~1,400 lines), phm.janet (~200 lines)
3. **Clojure source** (.clj files loadable at runtime): stdlib modules, SCI

## Current State

| Metric | Value |
|--------|-------|
| Total tests | 317 |
| Passing | 316 |
| Failing | 1 (lang.cljc deftype — deferred to Phase 15) |
| CLJS test files ported | ~15/60 |
| Total assertions | 860+ across 24 test files |
| Source lines | ~5,600 (7 core .janet files) |

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

### Phase 15: SCI Bootstrap

- Complete `sci.lang` namespace with Var type
- Load remaining SCI namespaces
- SCI test runner
- Fix SciVar `#?@` deftype issue

### Phase 16: Remaining Core Library + Tests

- Port ~20 remaining CLJS test files
- Fix found gaps: `&` rest destructuring, `seq` nil handling, vector/list equality
- `eval`, `syntax-quote` completion

### Phase 17: Optimization

- Compiler improvements: inline small core functions
- PersistentHashMap dynamic bucket growth
- Benchmarks

### Phase 18: Standard Library Completion

- Complete EDN reader/writer
- Complete java.io wrappers
- clojure.zip tests

## Implementation Order

1. ✅ Phases 0-14 (completed)
2. Phase 15 (SCI bootstrap) — **critical path**
3. Phase 16 (remaining test porting + feature gaps)
4. Phases 17-18 (optimization, stdlib)
