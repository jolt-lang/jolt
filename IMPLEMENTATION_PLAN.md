# Full Jolt Implementation Plan

## Strategy

Minimal Janet bootstrap (parser + var system + namespace system + core types)
→ load SCI's Clojure source → rest of stdlib is Clojure.

Jolt already has 3 phases matching SCI's architecture:
- Parser: reader.janet (hand-rolled, works)
- Analyzer: compiler.janet (2-phase: analyze-form → emit-expr)
- Evaluator: evaluator.janet (interpreter) + Janet's native eval (compiled path)

## Phase 0: Fix defn Bug + Symbol Resolution (now)

### 0.1: Bare symbols must go through compile path too
In api.janet eval-string, change:
  (if (and compile? (array? form))  → also true for bare symbols
Only skip compiler for literal values (nil, numbers, strings, keywords, booleans).
Everything else → compile-and-eval.

### 0.2: compile-and-eval for def must intern in Jolt namespace
After (eval (compile-ast form ctx)), if the form is a def:
  (ns-intern ns (name-sym :name) val)
So the interpreter can find the var later.

### 0.3: Add defn to Phase 6 tests
Test: (defn foo [x] x) → (foo 1) = 1, foo returns the fn

## Phase 1: Complete Var/Namespace System

### 1.1: Full Var semantics
- Var with dynamic bindings (push-thread-bindings/pop-thread-bindings)
- binding macro (already exists in evaluator, needs compiler support)
- alter-var-root, var-get, var-set
- ^:dynamic metadata
- ^:macro metadata (already there)
- Var metadata (already there)

### 1.2: Full Namespace system
- ns form with :require, :use, :import, :refer-clojure, :refer
- create-ns, find-ns, all-ns, remove-ns
- ns-aliases, ns-refers, ns-imports
- ns-name, ns-map, ns-interns
- in-ns (already there)

### 1.3: Interop symbol resolution
- clojure.core/* for fully qualified
- ns-alias/name for aliased
- Import resolution (ClassName → constructor)

Tests: Port SCI's var_test.cljs and ns_test.cljs

## Phase 2: Complete Core Library

### 2.1: PersistentHashMap
Port from CLJS (cljs.core/PersistentHashMap) using hash-array-mapped trie.
Current Janet struct fallback is O(n). Need:
- Array-based HAMT with 32-way branching
- hash and = consistent with Clojure semantics
- transient support for bulk operations

### 2.2: PersistentHashSet  
Port from CLJS. Wrap PersistentHashMap with sentinel values.

### 2.3: Lazy Sequences
- LazySeq type
- lazy-seq macro
- Realize-once semantics
- Sequence caching (already computed? check)

### 2.4: Remaining clojure.core fns (ported from SCI)
- clojure.string (port from CLJS)
- clojure.set (port from CLJS)  
- Sequence library: partition, split-at, split-with, interleave, interpose
- Reducers: reduce-kv, keep, keep-indexed
- sorted maps/sets (skip for now — use hash-based)
- chunked sequences (skip for now)

## Phase 3: Protocol System

### 3.1: defprotocol
Port from SCI. Protocols are maps of method-name → dispatch-fn.
Protocol methods compile to calls through the protocol dispatch table.

### 3.2: extend-type / extend-protocol
Type → protocol method implementations.
Stored in type metadata.

### 3.3: reify
Anonymous type implementing protocols.

### 3.4: satisfies? / extends?
Runtime protocol check.

Tests: Port SCI's protocol tests

## Phase 4: deftype / defrecord (Complete)

### 4.1: deftype with mutable fields
- ^:volatile-mutable / ^:unsynchronized-mutable
- Field accessors (.field obj)
- Field mutation (set! (.-field obj) val)

### 4.2: defrecord
- Creates type + factories (→RecordName, map->RecordName)
- IPersistentMap implementation
- equals/hashCode based on type + fields

## Phase 5: Multimethods

### 5.1: Hierarchy
- derive, isa?, ancestors, descendants
- make-hierarchy
- Global hierarchy atom

### 5.2: defmulti / defmethod
- Dispatch function
- Method dispatch via hierarchy
- :default dispatch value
- prefer-method

## Phase 6: Reader Extensions

### 6.1: Tagged Literals
- #inst, #uuid
- default-data-readers
- *data-readers* dynamic var

### 6.2: Reader Conditionals
- #? and #?@ for platform-specific code
- Feature expressions (:clj, :cljs, :jolt)

### 6.3: Metadata reader
- ^:key val and ^{:key val} forms
- Attach metadata to any form

## Phase 7: SCI Bootstrap

### 7.1: Load SCI source
- Parse and load sci.core, sci.lang, sci.impl.* namespaces
- Expose Jolt runtime as Clojure vars (types, vars, namespaces)
- SCI's Clojure code runs on Jolt's eval stack

### 7.2: Benchmark
- Compare Jolt (Janet) vs Jolt+SCI (Clojure-on-Jolt)
- Use SCI's existing benchmark suite
- Identify bottlenecks for optimization

## Phase 8: Compiler Optimization

### 8.1: Better symbol resolution
- Known locals → direct Janet var access
- Known core fns → direct fn invocation (already done)
- Var deref → inline deref

### 8.2: Loop optimization
- Primitive loop/recur (already done with closure-based)
- Tail-call optimization for self-recursion

### 8.3: Inlining
- Small core functions (inc, dec, +, -) inlined
- Type hints for numeric ops

## Phase 9: Integration Testing

### 9.1: Port CLJS test suite
- collections_test.cljs → Jolt
- core_test.cljs → Jolt
- seqs_test.cljs → Jolt
- predicates_test.cljs → Jolt
- recur_test.cljs → Jolt

### 9.2: Self-hosting test
- Jolt running Jolt's own test suite
- Jolt running SCI's test suite

## Phase 10: Standard Library (Clojure-written)

### 10.1: Port from CLJS/SCI
- clojure.string
- clojure.set  
- clojure.walk
- clojure.zip
- clojure.edn (reader already, need writer)
- clojure.java.io (Janet file I/O)

### 10.2: Jolt-specific
- jolt.interop (Janet interop)
- jolt.shell (Janet shell commands)
- jolt.http (Janet HTTP client)

## Implementation Order

1. Phase 0 (defn bug) — **now**
2. Phase 1 (var/ns system) — **this week**
3. Phase 2 (core library) — **next week**
4. Phase 6 (reader extensions) — allows loading SCI directly
5. Phase 7 (SCI bootstrap) — critical path
6. Phases 3-5 (protocols, deftype, multimethods) — as needed by SCI
7. Phases 8-10 (optimization, testing, stdlib) — polish

## Testing Strategy

Each phase: write tests FIRST (TDD), then implement.

Test files follow: test/<feature>_test.janet
  - Literal/source tests: (assert (= "expected" (compile-str "input")) "label")
  - Round-trip tests: (assert (= val (ct-eval ctx "input")) "label")  
  - Integration tests: load .clj files, eval, verify namespaces/vars

Run: janet test/<feature>_test.janet (single) or jpm test (all)
