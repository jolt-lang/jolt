# §9 The Core Library

**Status**: entry format fixed; exemplars for `first`, `reduce`, `parse-uuid`.
The full portable surface (≈500 vars after classification, dashboard in
`coverage.md`) is filled in chapter-by-chapter using this format.

Entries specify *behavioral contracts*, not implementations. Performance
characteristics are specified only where the language community relies on
them (e.g. vector `nth` is "effectively constant time" — SHOULD-level).

---

## Collection return types & laziness (cross-cutting)

Two contracts hold across the sequence library and are not restated per entry.

**Return-type fidelity.** A function returns the same *kind* of collection the
reference does — value equality is not enough, since `(= [0 1] '(0 1))`.

- Sequence transformations return **seqs** (lazy unless noted): `map`, `filter`,
  `remove`, `keep`, `mapcat`, `take`/`drop` and their `-while` forms, `partition`,
  `partition-all`, `partition-by`, `interpose`, `dedupe`, `distinct`, `concat`,
  `reductions`, `cons`, `rest`, `sequence`. The *elements* of `partition` /
  `partition-all` / `partition-by` are themselves seqs, not vectors.
- The vector variants return **vectors**: `mapv`, `filterv`, `vec`, `subvec`,
  `partitionv`, `partitionv-all`, `splitv-at`. `split-at` / `split-with` return a
  2-vector `[take drop]`. A transducer applied eagerly (`into []`, the
  `partition-all` transducer's chunks) yields vectors.
- Type-preserving functions return the input's type: `replace` over a vector is a
  vector, over any other seqable a (lazy) seq; `empty`/`into (empty coll)` keep the
  collection kind; `set`/`into #{}` return sets; `into {}`/`select-keys`/`zipmap`/
  `frequencies`/`group-by`/`merge` return maps (`group-by` values are vectors).

**Laziness.** The lazy sequence functions — including `sequence`, `eduction`, and
`mapcat` — MUST consume their source incrementally and so terminate on an infinite
or unbounded source when only a prefix is demanded: `(first (sequence (map inc)
(range)))` and `(take n (mapcat f (range)))` return without realizing the whole
source. `(apply concat coll-of-colls)` is likewise lazy in its argument seq. The
eager consumers (`reduce`, `into`, `count`, `vec`, `doall`) realize the demanded
portion fully.

These are exercised by the `seq / lazy over infinite` and the per-fn type-predicate
rows in the conformance corpus.

---

### first — since 1.0

```
(first coll)
```

**Semantics**

- S1. MUST return the first element of `(seq coll)`.
- S2. If `(seq coll)` is `nil` (i.e. `coll` is empty or `nil`), MUST return
  `nil`.
- S3. MUST accept anything *seqable* (§5): seqs, lists, vectors, maps
  (yielding map entries), sets, strings (yielding characters), `nil`.
- S4. On a lazy sequence, MUST realize at most the first element (§5
  laziness contract).

**Edge cases**

- E1. `(first nil)` ⇒ `nil`; `(first [])` ⇒ `nil`; `(first "")` ⇒ `nil`.
- E2. A `nil` or `false` first *element* is returned as-is — callers cannot
  distinguish "empty" from "first element is nil" via `first` alone (that is
  what `seq` is for).
- E3. On a map, the element is a map entry; on an unordered collection (map,
  set) *which* element is first is implementation-defined but MUST be
  consistent with that collection's seq order for the same collection value.

**Errors**

- X1. A non-seqable argument (e.g. a number) MUST throw.

**Examples**

```clojure
(first [1 2 3])      ;=> 1
(first '())          ;=> nil
(first "ab")         ;=> \a
(first {:a 1})       ;=> [:a 1]
(first [nil 2])      ;=> nil
```

**Conformance**

S1–S3, E1–E2 → jolt `sequences-spec` "seq / access"; clojure-test-suite
`core_test/first.cljc`. S4 → jolt `lazy-seqs-spec` counter cases. X1 →
clojure-test-suite `core_test/first.cljc` (throwing cases).

---

### reduce — since 1.0

```
(reduce f coll)
(reduce f init coll)
```

**Semantics**

- S1. With `init`: MUST return `init` if `(seq coll)` is nil; otherwise MUST
  return `(f … (f (f init e₁) e₂) … eₙ)`, applying `f` left-to-right over the
  elements, exactly once each.
- S2. Without `init`: if `coll` is empty, MUST return `(f)` (f called with
  no arguments); if `coll` has one element, MUST return that element
  *without calling `f`*; otherwise as S1 with `init = e₁` over `e₂…eₙ`.
- S3. **Reduced short-circuit**: if any intermediate result is a `reduced`
  value, iteration MUST stop and the dereferenced value MUST be returned
  immediately; `f` MUST NOT be called again.
- S4. `reduce` is eager: it MUST fully realize the consumed portion of a
  lazy `coll` (to the end, or to the `reduced` point).

**Edge cases**

- E1. `(reduce f nil)` ⇒ `(f)`; `(reduce f init nil)` ⇒ `init`.
- E2. A `reduced` value as the *initial* `init` is NOT unwrapped before the
  first call in the reference — ⚠ under-documented; differential result to
  pin down and test before this entry is marked verified.
- E3. Visit order over maps is entry order of the map's seq;
  over vectors/lists/seqs it is sequential order (normative).

**Errors**

- X1. Without `init`, on an empty coll, if `f` has no zero-arg arity the
  call `(f)` MUST throw (arity error).

**Examples**

```clojure
(reduce + [1 2 3 4])                                ;=> 10
(reduce + 10 [1 2 3 4])                             ;=> 20
(reduce + [])                                       ;=> 0    ; (+) is 0
(reduce + [5])                                      ;=> 5    ; f not called
(reduce (fn [a x] (if (> a 2) (reduced a) (+ a x))) 0 [1 2 3 4 5]) ;=> 3
```

**Conformance**

S1–S3, E1 → jolt `sequences-spec` "map filter reduce" group +
`transducers-spec` "reduce honors reduced"; clojure-test-suite
`core_test/reduce.cljc`. S2 (single-element, f-not-called) → jolt conformance
"reduce single no init". E2 → UNVERIFIED (differential test to add). S4 →
`lazy-seqs-spec`.

---

### parse-uuid — since 1.11

```
(parse-uuid s)
```

**Semantics**

- S1. If `s` is a string in canonical UUID form — five groups of hex digits
  of lengths 8, 4, 4, 4, 12 separated by `-` — MUST return a UUID value `u`
  such that `(uuid? u)` is true and `(str u)` is the lowercase form of `s`.
- S2. Parsing MUST be case-insensitive and equality on the results
  case-insensitive: `(= (parse-uuid s) (parse-uuid (upper-case s)))` is true.
- S3. If `s` is a string not in canonical form, MUST return `nil`.
  ⚠ reference-divergence: reference Clojure (java.util.UUID) additionally
  accepts non-canonical forms like `"0-0-0-0-0"`; ClojureScript and other
  dialects are strict. This spec adopts **strict** (the cross-dialect
  behavior); the reference's permissiveness is recorded as host leniency.
- S4. UUID values MUST support value equality, hashing (usable as map keys
  and set members), `str` (lowercase canonical form), and print as the
  tagged literal `#uuid "…"` such that the printed form reads back equal
  (§2 tagged literals).

**Edge cases**

- E1. `""`, over-long, truncated, non-hex characters, and misplaced dashes
  ⇒ `nil`.

**Errors**

- X1. A non-string argument MUST throw.

**Examples**

```clojure
(parse-uuid "b6883c0a-0342-4007-9966-bc2dfa6b109e")  ;=> #uuid "b6883c0a-…"
(uuid? *1)                                            ;=> true
(parse-uuid "df0993")                                 ;=> nil
(parse-uuid 1000)                                     ;; throws
```

**Conformance**

S1–S4, E1, X1 → jolt `uuid-spec` (30 cases) + 6 three-path conformance
cases; clojure-test-suite `core_test/parse_uuid.cljc`,
`core_test/uuid_qmark.cljc`, `core_test/random_uuid.cljc`.

---

### clojure.template/apply-template, clojure.test/are — since 1.1

```
(apply-template argv expr values)
(are argv expr & args)
```

**Semantics**

- S1. `apply-template` MUST replace every occurrence of each `argv` symbol
  in `expr` with its corresponding value by structural walk (postwalk symbol
  substitution), not by lexical binding. Occurrences inside `quote` and at
  any nesting depth substitute: `(apply-template '[x] '(f 'x) '[if])` ⇒
  `(f 'if)`.
- S2. `do-template` MUST partition `args` by `(count argv)` and expand to a
  `do` of one substituted `expr` per group.
- S3. `clojure.test/are` MUST expand through `do-template` with `expr`
  wrapped in `is`. Consequently `(are [x] (special-symbol? 'x) if def)`
  asserts `(special-symbol? 'if)` and `(special-symbol? 'def)` — a
  let-binding implementation is non-conforming (the quoted symbol would not
  substitute).

**Errors**

- X1. `are` MUST throw at macroexpansion when `(count args)` is not a
  positive multiple of a non-empty `(count argv)` (empty/empty is allowed).
- X2. `apply-template` MUST throw when `argv` is not a vector of symbols.

**Conformance**

S1–S3 → `test/chez/clojure-test.clj` (are with quoted template var);
clojure-test-suite `core_test/special_symbol_qmark.cljc` and every
`are`-based suite namespace.

---

### make-hierarchy, derive, underive, isa?, parents, ancestors, descendants — since 1.0

```
(make-hierarchy)
(derive tag parent)            (derive h tag parent)
(underive tag parent)          (underive h tag parent)
(isa? child parent)            (isa? h child parent)
(parents tag)  (ancestors tag)  (descendants tag)   ; + (f h tag) forms
```

**Semantics**

- S1. A hierarchy is a pure value `{:parents {tag #{...}} :ancestors {...}
  :descendants {...}}`; the 3-arity forms are pure, the shorter arities read and
  mutate the global hierarchy.
- S2. `isa?` is true when `(= child parent)`, when the host type system says
  parent is assignable from child (both classes), when the relationship was
  `derive`d — including a relationship derived on one of a class child's
  supers — or component-wise for equal-length vectors.
- S3. Class tags answer through the host type hierarchy: `(parents c)` includes
  the class's direct supers (`bases` — a concrete class's chain roots at
  `java.lang.Object`, an interface's does not); `(ancestors c)` is the
  transitive set plus anything `derive`d on the class or its supers. A
  deftype/defrecord class's ancestry includes its implemented protocol
  interfaces and, for records, the record interfaces
  (`clojure.lang.IRecord`/`IPersistentMap`/`Associative`/…; `clojure.lang.IType`
  for a bare deftype).
- S4. `derive` returns the updated hierarchy (3-arity) or nil (2-arity);
  deriving a relationship that already holds transitively, or one that would
  create a cycle, throws.

**Errors**

- X1. `derive` asserts its argument shapes: parent must be a namespaced Named
  value; tag must be a class or a Named value (namespaced in the 2-arity
  global form); `(derive h tag tag)` fails the `not=` assert. AssertionError.
- X2. `underive`/`derive` with a non-hierarchy `h` throw at the parents
  lookup (the map is called as a function, like the reference).
- X3. `(descendants h SomeClass)` throws UnsupportedOperationException
  ("Can't get descendants of classes") — Java type inheritance is not
  enumerable downward.

**Conformance**

S1–S4, X1–X3 → corpus `hierarchy / *` rows; clojure-test-suite
`core_test/{derive,underive,isa_…,parents,ancestors,descendants}.cljc`
(all fully passing).

---

### atom, add-watch, remove-watch, set-validator!, get-validator — since 1.0

```
(atom x & {:keys [meta validator]})
(add-watch iref key f)   (remove-watch iref key)
(set-validator! iref f)  (get-validator iref)
```

**Semantics**

- S1. Watches, validators, and reference metadata are one contract (the JVM's
  ARef/IRef) shared by atoms, vars, and agents. `add-watch`/`remove-watch`
  return the reference; re-adding a key replaces that watch in place.
- S2. A watch is called `(f key ref old new)` after a state change: atom
  swap!/reset!/compare-and-set!, var ROOT changes (`def` on a watched var,
  `var-set` outside a thread binding, `alter-var-root` — a thread-binding set
  does not notify), and each agent action's state change.
- S3. A validator gates every state change and, via the `:validator` ctor
  option, the initial value — an invalid initial value never constructs the
  reference.
- S4. The `:meta` ctor option attaches reference metadata (`meta` reads it,
  `alter-meta!`/`reset-meta!` update it); nil is allowed.

**Errors**

- X1. A rejected value (validator returns logical false or the ctor option
  fails on the initial value) throws IllegalStateException "Invalid reference
  state".
- X2. A non-map `:meta` ctor option throws ClassCastException.

**Conformance**

S1–S4, X1–X2 → corpus `iref / *` rows; clojure-test-suite
`core_test/{atom,add-watch,remove-watch}.cljc` (the remaining baselined error
in the watch namespaces is their STM `ref` section — refs are out of scope,
`stm-refs` in `coverage.md`).

---

### clojure.string coercion, some-fn, ifn? — since 1.2/1.3

```
(clojure.string/upper-case s) …    (some-fn p & ps)    (ifn? x)
```

**Semantics**

- S1. The clojure.string case fns and searches (`upper-case`, `lower-case`,
  `capitalize`, `starts-with?`, `ends-with?`, `includes?`, `index-of`,
  `replace`) take any Object `s` through its `toString`, like the reference's
  `^CharSequence`+`.toString` signatures: `(upper-case :kw)` is `":KW"`,
  `(capitalize 1)` is `"1"`. nil throws (method call on null); a nil `substr`
  throws.
- S2. `some-fn` follows the reference arities: at least one predicate
  (`(some-fn)` is an arity error) and the returned fn chains with `or`, so a
  no-match result is the last predicate's own falsy value (`false` stays
  `false`).
- S3. `ifn?` covers fns, keywords, symbols, maps, sets, vectors, vars,
  multimethods, promises (invoking a promise delivers it), and a
  deftype/defrecord implementing `clojure.lang.IFn`'s `invoke`.
- S4. A `defmulti`/`defmethod` deferred inside a fn body interns/resolves in
  the namespace it was WRITTEN in (the macros bake their expansion ns), not
  whatever namespace is current when it runs.

**Conformance**

S1–S4 → corpus `string / toString coercion`, `core / some-fn`, `core / ifn?`,
`multimethods / deferred definition`; clojure-test-suite string/some-fn/
ifn-qmark/boolean-qmark/reduce namespaces (all fully passing).

---

## Authoring notes

- Source examples from the ClojureDocs export (`clojuredocs-export.edn`,
  648 core vars have community examples) — but every example is verified
  against the reference before inclusion.
- When writing an entry surfaces a behavior question, settle it by
  differential test first; if dialects split, that's a classification
  decision (host-dependent / divergence note), not a coin flip.
- An entry is **Verified** when no field carries UNVERIFIED; `coverage.md`
  tracks per-var status.
