# Jolt

A Clojure interpreter running on [Janet](https://janet-lang.org). Jolt reads Clojure source, evaluates it with an interpreter written in pure Janet, and ships a Clojure-compatible standard library. The goal is a Janet-hosted [SCI](https://github.com/borkdude/sci) runtime — a minimal bootstrap that loads SCI's Clojure source as its standard library.

## Build

```bash
git clone https://github.com/yogthos/jolt.git
cd jolt
git submodule update --init   # pulls vendor/sci
jpm build                     # compiles build/jolt
```

Requires Janet ≥ 1.36 and `jpm`.

## Run

```
build/jolt                 # start a REPL
build/jolt file.clj [args] # run a file (binds *command-line-args* and *file*)
build/jolt -e EXPR [args]  # evaluate EXPR and print the result
build/jolt -h              # help
```

The REPL accumulates multi-line forms until they balance:

```
user=> (defn fib [n] (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
#'user/fib
user=> (map fib (range 10))
(0 1 1 2 3 5 8 13 21 34)
```

Running a file evaluates its top-level forms:

```
$ echo '(println "hello" (* 6 7))' > hello.clj
$ build/jolt hello.clj
hello 42
```

## Use as a library

```janet
(use jolt/api)

(def ctx (init))
(eval-string ctx "(+ 1 2)")            # → 3
(eval-string ctx "(map inc [1 2 3])")  # → [2 3 4]
```

`(init)` returns a context with `clojure.core` loaded. Each context is isolated; use separate contexts for separate environments.

## Host interop

Jolt exposes CLJS-style host interop through `.` on any Janet table or struct — a field holding a function is called with the receiver as the first argument:

```clojure
(def obj {:greet (fn [self name] (str "Hello " name))})
(. obj greet "Alice")   ; → "Hello Alice"
(.-greet obj)           ; field access (reader sugar for (. obj :greet))
```

Janet's standard library is reachable through `jolt.interop` (and the `jolt.shell` / `jolt.http` helpers built on it):

```clojure
(require '[jolt.interop :as j])
(j/janet-type [1 2])              ; → :tuple
(j/janet-table-keys {:a 1 :b 2})  ; → [:b :a]
```

## Differences from Clojure

Jolt targets Clojure semantics but runs on Janet, not the JVM. The notable divergences:

- **Host platform.** No JVM and no Java interop — `import`, `gen-class`, `proxy` of Java classes, and `java.*` are unavailable. `instance?` recognizes a small set of built-in types (`clojure.lang.Atom`, `Number`, `String`, …).
- **Numbers.** Janet integers and doubles. `(/ 1 3)` is `0.3333…` and large products lose precision. No ratios or `BigDecimal` (`ratio?` is always false, `bigdec` falls back to a double); `bigint`/`biginteger` use Janet's 64-bit `int/s64`, not arbitrary precision. The auto-promoting `+'`/`-'`/`*'`/`inc'`/`dec'` are aliases for the plain ops, since Janet numbers don't overflow. `quot`/`rem`/`mod` follow Clojure's sign rules. The symbolic values `##Inf`/`##-Inf`/`##NaN` read, and `infinite?`/`NaN?` work. Janet represents an integer and an integer-valued double identically, so `1` and `1.0` are indistinguishable: `(float?/double? 1.0)` is `false` and `(int? 1.0)` is `true` — `float?`/`double?` are true only for values with a fractional part or `##Inf`/`##NaN`.
- **Collections.** By default Jolt uses immutable persistent data structures: vectors are 32-way branching tries (structural-sharing persistent vectors with O(log₃₂ n) `conj`/`assoc`/`nth`), lists are persistent singly-linked cons cells (O(1) `conj`/`cons` prepend with structural sharing), and maps/sets are persistent hash structures. Value equality and sequence operations are Clojure-compatible, but hash-map/hash-set iteration order is unspecified and differs from Clojure — use `sorted-map`/`sorted-set` when order matters.
- **Mutable build mode.** Jolt can be compiled to use fast Janet-native *mutable* collections instead, via a build-time flag: `JOLT_MUTABLE=1 jpm build` (default `jpm build` is immutable). In mutable mode vectors and lists share one mutable array representation (so `conj` mutates in place and appends, and `vector?`/`list?` no longer distinguish them) — a performance/looseness trade-off. The default immutable build has full Clojure value semantics.
- **Concurrency / STM.** Single-threaded. No refs, `dosync`, agents, or `send`; `locking` evaluates its body without real locking. Atoms, volatiles, and delays are supported.
- **Regex.** Compiled to Janet's PEG engine (Janet has no regex). Supported: capturing groups (`[whole g1 …]`), greedy and lazy quantifiers with backtracking, `(?:…)`, lookahead `(?=…)`/`(?!…)`, alternation, anchors `^ $ \b \B`, character classes, and the `(?i)` flag. Not supported: lookbehind, backreferences (`\1`), and named groups (`(?<name>…)`).
- **Arrays.** Java-style arrays map onto Janet's native types: `byte-array` is a Janet buffer (contiguous, C-backed); `object-array`/`int-array`/`double-array`/etc. are Janet arrays. `aget`/`aset`/`alength`/`aclone` work over both.
- **Transients.** `transient`/`conj!`/`assoc!`/`dissoc!`/`disj!`/`pop!`/`persistent!` are real mutable scratch collections backed by Janet's native arrays and tables (vectors → arrays, maps/sets → tables), so building a collection with them avoids the per-step copying of the persistent path (notably for maps/sets). `persistent!` freezes back to a persistent value.
- **Not implemented.** JVM reflection, `proxy`, and the `clojure.repl`/`clojure.template` namespaces.

Supported and Clojure-compatible: chars as a distinct type, lazy/infinite sequences, transducers, destructuring, multimethods with hierarchies, protocols/records (`deftype`/`defrecord`/`reify`/`extend-protocol`), metadata, namespaces, and the reader (`#()`, `#_`, `#?`, tagged literals, `#"…"`).

## Test

```
jpm test                                    # full suite (recurses test/)
janet test/spec/sequences-spec.janet        # a single spec
janet test/integration/conformance-test.janet
```

Tests are organized in three layers:

- **`test/spec/`** — the contract. Black-box, behavior-defining tables (one file
  per public API area) that collectively pin down Jolt's defined behavior. This
  is the authoritative description of what Jolt promises.
- **`test/integration/`** — cross-cutting and regression batteries: the Clojure
  conformance suite, SCI bootstrap/runtime loading, jank conformance, the
  cross-dialect [clojure-test-suite](https://github.com/lread/clojure-test-suite)
  (run via a minimal `clojure.test` shim against `~/src/clojure-test-suite`, if
  present, and baseline-guarded), compile-mode tests, the library API, and a
  broad systematic-coverage net.
- **`test/unit/`** — white-box tests for individual components (reader,
  evaluator, types, persistent collections, regex, compiler).

`test/support/harness.janet` provides the shared `defspec` table runner (cases
are `["label" expected actual]`, compared with Jolt's own `=`) plus
`expect=`/`expect-throws` for unit tests.

The syntactic half of the contract — the surface syntax the reader accepts — is
specified as an EBNF grammar in [`doc/grammar.ebnf`](doc/grammar.ebnf), with
Jolt-vs-Clojure deviations noted inline. `test/spec/reader-syntax-spec.janet`
exercises it.

### clojure-test-suite conformance

The [clojure-test-suite](https://github.com/lread/clojure-test-suite) battery
runs ~3900 assertions green. Jolt validates its arguments like Clojure —
arithmetic on non-numbers, comparisons against `nil`, out-of-range indices,
malformed `conj!`/`assoc!`/`merge`, and non-seqable `first`/`seq`/`vec` all
throw. The assertions that remain failing are accounted for by the
platform/design differences above, not by missing behavior:

- **No bignum/ratio/BigDecimal** — `bigint`/`numerator`/`denominator`/`bigdec`,
  the `big-int?`/auto-promotion checks, and the `2N`/`1/2`/`1.0M` literals read
  but don't carry those exact types.
- **Integer/float identity** — Janet represents `1` and `1.0` identically, so
  `quot`/`rem`/`mod`'s `double?`/`int?` result-type assertions and many
  `float?`/`double?` cases can't distinguish them (`(str 0.0)` is `"0"`).
- **64-bit integers / Unicode** — `bit-and` etc. on full-width 64-bit constants
  lose precision (doubles), and `subs`/`count` work on bytes, not code points.
- **Eager seqs** — `map`/`filter`/`range` return vectors, so `seq?`/`vector?`/
  `sequential?` of their results differ, and sorts aren't guaranteed stable.

## License

[Eclipse Public License 1.0](https://opensource.org/licenses/EPL-1.0)
