# Jolt

[![tests](https://github.com/jolt-lang/jolt/actions/workflows/tests.yml/badge.svg)](https://github.com/jolt-lang/jolt/actions/workflows/tests.yml)

A Clojure implementation on [Janet](https://janet-lang.org). Jolt reads Clojure source and, by default, compiles each form to native Janet bytecode — falling back to a tree-walking interpreter for forms the compiler doesn't handle, so results always match the interpreter. It ships a Clojure-compatible standard library. The goal is a Janet-hosted [SCI](https://github.com/borkdude/sci)-style runtime with a minimal bootstrap.

## Build

```bash
git clone https://github.com/jolt-lang/jolt.git
cd jolt
git submodule update --init   # pulls vendor/sci
jpm build                     # builds build/jolt and build/jolt-deps
```

Requires `jpm` and a recent Janet (CI-tested against 1.41). See
[doc/building-and-deps.md](doc/building-and-deps.md) for build details, the
`jpm clean` caveat, how namespaces are resolved (`JOLT_PATH`), and pulling
Clojure libraries from a `deps.edn` with the `jolt-deps` tool.

## Run

```
build/jolt                     # start a REPL
build/jolt file.clj [args]     # run a file (binds *command-line-args* and *file*)
build/jolt -e EXPR [args]      # evaluate EXPR and print the result
build/jolt -m NS [args]        # require NS and call its -main
build/jolt nrepl-server [addr] # start an nREPL server ([host:]port, default 7888)
build/jolt --version           # print the version
build/jolt -h | --help         # help
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

### Evaluation pipeline: interpreted and compiled

Every form passes through one router (`loader/eval-toplevel`) that decides *per
form* whether to tree-walk it or compile it to Janet bytecode. The shipped
runtime **compiles by default**; set `JOLT_INTERPRET=1` to force the interpreter.

**Hybrid, always correct.** The compiler is incomplete by design: a form it can't
compile correctly throws `jolt/uncompilable`, and the router falls back to the
tree-walking interpreter (`eval-form`) for that form. So the result *always*
matches the interpreter — compilation is a transparent speedup, never a semantic
change. Only the compile step is guarded; runtime errors in compiled code
propagate normally (no double-evaluation, no hidden errors).

What compiles: `def`/`defn`, multi-arity / named / variadic fns, `recur` (in
`loop` and directly in `fn`), `let`/`if`/`do`/`try`/`throw`/`quote`, map and
vector literals, and calls. What falls back to the interpreter: context-modifying
and definitional forms (`ns`, `defmacro`, `deftype`, `defprotocol`,
`defmulti`/`defmethod`, `reify`, `require`, `binding`, …), destructuring, regex
literals, and the handful of interpreter-only special forms.

**Live redefinition.** Compiled global references deref through Jolt **var cells**
(Janet early-binds plain symbols, which would freeze redefinition), so redefining
a `def`/`defn` at the REPL is visible to already-compiled callers — Clojure's var
model. Hot numeric primitives (`+ - * < > <= >=`) emit native Janet ops, and
calls compile to direct Janet calls.

```janet
(def ctx (init {:compile? true}))
(eval-string ctx "(defn fib [n] (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))")
(eval-string ctx "(fib 30)")   ; → 832040, native Janet bytecode
```

For compute-heavy code the compiled path is dramatically faster than tree-walking,
at native Janet speed.

**Validated at parity.** The conformance suite passes 218/218 under *both*
interpreter and compiler (`conformance-test.janet` runs both in CI), and the full
clojure-test-suite under compilation matches the interpreter baseline across
~4.6k assertions — evidence the hybrid path doesn't diverge.

**AOT.** `aot.janet` marshals a compiled namespace to a Janet bytecode image
(`save-ns`) and loads it back into a fresh context (`load-ns-image`), skipping
parse/analyze/emit/compile on reload. Core fns are referenced by name against the
baked-in runtime; only user bytecode and var cells are serialized.

## Host interop

Jolt exposes CLJS-style host interop through `.` on any Janet table or struct — a field holding a function is called with the receiver as the first argument:

```clojure
(def obj {:greet (fn [self name] (str "Hello " name))})
(. obj greet "Alice")   ; → "Hello Alice"
(.-greet obj)           ; field access (reader sugar for (. obj :greet))
```

### The `janet` interop bridge

The whole Janet standard library is reachable from Clojure through an explicit
`janet` namespace segment, which marks every crossing into host code (where
Clojure semantics no longer hold):

```clojure
(janet.os/clock)                  ; → a Janet module fn:  os/clock
(janet.string/join ["a" "b"] ",") ; → janet `string/join`  (NB: takes a Janet
                                  ;    tuple, not a Jolt vector — convert first)
(janet/slurp "deps.edn")          ; → a Janet root builtin: slurp
(janet/type [1 2])                ; → :table
```

The rule is `janet/<name>` for a Janet root binding and `janet.<module>/<name>`
for a module binding. Because the boundary is explicit, you can tell at the call
site that a form drops into the host — and that values cross the boundary as
their Janet representations (a Jolt vector is a Janet table, etc.), so a Janet
function expecting a tuple needs an explicit conversion. The `jolt.interop`,
`jolt.shell`, and `jolt.http` namespaces are thin Clojure wrappers built on this.

This bridge is what makes networking (and everything else in Janet's stdlib)
available to ordinary Clojure — for example, `jolt.nrepl` (below) is plain
Clojure over `janet.net/*`.

```clojure
(require '[jolt.interop :as j])
(j/janet-type [1 2])              ; → :tuple
(j/janet-table-keys {:a 1 :b 2})  ; → [:b :a]
```

## nREPL

Jolt ships an [nREPL](https://nrepl.org) server and client (`jolt.nrepl`),
written in Clojure on top of the `janet.net/*` bridge. Start a server from the
CLI — it writes `.nrepl-port` so editors (CIDER, Calva, …) auto-connect:

```bash
jolt nrepl-server               # listen on 127.0.0.1:7888, write .nrepl-port
jolt nrepl-server 12345         # choose a port
jolt nrepl-server 0.0.0.0:12345 # choose host and port  (alias: nrepl)
```

Supported ops: `clone`, `describe`, `eval`, `load-file`, `close`, `ls-sessions`,
`interrupt` (acknowledged; an in-flight eval can't actually be interrupted), and
`eldoc`. `eval` streams `out`, reports the current `ns`, evaluates each form in
the message, and returns an `eval-error` status (the session stays usable) on
failure. One Jolt runtime backs the server and sessions share it, so `def`s
persist across a connection like a normal dev REPL.

It's also usable as a library — embed a server, or drive another nREPL as a
client:

```clojure
(require '[jolt.nrepl :as nrepl])
(def server (nrepl/start-server! {:port 7888}))
;; ... later ...
(nrepl/stop-server! server)

(def c (nrepl/connect {:port 7888}))
(def session (nrepl/client-clone c))
(nrepl/client-eval c "(+ 1 2)" session)  ; → responses incl. {"value" "3"}
(nrepl/client-close c)
```

## Differences from Clojure

Jolt targets Clojure semantics but runs on Janet, not the JVM. The notable divergences:

- **Host platform.** No JVM and no Java interop — `import`, `gen-class`, `proxy` of Java classes, and `java.*` are unavailable. `instance?` recognizes a small set of built-in types (`clojure.lang.Atom`, `Number`, `String`, …).
- **Numbers.** Janet integers and doubles. `(/ 1 3)` is `0.3333…` and large products lose precision. No ratios or `BigDecimal` (`ratio?` is always false, `bigdec` falls back to a double); `bigint`/`biginteger` use Janet's 64-bit `int/s64`, not arbitrary precision. The reader still accepts Clojure's numeric literal syntaxes — the BigInt/BigDecimal suffixes (`42N`, `1.5M`), ratios (`1/2`), radixed integers (`2r1010`, `16rFF`), and exponents (`1e3`) — but reads them as plain Janet numbers (a ratio becomes its double quotient). The auto-promoting `+'`/`-'`/`*'`/`inc'`/`dec'` are aliases for the plain ops, since Janet numbers don't overflow. `quot`/`rem`/`mod` follow Clojure's sign rules. The symbolic values `##Inf`/`##-Inf`/`##NaN` read, and `infinite?`/`NaN?` work. Janet represents an integer and an integer-valued double identically, so `1` and `1.0` are indistinguishable: `(float?/double? 1.0)` is `false` and `(int? 1.0)` is `true` — `float?`/`double?` are true only for values with a fractional part or `##Inf`/`##NaN`.
- **Collections.** By default Jolt uses immutable persistent data structures: vectors are 32-way branching tries (structural-sharing persistent vectors with O(log₃₂ n) `conj`/`assoc`/`nth`), lists are persistent singly-linked cons cells (O(1) `conj`/`cons` prepend with structural sharing), and maps/sets are persistent hash structures. Value equality and sequence operations are Clojure-compatible, but hash-map/hash-set iteration order is unspecified and differs from Clojure — use `sorted-map`/`sorted-set` when order matters.
- **Mutable build mode.** Jolt can be compiled to use fast Janet-native *mutable* collections instead, via a build-time flag: `JOLT_MUTABLE=1 jpm build` (default `jpm build` is immutable). In mutable mode vectors and lists share one mutable array representation (so `conj` mutates in place and appends, and `vector?`/`list?` no longer distinguish them) — a performance/looseness trade-off. The default immutable build has full Clojure value semantics.
- **Concurrency / STM.** No refs, `dosync`, agents, or `send`; `locking` evaluates its body without real locking. Atoms, volatiles, promises, and delays are supported.
- **Futures.** `future` runs its body on a *real* OS thread (Janet's `ev/thread`), so it can use a second core for CPU-bound work — unlike the cooperatively-scheduled `go` blocks. `deref`/`@` parks until the result is ready (with the optional `(deref f timeout-ms timeout-val)` arity); `future?`, `future-done?`, `realized?`, `future-cancel`, and `future-cancelled?` are supported. Two important divergences from the JVM: (1) **snapshot semantics** — Janet threads have separate heaps, so the body and the state it closes over are *copied* to the worker thread and only the return value is copied back; mutating a captured atom does not propagate to the parent (communicate via the return value). (2) **no thread interruption** — Janet OS threads can't be cancelled mid-run, so `future-cancel` marks the *future* cancelled (deref then throws and the predicates flip) but the underlying computation still runs to completion in the background. As on the JVM, a live future thread keeps the process alive until it finishes (the JVM's non-daemon future pool behaves the same).
- **core.async.** `clojure.core.async` runs on Janet fibers and channels (`chan`, `go`, `go-loop`, `<!`/`>!`/`<!!`/`>!!`, `close!`, `alts!`, `timeout`, `put!`/`take!`, `buffer`/`dropping-buffer`/`sliding-buffer`, and channel transducers via `(chan n xform)`). Because Janet fibers are stackful coroutines, a `go` block is just its body run in a fiber — no CPS/state-machine rewrite — so `<!`/`>!` work *anywhere*, including inside `try`, nested `fn`s, and loops (positions Clojure's `go` macro forbids). Go blocks are cooperatively scheduled on one OS thread, so parking (`<!`) and blocking (`<!!`) coincide; `thread` runs cooperatively too. Dynamic-var bindings are conveyed into `go` blocks (each go block sees the bindings in effect when it was spawned).
- **Regex.** Compiled to Janet's PEG engine (Janet has no regex). Supported: capturing groups (`[whole g1 …]`), greedy and lazy quantifiers with backtracking, `(?:…)`, lookahead `(?=…)`/`(?!…)`, alternation, anchors `^ $ \b \B`, character classes, and the `(?i)` flag. Not supported: lookbehind, backreferences (`\1`), named groups (`(?<name>…)`), and Unicode property classes (`\p{Lu}`).
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
  cross-dialect [clojure-test-suite](https://github.com/jank-lang/clojure-test-suite)
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

The [clojure-test-suite](https://github.com/jank-lang/clojure-test-suite) battery
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
