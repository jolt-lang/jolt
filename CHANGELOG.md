# Changelog

All notable changes to this project are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.4.1] - 2026-07-17

Correctness patch: the first round of a focused compiler review (correctness and
architecture), plus two loader fixes surfaced while running a real dependency
tree. Every behavioral change is regression-tested and, where it only shows in an
optimized build, pinned by a `--opt` build-smoke assertion.

### Fixed

- **`nil?`/`some?` folded to the wrong constant in optimized builds.** When
  inference proved a value nil, `jolt build --opt` folded `(nil? x)` to `false`
  and `(some? x)` to `true` — inverted — so an `if`/`if-some`/`when-some` gated
  on it took the wrong branch in a release binary (dev/interpreted mode was
  unaffected). The fold now matches: nil? true, some? and every type predicate
  false.
- **A `loop` that rebinds a record-typed outer local crashed under `--opt`.** The
  inference pass left the loop variable with the outer local's record type, so a
  slot read like `(:x p)` devirtualized to a raw record access and blew up when
  the loop actually carried something else — the common
  `(let [x (init)] (loop [x x] … (recur (f x))))` shape. Loop variables (and a
  `(fn f …)` self-reference) now correctly shadow the outer binding during
  inference.
- **`min`/`max` returned a float where they should return the original operand.**
  `(min 2.5 1)` returned `1.0` in optimized/release builds instead of `1` (dev
  gave `1`). Double contagion no longer applies to `min`/`max`, which return an
  argument unchanged.
- **`clojure.math` and `Math/*` leaked complex numbers.** Out-of-domain real
  inputs returned a Chez complex — `(Math/sqrt -1.0)` gave `0.0+1.0i` — instead
  of `##NaN`. `sqrt`, `pow`, `log`, `log10`, `log1p`, `asin`, and `acos` now
  return `##NaN` off their real domain, matching Java; in-domain results and
  `##NaN`/`##Inf` are unchanged.
- **`compare-and-set!`, `swap-vals!`, and `reset-vals!` were not atomic.** The
  overlay redefined them as check-then-act compositions that lost updates under
  real threads (futures/agents), shadowing the atomic mutex/CAS implementations.
  The atomic versions are restored.
- **Any missing namespace crashed with an opaque error.** `require` of a
  namespace with no source file raised `incorrect number of arguments 3 to
  throw-jvm` instead of a catchable `FileNotFoundException` naming the file —
  a stray argument in the loader's not-found path.
- **A failed nested `require` was blamed on the wrong file.** The reported source
  location pointed at the last form of a dependency that had just loaded
  successfully, not the `ns` form that issued the failing require. The loader now
  restores the source position after each nested load.

## [0.4.0] - 2026-07-17

Strict-resolution and default-fast-builds release: a five-dimension audit
(architecture, dead code, duplication, correctness, performance) followed by
two implementation waves (PRs #376–#388), every behavioral change certified
against reference JVM Clojure 1.12.5.

### Changed

- **Unresolved symbols are compile errors.** Top-level and operator-position
  references to undefined symbols throw "Unable to resolve symbol" at analyze
  time in every entry path (`-e`, files, `run`, built binaries) instead of
  silently producing unbound-var values that pattern-matched as truthy. Fn
  bodies still auto-declare (matching JVM-with-`declare` semantics); the nREPL
  path keeps late binding for interactive redefinition.
- **`-e` and file loading evaluate one top-level form at a time**, like the
  JVM: a `require` in one form is visible to the reader and analyzer of the
  next, so `joltc -e "(require '[x :as a]) ::a/k"` resolves. As a CLI
  convenience, `-e` auto-quotes `require`/`use` vector args.
- **Plain `jolt build` now direct-links and runs whole-program inference** —
  measured 2.5x on cross-namespace call loops with no flags. A plain `def` is
  frozen in the binary; `^:redef`/`^:dynamic` defs stay var-routed so runtime
  redefinition and `binding` keep working. Opt out with `--no-direct-link`,
  `--dev`, deps.edn `:jolt/build {:direct-link false}`, or
  `JOLT_NO_WP_INFER=1` for the inference fixpoint alone.
- **Vars are non-dynamic unless marked**, like the JVM: `binding` a var
  without `^:dynamic` metadata throws, `set!` of a dynamic var with no thread
  binding throws instead of mutating the root, and `(def ^:dynamic *x*)`
  declares bindable. All runtime-defined dynamic vars (`*out*`, `*1`,
  print flags, `&form`/`&env`, …) carry the tag.
- **`import` is a macro** (its specs are never evaluated, so bare
  `(import [java.nio.file Path])` works under strict analysis) and binds host
  class short names to class values; built binaries now run their `:import`
  clauses (they previously never did). `defmulti` interns its var at analysis
  like the JVM, so a reference later in the same form resolves.
- Errors across the runtime throw **typed JVM exceptions** — 79 sites that
  raised untyped host conditions are catchable by class:
  `(catch NoSuchElementException …)` for iterator exhaustion,
  `FileNotFoundException` for missing requires, `NumberFormatException`,
  `IndexOutOfBoundsException`, `ClassCastException`, `IllegalStateException`,
  `ArityException`, and friends, all oracle-verified. Broad
  `(catch Exception …)` continues to work everywhere.

### Added

- `jolt.deps/add-deps`: resolve an inline `:deps` map (git / local / Maven
  coordinates) at runtime and add the roots to the loader — the
  `babashka.deps/add-deps` idiom, detection included:
  `(when (System/getProperty "jolt.version") ((requiring-resolve 'jolt.deps/add-deps) '{:deps {…}}))`.
- `*jolt-version*` and `(System/getProperty "jolt.version")`: the release tag
  baked into binaries (else `git describe`, else `"dev"`); never nil under
  jolt, so it doubles as am-I-on-jolt detection.
- **Maven/gitlibs cache sharing with the JVM toolchain**: jars live at their
  standard `~/.m2/repository` paths (bidirectional reuse with clj);
  `:mvn/local-repo` in deps.edn relocates the repository like tools.deps,
  `JOLT_LOCAL_REPO` overrides from the environment; git deps reuse existing
  tools.gitlibs checkouts read-only and honor `$GITLIBS` for cache placement.
- Dev boot cache: `make devboot` precompiles the runtime so source-mode
  `bin/joltc` starts in ~0.3s instead of ~1.5s, with automatic staleness
  fallback. `jolt.main`/`jolt.deps` are AOT'd into the `joltc` binary
  (CLI commands no longer recompile them per invocation).
- Multimethods memoize isa?-resolved dispatch (invalidated by `defmethod`,
  `remove-method`, `prefer-method`, and hierarchy changes) with fixed-arity
  fast paths.

### Fixed

- for/doseq: `:while` can reference a preceding `:let` binding, and modifiers
  nest in written order (`:when`-skipped elements never reach a later
  `:while`).
- `extends?` sees inline `defrecord`/`deftype` protocol implementations
  without polluting `(extenders P)`; `select-keys` preserves metadata.
- Reader: `::alias/kw` with an unknown alias throws Invalid token instead of
  silently minting the wrong keyword; `\backspace`/`\formfeed` round-trip
  through pr; `*print-readably*` and `*print-namespace-maps*` are honored.
- `format`: unknown directives throw instead of emitting literal text while
  consuming the argument; `%s` renders nil as `"null"`.
- Deref of a failed future throws `ExecutionException` wrapping the original
  as its cause. `clojure.string/split` on zero-width matches splits between
  characters. Bit-shift counts mask to 6 bits; unary `bit-and`/`bit-or`/
  `bit-xor` throw `ArityException` (the raw variadic host primitives no
  longer leak through value positions). `(keyword 5)` returns nil. Var meta
  `:name` is a symbol. Transient read ops throw after `persistent!`.
  `System/gc` never throws (a guarded no-op while threads are active).
- Records: `assoc`/`dissoc` build the new record with one allocation and
  direct slot reads (~28% faster); `with-meta` on vectors shares structure
  (O(1), ~173x on kilo-element vectors); string hashing drops a per-hash
  UTF-16 allocation and caches like `String.hashCode` (18x on string-keyed
  lookups); bignum hashes are JVM-exact int32.
- Whole-program inference: the per-namespace IR cache stayed aligned past
  macro forms — under `--opt` every form after the first macro in a namespace
  had been silently compiled from the next form's IR, corrupting macro
  expanders.
- Startup/memory: embedded sources ship as UTF-8 bytevectors (~10MB steady
  RSS); the `joltc` boot GC peak is tunable; `ns-has-vars?` is O(1);
  stdout/stderr flush on every exit path (output no longer lost when the
  process exits while helper threads wind down).

### Internal

- The conformance corpus gate asserts `:expected :throws` rows raise on jolt
  and gates the crash bucket against an exact-label baseline; `jolt build`
  brackets each baked namespace with RT.load-parity compiler-var bindings;
  `jolt-load-string` no longer leaks a binding frame when the loaded source
  throws; the tree-shaker recognizes metadata-carrying defs as prunable and
  roots `global-hierarchy`.

## [0.3.3] - 2026-07-16

Full-codebase audit release: seven review rounds plus follow-ups (PRs
#362-#373), every behavioral fix certified against reference JVM Clojure.

### Fixed

- Build: `--embed` resources are baked into the binary at build time (shipped
  binaries no longer re-read build-machine paths at startup); tree-shaking is
  sound for redefined vars (duplicate-fqn refs union); binary namespace roots
  derive from the require graph, so namespaces loaded before the build hook
  (data-reader helpers and their requires) ship correctly.
- core.async: `alts!`/`alts!!` use handler registration — an alts put and an
  alts take on an unbuffered channel rendezvous instead of livelocking, and
  blocked alts no longer busy-poll. Fixed-buffer channels with transducers get
  real backpressure; pending rendezvous puts park through `close!` until a
  taker consumes their value (JVM-verified); `timeout` channels share one
  timer thread.
- Hashing: record hashes are JVM-exact defrecord hasheq (a bignum overflow
  into unchecked fixnum ops previously made equal values hash differently,
  nondeterministically); collections cache their hasheq lazily; vector/map/
  set hashes are value-identical to the JVM.
- Inference soundness: a `reduce` accumulator seeded `:double` no longer
  forces a coercion crash on nil-returning reducers; same-named records in
  different namespaces resolve exactly instead of by suffix; user `^double`
  hints survive HOF seeding; locals named like runtime identifiers
  (`jolt-nil`, `fl+`, …) are munged instead of shadowing.
- Reader/regex: mid-pattern `(?i)` applies to the remainder and `(?-i)`
  actually removes flags; `\Q…\E` quantifier scope, strict `\p{…}`, Java
  octal escapes, possessive quantifiers as atomic groups; radix-aware `N`
  literals (`042N` ⇒ `34N`); positioned EOF errors in string escapes;
  top-level `#?@` throws; record literals construct records; `#!` is a
  to-EOL comment (clojure reader only — EDN rejects it); syntax-quote
  resolves through the full alias/refer/core chain (`` `map `` ⇒
  `clojure.core/map`); core macros resolve as vars with `:macro` meta.
- clojure.test: `(is (instance? C x))` actually asserts; every assertion
  dispatches through the `report` multimethod; interned `:test`-meta tests
  run inside `:once` fixtures.
- Destructuring `:or` defaults are `get`'s not-found argument (JVM-exact:
  eager, sibling bindings in scope) — `{:or {b (inc a)}}` no longer throws.
- Laziness: `not-empty` uses `seq` (no more hanging on infinite seqs);
  `pmap` is semi-lazy with bounded look-ahead; `pprint` honors
  `*print-length*` by stopping; `when-first` tests the seq.
- Java compat: `io/copy` between files copies bytes (binary files no longer
  corrupted through a UTF-8 round-trip); deleting a non-empty directory
  throws/returns false; parsed timezone offsets apply; FQN and short class
  names share one statics table; bitwise `Math/getExponent`;
  `awaitTermination` actually waits; `ReentrantLock` is reentrant;
  interruptible bodies unwind their timers; `getAbsoluteFile` shares
  `getAbsolutePath`'s base; `(System/getenv)` reads the environment directly
  (multi-line values intact); shared counters and caches are mutex-guarded;
  string `index-of`/`last-index-of` from-args clamp like the JVM; `assert`
  messages evaluate at failure time.
- Memory: caught exceptions no longer root the captured continuation (a
  catch-complete hook clears it after the handler finishes; traces intact).
- On hosts with an unverified `struct stat` layout (e.g. aarch64 Linux),
  `getPosixFilePermissions`/`getOwner` throw a clear
  `UnsupportedOperationException` instead of reading garbage.

### Changed

- The whole-program shake's hand-maintained name lists are gate-verified; the
  17 run-gate scripts share one harness; `--opt` builds reuse the
  whole-program pass's analysis at emission; one mode→Chez-parameters table;
  the layered `Files` registrations collapse to one block per class; dead
  code across the runtime removed (shakesmoke byte-identity verified).
- The long-only integer boxing model is documented as the SPEC feature
  `:numerics/long-only` (`(short x)` range-checks but boxes as Long).

### Performance

- `subseq`/`rsubseq` seek from the comparator bound and walk lazily
  (O(log n) instead of materializing the collection); string scans stop
  allocating per candidate offset (`.indexOf` −20%, `replace` −12%);
  regex literals compile once per source string (~30× on literal-in-loop
  patterns); collection-as-map-key lookups no longer rehash O(n) per probe.

## [0.3.2] - 2026-07-15

### Changed

- Built binaries use roughly a third less memory. The launcher registers the
  appended boot image as a region of the executable (read through a file
  descriptor at startup) instead of holding a resident copy — 7–14 MB less
  depending on the app, on every platform. Tree-shaken binaries with no runtime
  eval now boot from `petite.boot` alone, dropping the bundled Chez compiler:
  another ~5 MB of memory and ~1 MB of binary size (macOS/Linux). A hello world
  goes from ~34 MB to ~22 MB resident; default (REPL-capable) builds keep the
  compiler and still save the boot copy.

## [0.3.1] - 2026-07-14

### Added

- Map destructuring follows Clojure 1.13.0-alpha4: idents after `&` in
  `:keys`/`:syms`/`:strs` (and the `!` variants) are keys, not binding symbols;
  `:or` accepts key→val entries; `:defaults name` binds a map of the resolved
  defaults; `:select name` binds a map of the mentioned keys, filled from `:or`
  and selecting deeply through nested map patterns. Adds `some-vals`.

### Fixed

- `(. Class staticMethod args)` now dispatches statically for the value classes
  (`Long`/`Integer`/`String`) and any registered/fully-qualified class, matching
  the `Class/staticMethod` slash form.
- `load-string` and `eval` handle source containing reader literals (`#inst`,
  `#uuid`, `#"regex"`): `load-string` reads raw forms like file loading, and
  `eval` self-evaluates opaque host values built by `read-string`.

## [0.3.0] - 2026-07-14

### Changed

- **Breaking:** `java.time.*` is no longer built into core — it is the
  [jolt-lang/time](https://github.com/jolt-lang/time) library. The full surface
  (`LocalDate`/`Time`/`DateTime`, `Instant`, `ZonedDateTime`, `OffsetDateTime`,
  `Duration`, `Period`, `Year`/`YearMonth`, zones with DST, `DateTimeFormatter`)
  is now portable Clojure over the value-semantics seams below, with
  [juxt/tick](https://github.com/juxt/tick) on top; tick's full suite passes. A
  program using `java.time.*` must depend on the library. Core keeps the `#inst`
  / `java.util.Date` layer and the libc zone/locale primitives (`tz-primitives`).

### Added

- `jolt.deps` resolves Maven coordinates. A Clojure library's Maven JAR carries
  its `.clj`/`.cljc` source, so a `:mvn/version` dep — including one pulled in
  transitively (tick declares its deps as Maven) — is fetched from Clojars/Central,
  extracted, and its `pom.xml` read for further transitive deps, with no JVM.
  Skips test/provided/optional deps, pure-Java or ClojureScript-only artifacts,
  and the clojurescript toolchain.
- Core value-semantics seams a library uses to give its own host values full
  Clojure semantics: `__register-eq!` / `__register-hash!` / `__register-str!` /
  `__register-pr!` / `__register-compare!`, and `__register-class!` so those
  values answer `class`/`type` and dispatch protocols extended to their class.
- `jolt.host/set-instant-ctor!` — the `#inst`/`Date` layer's `.toInstant` yields
  a library-owned instant, so `Date` and a library `Instant` are one representation.
- `java.util.Date` is now `Comparable` (`compareTo` / `clojure.core/compare`).

## [0.2.8] - 2026-07-13

### Added

- `jolt.fs` is now the [babashka.fs](https://github.com/babashka/fs) API. Jolt
  vendors babashka.fs over a new `java.nio.file` host shim — `Path`, `Files`,
  `FileTime`, file attributes, POSIX permissions, symbolic links, and directory
  walking with symlink-cycle detection. `jolt.fs` re-exports it as the public
  surface (require `babashka.fs` directly if you prefer). Symbolic links,
  creation time, and permissions — which the previous `java.io.File`-based
  `jolt.fs` could not do — now work through the shim's `stat`, `realpath`,
  `symlink`, `chmod`, and `getpwuid` bindings.
- A `java.nio.file` interop surface: `Paths`/`Path`, `Files` (predicates,
  create/delete/copy/move, read/write, temp files, `walkFileTree`,
  `newDirectoryStream`, attributes), `FileTime`, `PosixFilePermissions`,
  `FileVisitor`/`FileVisitResult`, and the `LinkOption`/`CopyOption`/`OpenOption`
  enums.
- `jolt.util/import-vars` — re-export a namespace's public vars as bakeable
  delegating definitions (functions and macros, with an `:exclude` set). The
  pattern for putting a public face on a vendored library; how `jolt.fs` wraps
  babashka.fs. Works in an AOT-built binary, unlike an `intern` over
  `ns-publics`.

### Fixed

- A built binary now includes a namespace's forms that follow a non-matching
  reader conditional. The AOT emission reader stopped at the first `#?(:cljs …)`
  (with no `:clj` branch), silently dropping every later `def` — so an AOT-built
  app crashed on an unbound var when it called one. This surfaced with
  babashka.fs (many cljs-only conditionals); a build-smoke fixture now builds a
  binary that uses the vendored library and checks it runs.

### Changed

- The documentation moved to the site ([jolt-lang.github.io](https://jolt-lang.github.io));
  the repo `docs/` folder is gone and the README links to the live pages.

### Notes

- `zip`/`unzip`/`gzip`/`gunzip` need `java.util.zip`, which Jolt does not shim
  yet, so those babashka.fs functions are excluded from `jolt.fs`.

## [0.2.7] - 2026-07-13

### Fixed

- `read-string`/`read` expand a syntax-quote at read time, like the JVM reader:
  `` (read-string "`(a ~b c)") `` returns the `(seq (concat (list 'ns/a) …))`
  form with symbols namespace-qualified against `*ns*` and auto-gensyms shared
  within a form, instead of a raw `(syntax-quote …)`. (edn and tools.reader are
  unaffected.)
- A qualified or aliased trailing-dot constructor — `(some.ns/Type. args)` or
  `(alias/Type. args)`, as SCI builds `sci.impl.types/Reified.` — now
  constructs the cross-namespace deftype instead of erroring "Unknown class
  \<ns\>"; a namespaced head never reached the constructor path before.

### Added

- The joltc CLI runs a bare file: `joltc FILE` (the `run` subcommand is now
  optional, like bb), and a `FILE` of `-` reads the program from stdin — so
  `joltc run -`, `joltc FILE`, and `joltc -` all work with piped input. A token
  that isn't a file still resolves as a deps.edn `:tasks` entry.

## [0.2.6] - 2026-07-13

### Fixed

- `defmacro` re-heads its generated expander with `clojure.core/fn`, not a bare
  `fn`, so a macro *named* `fn` — like prismatic/schema's `s/fn`, whose namespace
  does `:refer-clojure :exclude [fn]` — no longer resolves `fn` to the
  half-defined macro and fail at load with "Don't know how to create ISeq from:
  :object". Fixed in both the spine and the analyzer.
- `(class x)` returns a real class rather than the `:object` fallback for a few
  values whose class wasn't registered, so using one where a cast or `seq` is
  expected now reports the JVM's message: an unbound var value is
  `clojure.lang.Var$Unbound` (an exact match — the JVM throws the same
  `ClassCastException` for `(def x (+ x 1))`); a `reify` is a stable
  `clojure.lang.IObj$reify__0` placeholder (its JVM name is an unreproducible
  per-eval `ns$eval$reify__N`); `promise`/`future` match the JVM's stable
  enclosing-fn prefix, `clojure.core$promise$reify__0` / `$future_call$reify__0`.

### Added

- `resolve` gets the 2-arg `(resolve &env sym)` arity (nil when `sym` is a local).
- A `deftype`/`defrecord` type token (its constructor closure) is a full class
  value: `class?` is true, it carries `java.lang.Class` dispatch tags, `instance?`
  works when it's passed by value, and `.getName`/`.getSimpleName` answer off its
  tag. A named fn reports its own `ns$name` class plus `AFunction`/`IFn` tags —
  so a protocol extended to a Class value or a specific fn's class dispatches.
  (These, with `clojure.lang.MultiFn` `.addMethod` interop, `with-test`, and an
  `IdentityHashMap` shim, are what let prismatic/schema load, compile, and run.)
- The joltc CLI reads from stdin: `joltc -` runs a program read from stdin as a
  script, `joltc -e -` reads the expression from stdin; both set
  `*command-line-args*` from the trailing argv.

## [0.2.5] - 2026-07-12

Driven by running more libraries: camel-snake-kebab and clj-rss now pass their
suites, claxon passes its byte-parsing tests, and pretty passes four of its six
test namespaces. clj-rss runs over a new `clojure.data.xml` emitter shipped in
[jolt-lang/xml](https://github.com/jolt-lang/xml) v0.0.2.

### Fixed

- A char value reports `java.lang.Character` for protocol dispatch, so a
  protocol extended to `Character` matches a char. It reported nothing, so
  `(extend Character …)` never dispatched (camel-snake-kebab's separator split).
- Record literals `#pkg.Record{…}` read their map/vector values as data, like
  the JVM: `#user.Foo{:content ("a" "b")}` keeps the list instead of evaluating
  it as a call, while a nested record literal is still constructed.
- `(set! (.field obj) v)` compiles, matching `(set! (.-field obj) v)` — an
  instance-field write via the `.name` form was rejected.
- A chained numeric comparison with a `^long`/`^double` operand,
  `(<= 0x21 value 0x7e)`, expands to `(and (op a b) (op b c))` — the fast binary
  op received three arguments and emitted invalid code.
- `(String. bytes offset length charset)` decodes the requested slice; it
  decoded the whole array, ignoring offset/length.

### Added

- Clojure 1.12 qualified instance-method syntax `(ClassName/.method target
  args…)`, lowering to `(.method target args…)`.
- `clojure.lang.Compiler/CHAR_MAP` (the munge map).
- `java.util.WeakHashMap`, `java.util.Collections` (synchronized/unmodifiable/
  empty views), and `java.util.concurrent.atomic.Atomic{Reference,Integer,Long,
  Boolean}`.
- `java.util.concurrent.ExecutorService` / `Executors` backed by a real task
  queue and worker threads — a single-thread executor runs tasks strictly FIFO.
- `java.util.concurrent.locks.ReentrantLock`, `java.net.URI` `getUserInfo`,
  `System/console`/`lineSeparator`, `java.lang.Byte/toUnsignedLong`, and
  `java.nio.ByteBuffer` `slice` plus absolute/relative single-byte `get`.

## [0.2.4] - 2026-07-11

### Fixed

- Destructuring a rest pattern positionally walks the seq like the JVM:
  `(let [[[k v] & ks] a-map] …)` bound `k`/`v` to nil because the positional
  elements read `(nth coll i nil)` even when `&` is present. This silently
  broke `clojure.spec.alpha`'s `keys` conform — `s/valid?` accepted maps whose
  nested key specs failed.
- `empty?` is seq-based like the reference implementation: any seqable value
  answers (including the `java.util` collection shims) and a non-seqable
  raises `IllegalArgumentException` instead of an opaque host error.
- A deftype declaring a `clojure.lang` collection interface now matches the
  JVM at both ends: `instance?`/`map?`/`coll?`/`associative?` answer through
  the declared interface and its ancestry, and calling a declared-but-
  unimplemented method throws `AbstractMethodError` instead of falling back to
  the bare-deftype fields-as-map behavior.
- `inst?` is a real instance check covering `java.util.Date`, its `java.sql`
  subclasses, and `java.time.Instant` — the old tagged-map probe crashed on
  sorted collections and missed `Instant`.
- Throwables and reader conditionals no longer leak their internal map
  representation through `map?`/`coll?`/`ifn?`/`seqable?`/`instance? IObj`.
- Java regex hex and unicode escapes (`\xHH`, `\x{…}`, `\uHHHH`) translate to
  their characters before reaching the regex engine, which mis-parsed them.
- `keys`/`vals` accept any seq of map entries — `(keys (filter pred a-map))`
  works like `RT.keys`.
- A transient carries its source map's representation: an array map round-trips
  through `transient`/`persistent!` as an array map and reports
  `TransientArrayMap`; a hash map stays hash-ordered (previously everything
  came back in array mode).
- The `instance?` macro evaluates a var or local holding a Class value —
  `(def c (class x)) (instance? c y)` works — and `class?` recognizes Class
  values instead of always returning false.
- `clojure.pprint`'s cl-format engine: parametrized directives (`~5A`, `~2{`,
  `~20<`, …) rejected their own parameters, and a forward `~n@*` goto never
  moved. Both fixed, and the missing `~F`, `~$`, `~C`, `~R` (radix/roman), and
  `~(` case-conversion directives are implemented, so `(cl-format nil "~,2f" x)`
  and friends work. A JVM-certified subset of the upstream cl-format suite now
  runs as a standing gate.

### Added

- The JVM class model fills out across the board, driven by running type-
  introspection libraries (lasertag, expound, fireworks all pass or reach
  their documented ceilings): ~20 exception/error constructors with hierarchy
  placement, `java.util.ArrayDeque` and `HashSet`, `(class x)` for the
  `java.time` values, Agent/Volatile/Var/Delay/MultiFn/ReaderConditional/
  MapEntry, sorted and transient collections and hash-mode maps, JVM-shaped
  function class names and the `#object[…]` printed form, `Matcher`
  `.start`/`.end`, `String` `.repeat`/`.isBlank`, `getDeclaredFields`
  reflection over modeled types, a minimal `DateTimeFormatterBuilder`, and a
  `clojure.main` namespace with `demunge`.
- `clojure.test/*testing-contexts*` is a real bindable dynamic var and
  `testing` binds it; `testing-contexts-str` added.

### Changed

- Small sets preserve insertion order through the same array-mode backing that
  small map literals use (past 8 elements they go hash-ordered), so sets and
  maps share one deterministic iteration story. The `java.util` HashMap and
  HashSet shims iterate in insertion order too.
- Record fields fed a mix of integers and floats (`:num`) unbox in protocol-impl
  arithmetic at monomorphic call sites: whole-program builds emit a
  flonum-specialized clone per eligible impl (a `:num` field read beside a
  proven-double operand, where Clojure double contagion already fixes the
  result type), and devirtualized call sites resolve the clone while
  megamorphic dispatch keeps the shared impl. Mono-dispatch ~9% faster;
  results are bit-identical.
- Proven numeric sites and the protocol inline cache's warm-hit scan compile
  to Chez's per-site unsafe primitives (`#3%fl*`, `#3%vector-ref`): the type
  and bounds checks they skip are exactly the ones the compiler already
  proved redundant, so semantics are unchanged while megamorphic protocol
  dispatch gets ~4% faster. Checked `^long` arithmetic keeps its raising
  overflow behavior — fixnum ops are never emitted unsafe.
- `(double x)`, `(long x)`, `(int x)`, and `(float x)` casts feed the typed-
  arithmetic fast path the way `^double`/`^long` hints do: `(* (double x) 2.0)`
  compiles to flonum ops. The casts keep their full checked semantics
  (ClassCastException on a non-number, `(long ##NaN)` is 0, int range
  enforced), so they are a portable escape hatch where inference can't prove
  a type.
- BigDecimal literals follow JVM double contagion in compiled arithmetic:
  `(+ 1.5M 2.0)` is 3.5 (a Double) on the flonum fast path. Mixed
  bigdec/double expressions with non-literal bigdecs keep the generic
  (already correct) path.

- Whole-program builds infer record field types from the constructor
  arguments: a field every `(->Ctor …)` site fills with a flonum reads as a
  double (arithmetic over it unboxes, through protocol-method returns and
  reduce accumulators), and a field holding a record-or-nil narrows guarded
  reads to the direct accessor. No hints needed; conflicting or escaping
  constructors soundly leave fields untyped.

## [0.2.3] - 2026-07-11

### Fixed

- Release and optimized builds compile at Chez optimize-level 2, not 3 — level
  3 is unsafe mode (fx/fl/car operations skip their type checks) and jolt's
  error semantics depend on those raising: an optimized binary returned
  `(take nil coll)` instead of throwing and looped forever on a nil-count
  `repeat`. Costs ~8-13% on dispatch/allocation benchmarks, nothing on
  numeric ones.
- The standalone `joltc` binary's `-e` matches the script driver: trailing
  args bind `*command-line-args*`, the first `--` ends option parsing, and an
  uncaught throw reports its source location. Both entry points now share one
  dispatch (`cli-core.ss`), guarded against re-diverging by the load-manifest
  check.

### Changed

- The smoke and clojure-test-suite gates run against a freshly built joltc
  binary (10x faster boot than script mode): `make test` drops from ~12 to
  ~3 minutes (`make -j` parallelizes the rest), and the gates now exercise
  the shipped artifact — which is how both fixes above were found.

## [0.2.2] - 2026-07-10

### Added

- Refs and STM: `ref` (with `:validator`/`:meta`), `dosync`, `alter`, `commute`,
  `ref-set`, `ensure`, `sync`, `io!`, with serialized transactions on a single
  global lock; refs participate in watches/validators/metadata, and
  `*loaded-libs*` is a real ref over the loader registry (the tools.namespace
  reload pattern works). Transactions buffer writes and commit atomically:
  a thrown `dosync` rolls back, other threads never see uncommitted values,
  watches fire once per changed ref after commit, agent sends inside a
  transaction are held until commit, and transaction state does not leak into
  threads spawned inside a `dosync`. `(class (ref 0))` is `clojure.lang.Ref`,
  and `ref-min-history`/`ref-max-history` take the setter arity.
- `jolt.parser`: a general monadic parser-combinator core (`jolt.parser` +
  `jolt.parser.{basic,combinators,monad,position}`), adapted from rm-hull/jasentaa,
  with added combinators (`eof`, `between`, `sep-by`, an `optional` default-value
  arity, and the `digit`/`letter`/`alpha-num` character classes). Parse failures
  raise a jolt `ex-info`.
- `jolt.infix`: built-in infix math notation via the `infix`/`$=` macros and
  `from-string` (ported from rm-hull/infix), built on `jolt.parser`.
- Rounded out the `java.lang.Math` static surface: `atan2`, `sinh`, `cosh`,
  `tanh`, `cbrt`, `hypot`, `rint`, `floorDiv`, `floorMod`, `copySign`,
  `toRadians`, `toDegrees`, `log1p`, `expm1`.
- `java.text.ParseException` as a constructable/catchable host exception class,
  including `.getErrorOffset`.

### Changed

- `joltc` with no arguments starts a REPL, like `bb` and `clj` (piped stdin
  evaluates and exits). The nREPL server is the bare command
  `joltc nrepl-server [port]` — the flag spelling `--nrepl-server` is removed;
  `help` and `version` work as bare commands; an unknown command points at
  `joltc help`.
- Records store their fields inline (one heap object per record instead of a
  descriptor + separate values vector), and a typed non-nilable field read
  emits the receiver's direct per-arity slot accessor — no dispatch, one load.
  A retention-heavy construction microbenchmark allocates 25% less and runs
  ~44% faster; the mono-dispatch benchmark improves ~2.6x (101 → 39 ms,
  ~2.8x of JVM from ~7.8x). Nilable receivers keep the nil-safe read path
  (gate-pinned), and generic reads dispatch on the descriptor's field count.

### Fixed

- Reading a declared-but-unset var returns the `Var$Unbound` sentinel from
  every surface — a plain read, `@#'x`, and `var-get` all yield the same
  object (printing as `#object[clojure.lang.Var$Unbound …]`) instead of two
  of the three throwing; `bound?` still reports false.
- The self-host byte-fixpoint runs in CI: the seed rebuild is byte-identical
  on the pinned source-built Chez, so a seed source edited without a remint
  fails the gate on every platform.
- A tree-shaken binary crashed at startup when the project registered data
  readers (`data_readers.clj`): the emitted launcher re-scanned the source roots
  and eagerly reloaded each reader namespace through `jolt-compile-eval-form`,
  which a no-eval `--tree-shake` build has dropped. Data readers and reader
  namespaces are now baked once and not re-scanned at runtime, so a
  `(read-string "#my/tag …")` resolves its reader in the binary as it does under
  `joltc run`.
- Tree-shake soundness: a reader fn reached only through the baked
  `*data-readers*` map — including one registered programmatically via
  `alter-var-root`, not just via `data_readers.clj` — is now a DCE root, so the
  shake no longer prunes it and degrades `read-string` to a call error. App-form
  reference collection unions an IR walk (`:var`/`:the-var` nodes) with a text
  scan of the emitted Scheme, so a `(var-deref "ns" "nm")` a macro splices in
  with no IR node still roots its target.
- `jolt build --library`: the launcher guard now wraps the prologue (native
  loads + source-root setup) as well as the export-publish body, so an init
  failure anywhere reports and returns non-zero instead of leaving
  `jolt_lookup` silently returning `NULL` for every name.
- A warmed monomorphic protocol-call site in a direct-linked build now honors a
  runtime `extend-type`: the per-site cache carries the protocol epoch and
  re-resolves when an extension bumps it, so every dispatch path serves the new
  implementation.
- `--opt` builds no longer fold away a throwing operation: `/`, `quot`, `rem`,
  `mod`, `even?`, and `odd?` are not treated as pure, so
  `(:a {:a 1 :b (/ 1 0)})` raises `ArithmeticException` like Clojure instead of
  folding to `1`.
- A var read in a call or collection literal now evaluates in source order
  against a mutating sibling: `(f (do (def y 2) 0) y)` passes `[0 2]` like
  Clojure instead of reading `y` before the mutation.
- List libspecs whose second element is a keyword — `(:require (ns :only [x]))`
  — parse as libspecs everywhere (previously `require`/`use` mis-read them as
  prefix lists); the JVM rejects that shape outright, so this is a documented
  superset.
- A tree-shaken binary that queues agent sends inside a `dosync` no longer
  prunes `send` (the STM commit path resolves it by name at runtime); a new
  gate asserts every such runtime reference is a shake root.

## [0.2.1] - 2026-07-09

### Added

- `Throwable->map` (`:via`/`:cause`/`:data` over the `ex-cause` chain).
- The 11 core dynamic vars the JVM defines that were missing (`*agent*`,
  `*repl*`, `*compile-path*`, `*source-path*`, …), with real context behavior:
  `*agent*` is bound inside agent actions, `*repl*` and the `*1`/`*2`/`*3`/`*e`
  history work in `joltc repl`, `*file*`/`*source-path*` bind during loads, and
  `*command-line-args*` carries app args for `run` and `-m`.
- `clojure.test/test-var` and `test-vars`; `run-tests` discovers tests attached
  via `:test` var metadata, and `deftest` vars carry `:test` metadata.

### Changed

- `ns-map` returns every visible mapping (imports, refers, interns) and
  `ns-refers` includes the implicit `refer-clojure`, matching the JVM.
- Maps print with comma-separated entries (`{:a 1, :b 2}`).
- Double printing follows `Double.toString` (plain decimal only in
  `[1e-3, 1e7)`, otherwise `d.dddE±x`); `pr` of a beyond-long integer carries
  the BigInt `N` suffix.
- `hash-map` results iterate in insertion order up to the array-map threshold,
  like ClojureScript.

### Fixed

- The numeric fast path keeps `=` exactness-aware: `(= ^double-x 0)` is `false`
  like the JVM, and `:long` typing comes only from an explicit `^long` hint —
  an unhinted integer loop keeps arbitrary precision instead of raising a
  fixnum overflow.
- `require` honors `:reload`, `:reload-all`, and `:verbose`; a namespace whose
  load throws can be required again after the file is fixed; a data reader
  that resolves but throws surfaces its error (naming the tag) instead of
  silently degrading.
- `joltc -e EXPR args…` binds the trailing args as `*command-line-args*`
  (nil when empty), and the first standalone `--` is consumed as the POSIX
  end-of-options marker in every arg-taking path (`-e`, `run FILE`, `-m`,
  `-M` aliases, tasks, and `build` flags); later `--` stay literal.
- `(?x)` COMMENTS-mode regexes follow Java: whitespace (including newlines —
  multi-line patterns previously matched nothing) and `#`-comments are
  stripped, even inside character classes, and a mid-pattern cluster works.
- `$` matches before a final newline like Java; `\<`/`\>` are literal escapes;
  regex literals keep the backslash of an escaped quote in their source.
- `clojure.string/split-lines` drops trailing empty strings.
- `clojure.pprint` no longer emits trailing spaces before line breaks.

## [0.2.0] - 2026-07-09

### Added

- `jolt.fs` — file-system utilities in the standard library (predicates, glob,
  recursive copy/delete, move, `which`, temp dirs), shaped after `babashka.fs`.
- Data readers work in ahead-of-time binaries: reader namespaces are compiled in
  and `*data-readers*` is baked, so runtime `read-string` of `#tag` literals
  works in built executables.
- Reader errors report `file:line:column` in the message and carry
  `:file`/`:line`/`:column` in `ex-data`.
- [yamlstar](https://github.com/yaml/yamlstar) and
  [jolt-lang/yaml](https://github.com/jolt-lang/yaml) (libyaml bindings with a
  `clj-yaml.core` compat layer) are listed as supported libraries.

### Changed

- Performance round one: protocol dispatch goes through per-descriptor tables
  with polymorphic inline caches, record constructors inline, dynamic invoke and
  var access are cheaper, and collection equality/hash/reduce walk vector chunks
  directly. Geometric mean on the benchmark suite improved from ~6x to ~2.8x of
  JVM Clojure.
- Release builds run the inference passes (dispatch caches, devirtualization,
  constructor inlining) by default — 3.4x on dispatch-heavy code. Inlining and
  scalar replacement additionally require `--opt` with direct linking; projects
  can opt in via `deps.edn` `:jolt/build {:opt true}`.
- Optimized builds compile at Chez optimize-level 3 with compressed fasl output
  (−37% binary size).
- `defcfn` resolves its foreign symbol lazily on first call, so an optional
  `:jolt/native` library that is missing no longer aborts startup — a missing
  symbol is a catchable error at the call site.
- `spit` writes atomically (temp file + rename), so a crash mid-write can no
  longer truncate the target.
- The host class model (`instance?`, class tokens, type tags, `supers`) derives
  from a single class graph instead of parallel hand-maintained tables.

### Fixed

- Tree shaking soundness: `ns-publics`-family reflection triggers the
  keep-everything bail, a `defonce` no longer silently disables the whole shake,
  and data-reader functions are kept as roots.
- Native build link line: static archives precede system `-l` flags, paths are
  quoted, and Windows builds pass `--export-all-symbols`.
- Exceptions from `go`/`thread`/`Thread` bodies and data-reader load failures
  surface on stderr instead of being swallowed.
- A malformed `deps.edn` fails with a clear error instead of being ignored.
- `instance?` evaluates a local or var operand holding a class value instead of
  quoting it as a literal class name.
- Regex parity with Java: combined inline flag clusters (`(?sx)`, `(?si:…)`),
  scoped dot-all, escaped `]` inside character classes, and
  `Matcher.appendReplacement` escape semantics in replacement strings.
- `intern` and `alter-meta!` carry `:macro` through, and macro vars report
  `:macro` metadata.
- `require` of a namespace defined earlier in the same file is satisfied.
- `File.setLastModified` actually sets the file's mtime.
- `String.codePointAt` and `Character/toChars`; bigint edge-case coercions.

## [0.1.7] - 2026-07-06

### Added

- `jolt build --library` ahead-of-time compiles a project into a managed-runtime
  shared library (C ABI) for embedding Jolt in host applications, with
  Windows-friendly naming, build-time toolchain validation, and robust
  initialization.

### Changed

- The boot script now probes multiple names for the `chez` executable, improving
  discovery across installs.

## [0.1.6] - 2026-07-04

### Changed

- `JOLT_TRACE` tail-frame history now resolves each frame to its `ns/name`
  (`file:line`) source position instead of an opaque call site.

## [0.1.5] - 2026-07-04

### Fixed

- `JOLT_TRACE` is honored at runtime in a built `joltc` binary — it was
  previously baked in at build time and ignored the environment on the target
  machine.

## [0.1.4] - 2026-07-04

### Added

- Tail-call-optimized (elided) frames are recovered and shown in uncaught-error
  stack traces.

### Changed

- Tracing is on by default during REPL-driven development; `JOLT_TRACE` uses a
  single case-insensitive off-check covering both enable paths.

### Fixed

- Ahead-of-time builds run `-main` with `*ns* = user`, matching `clojure.main`.

## [0.1.3] - 2026-07-04

### Added

- Clojure 1.13 parity: `req!`, checked-keys destructuring, and keyword array maps.

### Fixed

- `build` invoked with a no-main entry namespace now runs the namespace as a
  script instead of crashing.

## [0.1.2] - 2026-07-04

### Added

- A `joltc` version string.

### Fixed

- nREPL server runs on Windows.
- `deps.edn` files that omit `org.clojure/clojure` no longer warn.
- Missing vendor submodules now fail with an actionable error.

## [0.1.1] - 2026-07-02

### Added

- Windows release binaries (x86_64) built via MSYS2/MinGW and statically linked
  into a single-file executable.
- The `clojure-test-suite` is vendored as a standing conformance gate
  (`make cts`).
- Every conformance corpus row is tagged with `:portability` (`:common` vs.
  `:jvm`).
- A single `IRef` seam shares watches, validators, and metadata across `atom`,
  `var`, and `agent`.

### Changed

- Binary numeric operators dispatch through a Numbers-style category model.
- Hierarchy functions follow the reference contracts, and `deftype` classes join
  the class graph.
- `clojure.string` performs `toString` coercion; `some-fn`/`ifn?` follow
  reference semantics.
- The reader enforces strict tokens, and EDN mode matches the reference's error
  contracts.
- `rand-nth` follows the reference shape.

### Fixed

- General divergences surfaced by the `clojure-test-suite`.
- `clojure.test/are` substitutes through `clojure.template`.
- Checked narrow casts, and runtime `require` in self-contained-built binaries.

### Removed

- Delisted `next.jdbc` (JVM/JDBC-driver dependent).
- Dropped `x86_64-macos` from releases (GitHub retired the Intel runner).

## [0.1.0] - 2026-07-01

Initial public release. Jolt is a self-hosting Clojure implementation on
[Chez Scheme](https://cisco.github.io/ChezScheme/) — it reads Clojure source,
analyzes it to a host-neutral IR, emits Scheme, and runs it on Chez, shipping a
Clojure-compatible standard library.

### Added

- **Language & runtime**: a self-hosted compiler (reader → analyzer → IR →
  Scheme backend) written in Clojure and driven by a checked-in bootstrap seed;
  `bin/joltc` evaluates expressions, runs a line REPL, and serves an nREPL
  server.
- **Persistent collections**: 32-way-trie vectors, HAMT hash maps and sets, with
  transient variants and linear-time builds.
- **Numeric tower**: exact integers, bignums, ratios, and doubles; category-aware
  `=` (`(= 3 3.0)` ⇒ `false`) and value-equality `==`.
- **Sequences & transducers**: lazy and infinite sequences, plus
  transducer-returning `map`/`filter`/`take`/… and `transduce`, `into`,
  `sequence`, `eduction`, and `reduced`.
- **Types & abstractions**: multimethods with hierarchies;
  `defprotocol`/`deftype`/`defrecord`/`reify`/`extend-protocol`/`extend-type`;
  metadata; and full `ns` forms.
- **Reference & concurrency types**: atoms (per-atom mutex, JVM-style CAS),
  volatiles, delays, `future`/`promise`/`agent`/`pmap`, and `clojure.core.async`
  over native channels.
- **Reader**: `#()` fn literals, `#_`, `#?` reader conditionals, tagged literals
  (`#inst`, `#uuid`), `#"…"` regex via vendored irregex, and a proper char type.
- **Runtime macroexpansion**: `eval`, `load-string`, and `defmacro` at runtime.
- **Standard library**: `clojure.string`, `clojure.set`, `clojure.walk`,
  `clojure.edn`, `clojure.pprint`, and the `jolt.ffi` foreign-function interface
  (foreign-callable callbacks, binary-faithful buffer I/O, `:blocking` calls,
  and `:jolt/native` library declarations).
- **Host interop shim**: a subset of the `java.*` standard library (including
  `java.time` Duration/Period/enums) so portable Clojure loads; class tokens are
  names rather than loaded classes, with no reflection or `gen-class`/`proxy`.
- **Ahead-of-time builds**: `joltc build -m ns -o out` compiles a project into a
  single self-contained executable (runtime + `clojure.core` + stdlib + app +
  `deps.edn` dependencies) with `--opt` inference/inlining passes and opt-in
  `--direct-link` and `--tree-shake` whole-program dead-code elimination.
- **Standalone toolchain binary**: `make joltc-release`/`make joltc-debug` link a
  single `joltc` that runs and `build`s apps without a local Chez or C toolchain.
- **Conformance gates**: a JVM-sourced conformance corpus (`make corpus`/
  `make certify`), a bootstrap self-hosting fixpoint (`make selfhost`), and an
  SCI compatibility stress gate (`make sci`).
- **Distribution**: a self-contained `joltc` binary, a Homebrew tap, and an
  install script.

[Unreleased]: https://github.com/jolt-lang/jolt/compare/v0.2.4...HEAD
[0.2.4]: https://github.com/jolt-lang/jolt/compare/v0.2.3...v0.2.4
[0.2.3]: https://github.com/jolt-lang/jolt/compare/v0.2.2...v0.2.3
[0.2.2]: https://github.com/jolt-lang/jolt/compare/v0.2.1...v0.2.2
[0.2.1]: https://github.com/jolt-lang/jolt/compare/v0.2.0...v0.2.1
[0.2.0]: https://github.com/jolt-lang/jolt/compare/v0.1.7...v0.2.0
[0.1.7]: https://github.com/jolt-lang/jolt/compare/v0.1.6...v0.1.7
[0.1.6]: https://github.com/jolt-lang/jolt/compare/v0.1.5...v0.1.6
[0.1.5]: https://github.com/jolt-lang/jolt/compare/v0.1.4...v0.1.5
[0.1.4]: https://github.com/jolt-lang/jolt/compare/v0.1.3...v0.1.4
[0.1.3]: https://github.com/jolt-lang/jolt/compare/v0.1.2...v0.1.3
[0.1.2]: https://github.com/jolt-lang/jolt/compare/v0.1.1...v0.1.2
[0.1.1]: https://github.com/jolt-lang/jolt/compare/v0.1.0...v0.1.1
[0.1.0]: https://github.com/jolt-lang/jolt/releases/tag/v0.1.0
