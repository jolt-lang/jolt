# Changelog

All notable changes to this project are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Changed

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

[Unreleased]: https://github.com/jolt-lang/jolt/compare/v0.2.1...HEAD
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
