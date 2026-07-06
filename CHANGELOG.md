# Changelog

All notable changes to this project are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

No unreleased changes. HEAD tracks the latest tag.

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

[Unreleased]: https://github.com/jolt-lang/jolt/compare/v0.1.7...HEAD
[0.1.7]: https://github.com/jolt-lang/jolt/compare/v0.1.6...v0.1.7
[0.1.6]: https://github.com/jolt-lang/jolt/compare/v0.1.5...v0.1.6
[0.1.5]: https://github.com/jolt-lang/jolt/compare/v0.1.4...v0.1.5
[0.1.4]: https://github.com/jolt-lang/jolt/compare/v0.1.3...v0.1.4
[0.1.3]: https://github.com/jolt-lang/jolt/compare/v0.1.2...v0.1.3
[0.1.2]: https://github.com/jolt-lang/jolt/compare/v0.1.1...v0.1.2
[0.1.1]: https://github.com/jolt-lang/jolt/compare/v0.1.0...v0.1.1
[0.1.0]: https://github.com/jolt-lang/jolt/releases/tag/v0.1.0
