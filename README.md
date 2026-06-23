# Jolt

[![tests](https://github.com/jolt-lang/jolt/actions/workflows/tests.yml/badge.svg)](https://github.com/jolt-lang/jolt/actions/workflows/tests.yml)

A Clojure implementation on [Chez Scheme](https://cisco.github.io/ChezScheme/).
Jolt reads Clojure source, analyzes it to a host-neutral IR, emits Scheme, and
runs it on Chez. The compiler is self-hosted: it is written in Clojure
(`jolt-core/`) and compiles itself. It ships a Clojure-compatible standard library.

## Requirements

Only [Chez Scheme](https://cisco.github.io/ChezScheme/) (the gate invokes it as
`chez`). The conformance gate additionally uses Clojure on the JVM as an oracle,
but running jolt does not.

## Build

There is no build step. The bootstrap seed (`host/chez/seed/{prelude,image}.ss`)
is checked in, so a fresh clone runs immediately:

```bash
git clone --recurse-submodules https://github.com/jolt-lang/jolt.git
cd jolt
bin/joltc -e '(+ 1 2)'        # => 3
```

After changing a compiler source — the reader (`host/chez/reader.ss`), the
analyzer/IR/backend (`jolt-core/jolt/*.clj`), or the `clojure.core` overlay
(`jolt-core/clojure/core/*.clj`) — re-mint the seed:

```bash
make remint                   # iterates host/chez/bootstrap.ss to a byte-fixpoint
```

## Run

```bash
bin/joltc -e EXPR             # evaluate a Clojure expression and print the result
```

```bash
$ bin/joltc -e '(->> (range 10) (filter even?) (map (fn [x] (* x x))) (reduce +))'
120
$ bin/joltc -e '(/ 1 2)'
1/2
```

## Architecture

A small Chez runtime (`host/chez/*.ss`: value model, persistent collections, seqs,
vars/namespaces, host interop) hosts a portable Clojure overlay split across two
source roots by *when* they load:

- **`jolt-core/`** is baked into the seed — the compiler (`jolt-core/jolt/`:
  reader/analyzer/IR/backend, plus `jolt.main`/`jolt.deps`) and `clojure.core` in
  dependency-ordered tiers (`jolt-core/clojure/core/NN-*.clj`). Changing anything
  here means re-minting the seed.
- **`stdlib/`** loads lazily at runtime off the source roots — the rest of the
  standard library (`clojure.string`/`set`/`walk`/`edn`/`pprint`/…) plus the
  `jolt.ffi` host library. Editing these needs no re-mint.

`bin/joltc` loads the checked-in seed and the spine, then compiles and evaluates on
Chez (read → analyze → IR → emit → eval). `host/chez/bootstrap.ss` rebuilds that
seed from source on pure Chez; the build is a self-hosting fixpoint (a rebuild
reproduces the checked-in seed byte-for-byte).

## Differences from Clojure

Jolt targets Clojure semantics but runs on Chez, not the JVM.

- **Host platform.** No JVM, no reflection, no `gen-class`/`proxy` of Java
  classes. Interop syntax (`Class.`, `Class/static`, `.method`) works against a
  shimmed subset of the `java.*` standard library, and a class token resolves to
  a name. See [docs/host-interop.md](docs/host-interop.md) for what's covered and
  how to register your own host classes from a library.
- **Numbers.** The full Scheme numeric tower, matching the JVM: exact integers and
  bignums, exact ratios (`(/ 1 2)` ⇒ `1/2`), and flonum doubles. `=` is
  category-aware (`(= 3 3.0)` ⇒ `false`); `==` is value-equality (`(== 3 3.0)` ⇒
  `true`). `integer?`/`int?` are exact integers, `float?`/`double?` are flonums,
  `ratio?` is an exact non-integer. No `BigDecimal` (`decimal?` is always false).
- **Concurrency.** `future`/`promise`/`agent`/`pmap` run on real OS threads over a
  **shared heap**, matching JVM semantics (not isolated-heap snapshots). Atoms use a
  per-atom mutex with JVM-style CAS. `clojure.core.async` provides blocking channels
  and `go`/`<!`/`>!`/`alts!`/`timeout`.
- **Regex.** Backed by [irregex](https://github.com/ashinn/irregex) (vendored),
  PCRE/Java-style patterns.
- **Collections.** Immutable persistent vectors, cons lists, and HAMT maps/sets.
  Hash-map/hash-set iteration order is unspecified — use `sorted-map`/`sorted-set`
  when order matters. Transients are real mutable scratch collections.

Supported and Clojure-compatible: lazy/infinite sequences, transducers,
destructuring, multimethods with hierarchies, protocols/records
(`deftype`/`defrecord`/`reify`/`extend-protocol`), metadata, namespaces, runtime
`eval`/`load-string`/`defmacro`, and the reader (`#()`, `#_`, `#?`, tagged literals,
`#"…"`).

## Test

```bash
make test                     # the full gate
make corpus                   # conformance corpus vs the JVM-sourced spec
make unit                     # host-specific unit cases
make selfhost                 # bootstrap fixpoint (rebuild == checked-in seed)
make smoke                    # bin/joltc CLI smoke
make sci                      # load borkdude/sci's source through joltc (compat stress)
make ffi                      # HTTP-server GC-safety + http-client temp paths
make transient                # transient mutation + linear-time builds
make certify                  # JVM oracle (skips if clojure is absent)
```

The conformance corpus (`test/chez/corpus.edn`) is a host-neutral language spec
whose expected values are sourced from reference JVM Clojure. See
[test/conformance/SPEC.md](test/conformance/SPEC.md).

## License

[Eclipse Public License 1.0](https://opensource.org/licenses/EPL-1.0)
