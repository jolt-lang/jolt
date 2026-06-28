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

## REPL and editor integration

```bash
bin/joltc repl                  # a line REPL with the project's deps loaded
bin/joltc --nrepl-server [port] # an nREPL server (default 7888) for editors
```

Both resolve the `deps.edn` in the current directory first, so the project's
source roots and native libraries are loaded — `(require '[my.ns])` works live.
`--nrepl-server` writes a `.nrepl-port` file in the project dir, so CIDER / Calva / Cursive
auto-detect the port; override it with the argument or `JOLT_NREPL_PORT`.

The server runs in dev mode — calls deref their var, so redefining a function
takes effect on the next call without restarting the process. The built-in
handler speaks `clone`/`describe`/`eval`/`load-file`/`close`; heavier ops
(sessions, interruptible eval, completion) are added as nREPL middleware listed
in `deps.edn` under `:nrepl/middleware`.

```clojure
;; from your editor, against the running process:
(require '[myapp.core :as app])
(app/start!)                  ; bring the app up
;; edit a handler, re-evaluate the defn — the running app sees it, no restart
(app/stop!)
```

## Compile a binary

`bin/joltc build` ahead-of-time compiles a project into a single self-contained
executable — the runtime, `clojure.core`, the standard library, the app, and its
`deps.edn` dependencies are linked in, so the result needs no Chez install, no
JVM, and no source on disk to run.

```bash
bin/joltc build -m myapp.core -o myapp   # compile myapp.core's -main into ./myapp
./myapp arg1 arg2                        # runs anywhere; args reach -main
```

Modes trade dynamism for speed: the default (release) build uses the proven code
generator; `--opt` also runs the inference + inlining + scalar-replacement passes
over the closed-world program; `--dev` is unoptimized.

Two opt-in closed-world flags cut dispatch cost and binary size:

```bash
bin/joltc build -m myapp.core --direct-link   # app->app calls bind directly (no var lookup)
bin/joltc build -m myapp.core --tree-shake    # ship only code reachable from -main
```

`--tree-shake` walks the call graph across your app, its libraries, and
`clojure.core`, drops everything unreachable from `-main` (and the compiler itself
when the app never `eval`s), and typically removes 1–2 MB. It stays sound by bailing
out — keeping everything, and reporting which library is responsible — when reachable
code resolves vars by name at runtime (`eval`/`resolve`/`ns-resolve`/…). See
[docs/tools-deps.md](docs/tools-deps.md) and `docs/rfc/0007`.

This needs Chez's kernel development files (`libkernel.a`, `scheme.h`) and a C
compiler. They come with a from-source Chez install; a distro `chezscheme`
package ships only the runtime, so `build` won't link a binary there.
RFC 0007 (`docs/rfc/`) covers the design and the three-mode model.

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

Jolt targets Clojure semantics but runs on Chez, not the JVM. Most portable
Clojure runs unchanged — persistent collections (32-way-trie vectors, HAMT
maps/sets), the numeric tower (exact integers, bignums, ratios, doubles), lazy
and infinite sequences, transducers, destructuring, multimethods with
hierarchies, protocols/records (`deftype`/`defrecord`/`reify`/`extend-protocol`),
metadata, namespaces, atoms, `future`/`promise`/`agent`/`pmap`,
`clojure.core.async`, runtime `eval`/`load-string`/`defmacro`, and the full
reader (`#()`, `#_`, `#?`, tagged literals, `#"…"`) all behave as on the JVM.
`=` is category-aware (`(= 3 3.0)` ⇒ `false`) and `==` is value-equality, as in
Clojure. The genuine divergences:

- **No JVM, no Java interop.** No reflection, no `gen-class`/`proxy`. Interop
  syntax (`Class.`, `Class/static`, `.method`) resolves only against a shimmed
  subset of the `java.*` standard library; a class token is a name, not a loaded
  class. See [docs/host-interop.md](docs/host-interop.md). To call C libraries
  directly, use the `jolt.ffi` foreign-function interface (how the db and
  http-client libraries bind SQLite/libpq and sockets/OpenSSL/zlib).
- **No `BigDecimal`.** `decimal?` is always false and there is no `M` literal;
  the rest of the numeric tower matches the JVM.
- **No STM.** No `ref`/`dosync`/`alter`/`commute` — coordinated shared state uses
  atoms (per-atom mutex, JVM-style CAS). The concurrency primitives above are
  otherwise present and run on a shared heap.
- **Regex engine.** Patterns compile through
  [irregex](https://github.com/ashinn/irregex) (vendored), not
  `java.util.regex`; common patterns work, Java-specific features can differ.
- **Coverage.** `clojure.core` is implemented function by function against the
  JVM-sourced conformance corpus — broad but not total; a namespace can load with
  most functions working and a few not yet implemented.

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

[Eclipse Public License 2.0](https://www.eclipse.org/legal/epl-2.0/)
