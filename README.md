# Jolt

[![tests](https://github.com/jolt-lang/jolt/actions/workflows/tests.yml/badge.svg)](https://github.com/jolt-lang/jolt/actions/workflows/tests.yml)

A Clojure implementation on [Chez Scheme](https://cisco.github.io/ChezScheme/).
Jolt reads Clojure source, analyzes it to a host-neutral IR, emits Scheme, and
runs it on Chez. The compiler is self-hosted: it is written in Clojure
(`jolt-core/`) and compiles itself. It ships a Clojure-compatible standard library.

## Install

Grab the self-contained `joltc` binary (Linux/macOS/Windows) — it bundles the
runtime, compiler, and standard library, so there is nothing else to install.
Download the binary archive for your platform from the
[releases page](https://github.com/jolt-lang/jolt/releases) (`joltc-<ver>-<platform>.tar.gz`,
or the `.zip` on Windows). The "Source code" archives GitHub attaches to every
release are not binaries — see [Build](#build) before using one.

With Homebrew:

```bash
brew install jolt-lang/jolt/jolt
```

Or with the install script (installs to `/usr/local/bin` by default; `--dir <dir>`
and `--version <v>` override that):

```bash
curl -sL https://raw.githubusercontent.com/jolt-lang/jolt/main/install | bash
```

Then `joltc -e '(+ 1 2)'`. To run from source instead (needs Chez), see
[Build](#build).

## Requirements

The prebuilt binaries are self-contained (runtime, compiler, and stdlib in one
executable) and need only the base system libraries:

- **Linux x86_64**: glibc 2.35 or newer (Ubuntu 22.04+, Debian 12+, RHEL 9+).
  The installer verifies the binary runs and reports the exact glibc mismatch
  if not.
- **macOS arm64**: macOS 14+.
- Anything else (Intel Mac, musl/Alpine, older glibc): build from source.

Building from source needs only [Chez Scheme](https://cisco.github.io/ChezScheme/)
(the gate invokes it as `chez`) and a C compiler. The conformance gate
additionally uses Clojure on the JVM as an oracle, but running jolt does not.

### Dependency resolution

Resolving a project's `deps.edn` uses a few standard tools, each needed only for
the coordinate types you use — a dependency that can't be fetched is skipped, never
fatal:

- **Git deps** (`:git/url`) need `git` on `PATH`.
- **Maven deps** (`:mvn/version`) are downloaded over HTTPS by jolt itself (no
  `curl`), using the system **OpenSSL** (`libssl`/`libcrypto`) via FFI, and
  extracted with `unzip`. A jar already in `~/.m2/repository` is reused with no
  download.
  - **macOS**: `brew install openssl@3` — jolt loads the Homebrew copy; the
    protected system `/usr/lib` OpenSSL can't be loaded into a non-Apple binary.
    `git`/`unzip` come with the Xcode command-line tools.
  - **Linux**: the distro `libssl3`/`libcrypto3` (or `libssl`/`libcrypto`) packages,
    plus `git` and `unzip`.
  - **Windows**: [Git for Windows](https://git-scm.com/download/win) supplies `git`,
    the OpenSSL DLLs (`libssl-3-x64.dll`/`libcrypto-3-x64.dll`), and `unzip`; run
    `joltc` from a shell with those on `PATH`.

## Build

There is no build step. The bootstrap seed (`host/chez/seed/{prelude,image}.ss`)
is checked in, so a fresh clone runs immediately:

```bash
git clone --recurse-submodules https://github.com/jolt-lang/jolt.git
cd jolt
bin/joltc -e '(+ 1 2)'        # => 3
```

The `--recurse-submodules` matters: jolt vendors its regex engine and test
suites as git submodules. In a checkout that's missing them (a plain
`git clone`, or after pulling a commit that adds one), fetch them with:

```bash
git submodule update --init --recursive
```

Note that GitHub's auto-generated "Source code (zip/tar.gz)" archives on the
releases page do **not** contain submodules, so they can't run or build —
clone the repo instead (or grab a prebuilt binary from the same page).

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

## Diagnostics

- **"Did you mean?"** — when a bare symbol doesn't resolve, the compile error lists
  the closest in-scope names by edit distance (current-namespace vars,
  `clojure.core` publics, and lexical locals):
  ```
  $ bin/joltc -e '(prinltn 1)'
  Unable to resolve symbol: prinltn in this context (did you mean print, printf, println?)
  ```
- **`JOLT_DIAG=edn`** — emit an uncaught error as a single line of valid EDN to
  stderr (with `:message` and source `:line`/`:column`/`:file`; an unresolved
  symbol also carries `:type`/`:symbol`/`:suggestions`/`:ns`) so an editor or tool
  can read it back. Default output is unchanged.
  ```
  $ JOLT_DIAG=edn bin/joltc -e '(prinltn 1)'
  {:type :unresolved-symbol, :symbol "prinltn", :suggestions ["print" "printf" "println"], :ns "user", :message "...", :line 1, :column 2, :file "-e"}
  ```
- **`JOLT_CHECK`** — opt-in success-type lint (RFC 0006): each runtime-compiled form
  is run through the checker and findings print as located warnings, e.g.
  `1:10: warning: `+` requires a number, but argument 2 is a keyword`. Off by
  default (zero cost); a checker error never breaks a compile.
- **`JOLT_DEBUG`** — verbose dependency resolution (the fetching / using-cache /
  skipping lines that are otherwise quiet) and the host static-shim drift warning.

## REPL and editor integration

```bash
bin/joltc repl                  # a line REPL with the project's deps loaded
bin/joltc nrepl-server [port]   # an nREPL server (default 7888) for editors
```

Both resolve the `deps.edn` in the current directory first, so the project's
source roots and native libraries are loaded — `(require '[my.ns])` works live.
`nrepl-server` writes a `.nrepl-port` file in the project dir, so CIDER / Calva / Cursive
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

Numeric code unboxes to raw flonum/fixnum machine ops when types are proven —
by whole-program inference (float literals, record fields, protocol returns:
no annotations needed), by JVM-style `^double`/`^long` hints, or by
`(double x)`/`(long x)` casts where inference can't see. See
[Building & Running](https://jolt-lang.github.io/docs/building-and-deps.html#typed-arithmetic-and-inference).

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
[deps.edn internals](https://jolt-lang.github.io/docs/tools-deps.html) and [RFC 0007](https://jolt-lang.github.io/docs/rfc/0007-compilation-modes-and-binary-output.html).

This needs Chez's kernel development files (`libkernel.a`, `scheme.h`) and a C
compiler. They come with a from-source Chez install; a distro `chezscheme`
package ships only the runtime, so `build` won't link a binary there.
[RFC 0007](https://jolt-lang.github.io/docs/rfc/0007-compilation-modes-and-binary-output.html) covers the design and the three-mode model.

## Compile a library

`bin/joltc build --library` compiles a project into a shared object
(`.so`/`.dylib`/`.dll`) that a C/C++/Rust host links or `dlopen`s and calls
through a small C ABI. Like `build`, the whole runtime is embedded — the result
is a *managed-runtime* library: it carries its own GC and must be entered
through `jolt_library_init` before any call.

The Jolt side publishes entry points with `jolt.ffi/export!`:

```clojure
(ns libadd.core
  (:require [jolt.ffi :as ffi]))

(defn add [x y] (+ x y))
(ffi/export! "add" add [:int :int] :int)
```

```bash
bin/joltc build --library -m libadd.core -o libadd   # => libadd.so / libadd.dylib
```

The C side calls `jolt_library_init` once, then resolves each entry by name with
`jolt_lookup` and casts to its type:

```c
#include <dlfcn.h>
typedef int (*init_fn)(int, char**);
typedef void* (*lookup_fn)(const char*);
typedef int (*add_fn)(int, int);

void* h = dlopen("./libadd.so", RTLD_NOW | RTLD_LOCAL);
((init_fn)dlsym(h, "jolt_library_init"))(0, NULL);        /* runs top-level, registers exports */
add_fn add = (add_fn)((lookup_fn)dlsym(h, "jolt_lookup"))("add");
add(2, 3);                                                 /* => 5 */
```

The type keywords (`:int`, `:string`, …) are the same ones `foreign-fn` uses;
see [Host Interop](https://jolt-lang.github.io/docs/host-interop.html) for the full list and limits.
The same `--opt`/`--dev`/`--direct-link`/`--tree-shake` flags apply, and the
same Chez kernel development files + C compiler are required to link.

## Standalone joltc binary

`make` builds joltc itself into a single self-contained native binary — the
runtime, compiler, `jolt-core`/`stdlib` source, and the Chez boots are baked in,
so the result runs and `build`s jolt apps on a machine with neither Chez nor a C
compiler. Build it on a host that *does* have both.

```bash
make joltc-release             # => target/release/joltc (optimize-level 3, compressed)
make joltc-debug               # => target/debug/joltc   (optimize-level 0, inspector + debug info)
make joltc                     # re-mint the seed first, then both
```

`make joltc` re-mints the seed so the embedded compiler image is current before
linking; use `joltc-release`/`joltc-debug` directly to skip that when the seed is
already minted. Like `build`, both require Chez's kernel development files
(`libkernel.a`, `scheme.h`) and a C compiler.

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
maps/sets), the numeric tower (exact integers, bignums, ratios, doubles,
`BigDecimal` with `M` literals and `with-precision`), lazy
and infinite sequences, transducers, destructuring, multimethods with
hierarchies, protocols/records (`deftype`/`defrecord`/`reify`/`extend-protocol`),
metadata, namespaces, atoms, refs/STM (`ref`/`dosync`/`alter`/`commute`),
`future`/`promise`/`agent`/`pmap`,
`clojure.core.async`, runtime `eval`/`load-string`/`defmacro`, and the full
reader (`#()`, `#_`, `#?`, tagged literals, `#"…"`) all behave as on the JVM.
`=` is category-aware (`(= 3 3.0)` ⇒ `false`) and `==` is value-equality, as in
Clojure. The genuine divergences:

- **No JVM, no Java interop.** No reflection, no `gen-class`/`proxy`. Interop
  syntax (`Class.`, `Class/static`, `.method`) resolves only against a shimmed
  subset of the `java.*` standard library; a class token is a name, not a loaded
  class. See [Host Interop](https://jolt-lang.github.io/docs/host-interop.html). To call C libraries
  directly, use the `jolt.ffi` foreign-function interface (how the db and
  http-client libraries bind SQLite/libpq and sockets/OpenSSL/zlib).
- **Codepoint strings.** Strings are Chez strings — codepoint-indexed, no
  UTF-16 surrogate pairs. `(count "😀")` is 1 (JVM: 2) and `subs` never splits
  a character; only code doing UTF-16 unit arithmetic notices.
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
