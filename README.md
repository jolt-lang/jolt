# Jolt

[![tests](https://github.com/jolt-lang/jolt/actions/workflows/tests.yml/badge.svg)](https://github.com/jolt-lang/jolt/actions/workflows/tests.yml)

A Clojure implementation on Scheme. Jolt reads Clojure source, analyzes it to a
host-neutral IR, emits Scheme, and runs it — on [Chez](https://cisco.github.io/ChezScheme/)
by default, or on [Gambit](https://gambitscheme.org/) compiled to JavaScript for
the browser. The compiler is self-hosted: it is written in Clojure (`jolt-core/`)
and compiles itself. It ships a Clojure-compatible standard library.

## This is not the JVM

Most portable Clojure runs unchanged, but there is no JVM underneath and JVM
reasoning does not carry over. The four that bite first:

- **No Java interop.** No reflection, no `gen-class`/`proxy`. Interop syntax
  (`Class.`, `Class/static`, `.method`) resolves against a shimmed subset of
  `java.*` written in Scheme; a class token is a name, not a loaded class. The
  shims reimplement their JVM counterparts' *API* — they are not the JVM, so
  classloaders, JVM GC behaviour, and JVM thread lifetime rules do not apply.
  To call C libraries, use the `jolt.ffi` foreign-function interface.
- **Codepoint strings.** `(count "😀")` is 1, not 2. No UTF-16 surrogate pairs.
- **A different regex engine.** Patterns compile through
  [irregex](https://github.com/ashinn/irregex), not `java.util.regex`.
- **Partial `clojure.core` coverage.** Broad but not total; a namespace can load
  with most functions working and a few not yet implemented.

[Differences from Clojure](#differences-from-clojure) below is the full list.
Read it before assuming a JVM behaviour holds.

## Contents

- [Install](#install) — prebuilt binaries, Homebrew, install script
- [Run](#run) — `-e`, project deps, `clj`-compatible options
- [Differences from Clojure](#differences-from-clojure) — what actually diverges
- [Runtime dependencies](#runtime-dependencies) — acquiring libraries in code
- [Diagnostics](#diagnostics) — error suggestions, EDN errors, the lint pass
- [REPL and editor integration](#repl-and-editor-integration) — nREPL, CIDER/Calva/Cursive
- [Compile a binary](#compile-a-binary) — self-contained executables
- [Compile a library](#compile-a-library) — shared objects with a C ABI
- [Documentation](#documentation) — the guides, API pages, and language spec
- [Contributing](#contributing) — building from source, architecture, test gates

Machine-readable index for coding agents: [`llms.txt`](llms.txt).

## Install

Prebuilt binaries are self-contained — runtime, compiler, and stdlib in one
executable — and need only the base system libraries: **Linux x86_64** wants
glibc 2.35 or newer (Ubuntu 22.04+, Debian 12+, RHEL 9+), **macOS arm64** wants
macOS 14+. Anything else (Intel Mac, musl/Alpine, older glibc) is not supported
by the prebuilt binaries — [build from source](CONTRIBUTING.md#build-from-source).

With Homebrew:

```bash
brew install jolt-lang/jolt/jolt
```

Or with the install script (installs to `/usr/local/bin` by default; `--dir <dir>`
and `--version <v>` override that):

```bash
curl -sL https://raw.githubusercontent.com/jolt-lang/jolt/main/install | bash
```

Or download the binary archive for your platform from the
[releases page](https://github.com/jolt-lang/jolt/releases)
(`jolt-<ver>-<platform>.tar.gz`, or the `.zip` on Windows). The "Source code"
archives GitHub attaches to a release are not binaries and omit the submodules,
so they can neither run nor build — clone the repo instead.

Then `jolt -e '(+ 1 2)'`.

Running from source has no build step. The bootstrap seed
(`host/chez/seed/{prelude,image}.ss`) is checked in, so a fresh clone runs
immediately:

```bash
git clone --recurse-submodules https://github.com/jolt-lang/jolt.git
cd jolt
bin/jolt -e '(+ 1 2)'        # => 3
```

The `--recurse-submodules` matters: jolt vendors its regex engine, its Maven
resolver, and its test suites as git submodules. In a checkout that's missing
them (a plain `git clone`, or after pulling a commit that adds one), fetch them
with:

```bash
git submodule update --init --recursive
```

`bin/jolt` needs a **threaded Chez Scheme 10.x**. It first honors `JOLT_CHEZ`,
then reuses a 10.x Chez already provisioned under `.cache/local` by `make`, and
finally searches `PATH` for `chez` or `chezscheme`. `make` provisions its own
10.4.1 when `PATH` has a different version and exports `JOLT_CHEZ` so both halves
of a build agree.

Note that GitHub's auto-generated "Source code (zip/tar.gz)" archives on the
releases page do **not** contain submodules, so they can't run or build —
clone the repo instead (or grab a prebuilt binary from the same page).

After changing a compiler source — the reader (`host/chez/reader.ss`), the
analyzer/IR/backend (`jolt-core/jolt/*.clj`), or the `clojure.core` overlay
(`jolt-core/clojure/core/*.clj`) — re-mint the seed:

```bash
make remint                   # iterates host/chez/bootstrap.ss to a byte-fixpoint
```

Resolving a project's `deps.edn` needs `git` for git deps, and OpenSSL
(`libssl`/`libcrypto`, loaded via FFI) plus `unzip` for Maven deps — jolt
downloads and resolves those itself, with no `curl` and no Java. A dependency
that can't be fetched is skipped, never fatal. See
[Getting Started](https://jolt-lang.github.io/docs/getting-started.html) for the
per-platform packages and [deps.edn internals](https://jolt-lang.github.io/docs/tools-deps.html)
for how resolution works.

## Run

```bash
jolt -e EXPR                 # evaluate a Clojure expression and print the result
```

```bash
$ jolt -e '(->> (range 10) (filter even?) (map (fn [x] (* x x))) (reduce +))'
120
$ jolt -e '(/ 1 2)'
1/2
```

When the current directory has a `deps.edn`, `-e` resolves it first, so the
expression can require the project's own namespaces and its dependencies.
`-Sdeps` and `-A` compose with it for a one-off evaluation, and `-M` takes the
same main options on the command line when the selected aliases declare none:

```bash
jolt -Sdeps '{:paths ["src" "test"]}' -e "(require 'my.app-test 'clojure.test)
                                          (clojure.test/run-tests 'my.app-test)"
jolt -A:test -M -e "(println :hi)"
```

The rest of the `clj` option surface works the same way — each takes the aliases
around it and runs no program:

```bash
jolt -Spath                  # the classpath (what an editor asks for before connecting)
jolt -Stree                  # the dependency tree, tools.deps format
jolt -Strace                 # write the expansion decisions to trace.edn
jolt -Sdescribe              # version, deps.edn chain, and caches, as edn
jolt -P                      # fetch every dependency, then stop (CI, images)
jolt -Srepro …               # ignore ~/.clojure/deps.edn for this run
jolt -Sverbose …             # say where deps are read from and fetched into
jolt -Scp "$(cat cp.txt)" …  # run against a recorded classpath, expanding nothing
```

An alias the project doesn't declare is skipped with a warning rather than
failing the query, so `jolt -A:test:dev -Spath` and `jolt -Spath -M:test` both
answer. Under `-Scp` the deps.edn is still read — aliases, `:main-opts` and
tasks work — but nothing is expanded, so a shared library declared by a
*dependency* is not loaded (the project's own `:jolt/native` still is).
`-Sforce`, `-Sthreads`, and `-Jopt` are accepted and ignored: there is no
classpath cache to force, fetching is serial, and there is no JVM to pass
options to.

## Differences from Clojure

Jolt targets Clojure semantics but runs on Chez, not the JVM. Most portable
Clojure runs unchanged — persistent collections (32-way-trie vectors, HAMT
maps/sets, RRB vectors), the numeric tower (exact integers, bignums, ratios,
doubles, `BigDecimal` with `M` literals and `with-precision`), lazy and infinite
sequences, transducers, destructuring, multimethods with hierarchies,
protocols/records (`deftype`/`defrecord`/`reify`/`extend-protocol`), metadata,
namespaces, atoms, refs/STM (`ref`/`dosync`/`alter`/`commute`),
`future`/`promise`/`agent`/`pmap`, `clojure.core.async`, runtime
`eval`/`load-string`/`defmacro`, and the full reader (`#()`, `#_`, `#?`, tagged
literals, `#"…"`) all behave as on the JVM. `=` is category-aware
(`(= 3 3.0)` ⇒ `false`) and `==` is value-equality, as in Clojure. The genuine
divergences:

- **No JVM, no Java interop.** No reflection, no `gen-class`/`proxy`. Interop
  syntax (`Class.`, `Class/static`, `.method`) resolves only against a shimmed
  subset of the `java.*` standard library; a class token is a name, not a loaded
  class. See [Host Interop](https://jolt-lang.github.io/docs/host-interop.html).
  To call C libraries directly, use the `jolt.ffi` foreign-function interface
  (how the db and http-client libraries bind SQLite/libpq and
  sockets/OpenSSL/zlib).
- **The `java.*` shims are not the JVM.** A shimmed class implements its JVM
  counterpart's API on Scheme, so it can look convincing while the surrounding
  runtime is not the JVM. Process and memory semantics in particular are Chez's:
  a thread does not keep the process alive after the main thread returns
  (`.setDaemon` is accepted and ignored), and there is no classloader, no JVM
  heap tuning, and no JVM GC behaviour to reason about.
- **Codepoint strings.** Strings are Chez strings — codepoint-indexed, no
  UTF-16 surrogate pairs. `(count "😀")` is 1 (JVM: 2) and `subs` never splits
  a character; only code doing UTF-16 unit arithmetic notices.
- **Regex engine.** Patterns compile through
  [irregex](https://github.com/ashinn/irregex) (vendored), not
  `java.util.regex`; common patterns work, Java-specific features can differ.
- **Coverage.** `clojure.core` is implemented function by function against the
  JVM-sourced conformance corpus — broad but not total; a namespace can load with
  most functions working and a few not yet implemented.
- **A `.jolt` extension.** A namespace's source can be `foo.jolt` as well as
  `foo.clj` or `foo.cljc`, and the three are the same language: the reader,
  analyzer, and emitter never look at the extension. `.jolt` is a marker for
  readers and tooling, saying the file uses jolt-specific interop and is not
  portable Clojure. It resolves first, so a library can ship a portable
  `foo.cljc` next to a `foo.jolt` that wins on jolt, the way `.clj` wins over
  `.cljc` on the JVM. `data_readers.jolt` works like `data_readers.clj` too.
- **Digit separators in numbers.** `1_000_000`, `0xFF_FF` and `36rR_Z` read as
  numbers; the JVM raises `Invalid number` on all three. The rule is Java's — an
  underscore must sit between two digits, never against a sign, radix marker,
  decimal point, exponent marker or `N`/`M` suffix — so `1_` and `0x_52` still
  raise. A leading underscore is still an ordinary symbol. `clojure.edn` refuses
  separators: edn's grammar has none, and a config that read only here would
  fail in every other edn reader. Additive — nothing that reads on the JVM
  changes meaning.
- **Protocol methods can carry a default body.** An fn-style `defprotocol`
  clause — `(greet-loudly ([this] (str (greet this) "!")))` — supplies a default
  any extender omitting that method resolves to. Java 8 default methods, scoped
  to the protocol: a non-extender still raises, so it is not an `Object`
  extension in disguise. Inline impls win.
- **Clojure is a terminal dependency.** jolt *is* Clojure, so
  `org.clojure/clojure` in a `deps.edn` contributes neither an artifact nor
  children. On the JVM that artifact pulls in `org.clojure/spec.alpha`, so a
  project declaring only Clojure still gets `clojure.spec.alpha`; here it has to
  be declared. See [Runtime dependencies](#runtime-dependencies).

The tracked, gated list of value-level divergences is
[test/conformance/known-divergences.edn](test/conformance/known-divergences.edn);
the prose version is [Differences from Clojure](https://jolt-lang.github.io/docs/differences.html)
on the docs site.

## Runtime dependencies

Jolt supplies `org.clojure/clojure` and `org.clojure/clojurescript` itself, so
those libraries are terminal when encountered transitively: their artifacts
and dependency trees are not acquired. Explicitly declared
`org.clojure/spec.alpha` and `org.clojure/core.specs.alpha` dependencies remain
ordinary dependencies.

Code can acquire and import dependencies while it runs with the portable
`clojurestar.deps/require-deps` macro:

```clojure
(require '[clojurestar.deps :refer [require-deps]])

(require-deps
 ["mvn:dev.weavejester/medley@1.10.0/medley.core" :as medley])
```

Literal dependency vectors need no quote; quoted vectors remain supported for
compatibility. Maven, Gist, and GitHub source-file coordinates support `:as`
and explicit `:refer` imports. An optional leading map accepts
`:mvn/local-repo` and `:gitlibs/dir`; `:cache-dir` remains a compatibility alias
for the source-file cache root. A pinned Gist file accepts either
`gist:<owner>/<id>/<file>@<revision>` or
`gist:<owner>/<id>/<revision>/<file>`; both forms use the same cache entry.
A GitHub source file accepts either
`github:<owner>/<repo>/<ref>/<path.clj|cljc>` or the equivalent
`github:<owner>/<repo>/blob/<ref>/<path.clj|cljc>` form. Refs occupy one path
segment; full commit SHAs reuse persistent cache while named refs refresh in a
new process. Selected files must be self-contained and begin with an `ns` form.
The explicit Maven option takes precedence over `JOLT_MAVEN_REPOSITORY`, which
takes precedence over `GRENADINE_MAVEN_REPOSITORY`. For Gist and GitHub source
dependencies, `JOLT_GITLIBS_DIR` takes precedence over
`GRENADINE_GITLIBS_DIR`, then `GITLIBS`; source lives under `gist/` or `github/`
in that effective root.

## Diagnostics

- **"Did you mean?"** — when a bare symbol doesn't resolve, the compile error
  lists the closest in-scope names by edit distance (current-namespace vars,
  `clojure.core` publics, and lexical locals):
  ```
  $ jolt -e '(prinltn 1)'
  Unable to resolve symbol: prinltn in this context (did you mean print, printf, println?)
  ```
- **`JOLT_DIAG=edn`** — emit an uncaught error as a single line of valid EDN to
  stderr (`:message` plus source `:line`/`:column`/`:file`; an unresolved symbol
  also carries `:type`/`:symbol`/`:suggestions`/`:ns`) so an editor or tool can
  read it back. Default output is unchanged.
- **`JOLT_CHECK`** — opt-in success-type lint (RFC 0006): each runtime-compiled
  form is run through the checker and findings print as located warnings, e.g.
  ``1:10: warning: `+` requires a number, but argument 2 is a keyword``. Off by
  default (zero cost); a checker error never breaks a compile.
- **`JOLT_DEBUG`** — verbose dependency resolution (the fetching / using-cache /
  skipping lines that are otherwise quiet) and the host static-shim drift warning.

## REPL and editor integration

```bash
jolt repl                    # a line REPL with the project's deps loaded
jolt nrepl-server [port]     # an nREPL server (default 7888) for editors
```

Both resolve the `deps.edn` in the current directory first, so the project's
source roots and native libraries are loaded — `(require '[my.ns])` works live.
`nrepl-server` writes a `.nrepl-port` file in the project dir, so CIDER / Calva /
Cursive auto-detect the port; override it with the argument or `JOLT_NREPL_PORT`.

The server runs in dev mode — calls deref their var, so redefining a function
takes effect on the next call without restarting the process. The built-in
handler speaks `clone`/`describe`/`eval`/`load-file`/`close`; everything past
that is nREPL middleware, listed in `deps.edn` under `:nrepl/middleware`.
[jolt-lang/nrepl](https://github.com/jolt-lang/nrepl) supplies both layers —
sessions and interruptible eval, plus the cider-nrepl ops an editor expects
(`info`, `complete`, the namespace browser, tests, error analysis):

```clojure
{:deps {jolt-lang/nrepl {:git/url "https://github.com/jolt-lang/nrepl"
                         :git/sha "<full-sha>"}}
 :nrepl/middleware [nrepl.middleware/default-middleware
                    cider.nrepl/cider-middleware]}
```

See [REPL-Driven Development](https://jolt-lang.github.io/docs/repl-driven-development.html).

## Compile a binary

`jolt build` ahead-of-time compiles a project into a single self-contained
executable — the runtime, `clojure.core`, the standard library, the app, and its
`deps.edn` dependencies are linked in, so the result needs no Chez install, no
JVM, and no source on disk to run.

```bash
jolt build -m myapp.core -o myapp   # compile myapp.core's -main into ./myapp
./myapp arg1 arg2                   # runs anywhere; args reach -main
```

Modes trade dynamism for speed: the default (release) build uses the proven code
generator; `--opt` also runs the inference + inlining + scalar-replacement passes
over the closed-world program; `--dev` is unoptimized. Numeric code unboxes to
raw flonum/fixnum machine ops when types are proven — by whole-program inference,
by JVM-style `^double`/`^long` hints, or by `(double x)`/`(long x)` casts where
inference can't see. See
[Building & Running](https://jolt-lang.github.io/docs/building-and-deps.html#typed-arithmetic-and-inference).

Two opt-in closed-world flags cut dispatch cost and binary size:

```bash
jolt build -m myapp.core --direct-link   # app->app calls bind directly (no var lookup)
jolt build -m myapp.core --tree-shake    # ship only code reachable from -main
```

`--tree-shake` walks the call graph across your app, its libraries, and
`clojure.core`, drops everything unreachable from `-main`, and typically removes
1–2 MB. It stays sound by bailing out — keeping everything, and naming the
library responsible — when reachable code resolves vars by name at runtime
(`eval`/`resolve`/`ns-resolve`/…). See
[RFC 0007](https://jolt-lang.github.io/docs/rfc/0007-compilation-modes-and-binary-output.html).

Built executables carry an optional startup profiler: launch one with
`JOLT_STARTUP_PROFILE=1` to get per-stage wall time, process CPU time,
collection counts, reclaimed bytes, and heap size on stderr, marked at the
native boot loader, the runtime files, each application namespace, and `-main`.
Normal launches leave it disabled and silent.

Linking a binary needs Chez's kernel development files (`libkernel.a`,
`scheme.h`) and a C compiler. They come with a from-source Chez install and with
the prebuilt jolt binary; a distro `chezscheme` package ships only the runtime,
so `build` won't link there.

## Compile a library

`jolt build --library` compiles a project into a shared object
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
jolt build --library -m libadd.core -o libadd   # => libadd.so / libadd.dylib
```

The C side `dlopen`s it, calls `jolt_library_init` once, then resolves each
entry by name with `jolt_lookup` and casts to its type;
[Native Interop](https://jolt-lang.github.io/docs/native-interop.html) has the
full example, the type keywords (the same ones `foreign-fn` uses), and the
threading limits. The same `--opt`/`--dev`/`--direct-link`/`--tree-shake` flags
apply, and the same Chez kernel development files + C compiler are required to
link.

## Documentation

Full documentation is at **[jolt-lang.github.io](https://jolt-lang.github.io)** —
[Getting Started](https://jolt-lang.github.io/docs/getting-started.html),
[Differences from Clojure](https://jolt-lang.github.io/docs/differences.html),
[Host Interop](https://jolt-lang.github.io/docs/host-interop.html),
[Native Interop (FFI)](https://jolt-lang.github.io/docs/native-interop.html),
[Writing Libraries](https://jolt-lang.github.io/docs/writing-libraries.html),
the [language specification](https://jolt-lang.github.io/docs/spec/README.html),
and the [RFCs](https://jolt-lang.github.io/docs/rfc/README.html). Every page is
listed in [`llms.txt`](llms.txt) as well.

## Contributing

Building from source, the seed and re-minting, the architecture, the Scheme
backends, and the test gates are in **[CONTRIBUTING.md](CONTRIBUTING.md)**.

## License

[Eclipse Public License 2.0](https://www.eclipse.org/legal/epl-2.0/)
