# deps.edn support — design notes

How Jolt loads pure-Clojure libraries from a `deps.edn`, and why it's built the
way it is. For how to *use* it, see [building-and-deps.md](building-and-deps.md).

Scope, decided up front:

- **git + local deps only** — no Maven/`~/.m2` resolution.
- **pure `clj`/`cljc`** — anything needing the JVM won't load or run; expected.
- **no classpath abstraction** — `require` just needs to find a dep's namespaces;
  "the classpath" is an ordered list of source directories.
- **own resolver, own reader** — `deps.edn` is read by jolt's own reader, and git
  fetch/cache is a thin shell-out to `git`; no external package manager.
- **deps-agnostic runtime core** — resolution is a CLI front-end concern, not a
  runtime one. The runtime knows nothing about `deps.edn`; it only consumes a
  list of source roots. The CLI resolves a `deps.edn` into those roots before
  running.

## How resolution works

`jolt.deps` (`jolt-core/jolt/deps.clj`) reads `deps.edn` (jolt's own reader
parses the EDN), then walks `:deps`:

- `:git/url` + `:git/sha` (+ optional `:deps/root`) → clone the sha into the git
  cache and contribute the checkout (or its `:deps/root` subdir);
- `:local/root` → the path as-is;
- `:mvn/*` → skipped with a warning;
- anything else → ignored.

git resolution shells out to `git` through `jolt.host/sh` — `git init` + remote
add + fetch + reset at the requested sha. Clones land in a global, sha-immutable
cache (`$JOLT_GITLIBS`, else `~/.jolt/gitlibs`) shared across projects, the
`tools.gitlibs` `~/.gitlibs` model.

Each resolved dependency contributes its own `:paths` (default `["src"]`) as
source roots; the walk is **breadth-first** so every top-level coordinate
registers before any transitive one — a top-level pin always wins, matching
tools.deps. The result is a de-duplicated, ordered list of directories.

Two tools.deps features are mirrored in reduced form. **Aliases**: `:aliases`
entries supply `:extra-paths`/`:extra-deps` (accumulate across the aliases
selected with `-A:a:b`) and `:main-opts` (last-wins, run with `-M:alias`).
**Tasks**: the honest subset of babashka's — a string task is a shell command, a
map task is `{:main-opts […]}`; bare Clojure expressions aren't a separate task
form.

## How the CLI ties it together

`jolt.main` (`jolt-core/jolt/main.clj`) is the CLI dispatch. Driven by `cli.ss`,
it resolves the project (`jolt.deps/resolve-project`), prepends the resolved
roots, and de-sugars the argv into a run:

- `run -m NS args` → load `NS`, call its `-main`;
- `run FILE` → load the file;
- `-M:alias` → run the alias's `:main-opts`;
- `-A:alias` → add the alias's paths/deps, then run the rest;
- `repl` → a line REPL;
- `path` → print the resolved roots;
- `build -m NS [-o OUT] [--opt|--dev]` → AOT-compile the app into a standalone binary;
- `<task>` → run a `deps.edn` `:tasks` entry.

The resolver lives in the overlay alongside the runtime, but the runtime's only
dependency interface is the list of source roots it's handed.

## Native libraries

A library that binds C declares the shared objects it needs under `:jolt/native`,
so `jolt.main` loads them before the namespace is required and its `foreign-fn`
bindings resolve. Each entry is a map — `{:name "sqlite3" :darwin
["libsqlite3.0.dylib" …] :linux ["libsqlite3.so.0" …]}` — with optional
`:optional true` (absence is fine, a feature-gated dep) and `:process true` (use
the running process's own symbols, e.g. libc sockets, no external file). A
project inherits its dependencies' `:jolt/native`.

### Static vs dynamic linking

When you `joltc build`, a native lib is **statically linked** into the binary by
default if the spec carries a `:static` archive — so the executable calls the C
code with no shared object present at runtime. Add `:static` alongside the runtime
candidates:

```clojure
{:name "sqlite3"
 :static {:archive "/opt/homebrew/lib/libsqlite3.a"}  ; or {:lib "sqlite3" :libdir "/usr/lib"}
 :darwin ["libsqlite3.0.dylib"]   ; still used by `run`/`repl` and by --dynamic
 :linux  ["libsqlite3.so.0"]}
```

`:static {:archive PATH}` force-loads the whole `.a` and is the reliable
cross-platform form. `:static {:lib NAME :libdir DIR}` links `-lNAME` (with a
`-Bstatic` preference on Linux); on macOS, which has no `-Bstatic`, prefer the
archive form. A spec with no `:static` (or a build passed `--dynamic`, or
`:jolt/build {:dynamic-natives true}`) keeps the old behavior — the shared object
is loaded at startup via `load-shared-object`.

Static linking needs a C compiler (`cc`) on `PATH` at build time (plus the C libs
the Chez kernel links — lz4, zlib, ncurses). The distributed `joltc` bundles the
Chez kernel, so it re-links the launcher stub with the archive baked in — no
external Chez, just `cc`. Without a `cc`, a `:static` lib fails with a message
pointing you to install one or pass `--dynamic`. Keep a `:darwin`/`:linux`
candidate on any `:static` spec so `run`/`repl` (which have no static binary) can
still load it.

## Standalone binaries

`joltc build -m NS` compiles the app and every library into one executable (the
runtime + compiler are baked in). Resolved `:jolt/native` libs are statically
linked in (or loaded at startup — see [Native libraries](#native-libraries)), so
an FFI app — sockets, SQLite — runs with no jolt or Chez on the path.

Output goes under the project's `target/`, cargo-style: `target/release/<project>`
by default and with `--opt`, `target/debug/<project>` with `--dev` (the
`<name>.build` scratch dir sits beside it). `-o PATH` overrides — absolute as-is,
relative against the project dir. Paths resolve against the project (`JOLT_PWD`),
not the CLI's cwd, since `bin/joltc` runs from the jolt repo.

`:jolt/build {:embed ["resources" …]}` bakes those directories' files into the
binary; `io/resource` serves them from the image with no files on disk. Resources
not embedded resolve at runtime against `JOLT_PWD` (or the cwd), so the
ship-the-binary-with-its-`resources/`-dir model also works. Files read through
`io/file` (e.g. a `config.edn` a config library loads) stay external by design —
edit them without rebuilding.

A standalone build needs Chez's kernel dev files (`libkernel.a`, `scheme.h`) and
a C compiler; `JOLT_CHEZ_CSV` overrides the auto-detected `csv<ver>/<machine>`
dir. `--opt` turns on the inference/flatten/scalar-replace passes; the default
`release` mode is const-fold only.

`--direct-link` (or `:jolt/build {:direct-link true}`) opts into a closed world: a
call between the app's own functions binds to its target directly, skipping the var
lookup and generic dispatch a runtime call pays — at the cost of runtime
redefinition of those vars and `eval`/`load-string`. It's off by default, so
ordinary builds (including `release` and `--opt`) stay dynamically linked. A var
marked `^:redef` or `^:dynamic` stays indirect even under `--direct-link`, and calls
into `clojure.core` stay indirect in every mode.

## Tree-shaking

`--tree-shake` (or `:jolt/build {:tree-shake true}`) ships only the code reachable
from `-main`. The build constructs one call graph spanning the app, every resolved
library, and the `clojure.core`/stdlib prelude, then keeps `-main`, every
side-effecting top-level form (so a `defmethod`/`defrecord`/protocol registration
keeps its targets live), and everything reachable from those — dropping the rest. A
reference counts whether it's a call or a value (`#'x`, a fn passed to `map`, a fn
stored in a map): any reference keeps its target live, so nothing reachable is ever
dropped. An app that never compiles at runtime (no reachable `eval`/`load-string`)
also drops the analyzer and back end from the binary. Typical savings are 1–2 MB;
behaviour is unchanged.

**It bails — keeps everything — when reachable code resolves a var by name at
runtime** (`eval`, `resolve`, `ns-resolve`, `requiring-resolve`, `find-var`,
`intern`, `load-string`, `load-file`). A static call graph can't follow a runtime
`resolve`, so dropping anything would be unsound. The build prints which definitions
forced the bail:

```
jolt build: tree-shake skipped (reachable code resolves vars at runtime):
  selmer.filters/generate-json -> clojure.core/resolve
  clojure.tools.logging/call-str -> clojure.core/ns-resolve
```

These are almost always libraries, not your code — `resolve` is how mature Clojure
libraries implement plugin systems and optional integrations (a logging backend
chosen at runtime, a template filter that lazily loads an optional dependency). On
the JVM that costs nothing; in a closed-world binary it defeats reachability. To make
an app tree-shakeable, keep runtime resolution off the *reachable* path: a backend
that's fixed on jolt can be referenced directly rather than resolved (the jolt
`tools.logging` port dropped the JVM's dynamic factory selection for exactly this),
and an optional integration you don't use can be dropped or hard-wired. Unreached
`resolve`-using code is shaken away like anything else — only resolution on the live
path triggers the bail.

The closed-world soundness model follows Stalin's dead-code analysis: in a program
with no `eval`, a definition is live iff it is referenced (called or as a value) from
a root, transitively.

## Limitations

- Pure `clj`/`cljc` only — JVM interop, host classes, and unimplemented
  `clojure.core` corners fail. Coverage is per-function: a namespace can load with
  most functions working and a few not.
- Source only; compiled `.class` files in a git dep are ignored.
- git `:git/sha` must be a full SHA (`git fetch` can't resolve a short one).

## Stack traces

An uncaught error prints the message, the top-level source location, and — when
frames are available — a `trace:` backtrace. In an AOT `jolt build --direct-link`
binary the frames map to `ns/name (file:line)`; on the runtime eval path they are
the surviving fn names. Tail-call optimization erases tail-called frames, so the
default trace shows only the non-tail spine.

Set `JOLT_TRACE=1` to opt into a fuller **tail-frame history**. Each compiled fn
then records itself on entry into a bounded ring-of-rings buffer, so the trace
recovers TCO-elided frames (including the immediate error site) while a tight tail
loop stays bounded and its non-tail caller context is preserved. It costs a small
per-call overhead, so it is off by default and never emitted into a built binary.

## Conformance

The known-working libraries (see [libraries.md](libraries.md)) and the
[examples](https://github.com/jolt-lang/examples) exercise real pure-`cljc` git
libraries end to end — resolving them from git, loading their namespaces, and
running sample calls. A library fails when it relies on something Jolt doesn't
provide — JVM interop, or a regex feature like Unicode property classes
(`\p{…}`).
