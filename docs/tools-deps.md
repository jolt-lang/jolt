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
- `<task>` → run a `deps.edn` `:tasks` entry.

The resolver lives in the overlay alongside the runtime, but the runtime's only
dependency interface is the list of source roots it's handed.

## Limitations

- Pure `clj`/`cljc` only — JVM interop, host classes, and unimplemented
  `clojure.core` corners fail. Coverage is per-function: a namespace can load with
  most functions working and a few not.
- Source only; compiled `.class` files in a git dep are ignored.
- git `:git/sha` must be a full SHA (`git fetch` can't resolve a short one).

## Conformance

The known-working libraries (see [libraries.md](libraries.md)) and the
[examples](https://github.com/jolt-lang/examples) exercise real pure-`cljc` git
libraries end to end — resolving them from git, loading their namespaces, and
running sample calls. A library fails when it relies on something Jolt doesn't
provide — JVM interop, or a regex feature like Unicode property classes
(`\p{…}`).
