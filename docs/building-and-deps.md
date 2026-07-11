# Building and dependencies

How to run Jolt from source and how to pull Clojure libraries into a project.

## Running

```bash
git clone https://github.com/jolt-lang/jolt.git
cd jolt
git submodule update --init   # vendor/sci (used by the SCI bootstrap tests)
bin/joltc -e '(println "hello")'
```

There is **no build step**. `bin/joltc` (`host/chez/cli.ss`) loads the
checked-in bootstrap seed (`host/chez/seed/{prelude,image}.ss`) plus the spine
and compiles+evals on Chez (read → analyze → IR → emit → eval), so a fresh
clone runs immediately. The whole `.clj` standard library
(`clojure.string`/`set`/`walk`/`edn`/`pprint`/…) and `clojure.core` are part of
the overlay, so they're always available.

`bin/joltc` is both the runtime (REPL, file/expr runner) and the dependency
front-end (`deps.edn` resolution, see below). A run with no `deps.edn` never
touches the resolver.

The bootstrap seed is **checked in**. After changing a seed source — the reader
(`host/chez/reader.ss`), the analyzer/IR/backend (`jolt-core/jolt/*.clj`), or the
`clojure.core` overlay (`jolt-core/clojure/core/*.clj`) — re-mint the seed with
`make remint` (it iterates `host/chez/bootstrap.ss` to a byte-fixpoint), or
`make selfhost` fails. Runtime-only `host/chez/*.ss` shims don't need a re-mint.

## How namespaces are found

`(require ...)` resolves a namespace to a file by searching an ordered list of
source roots — the stdlib first, then any extra roots — trying `<ns>.clj` then
`<ns>.cljc` (dots become directories, dashes become underscores). Extra roots
come from:

- `JOLT_PATH` — a colon-separated list of directories (like a classpath), applied
  at runtime;
- the `:paths` option to `init` when embedding Jolt as a library.

If a namespace isn't found on any root, the loader falls back to the stdlib in
the overlay — that's how `clojure.string` and friends resolve when you run
outside the source tree.

So you can point Jolt at a directory of Clojure source with no deps machinery at
all:

```bash
JOLT_PATH=/path/to/lib/src bin/joltc run myfile.clj
```

## Dependencies via deps.edn

`bin/joltc` reads a `deps.edn` in the current directory, fetches its
dependencies, and prepends the resolved source directories to the source roots
for the run. The CLI commands (`jolt.deps` + `jolt.main`):

```bash
bin/joltc run -m NS [args]      # resolve deps.edn, load NS, call its -main
bin/joltc run FILE              # resolve deps.edn, load a Clojure file
bin/joltc -M:alias [args]       # run the alias's :main-opts
bin/joltc -A:alias [args]       # add the alias's paths/deps, then run the rest
bin/joltc repl                  # start a line REPL (project deps + native libs loaded)
bin/joltc nrepl-server [port]   # start an nREPL server (default 7888) for editors
bin/joltc path                  # print the resolved source roots (':'-joined)
bin/joltc <task>                # run a deps.edn :tasks entry
```

Example `deps.edn`:

```clojure
{:paths ["src"]
 :deps {weavejester/medley {:git/url "https://github.com/weavejester/medley"
                            :git/sha "<full-sha>"}
        my/helpers          {:local/root "../helpers"}}}
```

```bash
bin/joltc run -m myapp.main
```

### What's supported

- **git deps** — `{:git/url … :git/sha …}` (use a full SHA; `git fetch` can't
  resolve a short one), with an optional `:deps/root` for a subdirectory.
  Transitive deps from each dependency's own `deps.edn` are resolved too.
- **local deps** — `{:local/root "../path"}`.
- The project's own `:paths` (default `["src"]`) are included.
- **aliases** — `:aliases {:dev {:extra-paths ["dev"] :extra-deps {…}
  :main-opts ["-e" "…"]}}`, selected with `-A:dev` (or several: `-A:dev:test`).
  `:extra-paths`/`:extra-deps` accumulate across selected aliases;
  `:main-opts` is last-wins and runs via `-M:alias`.
- **tasks** — `:tasks {clean "rm -rf target" test {:main-opts ["-m" "…"]}}`.
  A string task is a shell command; a map task runs jolt with its `:main-opts`.
  Run one with `bin/joltc <taskname>`.

Resolution is breadth-first, so a top-level coordinate always beats a transitive
one for the same lib.

Git clones land in a global, sha-immutable cache shared across projects —
`$JOLT_GITLIBS`, else `~/.jolt/gitlibs`.

### What's not

- **No Maven.** `:mvn/version` deps are skipped with a warning — git and local
  only.
- **Pure `clj`/`cljc` only.** A library that needs the JVM (Java interop, host
  classes) or a `clojure.core` feature Jolt doesn't implement will fail to load
  or fail at a call. Coverage is per-function: a namespace can load with most
  functions working and a few not.

See [`tools-deps.md`](tools-deps.md) for the design rationale.

## Building binaries

`jolt build` compiles a namespace and its dependencies into a standalone binary:

```bash
JOLT_PWD=/path/to/project bin/joltc build -m my.app
```

The binary contains the runtime + app forms + native launcher — no Jolt source or Chez
install needed on the target machine (a C compiler and Chez kernel dev files are needed
at build time only).

### Build modes

Three modes control which optimization passes apply. A mode is selected by the CLI flag
`--opt`, `--dev`, or by the `:jolt/build {:opt true}` key in `deps.edn`; the default is
`release`. CLI flags win over `deps.edn`.

| Mode | `--opt` / `{:opt true}` | `--dev` | Release (default) |
|------|--------------------------|---------|-------------------|
| const-fold | yes | yes | yes |
| numeric-annotate | yes | yes | yes |
| type inference (run-inference) | yes | - | yes |
| record-shape + protocol-method caches | yes | - | yes |
| inline + scalar-replace fixpoint | with `--direct-link` | - | - |

`--opt` enables the annotation-producing passes (type inference, PIC/devirtualization,
record-ctor caches) for better runtime performance without committing to a closed world.
Add `--direct-link` to also enable the inline + scalar-replace fixpoint — this gives the
best performance but gives up runtime redefinition of direct-linked vars. For fully
closed-world binaries, combine `--opt --direct-link --tree-shake` to drop dead code.

`--dev` produces a debug binary under `target/debug/` (const-fold + numeric annotate
only), typically used during development for faster build times.

### deps.edn build options

The `:jolt/build` map in `deps.edn` accepts these keys:

- **`:opt true`** — build in optimized mode (like `--opt`)
- **`:direct-link true`** — closed-world direct linking (like `--direct-link`)
- **`:tree-shake true`** — drop unreachable library code (like `--tree-shake`)
- **`:embed [dirs]`** — bake resource files into the binary so `io/resource` resolves
  with no files on disk
- **`:dynamic-natives true`** — load native shared objects at runtime instead of
  statically linking

Example:

```clojure
{:paths ["src"]
 :jolt/build {:opt true
              :direct-link true
              :tree-shake true
              :embed ["resources"]}}
```
