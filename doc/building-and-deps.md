# Building and dependencies

How to build Jolt from source and how to pull Clojure libraries into a project.

## Building

```bash
git clone https://github.com/jolt-lang/jolt.git
cd jolt
git submodule update --init   # vendor/sci (used by the SCI bootstrap tests)
jpm build
```

This produces two executables under `build/`:

- **`jolt`** — the runtime: REPL, file/expr runner, nREPL server. The whole `.clj`
  standard library (`clojure.string`/`set`/`walk`/`edn`/`zip`, `jolt.http`/
  `interop`/`shell`/`nrepl`) is baked into this binary at build time, so it loads
  from any directory — the build artifact is self-contained. (`clojure.core` is
  built into the runtime in Janet and auto-referred, so it's always available.)
- **`jolt-deps`** — a separate tool that resolves a `deps.edn` (see below). It
  sits beside the runtime the way `jpm` sits beside `janet`; the runtime itself
  knows nothing about deps.edn.

Needs `jpm` and a recent Janet — developed and CI-tested against **1.41**. The
futures and core.async layers use Janet's threaded `ev/` channels (`ev/thread`,
`ev/thread-chan`), so older Janets may not run the full suite.

`jpm build` doesn't always notice source changes; run `jpm clean && jpm build`
after editing `src/` to be sure the binaries are current. `jpm test` runs against
the source directly, so it never goes stale.

## How namespaces are found

`(require ...)` resolves a namespace to a file by searching an ordered list of
source roots — the stdlib first, then any extra roots — trying `<ns>.clj` then
`<ns>.cljc` (dots become directories, dashes become underscores). Extra roots
come from:

- `JOLT_PATH` — a colon-separated list of directories (like a classpath), applied
  at runtime;
- the `:paths` option to `init` when embedding Jolt as a library.

If a namespace isn't found on any root, the loader falls back to the stdlib baked
into the binary — that's how `clojure.string` and friends resolve when you run
the binary outside the source tree.

So you can point Jolt at a directory of Clojure source with no deps machinery at
all:

```bash
JOLT_PATH=/path/to/lib/src build/jolt myfile.clj
```

## Dependencies via deps.edn

`jolt-deps` reads a `deps.edn` in the current directory, fetches its
dependencies, and runs `jolt` with the resolved source directories on
`JOLT_PATH`.

```bash
jolt-deps path             # print the resolved roots (':'-joined)
jolt-deps run FILE [args]  # resolve, then run `jolt FILE …`
jolt-deps repl             # resolve, then start a REPL
jolt-deps -e EXPR [args]   # resolve, then evaluate EXPR
```

`jolt-deps` launches the `jolt` binary it finds on `PATH` (override with
`$JOLT_BIN`).

Example `deps.edn`:

```clojure
{:paths ["src"]
 :deps {weavejester/medley {:git/url "https://github.com/weavejester/medley"
                            :git/tag "1.0.0"}
        my/helpers          {:local/root "../helpers"}}}
```

```bash
jolt-deps run -m myapp.main
```

### What's supported

- **git deps** — `{:git/url … :git/tag …}` or `{:git/url … :git/sha …}` (use a
  full SHA; `git fetch` can't resolve a short one). Transitive deps from each
  dependency's own `deps.edn` are resolved too.
- **local deps** — `{:local/root "../path"}`.
- The project's own `:paths` (default `["src"]`) are included.

Resolution reuses jpm's git fetch and cache (a dependency is cloned once into
`jpm_tree/.cache` and reused). Resolved roots are cached on a hash of `deps.edn`,
so an unchanged `deps.edn` doesn't re-fetch.

### What's not

- **No Maven.** `:mvn/version` deps are ignored — git and local only.
- **Pure `clj`/`cljc` only.** A library that needs the JVM (Java interop, host
  classes) or a `clojure.core` feature Jolt doesn't implement will fail to load
  or fail at a call. Coverage is per-function: a namespace can load with most
  functions working and a few not.

### Bundling into one file

`jolt uberscript OUT.clj -m NS` (or `jolt-deps uberscript …`, which resolves deps
first) bundles `NS` and every namespace it requires — your code plus its
dependencies — into a single `.clj` in dependency order, ending with a call to
`NS/-main`. The result runs on a plain `jolt` with no `JOLT_PATH`, no deps
fetched, and no jpm:

```bash
jolt-deps uberscript app.clj -m myapp.main
jolt app.clj arg1 arg2
```

See [`tools-deps.md`](tools-deps.md) for the design rationale.
