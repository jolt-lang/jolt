# deps.edn support — design notes

How Jolt loads pure-Clojure libraries from a `deps.edn`, and why it's built the
way it is. For how to *use* it, see [building-and-deps.md](building-and-deps.md).

Scope, decided up front:

- **git + local deps only** — no Maven/`~/.m2` resolution.
- **pure `clj`/`cljc`** — anything needing the JVM won't load or run; expected.
- **no classpath abstraction** — `require` just needs to find a dep's namespaces;
  "the classpath" is an ordered list of source directories.
- **piggyback on jpm** — reuse jpm's git fetch + cache; don't write a package
  manager.
- **deps-agnostic runtime core** — resolution is a CLI front-end concern, not a
  runtime one. The `jolt` *runtime* knows nothing about deps.edn; it only reads
  source roots from `JOLT_PATH`. The `jolt` *CLI* resolves a deps.edn into that
  env var before running, in a module (`deps.janet`) that loads `jpm` lazily.
  (This was a separate `jolt-deps` binary originally; it was folded into `jolt`
  for a single-binary UX — the code boundary stayed, only the executable merged.
  A back-compat `jolt-deps` shim still ships and forwards to `jolt`.)

## How jpm handles dependencies

jpm's package code (`jpm/pm.janet`) splits into a fetch half and a build half,
and we use only the first:

- **`resolve-bundle`** normalizes a dep spec to `{:url :tag :type :shallow}`,
  accepting `:url`/`:repo` + `:tag`/`:sha`/`:commit`/`:ref`. A deps.edn
  `{:git/url … :git/sha …}` maps straight onto it.
- **`download-bundle url :git tag shallow`** clones into a content-addressed cache
  (`<modpath>/.cache/git_<tag>_<sanitized-url>`) and returns the path —
  `git init` + `remote add` + fetch + reset, plus submodules. No build step.
- **`bundle-install`** is the half we skip: it then runs `project.janet` build
  rules, which a Clojure lib doesn't have. It's cleanly separable from the clone.

So jpm gives us git resolution and a cache for free; calling `download-bundle`
needs `jpm/config/load-default` first (it sets `gitpath` and the cache dyns).

## How it works

`src/jolt/deps.janet` reads `deps.edn` (Janet parses it directly — EDN and Janet
syntax overlap for the `:deps`/`:paths` subset), then walks `:deps`:

- `:git/url` (+ `:git/sha` or `:git/tag`) → `resolve-bundle` + `download-bundle`
  into `jpm_tree/.cache`;
- `:local/root` → the path as-is;
- `:mvn/*` and anything else → ignored.

Each resolved dependency contributes its own `:paths` (default `["src"]`) as
source roots; the walk is **breadth-first** so every top-level coordinate
registers before any transitive one — a top-level pin always wins, matching
tools.deps, and a coordinate conflict warns on stderr naming both. The result
is a de-duplicated, ordered list of directories. `resolve-deps-cached` memoizes
that list in the project-local `.cpcache/jolt-deps.jdn`, keyed on a hash of the
project `deps.edn` + the user-level `deps.edn` + the selected aliases. jpm is
loaded lazily (`require`, not `import`) so it's pulled in only when resolving —
never embedded in a built binary.

Three tools.deps features are mirrored in reduced form. **Aliases**: `:aliases`
entries supply `:extra-paths`/`:extra-deps` (accumulate across the aliases
selected with `-A:a:b`) and `:main-opts` (last-wins, run with `-M:alias`).
**User config**: a `deps.edn` under `$JOLT_CONFIG` (else
`$XDG_CONFIG_HOME/jolt`, else `~/.jolt`) merges beneath the project file,
per key, project wins. **Tasks**: the honest subset of babashka's — a string
task is a shell command, a map task is `{:main-opts […] :doc "…"}`; bare
Clojure expressions aren't supported because the reader hands back parsed
data, and round-tripping it to source isn't worth the fragility.

Clones default to a global sha-immutable cache (`$JOLT_GITLIBS`, else
`<config-dir>/gitlibs`) shared across projects, the `tools.gitlibs`
`~/.gitlibs` model; per-project trees remain available by passing `tree`
explicitly.

The loader (`evaluator.janet/find-ns-file`) resolves a namespace by searching the
context's `:source-paths` in order (the stdlib `src/jolt` first), trying `<ns>.clj`
then `<ns>.cljc`. Extra roots come from `JOLT_PATH` or `init`'s `:paths` option.

The `jolt` CLI (`src/jolt/main.janet`, `resolve-deps-argv`) ties it together: on
a deps subcommand — or any runnable command in a directory that has a `deps.edn`
— it resolves the roots, sets `JOLT_PATH`/`JOLT_APP_PATHS`, and de-sugars the
argv into a plain runtime command (`-M:alias` → the alias `:main-opts`, `run
FILE` → `FILE`, …) that the normal dispatch then runs. `main.janet` imports
`deps.janet`, so the resolver ships in the `jolt` binary; but `deps.janet` loads
`jpm` lazily, and the runtime modules (`api`/`backend`/RT) never import it, so an
app baked from its own `jolt/api` entry doesn't link it. The runtime's only
dependency interface remains that one env var.

`jolt uberscript` bundles a namespace and everything it requires into one
standalone `.clj`. It requires the entry namespace and uses the order in which
the loader finishes loading files — a dependency finishes before the file that
required it, so the order is topological — then concatenates that source. The
baked-in stdlib is excluded (it's part of the runtime, not bundled).

Gotcha worth remembering: the `jolt` CLI's context is built into its image at
build time, so `JOLT_PATH` is applied at runtime in `main`, not in `init` (whose
env read would be frozen at build).

## Limitations

- Pure `clj`/`cljc` only — JVM interop, host classes, and unimplemented
  `clojure.core` corners fail. Coverage is per-function: a namespace can load with
  most functions working and a few not.
- Source only; compiled `.class` files in a git dep are ignored.
- git `:git/sha` must be a full SHA (`git fetch` can't resolve a short one).

## Conformance

`test/integration/deps-conformance-test.janet` resolves a few real pure-`cljc`
git libraries and reports whether their namespaces load and a sample call works.
It's network-gated behind `JOLT_CONFORMANCE=1` so CI stays offline. Use it to
check a library against the current interpreter, and to drive fixes for whatever
gap a failure points at (the same loop as the clojure-test-suite battery). A
library fails when it relies on something Jolt doesn't provide — JVM interop, or
a regex feature like Unicode property classes (`\p{…}`).

## Not yet

- **Compiling deps into a binary image.** `uberscript` already produces a
  standalone `.clj`; baking a project's dependencies directly into a custom
  executable image is a heavier variant that isn't implemented.

## Janet dependencies: `:jpm/module`

A jolt project can depend on janet libraries. jpm owns their installation;
`deps.edn` declares the requirement and `jolt` verifies it at resolve time:

```clojure
:deps {janet/spork-http {:jpm/module "spork/http"
                         :jpm/install "spork"}}
```

- `:jpm/module` — the janet module path that must be importable.
- `:jpm/install` (optional) — the jpm package to install when it isn't;
  `jolt` runs `jpm install <name>` once, then re-checks. Without it the resolve
  fails with the install hint.

A `:jpm/module` dep contributes no source roots. At runtime the `janet.*`
interop bridge autoloads the module on first reference
(`janet.spork.http/server`, …), so nothing else is needed.
