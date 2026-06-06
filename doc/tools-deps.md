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
- **separate tool** — resolution lives in `jolt-deps`, beside the runtime, the
  way `jpm` sits beside `janet`. The `jolt` runtime knows nothing about deps.edn.

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
source roots, and we recurse into its `deps.edn` for transitive deps. The result
is a de-duplicated, ordered list of directories. `resolve-deps-cached` memoizes
that list in the tree keyed on a hash of `deps.edn`, so an unchanged file doesn't
re-fetch. jpm is loaded lazily (`require`, not `import`) so it's pulled in only
when resolving — never embedded in a built binary.

The loader (`evaluator.janet/find-ns-file`) resolves a namespace by searching the
context's `:source-paths` in order (the stdlib `src/jolt` first), trying `<ns>.clj`
then `<ns>.cljc`. Extra roots come from `JOLT_PATH` or `init`'s `:paths` option.

`jolt-deps` (`src/jolt/deps_cli.janet`, its own `declare-executable`) ties it
together: it resolves the roots and runs the `jolt` binary with them on
`JOLT_PATH`. The runtime's only dependency interface is that env var.

Gotcha worth remembering: the `jolt` CLI's context is built into its image at
build time, so `JOLT_PATH` is applied at runtime in `main`, not in `init` (whose
env read would be frozen at build).

## Limitations

- Pure `clj`/`cljc` only — JVM interop, host classes, and unimplemented
  `clojure.core` corners fail. Coverage is per-function: a namespace can load with
  most functions working and a few not.
- Source only; compiled `.class` files in a git dep are ignored.
- git `:git/sha` must be a full SHA (`git fetch` can't resolve a short one).

## Status

- **Loader source roots** — done. `find-ns-file` + `:source-paths`, `.clj`/`.cljc`,
  `JOLT_PATH`/`:paths`. Test: `test/integration/deps-loader-test.janet`.
- **Resolve git/local deps via jpm** — done. `deps.janet` + the `jolt-deps` tool.
  Test: `test/integration/deps-resolve-test.janet`.
- **Bundling (uberscript)** — done. `jolt uberscript OUT -m NS` requires `NS`,
  records the load order the loader emits (a dependency finishes loading before
  the file that required it, so it's topological), and concatenates the source
  into one `.clj` that runs on a plain `jolt` — no deps, no jpm. `jolt-deps
  uberscript` resolves first. Test: `test/integration/uberscript-test.janet`.
  (This is the practical form of "compile the used namespaces into the build";
  embedding into a custom binary image is a heavier variant we haven't needed.)
- **Conformance** — harness in place:
  `test/integration/deps-conformance-test.janet` resolves real pure-`cljc` git
  libs and reports load/run status (network-gated behind `JOLT_CONFORMANCE=1`, so
  CI stays offline). First run:
  - `medley` — loads and runs.
  - `cuerdas` — now loads (it used `\{`, a one-char literal the reader rejected;
    fixed). A function (`kebab`) still hits a separate runtime gap — a smaller,
    per-function issue rather than a whole-namespace failure.
  - `stuartsierra/dependency` — `Long/MAX_VALUE`: JVM interop, so out of scope
    (it isn't actually pure-`cljc`).

  The loop from here is the same as the clojure-test-suite battery: add libs,
  see what breaks, fix interpreter/reader gaps.
