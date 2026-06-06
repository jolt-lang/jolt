# Loading Clojure libraries via deps.edn (git deps, on jpm)

Research notes for letting a Jolt project pull pure-Clojure libraries through a
`deps.edn` and `(require ...)` them. Scope is deliberately narrow:

- **git deps only** — no Maven/`~/.m2` resolution.
- **pure `clj`/`cljc`** — anything needing the JVM (Java interop, host classes)
  won't load or run, and that's expected.
- **no classpath machinery** — we just need `require` to find a dep's namespaces
  at dev time (REPL) so they can be compiled into the image at build time.
- **piggyback on jpm** — reuse jpm's existing git fetch + cache; don't write a
  package manager.

Nothing here is implemented yet. Everything below was verified against the
installed jpm and a real lib (medley).

## How jpm handles dependencies today

jpm's package code lives in `/opt/homebrew/lib/janet/jpm/pm.janet`. The relevant
pieces:

- **`resolve-bundle`** normalizes a dep spec to `{:url :tag :type :shallow}`. It
  accepts a table (`:url`/`:repo`, and `:tag`/`:sha`/`:commit`/`:ref`) or a
  `"url::type::tag"` string. So a deps.edn `{:git/url … :git/sha …}` maps onto it
  directly.
- **`download-bundle url :git tag shallow`** clones into a cache dir and returns
  the path. The cache is `(find-cache)` = `<modpath>/.cache`, and the per-dep
  directory is a deterministic id: `git_<tag>_<sanitized-url>`. Under the hood
  `download-git-bundle` does `git init` + `remote add origin` + fetch + reset to
  the tag/sha (plus submodules). Pure git — **no build step**.
- **`bundle-install`** is the part we *don't* want: after downloading it does
  `(require-jpm "./project.janet")` and runs build/install rules. A Clojure lib
  has no `project.janet`, so this would fail. The clone/cache half
  (`download-bundle`) is cleanly separable from this build half.

So jpm already gives us git resolution + a content-addressed cache for free; we
just skip its build phase.

### Verified

```janet
(import jpm/config :as cfg)
(import jpm/pm :as pm)
(cfg/load-default)                 ; sets gitpath, cache defaults, etc.
(setdyn :modpath "<project>/jpm_tree")   ; where the cache lives
(def s (pm/resolve-bundle {:url "https://github.com/weavejester/medley"
                           :tag "1.0.0" :shallow true}))
(pm/download-bundle (s :url) (s :type) (s :tag) (s :shallow))
;; => "<modpath>/.cache/git_1.0.0_https___github.com_weavejester_medley"
;; with source at <that>/src/medley/core.cljc
```

`cfg/load-default` is required — calling `download-bundle` without jpm's config
dyns set fails in its `shell` helper.

And the source loads and runs in Jolt: `(medley.core/abs -5)` → `5`,
`(medley.core/find-first odd? [2 4 5])` → `5` (coverage is per-function; a few
hit interpreter gaps, which is fine).

## The shape

1. **Resolve.** Read `deps.edn`, take the `:deps` whose specs are git
   (`:git/url` + `:git/sha`/`:git/tag`). For each, `resolve-bundle` +
   `download-bundle` into the project's `jpm_tree/.cache`. Read each cloned
   dep's own `deps.edn` and recurse for transitive git deps. (`:local/root`
   deps are even simpler — just a path, no fetch.)
2. **Collect source roots.** A dep's source dirs are its deps.edn `:paths`
   (default `["src"]`), so a root is `<clone-dir>/<path>`. The result is just an
   ordered list of directories — not a classpath abstraction, a list of roots.
3. **Teach the loader the roots.** `evaluator.janet/ns->path` currently hardcodes
   `src/jolt/<ns>.clj`. Generalize `maybe-require-ns` to, after the stdlib path,
   search each dep root for `<ns>.clj` then `<ns>.cljc`. `src/jolt/` stays first
   so the stdlib always wins. (Two small changes: a root list in the ctx, and
   trying `.cljc`.)
4. **Dev vs build.**
   - *Dev:* on REPL/CLI start, if `deps.edn` is present, resolve once (cache keyed
     on a hash of deps.edn so it's a no-op when unchanged) and register the roots.
     `(require '[medley.core])` then just works.
   - *Build:* the dep namespaces a project actually uses get compiled into the
     image the same way the embedded `jolt.nrepl` source is today — load them at
     build time so the shipped binary needs neither the deps nor jpm.

## Why this fits "no classpath"

We never construct a Java-style classpath or deal with jar extraction, ordering
semantics, or version conflict resolution. Resolution is a tree walk over git
deps; "the classpath" is just the list of `src` dirs of the clones. The loader
already does path-based namespace lookup — we widen it from one root to a few.

## Integration with jpm

jpm stays the build tool for the Jolt binary; this lives beside it and *calls
into* `jpm/pm` for the git/cache work (import the module at dev/build time — jpm
is always present then; the shipped binary doesn't need it). Open question:
whether to depend on jpm's internal `pm` functions (not a stable public API) or
shell `git` ourselves into the same cache layout. Reusing `pm` is less code and
shares jpm's cache; shelling git is ~15 lines and avoids the internal-API risk.
Leaning toward reusing `pm` first.

## Limitations

- Pure `clj`/`cljc` only. JVM interop, host classes, and unimplemented
  `clojure.core` corners fail — expected, not a goal.
- Per-function coverage: a namespace can load with most functions working and a
  few not.
- Source only; compiled `.class` files (if any in a git dep) are ignored.

## Plan

1. **Loader roots.** Generalize `maybe-require-ns` to search an ordered root list
   and try `.clj` + `.cljc`; add a `JOLT_PATH`/`--path` to set roots by hand.
   Point it at a checked-out lib's `src` and load it. Independently testable,
   unblocks the rest.
2. **Resolve git deps via jpm.** Read `deps.edn`, resolve `:git/*` (+
   `:local/root`) through `jpm/pm` into `jpm_tree/.cache`, recurse for transitive
   git deps, register the roots. `jolt deps` to resolve/print; auto on startup
   when `deps.edn` exists.
3. **Build-time compile-in.** Fold the used dep namespaces into the image at
   build (as with embedded `jolt.nrepl`).
4. **Conformance.** Pull a few popular pure-`cljc` git libs, see what loads/runs,
   and drive interpreter gaps from the failures — same loop as the
   clojure-test-suite battery.
