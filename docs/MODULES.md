# Module map

Where things live and what to read before changing them. Start here to answer
"where does feature X live?" and "what else do I need to touch?"

## Areas

| Area | Directory | Responsibility | Re-mint? |
| --- | --- | --- | --- |
| Chez runtime | `host/chez/*.ss` | The substrate: value model, persistent collections, seqs, vars/namespaces, host interop, native `clojure.core` shims, regex, FFI, IO, the **reader**. Composed by `rt.ss`. | only `reader.ss` |
| Compiler | `jolt-core/jolt/*.clj` | analyzer → IR → backend, the optimization passes, the CLI, the deps resolver, nREPL. Baked into the seed. | **yes** |
| `clojure.core` overlay | `jolt-core/clojure/core/NN-*.clj` | Portable `clojure.core` in dependency-ordered tiers (`00-syntax` … `50-io`); the `NN` prefix *is* the load order. | **yes** |
| Stdlib | `stdlib/clojure/*.clj` | Lazily-loaded portable namespaces (string/set/walk/edn/pprint/zip/test/data). | no |
| Build & tooling | `host/chez/build.ss`, `emit-image.ss`, `compile-eval.ss`, `loader.ss`, `cli.ss`, `bootstrap.ss` | AOT binary build, cross-compile, runtime eval/load, CLI spine, seed mint. | no (except via `reader.ss`) |
| Tests & gate | `test/chez/`, `test/conformance/`, `host/chez/run-*.ss`, `Makefile` | Corpus (JVM oracle), unit, per-feature tests. Every `make` target has a comment. | no |

**The reader is in `host/chez/reader.ss`** (Scheme, a seed source) — *not* in
`jolt-core/jolt/` with the rest of the compiler. Re-mint applies to it.

`rt.ss` is the runtime's load-order manifest: it `(load …)`s every shim in
dependency order with a per-file comment. Read it to see how the runtime is
composed and where a given `.ss` fits.

## `host/chez/*.ss` by family

- **Value model**: `values.ss` (nil/numbers/keywords/symbols), `collections.ss`
  (persistent vec + HAMT map/set), `seq.ss` + `lazy-bridge.ss` (seqs, lazy-seqs),
  `transients.ss`, `records.ss` + `records-interop.ss`.
- **Native `clojure.core` shims**: `natives-*.ss` (array/coll/format/meta/misc/num/
  queue/reader/seq/str/transduce), plus `predicates.ss`, `converters.ss`, `printing.ss`.
- **Vars / namespaces / dynamics**: `vars.ss`, `ns.ss`, `dyn-binding.ss` (the
  thread-local binding stack), `dynamic-var-defaults.ss` (a few `*…*` constant defaults),
  `atoms.ss`, `multimethods.ss`.
- **Host interop**: `host-class.ss` (class tokens + method dispatch),
  `host-static.ss` (interop registry core) + `host-static-methods.ss` (`Class/member`
  statics) + `host-static-classes.ss` (instantiable object classes), `host-table.ss`,
  `host-contract.ss` (the `jolt.host` seam the compiler resolves against),
  `dot-forms.ss`, `records-interop.ss`.
- **Scalars / misc**: `regex.ss` (vendored irregex), `math.ss`, `inst-time.ss`,
  `bigdec.ss`, `syntax-quote.ss`.
- **IO / system / concurrency / FFI**: `io.ss`, `png.ss`, `concurrency.ss`,
  `async.ss`, `ffi.ss`.
- **Compiler entry on Chez**: `reader.ss`, `compile-eval.ss`, `emit-image.ss`,
  `loader.ss`, `cli.ss`, `build.ss`, `bootstrap.ss`.

## Where is a `clojure.core` fn implemented?

Two homes, with a defined precedence:

1. **Native shim** — a `(def-var! "clojure.core" "name" …)` in a `host/chez/*.ss`
   (hot/representation-coupled fns: `first`, `get`, `=`, the predicates).
2. **Overlay** — a `defn` in a `jolt-core/clojure/core/NN-*.clj` tier (most of
   `clojure.core`, in portable Clojure).
3. **`post-prelude.ss`** re-asserts a handful of natives *after* the overlay loads,
   so the native version wins (the overlay's value-reading versions are wrong for
   Chez-native chars/atoms/etc.). Each entry there says why.

`grep 'def-var! "clojure.core" "frequencies"' host/chez` and
`grep -rn 'defn frequencies' jolt-core/clojure/core` to find a given fn. See
[seed-overlay-registry.md](seed-overlay-registry.md) for the shadowing mechanism.

## Cross-cutting features — touch points

A feature's *core* lives in one file; these are the other files you must keep in
sync when changing it.

- **Tree-shaking / DCE** (`--tree-shake`): `emit-image.ss` (the `dce-*` helpers +
  record producers) and `build.ss` (`bld-shake-all` reachability + the manifest
  splice in `bld-emit-runtime`); the flag in `main.clj`; validated by
  `host/chez/tree-shake-smoke.sh` (`make shakesmoke`) and `build-smoke.sh`. See
  [tools-deps.md](tools-deps.md#tree-shaking).
- **Direct-linking** (`--direct-link`): `backend_scheme.clj` (`direct-link?`,
  `emit-top-form`, the `jv$<fqn>` bindings); `build.ss` turns it on; `main.clj` the
  flag; `test/chez/directlink-test.ss`.
- **Numeric fl*/fx\*** (`^double`/`^long` hints): `jolt-core/jolt/passes/numeric.clj`
  (the hint-directed pass + loop-counter + `:coerce`); `backend_scheme.clj`
  (`dbl-ops`/`lng-ops` op strings, `emit-numeric`, entry/return coercion);
  `analyzer.clj` (`nhint-of`, `:nhints`, `with-ret-nhint`); `host-contract.ss`
  (`:num-ret` on resolve); `rt.ss` (`jolt->fx`); `test/chez/numeric-test.ss`.
- **IR inlining** (under `--opt`): `passes/inline.clj` (splice) + `passes.clj`
  (stash) + `host-contract.ss` (`inline-ir`/`stash-inline!`); `test/chez/inline-test.ss`.
- **Multimethods**: `host/chez/multimethods.ss` (dispatch) + the overlay
  `defmulti`/`defmethod` macros + `host-contract.ss` late-bind.
- **AOT namespace context** (`jolt build`): `build.ss` (`bld-ns-prelude`) emits
  `(set-chez-ns! ns)` + `chez-register-alias!` per app namespace (both the normal
  and tree-shake emit paths), matching the loader's per-file ns context;
  `test/chez/build-app` (`make buildsmoke`).
- **Deps resolution**: `jolt-core/jolt/deps.clj` (the only file) + `main.clj`
  (applies the roots) + `loader.ss` (the `require` path).

## Conventions you must preserve

See **CLAUDE.md → "Conventions & Patterns"** for the load-bearing rules: the
re-mint trigger, the tier macro-ordering rule, the `get`-on-your-own-wrapper trap,
`:jolt/type`-as-a-key parsing, the `var-deref` calling convention (the compiler is
reached from the `.ss` runtime by string lookup, so a public `defn` with no
in-Clojure callers can still be live), and the writing style.
