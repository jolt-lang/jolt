# Chez bootstrap seed

These two files are the **bootstrap compiler** for jolt-on-Chez — the seed that
makes the build self-hosting with no Janet in the loop:

- `prelude.ss` — the `clojure.core` prelude (all tiers + clojure.string/walk/
  template/edn/set/pprint) as Scheme `def-var!` forms.
- `image.ss` — the compiler image (`jolt.ir` + `jolt.analyzer` +
  `jolt.backend-scheme`) as Scheme `def-var!` forms.

Both are **generated**, not hand-written. They are checked in because a fresh
checkout must be able to build jolt-on-Chez using only Chez: `host/chez/bootstrap.ss`
loads this seed, then rebuilds the prelude + image from the `.clj`/`.ss` sources via
the on-Chez compiler (read → analyze → emit, all on Chez). The seed is a **joint
byte-fixpoint**: rebuilding from an up-to-date seed reproduces it exactly
(`test/chez/bootstrap-test.janet` verifies this).

Janet was used once, historically, to mint the very first seed (the Janet analyzer/
emitter cross-compiled the sources, then the on-Chez compiler iterated to the
fixpoint — see `test/chez/fixpoint-test.janet`). After that, Janet is never needed
to build or run jolt-on-Chez.

## Re-minting

When the seed sources change (the core tiers, the compiler namespaces, the host
contract, the reader, `emit-image.ss`), the seed drifts and `bootstrap-test`
fails. Re-mint it:

```janet
(import host/chez/driver :as d)
(import host/chez/jolt-chez :as jc)
(def ctx (d/make-ctx))
(d/mint-chez-seed* (jc/ensure-prelude ctx)
                   (d/ensure-compiler-image ctx "/tmp/stage1.ss")
                   "host/chez/seed/prelude.ss"
                   "host/chez/seed/image.ss")
```

Then commit the refreshed `prelude.ss` / `image.ss`.
