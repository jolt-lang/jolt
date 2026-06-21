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
byte-fixpoint**: rebuilding from an up-to-date seed reproduces it exactly.
`make selfhost` (`host/chez/selfcheck.sh`) runs `host/chez/bootstrap.ss` and diffs
the rebuilt artifacts against the checked-in seed.

## Re-minting

When the seed sources change (the core tiers, the compiler namespaces, the host
contract, the reader, `emit-image.ss`), the seed drifts and `make selfhost`
fails. Re-mint it by running `host/chez/bootstrap.ss` and writing the freshly
rebuilt prelude/image back to `host/chez/seed/prelude.ss` /
`host/chez/seed/image.ss`, then commit the refreshed files.
