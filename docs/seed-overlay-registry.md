# Seed ↔ Overlay Registry

Jolt is "Clojure on Janet": a shrinking **Janet seed** (`src/jolt/*.janet`)
hosts a **Clojure overlay** (`jolt-core/clojure/core/NN-*.clj`). Both define
`clojure.core`-facing functions, and for a handful of names *both* tiers carry a
definition. Which copy is authoritative has been tribal knowledge. This document
is the single source of truth; `test/unit/seed-overlay-registry-test.janet` is a
build-time drift check that fails if reality diverges from what is written here.

## The registration mechanism

Seed core functions are named with a `core-` prefix (`core-into`, `core-conj`,
`core-transduce`) and registered into the `clojure.core` namespace by the
`core-bindings` table in `src/jolt/core.janet`. Each entry maps a **public
Clojure name** (the string key) to a seed function value:

```janet
(def- core-bindings
  @{"into"   core-into
    "reduce" core-reduce
    ...})
```

`init-core!` (`src/jolt/core.janet`) interns every pair into `clojure.core`.
The overlay tiers load afterwards (`api.janet`: 00-syntax, 00-kernel, 10-seq,
20-coll, 25-sorted, 30-macros, 40-lazy, 50-io). When an overlay tier `(defn X …)`
for a name that `core-bindings` already registered, the **overlay def shadows the
seed binding** — the seed `core-X` then survives only if some other seed code
still calls it directly.

So a name's *home* is determined by two facts:

1. is it a key in `core-bindings`? (registered ⇒ the seed `core-X` is reachable)
2. does an overlay tier `(defn X …)`? (defined ⇒ the overlay copy shadows)

## Dispatch-only seed helpers: the `__` prefix

Seed functions that are **not** public Clojure vars but must be reachable by
name from compiled/overlay code (compiler hooks, macro-expansion targets) are
registered under a `__`-prefixed key — e.g. `"__sq1"`, `"__write"`,
`"__bit-and"`, `"__jdbc-conn-raw"`. The `__` prefix is unreadable as a
user-level symbol, so these never collide with or masquerade as public API. When
you add a dispatch-only hook, give it a `__` key; do not register it under a bare
name.

## Dispatch twins

A **twin** is a name with *both* a seed `core-X` defn and an overlay `(defn X …)`.
There are exactly five. Each seed site carries a greppable `SEED-TWIN:` comment.

| name          | overlay (authoritative public)        | seed copy (`core-X`)                | registered? | role of the seed copy |
|---------------|----------------------------------------|--------------------------------------|-------------|------------------------|
| `char?`       | `20-coll.clj` `char?`                  | `core_types.janet` `core-char?`      | no          | internal type dispatch |
| `sorted-map?` | `25-sorted.clj` `sorted-map?`          | `core_types.janet` `core-sorted-map?`| no          | internal dispatch (sorted-op) |
| `sorted-set?` | `25-sorted.clj` `sorted-set?`          | `core_types.janet` `core-sorted-set?`| no          | internal dispatch |
| `sorted?`     | `25-sorted.clj` `sorted?`              | `core_types.janet` `core-sorted?`    | no          | internal dispatch |
| `transduce`   | `20-coll.clj` `transduce`              | `core_coll.janet` `core-transduce`   | no          | internal helper for `core-into` only |

None of the five is registered in `core-bindings`: the overlay copy is the public
one, and the seed copy is reached only by other *seed* code (so editing the seed
copy alone will not change what user code sees — change both, or move the logic).

## The surprising asymmetry: `into` vs `transduce`

`into` and `reduce` are **seed-public**: registered in `core-bindings`, and the
overlay deliberately does *not* redefine them (they sit on the perf wall — see
the "into stays in the seed" note in `20-coll.clj`). `transduce`, by contrast, is
**overlay-public**: the overlay `transduce` is the real one, and `core-transduce`
remains only because `core-into` calls it directly. So two functions that read as
a matched pair have opposite homes. That asymmetry is intentional and is the
reason this registry exists.

## Drift check

`test/unit/seed-overlay-registry-test.janet` recomputes the twin set from source
(names with both a seed `core-X` defn and an overlay `defn X`) and asserts it
equals the five above. It also asserts none of the twins is registered in
`core-bindings`, and that every non-`__` `core-bindings` key is a plausible
public name (no accidental `__`-less dispatch helper). If you add, remove, or
re-home a twin, update this table and that test together.
