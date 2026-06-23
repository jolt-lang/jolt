# Seed ↔ Overlay Registry

Jolt is Clojure on Chez Scheme. `clojure.core` is built from two tiers that both
define `clojure.core`-facing vars, and for a handful of names *both* tiers carry
a definition. This document records how the two tiers relate and which copy is
authoritative.

## The two tiers

- **Native shims** (`host/chez/natives-*.ss`) bind a set of `clojure.core` vars
  directly to Scheme runtime values via `def-var!` — collection constructors,
  seq fns, numeric/string ops, and so on. These cover names the overlay assumes
  exist as bare `clojure.core` vars but does not define itself.
- **The Clojure overlay** (`jolt-core/clojure/core/NN-*.clj`) defines the rest of
  `clojure.core` in dependency-ordered tiers, loaded in order: `00-syntax`,
  `00-kernel`, `10-seq`, `20-coll`, `25-sorted`, `30-macros`, `40-lazy`, `50-io`.

The overlay loads after the native shims. When an overlay tier `(defn X …)` for a
name a native shim already bound, the **overlay def shadows the native binding** —
user code sees the overlay copy. The native binding then survives only if some
other native/runtime code still calls the Scheme value directly.

So a name's *home* is determined by two facts:

1. is it bound by a native shim? (the Scheme value is reachable from the runtime)
2. does an overlay tier `(defn X …)`? (the overlay copy is what user code sees)

## The compiled seed

`clojure.core` is compiled ahead of time into the checked-in seed
(`host/chez/seed/{prelude,image}.ss`) as Scheme `def-var!` forms. The seed's
source twin is the overlay (`jolt-core/clojure/core/*.clj` plus the stdlib
namespaces under `stdlib/clojure/`); `host/chez/emit-image.ss` re-emits the
prelude from those sources on Chez. The build is a byte-fixpoint: rebuilding from
an up-to-date seed reproduces it exactly.

## Consistency guard

There is no separate drift-check test for the registry. The self-hosting
fixpoint is the guard: after changing a seed source (a core tier, the compiler
namespaces, the host contract, the reader, or `emit-image.ss`) you must re-mint
the seed (`make remint`), and `make selfhost` fails if the checked-in seed and
its sources have drifted. So if the overlay's shadowing relationship changes, the
re-minted prelude changes with it, and the fixpoint check keeps source and seed
in agreement.
