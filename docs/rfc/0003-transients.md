# RFC 0003: Transients — semantics and why they live in the Janet seed

Status: accepted (design note)

This note pins down what transients *are* in Jolt, where their behavior
deviates from JVM Clojure and why, and why the transient machinery is part of
the irreducible Janet seed rather than a candidate for the core-in-Clojure
migration (jolt-tzo). It exists so the kernel-shrink ladder doesn't revisit
transients every round.

## What a transient is in Jolt

A transient is a tagged Janet table wrapping a *native* mutable host value
(`core.janet`, "Transients" section):

- transient vector — `@{:jolt/type :jolt/transient :kind :vector :arr ARRAY}`,
  a Janet array.
- transient map — `:kind :map :tbl TABLE`, a Janet table mapping
  `canon-key(k)` → `@[k v]`. Keying by canonical key keeps collection keys
  comparing by value across representations (`[1 2]` the pvec and `[1 2]` the
  tuple are one key), and storing the `@[k v]` pair preserves the *original*
  key for the rebuilt persistent map.
- transient set — `:kind :set :tbl TABLE` mapping `canon-key(x)` → `x`.

`transient` accepts pvecs, mutable-build arrays, tuples (reader vectors and
map entries — added in the seed-shrink rounds so `(into [] (first {:a 1}))`
works through the vector fast path), sets, phms, and untagged struct maps.
Sorted collections are rejected, as on the JVM (not editable).

The bang ops (`conj!`, `assoc!`, `dissoc!`, `disj!`, `pop!`) mutate that host
value in place and return the transient — O(1) per op (amortized for array
push). `persistent!` rebuilds a persistent value from the host value and
invalidates the transient (`:jolt/persistent` flag; any further bang op or a
second `persistent!` throws "Transient used after persistent! call", matching
Clojure's invalidation contract).

Read ops work on an active transient where Clojure supports them: `get`,
`contains?`, `count`, and `nth` (vector kind) branch on the transient tag.
`seq` on a transient is not supported, as in Clojure.

## Deviations from JVM Clojure (deliberate)

**O(n) edges, O(1) middle.** Clojure's `(transient v)` is O(1) — the transient
*shares* the persistent trie and marks nodes editable; `persistent!` is O(1)
too. Jolt's `transient` copies the source into a native array/table (O(n)) and
`persistent!` rebuilds (O(n)). The bang ops in between are native-host O(1),
which is *faster* per-op than trie editing. So the asymptotics of the usual
pattern

    (persistent! (reduce conj! (transient []) coll))

are identical (O(n) total either way) with a better constant in the loop and a
worse constant at the two edges. The pattern transients exist for — batch
construction — is fully served. What is NOT served is transient-editing a
*large* collection to change a few keys: that's O(n) in Jolt vs O(log n) in
Clojure, because `transient` flattens the pvec trie / phm buckets into a
native array/table and `persistent!` rebuilds them.

**No thread-ownership check.** JVM Clojure ≥1.7 also dropped the owner-thread
assertion (for fork/join), keeping only "don't use after persistent!", which
Jolt enforces. Jolt code is fiber-concurrent; when real OS-thread futures land
(jolt-ejx), a transient handed across threads is a data race exactly as in
Clojure — documented, not checked, same as the JVM.

**`(conj!)` / `(conj! t)` arities** follow Clojure's transducer-era contract:
zero args makes a fresh `(transient [])`, one arg returns it untouched.
`assoc!` tolerates a dangling final key (treated as `k nil`), matching the
lenient kvs walk of Jolt's `assoc`.

**No transient sorted variants** — same as Clojure. One leniency: Clojure
throws on `(transient '(1))`, but Jolt's lists are Janet arrays underneath and
fall into the mutable-build branch, yielding a transient *vector*. Harmless
(the result of `persistent!` is a vector, never silently a list) but
non-Clojure; tighten if it ever bites.

## Why transients stay in the Janet seed

The migration ladder (jolt-tzo) moves anything expressible as *pure Clojure
over existing primitives* out of the seed. Transients fail that test on three
grounds:

1. **They are the mutation kernel.** A transient's entire value is direct
   mutation of a host array/table. The overlay's only mutation seam is
   `jolt.host/ref-put!` (a single table-put). Re-expressing `tr-conj!` etc. in
   Clojure would mean either growing the host surface one-for-one
   (`host-array-push!`, `host-table-put!`, …, i.e. moving the same code behind
   more indirection) or simulating mutation over persistent values (defeating
   the point of transients). Either way the Janet line count moves, it doesn't
   shrink.

2. **They sit under the seed's own dispatch.** `conj`/`assoc`/`get`/`count`/
   `contains?` in the seed branch on the transient tag. Hoisting the transient
   ops above that dispatch (the hierarchy-port pattern of lazily-resolved
   overlay vars) would put an interpreted/compiled-Clojure call inside the
   hottest native paths for no semantic gain — transients have no semantics to
   *fix* (unlike hierarchy, which had real correctness gaps).

3. **The value layer is declared irreducible.** The self-hosting design doc
   (docs/self-hosting-compiler.md, "The kernel") keeps the value/representation
   layer — persistent collections and, with them, their mutable scratch
   counterparts — in the host. Transients are representation, not library.

What CAN move (and mostly has): anything *derived* — e.g. `into`'s
transient-using fast path, or future `update!`-style conveniences — is plain
Clojure over `transient`/bang-ops/`persistent!` and belongs in the overlay
tiers as ordinary migration batches.

## Future work

- pvec is already a 32-way trie with structural sharing (pv.janet), so
  Clojure-style O(1) `transient`/`persistent!` via editable nodes is a real
  option for vectors — an internal change behind the same surface, not a
  semantics change. phm is bucket-based copy-on-write; the same trick applies
  if it ever becomes a HAMT.
- `transient?` (Jolt extension, useful in tests) stays; Clojure has no public
  predicate, so it must not leak into portability-sensitive code.
