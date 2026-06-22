# RFC 0003: Transients — semantics and the Chez mutable backing

Status: accepted (design note)

This note pins down what transients *are* in Jolt, where their behavior
deviates from JVM Clojure and why, and how the transient machinery is
represented in the Chez runtime. It exists so the design doesn't revisit
transients every round.

## What a transient is in Jolt

A transient is a Chez record (`jolt-transient`, `host/chez/transients.ss`)
wrapping *true mutable* host backing, snapshotted to the immutable collection on
`persistent!`. The backing is per kind:

- transient vector — a growable Scheme vector (a capacity buffer plus a fill
  count `n`). `conj!`/`pop!` are in-place, amortized O(1); the buffer doubles on
  growth.
- transient map — a Chez hashtable keyed by `key-hash` / `jolt=`
  (value-equality, nil-safe). Hashing by value keeps collection keys comparing
  across representations.
- transient set — a Chez hashtable of elements.
- `cow` — a copy-on-write fallback for anything else (e.g. a sorted coll).

`transient` accepts pvecs, pmaps, psets, and the exotic colls handled by the
`cow` path. Each kind copies its source into the matching mutable backing once.

The bang ops (`conj!`, `assoc!`, `dissoc!`, `disj!`, `pop!`) mutate that backing
in place and return the transient — O(1) per op (amortized for the vector push).
`persistent!` snapshots a persistent value from the backing (folding the
hashtable into a pmap/pset, handing off the buffer as a pvec) and invalidates the
transient (the record's active flag clears; any further bang op or a second
`persistent!` throws "transient used after persistent!", matching Clojure's
invalidation contract).

Read ops work on an active transient where Clojure supports them: `get`,
`contains?`, `count`, and `nth` (vector kind) see through the transient.
`seq` on a transient is not supported, as in Clojure.

## Deviations from JVM Clojure (deliberate)

**O(n) edges, O(1) middle.** Clojure's `(transient v)` is O(1) — the transient
*shares* the persistent trie and marks nodes editable; `persistent!` is O(1)
too. Jolt's `transient` copies the source into a mutable buffer/hashtable (O(n))
and `persistent!` snapshots back (O(n)). The bang ops in between are host-mutable
O(1), which is *faster* per-op than trie editing. So the asymptotics of the usual
pattern

    (persistent! (reduce conj! (transient []) coll))

are identical (O(n) total either way) with a better constant in the loop and a
worse constant at the two edges. The pattern transients exist for — batch
construction — is fully served. What is NOT served is transient-editing a
*large* collection to change a few keys: that's O(n) in Jolt vs O(log n) in
Clojure, because `transient` copies the source into a growable Scheme vector /
Chez hashtable and `persistent!` snapshots it back.

**No thread-ownership check.** JVM Clojure ≥1.7 also dropped the owner-thread
assertion (for fork/join), keeping only "don't use after persistent!", which
Jolt enforces. A transient handed across threads is a data race exactly as in
Clojure — documented, not checked, same as the JVM.

**`(conj!)` / `(conj! t)` arities** follow Clojure's transducer-era contract:
zero args makes a fresh `(transient [])`, one arg returns it untouched.
`assoc!` tolerates a dangling final key (treated as `k nil`), matching the
lenient kvs walk of Jolt's `assoc`.

**No transient sorted variants** — same as Clojure. One leniency: Clojure
throws on `(transient '(1))`, but Jolt routes a list through the `cow` fallback
path, yielding a transient. Harmless but non-Clojure; tighten if it ever
bites.

## Why transients live in the host

Transients are part of the value/representation layer in the Chez runtime
(`host/chez/transients.ss`), not the portable `clojure.core` overlay, on three
grounds:

1. **They are the mutation kernel.** A transient's entire value is direct
   mutation of a host buffer/hashtable. The overlay has no mutation seam of its
   own. Re-expressing the bang ops in Clojure would mean either growing the host
   surface one-for-one (a host-vector-push, a host-hashtable-put, …, i.e. moving
   the same code behind more indirection) or simulating mutation over persistent
   values (defeating the point of transients).

2. **They sit under the collection dispatch.** `conj`/`assoc`/`get`/`count`/
   `contains?` see through a transient. Hoisting the transient ops above that
   dispatch would put a compiled-Clojure call inside the hottest paths for no
   semantic gain — transients have no semantics to *fix*.

3. **The value layer is the host's job.** The persistent collections and, with
   them, their mutable scratch counterparts, live in the Chez runtime alongside
   the value model. Transients are representation, not library.

What lives in the overlay: anything *derived* — e.g. `into`'s transient-using
fast path, or `update!`-style conveniences — is plain Clojure over
`transient`/bang-ops/`persistent!`.

## Future work

- The persistent map/set are a bitmap HAMT with structural sharing
  (`host/chez/collections.ss`), so Clojure-style O(1) `transient`/`persistent!`
  via editable nodes is a real option there — an internal change behind the same
  surface, not a semantics change. The persistent vector is a flat
  copy-on-write Scheme vector rather than a trie, so the transient surface for
  it stays the copy-to-growable-vector path.
- `transient?` (Jolt extension, useful in tests) stays; Clojure has no public
  predicate, so it must not leak into portability-sensitive code.
