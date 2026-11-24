# Collection Edit Fusion Benchmarks

## jolt-44

Block-copy `vec-append2` (`sa-vector-append`, with a direct `(vector k x)` fast path for empty slot vectors) and half-shift block-copy `vec-remove2` (`sa-subvector` of the longer surviving side + fill of the shorter side, zero extra allocation) in `host/chez/collections.ss`.

- **Parent commit measured against:** `eb53cc139` (`perf/coll-edit-fusion`)
- **Date:** 2026-11-23
- **Machine:** Apple M3 Ultra (Virtual), macOS 15.7.3
- **Chez:** 10.4.1 (`JOLT_CHEZ=/Users/admin/src/jolt/.cache/local/chezscheme-10.4.1/bin/scheme`)
- **Build flags:** `bin/jolt build -m <ns> -o <out> --direct-link --opt`
- **Gambit:** not installed on this machine; `portcheck`, `gambitseedcheck`, `gambitgencheck`, and `gambittwins` passed; Gambit-runtime gates (`gambitcheck`, `gambitkernel`, `gambiteval`, `gambitboot`, `gambitunbound`, `gambitvars`, `gambitstatics`, `gambitprofile`) did not run.

### Commands

Ad hoc AOT microbenchmark (built out-of-tree in a temporary directory with `{:paths ["."]}` and removed before commit):

```sh
# Built before and after host/chez/collections.ss edits:
bin/jolt build -m bench44 -o /tmp/jolt-44-bench/bench44-{base,cand} --direct-link --opt
# Timed interleaved A1 -> B -> A2 (7 runs of 2,000,000 ops each after 3 warmup runs, System/gc before each run):
/tmp/jolt-44-bench/bench44-base
/tmp/jolt-44-bench/bench44-cand
/tmp/jolt-44-bench/bench44-base
```

Existing `bench/run.sh` rows (built with the exact same `jolt build -m <ns> --direct-link --opt` flags as `bench/run.sh`, timed interleaved A1 -> B -> A2):

```sh
(cd bench && JOLT_PWD="$PWD" ../bin/jolt build -m <ns> -o /tmp/jolt-44-bench/<ns>-{base,cand} --direct-link --opt)
# Default args from bench/run.sh: coll-ops 100000, collections 30000, coll-dispatch 200000, metadata 100000
# Plus coll-ops with pinned nursery (JOLT_GC_TRIP_BYTES=16m and 64m) to isolate adaptive-GC variance.
```

### Benchmark map construction and asserted representations

Every receiver in `bench44` asserts its runtime class at startup via `(.getName ^Class (class obj))`:

- `m0`: `{}` — `clojure.lang.PersistentArrayMap` (0 keys)
- `m3`: `{:k0 0 :k1 1 :k2 2}` — `clojure.lang.PersistentArrayMap` (3 keys, literal)
- `m8`: `(reduce (fn [m i] (assoc m (keyword (str "k" i)) i)) {} (range 8))` — `clojure.lang.PersistentArrayMap` (8 keyword keys, array-mode)
- `m24`: `(reduce (fn [m i] (assoc m (keyword (str "k" i)) i)) {} (range 24))` — `clojure.lang.PersistentArrayMap` (24 keyword keys, array-mode under the 64-keyword limit)
- `hm24`: `(apply hash-map (mapcat (fn [i] [(keyword (str "k" i)) i]) (range 24)))` — `clojure.lang.PersistentHashMap` (24 keyword keys, hash-mode control)
- `v32`: `(vec (range 32))` — `clojure.lang.PersistentVector` (32 elements, tail-full control)
- `rec`: `(->Rec 1 2 3)` from `(defrecord Rec [a b c])` — record control (in-field `assoc :b` and ext-map `assoc :d`)

### Ad hoc AOT results (median ns/op of 7 runs, A1 -> B -> A2)

| Row | Parent A1 (ns/op) | Candidate B (ns/op) | Parent A2 (ns/op) | Speedup (avg A / B) | Notes |
|---|---:|---:|---:|---:|---|
| `assoc-new-0` | 21.41 | 15.84 | 21.37 | **1.35x** | `vec-append2` empty fast path `(vector k x)` |
| `assoc-new-3` | 31.31 | 21.74 | 31.45 | **1.44x** | `vec-append2` via `sa-vector-append` |
| `assoc-new-8` | 48.47 | 27.67 | 49.03 | **1.76x** | `vec-append2` via `sa-vector-append` |
| `assoc-new-24` | 104.33 | 44.72 | 102.79 | **2.32x** | `vec-append2` via `sa-vector-append` |
| `assoc-replace-8` | 23.85 | 23.74 | 23.61 | 1.00x | control (`vec-set`, unchanged) |
| `dissoc-3-first` | 24.97 | 20.92 | 25.08 | **1.20x** | `vec-remove2` end fast path (`sa-subvector` only) |
| `dissoc-3-mid` | 25.86 | 24.72 | 25.64 | **1.04x** | `vec-remove2` half-shift |
| `dissoc-3-last` | 26.01 | 22.35 | 25.54 | **1.15x** | `vec-remove2` end fast path (`sa-subvector` only) |
| `dissoc-3-all` | 29.83 | 26.56 | 29.02 | **1.11x** | uniform sweep across all 3 keys |
| `dissoc-8-first` | 39.98 | 24.10 | 39.36 | **1.65x** | `vec-remove2` end fast path |
| `dissoc-8-mid` | 42.28 | 34.18 | 43.82 | **1.26x** | `vec-remove2` half-shift |
| `dissoc-8-last` | 42.01 | 26.97 | 43.40 | **1.58x** | `vec-remove2` end fast path |
| `dissoc-8-all` | 44.64 | 34.00 | 44.42 | **1.31x** | uniform sweep across all 8 keys |
| `dissoc-24-first` | 88.17 | 35.46 | 85.30 | **2.45x** | `vec-remove2` end fast path |
| `dissoc-24-mid` | 93.94 | 68.05 | 92.05 | **1.37x** | `vec-remove2` half-shift |
| `dissoc-24-last` | 96.80 | 44.86 | 95.16 | **2.14x** | `vec-remove2` end fast path |
| `dissoc-24-all` | 96.40 | 59.66 | 94.97 | **1.60x** | uniform sweep across all 24 keys |
| `ctrl-hash-assoc-24` | 84.12 | 81.05 | 81.52 | 1.02x | miss-path control (hash-mode map `assoc`) |
| `ctrl-hash-dissoc-24` | 59.02 | 53.27 | 58.08 | **1.10x** | hash-mode `dissoc` (HAMT `node-dissoc` calls `vec-remove2`) |
| `ctrl-vec-conj-32` | 23.38 | 23.39 | 23.60 | 1.00x | miss-path control (tail-full vector `conj`) |
| `ctrl-rec-assoc-field` | 25.89 | 25.83 | 25.94 | 1.00x | miss-path control (record declared-field `assoc`) |
| `ctrl-rec-assoc-ext` | 43.78 | 37.80 | 43.68 | **1.16x** | record ext-map `assoc` (appends into array-mode ext-map) |

### Design note on `vec-remove2`

A two-piece `(sa-vector-append (sa-subvector v 0 i) (sa-subvector v (fx+ i 2) n))` rewrite was tested and rejected: it allocates two temporary subvectors plus the final vector (roughly 2x bytes/op) and measured slower than the original loop on interior removals at 3 keys (15.1 vs 13.0 ns in standalone Chez) and slower than half-shift across uniform sweeps at 3, 4, 8, 16, 24, and 48 keys (e.g. 41.6 vs 38.2 ns/op and 751 vs 384 B/op at 24 keys; 100.9 vs 74.4 ns/op at 48 keys). Half-shift (`sa-subvector` of the larger surviving half + unchecked fill of the shorter half) matches the original allocation byte-for-byte while speeding up every size and position. `vec-fill-prefix!` remains live because `vec-remove2` calls it for the prefix half (`2*i <= m` and `i > 0`). No converted primitive was slower.

### Existing `bench/run.sh` rows (A1 -> B -> A2)

| Benchmark (arg) | Parent A1 (ms) | Candidate B (ms) | Parent A2 (ms) | Ratio (avg A / B) |
|---|---:|---:|---:|---:|
| `coll-ops` (`small-assoc`, n=100000) | 1.60 | 1.24 | 1.58 | **1.28x** |
| `coll-ops` (`small-merge`, n=100000) | 1.18 | 0.87 | 1.15 | **1.34x** |
| `collections` (n=30000) | 5.9 | 5.8 | 5.6 | 0.99x |
| `coll-dispatch` (n=200000) | 87 | 87 | 90 | 1.02x |
| `metadata` (n=100000) | 69 | 65 | 69 | **1.06x** |

#### Investigation of `coll-ops` `v-assoc` / `hm-hash` / `hm-replace` / `hs-build` / `hm-dissoc` variance

In the initial `coll-ops 100000` A1/B/A2 run, unrelated heavy-allocating rows moved together (`v-assoc` 14.66 / 9.97 / 15.53 ms, `hm-hash` 38.48 / 30.56 / 36.32 ms, `hm-replace` 34.01 / 29.47 / 36.74 ms, `hs-build` 33.50 / 28.30 / 33.56 ms, total mean 297.6 / 266.6 / 302.2 ms). Re-running 5 additional interleaved pairs of the exact same binaries showed both base and candidate flipping bimodally between the same two regimes (`v-assoc` 9.5–10.5 ms vs 14.3–16.3 ms on both binaries).

Inspecting `JOLT_GC_LOG=1` and `host/chez/rt.ss:977-1050` identified the exact cause: the adaptive nursery controller doubles `collect-trip-bytes` from 16 MB to 32 MB / 64 MB when collection share crosses 10% (`gc-target-share`), which spills the L2 cache on heavy-allocating ops (the same effect noted in `rt.ss:1022`). Pinning the nursery removes the bimodal flip on both binaries:

| `coll-ops 100000` row | Base (`16m`, 3-run med) | Cand (`16m`, 3-run med) | Base (`64m`) | Cand (`64m`) |
|---|---:|---:|---:|---:|
| `small-assoc` | 1.67 ms | **1.29 ms** (1.29x) | 1.60 ms | **1.25 ms** (1.28x) |
| `small-merge` | 1.20 ms | **0.91 ms** (1.32x) | 1.19 ms | **0.89 ms** (1.34x) |
| `hm-dissoc` | 29.67 ms | 32.01 ms (range 27.4–32.2 vs 28.4–33.9) | 37.59 ms | 36.67 ms |
| `v-assoc` | 11.99 ms | 11.80 ms | 15.43 ms | 16.58 ms |
| `hm-hash` | 35.04 ms | 34.69 ms | 40.19 ms | 38.39 ms |
| `hm-replace` | 31.90 ms | 35.32 ms | 40.58 ms | 38.25 ms |
| `hs-build` | 31.60 ms | 31.91 ms | 35.20 ms | 33.95 ms |

## jolt-39

Single-allocation multi-pair `assoc` for 2..4 pairs on non-promoting array-mode maps with callback-free keys (`keyword-t?`, `fixnum?`, `char?`), plus direct `nil` construction, implemented as `case-lambda` fixed clauses (5, 7, 9, and 3 args, ahead of variadic fallback) on `jolt-assoc` in `host/chez/collections.ss`.

- **Parent commit measured against:** `c450a26f9` (`perf/coll-edit-fusion`)
- **Date:** 2026-11-23
- **Machine:** Apple M3 Ultra (Virtual), macOS 15.7.3
- **Chez:** 10.4.1 (`JOLT_CHEZ=/Users/admin/src/jolt/.cache/local/chezscheme-10.4.1/bin/scheme`)
- **Build flags:** `bin/jolt build -m <ns> -o <out> --direct-link --opt`
- **Nursery pin:** `JOLT_GC_TRIP_BYTES=16m` (isolates adaptive nursery doubling investigated in `jolt-44`)
- **Gambit:** not installed on this machine; `portcheck`, `gambitseedcheck`, `gambitgencheck`, and `gambittwins` passed; Gambit-runtime gates (`gambitcheck`, `gambitkernel`, `gambiteval`, `gambitboot`, `gambitunbound`, `gambitvars`, `gambitstatics`, `gambitprofile`) did not run.

### Corpus arity split (50 repos, `/tmp/jolt-opt-corpus-20261123`)

Scanned 15,313 `.clj`/`.cljc`/`.cljs` files (0 parse errors) with a standalone out-of-tree Clojure tokenizer/parser separating direct `(assoc m k1 v1 ...)`, thread-first `(-> / some-> / cond-> ... (assoc k1 v1 ...))`, and value-position `(swap! / update / update-in ... assoc k1 v1 ...)` sites:

| Pairs | Direct `(assoc m ...)` | Threaded `(-> ... (assoc ...))` | Combined direct + threaded | Multi-pair share | Value-pos (`swap!`/`update`/`update-in`) |
|---:|---:|---:|---:|---:|---:|
| 1 | 10,231 | 7,001 | 17,232 | — | 1,259 |
| **2** | **1,890** (67.9%) | **494** (62.7%) | **2,384** | **66.7%** | **142** |
| **3** | **487** (17.5%) | **165** (20.9%) | **652** | **18.3%** | **38** |
| **4** | **207** (7.4%) | **53** (6.7%) | **260** | **7.3%** | **22** |
| 5 | 86 | 33 | 119 | 3.3% | 9 |
| 6+ | 114 | 43 | 157 | 4.4% | 4 |
| **Total ≥2** | **2,784** | **788** | **3,572** | **100.0%** | **215** |

Cumulative 2..4 pairs covers **92.3%** (3,296 / 3,572) of multi-pair direct+threaded sites (92.8% of direct-only sites; >97% all-keyword-literal), plus 202 multi-pair and 1,259 1-pair value-position sites and downstream emission demand from `jolt-41` (`assoc`-chain fusion) and `jolt-43` (mixed `assoc`/`update` fusion). All three arities (2, 3, and 4 pairs) are shipped, plus the 1-pair clause on `jolt-assoc` for value-position callers.

### Mechanism choice (plan section 8): `case-lambda` on `jolt-assoc` vs `:fixed` registry helpers

Measured at 2 pairs with identical fast-path body under `JOLT_GC_TRIP_BYTES=16m` (1,000,000 iters, 7 runs, median ns/op). Note: the `:fixed` `jolt-assoc5` only and hybrid columns came from a prototype that is no longer on disk and cannot be re-measured.

| Row | `case-lambda` on `jolt-assoc` (run 1 / run 2) | `:fixed` `jolt-assoc5` only | Hybrid (`:fixed` + `case-lambda`) |
|---|---:|---:|---:|
| `assoc2-3-new` | 18.98 / 19.55 | 18.86 | 18.74 |
| `assoc2-3-exist` | 15.54 / 16.15 | 15.51 | 16.82 |
| `assoc2-12-new` | 34.23 / 33.21 | 32.82 | 34.42 |
| `assoc2-24-array-new` | 49.04 / 51.57 | 50.78 | 49.54 |
| `assoc2-24-array-exist` | 45.07 / 45.35 | 47.78 | 45.94 |
| `assoc2-nil` | 8.61 / 8.57 | 8.67 | 8.46 |
| `swap2-3-exist` | **49.51 / 50.20** | 76.56 | 49.80 |
| `swap1-3-exist` | **31.54 / 31.63** | 33.91 | 32.18 |
| `update2-3-exist` | **140.79 / 141.57** | 170.97 | 144.67 |
| `valpos2-3-exist` | **29.63 / 29.36** | 56.29 | 30.17 |

**Decision:** `case-lambda` on `jolt-assoc` (clauses ordered by frequency: 5, 7, 9, 3 args, variadic). With the 5-arg clause first, direct-call throughput matches `:fixed` `jolt-assoc5` (18.24 / 14.81 ns/op at 2M iters below), value-position calls (`swap!`, `update`, indirect `assoc`) gain 1.21x–1.91x where `:fixed` gains nothing, and neither `jolt-core/jolt/op_registry.clj` nor the compiler seeds need changes (`selfhost` and `gambitseedcheck` stay green on the committed tree). `jolt-38` should reuse `case-lambda` on `jolt-conj` and `jolt-dissoc`; `jolt-41` and `jolt-43` emit `(jolt-assoc m k1 v1 ...)` directly.

### Emitted Scheme (before and after)

Real call sites from `clj-kondo/impl/analyzer.clj` (`:4467`, `:4839`, `:4843`):

```clojure
(fn [ctx] (assoc ctx :arg-types nil :in-fn-literal true))
(fn [ctx lang filename uri] (assoc ctx :base-lang lang :lang lang :filename filename :uri uri))
```

Because `jolt-assoc` carries the fixed `case-lambda` clauses directly (no `:fixed` registry rewrite), the emitted call head is unchanged before and after and dispatches directly into the fixed 5/7/9-arg clause without allocating a rest-arg list:

```scheme
;; 2-pair (before and after):
(jolt-assoc ctx _kc$3 jolt-nil _kc$4 #t)
;; 4-pair (before and after):
(jolt-assoc ctx _kc$13 lang _kc$14 lang _kc$15 filename _kc$16 uri)
```

### Commands

```sh
# Built before and after host/chez/collections.ss edits:
(cd /tmp/jolt-39-bench && "$REPO/bin/jolt" build -m bench39 -o /tmp/jolt-39-bench/bench39-{base,cand} --direct-link --opt)
(cd bench && JOLT_PWD="$PWD" "$REPO/bin/jolt" build -m coll-ops -o /tmp/jolt-39-bench/coll-ops-{base,cand} --direct-link --opt)
# Timed interleaved A1 -> B -> A2 (7 runs of 2,000,000 ops after 3 warmup runs, System/gc before each run):
JOLT_GC_TRIP_BYTES=16m /tmp/jolt-39-bench/bench39-base 2000000
JOLT_GC_TRIP_BYTES=16m /tmp/jolt-39-bench/bench39-cand 2000000
JOLT_GC_TRIP_BYTES=16m /tmp/jolt-39-bench/bench39-base 2000000
# coll-ops (n=100000, 5 runs, min ms per op):
JOLT_GC_TRIP_BYTES=16m /tmp/jolt-39-bench/coll-ops-{base,cand,base} 100000
```

### Benchmark map construction and asserted representations

Every receiver in `bench39` asserts its runtime class at startup via `(.getName ^Class (class obj))`:

- `m3`: `{:k0 0 :k1 1 :k2 2}` — `clojure.lang.PersistentArrayMap` (3 keys, literal)
- `m12`: `(reduce (fn [m i] (assoc m (keyword (str "k" i)) i)) {} (range 12))` — `clojure.lang.PersistentArrayMap` (12 keyword keys, array-mode)
- `m24`: `(reduce (fn [m i] (assoc m (keyword (str "k" i)) i)) {} (range 24))` — `clojure.lang.PersistentArrayMap` (24 keyword keys, array-mode under the 64-keyword limit)
- `hm24`: `(apply hash-map (mapcat (fn [i] [(keyword (str "k" i)) i]) (range 24)))` — `clojure.lang.PersistentHashMap` (24 keyword keys, hash-mode control)
- `v32`: `(vec (range 32))` — `clojure.lang.PersistentVector` (32 elements, tail-full control)
- `rec`: `(->Rec 1 2 3)` from `(defrecord Rec [a b c])` — `bench39.Rec` (record control, declared-field and ext-map rows)
- `outer`: `{:sub m3 :other 1}` — `clojure.lang.PersistentArrayMap` (nested map for `update` row)

### Ad hoc AOT results (median ns/op of 7 runs x 2,000,000 ops, `JOLT_GC_TRIP_BYTES=16m`, A1 -> B -> A2)

| Row | Parent A1 (ns/op) | Candidate B (ns/op) | Parent A2 (ns/op) | Speedup (avg A / B) | Notes |
|---|---:|---:|---:|---:|---|
| `assoc2-3-new` | 49.78 | **18.24** | 47.44 | **2.67x** | 2 pairs, both new on 3-key array map |
| `assoc2-3-exist` | 42.96 | **14.81** | 41.27 | **2.84x** | 2 pairs, both existing on 3-key array map |
| `assoc2-3-mix` | 44.70 | **17.13** | 45.52 | **2.63x** | 2 pairs, 1 existing + 1 new on 3-key array map |
| `assoc2-12-new` | 68.16 | **31.10** | 70.41 | **2.23x** | 2 pairs, both new on 12-keyword array map |
| `assoc2-12-exist` | 56.02 | **27.54** | 58.33 | **2.08x** | 2 pairs, both existing on 12-keyword array map |
| `assoc2-24-array-new` | 93.10 | **48.13** | 96.40 | **1.97x** | 2 pairs, both new on 24-keyword array map |
| `assoc2-24-array-exist` | 75.87 | **46.15** | 81.14 | **1.70x** | 2 pairs, both existing on 24-keyword array map |
| `assoc2-nil` | 48.97 | **7.99** | 49.65 | **6.17x** | 2 pairs on `nil` (direct array map build) |
| `assoc3-3-new` | 69.12 | **31.71** | 69.25 | **2.18x** | 3 pairs, all new on 3-key array map (re-measured after 3/4-pair simplification; avg of 31.07 / 32.34 vs prev 31.62 / 31.57) |
| `assoc3-3-exist` | 59.14 | **16.37** | 58.17 | **3.58x** | 3 pairs, all existing on 3-key array map (re-measured; avg of 16.35 / 16.39 vs prev 16.33 / 16.32) |
| `assoc3-3-mix` | 64.47 | **29.99** | 64.70 | **2.15x** | 3 pairs, 1 existing + 2 new on 3-key array map (re-measured; avg of 30.13 / 29.85 vs prev 29.41 / 29.35) |
| `assoc4-3-new` | 93.17 | **38.55** | 92.34 | **2.41x** | 4 pairs, all new on 3-key array map (re-measured; avg of 38.22 / 38.87 vs prev 41.01 / 42.24) |
| `assoc4-12-exist` | 105.10 | **37.36** | 105.17 | **2.81x** | 4 pairs, all existing on 12-keyword array map (re-measured; avg of 37.67 / 37.05 vs prev 37.21 / 37.72) |
| `assoc4-3-mix` | 83.66 | **31.84** | 84.11 | **2.63x** | 4 pairs, 2 existing + 2 new on 3-key array map (re-measured; avg of 32.02 / 31.66 vs prev 39.74 / 39.55) |
| `swap2-3-exist` | 75.06 | **49.28** | 77.49 | **1.55x** | `(swap! a assoc :k0 i :k2 (+ i 1))` value-pos |
| `swap1-3-exist` | 33.91 | **31.60** | 34.84 | **1.09x** | `(swap! a assoc :k0 i)` 1-pair value-pos |
| `update2-3-exist` | 168.41 | **140.27** | 171.44 | **1.21x** | `(update outer :sub assoc :k0 i :k2 (+ i 1))` |
| `valpos2-3-exist` | 54.51 | **29.46** | 54.26 | **1.85x** | indirect `(f m3 :k0 i :k2 (+ i 1))` |
| `ctrl-hash-24` | 104.38 | 105.29 | 105.24 | 1.00x | miss-path control (24-key hash-mode map) |
| `ctrl-vec-32` | 65.64 | 63.60 | 64.15 | 1.02x | miss-path control (32-elem tail-full vector) |
| `ctrl-rec-field` | 62.36 | 63.19 | 62.32 | 0.99x | miss-path control (record declared fields) |
| `ctrl-rec-ext` | 90.41 | 88.60 | 90.13 | 1.02x | miss-path control (record ext-map keys) |
| `ctrl-nonstable-3` | 54.59 | 51.36 | 53.05 | 1.05x | miss-path control (string keys, fallback without rest list) |

### Existing `bench/run.sh` row (`coll-ops 100000`, `JOLT_GC_TRIP_BYTES=16m`, A1 -> B -> A2)

| Row | Parent A1 (ms) | Candidate B (ms) | Parent A2 (ms) | Ratio (avg A / B) |
|---|---:|---:|---:|---:|
| `small-assoc` (`(assoc m :b 0 :z 1)`) | 1.16 | **0.54** | 1.25 | **2.23x** |
| `small-merge` | 0.87 | 0.84 | 0.90 | 1.05x |
| `hm-replace` | 23.94 | 24.74 | 25.56 | 1.00x |
| `v-assoc` | 8.98 | 8.92 | 10.64 | 1.10x |
| `coll-ops` total mean | 230.6 | 237.3 | 251.5 | 1.02x |

### Design note on simplified zero-`set!` 3- and 4-pair clauses

An initial 3/4-pair implementation using local `(let ((c cnt) ...) (set! ...))` boxed every assigned local under Chez's source-loaded test runner (144 bytes/call for 3-pair, 176 bytes/call for 4-pair on no-op batches). Rewriting those steps as pure `let*` per-step count/duplicate checks (falling back to `jolt-assoc-fb3`/`fb4` on promotion or repeated new keys) plus one `sa-vector-append` and a small `amap-put!` macro keeps no-op allocation at 0 extra bytes on all three arities (24-keyword array map bytes/op: 2p 544 vs 1,024; 3p 576 vs 1,552; 4p 608 vs 2,096; no-op 0/0/0), cuts ~100 lines from `host/chez/collections.ss`, holds 3-pair at parity, and speeds up 4-pair new/mix (`41.6 -> 38.5` ns/op and `39.6 -> 31.8` ns/op).

### Gate status

- `make -j8 test` aborts on this host at environmental failures: `certify` (requires JDK 21; host has JDK 25.0.2), `filelock` (lock-starvation failure under parallel load; also passed when rerun), and `lazyretain` (requires GNU `timeout`, not installed on macOS).
- Rerun of the full `SHARD-POOL` (208 targets) excluding `certify`, `filelock`, and `lazyretain` (`200` parallel `-j8` + `devbootsmoke`/`gatebootsmoke` + `fastpathratio`/`stdlibratio`/`pmoverhead` at `-j1`): all 205 targets in the filtered rerun exited 0, including `selfhost`, `arraymap` (92/92 with the 111,720-case exhaustive small matrix and compiled `(fn [m] (assoc m :ka 1 :kb 2 ...))` allocation checks), `portcheck`, `deadhost`, `recordshadow`, `footprint`, `vfaslceiling`, `attributioncheck`, `gambitseedcheck`, `gambitgencheck`, and `gambittwins`.
- Gambit did not run: `gambitcheck`, `gambitkernel`, `gambiteval`, `gambitboot`, `gambitunbound`, `gambitvars`, `gambitstatics`, and `gambitprofile` skipped because Gambit is not installed on this machine.

## jolt-38

Single-copy multi-arg `conj` for 2..3 elements on persistent vectors with tail room (plus a 1-element value-position clause) and single-allocation multi-key `dissoc` for 2..4 keys on array-mode maps with callback-free keys (`keyword-t?`, `fixnum?`, `char?`), reusing `jolt-39`'s `case-lambda` mechanism on `jolt-conj` and `jolt-dissoc` (with matching fixed clauses on the `records-coll.ss` and `host-table.ss` `jolt-dissoc` wrappers, and regenerated `host/gambit/records-gambit.ss`).

- **Parent commit measured against:** `7980e4388` (`perf/coll-edit-fusion`)
- **Date:** 2026-11-24
- **Machine:** Apple M3 Ultra (Virtual), macOS 15.7.3
- **Chez:** 10.4.1 (`JOLT_CHEZ=/Users/admin/src/jolt/.cache/local/chezscheme-10.4.1/bin/scheme`)
- **Build flags:** `bin/jolt build -m <ns> -o <out> --direct-link --opt`
- **Nursery pin:** `JOLT_GC_TRIP_BYTES=16m` (isolates adaptive nursery doubling investigated in `jolt-44`)
- **Gambit:** not installed on this machine; `portcheck`, `gambitseedcheck`, `gambitgencheck`, and `gambittwins` passed; Gambit-runtime gates (`gambitcheck`, `gambitkernel`, `gambiteval`, `gambitboot`, `gambitunbound`, `gambitvars`, `gambitstatics`, `gambitprofile`) did not run.

### Corpus arity split (50 repos, `/tmp/jolt-opt-corpus-20261123`)

Scanned 15,313 `.clj`/`.cljc`/`.cljs` files (0 parse errors) with a standalone out-of-tree Clojure reader separating direct calls, thread-first (`->` / `some->` / `cond->`), and value-position (`swap!` / `update` / `update-in`) sites:

#### `conj` (elements added)

| Elements | Direct `(conj c ...)` | Threaded `(-> ... (conj ...))` | Combined direct + threaded | Multi-arg share | Value-pos (`swap!`/`update`/`update-in`) |
|---:|---:|---:|---:|---:|---:|
| 1 | 2,956 | 1,348 | 4,304 | — | 3,106 |
| **2** | **206** | **83** | **289** | **75.3%** | **11** |
| **3** | **42** | **14** | **56** | **14.6%** | **4** |
| 4 | 11 | 7 | 18 | 4.7% | 1 |
| 5 | 8 | 1 | 9 | 2.3% | 1 |
| 6+ | 9 | 3 | 12 | 3.1% | 2 |
| **Total ≥2** | **276** | **108** | **384** | **100.0%** | **19** |

2..3 elements cover **89.8%** (345 / 384) of multi-arg `conj` sites plus 3,106 1-element value-position sites. 4 elements is only 18 sites (4.7%) with no downstream chain-fusion consumer and is omitted.

#### `dissoc` (keys removed)

| Keys | Direct `(dissoc m ...)` | Threaded `(-> ... (dissoc ...))` | Combined direct + threaded | Multi-key share | Value-pos (`swap!`/`update`/`update-in`) |
|---:|---:|---:|---:|---:|---:|
| 1 | 1,605 | 903 | 2,508 | — | 511 |
| **2** | **391** | **191** | **582** | **58.5%** | **29** |
| **3** | **136** | **67** | **203** | **20.4%** | **3** |
| **4** | **50** | **34** | **84** | **8.4%** | **3** |
| 5 | 25 | 13 | 38 | 3.8% | 0 |
| 6+ | 55 | 33 | 88 | 8.8% | 4 |
| **Total ≥2** | **657** | **338** | **995** | **100.0%** | **39** |

2..4 keys cover **87.3%** (869 / 995) of multi-key `dissoc` sites (**95.1%** all-keyword-literal: 946 / 995), plus 511 1-key and 35 2..4-key value-position sites. All three arities (2, 3, and 4 keys) are shipped, with matching fixed clauses on the `records-coll.ss` and `host-table.ss` wrappers.

### Emitted Scheme (before and after)

Real corpus call sites (`clj-kondo/corpus/.clj-kondo/hooks/better_cond.clj_kondo:15` and `clj-kondo/src/clj_kondo/impl/analyzer.clj:2740`):

```clojure
(fn [new-body lhs rhs] (conj new-body lhs rhs))
(fn [ctx] (dissoc ctx :idx :len))
```

Because `jolt-conj` and `jolt-dissoc` (plus its two load-time wrappers) carry fixed `case-lambda` clauses directly without modifying `op_registry.clj`, the emitted call head is unchanged before and after and dispatches directly into the fixed clauses without `apply` or rest-list allocation:

```scheme
;; 2-element conj (before and after):
(jolt-conj new-body lhs rhs)
;; 2-key dissoc (before and after):
(jolt-dissoc ctx _kc$5 _kc$6)
```

### Commands

```sh
# Built before (detached worktree at 7980e4388) and after edits:
(cd /tmp/jolt-38-bench && "$REPO/bin/jolt" build -m bench38 -o /tmp/jolt-38-bench/bench38-{base,cand} --direct-link --opt)
(cd "$REPO/bench" && JOLT_PWD="$PWD" "$REPO/bin/jolt" build -m coll-ops -o /tmp/jolt-38-bench/coll-ops-{base,cand} --direct-link --opt)
# Timed interleaved A1 -> B -> A2 (7 runs of 2,000,000 ops after 3 warmup runs, System/gc before each run):
JOLT_GC_TRIP_BYTES=16m /tmp/jolt-38-bench/bench38-{base,cand,base} 2000000
# coll-ops (n=100000, 5 runs, min ms per op):
JOLT_GC_TRIP_BYTES=16m /tmp/jolt-38-bench/coll-ops-{base,cand,base} 100000
```

### Benchmark receiver construction and asserted representations

Every receiver in `bench38` asserts its runtime class at startup via `(.getName ^Class (class obj))`:

- `v0`: `[]` — `clojure.lang.PersistentVector` (0 elements)
- `v5`: `[0 1 2 3 4]` — `clojure.lang.PersistentVector` (5 elements)
- `v20`: `(vec (range 20))` — `clojure.lang.PersistentVector` (20 elements)
- `v32`: `(vec (range 32))` — `clojure.lang.PersistentVector` (32 elements, tail-full control)
- `lst`: `'(1 2 3 4 5)` — `clojure.lang.PersistentList` (list control)
- `m5`: `{:k0 0 :k1 1 :k2 2 :k3 3 :k4 4}` — `clojure.lang.PersistentArrayMap` (5 keyword keys, literal)
- `m12`: `(reduce (fn [m i] (assoc m (keyword (str "k" i)) i)) {} (range 12))` — `clojure.lang.PersistentArrayMap` (12 keyword keys, array-mode)
- `m24`: `(reduce (fn [m i] (assoc m (keyword (str "k" i)) i)) {} (range 24))` — `clojure.lang.PersistentArrayMap` (24 keyword keys, array-mode under 64-keyword limit)
- `hm24`: `(apply hash-map (mapcat (fn [i] [(keyword (str "k" i)) i]) (range 24)))` — `clojure.lang.PersistentHashMap` (24 keyword keys, hash-mode control)
- `rec`: `(assoc (->Rec 1 2 3) :x 10 :y 20)` from `(defrecord Rec [a b c])` — `bench38.Rec` (record control, declared-field and ext-map rows)
- `m5-str`: `{:k0 0 "s1" 1 "s2" 2 :k3 3 :k4 4}` — `clojure.lang.PersistentArrayMap` (non-stable string-key fallback control)
- `outer-v` / `outer-m`: `{:v v5 :other 1}` / `{:m m5 :other 1}` — `clojure.lang.PersistentArrayMap` (`update` value-position controls)

### Ad hoc AOT results (median ns/op of 7 runs x 2,000,000 ops, `JOLT_GC_TRIP_BYTES=16m`, A1 -> B -> A2)

| Row | Parent A1 (ns/op) | Candidate B (ns/op) | Parent A2 (ns/op) | Speedup (avg A / B) | Notes |
|---|---:|---:|---:|---:|---|
| `conj2-v0` | 36.73 | **6.42** | 36.51 | **5.70x** | 2-arg `conj` on `[]` (direct `(vector a b)`) |
| `conj2-v5` | 46.55 | **11.14** | 46.48 | **4.18x** | 2-arg `conj` on 5-elem vector |
| `conj2-v20` | 73.40 | **16.04** | 73.45 | **4.58x** | 2-arg `conj` on 20-elem vector |
| `conj3-v5` | 68.56 | **12.54** | 66.83 | **5.40x** | 3-arg `conj` on 5-elem vector |
| `conj3-v20` | 107.57 | **17.55** | 107.33 | **6.12x** | 3-arg `conj` on 20-elem vector |
| `conj2-update-v5` | 70.72 | **35.05** | 71.46 | **2.03x** | `(update outer-v :v conj i 99)` |
| `conj1-valpos-v5` | 26.84 | **22.71** | 26.80 | **1.18x** | indirect 1-arg `(f v5 i)` |
| `conj2-valpos-v5` | 46.82 | **11.84** | 46.84 | **3.96x** | indirect 2-arg `(f v5 i 99)` |
| `ctrl-conj2-v32-full` | 42.75 | **37.08** | 42.82 | **1.15x** | miss control: 32-elem tail-full vector (no rest list) |
| `ctrl-conj3-v32-full` | 57.96 | **52.98** | 58.19 | **1.10x** | miss control: 32-elem tail-full vector, 3 items |
| `ctrl-conj2-list` | 31.29 | **27.23** | 31.25 | **1.15x** | miss control: 5-elem list (no rest list) |
| `dissoc2-m5-present` | 59.37 | **28.05** | 59.17 | **2.11x** | 2-key `dissoc` on 5-key array map, both present |
| `dissoc2-m5-absent` | 35.62 | **9.96** | 35.68 | **3.58x** | 2-key `dissoc` on 5-key array map, both absent (`identical?`, 0 alloc) |
| `dissoc3-m5-present` | 71.73 | **26.48** | 71.73 | **2.71x** | 3-key `dissoc` on 5-key array map, all present (re-measured after `amap-dissoc-keys` macro; prev 28.55 ns) |
| `dissoc3-m5-absent` | 42.44 | **10.26** | 43.44 | **4.19x** | 3-key `dissoc` on 5-key array map, all absent (`identical?`, 0 alloc; re-measured, prev 10.49 ns) |
| `dissoc4-m5-present` | 79.66 | **26.06** | 80.23 | **3.07x** | 4-key `dissoc` on 5-key array map, all present (re-measured after `amap-dissoc-keys` macro; prev 27.65 ns) |
| `dissoc4-m5-absent` | 54.48 | **11.79** | 54.70 | **4.63x** | 4-key `dissoc` on 5-key array map, all absent (`identical?`, 0 alloc; re-measured, prev 12.09 ns) |
| `dissoc2-m12-present` | 75.11 | **41.72** | 72.90 | **1.77x** | 2-key `dissoc` on 12-keyword array map |
| `dissoc2-m24-present` | 99.77 | **62.58** | 99.46 | **1.59x** | 2-key `dissoc` on 24-keyword array map |
| `dissoc3-m24-present` | 143.95 | **82.51** | 145.53 | **1.75x** | 3-key `dissoc` on 24-keyword array map (re-measured after `amap-dissoc-keys` macro; prev 80.96 ns) |
| `dissoc4-m24-present` | 196.39 | **80.18** | 197.55 | **2.46x** | 4-key `dissoc` on 24-keyword array map (re-measured after `amap-dissoc-keys` macro; prev 79.94 ns) |
| `dissoc2-update-m5` | 86.33 | **52.70** | 85.96 | **1.63x** | `(update outer-m :m dissoc :k1 :k3)` |
| `dissoc2-valpos-m5` | 60.31 | **28.64** | 59.83 | **2.10x** | indirect 2-key `(f m5 :k1 :k3)` |
| `ctrl-dissoc2-hash-24` | 116.03 | **95.28** | 113.09 | **1.20x** | miss control: 24-key hash-mode map (wrappers no longer `apply`) |
| `ctrl-dissoc2-rec-ext` | 86.11 | **72.99** | 86.11 | **1.18x** | miss control: record ext-map keys (re-measured after folding through `jrec-dissoc-many`; prev 69.23 ns, still faster than parent) |
| `ctrl-dissoc2-rec-field` | 215.07 | **202.16** | 214.79 | **1.06x** | miss control: record declared field + ext key (re-measured after folding through `jrec-dissoc-many`; prev 198.61 ns, still faster than parent) |
| `ctrl-dissoc2-nonstable` | 61.65 | **43.59** | 60.57 | **1.40x** | miss control: string + keyword on 5-key array map |

### Notes on timings and simplification

The two-element `conj` gain is about 4x because the single-step path it replaces (`vec-snoc`, `host/chez/collections.ss` near line 142) still uses `make-vector` plus a copy loop, so each old step cost about 24.5 ns. The miss-control rows got faster because `jolt-conj` no longer builds a rest list or calls `fold-left`, and `jolt-dissoc` loses a rest list and an `apply` in each of the two wrappers.

Replacing the duplicated 3-key and 4-key array-map `dissoc` clauses with the `amap-dissoc-keys` `syntax-rules` macro leaves the 3/4-key rows unchanged within measurement noise (the 2-key clause stays on `vec-remove4`), and folding record receivers through `jrec-dissoc-many` adds ~3.6–3.8 ns relative to the duplicated inline record clauses while remaining faster than the parent commit (`72.99` vs `86.11` ns for ext-map keys, `202.16` vs `214.93` ns for declared field + ext key).

### Existing `bench/run.sh` row (`coll-ops 100000`, `JOLT_GC_TRIP_BYTES=16m`, A1 -> B -> A2)

| Row | Parent A1 (ms) | Candidate B (ms) | Parent A2 (ms) | Ratio (avg A / B) |
|---|---:|---:|---:|---:|
| `l-conj` (`(reduce conj () ks)`) | 1.52 | **1.05** | 1.46 | **1.42x** |
| `v-conj` (`(reduce conj [] ks)`) | 3.85 | **3.43** | 3.95 | **1.14x** |
| `hm-dissoc` (`(reduce dissoc hm ks)`) | 21.58 | **20.18** | 22.42 | **1.09x** |
| `hs-build` (`(reduce conj #{} ks)`) | 24.91 | **24.08** | 25.24 | **1.04x** |
| `small-assoc` | 0.48 | 0.50 | 0.49 | 0.97x |
| `coll-ops` total mean | 221.3 | **218.6** | 233.1 | **1.04x** |

### Allocation checks (compiled Clojure fns, `test/chez/arraymap-test.ss`)

- 20-element vector `conj`: 2-arg 304 vs 512 bytes; 3-arg 304 vs 768 bytes.
- 24-keyword array map `dissoc`: 2-key 432 vs 864 bytes; 3-key 416 vs 1,264 bytes; 4-key 400 vs 1,648 bytes; all-absent 2/3/4-key 16/16/16 bytes (0 extra bytes over base 16, proving both outer `jolt-dissoc` wrappers bypass `apply` and rest-list consing).

### Gate status

- Filtered full `SHARD-POOL` (205 of 208 targets, excluding the three host environmental failures `certify` [requires JDK 21], `lazyretain` [requires GNU `timeout`], and `filelock` [pre-existing `starved after 10000 cycles` failure on this host]): `200` parallel `-j8` + `devbootsmoke`/`gatebootsmoke` at `-j1` + `fastpathratio`/`stdlibratio`/`pmoverhead` at `-j1` — all **205/205** targets exited 0, including `selfhost`, `arraymap` (116/116 with the 466-case exhaustive `conj` matrix, 40,248-case exhaustive `dissoc` matrix, 1,500-trial randomized differential `dissoc` test, compiled fn allocation assertions, and 1-arg/5+/non-stable-position/record/sorted-map variadic clause tests; mutant-checked tests: the exhaustive `conj` matrix, the exhaustive `dissoc` matrix, and the 1-element value-position `conj` metadata test by removing `meta-carry-conj` from that clause and observing `arraymap` fail at 115/116), `sortedcoll` (145/145), `recfast` (518/518), `rrbprop` (11,170/11,170), `portcheck`, `deadhost`, `recordshadow`, `footprint`, `vfaslceiling`, `attributioncheck`, `gambitseedcheck`, `gambitgencheck`, and `gambittwins`.
- Gambit did not run: `gambitcheck`, `gambitkernel`, `gambiteval`, `gambitboot`, `gambitunbound`, `gambitvars`, `gambitstatics`, and `gambitprofile` skipped because Gambit is not installed on this machine. All new identifiers in `collections.ss`, `records-coll.ss`, and `host-table.ss` were audited against the Gambit boot files, and `host/gambit/records-gambit.ss` was regenerated via `make gambitgen` and verified by `gambitgencheck`.
