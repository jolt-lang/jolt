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

## jolt-41

Emit-time peephole in `jolt-core/jolt/backend_scheme/fused.clj` collapsing straight-line nested or threaded `assoc` chains totaling 2..4 pairs into one `(jolt-assoc m k1 v1 ...)` call (dispatching directly to the fixed `case-lambda` clauses shipped in `jolt-39`), sharing `pure-call-head?` from `jolt-core/jolt/passes/inline.clj` over `op-registry/pure-ops`.

- **Parent commit measured against:** `9ae023f4f` (`perf/coll-edit-fusion`)
- **Date:** 2026-11-24
- **Machine:** Apple M3 Ultra (Virtual), macOS 15.7.3
- **Chez:** 10.4.1 (`JOLT_CHEZ=/Users/admin/src/jolt/.cache/local/chezscheme-10.4.1/bin/scheme`)
- **Build flags:** `bin/jolt build -m bench41 -o <out> --direct-link --opt`
- **Nursery pin:** `JOLT_GC_TRIP_BYTES=16m` (isolates adaptive nursery doubling investigated in `jolt-44`)
- **Semantics decisions relied on (`coll-edit-fusion-decisions.md`):** Decision 1 (pure-op calls in later operands may evaluate before an earlier edit; observable only when both throw) and Decision 3 (no identity guarantee under raw Scheme `call/cc` re-entry). A throw from a fused call may report the outermost call's line.
- **Seed mint impact:** The peephole runs during `make remint` (converged in 3 passes, 0 forms skipped). Exactly **1** seed call site fused in `host/chez/seed/prelude.ss` (and `host/gambit/seed/prelude.ss`): `clojure.core/shuffle` (`jolt-core/clojure/core/21-coll.clj:23`, collapsing `(assoc (assoc v i (nth v j)) j t)` into `(jolt-assoc v i (jolt-nth/i v j) j t)`; `(jolt-assoc ` `3 -> 4`, `(jolt-assoc3 ` `89 -> 87`). **0** call sites fused in `host/chez/seed/image.ss` (`(jolt-assoc ` `61 -> 61`, `(jolt-assoc3 ` `249 -> 249`). Per the seed rule, seed files were restored before committing (`git checkout -- host/chez/seed host/gambit/seed`).
- **Gambit:** not installed on this machine; `gambitseedcheck`, `gambitgencheck`, `gambittwins`, and `portcheck` passed on the reminted tree; Gambit-runtime gates (`gambitcheck`, `gambitkernel`, `gambiteval`, `gambitboot`, `gambitunbound`, `gambitvars`, `gambitstatics`, `gambitprofile`) did not run.

### Corpus recount (50 repos, 15,313 `.clj`/`.cljc`/`.cljs` files, `/tmp/jolt-opt-corpus-20261123`)

Scanned all 15,313 files with a standalone out-of-tree Clojure reader/scanner expanding `->` and applying the exact shipped operand rule (first step unrestricted; later steps restricted to locals, constants, or calls to `op-registry/pure-ops` / keyword lookups at valid arity). Both strict lexical-binding tracking (`lex`) and counting unqualified non-earmuffed macro-bound symbols as locals (`unq`) are reported:

| Pattern | Syntactic | Eligible (`lex`) | Eligible (`unq`) | Strict local+const only (`lex` / `unq`) | Notes |
|---|---:|---:|---:|---:|---|
| `assoc` chain, 2 pairs | 298 | **170** | 198 | — | |
| `assoc` chain, 3 pairs | 178 | **59** | 62 | — | |
| `assoc` chain, 4 pairs | 99 | **67** | 70 | — | |
| **`assoc` chain, 2..4 pairs total** | **575** | **296** | **330** | **195 / 206** | Decision 1 adds +101 (+52%) / +124 (+60%) sites; +48 syntactic chains have 5+ pairs (623 total, unfused) |
| **`dissoc` -> `assoc` (1-key -> 1-pair, for `jolt-46`)** | **141** | **84** | **93** | 78 (`lex`) | `aleph/http.clj:483` (`(- end start)`) qualifies (`-` is pure) |
| `dissoc` -> `assoc` (all arities, for `jolt-46`) | 317 | 168 | 193 | — | |
| **`assoc` -> `dissoc` (1-pair -> 1-key, for `jolt-46`)** | **118** | **117** | **118** | 117 (`lex`) | |
| `assoc` -> `dissoc` (all arities, for `jolt-46`) | 211 | 209 | 210 | — | |

- **`clj-kondo/impl/analyzer.clj:1118`** (`(-> ctx (assoc :fn-args (:children arg-vec)) (assoc :body-children-count (count children)) (assoc :pre-post-map (some? pre-post-map)))`): **does not fuse** because `count` is not in `op-registry/pure-ops` (`:pure?` means pure and total for `inline.clj` duplication/discard, so `count` stays out) and chains with an ineligible later step are left unfused rather than partially split.

### Emitted Scheme (before and after), emitted bytes, and compile time

Real corpus site (`riemann/src/riemann/graphite.clj:160-161`, 2-step chain with pure keyword lookup) and 3-step grow (`chain3-grow`):

```clojure
;; riemann/graphite.clj:160-161
(defn pool-opts [m opts]
  (-> m
      (assoc :size (:pool-size opts))
      (assoc :regenerate-interval (:reconnect-interval opts))))

;; 3-step grow (m4 -> 7)
(defn chain3-grow [m x y z]
  (-> m (assoc :k4 x) (assoc :k5 y) (assoc :k6 z)))
```

```scheme
;; riemann/graphite.clj:160-161 BEFORE (143 bytes at call expr, 1,023 bytes full def):
(let* ((_a$26 (jolt-assoc3 m _kc$20 (jolt-kw-get-site opts _kc$22 _kc$21)))
       (_a$27 _kc$23)
       (_a$28 (jolt-kw-get-site opts _kc$25 _kc$24)))
  (jolt-assoc3 _a$26 _a$27 _a$28))

;; riemann/graphite.clj:160-161 AFTER (139 bytes at call expr, 1,019 bytes full def):
(let* ((_a$8 m)
       (_a$9 _kc$2)
       (_a$10 (jolt-kw-get-site opts _kc$4 _kc$3))
       (_a$11 _kc$5)
       (_a$12 (jolt-kw-get-site opts _kc$7 _kc$6)))
  (jolt-assoc _a$8 _a$9 _a$10 _a$11 _a$12))

;; chain3-grow BEFORE (67 bytes at call expr, 872 bytes full def):
(jolt-assoc3 (jolt-assoc3 (jolt-assoc3 m _kc$46 x) _kc$47 y) _kc$48 z)

;; chain3-grow AFTER (38 bytes at call expr, -29 bytes / -43%; 843 bytes full def):
(jolt-assoc m _kc$36 x _kc$37 y _kc$38 z)
```

For `chain2-arith` (`(-> m (assoc :k0 x) (assoc :dt (- end start)))`), fusion also eliminates the ordering `let*` wrapper between the two `assoc` steps: call expression shrinks from **109 to 43 bytes (-66 bytes / -61%)** (925 -> 859 bytes full def).

- **Compile time (`bench41`, `bin/jolt build --direct-link --opt`):** parent **22.003 s** vs candidate **21.984 s** (`JOLT_BUILD_PROFILE=1`: `emit app namespaces` 118 ms vs 116 ms, `emit: emit-top` 7 ms vs 7 ms, cumulative pre-Chez 307 ms vs 307 ms — no measurable overhead).

### Commands

```sh
# Built before (detached worktree at 9ae023f4f) and after (reminted worktree):
(cd /tmp/jolt-41-bench && JOLT_PWD="$PWD" /tmp/jolt-41-parent/bin/jolt build -m bench41 -o /tmp/jolt-41-bench/bench41-base --direct-link --opt)
(cd /tmp/jolt-41-bench && JOLT_PWD="$PWD" "$REPO/bin/jolt" build -m bench41 -o /tmp/jolt-41-bench/bench41-cand --direct-link --opt)
# Timed interleaved A1 -> B -> A2 (7 runs of 2,000,000 ops after 3 warmup runs, System/gc before each run):
JOLT_GC_TRIP_BYTES=16m /tmp/jolt-41-bench/bench41-base 2000000
JOLT_GC_TRIP_BYTES=16m /tmp/jolt-41-bench/bench41-cand 2000000
JOLT_GC_TRIP_BYTES=16m /tmp/jolt-41-bench/bench41-base 2000000
```

### Benchmark receiver construction and asserted representations

Every receiver in `bench41` asserts its runtime class at startup via `(.getName ^Class (class obj))`:

- `m3`: `{:k0 0 :k1 1 :k2 2}` — `clojure.lang.PersistentArrayMap` (3 keys, literal)
- `m4`: `{:k0 0 :k1 1 :k2 2 :k3 3}` — `clojure.lang.PersistentArrayMap` (4 keys, literal; `(chain3-grow m4 4 5 6)` also asserted `clojure.lang.PersistentArrayMap` at 7 keys)
- `m12`: `(reduce (fn [m i] (assoc m (keyword (str "k" i)) i)) {} (range 12))` — `clojure.lang.PersistentArrayMap` (12 keyword keys, array-mode)
- `opts`: `{:pool-size 16 :reconnect-interval 5000}` — `clojure.lang.PersistentArrayMap` (2 keys, literal)
- `hm24`: `(apply hash-map (mapcat (fn [i] [(keyword (str "k" i)) i]) (range 24)))` — `clojure.lang.PersistentHashMap` (24 keyword keys, hash-mode miss control)
- `v32`: `(vec (range 32))` — `clojure.lang.PersistentVector` (32 elements, tail-full vector miss control)
- `rec`: `(->Rec 1 2 3)` from `(defrecord Rec [a b c])` — `bench41.Rec` (record miss control, declared-field and ext-map rows)

### Ad hoc AOT results (median ns/op of 7 runs x 2,000,000 ops, `JOLT_GC_TRIP_BYTES=16m`, A1 -> B -> A2)

| Row | Parent A1 (ns/op) | Candidate B (ns/op) | Parent A2 (ns/op) | Speedup (avg A / B) | Notes |
|---|---:|---:|---:|---:|---|
| `chain2-m3-replace` | 37.02 | **13.05** | 36.20 | **2.81x** | 2-step `assoc` replacing 2 existing keys on 3-key array map |
| `chain2-m3-new` | 43.58 | **16.26** | 43.03 | **2.66x** | 2-step `assoc` adding 2 new keys on 3-key array map |
| `chain3-m3-replace` | 56.82 | **15.49** | 55.13 | **3.61x** | 3-step `assoc` replacing all 3 keys on 3-key array map |
| `chain2-m4-grow6` | 44.85 | **17.40** | 44.78 | **2.58x** | 2-step `assoc` growing 4-key array map to 6 keys |
| `chain3-m4-grow7` | 68.40 | **31.28** | 68.77 | **2.19x** | 3-step `assoc` growing 4-key array map to 7 keys |
| `chain4-m4-grow8` | 93.82 | **39.27** | 93.76 | **2.39x** | 4-step `assoc` growing 4-key array map to 8 keys |
| `chain2-m12-new` | 62.03 | **30.67** | 62.23 | **2.03x** | 2-step `assoc` adding 2 new keys on 12-keyword array map |
| `chain2-nil` | 43.22 | **6.21** | 44.93 | **7.10x** | 2-step `assoc` on `nil` (direct array map construction) |
| `chain2-pure-arith` | 39.14 | **15.91** | 39.44 | **2.47x** | 2-step with pure `(- end start)` later operand (Decision 1) |
| `chain2-riemann-shape` | 50.59 | **21.77** | 49.41 | **2.30x** | 2-step with pure `(:reconnect-interval opts)` later operand |
| `ctrl-hash24-chain2` | 85.95 | 86.55 | 87.04 | 1.00x | miss control: 2-step on 24-key hash-mode map |
| `ctrl-hash24-chain3` | 140.00 | 141.40 | 141.52 | 1.00x | miss control: 3-step on 24-key hash-mode map |
| `ctrl-vec32-chain3` | 95.01 | **87.51** | 94.08 | **1.08x** | miss control: 3-step on 32-elem vector (1 `meta-carry` vs 3) |
| `ctrl-rec-fields-c3` | 75.68 | **68.63** | 76.33 | **1.11x** | miss control: 3-step on record declared fields (1 `meta-carry` vs 3) |
| `ctrl-rec-ext-c2` | 76.84 | **73.84** | 76.78 | **1.04x** | miss control: 2-step on record ext keys (1 `meta-carry` vs 2) |

### Gate status

- **Reminted worktree (filtered full `SHARD-POOL`, 205 of 208 targets):** excluded the three pre-existing host environmental failures (`certify` [requires JDK 21], `lazyretain` [requires GNU `timeout`], and `filelock` [pre-existing `starved after 10000 cycles` failure]). Ran `200` parallel targets (`-j8`) + `devbootsmoke`/`gatebootsmoke` (`-j1`) + `fastpathratio`/`stdlibratio`/`pmoverhead` (`-j1`) — all **205/205** targets exited 0, including `selfhost`, `gambitseedcheck`, `gambitgencheck`, `gambittwins`, `emittedheads` (293 heads, 0 shadowable), `inline` (376/376 with 30 new assertions: 12 positive emitted-head tests, 11 negative tests including `kondo:1118` `count`, `:clear-caps`, and 5-step / 3+2 no-tiling, 3 evaluation-order tests, `loop-maps` and `types.clj` record-field-demotion regressions, 7-receiver result equivalence, and 400-trial randomized differential test), `irvalidate`, `arraymap` (116/116), `recfast` (518/518), `recfold` (13/13), `oparity` (304/304), `directlink` (32/32), `cts`, `unit`, `corpus`, `footprint`, `vfaslceiling`, `portcheck`, `deadhost`, `recordshadow`, and `attributioncheck`.
- **Mutant-checked tests (`test/chez/inline-test.ss`):** M1 (disable `emit-assoc-chain` -> 14 emitted-head tests fail, `362/376`), M2 (drop `:clear-caps` check -> both `:clear-caps` emission tests fail + `^:once` capture-retry runtime test throws NPE), M3 (drop `*unfused-assoc-nodes*` suppression -> 5-step no-tiling test fails, `375/376`), M4 (drop `later-pure?` check -> all 7 impure negative tests + throw-before-effectful-operand test fail + `^:once` retry throws).
- **Committed tree (seeds restored per the seed rule):** `selfhost`, `gambitseedcheck`, and `inline` (14 emitted-head assertions, `362/376`) fail strictly for seed staleness until `make remint` (or `jolt-45`); `emittedheads` and `attributioncheck` pass on the committed tree.
- **Gambit did not run:** `gambitboot`, `gambitcheck`, `gambiteval`, `gambitkernel`, `gambitprofile`, `gambitstatics`, `gambitunbound`, and `gambitvars` skipped because Gambit is not installed on this machine.

## jolt-46

Emit-time peephole in `jolt-core/jolt/backend_scheme/fused.clj` and portable runtime helpers `jolt-dissoc1-assoc1` and `jolt-assoc1-dissoc1` (sharing one `amap-edit-pair` `syntax-rules` macro taking a literal order flag, with `vec-remove2-append2`) in `host/chez/collections.ss`, fusing adjacent single-key `dissoc` / single-pair `assoc` pairs in both orders under the shared `pure-edit-operand?` / `pure-call-head?` operand rule from `jolt-41`. Both orders were built based on `jolt-41`'s 50-repo corpus recount (`84`/`93` eligible single `dissoc`->`assoc` sites including `aleph/http.clj:483`, and `117`/`118` eligible single `assoc`->`dissoc` sites). Where both rules could match (e.g. `(-> m (dissoc :a) (assoc :b 1) (assoc :c 2))` or `(-> m (assoc :a 1) (assoc :b 2) (dissoc :c))`), `emit-assoc-chain` keeps precedence.

- **Parent commit measured against:** `e0f6096ce` (`perf/coll-edit-fusion`)
- **Date:** 2026-11-24
- **Machine:** Apple M3 Ultra (Virtual), macOS 15.7.3
- **Chez:** 10.4.1 (`JOLT_CHEZ=/Users/admin/src/jolt/.cache/local/chezscheme-10.4.1/bin/scheme`)
- **Build flags:** `bin/jolt build -m bench46 -o <out> --direct-link --opt`
- **Nursery pin:** `JOLT_GC_TRIP_BYTES=16m` (isolates adaptive nursery doubling investigated in `jolt-44`)
- **Semantics decisions relied on (`coll-edit-fusion-decisions.md`):** Decision 1 (pure-op calls in outer operands may evaluate before the inner edit; observable only when both throw) and Decision 3 (no identity guarantee under raw Scheme `call/cc` re-entry). Smaller relaxations: net-unchanged non-promoting `assoc`->`dissoc` of an absent same key returns the `identical?` receiver; a throw from a fused call may report the outermost call's line; fallback runs the live single-step bindings `(jolt-assoc3 (jolt-dissoc2 m dk) ak av)` / `(jolt-dissoc2 (jolt-assoc3 m ak av) dk)` in order; fast paths take `stable-hamt-key?` keys only.
- **Seed mint impact:** `make remint` converged in 3 passes (0 forms skipped). **0** sites fused in `host/chez/seed/prelude.ss`; **1** site fused in `host/chez/seed/image.ss` (and `host/gambit/seed/image.ss`): `jolt.analyzer/stamp-def` (`jolt-core/jolt/analyzer.clj:1445`, collapsing `(assoc (dissoc env :top?) :cx :eval)` into `(jolt-dissoc1-assoc1 env _kc$40753 _kc$40754 _kc$40755)`). Per the seed rule, seed files were restored before committing (`git checkout -- host/chez/seed host/gambit/seed`).
- **Gambit:** not installed on this machine; `portcheck`, `gambitseedcheck`, `gambitgencheck`, and `gambittwins` passed on the reminted tree, and every identifier added to `collections.ss` was audited against `host/gambit/scheme-adapter-runtime.ss`; Gambit-runtime gates (`gambitboot`, `gambitcheck`, `gambiteval`, `gambitkernel`, `gambitprofile`, `gambitstatics`, `gambitunbound`, `gambitvars`) did not run.

### Emitted Scheme (before and after) and compile time

Real corpus site (`aleph/src/aleph/http.clj:483-485`, `cleanup-conn`):

```clojure
(defn cleanup-conn [rsp end start]
  (-> rsp
      (dissoc :aleph/destroy-conn?)
      (assoc :connection-time (- end start))))
```

```scheme
;; BEFORE (109 bytes at call expr, 867 bytes full def):
(let* ((_a$4 (jolt-dissoc2 rsp _kc$2))
       (_a$5 _kc$3)
       (_a$6 (jolt-n- end start)))
  (jolt-assoc3 _a$4 _a$5 _a$6))

;; AFTER (53 bytes at call expr, -56 bytes / -51%; 809 bytes full def):
(jolt-dissoc1-assoc1 rsp _kc$2 _kc$3 (jolt-n- end start))
```

- **Compile time (`bench46`, `bin/jolt build --direct-link --opt`):** parent **22.83 s** vs candidate **22.42 s** (`JOLT_BUILD_PROFILE=1`: `emit app namespaces` 118 ms vs 126 ms, `emit: emit-top` 14 ms vs 13 ms, cumulative pre-Chez 320 ms vs 341 ms — no measurable compile-time regression).

### Commands

```sh
# Built before (detached worktree at e0f6096ce, reminted) and after (reminted worktree):
(cd /tmp/jolt-46-bench && JOLT_PWD="$PWD" /tmp/jolt-46-parent/bin/jolt build -m bench46 -o /tmp/jolt-46-bench/bench46-base --direct-link --opt)
(cd /tmp/jolt-46-bench && JOLT_PWD="$PWD" "$REPO/bin/jolt" build -m bench46 -o /tmp/jolt-46-bench/bench46-cand --direct-link --opt)
# Timed interleaved A1 -> B -> A2 (7 runs of 2,000,000 ops after 3 warmup runs, System/gc before each run):
JOLT_GC_TRIP_BYTES=16m /tmp/jolt-46-bench/bench46-base 2000000
JOLT_GC_TRIP_BYTES=16m /tmp/jolt-46-bench/bench46-cand 2000000
JOLT_GC_TRIP_BYTES=16m /tmp/jolt-46-bench/bench46-base 2000000
```

### Benchmark receiver construction and asserted representations

Every receiver in `bench46` asserts its runtime class at startup via `(.getName ^Class (class obj))`:

- `m5`: `{:k0 0 :k1 1 :k2 2 :k3 3 :k4 4}` — `clojure.lang.PersistentArrayMap` (5 keyword keys, literal)
- `m12`: `(reduce (fn [m i] (assoc m (keyword (str "k" i)) i)) {} (range 12))` — `clojure.lang.PersistentArrayMap` (12 keyword keys, array-mode)
- `hm24`: `(apply hash-map (mapcat (fn [i] [(keyword (str "k" i)) i]) (range 24)))` — `clojure.lang.PersistentHashMap` (24 keyword keys, hash-mode miss control)
- `rec`: `(assoc (->Rec 1 2 3) :x 10)` from `(defrecord Rec [a b c])` — `bench46.Rec` (record miss control, ext-map and declared-field rows)
- `m5-str`: `{:k0 0 :k1 1 "s2" 2 :k3 3 :k4 4}` — `clojure.lang.PersistentArrayMap` (non-stable string-key miss control)
- `rsp-http2`: `{:status 200 :headers {"content-type" "application/json"} :aleph/keep-alive? true :aleph/destroy-conn? false :body :stream}` — `clojure.lang.PersistentArrayMap` (matches `aleph/http/http2.clj:1166-1170`)
- `rsp-http1`: `(NettyResponseStandIn. rsp-http2)` — `bench46.NettyResponseStandIn` (`deftype` implementing `IPersistentMap`, `ILookup`, `Counted`, `Seqable` standing in for Potemkin's `NettyResponse`, because Potemkin is not in the local Maven cache)
- Tail-full vector control: omitted because `(dissoc v k)` throws `ClassCastException` on vectors (covered as an error test in `test/chez/arraymap-test.ss`).

### Ad hoc AOT results (median ns/op of 7 runs x 2,000,000 ops, `JOLT_GC_TRIP_BYTES=16m`, A1 -> B -> A2)

| Row | Parent A1 (ns/op) | Candidate B (ns/op) | Parent A2 (ns/op) | Speedup (avg A / B) | Notes |
|---|---:|---:|---:|---:|---|
| `da-m5-present` | 53.09 | **24.50** | 53.52 | **2.18x** | 5-key array map, `dk` present, `ak` new (audit prototype: 1.44x) |
| `da-m5-absent` | 39.23 | **19.92** | 40.32 | **2.00x** | 5-key array map, `dk` absent, `ak` new (audit prototype: 0.97x) |
| `da-m5-present-upd` | 51.56 | **26.32** | 53.20 | **1.99x** | 5-key array map, `dk` present, `ak` existing |
| `da-m5-absent-noop` | 27.31 | **10.84** | 27.33 | **2.52x** | 5-key array map, `dk` absent & `ak` unchanged (`identical?`, 0 alloc) |
| `da-m5-same-key` | 45.82 | **28.65** | 46.54 | **1.61x** | 5-key array map, `dk = ak` present (remove-then-append to end) |
| `da-m12-present` | 72.89 | **42.68** | 75.49 | **1.74x** | 12-keyword array map, `dk` present, `ak` new |
| `da-aleph-http2` | 52.27 | **24.49** | 52.84 | **2.15x** | `aleph/http.clj:483` shape on HTTP/2 map literal |
| `da-aleph-http1` | 214.16 | 200.64 | 205.79 | 1.05x | `aleph/http.clj:483` shape on `NettyResponseStandIn` `deftype` (misses `pmap?`) |
| `ad-m5-present` | 54.63 | **24.50** | 54.69 | **2.23x** | 5-key array map, `assoc`->`dissoc`, `dk` present, `ak` new |
| `ad-m5-absent` | 39.79 | **19.54** | 39.69 | **2.03x** | 5-key array map, `assoc`->`dissoc`, `dk` absent, `ak` new |
| `ad-m5-present-upd` | 52.76 | **25.86** | 53.08 | **2.05x** | 5-key array map, `assoc`->`dissoc`, `ak` existing, `dk` present |
| `ad-m12-present` | 76.76 | **42.97** | 75.67 | **1.77x** | 12-keyword array map, `assoc`->`dissoc`, `dk` present, `ak` new |
| `ctrl-da-hash24` | 122.39 | 123.49 | 120.95 | 0.99x | miss control: 24-key hash-mode map (`+1.82` ns guard) |
| `ctrl-ad-hash24` | 124.75 | 123.40 | 121.60 | 1.00x | miss control: 24-key hash-mode map (`+0.22` ns guard) |
| `ctrl-da-rec-ext` | 92.55 | 91.16 | 91.58 | 1.01x | miss control: record ext-map keys |
| `ctrl-ad-rec-ext` | 101.99 | 103.91 | 103.03 | 0.99x | miss control: record ext-map keys |
| `ctrl-da-rec-field` | 147.64 | 148.44 | 147.17 | 0.99x | miss control: record declared-field downgrade (`+1.03` ns guard) |
| `ctrl-da-nonstable` | 57.40 | 61.61 | 57.76 | 0.93x | miss control: non-stable string key on 5-key array map (3-round rerun: `59.34` vs `59.55` pre) |
| `ctrl-da-nil` | 36.13 | 36.89 | 36.99 | 0.99x | miss control: `nil` receiver (`+0.33` ns guard) |

### Notes on timings

- **`da-m5-absent` (2.00x vs audit prototype 0.97x):** Unfused `(assoc (dissoc m5 :abs) :k99 i)` runs through both top-level `set!` wrappers on `jolt-dissoc2` (`host-table.ss` -> `records-coll.ss` -> `collections.ss`), scans `root` once for `:abs`, then calls `jolt-assoc3` -> `jolt-assoc1` -> `pmap-assoc` (scanning `root` a second time for `:k99`) plus `meta-carry`. `jolt-dissoc1-assoc1` checks `pmap?`/`hnode?`/`amap-stable-key?` once, scans `root` once for both keys, calls `vec-append2`, and copies `(pmap-meta m)` directly into `%mk-pmap`.
- **Shared `amap-edit-pair` macro vs separate helper bodies (interleaved Pre1/B/Pre2 on `53dc7e4ac` vs patched):** Geometric mean across all 19 rows is **0.989x** (~1.1% faster overall, `-40` lines in `host/chez/collections.ss`). Every `assoc`->`dissoc` fast-path row sped up (`ad-m5-present` `26.35 -> 24.50` ns / `0.930x`, `ad-m5-absent` `20.94 -> 19.54` ns / `0.933x`, `ad-m5-present-upd` `27.36 -> 25.86` ns / `0.945x`, `ad-m12-present` `47.24 -> 42.97` ns / `0.910x`, reaching parity with `da-m12-present` `42.68` ns) because `amap-edit-pair` moves `(eq? ak dk)` out of the leading post-scan branch and aligns the scan order with `jolt-dissoc1-assoc1`.
- **`da-m5-absent-noop` (`10.21` pre -> `10.84` post, `+0.63` ns / `+6.2%`; 3 extra interleaved rounds: medians `10.32` pre vs `10.99` post, `+0.67` ns / `+6.5%`):** Verified directly in Chez `expand/optimize` (`optimize-level 2`): on `di < 0, ai >= 0, root[ai+1] eq? av`, the old `jolt-dissoc1-assoc1` took 3 tests (`(fx<? di 0)` -> `(fx>=? ai 0)` -> `(eq? val av)`), whereas `amap-edit-pair` checks `(fx>=? ai 0)` -> `(not (fx=? ai di))` -> `(fx>=? di 0)` -> `(eq? val av)` (one extra `fx=?` compare+branch on a 10 ns zero-allocation path, still **2.52x faster** than parent `e0f6096ce`). `da-m5-absent` moved `+0.53` ns (`+2.7%`, medians `19.35` pre vs `19.88` post) from the `(fx>=? di 0)` test in the `amap-keep?` count argument, still **2.00x faster** than parent.
- **Miss-path controls:** Re-measuring `ctrl-da-nonstable` across 3 extra interleaved rounds gave medians `59.55` pre vs `59.34` post (`0.996x`; the guard check before fallback is identical in both versions). Every miss-path control stays at `0.99x–1.05x` (or `~0.97x` on `ctrl-da-nonstable` rerun vs `57.58` parent, `+1.76` ns guard).

### Allocation checks (compiled Clojure fns, `test/chez/arraymap-test.ss`)

- 24-keyword array map:
  - `(-> m (dissoc :k5) (assoc :k99 1))`: **464 vs 928 bytes**
  - `(-> m (dissoc :k5) (assoc :k12 999))`: **448 vs 880 bytes**
  - `(-> m (assoc :k99 1) (dissoc :k5))`: **464 vs 960 bytes**
  - `(-> m (assoc :k12 999) (dissoc :k5))`: **448 vs 896 bytes**
  - No-op pairs (`(-> m (dissoc :abs) (assoc :k12 12))` and `(-> m (assoc :k12 12) (dissoc :abs))`): **16 / 16 bytes (base 16, 0 extra bytes)**.

### Benchmark source (`bench46.clj`, with `deps.edn` `{:paths ["."]}`)

```clojure
(ns bench46)

(defrecord Rec [a b c])

;; Stand-in for Potemkin NettyResponse (HTTP/1 receiver; Potemkin is not in
;; the local Maven cache). Implements IPersistentMap/ILookup/Counted/Seqable
;; so it misses pmap? and exercises the fallback path.
(deftype NettyResponseStandIn [m]
  clojure.lang.IPersistentMap
  (without [_ k] (NettyResponseStandIn. (dissoc m k)))
  (assoc [_ k v] (NettyResponseStandIn. (assoc m k v)))
  (assocEx [_ k v] (NettyResponseStandIn. (assoc m k v)))
  clojure.lang.ILookup
  (valAt [_ k] (get m k))
  (valAt [_ k d] (get m k d))
  clojure.lang.Counted
  (count [_] (count m))
  clojure.lang.Seqable
  (seq [_] (seq m)))

(defn da-present [m i] (-> m (dissoc :k2) (assoc :k99 i)))
(defn da-absent  [m i] (-> m (dissoc :abs) (assoc :k99 i)))
(defn da-upd     [m i] (-> m (dissoc :k2) (assoc :k4 i)))
(defn da-noop    [m _] (-> m (dissoc :abs) (assoc :k2 2)))
(defn da-same    [m i] (-> m (dissoc :k0) (assoc :k0 i)))
(defn da-m12     [m i] (-> m (dissoc :k5) (assoc :k99 i)))
(defn da-aleph   [rsp end start]
  (-> rsp
      (dissoc :aleph/destroy-conn?)
      (assoc :connection-time (- end start))))
(defn ad-present [m i] (-> m (assoc :k99 i) (dissoc :k2)))
(defn ad-absent  [m i] (-> m (assoc :k99 i) (dissoc :abs)))
(defn ad-upd     [m i] (-> m (assoc :k4 i) (dissoc :k2)))
(defn ad-m12     [m i] (-> m (assoc :k99 i) (dissoc :k5)))
(defn da-rec-ext [r i] (-> r (dissoc :x) (assoc :y i)))
(defn ad-rec-ext [r i] (-> r (assoc :y i) (dissoc :x)))
(defn da-rec-fld [r i] (-> r (dissoc :a) (assoc :y i)))
(defn da-nonst   [m i] (-> m (dissoc "s2") (assoc :k99 i)))
(defn da-nil     [m i] (-> m (dissoc :a) (assoc :b i)))

(defn- assert-class [label obj expected]
  (let [actual (.getName ^Class (class obj))]
    (when (not= actual expected)
      (throw (ex-info (str "Unexpected class for " label ": " actual " != " expected) {})))))

(defn- median [xs]
  (let [s (vec (sort xs))]
    (nth s (quot (count s) 2))))

(defn- run-row [label n f]
  (dotimes [_ 3] (f (quot n 10)))
  (let [samples
        (mapv (fn [_]
                (System/gc)
                (let [t0 (System/nanoTime)
                      acc (f n)
                      dt (- (System/nanoTime) t0)]
                  [(/ (double dt) (double n)) acc]))
              (range 7))
        med (median (mapv first samples))]
    (println (format "  %-20s %7.2f ns/op  (acc=%s)" label med (second (first samples))))))

(defn -main [& args]
  (let [n (if (seq args) (Integer/parseInt (first args)) 2000000)
        m5 {:k0 0 :k1 1 :k2 2 :k3 3 :k4 4}
        m12 (reduce (fn [m i] (assoc m (keyword (str "k" i)) i)) {} (range 12))
        hm24 (apply hash-map (mapcat (fn [i] [(keyword (str "k" i)) i]) (range 24)))
        rec (assoc (->Rec 1 2 3) :x 10)
        m5-str {:k0 0 :k1 1 "s2" 2 :k3 3 :k4 4}
        rsp-http2 {:status 200
                   :headers {"content-type" "application/json"}
                   :aleph/keep-alive? true
                   :aleph/destroy-conn? false
                   :body :stream}
        rsp-http1 (NettyResponseStandIn. rsp-http2)]
    (assert-class "m5" m5 "clojure.lang.PersistentArrayMap")
    (assert-class "m12" m12 "clojure.lang.PersistentArrayMap")
    (assert-class "hm24" hm24 "clojure.lang.PersistentHashMap")
    (assert-class "rec" rec "bench46.Rec")
    (assert-class "m5-str" m5-str "clojure.lang.PersistentArrayMap")
    (assert-class "rsp-http2" rsp-http2 "clojure.lang.PersistentArrayMap")
    (assert-class "rsp-http1" rsp-http1 "bench46.NettyResponseStandIn")
    (run-row "da-m5-present" n
      (fn [k] (loop [i 0 acc 0] (if (< i k) (recur (inc i) (+ acc (count (da-present m5 i)))) acc))))
    (run-row "da-m5-absent" n
      (fn [k] (loop [i 0 acc 0] (if (< i k) (recur (inc i) (+ acc (count (da-absent m5 i)))) acc))))
    (run-row "da-m5-present-upd" n
      (fn [k] (loop [i 0 acc 0] (if (< i k) (recur (inc i) (+ acc (count (da-upd m5 i)))) acc))))
    (run-row "da-m5-absent-noop" n
      (fn [k] (loop [i 0 acc 0] (if (< i k) (recur (inc i) (+ acc (count (da-noop m5 i)))) acc))))
    (run-row "da-m5-same-key" n
      (fn [k] (loop [i 0 acc 0] (if (< i k) (recur (inc i) (+ acc (count (da-same m5 i)))) acc))))
    (run-row "da-m12-present" n
      (fn [k] (loop [i 0 acc 0] (if (< i k) (recur (inc i) (+ acc (count (da-m12 m12 i)))) acc))))
    (run-row "da-aleph-http2" n
      (fn [k] (loop [i 0 acc 0] (if (< i k) (recur (inc i) (+ acc (count (da-aleph rsp-http2 (+ i 100) i)))) acc))))
    (run-row "da-aleph-http1" n
      (fn [k] (loop [i 0 acc 0] (if (< i k) (recur (inc i) (+ acc (count (da-aleph rsp-http1 (+ i 100) i)))) acc))))
    (run-row "ad-m5-present" n
      (fn [k] (loop [i 0 acc 0] (if (< i k) (recur (inc i) (+ acc (count (ad-present m5 i)))) acc))))
    (run-row "ad-m5-absent" n
      (fn [k] (loop [i 0 acc 0] (if (< i k) (recur (inc i) (+ acc (count (ad-absent m5 i)))) acc))))
    (run-row "ad-m5-present-upd" n
      (fn [k] (loop [i 0 acc 0] (if (< i k) (recur (inc i) (+ acc (count (ad-upd m5 i)))) acc))))
    (run-row "ad-m12-present" n
      (fn [k] (loop [i 0 acc 0] (if (< i k) (recur (inc i) (+ acc (count (ad-m12 m12 i)))) acc))))
    (run-row "ctrl-da-hash24" n
      (fn [k] (loop [i 0 acc 0] (if (< i k) (recur (inc i) (+ acc (count (da-m12 hm24 i)))) acc))))
    (run-row "ctrl-ad-hash24" n
      (fn [k] (loop [i 0 acc 0] (if (< i k) (recur (inc i) (+ acc (count (ad-m12 hm24 i)))) acc))))
    (run-row "ctrl-da-rec-ext" n
      (fn [k] (loop [i 0 acc 0] (if (< i k) (recur (inc i) (+ acc (count (da-rec-ext rec i)))) acc))))
    (run-row "ctrl-ad-rec-ext" n
      (fn [k] (loop [i 0 acc 0] (if (< i k) (recur (inc i) (+ acc (count (ad-rec-ext rec i)))) acc))))
    (run-row "ctrl-da-rec-field" n
      (fn [k] (loop [i 0 acc 0] (if (< i k) (recur (inc i) (+ acc (count (da-rec-fld rec i)))) acc))))
    (run-row "ctrl-da-nonstable" n
      (fn [k] (loop [i 0 acc 0] (if (< i k) (recur (inc i) (+ acc (count (da-nonst m5-str i)))) acc))))
    (run-row "ctrl-da-nil" n
      (fn [k] (loop [i 0 acc 0] (if (< i k) (recur (inc i) (+ acc (count (da-nil nil i)))) acc))))))
```

### Gate status

- **Reminted worktree (filtered full `SHARD-POOL`, 205 of 208 targets):** excluded the three pre-existing host environmental failures (`certify` [requires JDK 21], `lazyretain` [requires GNU `timeout`], and `filelock` [pre-existing `starved after 10000 cycles` failure]). Ran `200` parallel targets (`-j8`) + `devbootsmoke`/`gatebootsmoke` (`-j1`) + `fastpathratio`/`stdlibratio`/`pmoverhead` (`-j1`) — all **205/205** targets exited 0, including `selfhost`, `gambitseedcheck`, `gambitgencheck`, `gambittwins`, `emittedheads` (293 heads, 0 shadowable), `inline` (423/423, including 2 new `assoc`-chain negative regression guards + 45 new `jolt-46` positive/negative/precedence/eval-order/`^:once`/shadowing/`types.clj`-demotion/equivalence tests), `irvalidate`, `arraymap` (130/130, including 5,726-case exhaustive small matrix, 1,500-trial randomized differential tests for both helpers, promotion boundaries, `dk=ak` move-to-end, emptying fresh empty, fallback coverage across `nil`/hash-map/vector/`defrecord`/`sorted-map`/custom `deftype` with metadata, 2 present non-stable written-key in-place fallback assertions, and compiled-fn allocation assertions), `recfast` (518/518), `recfold` (13/13), `oparity` (304/304), `cts`, `unit`, `corpus`, `footprint`, `vfaslceiling` (25/25), `portcheck`, `deadhost`, `recordshadow`, and `attributioncheck`.
- **Mutant-checked tests:** Parent commit `e0f6096ce` fails all 14 new emitted-head assertions in `test/chez/inline-test.ss` (`409/423`); M1 (drop `(not (fx=? ai di))` in `jolt-dissoc1-assoc1`) fails 5 tests in `test/chez/arraymap-test.ss` (`123/128`); M2 (check promotion against `(fx- cnt 1)` instead of `cnt` in `jolt-assoc1-dissoc1`) fails 3 tests (`125/128`); M3 (replace `(amap-stable-key? ak)` with `#t`) fails the matching present non-stable written-key assertion individually before the macro patch (`129/130` for each helper) and both assertions together in `amap-edit-pair` (`128/130`).
- **Committed tree (seeds restored per the seed rule):** `selfhost`, `gambitseedcheck`, `inline` (30 emitted-head/precedence assertions, `393/423`, covering `jolt-41` and `jolt-46`), and `arraymap` (4 compiled-fn single-allocation assertions, `126/130`; all 126 runtime helper checks pass) fail strictly for seed staleness until `make remint` (or `jolt-45`); `deadhost`, `portcheck`, `recordshadow`, `emittedheads`, and `attributioncheck` pass on the committed tree.
- **Gambit did not run:** `gambitboot`, `gambitcheck`, `gambiteval`, `gambitkernel`, `gambitprofile`, `gambitstatics`, `gambitunbound`, and `gambitvars` skipped because Gambit is not installed on this machine.

## jolt-43

Emit-time peephole (`emit-assoc-update-chain`) in `jolt-core/jolt/backend_scheme/fused.clj` (with `pmap?` registered in `rt-emitted-names` in `jolt-core/jolt/backend_scheme/names.clj`) collapsing straight-line 2..4-pair chains mixing `assoc` and `update` on distinct literal keyword keys into a single guarded multi-pair `(jolt-assoc m k1 v1 ...)` call (reaching `jolt-39`'s fixed `case-lambda` clauses). Chez direct-link builds only (`chez-linked-fast-path?` + `seed-callable?` on `clojure.core/update`; plain/non-direct-link and Gambit builds emit ordinary calls).

- **Parent commit measured against:** `60de4931e` (`perf/coll-edit-fusion`)
- **Date:** 2026-11-24
- **Machine:** Apple M3 Ultra (Virtual), macOS 15.7.3
- **Chez:** 10.4.1 (`JOLT_CHEZ=/Users/admin/src/jolt/.cache/local/chezscheme-10.4.1/bin/scheme`)
- **Build flags:** `bin/jolt build -m <ns> -o <out> --direct-link --opt`
- **Nursery pin:** `JOLT_GC_TRIP_BYTES=16m` (isolates adaptive nursery doubling investigated in `jolt-44`)
- **Precedence among the three rules in `fused.clj`:**
  1. `emit-assoc-update-chain` (mixed chains containing `>=1` `update`, 2..4 distinct literal keyword pairs, Chez direct-link only)
  2. `emit-assoc-chain` (pure `assoc` chains, 2..4 pairs, all modes)
  3. `emit-dissoc-assoc-pair` (single `dissoc`/`assoc` pairs, all modes; defers to `emit-assoc-chain` and eligible `emit-assoc-update-chain`)
  Rejected mixed chains suppress partial mixed tiling via `*unfused-update-nodes*` while still allowing pure `assoc` chains or `dissoc`/`assoc` pairs on sub-expressions.
- **Semantics decisions relied on (`coll-edit-fusion-decisions.md`):** Decision 1 (`pure-edit-operand?` / `pure-call-head?` without `:clear-caps` for later `assoc` values and extra `update` arguments), Decision 2 (one `clojure.core/update` root check at chain entry via `jolt-loop-map-update?`, honouring `with-redefs` and `alter-var-root` around the call; does not notice an updater redefining `update` mid-chain), and Decision 3 (no identity guarantee under raw Scheme `call/cc` re-entry). Smaller relaxations: all-no-op chains return the `identical?` receiver; a throw from the final `jolt-assoc` may report the outermost call's line; fallback runs the original unfused calls unchanged.
- **Seed mint impact:** `make remint` converged in 3 passes (0 forms skipped). **0** call sites fused in `host/chez/seed/prelude.ss` and **0** in `host/chez/seed/image.ss` (gated on `(not (seed-mint?))` via `chez-linked-fast-path?`). Per the seed rule, seed files were restored before committing (`git checkout -- host/chez/seed host/gambit/seed`).
- **Gambit:** not installed on this machine; `portcheck`, `gambitseedcheck`, `gambitgencheck`, and `gambittwins` passed on the reminted tree; Gambit-runtime gates (`gambitboot`, `gambitcheck`, `gambiteval`, `gambitkernel`, `gambitprofile`, `gambitstatics`, `gambitunbound`, `gambitvars`) did not run.

### Go/no-go measurement on base tip (`60de4931e`)

Before implementing, measured the two target shapes on reminted `60de4931e` (`bin/jolt build -m gonogo --direct-link --opt`, `JOLT_GC_TRIP_BYTES=16m`, 7 runs × 2,000,000 ops after 3 warmup runs, median ns/op) against hand-written Clojure equivalents using multi-arg `assoc` (reaching `jolt-39`'s fast multi-pair clauses):

| Shape | Original `->` (ns/op) | Hand-written multi-`assoc` (ns/op) | Ratio | Notes |
|---|---:|---:|---:|---|
| `athens` (4-key real state) | 53.48 | **23.30** | **2.30x** | `(-> state (assoc :last-op op) (update :op-count inc))` |
| `reframe-dec` (5-key real state) | 182.42 | **136.51** | **1.34x** | `decrement-pending-child` taking `assoc` branch (`n > 1`) |
| `reframe-dissoc` (5-key real state) | 188.79 | **135.39** | **1.39x** | `decrement-pending-child` taking `dissoc` branch (`n = 1`) |
| `reframe-nilparent` (5-key real state) | 146.32 | **102.05** | **1.43x** | `parent-id` = `nil` |
| `reframe-3conj` (5-key state) | 201.04 | **161.62** | **1.24x** | 3 `conj` updates (`:seen-ids`, `:cascade-epochs`, `:cascade-ids`) |

**Decision: GO** — both target shapes clear the 1.15x bar by a wide margin (`2.30x` on `athens`, `1.24x–1.43x` on `re-frame`).

### Corpus recount (50 repos, 15,551 `.clj`/`.cljc`/`.cljs` files, `/tmp/jolt-opt-corpus-20261123`)

Scanned all 15,551 files with a standalone out-of-tree scanner matching maximal contiguous `assoc`/`update` runs inside `->` containing `>=1` `update`, applying the shipped key and operand rules (`op-registry/pure-ops` + keyword lookups; unqualified non-earmuffed symbol local approximation, matching `jolt-41`'s `unq` column):

| Category | Count | Breakdown / Notes |
|---|---:|---|
| Syntactic `->` mixed `assoc`/`update` runs | 810 | |
| Excluded: `>4` pairs / non-keyword key / duplicate key | 143 | 59 `>4` pairs, 36 non-keyword key, 48 duplicate key |
| Distinct literal keyword 2..4 pairs | 667 | |
| **Eligible (shipped operand rule, Decision 1)** | **420** | **316** 2-pair, **72** 3-pair, **32** 4-pair (283 update-only, 68 `assoc`+`update`, 40 `update`+`assoc`, 29 mixed 3..4-step) |
| Eligible (strict local+const only) | 387 | Decision 1 adds +33 sites (+8.5%) |

### Emitted Scheme (before and after), call-site bytes, and compile time

Real corpus sites (`athens/src/cljc/event_sync/core.cljc:19-24` and `re-frame/src/re_frame/router.cljc:483-490`, with `decrement-pending-child` direct-linked):

```clojure
;; athens/event_sync/core.cljc:19-24
(defn update-op [state op]
  (-> state
      (assoc :last-op op)
      (update :op-count inc)))

;; re-frame/router.cljc:487-490
(defn accept-trace-epoch [state id parent-id epoch]
  (-> state
      (update :seen-ids conj id)
      (update :pending-children decrement-pending-child parent-id)
      (update :cascade-epochs conj epoch)))
```

```scheme
;; athens BEFORE (211 bytes at call expr, 940 bytes full def):
(let* ((_a$32 (jolt-assoc3 state _kc$30 op)) (_a$33 _kc$31) (_a$34 jolt-inc))
  (let ((_lp$35 jv$clojure.core/update))
    (if (#3%procedure? _lp$35) (_lp$35 _a$32 _a$33 _a$34) (jolt-invoke _lp$35 _a$32 _a$33 _a$34))))

;; athens AFTER (382 bytes at call expr, +171 bytes [98B guard + 67B fast + 211B fallback + 6B if]; 1,111 bytes full def):
(if (and (or (pmap? state) (eq? state jolt-nil)) (jolt-loop-map-update? jv$clojure.core/update))
    (jolt-assoc state _kc$35 op _kc$36 (jolt-n-inc (jolt-get state _kc$36)))
    (let* ((_a$37 (jolt-assoc3 state _kc$35 op)) (_a$38 _kc$36) (_a$39 jolt-inc))
      (let ((_lp$40 jv$clojure.core/update))
        (if (#3%procedure? _lp$40) (_lp$40 _a$37 _a$38 _a$39) (jolt-invoke _lp$40 _a$37 _a$38 _a$39)))))

;; re-frame BEFORE (594 bytes at call expr, 1,560 bytes full def)
;; re-frame AFTER (1,041 bytes at call expr, +447 bytes [98B guard + 343B fast + 594B fallback + 6B if]; 2,007 bytes full def):
(if (and (or (pmap? state) (eq? state jolt-nil)) (jolt-loop-map-update? jv$clojure.core/update))
    (let* ((_mu$10 (jolt-conj2 (jolt-get state _kc$9) id))
           (_mu$14 (let ((_lp$12 jv$u/decrement-pending-child) (_la$13 (jolt-get state _kc$11)))
                     (if (#3%procedure? _lp$12) (_lp$12 _la$13 parent-id) (jolt-invoke _lp$12 _la$13 parent-id))))
           (_mu$16 (jolt-conj2 (jolt-get state _kc$15) epoch)))
      (jolt-assoc state _kc$9 _mu$10 _kc$11 _mu$14 _kc$15 _mu$16))
    <unfused 3-step update chain>)
```

- **Compile time (`bench43`, `bin/jolt build --direct-link --opt`):** parent **21.34 s** vs candidate **21.15 s** (`JOLT_BUILD_PROFILE=1`: `emit app namespaces` 107 ms vs 109 ms, `emit: emit-top` 8 ms vs 11 ms, cumulative 168 ms vs 173 ms — no measurable compile-time overhead).

### Commands

```sh
# Go/no-go on base tip (60de4931e, reminted):
(cd /tmp/jolt-43-gonogo && JOLT_PWD="$PWD" "$REPO/bin/jolt" build -m gonogo -o /tmp/jolt-43-gonogo/gonogo --direct-link --opt)
JOLT_GC_TRIP_BYTES=16m /tmp/jolt-43-gonogo/gonogo 2000000

# Before/after AOT benchmarks (detached worktree at 60de4931e, reminted, vs reminted candidate):
(cd /tmp/jolt-43-bench && JOLT_PWD="$PWD" /tmp/jolt-43-parent/bin/jolt build -m bench43 -o /tmp/jolt-43-bench/bench43-base --direct-link --opt)
(cd /tmp/jolt-43-bench && JOLT_PWD="$PWD" "$REPO/bin/jolt" build -m bench43 -o /tmp/jolt-43-bench/bench43-cand --direct-link --opt)
JOLT_GC_TRIP_BYTES=16m /tmp/jolt-43-bench/bench43-base 2000000
JOLT_GC_TRIP_BYTES=16m /tmp/jolt-43-bench/bench43-cand 2000000
JOLT_GC_TRIP_BYTES=16m /tmp/jolt-43-bench/bench43-base 2000000
```

### Benchmark receiver construction and asserted representations

Every receiver in `bench43` asserts its runtime class at startup via `(.getName ^Class (class obj))`:

- `athens-state`: `{:id :q1 :stages {:s1 {} :s2 {}} :last-op :initialization :op-count 0}` — `clojure.lang.PersistentArrayMap` (4 outer keys, matches `athens/event_sync/core.cljc:69-72`)
- `rf-state-dec`, `rf-state-dissoc`, `rf-state-nil`: 5-key map literals matching `re-frame/router.cljc:470-474` (`:cascade-ids`, `:seen-ids`, `:cascade-epochs`, `:pending-children`, `:unmatched-epochs`) — `clojure.lang.PersistentArrayMap`
- `m4`: `{:k0 0 :k1 1 :k2 2 :k3 3}` — `clojure.lang.PersistentArrayMap` (4 keyword keys, literal)
- `m12`: `(reduce (fn [m i] (assoc m (keyword (str "k" i)) i)) {} (range 12))` — `clojure.lang.PersistentArrayMap` (12 keyword keys, array-mode)
- `hm24`: `(apply hash-map (mapcat (fn [i] [(keyword (str "k" i)) i]) (range 24)))` — `clojure.lang.PersistentHashMap` (24 keyword keys, hash-mode control)
- `rec`: `(assoc (->Rec 1 2 3) :x 10 :y 20)` from `(defrecord Rec [a b c])` — `bench43.Rec` (record miss control, declared-field and ext-map rows)
- `sm`: `(sorted-map :k0 0 :k1 1 :k2 2 :k3 3)` — `clojure.lang.PersistentTreeMap` (sorted-map miss control)
- `ca`: `(CustomAssoc. m4)` — `bench43.CustomAssoc` (`deftype` implementing `Associative`/`ILookup`/`Counted`, custom miss control)
- Tail-full vector control: omitted from timed rows because keyword-keyed `assoc`/`update` throws `IllegalArgumentException` on vectors (covered in `test/chez/inline-test.ss`).

### Ad hoc AOT results (median ns/op of 7 runs × 2,000,000 ops, `JOLT_GC_TRIP_BYTES=16m`, A1 -> B -> A2)

| Row | Parent A1 (ns/op) | Candidate B (ns/op) | Parent A2 (ns/op) | Speedup (avg A / B) | Notes |
|---|---:|---:|---:|---:|---|
| `athens-4key` | 52.96 | **23.26** | 52.66 | **2.27x** | `athens/event_sync/core.cljc:22` on 4-key real state |
| `reframe-5key-dec` | 177.86 | **135.88** | 179.96 | **1.32x** | `re-frame/router.cljc:487` (`decrement-pending-child` `assoc` branch) |
| `reframe-5key-dissoc` | 190.53 | **136.01** | 189.15 | **1.40x** | `re-frame/router.cljc:487` (`decrement-pending-child` `dissoc` branch) |
| `reframe-5key-nilp` | 140.45 | **101.41** | 142.35 | **1.39x** | `re-frame/router.cljc:487` (`parent-id` = `nil`) |
| `ua-m4` | 48.98 | **22.23** | 50.90 | **2.25x** | 2-step `update`+`assoc` on 4-key array map |
| `uu-m4` | 60.73 | **27.39** | 59.27 | **2.19x** | 2-step `update`+`update` on 4-key array map |
| `mixed4-m4` | 102.40 | **34.01** | 98.12 | **2.95x** | 4-step `assoc`+`update`+`assoc`+`update` on 4-key array map |
| `mixed4-m12` | 122.01 | **49.20** | 118.78 | **2.45x** | 4-step mixed chain on 12-keyword array map |
| `mixed2-nil` | 125.27 | **90.92** | 124.32 | **1.37x** | 2-step `assoc`+`update` (`inc0 = (fnil inc 0)`) on `nil` |
| `ctrl-hash24-ua` | 111.44 | **107.08** | 113.87 | **1.05x** | hash-mode 24-key map (takes fast lane, saves `update` dispatch) |
| `ctrl-hash24-mixed4` | 252.87 | **242.18** | 251.28 | **1.04x** | hash-mode 24-key map, 4-step mixed |
| `ctrl-rec-field` | 79.82 | 79.38 | 79.17 | 1.00x | miss control: `defrecord` declared fields (misses `pmap?` inline) |
| `ctrl-rec-ext` | 125.67 | 125.93 | 124.91 | 0.99x | miss control: `defrecord` ext keys (misses `pmap?` inline) |
| `ctrl-sorted-ua` | 355.43 | 356.09 | 353.27 | 0.99x | miss control: `sorted-map` (misses `pmap?` inline) |
| `ctrl-custom-ua` | 190.29 | 191.26 | 190.81 | 1.00x | miss control: custom `Associative` `deftype` (misses `pmap?` inline) |

### Allocation checks (compiled Clojure fns, `bytes-allocated` over 1,000 iterations)

- `athens-4key`: **256 -> 128 bytes/op** (2.0x less allocation: 1 outer 4-key array map instead of 2)
- `reframe-5key-dec`: **800 -> 512 bytes/op** (-288 bytes/op: eliminates 2 intermediate 5-key outer maps, 144 bytes each)
- `mixed4-m4`: **512 -> 128 bytes/op** (4.0x less allocation: 1 outer 4-key array map instead of 4)
- `noop-m4` (`(-> m4 (assoc :k0 0) (update :k1 identity))`): **0 -> 0 bytes/op** (`identical?` receiver)

### Benchmark source (`gonogo.clj` and `bench43.clj`, with `deps.edn` `{:paths ["."]}`)

```clojure
;; gonogo.clj (go/no-go harness run on base tip 60de4931e)
(ns gonogo)

(defn athens-orig [state op]
  (-> state (assoc :last-op op) (update :op-count inc)))

(defn athens-hand [state op]
  (let [v1 op, v2 (inc (get state :op-count))]
    (assoc state :last-op v1 :op-count v2)))

(defn decrement-pending-child [pending parent-id]
  (if parent-id
    (let [n (get pending parent-id 0)]
      (cond (> n 1) (assoc pending parent-id (dec n))
            (= n 1) (dissoc pending parent-id)
            :else pending))
    pending))

(defn reframe-orig [state id parent-id epoch]
  (-> state
      (update :seen-ids conj id)
      (update :pending-children decrement-pending-child parent-id)
      (update :cascade-epochs conj epoch)))

(defn reframe-hand [state id parent-id epoch]
  (let [v1 (conj (get state :seen-ids) id)
        v2 (decrement-pending-child (get state :pending-children) parent-id)
        v3 (conj (get state :cascade-epochs) epoch)]
    (assoc state :seen-ids v1 :pending-children v2 :cascade-epochs v3)))

(defn reframe-3conj-orig [state id epoch]
  (-> state
      (update :seen-ids conj id)
      (update :cascade-epochs conj epoch)
      (update :cascade-ids conj id)))

(defn reframe-3conj-hand [state id epoch]
  (let [v1 (conj (get state :seen-ids) id)
        v2 (conj (get state :cascade-epochs) epoch)
        v3 (conj (get state :cascade-ids) id)]
    (assoc state :seen-ids v1 :cascade-epochs v2 :cascade-ids v3)))
```

```clojure
;; bench43.clj (A1/B/A2 benchmark harness)
(ns bench43)

(defrecord Rec [a b c])

(deftype CustomAssoc [m]
  clojure.lang.Associative
  (containsKey [_ k] (contains? m k))
  (entryAt [_ k] (find m k))
  (assoc [_ k v] (CustomAssoc. (assoc m k v)))
  clojure.lang.ILookup
  (valAt [_ k] (get m k))
  (valAt [_ k d] (get m k d))
  clojure.lang.Counted
  (count [_] (count m)))

(defn athens-update-op [state op]
  (-> state
      (assoc :last-op op)
      (update :op-count inc)))

(defn decrement-pending-child [pending parent-id]
  (if parent-id
    (let [n (get pending parent-id 0)]
      (cond
        (> n 1) (assoc pending parent-id (dec n))
        (= n 1) (dissoc pending parent-id)
        :else pending))
    pending))

(defn reframe-accept [state id parent-id epoch]
  (-> state
      (update :seen-ids conj id)
      (update :pending-children decrement-pending-child parent-id)
      (update :cascade-epochs conj epoch)))

(defn update-assoc-2 [m x] (-> m (update :k0 inc) (assoc :k1 x)))
(defn update-update-2 [m] (-> m (update :k0 inc) (update :k1 inc)))
(defn mixed-4 [m x y] (-> m (assoc :k0 x) (update :k1 inc) (assoc :k2 y) (update :k3 inc)))
(def inc0 (fnil inc 0))
(defn mixed-nil-2 [m x] (-> m (assoc :a x) (update :b inc0)))
(defn mixed-rec-field [r x] (-> r (assoc :a x) (update :b inc)))
(defn mixed-rec-ext [r x] (-> r (assoc :x x) (update :y inc)))

(defn- assert-class [label obj expected]
  (let [actual (.getName ^Class (class obj))]
    (when (not= actual expected)
      (throw (ex-info (str "Unexpected class for " label ": " actual " != " expected) {})))))

(defn- median [xs]
  (let [s (vec (sort xs))]
    (nth s (quot (count s) 2))))

(defn- run-row [label n f]
  (dotimes [_ 3] (f (quot n 10)))
  (let [samples
        (mapv (fn [_]
                (System/gc)
                (let [t0 (System/nanoTime)
                      acc (f n)
                      dt (- (System/nanoTime) t0)]
                  [(/ (double dt) (double n)) acc]))
              (range 7))
        med (median (mapv first samples))]
    (println (format "  %-24s %7.2f ns/op  (acc=%s)" label med (second (first samples))))))

(defn -main [& args]
  (let [n (if (seq args) (Integer/parseInt (first args)) 2000000)
        athens-state {:id :q1 :stages {:s1 {} :s2 {}} :last-op :initialization :op-count 0}
        op [:add :s1 :e1 {:payload 1} false]
        rf-state-dec {:cascade-ids #{:root} :seen-ids #{:root}
                      :cascade-epochs [{:dispatch-id :root}]
                      :pending-children {:root 2 :p2 1} :unmatched-epochs []}
        rf-state-dissoc {:cascade-ids #{:root} :seen-ids #{:root}
                         :cascade-epochs [{:dispatch-id :root}]
                         :pending-children {:root 1 :p2 2} :unmatched-epochs []}
        rf-state-nil {:cascade-ids #{} :seen-ids #{} :cascade-epochs []
                      :pending-children {} :unmatched-epochs []}
        epoch {:dispatch-id :e2 :parent-dispatch-id :root :effects []}
        m4 {:k0 0 :k1 1 :k2 2 :k3 3}
        m12 (reduce (fn [m i] (assoc m (keyword (str "k" i)) i)) {} (range 12))
        hm24 (apply hash-map (mapcat (fn [i] [(keyword (str "k" i)) i]) (range 24)))
        rec (assoc (->Rec 1 2 3) :x 10 :y 20)
        sm (sorted-map :k0 0 :k1 1 :k2 2 :k3 3)
        ca (CustomAssoc. m4)]
    (assert-class "athens-state" athens-state "clojure.lang.PersistentArrayMap")
    (assert-class "rf-state-dec" rf-state-dec "clojure.lang.PersistentArrayMap")
    (assert-class "rf-state-dissoc" rf-state-dissoc "clojure.lang.PersistentArrayMap")
    (assert-class "rf-state-nil" rf-state-nil "clojure.lang.PersistentArrayMap")
    (assert-class "m4" m4 "clojure.lang.PersistentArrayMap")
    (assert-class "m12" m12 "clojure.lang.PersistentArrayMap")
    (assert-class "hm24" hm24 "clojure.lang.PersistentHashMap")
    (assert-class "rec" rec "bench43.Rec")
    (assert-class "sm" sm "clojure.lang.PersistentTreeMap")
    (assert-class "ca" ca "bench43.CustomAssoc")
    (run-row "athens-4key" n
      (fn [k] (loop [i 0 acc 0] (if (< i k) (recur (inc i) (+ acc (:op-count (athens-update-op athens-state op)))) acc))))
    (run-row "reframe-5key-dec" n
      (fn [k] (loop [i 0 acc 0] (if (< i k) (recur (inc i) (+ acc (count (:cascade-epochs (reframe-accept rf-state-dec :e2 :root epoch))))) acc))))
    (run-row "reframe-5key-dissoc" n
      (fn [k] (loop [i 0 acc 0] (if (< i k) (recur (inc i) (+ acc (count (:cascade-epochs (reframe-accept rf-state-dissoc :e2 :root epoch))))) acc))))
    (run-row "reframe-5key-nilp" n
      (fn [k] (loop [i 0 acc 0] (if (< i k) (recur (inc i) (+ acc (count (:cascade-epochs (reframe-accept rf-state-nil :e2 nil epoch))))) acc))))
    (run-row "ua-m4" n
      (fn [k] (loop [i 0 acc 0] (if (< i k) (recur (inc i) (+ acc (:k0 (update-assoc-2 m4 i)))) acc))))
    (run-row "uu-m4" n
      (fn [k] (loop [i 0 acc 0] (if (< i k) (recur (inc i) (+ acc (:k1 (update-update-2 m4)))) acc))))
    (run-row "mixed4-m4" n
      (fn [k] (loop [i 0 acc 0] (if (< i k) (recur (inc i) (+ acc (:k3 (mixed-4 m4 i (+ i 1))))) acc))))
    (run-row "mixed4-m12" n
      (fn [k] (loop [i 0 acc 0] (if (< i k) (recur (inc i) (+ acc (:k3 (mixed-4 m12 i (+ i 1))))) acc))))
    (run-row "mixed2-nil" n
      (fn [k] (loop [i 0 acc 0] (if (< i k) (recur (inc i) (+ acc (:b (mixed-nil-2 nil i)))) acc))))
    (run-row "ctrl-hash24-ua" n
      (fn [k] (loop [i 0 acc 0] (if (< i k) (recur (inc i) (+ acc (:k0 (update-assoc-2 hm24 i)))) acc))))
    (run-row "ctrl-hash24-mixed4" n
      (fn [k] (loop [i 0 acc 0] (if (< i k) (recur (inc i) (+ acc (:k3 (mixed-4 hm24 i (+ i 1))))) acc))))
    (run-row "ctrl-rec-field" n
      (fn [k] (loop [i 0 acc 0] (if (< i k) (recur (inc i) (+ acc (:b (mixed-rec-field rec i)))) acc))))
    (run-row "ctrl-rec-ext" n
      (fn [k] (loop [i 0 acc 0] (if (< i k) (recur (inc i) (+ acc (:y (mixed-rec-ext rec i)))) acc))))
    (run-row "ctrl-sorted-ua" n
      (fn [k] (loop [i 0 acc 0] (if (< i k) (recur (inc i) (+ acc (:k0 (update-assoc-2 sm i)))) acc))))
    (run-row "ctrl-custom-ua" n
      (fn [k] (loop [i 0 acc 0] (if (< i k) (recur (inc i) (+ acc (:k0 (update-assoc-2 ca i)))) acc))))))
```

### Gate status

- **Reminted worktree (filtered full `SHARD-POOL`, 205 of 208 targets):** excluded the three pre-existing host environmental failures (`certify` [requires JDK 21], `lazyretain` [requires GNU `timeout`], and `filelock` [pre-existing `starved after 10000 cycles` failure]). Ran `200` parallel targets (`-j8`) + `devbootsmoke`/`gatebootsmoke` (`-j1`) + `fastpathratio`/`stdlibratio`/`pmoverhead` (`-j1`) — all **205/205** targets exited 0, including `selfhost`, `gambitseedcheck`, `gambitgencheck`, `gambittwins`, `emittedheads` (295 heads, 0 shadowable), `inline` (483/483, including 60 new `jolt-43` positive/non-direct-link/precedence/negative/`with-redefs`/`alter-var-root`/throwing/eval-order/`^:once`/shadowing/receiver-fallback/`loop-maps`/`types.clj`/400-trial randomized differential tests), `irvalidate`, `arraymap` (130/130), `recfast` (518/518), `recfold` (13/13), `oparity`, `directlink`, `cts` (6094 pass, matches baseline), `unit`, `corpus`, `footprint`, `vfaslceiling`, `portcheck`, `deadhost`, `recordshadow`, and `attributioncheck`.
- **Mutant-checked tests (`test/chez/inline-test.ss`):** Parent commit `60de4931e` fails all 20 new emitted-head/precedence/demotion-fusion assertions (`463/483`); M1 (drop duplicate-key check) fails 9 assertions (`474/483`); M2 (drop `jolt-loop-map-update?` root guard) fails 22 assertions (`461/483`, including both `with-redefs` and `alter-var-root` fallback tests); M3 (drop `(or (pmap? ...) (eq? ... jolt-nil))` receiver guard) fails emitted-head + eval-order tests and throws on `[1 2]`; M4 (drop `*unfused-update-nodes*` suppression) fails 3 no-tiling assertions (`480/483`); M5 (drop `pure-edit-operand?` on later steps) fails all 9 impure/`:clear-caps` negative assertions (`474/483`).
- **Committed tree (seeds restored per the seed rule):** `selfhost`, `gambitseedcheck`, `emittedheads` (`"pmap?"` absent from committed seed's `rt-emitted-names`), and `inline` (54 emitted-head/precedence assertions, `429/483`, covering `jolt-41`, `jolt-46`, and `jolt-43`) fail strictly for seed staleness until `make remint` (or `jolt-45`); `attributioncheck` passes on the committed tree.
- **Gambit did not run:** `gambitboot`, `gambitcheck`, `gambiteval`, `gambitkernel`, `gambitprofile`, `gambitstatics`, `gambitunbound`, and `gambitvars` skipped because Gambit is not installed on this machine.
