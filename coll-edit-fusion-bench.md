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

Measured at 2 pairs with identical fast-path body under `JOLT_GC_TRIP_BYTES=16m` (1,000,000 iters, 7 runs, median ns/op):

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
| `assoc3-3-new` | 70.39 | **31.91** | 72.55 | **2.24x** | 3 pairs, all new on 3-key array map |
| `assoc3-3-exist` | 59.59 | **17.59** | 61.33 | **3.44x** | 3 pairs, all existing on 3-key array map |
| `assoc4-3-new` | 94.29 | **40.63** | 96.30 | **2.35x** | 4 pairs, all new on 3-key array map |
| `assoc4-3-exist` | 106.65 | **38.78** | 109.07 | **2.78x** | 4 pairs, all existing on 12-keyword array map |
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

### Design note on zero-`set!` 3- and 4-pair clauses

An initial 3/4-pair implementation using local `(let ((c cnt) ...) (set! ...))` boxed every assigned local under Chez's source-loaded test runner (144 bytes/call for 3-pair, 176 bytes/call for 4-pair on no-op batches). Rewriting those steps as pure `let*` bindings with an all-existing fast branch dropped no-op allocation to 0 extra bytes on all three arities (24-keyword array map bytes/op: 2p 544 vs 1,024; 3p 576 vs 1,552; 4p 608 vs 2,096; no-op 0/0/0).
