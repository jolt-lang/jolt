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
