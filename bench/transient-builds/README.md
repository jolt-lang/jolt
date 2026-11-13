# Transient collection builds

The seed-42 collection benchmark from `gen-research` found 15 transient builds
where Jolt took longer than both Babashka and JVM Clojure. A release build of
`origin/main` at `ef8dd3e9` reproduced all 15. All 2,754 warmup and measured
batches passed the arithmetic checksum. [Original measurements](https://github.com/jolt-lang/jolt/blob/af74d25e599c649834edfb6c68a94c6a5160a1fd/benchmarks/generated-project/results/2026-11-10-seed42-collection-sizes.md)

## Short edits to an existing map

A transient created from a populated hash map or set copies compact trie paths
for its first eight writes. Longer runs then claim editable nodes. Builds from
empty enter editable mode immediately. If a key callback changes the transient
during a compact edit, the operation applies to the latest root and reuses the
comparisons and hashes it already obtained.

Each timed operation creates a transient from the same map, edits existing
integer keys, calls `persistent!` and reads the result count. The table gives
nanoseconds for the entire operation, including both replacements or all eight
where indicated. Each release ran five launches in rotating order, with four
warmups and seven measured batches of 8,192 operations per launch. Each cell
is the median of the five process medians. The eight-entry map uses array mode.
[Edit timings and allocations](single-edit-medians.csv)

| Map size | Operation | Main (`ef8dd3e9`), ns | Before (`2d944e93`), ns | Fixed, ns |
| ---: | --- | ---: | ---: | ---: |
| 8 | Replace | 146 | 143 | 145 |
| 8 | Delete | 138 | 137 | 135 |
| 1,024 | Replace | 664 | 782 | 202 |
| 1,024 | Delete | 673 | 790 | 227 |
| 32,768 | Replace | 1,003 | 1,253 | 259 |
| 32,768 | Delete | 1,039 | 1,213 | 281 |
| 32,768 | Replace the same key twice | 1,080 | 1,321 | 448 |
| 32,768 | Replace two distinct keys | 1,639 | 2,055 | 446 |
| 32,768 | Replace eight distinct keys | 5,447 | 6,696 | 1,501 |

The original review workload also ran across six rotating launches per binary.
Replacing the same key twice on the 32,768-entry map took 1,062 ns on main,
1,410 ns on the reviewed commit (`96f95c26`) and 383 ns on the fix. This
separate control used five warmups and seven measured batches per launch.

The original three-runtime bulk suite checked 2,754 warmup and timed batches
per binary. All checksums matched. The table gives Jolt nanoseconds per input
element from the median of three process medians. [Bulk benchmark samples](single-edit-bulk-samples.csv)

| Elements | Map, main / before / fixed | Set, main / before / fixed | Vector, main / before / fixed |
| ---: | ---: | ---: | ---: |
| 8 | 39.0 / 23.3 / 23.1 | 61.2 / 33.7 / 32.6 | 22.7 / 8.3 / 9.0 |
| 9 | 77.0 / 45.5 / 45.6 | 58.1 / 33.6 / 32.0 | 26.1 / 10.5 / 10.7 |
| 32 | 89.1 / 48.6 / 45.0 | 70.0 / 39.8 / 37.7 | 18.1 / 3.3 / 3.5 |
| 33 | 89.7 / 50.4 / 45.9 | 71.5 / 40.9 / 39.1 | 21.2 / 6.3 / 5.7 |
| 1,024 | 106.7 / 53.3 / 52.2 | 90.7 / 45.4 / 43.0 | 15.5 / 2.3 / 2.5 |
| 32,768 | 141.1 / 78.0 / 79.8 | 125.4 / 66.6 / 67.0 | 15.1 / 2.2 / 2.1 |

An isolated vector-build repeat used five alternating launches of `2d944e93`
and the fix. At eight elements the medians were 8.84 and 8.81 ns/element,
and at 1,024 elements they were 2.14 and 2.10 ns/element.

The optimized AOT suite ran all 51 workloads and the startup benchmark on
`ef8dd3e9` and the fix. The `transients` workload went from 99.0 to 69.0 ms,
and `int-map-build` from 62.3 to 44.3 ms. Only `pool-tasks` exceeded the
suite's 1.07× noise threshold (2,318.9 to 2,966.8 ms). The isolated gate
alternated five runs per binary and measured 2,613.7 to 2,735.8 ms, or 1.05×.
The pinned historical vector probe measured 15.0 ns/element for `reduce conj!` on this run.

## Follow-up experiments on 40584f39

Walking the input vector's leaves directly and specializing fixnum key hashing
reduced map and set build times at all six original sizes. These tables compare
release binaries on the same machine. The Vector, Map and Set columns give
median time before → after in ns per input element. The last two columns
are set times from the final three-runtime sweep. Both sweeps checked all
2,754 warmup and timed batches. [Three-runtime samples](experiment-build-samples.csv)

| Elements | Vector | Map | Set | Babashka set | JVM set |
| ---: | ---: | ---: | ---: | ---: | ---: |
| 8 | 8.9 → 8.9 | 31.9 → 22.9 | 50.7 → 36.6 | 39.8 | 24.7 |
| 9 | 10.8 → 11.5 | 60.1 → 46.5 | 48.3 → 34.9 | 38.2 | 23.7 |
| 32 | 3.6 → 3.6 | 66.6 → 49.8 | 58.5 → 41.0 | 51.6 | 40.7 |
| 33 | 6.2 → 6.3 | 68.4 → 51.8 | 59.5 → 42.9 | 51.4 | 42.6 |
| 1,024 | 2.4 → 2.6 | 69.1 → 55.6 | 62.8 → 47.9 | 62.5 | 45.1 |
| 32,768 | 2.1 → 2.3 | 96.4 → 78.3 | 84.4 → 70.7 | 84.1 | 74.3 |

The vector-only control alternated five launches of each binary, using the
same vector kernel with a 524,288-element budget, seven warmups and eleven
timed samples. Median changes were within 0.11 ns/element at each size.
At 32,768 elements the vector median went from 2.05 to 2.14 ns/element.

The direct loop selects the map or set insertion operation once per reduction.
It still validates each map entry and checks transient liveness before each
write, including after user-defined hash or equality code runs. Integer keys
feed the HAMT an unsigned Murmur3 hash directly. The high 32-bit word of
integers between −2³² and 2³²−1 has one of two constant mixes. Signed Clojure
hashes and unsigned HAMT hashes matched the JVM on 1,000,013 inputs in both the
wide and forced-narrow Chez paths. Native Gambit matched on a 10,013-input subset.

We also built each change independently. The two sparse-node variants used
persistent `node-assoc` on an immutable child while its slot count was below
four or eight. This avoids expanding that child and repacking it at
`persistent!`, but another write into the child then copies compact arrays.
The table shows set times in ns/element with the same generated workload, ten
warmups and seven samples in each of three launches. [Per-process medians and allocation](experiment-medians.csv)

| Elements | Baseline | Direct loop | Fixnum hash | Compact < 4 | Compact < 8 | Loop + hash |
| ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| 8 | 50.2 | 43.8 | 42.8 | 51.0 | 50.4 | 34.9 |
| 32 | 58.6 | 49.7 | 48.5 | 55.1 | 55.0 | 40.2 |
| 512 | 58.6 | 53.5 | 48.5 | 63.3 | 73.2 | 41.6 |
| 1,024 | 63.4 | 56.9 | 53.3 | 65.1 | 69.2 | 46.7 |
| 16,384 | 72.7 | 66.1 | 61.9 | 81.4 | 95.7 | 57.3 |
| 32,768 | 83.3 | 80.4 | 75.3 | 85.2 | 95.7 | 69.8 |

At 32 elements both thresholds reduced set allocation from 102.0 to 79.5
bytes/element. At 16,384 elements it rose from 80.4 to 93.0 and 145.9,
respectively. Maps showed the same reversal, so the runtime keeps the existing
expansion policy. These workloads build from empty with distinct integer keys.

## First runtime fix at 40584f39

With the runtime change, every measured build took less time on Jolt. The
table gives before → after in ns per element on the original workloads.

| Elements | Vector | Map | Set |
| ---: | ---: | ---: | ---: |
| 8 | 23.4 → 9.2 | 40.5 → 32.7 | 63.5 → 52.2 |
| 9 | 27.3 → 11.4 | 79.8 → 61.8 | 60.8 → 50.1 |
| 32 | 18.3 → 3.6 | 91.0 → 68.9 | 73.7 → 60.4 |
| 33 | 22.8 → 6.4 | 92.2 → 71.0 | 74.7 → 62.5 |
| 1,024 | 16.2 → 2.5 | 112.3 → 72.2 | 94.8 → 64.6 |
| 32,768 | 15.7 → 2.3 | 141.4 → 99.4 | 124.5 → 90.5 |

A three-argument `reduce` with the built-in `conj!`, a live vector transient
and a persistent vector source now reserves destination space once and copies
each source leaf. Maps and sets use 32 hash-indexed slots per editable trie node,
so inserting a key takes no slot shifts. Freezing packs just the occupied slots
in hash order. Sparse editable nodes can use more space while the transient is
live. The persistent representation stays compact.

The fixed binary ran the same three-runtime sweep with the original oracle.
All 2,754 batches matched. Jolt's vector median was lowest at every size. The
six set medians still trailed both peers, with gaps to Babashka of 0.4 and
2.0 ns per element at the two largest sizes. [Fixed build samples](fixed-build-samples.csv)

## Baseline at ef8dd3e9

Each cell gives **Jolt / Babashka / JVM Clojure** in nanoseconds per input
element. The timed operation reduces `conj!` over a prebuilt vector into a
fresh empty transient, calls `persistent!`, and probes the result. The map
input contains `[k (inc k)]` pairs, the set input contains consecutive integer
keys, and the vector input contains consecutive integer values.

| Elements | Vector | Map | Set |
| ---: | ---: | ---: | ---: |
| 8 | 23.4 / 24.0 / 17.0 | 40.5 / 79.5 / 34.3 | 63.5 / 40.4 / 23.9 |
| 9 | 27.3 / 22.1 / 13.2 | 79.8 / 102.8 / 44.4 | 60.8 / 39.1 / 22.8 |
| 32 | 18.3 / 11.1 / 8.8 | 91.0 / 76.6 / 53.6 | 73.7 / 52.7 / 41.0 |
| 33 | 22.8 / 11.5 / 7.3 | 92.2 / 75.6 / 52.0 | 74.7 / 52.4 / 46.8 |
| 1,024 | 16.2 / 7.1 / 6.2 | 112.3 / 75.1 / 53.2 | 94.8 / 63.1 / 47.7 |
| 32,768 | 15.7 / 7.4 / 6.1 | 141.4 / 98.2 / 80.9 | 124.5 / 86.1 / 78.3 |

The three runtimes ran sequentially on an Apple M3 Ultra VM, arm64 macOS
15.7.3, with Babashka 1.12.218 and Clojure 1.12.4 on OpenJDK 25.0.2. The
runner rotated runtime order and shuffled profiles across three launches.
Each process ran 10 warmups and seven timed samples per profile, with
`quot(65536, size)` complete builds per sample. The table uses the median of
the process medians divided by the actual number of elements built.
[Raw samples](build-samples.csv)

## Where the baseline spent the time

At 32,768 elements, 93–94% of sampled CPU time was in the vector reduction
that feeds the destination. The same source traversal drives all three
workloads. These are inclusive shares of each workload's sampled CPU time.
Nested rows overlap. Inlining and tail calls can charge a callee's work to
its caller.

| Runtime frame | Vector | Map | Set |
| --- | ---: | ---: | ---: |
| Source traversal and reducer, `vec-reduce-general` | 94.0% | 94.0% | 93.1% |
| Vector capacity growth, `tvec-ensure!` | 15.8% | | |
| Hash and insert one entry, `thash-put!` | | 57.3% | 69.3% |
| Insert a slot in an editable node, `enode-insert!` | | 18.9% | 24.8% |
| Hash a key, `key-hash` | | 14.5% | 19.1% |
| Finalize the vector trie or freeze editable hash nodes | 6.0% | 6.0% | 6.9% |

**Vectors.** Reserving the destination buffer once saved 1.2 ns/element.
Calling the vector-specific append helper for each input saved 4.5 ns/element
with ordinary buffer growth. The largest change came from copying whole
source leaves into the reserved buffer. All five variants paid for the same
final `persistent!` and read a varying index.

| Vector control at 32,768 elements | ns/element |
| --- | ---: |
| `reduce conj!` | 15.7 |
| `reduce conj!`, reserve once | 14.5 |
| `reduce` with `tvec-conj1!` | 11.2 |
| `reduce` with `tvec-conj1!`, reserve once | 10.0 |
| Reserve and copy source leaves into the buffer | 2.2 |

The copy control measures the opportunity for a bulk path when the reducer
is the built-in `conj!`, the input is a vector, and the accumulator is a
fresh empty vector transient. It uses private runtime functions under those
assumptions. An implementation in the general reducer would also have to
handle live transients with existing contents and preserve the behavior of
custom reducers and transient types. The probe checks complete vector equality
before timing. [Vector control samples](vector-samples.csv)

**Maps and sets.** Both use the editable hash trie. On insertion the runtime
hashes the incoming key, descends the trie, and opens a slot in a node.
`enode-insert!` shifts occupied slots and, when a node is full, copies it to
a larger array first. This is the first shared helper to benchmark for a
map/set improvement. Its sampled share includes its callees. Map builds also
validate and unpack each input pair in `tmap-conj-entry!`, which received
11.5% of map self time. The jump at nine map entries coincides with the first
promotion of the eight-entry slot buffer to a hash trie. A set starts in hash
mode. These profiles cover builds from empty with distinct integers.
[Transient operations](../../host/chez/transients.ss) · [Editable trie](../../host/chez/collections.ss)

## Reproduce

Run from the branch checkout with `bb`, `clojure` and the Jolt build
dependencies. Build the pinned baseline and the revision before the compact-edit
optimization in detached worktrees, including their submodules.
[Build prerequisites](../../CONTRIBUTING.md)

```sh
set -eu
root="$PWD"
work=$(mktemp -d)
build_revision() {
  git -C "$root" worktree add --detach "$work/$1" "$2"
  git -C "$work/$1" submodule update --init --recursive
  make -C "$work/$1" jolt-release
}
build_revision main ef8dd3e9903d2cb8f211b3ecfc88bafb006e37e4
build_revision before 2d944e93982f7c626a53c2c4b5e797aa0deff4ce
make -C "$root" jolt-release
baseline="$work/main/target/release/jolt"
before="$work/before/target/release/jolt"
candidate="$root/target/release/jolt"

git archive af74d25e benchmarks/generated-project | tar -x -C "$work"
kit="$work/benchmarks/generated-project"
bb "$kit/generate.bb" "$work/suite" --seed 42 --collection-sizes 8,9,32,33,1024,32768 \
  --only collections/vector/build-transient,collections/map/build-transient,collections/set/build-transient
cd "$work/suite"
clojure -Srepro -P -M
```

The bulk runner finds Jolt on `PATH`. The edit runner invokes each binary by
path and writes `$work/single-edit-medians.csv` in the checked-in format. It
rotates main, before and fixed over five launches per binary, including the
two- and eight-replacement cases. The multiple-write timings are per complete
transient/persistent round trip.

```sh
sh "$root/bench/transient-builds/run-edits.sh" "$baseline" "$before" "$candidate" "$work"

run_bulk() (
  binary=$1
  version=$2
  export PATH="$(dirname "$binary"):$PATH" JOLT_CACHE_DIR="$work/cache-$version"
  bb run.bb . 42 256 --samples 1
  bb run.bb . 42 65536 --warmup 10 --samples 7 --launches 3 --csv "$work/$version-builds.csv"
)
run_bulk "$baseline" main
run_bulk "$before" before
run_bulk "$candidate" fixed
```

The historical vector controls and CPU profile use the probe script from this
branch with the **baseline binary**. The vector probe checks equality, warms
each variant ten times, then measures seven rotating rounds of 16 builds.
The profiler samples each kernel separately at 1 ms.

```sh
for launch in 1 2 3; do
  JOLT_CACHE_DIR="$work/cache-main" "$baseline" "$root/bench/transient-builds/probe.clj" vector \
    > "$work/vector-$launch.csv"
done
JOLT_CACHE_DIR="$work/cache-main" "$baseline" "$root/bench/transient-builds/probe.clj" profile
```

For isolated map/set timings, run the same probe against each binary in the
generated suite directory. The launch argument rotates the order of the sizes.
The full optimized AOT suite runs from the repository root.

```sh
for version in main before fixed; do
  case "$version" in
    main) binary=$baseline ;;
    before) binary=$before ;;
    fixed) binary=$candidate ;;
  esac
  for launch in 0 1 2; do
    JOLT_CACHE_DIR="$work/cache-$version" "$binary" "$root/bench/transient-builds/compare.clj" "$launch" > "$work/$version-compare-$launch.txt"
  done
done
cd "$root"
NO_JVM=1 JOLT_BIN="$baseline" sh bench/run.sh > "$work/main-aot.log"
NO_JVM=1 JOLT_BIN="$candidate" sh bench/run.sh > "$work/fixed-aot.log"
git worktree remove --force "$work/main"
git worktree remove --force "$work/before"
```
