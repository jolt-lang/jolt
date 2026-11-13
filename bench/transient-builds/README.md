# Transient collection builds

The seed-42 collection benchmark from `gen-research` found 15 transient builds
where Jolt took longer than both Babashka and JVM Clojure. A release build of
`origin/main` at `ef8dd3e9` reproduced all 15. All 2,754 warmup and measured
batches passed the arithmetic checksum. [Original measurements](https://github.com/jolt-lang/jolt/blob/af74d25e599c649834edfb6c68a94c6a5160a1fd/benchmarks/generated-project/results/2026-11-10-seed42-collection-sizes.md)

## Follow-up experiments on 40584f39

Walking the input vector's leaves directly and specializing fixnum key hashing
reduced map and set build times at all six original sizes. These tables compare
release binaries on the same machine. Each cell in the first three columns is
the median time before → after in ns per input element. The last two columns
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

Run from the repo root with `bb`, `clojure` and the Jolt build dependencies.
The generator and oracle come from the original research commit. Only the
three transient workloads are selected.

```sh
make jolt-release
root="$PWD"
work=$(mktemp -d)
export PATH="$root/target/release:$PATH" JOLT_CACHE_DIR="$work/cache"
git archive af74d25e benchmarks/generated-project | tar -x -C "$work"
kit="$work/benchmarks/generated-project"
bb "$kit/generate.bb" "$work/suite" --seed 42 --collection-sizes 8,9,32,33,1024,32768 \
  --only collections/vector/build-transient,collections/map/build-transient,collections/set/build-transient
cd "$work/suite"
clojure -Srepro -P -M
bb run.bb . 42 256 --samples 1
bb run.bb . 42 65536 --warmup 10 --samples 7 --launches 3 --csv "$work/builds.csv"
for launch in 1 2 3; do
  jolt "$root/bench/transient-builds/probe.clj" vector > "$work/vector-$launch.csv"
done
jolt "$root/bench/transient-builds/probe.clj" profile
```

The vector controls use 16 builds per sample, ten warmups per variant, then
seven timed rounds with rotating variant order. The sampling run exercises
the original kernels separately with a 1 ms CPU sampling interval and two
probes per batch. It collected 1,233 vector, 1,442 map and 1,592 set samples.

To repeat the isolated timing and allocation measurements, invoke each candidate
release binary from the generated suite directory. The script checks each
checksum, varies the probes, and rotates the order of sizes between launches.

```sh
for launch in 0 1 2; do
  jolt "$root/bench/transient-builds/compare.clj" "$launch"
done
```
