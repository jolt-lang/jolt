# Transient collection builds

The seed-42 collection benchmark from `gen-research` found 15 transient builds
where Jolt took longer than both Babashka and JVM Clojure. A release build of
`origin/main` at `ef8dd3e9` reproduced all 15. All 2,754 warmup and measured
batches passed the arithmetic checksum. [Original measurements](https://github.com/jolt-lang/jolt/blob/af74d25e599c649834edfb6c68a94c6a5160a1fd/benchmarks/generated-project/results/2026-11-10-seed42-collection-sizes.md)

## What reproduces

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

## Where Jolt spends the time

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
