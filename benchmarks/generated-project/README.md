# Seeded Clojure workloads

Generate one project and run the same source on Jolt, JVM Clojure and Babashka.
Install `jolt`, the `clojure` CLI (Clojure 1.11+) and `bb` on `PATH`.
From the repository root, run

```sh
kit=benchmarks/generated-project
bb "$kit/generate.bb" "$kit/out/mixed" --seed 42 --lines 5k
bb "$kit/run.bb" "$kit/out/mixed" 42 3
```

## Select a workload

Use `--list-categories` to see every leaf. Both `--only` and `--exclude` accept
comma-separated paths and can be repeated. A parent selects all its
descendants. The default mixed-source workload enables all mixed-source categories.
Use the collection-size mode below for focused data-structure measurements. Exclusions apply last.

```sh
bb "$kit/generate.bb" --list-categories
bb "$kit/generate.bb" "$kit/out/buffers" --seed 42 --lines 5k --only jvm/byte-buffer
bb "$kit/generate.bb" "$kit/out/fs" --seed 42 --lines 5k --only babashka/fs --exclude babashka/fs/normalize
bb "$kit/generate.bb" "$kit/out/dispatch" --seed 42 --lines 5k --only dispatch,core/collections
bb "$kit/generate.bb" "$kit/out/no-interop" --seed 42 --lines 5k --exclude jvm,babashka
```

| Category | What it selects |
| --- | --- |
| `core` | Random expressions grouped by arithmetic, collections, sequences, control flow, functions, state and text |
| `dispatch` | Protocols with records and types, or multimethods with dispatch hierarchies |
| `syntax/macros` | Generated macro definitions and calls |
| `load/unused` | Deliberately unused definitions, arities and namespaces for studying load cost |
| `jvm` | Java calls, grouped by class. Select `jvm/byte-buffer/get-short` for one operation or `jvm/byte-buffer` for the class |
| `babashka/fs` | Lexical path operations. Each of `file-name`, `parent`, `extension`, `strip-ext` and `normalize` is selectable |

Each worker gets a seeded ranking of the method pool. It takes two to four
distinct enabled entries, or every entry when fewer are available. Every
selected entry runs on the checksum path at least once per iteration. The
method ranking has its own random stream and is stable for a given seed and
worker index across category choices. Changing categories can change worker
counts and code shapes. Categories select generated definitions and operations.
Their implementations still use ordinary Clojure. For example, a sequence
operation can take a vector literal and a protocol implementation can use a
volatile. Every profile includes function calls and checksum arithmetic.

The Java pool covers numeric and string methods, string builders, collections,
byte buffers, time, UUIDs, atomic longs and CRC32. Candidates come from
Jolt's Java implementations. The pool test runs each emitted call directly on
all three runtimes and checks multiple inputs against the oracle.
[Jolt Java shims](../../host/chez/java) · [Method catalog](features.bb)

Projects using `babashka.fs` use the bundled library on Jolt and Babashka. The
generated JVM-only `:bench-jvm` alias pins `babashka/fs` to `0.3.17`. The JVM
CLI needs that dependency cached or available from Clojars on its first run.
To fetch it before measuring startup, run `clojure -Srepro -P -M:bench-jvm`
inside the generated project.
The filesystem workloads manipulate path components in memory.

## Measure collection operations by size

Generate a suite of persistent maps, vectors, and sets. The suite measures every
selected operation at every requested element count. Each pair has a separate
timing and an independently computed checksum. From the repository root run

```sh
kit="$PWD/benchmarks/generated-project"
project="$(mktemp -d)/collections"
bb "$kit/generate.bb" "$project" --seed 42 --collection-sizes 8,9,32,33,1024,32768
(cd "$project" && clojure -Srepro -P -M)
bb "$kit/run.bb" "$project" 42 65536 --warmup 10 --samples 7 --launches 3 --csv "$project/samples.csv"
```

Select one operation or a subtree with, for example, `--only collections/map/get-hit`
or `--only collections/vector,collections/set --exclude collections/vector/pop`.
Selecting `collections` or a descendant implies this mode and defaults to the
six sizes above. Explicit sizes range from 1 to 1,000,000 elements. The seed
sets the starting position and a stride coprime to the collection size, so a
full cycle probes every key or index exactly once.

| Family | Measured operations |
| --- | --- |
| Map | Hit and miss lookup, replacement, insertion, removal, `reduce-kv`, transient build |
| Vector | Indexed read and replacement, append, pop, reduce, transient build |
| Set | Hit and miss membership, insertion, removal, reduce, transient build |
| Control | Read the same probe array and accumulate its entries |

The positional arguments for this mode are the input offset and the work
budget (defaults 1 and 32,768). Point operations perform exactly that many
calls. Traversals and builds perform `max(1, quot(budget, size))` complete
calls. Each profile prepares integer keys, source collections, and probes
before warmup. Persistent writes start from the same immutable root and read
the result back. Transient builds start empty and include `persistent!` and a
probe of the resulting collection. The generated source shows the exact
expression for each row.

Each launch starts one fresh process per runtime. All profiles execute in that
process, with a seeded shuffle of profile order for each launch and a rotation
of runtime order. The runner finishes with a side-by-side table of the three
execution medians and Jolt’s elapsed-time ratio to each peer. Startup/load
stops at the readiness marker before any
profile prepares its fixtures. The CSV records calls per batch, checksum,
startup/load, and every measured execution in milliseconds. Startup repeats
on rows from the same process. Divide elapsed time by calls for point
operations, or by calls × size for time per input element on traversals and builds.

The recorded seed-42 sweep includes all 120 profiles. [Results and comparison chart](results/2026-11-10-seed42-collection-sizes.md)

## Reproduce and time a run

The generation seed selects the program. `--lines` targets a nonblank source
line count, and `--namespace-lines` controls the approximate size of a worker.
Generate into an empty or absent directory. Git ignores projects under
`out/`. In `manifest.edn`, `:generator-options` records the resolved
categories and seed, `:worker-api-methods` lists each worker's choices, and
`:api-call-sites` counts the generated call sites for each method.

```sh
bb "$kit/run.bb" "$kit/out/buffers" 7 100 --warmup 3 --samples 7
```

Here `7` is the initial input and `100` is the number of passes through the
workers per execution. Input ranges from 0 to 1,000,002 and iterations from
1 to 1,000,000. Both default to 1. Each warmup and measured sample starts
with the same input in the same loaded process.

The runner computes the expected checksum from `model.edn` outside the timed
intervals. The oracle models each selected operation independently. For
example, it obtains a buffer's signed short from the input bits and derives
the expected path component from the original strings. A mismatch on any
warmup or measured sample makes the runner exit nonzero.

**Startup/load** is wall time from process launch to the child's flushed
readiness marker after loading the project. **Execution** is the child's
monotonic time inside `workload.main/run`. By default each runtime gets one
fresh process, zero warmups and one execution sample. With `--samples N`, the
runner reports the median and the individual times. Repeat the whole command
for independent startup samples. Existing runtime and filesystem caches
affect startup, and the first execution can include JIT compilation.

Each generated project's README gives its standalone launch commands. Run
`bb benchmarks/generated-project/test.bb` for the pool, selection, timing,
determinism and checksum checks.

The generator and oracle are adapted from
[light-cljs](https://github.com/isaksky/light-cljs/tree/5ed076d3230ec89be432b5d4c9156f9ee34d298c/benchmarks/generated-project)
under the [MIT license](LICENSE).
