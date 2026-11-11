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
descendants. The default enables all categories. Exclusions apply last.

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
