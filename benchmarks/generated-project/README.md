# Seeded Clojure workloads

Generate one project and run its source on Jolt, JVM Clojure and Babashka.
Install `jolt`, the `clojure` CLI (Clojure 1.11+), and `bb` on `PATH`.
From the repository root, run

```sh
kit=benchmarks/generated-project
bb "$kit/generate.bb" "$kit/out/5k" --seed 42 --lines 5k
bb "$kit/run.bb" "$kit/out/5k" 42 3
```

**Generation controls.** The seed selects the program. `--lines` targets a
nonblank source line count, and `--namespace-lines` controls the approximate
size of each worker. The destination must be absent or empty. The same
generation options reproduce the same files. Git ignores workloads under
`out/`. A generated directory is self-contained and can be copied to another
machine.

**Runtime controls.** The two numbers after the project path are the initial
input (0–1,000,002) and the iteration count (1–1,000,000). Both default to 1.
Each iteration passes the previous result through every worker. Change the
input to exercise different branches. Increase iterations to spend more time
in the same loaded program.

**Correctness.** Each runtime loads the same `src/workload/*.clj` files.
Workers combine maps, vectors, sequence transformations, reductions,
protocols, records, multimethods, macros and dynamic bindings. Every worker
also uses a shared Java interop function whose result feeds the checksum.
It calls `Math/abs(long)`, `Long/parseLong(String)`, `String.length()` and
`String.indexOf(String)` on bounded integers and ASCII decimal strings.

The runner first asks `oracle.bb` to interpret `model.edn` independently.
For example, the oracle models decimal parsing with arithmetic. The runner
prints each runtime's checksum and exits nonzero if any runtime fails or
disagrees with the oracle. A match checks the path taken by that input.
The manifest describes generated operations and source sizes. Some generated
definitions are intentionally unused and contribute only to source or load size.

**Timing.** The runner starts one fresh process at a time. Startup/load is
wall time from launch until the parent receives the child's flushed readiness
marker after loading the workload. The parent then signals the child to run.
Execution is the monotonic time the child spends inside `workload.main/run`.
Argument parsing, the oracle, reporting, and process exit are outside the
execution interval. There is no warmup. The first execution can include JIT
compilation, and startup/load can benefit from existing filesystem and runtime
caches. Repeat the runner for independent process samples.

Inside a generated directory, the individual entry points are

```sh
jolt -m workload.main 42 3
clojure -Srepro -M -m workload.main 42 3
bb -cp src -m workload.main 42 3
bb oracle.bb 42 3
```

Run `bb benchmarks/generated-project/test.bb` from the repository root for
the portability and mismatch checks.

The generator and oracle are adapted from
[light-cljs](https://github.com/isaksky/light-cljs/tree/5ed076d3230ec89be432b5d4c9156f9ee34d298c/benchmarks/generated-project)
under the [MIT license](LICENSE).
