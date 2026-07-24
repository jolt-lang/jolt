# jolt benchmark suite

Benchmarks that isolate the workload axes jolt's optimizing passes target. The
ray tracer (`examples/ray-tracer`) is **float-compute-bound** — its time is
irreducible algorithmic math (hit-testing + transcendentals), and devirt,
allocation removal, and type-proving all measured **flat** on it. So it can't
tell us whether those passes work. These benchmarks make each pass's target
workload the *dominant* cost.

Reference: the cross-language suites these draw from —
[Are We Fast Yet?](https://github.com/smarr/are-we-fast-yet) (Marr et al., DLS '16)
and the [Computer Language Benchmarks Game](https://benchmarksgame-team.pages.debian.net/benchmarksgame/).
The benchmarks are portable Clojure, so they also run on JVM Clojure for an
absolute reference.

## Benchmarks

| Benchmark | Axis | Pass it exercises | Source |
|---|---|---|---|
| `binary-trees` | allocation / GC pressure (escaping short-lived records) | scalar-replace, escape analysis | CLBG |
| `dispatch` | polymorphic (**megamorphic**) protocol dispatch | devirt, inline-cache | AWFY-style |
| `mono-dispatch` | **monomorphic** protocol dispatch (devirt/inline-cache *can* fire) | devirt, inline-cache | AWFY-style |
| `collections` | persistent map/vector churn (HAMT / 32-way tries) + map/filter/take/reduce over the built vector | persistent structures, transients | CLBG k-nucleotide-style |
| `mandelbrot` | pure float compute (tight arith loops, no alloc/dispatch) | native arith, loop codegen | CLBG |
| `arrays` | primitive `double-array` throughput (unboxed `aget`/`aset`, no boxing/collections) | unboxed primitive-array codegen (flvector read/write) | CLBG-style |
| `mathfns` | transcendental math (`java.lang.Math` sqrt/sin/cos/log/pow/atan2 over doubles) | native `Math` op lowering (`flsqrt`/`flsin`/… vs generic host-static dispatch) | CLBG-style |
| `fib` | recursion: function-call + integer-arith overhead | native arith, small-fn inlining | CLBG |
| `tak` | deep three-way self-recursion (denser call overhead than `fib`) | native arith, small-fn inlining, self-call direct-link | Gabriel |
| `loop-recur` | tight loop/recur iteration, no seq/collection alloc (single, nested, branchy) | native arith, loop codegen | AWFY-style |
| `seqs` | **lazy-seq + HOF pipelines** (range/map/filter/reduce, every?, iterate/take, mapcat) | lazy-seq allocation, per-element call overhead | AWFY-style |
| `transducers` | transducer pipelines (comp of map/filter/take via transduce/into/eduction) | reducing-fn composition, no lazy-seq cells | AWFY-style |

What the ray tracer does **not** capture and these do: allocation as the
bottleneck (~7% there), megamorphic *and* monomorphic dispatch (its dispatch is
monomorphic and cheap), persistent-collection throughput (it uses fixed records,
no collections in the hot loop), and isolated compute/call overhead.

Planned additions: Richards / DeltaBlue (heavier OO dispatch), NBody (float
control with record state), k-nucleotide proper.

## Holistic scorecard

`bench/run.sh` compiles each benchmark to an **optimized AOT binary** (`joltc build
--direct-link --opt`) and times it against JVM Clojure running the same portable
source — the jolt/JVM scorecard. jolt's optimizing passes fire only in a build;
`joltc run -m` is unoptimized, so the harness always builds.

Indicative ratios (M-series, best of two isolated runs — numbers are
machine-specific, regenerate locally), ascending. **opt** = `--direct-link
--opt`; **release** = a plain `jolt build` (`MODE_A=1` adds this column). The
`opt ms` / `jvm ms` columns are raw per-bench means from one indicative run, to
show absolute scale — they float run-to-run and won't line up exactly with the
curated ratios:

| benchmark | opt | release | opt ms | jvm ms | axis |
|---|---|---|---|---|---|
| `tak` | ~0.3× | ~0.3× | 6.7 | 19.8 | deep three-way self-recursion + integer arith (beats the JVM) |
| `fib` | ~0.6× | ~0.6× | 7.3 | 6.9 | recursion: call + integer arith (beats the JVM) |
| `collections` | ~1.1× | ~1.1× | 23.0 | 10.9 | persistent map/vector churn |
| `dispatch` | ~1.9× | ~1.8× | 68.3 | 54.2 | megamorphic protocol dispatch |
| `mandelbrot` | ~2.0× | ~2.0× | 28.3 | 13.8 | pure float compute |
| `transducers` | ~3.3× | ~3.3× | 235.3 | 32.8 | transducer pipelines (comp of map/filter/take) |
| `mono-dispatch` | ~3.7× | ~3.9× | 37.7 | 14.4 | monomorphic protocol dispatch |
| `binary-trees` | ~7.4× | ~7.3× | 282.7 | 39.9 | escaping short-lived records (allocation/GC) |
| `seqs` | ~8.0× | ~7.7× | 894.3 | 145.0 | lazy-seq + HOF pipelines (allocation + per-element calls) |
| `loop-recur` | ~8.3× | ~8.3× | 113.7 | 18.1 | tight loop/recur + per-iteration integer arith (`mod`, `quot`, `bit-xor`) |
| `arrays` | ~18.6× | ~18.6× | 675.0 | 36.3 | primitive `double-array` throughput (unboxed `aget`/`aset`) |
| `mathfns` | ~22.7× | ~23.2× | 376.7 | 16.6 | transcendental math (`Math` sqrt/sin/cos/log/pow/atan2 over doubles) |

`opt` and `release` now track each other closely across the suite — the plain
`jolt build` picks up most of the win. Earlier the release column trailed opt on
`mandelbrot` (~6.5×) and `mono-dispatch` (~4×); that gap has since closed, so the
scorecard still tracks both modes but they now mostly agree.

The two widest gaps are `loop-recur` and `seqs` (~8×), for different reasons.
`seqs` is the allocation axis idiomatic Clojure hits most: range/map/filter/reduce
chains, short-circuiting `every?`, `iterate`/`take`, and `mapcat` all build
lazy-seq cells and call a closure per element — a chain of allocated thunks with a
var-routed HOF call at each stage, none of which the optimizer's
arithmetic/dispatch/alloc passes target. The recent lazy-seq work (single-thread
lock elision, chunk fusion, transducer arities) brought this ratio down from
~10.9×, but it stays the dominant cost of script-style workloads (e.g.
ys-compiled programs). `loop-recur` is the arithmetic-loop axis: jolt matches the
JVM on the pure counted-loop back-edge (see `fib`/`mandelbrot`), so the gap is the
per-iteration integer arithmetic — a `mod` every step, plus `quot`/`even?`/`bit-xor`
in the nested and Collatz arms. Those ops emit inline (they are hot-primitive
lowered, not var-routed), so the residual is the arithmetic itself against what the
JIT compiles to primitive long ops.

- **Faster than the JVM (`tak` ~0.3×, `fib` ~0.6×)**: integer recursion and dense
  self-calls, where jolt direct-links the self-call and runs the arithmetic as
  proven fixnum ops.
- **`collections`** closed to ~1.1× with JVM-exact Murmur3 hashing (so `hash`,
  set/map iteration order, and hash-dependent output match the JVM byte for byte)
  and the array-map `(k . v)` fold, which drops the per-key HAMT lookup when
  iterating. The residual is Murmur3 on integer keys, which the JVM JITs to a
  handful of instructions and jolt runs as (unsafe, proven) fx ops.
- **`dispatch` ~1.9×, `mandelbrot` ~2.0×**: a megamorphic `dispatch` site runs a
  per-site polymorphic inline cache (4-slot descriptor scan, unsafe `#3%` vector
  reads since the cache shape is proven), so it no longer pays a registry lookup
  per call. `mandelbrot` is fl-unboxed float compute, now ~2× in both modes.
- **Dispatch & allocation (`transducers`/`mono-dispatch` ~3.3–3.9×,
  `binary-trees` ~7.4×)**: collapsed from two orders of magnitude by the
  type-proving / inline-field / bare-read work (`binary-trees` ~140×→~7×,
  `mono-dispatch` ~330×→~3.7×). On a *statically proven* monomorphic receiver —
  which whole-program inference gives for a record iterated out of a vector —
  devirt resolves the impl and a per-site inline cache holds it (resolved once,
  not per call), and an impl whose mixed-numeric (`:num`) field reads sit beside a
  proven double resolves a flonum-specialized clone. `binary-trees` nodes escape
  into the tree, so scalar-replace can't remove them: residual GC pressure over
  generic reads of untyped nilable fields.

## 64-bit integer arithmetic & generators (test.check)

The AOT suite above is float-compute / dispatch / allocation bound; none of it
exercises **64-bit integer arithmetic**, which Chez can't hold in a fixnum
(61-bit), so genuine 64-bit values are heap bignums. The SplitMix PRNG behind
`clojure.test.check` is the worst case — every `rand-long` is ~8 bignum ops. These
were measured in **run mode** (`joltc run`, where per-site var-cell caching is on;
the AOT build keeps it off) against JVM Clojure on the same portable source. The
first two rows are isolating microbenchmarks; the rest are real test.check
generators.

| workload | jolt | JVM | ratio | bound by |
|---|---|---|---|---|
| SplitMix `mix-64` (×100k) | 45ms | 14ms | ~3.2× | 64-bit integer arithmetic |
| deftype alloc + protocol dispatch (×100k) | 41ms | 5ms | ~8× | open-world dispatch |
| raw `split` + `rand-long` (×20k) | 74ms | 6ms | ~12× | bignum 64-bit + dispatch |
| `gen/large-integer` (×2k) | 108ms | 23ms | ~4.7× | arithmetic + rose-tree machinery |
| `(gen/vector gen/large-integer)` (×500) | 1289ms | 88ms | ~14.6× | element gen + gen machinery |

Two no-C codegen levers collapsed the **arithmetic** half: emitting `bit-and`/
`bit-or`/`bit-xor`/`bit-not` as inlined Chez `bitwise-*` primitives (they had gone
through a var-deref'd variadic overlay), and caching the resolved var cell per
reference site (a name lookup was ~45ns/access). Together they took `mix-64` from
~18× → ~3.2× JVM and the raw PRNG from ~30× → ~12×, and the generators ~1.6× each.

The residual gap is **machinery, not arithmetic**: the open-world generator
deftype/protocol dispatch + rose-tree allocation (~8–10×) can't be devirtualized
without static types, and the raw 64-bit ops bottom out at the Chez bignum floor
(~20× a native long, substrate-inherent). A native SplitMix C/FFI shim would give
the PRNG ~27× but is the only path that needs C.

## Running

```sh
bench/run.sh                 # full suite + JVM scorecard
bench/run.sh fib             # one benchmark, default size
bench/run.sh fib 32          # one benchmark, custom size
NO_JVM=1 bench/run.sh        # jolt only (skip the JVM reference)
MODE_A=1 bench/run.sh        # also time each bench as a plain release build
```

Two build modes matter: **optimized** (`--direct-link --opt`, the default
scorecard — inlining, scalar replacement, closed-world direct linking) and
**release** (plain `jolt build`, what a default build ships — inference passes
but no direct-link/inlining). `MODE_A=1` adds the release column so a
release-mode win or regression is visible; it roughly doubles build time, so
it's on demand.

Needs Chez's kernel dev files (`libkernel.a` + `scheme.h`) and `cc` for the build,
like `jolt build`; set `JOLT_CHEZ_CSV` to override the detected csv dir.

## Startup / small-program latency

`bench/run.sh` builds each benchmark to a binary and times the compute *inside*
it, so it deliberately excludes `joltc`'s own startup. That fixed floor — boot the
runtime + compiler image, then compile the program — is what dominates ys-style
workloads: many short `joltc prog.clj` runs where the program itself runs for
milliseconds. `bench/startup.sh` measures it, whole-process wall clock (best of N)
for a built joltc against babashka on the same sources:

```sh
bench/startup.sh                          # default 7 reps
REPS=15 bench/startup.sh                   # more reps
JOLT_BIN=/path/to/joltc bench/startup.sh   # pick the binary
```

Three sizes: `version` (pure boot floor, no program), `trivial` (boot + compile +
run a one-liner), `script` (a small lazy-seq pipeline). Use a BUILT joltc
(`target/release/joltc` or an installed one), not the dev `bin/joltc` source
launcher — the dev script boots from source and opts out of the AOT cache, so it
is not representative. Indicative (M-series): ~117ms vs babashka ~18ms (~6.5×).
The floor is runtime + compiler image instantiation that re-runs each boot (Chez
has no heap snapshot); see the CLI-closure AOT work that removed the per-boot
recompile of `jolt.main`.

`startup.sh` tells you the floor is there but not where it goes. `bench/startup-phases.sh`
attributes a `joltc prog.clj` run to four phases so a change shows which one it moved:

```sh
bench/startup-phases.sh                              # 7 reps, 400 defns, 30M-iter loop
REPS=15 bench/startup-phases.sh                      # more reps
DEFNS=800 LOOP=60000000 bench/startup-phases.sh      # heavier compile / run
JOLT_BIN=/path/to/joltc bench/startup-phases.sh      # pick the binary
```

`boot` is `joltc --version` (runtime + image load, `jolt.main` recompile).
`dispatch` is the deps/project resolve + load-file setup a file run adds on top,
measured against a `nil` file. `compile` is the delta of a compile-heavy,
run-trivial program (many defns) over the `nil` file, and `run` is the delta of a
run-heavy, compile-trivial program (one long loop). The phases are external
subtractions, each isolating one cost by construction — honest approximations,
not a strict partition, but directional: speed up the compiler and `compile`
drops, speed up the runtime and `run` drops. Indicative (M-series): boot ~110ms,
dispatch ~1ms, compile ~400ms for 400 defns, run ~120ms for a 30M-iter loop —
compilation is the dominant per-program cost.

## A/B against a change

To measure a pass, run the suite on `main`, then on the branch, back to back
(same machine, quiet). Each benchmark prints `runs: [...]` and `mean: N ms`;
compare the means. A pass is worth landing when it moves a benchmark whose axis it
targets, even if the ray tracer stays flat.

`bench/aba.sh` automates an A1/B/A2 over the six benches: it checks out the
parent's compiler files (`host/chez/seed/image.ss` +
`jolt-core/jolt/passes/types.clj`), builds and times each bench against `HEAD`,
then restores the working tree. A1≈A2 rules out drift; B vs A is the change.
