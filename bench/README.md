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
| `collections` | persistent map/vector churn (HAMT / 32-way tries) | persistent structures, transients | CLBG k-nucleotide-style |
| `mandelbrot` | pure float compute (tight arith loops, no alloc/dispatch) | native arith, loop codegen | CLBG |
| `fib` | recursion: function-call + integer-arith overhead | native arith, small-fn inlining | CLBG |

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

Indicative ratios (M-series, single isolated run — numbers are machine-specific,
regenerate locally). They cluster into two regimes:

| benchmark | ratio | axis |
|---|---|---|
| `mandelbrot` | ~8× | pure float compute |
| `fib` | ~9× | call + integer arith |
| `collections` | ~9× | persistent map/vector churn |
| `dispatch` | ~130× | megamorphic protocol dispatch |
| `binary-trees` | ~140× | escaping short-lived records (allocation/GC) |
| `mono-dispatch` | ~330× | monomorphic protocol dispatch |

- **Compute (~8–9×)** is the substrate floor: Chez is a native-compiling AOT
  Scheme, not a profiling JIT, so it can't match HotSpot on hot loops. Native arith
  already gets jolt closest here.
- **Dispatch & allocation (~130–330×)** are the architectural gaps. jolt does a
  full protocol-registry lookup on every call; the JVM inline-caches a
  runtime-monomorphic site to near-free — which is why `mono-dispatch` is *worse*
  than megamorphic. devirt only fires on *statically proven* receivers (which
  `reduce`/`mapv` over a heterogeneous vector never gives), so the passes don't
  engage; a call-site inline cache is the missing lever. `binary-trees` nodes
  escape into the tree, so scalar-replace can't remove them — this is GC pressure.
- The optimization passes move these benchmarks <10% vs the unoptimized run, so the
  gaps are not a missing-flag problem; they're the dispatch/GC/JIT-floor work.

## Running

```sh
bench/run.sh                 # full suite + JVM scorecard
bench/run.sh fib             # one benchmark, default size
bench/run.sh fib 32          # one benchmark, custom size
NO_JVM=1 bench/run.sh        # jolt only (skip the JVM reference)
```

Needs Chez's kernel dev files (`libkernel.a` + `scheme.h`) and `cc` for the build,
like `jolt build`; set `JOLT_CHEZ_CSV` to override the detected csv dir.

## A/B against a change

To measure a pass, run the suite on `main`, then on the branch, back to back
(same machine, quiet). Each benchmark prints `runs: [...]` and `mean: N ms`;
compare the means. A pass is worth landing when it moves a benchmark whose axis it
targets, even if the ray tracer stays flat.
