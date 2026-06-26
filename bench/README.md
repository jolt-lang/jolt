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
regenerate locally), ascending:

| benchmark | ratio | axis |
|---|---|---|
| `fib` | ~0.6× | call + integer arith |
| `collections` | ~4× | persistent map/vector churn |
| `mandelbrot` | ~7.5× | pure float compute |
| `binary-trees` | ~10× | escaping short-lived records (allocation/GC) |
| `dispatch` | ~12× | megamorphic protocol dispatch |
| `mono-dispatch` | ~48× | monomorphic protocol dispatch |

- **Compute (~0.6–7.5×)** is the substrate floor: Chez is a native-compiling AOT
  Scheme, not a profiling JIT. With native arith + direct-linking + inlining jolt
  is at parity here — `fib` runs *faster* than JVM Clojure (no JIT warmup over a
  short run), `collections` is within ~4×, and `mandelbrot` (~7.5×) is the
  pure-tight-loop float ceiling that only native codegen moves further.
- **Dispatch & allocation (~10–48×)** are the remaining architectural gaps, though
  the type-proving / native-record / bare-field-read work has collapsed them by an
  order of magnitude (`binary-trees` ~140×→~10×). jolt still does a full protocol-
  registry lookup on every call; the JVM inline-caches a runtime-monomorphic site
  to near-free — which is why `mono-dispatch` is *worse* than megamorphic and is now
  the standout gap. devirt fires only on a *statically proven* receiver; whole-
  program inference now proves more of them, but a value iterated out of a vector
  still needs one — a call-site inline cache is the missing lever. `binary-trees`
  nodes escape into the tree, so scalar-replace can't remove them — residual GC
  pressure.

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
