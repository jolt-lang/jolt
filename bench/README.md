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
| `binary-trees` | allocation / GC pressure (escaping short-lived records) | jolt-15jq scalar-replace, jolt-8flj escape analysis | CLBG |
| `dispatch` | polymorphic (**megamorphic**) protocol dispatch | jolt-41m devirt, inline-cache | AWFY-style |
| `mono-dispatch` | **monomorphic** protocol dispatch (devirt/inline-cache *can* fire) | jolt-41m devirt, jolt-ez5h inline-cache | AWFY-style |
| `collections` | persistent map/vector churn (HAMT / 32-way tries) | persistent structures (jolt-684u/0hbr), transients | CLBG k-nucleotide-style |
| `mandelbrot` | pure float compute (tight arith loops, no alloc/dispatch) | jolt-3pl native arith, loop codegen | CLBG |
| `fib` | recursion: function-call + integer-arith overhead | jolt-3pl native arith, jolt-826 small-fn inlining | CLBG |

What the ray tracer does **not** capture and these do: allocation as the
bottleneck (~7% there), megamorphic *and* monomorphic dispatch (its dispatch is
monomorphic and cheap), persistent-collection throughput (it uses fixed records,
no collections in the hot loop), and isolated compute/call overhead.

Planned additions: Richards / DeltaBlue (heavier OO dispatch), NBody (float
control with record state), k-nucleotide proper.

## Holistic scorecard

`JVM=1 bench/run.sh` runs each benchmark on jolt **and** JVM Clojure and prints
the jolt/JVM ratio — the epic's (jolt-ffn) absolute-reference scorecard. As of
the broadening (2026-06-16), ratios cluster by axis:

- **pure compute** (`mandelbrot`) is the floor, ~15× — native arith (jolt-3pl)
  already gets jolt closest to the JVM.
- **collections** ~28×, **fib** ~37×.
- **dispatch** ~75× (megamorphic), and `mono-dispatch` is *worse* (~110×): the
  JVM inline-caches a runtime-monomorphic call site to near-free, while jolt does
  a full registry dispatch regardless (devirt only fires on *statically* proven
  receivers, which `reduce` over a vector doesn't give). This is the signal for
  the call-site inline cache (jolt-ez5h).
- **allocation** (`binary-trees`) is the widest gap — but also the most inflated
  by host memory pressure, so read it as "alloc is the worst axis," not a precise
  multiple. Numbers are machine-specific; regenerate with `JVM=1 bench/run.sh`.

## Running

```sh
jpm build && export PATH="$PWD/build:$PATH"
bench/run.sh                      # whole-program optimization on (default)
JOLT_WHOLE_PROGRAM=0 bench/run.sh # WP off, to measure what WP buys
bench/run.sh binary-trees 16      # one benchmark, custom size
```

## A/B against a change

To measure a pass, run the suite on `main`, then on the branch, back to back
(same machine, quiet) — the protocol used for `test/bench/core-bench.janet` and
the ray tracer. Each benchmark prints `runs: [...]` and `mean: N ms`; compare
the means. A pass is worth landing when it moves a benchmark whose axis it
targets, even if the ray tracer stays flat.
