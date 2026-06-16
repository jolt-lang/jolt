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
| `dispatch` | polymorphic (megamorphic) protocol dispatch | jolt-41m devirt, inline-cache | AWFY-style |
| `collections` | persistent map/vector churn (32-way tries) | persistent structures, transients | CLBG k-nucleotide-style |

What the ray tracer does **not** capture and these do: allocation as the
bottleneck (~7% there), megamorphic dispatch (its dispatch is monomorphic and
cheap), and persistent-collection throughput (it uses fixed records, no
collections in the hot loop).

Planned additions: Richards / DeltaBlue (heavier OO dispatch), a **monomorphic**
dispatch variant (where devirt *can* fire — the megamorphic `dispatch` can't),
NBody (float control, parallels the ray tracer), k-nucleotide proper.

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
