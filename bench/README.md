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
| `collections` | ~3.5× | persistent map/vector churn |
| `mandelbrot` | ~7.5× | pure float compute |
| `binary-trees` | ~10× | escaping short-lived records (allocation/GC) |
| `dispatch` | ~12× | megamorphic protocol dispatch |
| `mono-dispatch` | ~15× | monomorphic protocol dispatch |

- **Compute (~0.6–7.5×)** is the substrate floor: Chez is a native-compiling AOT
  Scheme, not a profiling JIT. With native arith + direct-linking + inlining jolt
  is at parity here — `fib` runs *faster* than JVM Clojure (no JIT warmup over a
  short run), `collections` is within ~3.5×, and `mandelbrot` (~7.5×) is the
  pure-tight-loop float ceiling that only native codegen moves further.
- **Dispatch & allocation (~10–15×)** are the remaining architectural gaps, though
  the type-proving / native-record / bare-field-read work has collapsed them by an
  order of magnitude (`binary-trees` ~140×→~10×, `mono-dispatch` ~330×→~15×). On a
  *statically proven* monomorphic receiver — which whole-program inference now gives
  for a record iterated out of a vector — devirt resolves the impl and a per-site
  inline cache holds it (resolved once, not per call), so `mono-dispatch` is no
  longer worse than megamorphic. The remaining lever is `dispatch`: a *megamorphic*
  site has no static type, so it pays a full protocol-registry lookup every call
  where the JVM uses a polymorphic inline cache — a runtime (receiver-type-keyed)
  cache is the missing piece. `binary-trees`
  nodes escape into the tree, so scalar-replace can't remove them — residual GC
  pressure.

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
```

Needs Chez's kernel dev files (`libkernel.a` + `scheme.h`) and `cc` for the build,
like `jolt build`; set `JOLT_CHEZ_CSV` to override the detected csv dir.

## A/B against a change

To measure a pass, run the suite on `main`, then on the branch, back to back
(same machine, quiet). Each benchmark prints `runs: [...]` and `mean: N ms`;
compare the means. A pass is worth landing when it moves a benchmark whose axis it
targets, even if the ray tracer stays flat.
