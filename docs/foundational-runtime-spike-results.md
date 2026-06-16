# Foundational Runtime Spike — Results (the 15× floor, localized)

**Epic:** jolt-5vsp · **Date:** 2026-06-16
**Spike:** the START HERE section of `docs/foundational-runtime-handoff.md` —
jolt vs hand-written-Janet vs JVM `mandelbrot`, to localize the ~15× compute
floor before committing to native codegen (lever 1) vs a backend fix.

## Setup

Three implementations of the same nested mandelbrot loop, all returning the
identical result (3288753 at n=200, confirming correctness across all legs):

- **jolt-compiled** — `bench/mandelbrot.clj` (`jolt -m mandelbrot 200`, WP + direct-link on)
- **hand-Janet (`while`)** — `bench/mandelbrot-hand.janet` (idiomatic Janet: `while` + `var`/`set`)
- **JVM Clojure** — `bench/mandelbrot.clj` on the JVM

Plus a diagnostic fourth leg:

- **hand-Janet (recursive)** — `bench/mandelbrot-hand-rec.janet`: hand Janet that
  *mirrors jolt's loop lowering* (self-recursive local closure called per
  iteration), to test whether the loop lowering alone explains jolt's overhead.

Numbers are stable and sandwiched (A/B/A/B); machine noise < 1%.

## The numbers (n=200, mean of 3, after warmup)

| Leg | mean | × JVM |
|---|---|---|
| JVM Clojure | 14.2 ms | 1.0× |
| **hand-Janet (`while`)** | **153.4 ms** | **10.8×** |
| hand-Janet (recursive, mirrors jolt) | 215.3 ms | 15.2× |
| **jolt-compiled** | **219.0 ms** | **15.4×** |

## What this localizes

The 15.4× floor **decomposes into two distinct layers**:

1. **Janet VM floor ≈ 10.8× JVM** (70% of the gap). Optimal hand-written Janet —
   pure `while` loop over unboxed doubles, zero allocation — is still ~11× slower
   than JVM Clojure. This is the cost of the Janet bytecode VM itself (no JIT, no
   native code). **Only native codegen (lever 1) can touch this.** It is the
   dominant share and validates lever 1 as the big structural lever.

2. **jolt backend loop-lowering ≈ 1.43× on top** (the remaining 30%). jolt is
   `219 / 153 = 1.43×` slower than optimal Janet. The diagnostic leg pins this
   *entirely* to one cause: jolt lowers every `loop`/`recur` to a **self-recursive
   local closure called once per iteration**, not a `while` loop. Hand-Janet
   written that same way (recursive leg) lands at **215 ms ≈ jolt's 219 ms** —
   so the recursive-closure lowering accounts for essentially all of jolt's
   backend overhead on pure-compute code.

   See the emitted Janet (`bench/dump-mandelbrot-emit.janet`): `emit-loop`
   (`src/jolt/backend.janet:210`) produces
   `(do (var L nil) (set L (fn (i zr zi) … (L (+ i 1) …))) (let (…) (L …)))`
   and `emit-recur` (`:228`) produces the per-iteration call `(L …)`. It relies
   on Janet TCO for stack safety, but each iteration still pays a function
   invocation (frame setup + arg bind) that a `while` loop skips.

## Decision

The handoff posed it as binary (Janet-VM floor *or* backend headroom). It is
**both**, now sized:

- **Native codegen (lever 1) is the only thing that moves the dominant ~70%.**
  Confirmed as the big lever. Pursue the incremental jolt-IR→C spike for one hot
  fn next, per the handoff.
- **A cheap, localized ~30% win sits in the backend**, independent of any new
  runtime: lower tail-position `loop`/`recur` with scalar bindings to a Janet
  `while` + `var`/`set` instead of a recursive closure. Closes the 1.43×, taking
  `mandelbrot` from 15.4× → ~10.8× JVM. Filed separately (see epic children).

## Open questions answered

- **Are Janet numbers boxed?** No — already unboxed. The `while` leg does pure
  double arithmetic at a steady 153 ms with no allocation and no GC stutter, and
  matches the other legs bit-for-bit. Janet's `number` is an immediate IEEE
  double (stored inline in the Janet value, not heap-allocated). **Unboxing is
  not a lever; it's done.**
- **GC share of `binary-trees`** — not measured here (the dev machine swaps
  heavily, which distorts alloc-heavy benches; the handoff flags this). Size
  lever 2 on a clean machine. The `mandelbrot` legs are alloc-free so are
  unaffected and trustworthy.
- **Janet native-module / incremental C path** — not yet confirmed; this is the
  gating question for the lever-1 spike (hot fns → C, rest → bytecode).

## Artifacts (kept in `bench/`)

- `mandelbrot-hand.janet` — optimal `while` Janet (the Janet VM floor reference)
- `mandelbrot-hand-rec.janet` — recursive-closure Janet (the loop-lowering diagnostic)
- `dump-mandelbrot-emit.janet` — dumps the Janet jolt emits for the hot fns

The bench harness (`bench/run.sh`) ignores these (it iterates a fixed bench list).
