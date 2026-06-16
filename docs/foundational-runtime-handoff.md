# Foundational Runtime Epic — Handoff

**Epic:** jolt-5vsp · **Predecessor:** jolt-ffn (targeted specialization — concluded)
**Date:** 2026-06-16

This is a cold-start handoff. Read it top to bottom before touching code. Its
whole point is to keep the fresh session from re-running the experiments that
already came back flat, and to start from the one measurement that actually
tells us where to invest.

## Why this epic exists

The targeted-specialization epic (jolt-ffn) tried to close jolt's constant-factor
gap vs JVM Clojure with per-form compiler passes. Three independent attempts all
came back flat:

| Attempt | Bead | Result |
|---|---|---|
| Record field-read guard removal (bare field reads) | jolt-3ko | ~3% on dispatch (shipped #141 — kept for correctness, not speed) |
| Protocol inline cache (runtime, per-method) | jolt-ez5h | ~0% — the per-dispatch gen-check exactly cancels the find-protocol-method saving; `find` was never the bottleneck |
| Record-ctor descriptor-baking (fewer allocs/record) | jolt-p7fo | flat on binary-trees + broke the gate; reverted |

The conclusion: **the gap is structural to jolt-on-Janet, not a missing
optimization.** Targeted passes remove only the cheap parts; the structural floor
remains.

## The scorecard (jolt / JVM Clojure)

Regenerate any time with `JVM=1 bench/run.sh` (the absolute-reference mode).

| Axis | Bench | jolt/JVM |
|---|---|---|
| Pure float compute | `mandelbrot` | **~15×  ← THE FLOOR** |
| Persistent collections (HAMT) | `collections` | ~28× |
| Recursion (call + arith) | `fib` | ~37× |
| Megamorphic dispatch | `dispatch` | ~76× |
| Monomorphic dispatch | `mono-dispatch` | ~109× |
| Allocation / GC | `binary-trees` | ~314× (≈150× at depth 10) |

`mandelbrot` is the floor: pure tight arithmetic loops — no dispatch, no
allocation, no collections — and native arith already fires (jolt-3pl). So ~15×
is what jolt's *execution substrate* costs on the simplest possible workload.
Every other axis adds structural overhead **on top** of that floor.

**Machine caveat:** the dev machine swaps heavily (~13 GB). Alloc-heavy benches
(`binary-trees`, `collections`) inflate badly; light benches (`mandelbrot`,
`fib`, `dispatch`) are trustworthy. Get absolute alloc numbers on a clean machine.

## The four structural walls

1. **Bytecode-VM execution.** jolt's backend emits **Janet** (a register-bytecode
   VM) and runs it on the Janet interpreter loop — no JIT, no native code. Every
   op is bytecode dispatch. This is the `mandelbrot` 15× floor.
2. **Mark-sweep GC.** Janet's GC scans all live objects each cycle (no
   generations). Live-data + alloc-heavy workloads (`binary-trees` retains the
   tree) pay O(live) per GC. The JVM's generational GC makes young-object churn
   nearly free.
3. **Indirect calls.** Protocol dispatch and fn calls go through indirection
   (closures, the protocol registry). The JVM inlines/devirtualizes. jolt's
   devirt (jolt-41m) only fires on *statically*-proven monomorphic sites;
   `reduce`/`mapv` over a collection doesn't give that proof, so the common
   runtime-monomorphic case pays full dispatch (that's why `mono-dispatch` is
   *worse* than megamorphic — the JVM inline-caches it to near-free, jolt doesn't).
4. **Boxed / generic representations.** Records are tuples `[descriptor field…]`;
   field access goes through a tag guard unless the type is proven. Generic ops
   carry runtime type checks. (Open question: are Janet *numbers* boxed? Verify
   in the spike — it decides whether unboxing is a lever or already done.)

## Foundational levers (ranked)

1. **Native codegen — emit C, not Janet bytecode.** The Stalin approach. Compile
   jolt IR → C → machine code via the system compiler. The *only* lever that
   moves the 15× compute floor; could approach C/JVM speed on compute-bound code.
   Massive (a new backend). Plausible incremental shape: a jolt-IR→C compiler for
   *hot* fns with a fallback to the existing bytecode path for unsupported forms —
   mirroring today's interpret/compile hybrid. Needs to confirm Janet's C-API /
   native-module story can be targeted incrementally.
2. **Structural GC-pressure reduction.** Value-type small records (avoid heap),
   transient/editable-node hot paths (RFC 0003 future work — pvec/phm/sorted are
   now tries/HAMT/RB, so O(1) `transient`/`persistent!` via editable nodes is
   open). Helps the alloc-bound axes (`binary-trees`, `collections`). Does **not**
   touch the compute floor.
3. **Deeper devirt + body inline.** Propagate element/return types so devirt
   fires on runtime-monomorphic collections, then inline the method body
   (jolt-4x9 element types + jolt-t6r). Helps dispatch. Bounded ceiling (still
   bytecode underneath).

## START HERE — the spike (do this before committing to any lever)

**Localize the 15× floor.** Build three `mandelbrot` implementations and compare:

- **jolt-compiled** `mandelbrot` (already in `bench/mandelbrot.clj`),
- **hand-written Janet** `mandelbrot` (the same nested loop, idiomatic Janet —
  write it directly, no jolt),
- **JVM Clojure** `mandelbrot`.

Two ratios fall out:

- **jolt-emitted-Janet vs hand-Janet** → how much overhead jolt's *backend* adds
  over optimal Janet. To see jolt's emitted Janet, use the backend emit path
  (`backend/emit-ir` on the analyzed `run`/`count-point` fns) — note `:arities`
  etc. are jolt pvecs, so introspection is awkward; easier to read the emitted
  Janet via the compile path or just A/B the timings.
- **hand-Janet vs JVM** → the Janet VM's own floor.

Decision:

- If **hand-Janet ≈ jolt** and hand-Janet is ~15× JVM → the floor is **Janet's
  bytecode VM**. Native codegen (lever 1) is the only fix. Commit to the spike of
  a jolt-IR→C path for one hot fn and measure.
- If **jolt ≫ hand-Janet** → jolt's backend emits suboptimal Janet; there's
  headroom in the **backend** (cheaper, no new runtime). Find what it emits that
  hand-Janet doesn't.

Also measure the **GC share** on `binary-trees` (Janet GC stats around the run —
`(gccollect)` / `gcinterval`, or count allocations) to size lever 2 honestly.

## Key files / mechanisms

- **Backend (IR → Janet emit):** `src/jolt/backend.janet`. `native-ops` (~L322)
  emits native Janet arith; `emit-ir` (~L674) runs passes then emits. A native-C
  backend would branch here.
- **Passes / inference:** `jolt-core/jolt/passes.clj` (`run-passes`),
  `jolt-core/jolt/passes/types.clj` (inference; the `:fn` branch ~L527 now seeds
  ^Record param hints — #141), `jolt-core/jolt/passes/inline.clj`
  (scalar-replace, `ctor-shape`).
- **Record representation:** `src/jolt/types_protocols.janet` — `make-record`
  (~L145, the ~5-alloc/record path), `record-shape-for` (~L139, rebuilds its
  cache key every call), `record-tag`. Records are tuples `[descriptor field…]`.
- **Dispatch + ctors:** `src/jolt/eval_runtime.janet` —
  `protocol-dispatch-impl` (~L62), `make-deftype-ctor-impl` (~L382).
- **Config knobs:** `src/jolt/config.janet` — `JOLT_DIRECT_LINK`,
  `JOLT_WHOLE_PROGRAM`, `JOLT_OPTIMIZE`, the `ctx-shaping-env-vars` list (any new
  ctx-shaping env var MUST be added there and to `image-cache-path`).
- **Self-hosting design:** `docs/self-hosting-compiler.md` (the kernel/value-layer
  boundary), `docs/rfc/0003-transients.md` (editable-node future work).

## How to build, run, measure

```sh
jpm build                         # build/jolt (ctx baked, ~20ms startup); from-source is ~8s cold
export PATH="$PWD/build:$PATH"
bench/run.sh                      # jolt only, WP on
JVM=1 bench/run.sh                # jolt vs JVM scorecard (needs `clojure` on PATH)
bench/run.sh mandelbrot 400       # one bench, custom size
JOLT_WHOLE_PROGRAM=0 bench/run.sh # measure what WP buys
```

Gate: `jpm build; janet run-tests.janet` (parallel, ~100s; `JOLT_TEST_JOBS`
overrides). Bench memory hygiene (`bd memories bench-isolation-gotcha`): never run
a perf matrix while other CPU work runs — it starves later configs and produces
bogus numbers. Sandwich A/B/A.

## What NOT to repeat (already flat — see beads for detail)

- Runtime protocol inline cache (jolt-ez5h): gen-check cancels the saving.
- Field-read guard removal as a *speed* play (jolt-3ko): ~3%; machinery dominates.
  (The #141 change is kept for correctness + the `with-meta`-on-symbols fix.)
- `make-record` descriptor-baking (jolt-p7fo): flat — `binary-trees` is dominated
  by the live retained tree + GC, not the short-lived intermediate allocs.

## Open questions for the spike

- Are Janet numbers boxed? (Lever or already done.)
- Does Janet expose a native-module / C-codegen path jolt can target incrementally
  (hot fns → C, rest → bytecode)?
- What fraction of `binary-trees` is GC vs execution?
- Is there a cheaper record representation (Janet struct vs tuple-with-descriptor)
  that lowers field-read + alloc cost without a new backend?
