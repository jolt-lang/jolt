# Chez Scheme re-host spike — results

Branch `spike/chez-bootstrap`. Question: would re-hosting jolt's substrate from
Janet onto Chez Scheme (cisco/ChezScheme 10.4.1) buy speed, at what size/memory
cost? This spike does NOT port jolt-core/RT — it measures the **execution
substrate ceiling** by hand-translating the two compute-bound benches (fib,
mandelbrot) into the Scheme a jolt->Chez backend would emit, plus real
size/memory of the Chez runtime.

Machine: darwin arm64, M-series. Same caveat as the handoff doc — this dev box
swaps under load, so alloc-heavy absolute numbers inflate; compute benches
(fib/mandelbrot) are trustworthy. All runs isolated (no other CPU work).

## Speed (mean ms, 3 runs after warmup; same sizes as bench/run.sh)

| Bench                         | Janet jolt | Chez best | Speedup | Note |
|-------------------------------|-----------:|----------:|--------:|------|
| fib 30                        |      246.6 |       5.2 |  ~47x   | fixnum arith — immediate, unboxed |
| mandelbrot 200 (generic ops)  |      166.3 |      98.1 |  ~1.7x  | `+ - * >` box every flonum |
| mandelbrot 200 (flonum ops)   |      166.3 |      13.4 |  ~12.4x | `fl*/fl+/fl<` unboxed |

Correctness verified: fib 30 = 832040, mandelbrot 200 count = 3288753 (both
match jolt). optimize-level 2 vs 3 made no material difference here.

**The key finding** is the mandelbrot split. Generic Scheme arithmetic on floats
sends Chez through the numeric tower and **heap-boxes every flonum** — so the
naive emit gets almost nothing (~1.7x) and opt-level doesn't help. Emitting
flonum-specific ops (`fl+`/`fl*`/`fl<`, `fx` for the integer counter) lets Chez
keep flonums unboxed in registers and the same code drops to 13.4 ms.

13.4 ms ~= jolt's own JOLT_CGEN C-codegen result (12.4 ms, which already beat
JVM per docs/foundational-runtime-lever1-native-codegen.md). So **Chez's native
compiler reaches the hand-emitted-C ceiling on its own**, with no separate `cc`
step, no `.so` cache, no AOT manifest — just runtime compilation, REPL intact.

Implication for a real backend: the win is gated on the same type-inference ->
specialized-op lowering jolt ALREADY has (passes/types.clj feeds native-arith on
Janet today). fib's 47x is free (fixnums); mandelbrot's 12x needs that typed
path wired to `fl*` emission instead of (or alongside) the Janet/C path.

## Size (deployable footprint)

App code is negligible — fib compiled to a native object (`compile-program`,
optimize-level 3) is **2 KB**. The footprint is the Chez runtime:

| Artifact                                          |  Size   | vs Janet |
|---------------------------------------------------|--------:|---------:|
| Janet `build/jolt` (complete, jolt baked in)      | 2.21 MB |   1.0x   |
| Chez base, AOT (kernel + petite.boot + app)       | 2.89 MB |   1.3x   |
| Chez base, dynamic/REPL (+ scheme.boot compiler)  | 3.96 MB |   1.8x   |

components: libkernel.a 0.83 MB, petite.boot (runtime lib) 2.07 MB, scheme.boot
(compiler) 1.07 MB.

Caveat: the Chez rows are the runtime base ONLY. A complete jolt adds compiled
jolt-core (analyzer + clojure.core + persistent-collection RT) on top, which the
Janet 2.21 MB already includes. Estimated full Chez jolt ~4-6 MB. Still
single-digit MB, ~2-3x Janet, vastly under a JVM (40 MB+). petite.boot carries
much jolt won't use; a stripped custom boot file could shrink it.

## Memory (max RSS)

| Scenario                          | Janet   | Chez            |
|-----------------------------------|--------:|----------------:|
| startup / trivial                 | 12.5 MB | 32.1 (petite) / 49.5 (full) |
| mandelbrot 200                    | 20.8 MB | ~32 MB (AOT under petite)   |
| fib 30                            | 19.8 MB | 32.1 MB         |

Chez's baseline is flat across workloads (fib allocates ~nothing and doesn't
move it), so the ~32 MB (runtime) / ~49.5 MB (runtime + resident compiler) is
**fixed reservation**, not workload allocation. This is the one axis where Chez
is clearly worse: ~2.5x Janet's fixed footprint. Trades RAM for speed.
(Potentially tunable via Chez heap params / a stripped boot file; not explored.)

## Verdict

- **Speed: validated and large on compute** — 47x (fib) and 12.4x (mandelbrot),
  the latter matching jolt's C-codegen ceiling, **conditional** on the backend
  emitting typed/specialized numeric ops. Naive generic emit is nearly flat on
  floats. jolt's existing type passes are the lever that makes this real.
- **Chez could subsume the cgen path:** runtime native compile gets C-level
  numeric speed while keeping live redefinition — collapsing the
  interpret/compile/cgen-to-C hybrid into one native path.
- **Size: fine** (~1.3-1.8x base, ~2-3x full; single-digit MB).
- **Memory: the cost** (~2.5x fixed baseline).

## Phase 1 — real-pipeline measurement (2026-06-18)

The numbers above are hand-translated Scheme (the substrate ceiling). Phase 1
(jolt-cf1q.2) ran the SAME benches end to end through the real pipeline (Clojure
source -> Janet-hosted analyzer -> IR -> Scheme emitter -> Chez compile), timed
in-process (`test/chez/bench-pipeline.janet`, Chez startup excluded):

| bench               | real pipeline | ceiling (this run)        | gap = Phase 4 lever |
|---------------------|---------------|---------------------------|---------------------|
| fib 30 (flonum)     | 14.4 ms       | 7.1 ms hand-flonum        | 2.0x dispatch/var   |
| fib 30 (vs fixnum)  | 14.4 ms       | 5.2 ms fixnum             | all-double model    |
| mandelbrot 200      | 87.3 ms       | 98.1 ms generic-flonum    | AT/below ceiling    |
| mandelbrot 200 typed| 87.3 ms       | 13.4 ms typed fl*/fx*     | typed emit (Phase 4)|

Findings: (1) **compile-only is total** for the compute subset — every form
emits, no interpreter fallback (Chez has none). (2) Mandelbrot through the real
pipeline runs AT the generic-flonum ceiling (87 vs 98 ms) — the substrate ceiling
is reached end-to-end. (3) The fib residual is jolt's all-double number model
(the spike's 5.2 ms fib is fixnum); closing it to the 13.4 ms / fixnum ceiling is
the typed fl*/fx* emission Phase 4 owns. Eliding the redundant `jolt-truthy?`
wrapper on boolean-test `if`s (jolt-nkcb) cut fib 24.0 -> 14.4 ms.

## NOT yet measured (needs the RT port — the real project, not a spike)

- collections / binary-trees: these hit persistent collections + GC. Chez's GC
  is **generational** (vs Janet's non-generational mark-sweep), so binary-trees
  (jolt's worst axis, ~314x JVM) is exactly where Chez's GC should help most —
  but it requires porting the persistent-collection RT first. This is the next
  validation and the highest-uncertainty remaining question.
- Startup time (Janet jolt baked-image ~20ms; Chez boot-file load TBD).
- fiber/async layer (Janet fibers -> call/cc + threads rebuild).

## Repro

    cd spike/chez
    chez --script fib.ss 30 3
    chez --script mandelbrot.ss 200 3        # generic (boxed) — slow
    chez --script mandelbrot-fl.ss 200 3     # flonum-typed — the ceiling
