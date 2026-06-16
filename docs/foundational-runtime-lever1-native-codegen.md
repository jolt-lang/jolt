# Lever 1 — Native codegen (jolt-IR → C): feasibility spike

**Epic:** jolt-5vsp · **Date:** 2026-06-16
**Predecessor:** the localization spike (`docs/foundational-runtime-spike-results.md`)
showed the 15.4× mandelbrot floor is ~70% Janet-VM floor (only native codegen
moves it) + ~30% loop-lowering (cheap backend fix, jolt-v28u). This spike probes
**lever 1's ceiling and the incremental hot-fn-in-C strategy** before committing
to a backend.

All legs return the identical result (3288753 at n=200). Numbers are means of 3
after warmup; the dev machine swaps, so treat these as orders-of-magnitude (the
≈ vs JVM call is robust; ±2ms is noise).

## The native-C ceiling — it beats JVM

Native mandelbrot built as a Janet native module (`spike/native/mandel.c`):

| Leg | mean | vs jolt (219ms) | vs JVM (14.2ms) |
|---|---|---|---|
| **native-C whole run** (pure C, no Janet in loop) | **~10–12 ms** | **~18–22× faster** | **faster than JVM** |
| Janet loop → C hot-fn (forward crossing) | ~11–13 ms | ~18× faster | ≈ JVM |
| C loop → `janet_call` bytecode (reverse crossing) | ~152 ms | ~no better | ~11× slower |
| *(reference)* jolt-compiled | 219 ms | — | 15.4× |
| *(reference)* JVM Clojure | 14.2 ms | — | 1.0× |

**Verdict: lever 1 is validated and its ceiling is excellent.** Compiling the hot
compute path to C makes it ~18–22× faster than today's jolt and *edges out JVM
Clojure* — native code has no VM-dispatch floor at all. This is the only lever
that touches the ~10.8× Janet-VM floor, and the payoff is the full gap.

## The crossing-direction rule (the key strategic finding)

The boundary cost is wildly asymmetric:

- **Forward (bytecode → C): nearly free.** A Janet bytecode loop calling a C
  hot-fn n² (=40 000) times runs at ~11–13 ms — within ~15% of pure C. So you can
  compile just the *inner* hot fn to C and capture ~95% of the win while the outer
  loop stays bytecode. **Incremental adoption works.**
- **Reverse (C → `janet_call` → bytecode): ~3.5 µs/call.** A C fn calling a
  bytecode helper per iteration runs at ~152 ms — *no better than jolt today*. The
  `janet_call` cost (entering the VM/fiber per call) dominates.

**Design constraint → compile leaf-first / whole-hot-cluster.** A fn is a
profitable C-compilation candidate only if its hot path calls **nothing that stays
in bytecode** — only primitives or other C-compiled fns. Cross the boundary only at
*cold* edges. For mandelbrot, `count-point` is a leaf (calls only arithmetic
primitives) → the ideal first target; compiling it alone captures the win
(forward crossing), but a half-compiled hybrid that `janet_call`s back per
iteration buys nothing.

## The dynamic-compile path works (no jpm needed)

jolt's compile model is dynamic (analyze → IR → Janet → eval at runtime). Native
codegen fits the same shape: a `.so` compiled with a **plain `cc` invocation**
(no jpm/project.janet) loads at runtime via `require` and runs at full native
speed (verified: `run-c(200)` correct, 13.5 ms cold).

```
cc -shared -fPIC -O2 -I/opt/homebrew/include -undefined dynamic_lookup \
   mandel.c -o mandel.so          # macOS; Linux drops -undefined dynamic_lookup
(require "path/to/mandel")          # loads at runtime, cfunctions callable
```

So the native tier mirrors today's interpret/compile hybrid: emit C for a hot
fn → shell to `cc` → `require` the `.so` → bytecode callers call into it via the
(cheap, forward) native-module call path. Caching keyed by fn-source-hash mirrors
the existing ctx image cache.

## Toolchain confirmed (this machine)

- `janet.h` present (`/opt/homebrew/include/janet.h`, Janet 1.41.2).
- `jpm declare-native` builds a `.so` cleanly.
- Direct `cc` (no jpm) builds a loadable `.so`.
- C API used: `janet_getnumber/getinteger`, `janet_wrap_number`, `janet_fixarity`,
  `janet_getfunction`, `janet_call`, `janet_cfuns`, `JANET_MODULE_ENTRY`.

## Status: wired into the compile path (JOLT_CGEN, opt-in)

`src/jolt/cgen.janet` (IR→C translator) is wired into the backend's `:def` emit
via `cgen-root`, gated behind **`JOLT_CGEN=1`** (off by default; needs
direct-linking). When on, a plain defn of a numeric-leaf fn is compiled to C at
def time and the cfunction installed as the var root — so direct-linked callers
embed native code. The fn is NOT inline-stashed when cgen fires (callers must
call the C fn, not inline the bytecode body). `^:redef`/`^:dynamic` defns stay
bytecode.

The leaf-first rule emerges for free: `run` calls `count-point` (a user var, not
a native-op), so `run` isn't a numeric leaf and stays bytecode — calling the
native `count-point` over the cheap forward crossing.

**Measured end-to-end (`jolt -m mandelbrot 200`): 224 ms → 12.4 ms, ~18×**, with
the correct result — matching the spike's native-C ceiling. The default gate
(cgen off) is unchanged. Tests: `test/integration/cgen-pipeline-test.janet`.

Known limitation: building *core* with `JOLT_CGEN=1` would try to cgen core
numeric-leaf fns into the cached ctx image, where embedded cfunctions may not
serialize — keep cgen for app/user code until image-cache interaction is handled.

## Build-time AOT: native speed without a toolchain on the target (jolt-a7ds)

The JIT path above runs `cc` at runtime. The AOT path moves compilation to build
time so the deploy target needs no `cc`/`janet.h`:

- **Build phase** (`:cgen-collect?`, needs cc): loading the app records every
  numeric-leaf defn's IR; `cgen/aot-build` compiles them all into ONE native
  module (`gen-c-module`) and `write-manifest` persists `{sopath, [{ns name sym}]}`.
- **Deploy phase** (`:cgen-prebuilt`, NO cc): `cgen/load-aot` loads the prebuilt
  `.so` (via the `native` builtin — no compiler) into a qname→cfunction map; the
  backend's `:def` hook installs each as the var root with the same timing as the
  JIT path, so callers direct-link to native code.

**Proven** (`spike/native/aot-demo.janet`, two processes): build with cc, then
deploy with `cc` removed from PATH → `count-point` is still native, mandelbrot =
3288753 at **12.4 ms** (full 18×). Test: `test/integration/cgen-aot-test.janet`.

This removes the runtime-toolchain dependency — the core of the deployment story.

### The literal single binary (`jolt cgen-build`, done)

`src/jolt/cgen_build.janet` + the `jolt cgen-build -m NS -o OUT` CLI fuse the
native code into the executable, so an app ships as ONE static file — no sidecar
`.so`, no toolchain to run. The driver:

1. loads the app with `:cgen-collect?` to get its numeric-leaf fns + the source
   files loaded (the uberscript-style bundle);
2. emits `cg.c` (one native module of those fns via `cgen/gen-c-module`) + a
   positional manifest;
3. stages a build dir: `src`/`jolt-core` symlinks into the jolt tree, `cg.c`, the
   app bundle, and an entry that bakes the runtime, installs the native fns as var
   roots (`:cgen-prebuilt`), and runs `-main`;
4. runs `jpm build` there — `declare-native` builds `cg.a`, `declare-executable`
   static-links it into the final exe (jpm's `create-executable` marshals the
   module's cfunctions and calls its static entry at startup).

Build needs `cc` + `jpm`; the result needs neither. Proven end-to-end:
`test/integration/cgen-build-test.janet` builds the mandelbrot fixture, runs it
from a clean dir with no `src/` and no `cg.so`, and gets the right total at native
speed (the count-point leaf is the linked cfunction).

Build mechanics that bit (codified in `cgen_build.janet`): `stdlib_embed` slurps
`.clj` relative to cwd, so the build runs in a dir mirroring the repo layout (the
symlinks); jpm hardcodes `./project.janet` and sets `syspath = modpath`; the
executable's dofile imports `cg` and static-links `cg.a`, neither ordered nor
release-built by default, so the deps are wired explicitly; cleanup must `lstat`
(never follow the tree symlinks). The inner build runs `--workers=1` so it doesn't
saturate cores inside the parallel test gate.

Open follow-ups (filed): widen the cgen grammar (jolt-l1l4) so more of an app's
hot fns qualify; hot-fn auto-detection (jolt-qx70) to drop the manual collect;
DCE on the bundled source (the uberscript path already does it).

## Open questions for the implementation (next beads)

1. **IR→C for the numeric subset.** Translate jolt IR → C for proven-double
   arithmetic + tail `loop`/`recur` (count-point's shape). The native-arith type
   proof (jolt-3pl) that already gates native *Janet* arith is the same proof that
   gates C unboxing — reuse it. Start narrow: unbox doubles at entry, primitive
   ops inline, rebox at exit; bail to bytecode for any unsupported form.
2. **Boundary policy.** Non-primitive args stay Janet values (no unbox);
   per-iteration calls allowed only to other C-compiled fns. Encode the
   leaf-first/cluster rule as the compile-candidate predicate.
3. **Trigger + cache.** AOT at build/first-run vs lazy JIT on hot fns; `.so`
   cache keyed by source hash + flags (add to `ctx-shaping-env-vars` /
   image-cache machinery if it becomes a ctx knob).
4. **Coverage.** Closures/upvalues, multi-arity, `recur` across the C boundary,
   portability of `cc` flags per platform.

## Artifacts (`spike/native/`)

- `mandel.c` — native mandelbrot: `run-c` (pure C), `count-point-c` (leaf cfn),
  `run-callback` (C loop → `janet_call` back, the reverse-crossing probe)
- `project.janet` — `declare-native` build
- `bench-native.janet` — the three-leg benchmark + harness
