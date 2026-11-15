# jolt-3: cheapest call-site tracing without changing Chez

The control for a Chez trace primitive: how cheap can the emitter alone make
call-site tracing on the generated Clojure 200k project, keeping `JOLT_TRACE`
output the same? Measured with jolt-1's harness (`run.sh`, `volume.ss`,
`JOLT_PHASE_TIMES=1`) on `exp/chez-primitives` (90a64c41) plus this pellet's
commits. Machine, project and checksum (374878) as in `jolt-1-harness.md`.

## What tracing costs today

With tracing on, a tail call that can run a jolt fn stores its static
`('ns/fn' . line)` pair in virtual register 2 (`jolt-site!`), and every def
registers its call sites (`jolt-register-callsite!`). Since R2 (jolt-knn8)
nothing is recorded at a non-tail call; it carries a **restore** instead: save
vregs 2 and 12, move a pair in 2 to 12, clear 2, make the call, put both back.
Without it a returned tail call's pair stays in the slot and a later host-side
throw reports the returned fn as its innermost frame. Inside a fn the restore is
spelled once per clause as a local `jolt-site-call` helper and each site is
`(jolt-site-call (lambda () call))`; outside a fn it is spelled inline (#1423).

On the 200k project that is 161,183 restores, 17,565 per-clause helpers, 81,136
registrations and 8.7k tail stores. Turning tracing off saves 12.3 s of the
42.8 s cold build.

### Does cp0 inline the per-clause helper? Not at nested sites

`session.clj` said cp0 inlines the helper and the thunk at every site. It does
not. Over the first 40 namespaces of the cache (`expand/optimize`, counting
identifiers by name), 3,073 of 13,875 `jolt-site-call` references survive cp0, in
352 of 2,570 top-level forms. A synthetic nest `(site (f (site (f ... x))))`
inlines through depth 3 and leaves closures and out-of-line calls at depth 5. This
agrees with jolt-2's count (3,061 over the same files). My first check reported 0
survivors because it compared symbols with `eq?`, which misses the renamed
identifiers in the expander's output. The comment in `session.clj` is corrected.

## Variants

All are off by default. `JOLT_TRACE_EMIT=<mode>` is read by
`jolt-enable-trace!` (compile-eval.ss) and respells only a non-tail call's
restore (`calls.clj` `site-restore`). Unset, the emitted Scheme is byte-identical
to the base: all 521 `.scm` files of the 200k cold build have the same md5 as the
base binary's.

| mode | a non-tail site becomes | trace-identical |
|---|---|---|
| default | per-clause helper + thunk inside fns, the inline restore outside | (reference) |
| **V1** `global` | `(jolt-site-call (lambda () call))` everywhere, calling one top-level procedure in rt.ss | yes |
| **V2** `macro` | `(jolt-site-inline call)`, an rt.ss macro that expands to the inline restore, no thunk | yes |
| V2b `inline` | the inline restore text everywhere (the pre-#1423 shape) | yes |
| **V3** `norestore` | `call`, with no restore (tail stores and registrations unchanged) | **no** |
| off | `JOLT_TRACE=0` | n/a |

## Results

### 200k project, `run.sh` (cold median of 3, warm median of 5, one config at a time)

| | cold stock | cold patched | warm stock | warm patched | emitted, non-ws bytes | `.so` (stock) |
|---|---|---|---|---|---|---|
| default | 42.75 s | 40.80 s | 2.79 s | 2.89 s | 65,882,015 | 124,303,893 |
| V1 global | 40.69 s | 40.60 s | 2.74 s | 2.80 s | 58,016,100 (−11.9%) | 124,702,973 (+0.3%) |
| V2 macro | 42.76 s | 42.33 s | 2.77 s | 2.54 s | 56,738,500 (−13.9%) | 121,495,264 (−2.3%) |
| V2b inline | 42.43 s | 44.11 s | 2.90 s | 2.99 s | 93,594,946 (+42.1%) | 128,556,577 (+3.4%) |
| V3 norestore | 33.77 s | 34.47 s | 2.41 s | 2.46 s | 53,863,900 (−18.2%) | 112,027,090 (−9.9%) |
| off | 30.46 s | 32.86 s | 2.06 s | 2.30 s | 44,901,212 (−31.8%) | 106,147,275 (−14.6%) |

Checksum 374878 in every run. The patched rows ran while another worker's
compile measurements were running and spread more (default cold 39.9 to 42.2 s,
warm 2.59 to 3.08 s), so the four trace-identical modes were run again
**interleaved** (one run of each mode in turn, both Chez builds, same binaries).
This is the comparison to trust:

| interleaved | cold stock | cold patched | warm stock | warm patched |
|---|---|---|---|---|
| default | 43.53 s (41.84 43.76 43.53) | 43.44 s (43.44 43.54 43.38) | 2.87 s | 2.92 s |
| V1 global | **41.45 s** (41.45 41.61 41.40) | **41.38 s** (41.38 41.33 41.51) | 2.82 s | 2.77 s |
| V2 macro | 43.70 s (43.57 43.70 43.78) | 43.59 s (43.53 43.73 43.59) | 2.90 s | 2.80 s |
| V2b inline | 45.56 s (45.56 45.11 45.56) | 45.67 s (45.67 45.04 46.00) | 3.15 s | 3.07 s |

Warm medians are of 5 interleaved rounds. The first round after the cold rounds
is 0.3 to 0.5 s slower for every mode (jolt-1 saw the same).

### Cold-build phases (stock, `JOLT_PHASE_TIMES=1`, one run each, ms)

| | analyze+emit | read | expand | compile | execute | total |
|---|---|---|---|---|---|---|
| default | 15,316 | 1,118 | 1,260 | 21,140 | 1,304 | 42,726 |
| V1 global | 15,329 | 984 | 1,237 | **19,279** | 1,304 | 40,543 |
| V2 macro | 15,017 | 981 | 1,451 | 20,824 | 1,271 | 41,973 |
| V2b inline | 15,179 | 1,512 | 1,586 | 20,199 | 1,226 | 42,431 |
| V3 norestore | 13,991 | 863 | 871 | 14,561 | 1,116 | 33,583 |
| off | 13,160 | 664 | 797 | 13,490 | 839 | 30,592 |

Patched, same order: compile 19,801 / 19,499 / 21,216 / 21,945 / 15,751 / 14,668.

The `.so` the cache keeps is compiled a second time, in the background worker
(`compile-file`). With `JOLT_AOT_ASYNC=0` it runs in-process and shows as
`aot-compile` (stock, one run each):

| | in-process compile | `.so` compile (aot-compile) | wall |
|---|---|---|---|
| default | 19,767 ms | 25,529 ms | 66.5 s |
| V1 global | 18,296 ms | 23,749 ms | 62.5 s |
| V2 macro | 19,785 ms | 25,849 ms | 66.9 s |
| V2b inline | 20,007 ms | 27,214 ms | 69.9 s |
| V3 norestore | 14,776 ms | 19,597 ms | 54.4 s |
| off | 13,647 ms | 18,074 ms | 49.7 s |

### Runtime with tracing on (`jolt -m <bench>`, fresh cache, median of 5 means, ms)

`fib` and `tak` recurse only through self-calls, which the emitter handles on a
separate path (`*self-wrap*`, the 3-argument `site-restore`) that no variant
touches. Their `tak` definition is identical in every mode up to gensym numbers.
`mfib` (below) is fib split across two fns, so every call is a non-tail call to
another fn and goes through the restore.

| | fib stock | fib patched | tak stock | tak patched | mfib stock | mfib patched |
|---|---|---|---|---|---|---|
| default | 23.1 | 23.0 | 7.0 | 7.0 | 70.4 | 70.3 |
| V1 global | 23.0 | 22.9 | 7.0 | 6.5 | **86.7 (+23%)** | **84.3 (+20%)** |
| V2 macro | 23.0 | 22.1 | 7.0 | 6.5 | 70.4 | 68.3 |
| V2b inline | 23.1 | 22.6 | 7.0 | 6.5 | 70.4 | 68.3 |
| V3 norestore | 23.0 | 22.6 | 7.0 | 6.6 | 66.2 | 64.3 |
| off | 19.3 | 19.0 | 7.0 | 7.0 | 70.7 | 70.4 |

The patched `tak` 6.5 against 7.0 did not reproduce: three interleaved runs each
of default and macro gave 7.0 ms on every first run and 6.5 to 6.9 on every
cached second run, for both modes.

```clojure
(ns mfib)
(declare fb)
(defn fa [n] (if (< n 2) n (+ (fb (- n 1)) (fb (- n 2)))))
(defn fb [n] (if (< n 2) n (+ (fa (- n 1)) (fa (- n 2)))))
;; -main as in bench/fib.clj: 2 warmups of (fa 26), then 3 timed (fa 32)
```

`JOLT_TRACE=0` does not make `mfib` faster (70.7 against 70.4 ms): untraced code
calls a fn value through `jolt-invoke1`, which wraps the traced `jolt-invoke-t1`
in `jolt-site-kept` (seq.ss), a save and restore of vreg 2 per call.

## Trace identity

- Default emission: byte-identical `.scm` on the whole 200k project.
- `make tracesmoke` (82 checks) and `make errorreport` (52 golden cases), run
  with `JOLT_BIN` set to this binary, `JOLT_TRACE_EMIT=<mode>` and a fresh
  `JOLT_CACHE_DIR` per mode: **all pass for global, macro and inline**.
  **norestore fails one**: `faultcatch: a failed validator starts at swap!'s
  caller` reports `app.faultcatch$names:7` as the innermost frame, a fn that
  had already returned. That is the stale slot the restore exists to clear.
- Hand-checked traces (a small project: a tail chain `top -> mid -> leaf ->
  kaboom`, a throw through `map`'s callback, a host throw right after a callee
  that tail-called, a host throw after a callback that tail-called): identical
  output in all five modes, norestore included, so these shapes are caught by the
  reporter's callsite validation and the failing one needs a validator.

## Conclusions

1. **V1 (one top-level procedure) is the only trace-identical variant that makes
   the build cheaper**: cold −2.1 s (−4.8%) on both Chez builds, the in-process
   compile −1.9 s and the `.so` compile −1.8 s, 12% less Scheme text. It costs
   at run time: every non-tail call allocates a closure and makes an out-of-line
   call, +23% on `mfib`. The 200k workload's warm run does not notice (it is
   mostly loading), a call-heavy program does.
2. **V2 (macro, no thunk) buys nothing in compile time** although it removes
   every surviving helper closure: cold +0.2 s, compile phase within 0.3 s,
   `.so` compile +0.3 s, expand +0.2 s. It does give 14% less text, a 2.3%
   smaller `.so` and the same run time as today. V2b (the inline text) is worst
   everywhere (+2 s cold, +42% text, +0.15 to 0.28 s warm). So the 35k closures that cp0
   leaves at nested sites (jolt-2) are not what tracing costs Chez to compile.
   The inline restore is: V1 compiles faster with roughly 160k more code
   objects, which suggests the backend's cost grows with the restore's code and
   temporaries in large fn bodies rather than with the number of procedures.
3. **V3: recording only where a frame can be lost is already the design.** Only
   tail calls record anything (8.7k stores), and non-tail calls keep their frame
   and their line. What non-tail calls pay for is the restore, which is not
   recording but slot hygiene. Removing it entirely is the ceiling for any
   restore work: cold −9.0 s (−21%, stock), compile −6.6 s, `.so` −10%,
   warm −0.38 s, `mfib` −6%, leaving 3.3 s (tail stores, 81k registrations,
   line markers) to `JOLT_TRACE=0`. It is not trace-identical (above), and no
   sound narrowing was found: the restore also clears the slot during the call,
   which matters for any callee that can throw, and callees that cannot throw or
   run a jolt fn (`no-jolt-call-procs`) are already left bare.
4. For the Chez primitive: the target is the 6.6 s (stock compile phase) between
   today and norestore, and an emitter-only change recovers at most 1.9 s of it
   (V1), at a run-time price. A primitive that Chez lowers to the register
   save/restore without adding temporaries live across the call in the caller's
   body is what would close the gap.

## Reproduce

```sh
E=/Users/admin/.cache/jolt-exp/jolt1          # inputs from jolt-1
PC=/Users/admin/.cache/jolt-chez-patched/10.4.1-p0001-0007/bin/chez
JOLT_CHEZ=/opt/homebrew/bin/chez /opt/homebrew/bin/chez --script host/chez/build-jolt.ss release target/release/jolt-v-stock
JOLT_CHEZ=$PC $PC --script host/chez/build-jolt.ss release target/release/jolt-v-patched
# per mode (global, macro, inline, norestore; JOLT_TRACE=0 for off; nothing for default)
JOLT_TRACE_EMIT=global COLD_N=3 WARM_N=5 EDIT_N=0 \
  sh benchmarks/chez-primitives/run.sh target/release/jolt-v-stock /opt/homebrew/bin/chez $E/clj200k - <work>
# in-process .so compile
(cd $E/clj200k && JOLT_TRACE_EMIT=global JOLT_AOT_ASYNC=0 JOLT_PHASE_TIMES=1 JOLT_CACHE_DIR=<fresh> \
  target/release/jolt-v-stock -m workload.main 42 3)
# trace identity
JOLT_TRACE_EMIT=global JOLT_CACHE_DIR=<fresh> JOLT_BIN=target/release/jolt-v-stock sh host/chez/trace-smoke.sh
JOLT_TRACE_EMIT=global JOLT_CACHE_DIR=<fresh> JOLT_BIN=target/release/jolt-v-stock sh host/chez/error-report-check.sh
```

`EDIT_N=0` skips the leaf and common edits; `run.sh` then prints empty leaf and
common rows flagged as checksum mismatches, which carry no data.
