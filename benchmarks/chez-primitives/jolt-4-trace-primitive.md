# jolt-4: call-site tracing as a Chez primitive

Can a Chez change make the non-tail site restore cheaper to compile than any
emitter-only spelling (jolt-3), with traces unchanged? Two schemes were built
and measured against jolt-3's best emitter-only variant (V1 `global`), tracing
off and the baseline, on stock Chez, Jolt's patched Chez (0001-0007) and that
Chez plus this pellet's patch 0008. Generated Clojure 200k project, machine and
checksum (374878) as in `jolt-1-harness.md`; `exp/chez-primitives` 9cb0913c plus
this pellet's commits.

## What the restore is

With tracing on, a non-tail call that can run a jolt fn is wrapped in a restore
(jolt-3): save vregs 2 and 12, hand a site pair in 2 to 12, clear 2, call, put
both back. 161,183 such sites on the 200k project. Removing it (jolt-3's
`norestore`, not trace-identical) is the ceiling: 6.6 s of in-process compile.

## Step 1: do continuation attachments already provide it?

Chez 10.4.1 has Racket's continuation attachments
(`$call-setting-continuation-attachment` and friends, `s/cpnanopass.ss`
`np-recognize-attachment`). A non-tail `($call-setting-continuation-attachment
v (lambda () call))` compiles to a push (one pair allocated onto the thread's
attachment list) before the call and a pop folded into the return point, with
no temporaries live across the call, and Chez restores the list on any escape.

That is enough to express the restore exactly, with no Chez change, if the slot
moves from vreg 2 to the head of the attachment list (`JOLT_TRACE_EMIT=attach`):

- a non-tail site pushes a fresh 0 for its callee (`jolt-site-attach`);
- a tail site replaces the head's car (`jolt-site-set!`), so it never sets an
  attachment in tail position, which would reify the continuation;
- the old vreg 12 is the first pair below the head; vreg 2 is the slot again
  when the list is empty, which is all the default mode ever sees;
- every other reader and writer of vreg 2 (catch, self-call, `throw`
  outside a fn, `jolt-site-kept`, `seam-invoke`, `jolt-site-reset!`, the
  throw-time snapshot) goes through the same two helpers.

One thing attachments do not give: a host fault caught by a jolt `try` is
converted in the guard's handler, after the unwind has popped the pushes that
held its site, and a raise-time `condition-continuation` does not carry the
frame's own attachment either (checked in plain Chez: the list it keeps is the
one below the faulting frame). So in attach mode a try body runs under a
`with-exception-handler` that stashes the site at the raise, as the uncaught
path already does (`jolt-site-try-body`), and the fault's throwable takes that
site. Without it `tracesmoke` loses the erased frames of a caught host fault.

## Step 2: the primitive

Patch `host/chez/chez-patches/0008-jolt-site-call-primitive.patch` adds
`($jolt-site-call (lambda () call))`. `np-recognize-attachment`, the pass that
lowers attachments (after cp0, cptypes and cpletrec, before closure
conversion), open-codes it as exactly today's restore, so the vreg model, the
runtime and the traces are unchanged, but everything before that pass sees one
call where the inline restore has a `let`, an `if` and five vreg operations.
A non-literal thunk calls the library version (`s/prims.ss`).

| | files | lines |
|---|---|---|
| `s/cpnanopass.ss` | 1 clause in `np-recognize-attachment` | +23 |
| `s/primdata.ss` | the primitive's signature | +1 |
| `s/prims.ss` | out-of-line definition | +6 |
| total | 3 Scheme files, no C, nothing removed or changed | +30 (63-line patch) |

Jolt emits `(jolt-site-prim call)` under `JOLT_TRACE_EMIT=prim`;
`sa-jolt-site-call` in `scheme-adapter-runtime.ss` decides when the adapter is
compiled whether this Chez has the primitive (`($primitive $jolt-site-call)`
evaluates) and otherwise expands to the inline restore. Both modes are off
unless the variable names them; unset, the emission is byte-identical to the
base (md5 of a test namespace's `.scm` from the base binary and this one).

## Results

### Compile cost of the restore alone (in-process `compile-file`, `site-compile.ss`)

One cold cache built with `JOLT_TRACE_EMIT=macro` (every non-tail site is
`(jolt-site-inline call)`), its 521 `.scm` compiled with the macro redefined,
everything else fixed. Patched Chez with 0008, interleaved, median of 3:

| restore spelled as | real ms (runs) | `.so` bytes | of the M-N gap |
|---|---|---|---|
| M inline restore (today) | 28,641 (28,562 28,641 28,979) | 120,898,502 | |
| **P `$jolt-site-call`** | **25,289** (25,632 25,289 25,009) | 117,400,591 (-2.9%) | **-3,352 ms (49%)** |
| A attachment push | 25,059 (25,070 25,059 24,808) | 117,867,774 (-2.5%) | -3,582 ms (52%) |
| N no restore | 21,823 (21,827 21,645 21,823) | 111,370,953 (-7.9%) | |

Stock Chez, M/A/N only: 28,464 / 23,766 / 21,308 ms, the same order (A's share
is 66% there; the third round's `.so` came out 0.4% larger for all three
variants alike after `rt.ss` was edited mid-run, the first two are identical
to the patched sizes).

Where the time goes (Chez pass timing, one run each, s of CPU):

| pass | M | P | A | N |
|---|---|---|---|---|
| cptypes | 2.28 | 1.34 | 1.30 | 1.24 |
| cp0 | 2.06 | 1.37 | 1.41 | 1.31 |
| expand | 1.59 | 1.19 | 1.19 | 1.05 |
| pfasl (write `.so`) | 3.66 | 3.21 | 3.27 | 3.06 |
| cpletrec + cpvalid + cpnanopass | 0.98 | 0.70 | 0.71 | 0.62 |
| register allocation, conflicts, liveness, instruction selection, codegen | 7.01 | 7.08 | 7.23 | 5.71 |
| total | 28.73 | 25.63 | 25.14 | 21.84 |

The premise holds for the front end: the primitive removes nearly all of the
restore's cost in cp0, cptypes, cpletrec, the expander and the `.so` writer
(2.8 s). It removes none of the back end's 1.3 s, and neither does the
attachment push: a push plus pop costs the register allocator as much as two
saved temporaries (1.5 s). The rest, about 1.9 s, is spread thinly over the
other passes: what any per-site save and restore costs in Chez's back end.

### 200k project, `run.sh` (3 interleaved rounds; cold median of 3, warm median of 6)

| config | cold | warm | emitted non-ws bytes | `.so` |
|---|---|---|---|---|
| stock default (baseline) | 40.88 s (40.88 40.05 43.05) | 2.61 s | 65,882,015 | 124,304,091 |
| stock V1 global | 38.36 s (38.36 37.95 40.48) | 2.68 s | 58,016,100 | 124,703,154 |
| stock attach | 39.61 s (37.73 39.61 39.81) | 2.66 s | 56,774,076 | 119,488,987 |
| stock off | 32.47 s (30.87 32.77 32.47) | 2.11 s | 44,901,212 | 106,147,543 |
| p0007 default | 40.10 s (40.10 39.88 42.29) | 2.59 s | 65,882,015 | 124,218,855 |
| p0007 V1 global | 40.37 s (38.14 40.65 40.37) | 2.64 s | 58,016,100 | 124,617,863 |
| p0007 off | 32.32 s (30.32 33.07 32.32) | 2.17 s | 44,901,212 | 106,033,733 |
| p0008 default | 42.51 s (40.62 44.40 42.51) | 2.78 s | 65,882,015 | 124,218,857 |
| **p0008 prim** | **39.99 s** (38.98 40.82 39.99) | **2.55 s** | 56,419,100 (-14.4%) | 117,853,099 (-5.1%) |
| p0008 attach | 39.01 s (39.01 38.83 39.96) | 2.71 s | 56,774,076 | 119,403,726 |

Checksum 374878 in every run. Cold runs of one config spread up to 3 s
(another worker's suite ran through part of the window), which is as large as
the differences, so the phase and in-process numbers below are the ones to
compare. `p0008 default` is the same code as `p0007 default` (the patch only
adds a primitive), so its 2.4 s higher cold median is that spread. `prim` on a
Chez without 0008 expands to the inline restore, i.e. jolt-3's `macro`
(within 0.3 s of default there).

### Cold-build phases (`JOLT_PHASE_TIMES=1`, one run each, ms)

| config | analyze+emit | read | expand | compile | `.so` compile (`JOLT_AOT_ASYNC=0`) | wall, in-process `.so` |
|---|---|---|---|---|---|---|
| stock default | 15,410 | 1,122 | 1,222 | 21,081 | 25,715 | 66.7 s |
| stock V1 global | 15,122 | 982 | 1,085 | 19,649 | 23,949 | 62.8 s |
| stock attach | 15,229 | 974 | 1,187 | 18,816 | 22,797 | 61.0 s |
| stock off | 14,043 | 706 | 854 | 14,400 | 18,023 | 49.4 s |
| p0007 default | 15,040 | 1,092 | 1,212 | 20,556 | 24,931 | 64.6 s |
| p0007 V1 global | 14,561 | 936 | 1,056 | 18,492 | 23,430 | 61.3 s |
| p0007 off | 13,703 | 697 | 854 | 13,940 | 18,023 | 48.8 s |
| p0008 default | 14,819 | 1,051 | 1,201 | 20,231 | 25,194 | 65.1 s |
| **p0008 prim** | 14,966 | 949 | 1,163 | **18,741** | **22,847** | **60.8 s** |
| p0008 attach | 15,305 | 953 | 1,157 | 18,609 | 22,672 | 60.7 s |

Against the default on the same Chez, the primitive takes 1.5 s off the
in-process compile and 2.3 s off the `.so` compile (4.3 s of the in-process
wall); V1 takes 2.1 and 1.5 s on p0007, attachments 2.3 and 2.9 s on stock.
`JOLT_TRACE=0` is still 4.8 s of compile and 4.8 s of `.so` compile below the
primitive: the tail stores, the 81k registrations and the line markers.

### Run time with tracing on (`jolt -m <bench>`, fresh cache, median of 5 means, ms)

| config | fib | tak | mfib |
|---|---|---|---|
| stock default | 23.0 | 7.0 | 69.5 |
| stock V1 global | 23.0 | 7.0 | 85.8 (+23%) |
| stock attach | 23.8 | 7.0 | **96.1 (+38%)** |
| stock off | 19.1 | 7.0 | 69.5 |
| p0007 default | 23.0 | 7.0 | 69.7 |
| p0007 V1 global | 23.1 | 7.0 | 85.6 (+23%) |
| p0007 off | 19.1 | 7.0 | 69.6 |
| p0008 default | 23.0 | 7.0 | 69.6 |
| **p0008 prim** | 23.0 | 7.0 | **69.7** |
| p0008 attach | 22.8 | 7.0 | 94.8 (+36%) |

`fib` and `tak` only self-call, which no variant touches (jolt-3). `mfib` (two
fns calling each other, jolt-3's) makes every call a restored non-tail call:
the primitive costs nothing, V1 a closure and an out-of-line call per call,
and attachments a pair allocated per call.

## Trace identity

- Unset `JOLT_TRACE_EMIT`: `.scm` byte-identical to the base binary's;
  `traceemit` (95), `traceeval` (10), `degradedbacktrace` (6) pass.
- `tracesmoke` (82 checks) and `errorreport` (52 golden cases), each run with
  `JOLT_BIN` set to the binary, `JOLT_TRACE_EMIT` set and a fresh cache, all
  pass for: default (stock), prim on p0008 (the primitive), prim on stock (the
  fallback), attach on stock and attach on p0008. Attach passes only with the
  try-body raise-time stash above; without it one check fails
  (`host fault: printStackTrace reaches the faulting fn`).
- `make corpus` runs untraced (run-corpus.ss never enables tracing), so it
  checks the default path only; it is in the full gate run.

## Conclusions

1. **The primitive does what it was meant to, for the front end.** With
   `$jolt-site-call` the cp0/cptypes/cpletrec/expander share of the restore
   (2.8 s of the 6.8 s in-process gap) is gone, with the same vreg model, no
   runtime change and no run-time cost (mfib 69.7 against 69.6 ms). On the
   cold build it is worth 1.5 s of in-process compile and 2.3 s of `.so`
   compile, about what V1 `global` gets (2.1 and 1.5 s), without V1's +23% on
   call-heavy code. It is the best trace-identical option measured.
2. **Attachments suffice semantically and need no Chez change**, and they
   compile as cheaply as the primitive (52% of the gap in-process, 2.3 and
   2.9 s on the cold build), but every restored call allocates a pair: +36 to
   38% on mfib, worse than V1. They also need the raise-time stash for caught
   host faults. Not worth it over the primitive.
3. **The back end keeps half the gap.** Neither scheme moves the 1.3 s of
   register allocation, conflict graphs, liveness and instruction selection, or
   the 1.9 s over the other back-end passes, that a per-site save/restore
   costs; only not restoring does. Closing more of the
   6.6 s would need the restore to stop being per call site (jolt-3's
   `norestore`, not trace-identical) rather than a cheaper spelling of it.
4. **Invasiveness.** Patch 0008 is 30 added Scheme lines in 3 files of the
   compiler (no kernel change, nothing existing modified) and stays inert unless
   Jolt emits the primitive. Jolt's side is the `prim` spelling plus a
   build-time switch in the adapter; the attach mode touches every vreg-2
   reader and writer (rt.ss, host-faults.ss, host-static-classes.ss, the
   catch/throw/self-call emitters).

## Reproduce

```sh
E=/Users/admin/.cache/jolt-exp/jolt1/clj200k
P7=/Users/admin/.cache/jolt-chez-patched/10.4.1-p0001-0007/bin/chez
# patched Chez with 0008: clone v10.4.1, sh ci/apply-chez-patches.sh <src>, configure/make/install
P8=<prefix>/bin/chez
JOLT_CHEZ=/opt/homebrew/bin/chez /opt/homebrew/bin/chez --script host/chez/build-jolt.ss release target/release/jolt-v-stock
JOLT_CHEZ=$P7 $P7 --script host/chez/build-jolt.ss release target/release/jolt-v-p0007
JOLT_CHEZ=$P8 $P8 --script host/chez/build-jolt.ss release target/release/jolt-v-p0008
# one round of one config (rounds interleaved across configs; prim|attach|global, JOLT_TRACE=0 for off)
JOLT_TRACE_EMIT=prim COLD_N=1 WARM_N=2 EDIT_N=0 PHASES=0 \
  sh benchmarks/chez-primitives/run.sh target/release/jolt-v-p0008 $P8 $E - <work>
# phases, and the .so compile in-process
(cd $E && JOLT_TRACE_EMIT=prim JOLT_PHASE_TIMES=1 [JOLT_AOT_ASYNC=0] JOLT_CACHE_DIR=<fresh> \
  target/release/jolt-v-p0008 -m workload.main 42 3)
# compile cost of the restore alone, from a JOLT_TRACE_EMIT=macro cold cache's v1/
$P8 --script benchmarks/chez-primitives/site-compile.ss P <cache>/v1 <outdir>   # M P A N
# trace identity
JOLT_TRACE_EMIT=prim JOLT_CACHE_DIR=<fresh> JOLT_BIN=target/release/jolt-v-p0008 sh host/chez/trace-smoke.sh
JOLT_TRACE_EMIT=prim JOLT_CACHE_DIR=<fresh> JOLT_BIN=target/release/jolt-v-p0008 sh host/chez/error-report-check.sh
```
