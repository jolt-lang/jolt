# jolt-8: spell the tracing site restore so cp0 leaves no closures

jolt-2 found that with tracing on, cp0 does not inline the per-clause
`jolt-site-call` helper and its `(lambda () call)` thunk at nested sites, and
that tracing adds about 35k code objects (108k against 72k off). It recommended,
before any Chez primitive, spelling the restore so nothing is left to inline:
a runtime `define-syntax`, or the inline `let` the top-level sites already use.

## Both spellings already exist

jolt-3 built both as `JOLT_TRACE_EMIT` modes (`calls.clj` `site-restore`), off
by default with the default emission byte-identical:

- `macro`: `(jolt-site-inline call)`, an `rt.ss` `define-syntax` that expands
  to the inline restore, no helper and no thunk;
- `inline`: the inline restore text at every site (the pre-#1423 shape).

Those are exactly the two spellings this pellet names. jolt-4 added the
remaining ones (a Chez primitive, continuation attachments), and no other
spelling of the same restore is left to try, so nothing new was built here. jolt-3 and
jolt-4 measured these modes with `run.sh`, phases and `site-compile.ss`, but not
with jolt-2's `measure.ss`, nor did they count code objects. This pellet adds that
measurement against jolt-2's rows, from the existing modes.

## Setup

`exp/chez-primitives` at ad2b0852, Apple Silicon. Two binaries built from it
with `build-jolt.ss release`: stock Homebrew Chez 10.4.1 and the baseline
patched Chez (`10.4.1-p0001-0007`). The generated Clojure 200k project
(`jolt-exp/jolt1/clj200k`). One cold cache per mode with the stock binary
(default, `macro`, `inline`, `JOLT_TRACE=0`), checksum 374878 each. The default
cache's 521 `.scm` files are byte-identical to jolt-2's orig cache (the same md5
over all of them), so orig below is jolt-2's orig. A is jolt-2's `rewrite.ss A`
over that orig.

`measure.ss` (`compile-file` of all 521 files, Jolt's runtime loaded, inspector
information on, as the AOT cache does). Three interleaved rounds: in each round
every variant once on stock, then every variant once on patched. One run of
stock `inline` in round 1 wrote no result line. Its other runs all exited 0
with no failed file, so stock `inline` is the 2 remaining runs. A later
out-of-round stock pair overlapped a peer's test suite (orig real 34.6 s, CPU
30.1 s), so it is not in the table. Its CPU ratio, inline over orig (+4.0%),
agrees with the table. Real and CPU time agreed within 0.3% on every run in the
table.

## Results

### `compile-file`, the whole project (`measure.ss`, median of 3, ms)

| variant | stock | vs orig | patched | vs orig | `.so` bytes | vs orig | code objects |
|---|---|---|---|---|---|---|---|
| orig (default emission) | 27,098 (27,223 26,939 27,098) | | 26,865 (26,594 26,865 27,104) | | 122,286,628 | | 107,906 |
| A (jolt-2: one opaque call per site) | 21,363 (21,363 21,380 21,329) | −21.2% | 21,241 (21,241 21,346 21,119) | −20.9% | 113,724,965 | −7.0% | 72,506 |
| **macro** (`define-syntax` restore) | **27,491** (27,318 27,555 27,491) | **+1.5%** | **27,254** (27,088 27,277 27,254) | **+1.4%** | 119,563,769 | −2.2% | **71,712** |
| **inline** (restore text at every site) | **28,256 to 28,582** (2 runs) | **+4.3 to +5.5%** | **28,217** (28,094 28,217 28,302) | **+5.0%** | 126,654,431 | +3.6% | **71,712** |
| T0 (`JOLT_TRACE=0`) | 18,781 (18,080 18,975 18,781) | −30.7% | 18,924 (18,924 18,942 18,854) | −29.6% | 104,270,807 | −14.7% | 72,215 |

jolt-2's rows for comparison: orig 28.98 s, A 23.17 s (−20.1%), T0 20.42 s
(−29.5%). This pass was about 1.9 s faster on orig. A and T0 keep jolt-2's
ratios within 1.2 points, so the comparison holds across passes.
`.so` sizes are identical on stock and patched (`cmp` equal per file). Patches
0001-0007 do not change the code the compiler emits for this input.
Code objects are the number of `assign-frame!` calls under
`CC_PASS_TIMING=1` (stock, one run each), the same count jolt-2 reported.

### Cold build (`jolt -m workload.main 42 3`, fresh cache, `JOLT_PHASE_TIMES=1`, median of 3, interleaved, ms)

| mode | stock wall | stock compile phase | patched wall | patched compile phase |
|---|---|---|---|---|
| default | 42,460 | 20,851 | 42,526 | 20,783 |
| macro | 42,321 | 20,893 | 42,284 | 20,802 |
| inline | 44,046 | 20,796 | 44,233 | 20,986 |
| off (`JOLT_TRACE=0`) | 32,720 | 14,188 | 33,332 | 14,363 |

Checksum 374878 in all 24 runs. Patched round 3 was slower for every mode
alike (+2.7 to +10.4 s over that mode's median), so no median comes from it. `inline` pays its extra
2 s in reading the text back (+0.45 s), expanding (+0.4 s), analyze+emit (+0.4 s)
and loading (+0.3 s), not in compile.

### Where the time goes (stock, `CC_PASS_TIMING=1`, CPU s)

| pass | orig | macro | inline | A | T0 |
|---|---|---|---|---|---|
| expand | 1.26 | 1.50 | 1.68 | 0.96 | 0.88 |
| cp0 | 1.91 | 1.98 | 1.99 | 1.26 | 1.16 |
| cptypes | 1.98 | 2.18 | 2.21 | 1.21 | 1.08 |
| pfasl (write `.so`) | 3.40 | 3.55 | 3.67 | 3.08 | 2.73 |
| np-generate-code | 1.62 | 1.51 | 1.53 | 1.35 | 1.20 |
| select-instructions! | 1.39 | 1.33 | 1.31 | 1.15 | 1.01 |
| all passes | 27.45 | 27.62 | 28.30 | 21.75 | 19.38 |

## Conclusions

1. **The spelling does what the pellet asked: no closures are left.** Under
   `macro` and `inline` the project compiles to 71,712 code objects, fewer than
   with tracing off (72,215), against 107,906 today. That removes all of the
   36k extra code objects. Trace output is unchanged: `tracesmoke` (82) and
   `errorreport` (52) pass in both modes on this base, as jolt-3 found.
2. **It buys no compile time.** `macro` compiles 1.4 to 1.5% slower than today
   on both Chez builds, and `inline` 4 to 5% slower. The cold build's compile
   phase is within 0.2 s of the default for both. Removing the closures saves
   about 0.3 s in the back end (code generation, instruction selection, frame
   and conflict passes), but expand (+0.24 s), cptypes (+0.20 s), the
   `.so` writer (+0.16 s) and cp0 (+0.07 s) spend more on the inlined restore than
   that. This repeats jolt-3's run.sh result (43.7 against 43.5 s cold) and
   jolt-4's in-process result (inline 28.6 s, against 25.3 s for the primitive).
3. **jolt-2's recommendation rested on a wrong attribution.** The 35k closures
   correlate with tracing's cost but do not cause it. A (−21%) and T0 (−30%)
   have the same code-object count as `macro`, which compiles as slowly as
   today. What costs compile time is the size of the restore that every pass
   has to carry (a `let`, an `if`, five vreg operations and two temporaries live
   across the call), not the number of procedures. Only a shorter spelling helps:
   an opaque call (A, jolt-3's `global`: faster to compile, +23% run time on
   call-heavy code), the open-coded primitive (jolt-4's patch 0008, the best
   trace-identical option), or no restore at all (not trace-identical).
4. **The only win is size.** `macro` writes 14% less Scheme (56.7 MB against 65.9 MB
   non-whitespace) and a 2.2% smaller `.so`, at the same run time as today
   (jolt-3's mfib). That is not enough to make it the default. Nothing changes in the
   emitter.

## Reproduce

```sh
E=/Users/admin/.cache/jolt-exp/jolt1/clj200k
P7=/Users/admin/.cache/jolt-chez-patched/10.4.1-p0001-0007/bin/chez
JOLT_CHEZ=/opt/homebrew/bin/chez /opt/homebrew/bin/chez --script host/chez/build-jolt.ss release target/release/jolt-stock
JOLT_CHEZ=$P7 $P7 --script host/chez/build-jolt.ss release target/release/jolt-p7
# one cold cache per mode (unset | JOLT_TRACE_EMIT=macro | =inline | JOLT_TRACE=0)
(cd $E && JOLT_TRACE_EMIT=macro JOLT_PHASE_TIMES=1 JOLT_CACHE_DIR=<cache-macro> \
  <worktree>/target/release/jolt-stock -m workload.main 42 3)
# A from the default cache's .scm
chez --script benchmarks/chez-primitives/compact-ceiling/rewrite.ss A <work>/A <work>/orig/*.scm
# one measurement (interleave variants and builds by hand); CC_PASS_TIMING=1 for the pass table
chez --script benchmarks/chez-primitives/compact-ceiling/measure.ss macro <work>/macro <work>/so-macro '#t' 1
# trace identity
JOLT_TRACE_EMIT=macro JOLT_CACHE_DIR=<fresh> JOLT_BIN=target/release/jolt-stock sh host/chez/trace-smoke.sh
JOLT_TRACE_EMIT=macro JOLT_CACHE_DIR=<fresh> JOLT_BIN=target/release/jolt-stock sh host/chez/error-report-check.sh
```
