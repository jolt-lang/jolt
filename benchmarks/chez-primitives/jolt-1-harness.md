# jolt-1: measurement harness and baselines

Baselines for the Chez-primitives experiments (pellet group
`chez-primitives-experiments`), measured with the harness in this directory on
branch `exp/chez-primitives` at 7ed36439 plus this pellet's commits. The
deftype fix is not on this base, so these numbers are the ones to compare
against, not the group context's (jolt 95925e30, different machine state).

Machine: Apple M3 Ultra (virtual), 24 cores, 32 GB, macOS 15 (Darwin 24.6).
Every number below is wall time from one sequential run of each configuration,
with nothing else running in that worker.

## Baseline table

Clojure 200k = `generate.bb --seed 42 --lines 200k --namespace-lines 400`
(199,111 lines, 547 files). Scheme 200k = chez-reference `gen.py` (seed 42,
198,032 lines, 433 libraries). Workload arguments `42 3`. Cold and common are
medians of 3 runs, warm of 5, leaf of 3. Checksums were identical in every run:
374878 for the Clojure project, 838106 for the Scheme one.

| | Jolt, stock Chez | Jolt, patched Chez | Chez ref, stock | Chez ref, patched |
|---|---|---|---|---|
| Cold build, writing the cache | 43.90 s | 44.29 s | 8.73 s | 8.54 s |
| Cold, `JOLT_TRACE=0` | 33.85 s | 33.32 s | | |
| Warm run | 2.85 s | 2.85 s | 0.32 s | 0.32 s |
| Warm, `JOLT_TRACE=0` | 2.32 s | 2.27 s | | |
| Edit a leaf namespace | 2.92 s | 2.86 s | 0.32 s | 0.32 s |
| Leaf, `JOLT_TRACE=0` | 2.40 s | 2.34 s | | |
| Edit the common namespace | 43.90 s | 43.18 s | 8.66 s | 8.60 s |
| Common, `JOLT_TRACE=0` | 32.66 s | 34.37 s | | |
| Source, non-whitespace bytes | 4,307,945 (4.1 MiB) | same | 4,285,945 (4.1 MiB) | same |
| Scheme handed to Chez, non-whitespace | 65,882,015 (62.8 MiB) | same | 4,285,945 | same |
| ... with `JOLT_TRACE=0` | 44,901,212 (42.8 MiB) | same | | |
| Compiled `.so` | 124,029,087 (118.3 MiB) | 124,081,533 (118.3 MiB) | 16,726,593 (16.0 MiB) | 16,727,077 (16.0 MiB) |
| ... with `JOLT_TRACE=0` | 106,212,017 (101.3 MiB) | 106,264,501 (101.3 MiB) | | |

Individual runs (s), in order:

| | stock | patched |
|---|---|---|
| clj cold | 43.90 43.74 43.97 | 44.29 44.33 43.88 |
| clj warm | 3.23 2.85 2.85 2.84 2.85 | 3.17 2.85 2.81 2.82 2.86 |
| clj leaf | 2.92 2.92 2.92 | 2.92 2.86 2.83 |
| clj common | 43.63 43.90 44.00 | 43.20 43.14 43.18 |
| clj cold, trace off | 33.94 33.62 33.85 | 33.27 33.83 33.32 |
| clj warm, trace off | 2.82 2.33 2.32 2.30 2.30 | 2.58 2.27 2.22 2.30 2.24 |
| clj leaf, trace off | 2.40 2.40 2.44 | 2.31 2.34 2.45 |
| clj common, trace off | 33.16 32.62 32.66 | 34.37 35.97 32.51 |
| scm cold | 8.59 8.78 8.73 | 8.60 8.54 8.53 |
| scm warm | 0.32 0.32 0.32 0.32 0.32 | 0.33 0.32 0.31 0.32 0.31 |
| scm leaf | 0.32 0.32 0.33 | 0.32 0.31 0.32 |
| scm common | 8.65 8.66 8.68 | 8.65 8.57 8.60 |

The leaf edits went to `workload.breraho` and `(workload breiplimfer)`: the
first worker, by name, that no other worker requires.

### Stock against patched Chez

- **No difference in time** beyond run-to-run spread: cold 43.9 against 44.3 s,
  warm 2.85 s on both, common 43.9 against 43.2 s, and the Scheme reference the
  same within 0.2 s. The one wide row is patched common with tracing off
  (32.5 to 36.0 s); its median is the only place patched looks slower, and its
  fastest run is the fastest of all four common series.
- **No difference in `.so` size**: patched is 0.04% larger (52 KB of 124 MB) for
  jolt and 484 bytes larger for the reference. Patch 0004
  shares equal string literals only in `s/vfasl.ss`, that is when fasl entries
  are combined into a vfasl image (the boot files a jolt build embeds). The AOT
  cache's `.so` files come from `compile-file`, which does not go through that
  path, so they keep one copy of each literal per top-level form on both builds.
  The emitted Scheme is byte-identical on both (same jolt sources).

### Things worth knowing when reading later numbers

- The first warm run after a cold build is 0.3 to 0.5 s slower than the four
  after it, every time, on both builds, tracing on or off. It writes nothing
  to the cache except the generation's `.used` timestamp (checked by
  diffing `stat` of every cache file around that run) and its user CPU equals
  the later runs' (2.66 against 2.62 s); the extra is system time and wall
  time. Medians of 5 hide it; a 3-run median would not always.
- `+cache` (the time until jolt's background `.so` worker has drained) is
  only 0.1 to 0.15 s after the run on every row: the worker compiles each
  namespace while the parent is still analyzing the next ones, and only the
  last few (workload.main, the gN group namespaces) are left when the parent
  exits. So the cold column is the full cost of writing the cache, not just
  the in-process half.
- A leaf edit costs 0.07 s over warm (2.92 against 2.85 s): the leaf, its group
  namespace and workload.main recompile. A common edit recompiles everything and
  costs what a cold build does.
- The Scheme reference's leaf edit is indistinguishable from its warm run.

## Cold-build phases (JOLT_PHASE_TIMES=1)

One cold run per configuration, exclusive times in ms (count = times the phase
was entered). The phases sum to `total`, which runs from the runtime's env init
to exit; the run's wall time is about 0.1 s more (process start).

| Phase | stock | patched | stock, trace off | patched, trace off |
|---|---|---|---|---|
| analyze+emit | 16,299 | 15,878 | 14,481 | 14,233 |
| read (emitted text back) | 1,274 | 1,169 | 744 | 728 |
| expand (Chez) | 1,377 | 1,308 | 886 | 905 |
| compile (Chez) | 21,858 | 21,493 | 14,551 | 14,414 |
| execute | 1,361 | 1,323 | 919 | 904 |
| clj-read (Clojure source) | 544 | 502 | 476 | 457 |
| loader | 1,402 | 1,423 | 631 | 614 |
| cache-load | 8 | 8 | 8 | 8 |
| aot-write (.scm + sidecars) | 771 | 729 | 847 | 593 |
| aot-compile (in-process .so) | 0 | 0 | 0 | 0 |
| other | 80 | 78 | 72 | 72 |
| **total** | **44,975** | **43,909** | **33,613** | **32,928** |
| run wall time | 45.09 s | 44.02 s | 33.71 s | 33.03 s |

29,921 forms are analyzed and compiled; 14,267 top-level forms read from 531
source files. `execute` is entered for 28,093 forms with tracing on and 20,847
with it off: Chez runs the rest through its interpreter shortcut
(`compile-interpret-simple`) inside `compile`. `aot-compile` is 0 because the
`.so` compile happens in the background worker (`JOLT_AOT_ASYNC=0` moves it
in-process and into that row). The phased run is within 1.2 s of the unphased
cold median on every configuration, so the instrumentation's own cost is
small.

These agree with the group context's reverted instrumentation on the shape
(compile, then analyze+emit, dominate; read, expand and execute are 1 to 1.6 s
each) and read 1 to 1.6 s lower on compile.

## Code volume

From `volume.ss` on the last cold run's cache (stock; patched emits the same
text). Group-context counts reproduced exactly: 161,183 site-call wrappers,
76,118 `virtual-register` + 142,649 `set-virtual-register!` (218,767), 81,136
`jolt-register-callsite!`, 106,424 `keyword`, 34,337 `jolt-symbol`, 20,252
`image-fn-form-src`, 101,882 `var-cell-deref`, 81,874 `jolt-var`; with tracing
off, 41,886 `jolt-invoke1` + 29,405 `jolt-invoke2` (71k).

Emitted Scheme by top form, non-whitespace bytes:

| Top form | count | tracing on | share | `JOLT_TRACE=0` | share |
|---|---|---|---|---|---|
| `(begin (image-register-fn-form! ...) (def-var-with-meta! ...))`, a def holding a fn | 11,044 | 48,145,147 | 73.1% | 33,409,121 | 74.4% |
| `let`, a top-level call (tracing wraps every one) | 10,686 / 3,440 | 12,301,414 | 18.7% | 5,651,192 | 12.6% |
| `def-var-with-meta!`, a def of anything else | 6,364 | 4,215,153 | 6.4% | 3,631,059 | 8.1% |
| `aot-replay-def-ordinals!` (one per namespace) | 521 | 1,023,702 | 1.6% | 1,023,702 | 2.3% |
| top-level `jolt-invoke1/2/4`/`jolt-invoke` calls | | | | 1,039,524 | 2.3% |
| everything else | | 196,599 | 0.3% | 146,614 | 0.3% |

Source by top form: `defn` 44.5%, `defrecord` 11.9%, qualified macro calls 11.1%,
`defn-` 7.5%, `extend-type` 6.6%, `extend-protocol` 5.1%, `defmethod` 5.0%,
the rest under 2.5% each. The Scheme reference: `define` 56.0%,
`extend-generic!` 23.7%, `add-method!` 5.3%.

## Regenerating

### Patched Chez (shared, do not modify)

Installed at **`/Users/admin/.cache/jolt-chez-patched/10.4.1-p0001-0007`**
(binary `bin/chez`). Built from Chez Scheme v10.4.1 with
host/chez/chez-patches/0001 to 0007 applied, exactly as follows (the source tree
was deleted afterwards; any scratch directory works for it):

```sh
P=/Users/admin/.cache/jolt-chez-patched/10.4.1-p0001-0007
SRC=<any scratch directory>
git clone --depth 1 --branch v10.4.1 --recurse-submodules \
  https://github.com/cisco/ChezScheme.git "$SRC"
sh ci/apply-chez-patches.sh "$SRC"          # from the jolt checkout
cd "$SRC" && ./configure --installprefix="$P" --threads --disable-x11 \
  && make -j8 && make install
printf '#!/bin/sh\nexec %s/bin/scheme "$@"\n' "$P" > "$P/bin/chez"; chmod +x "$P/bin/chez"
```

A new patch gets its own prefix built the same way; this one stays the baseline.

### Jolt binaries

`JOLT_CHEZ` must name the same Chez that runs the build: build-jolt.ss takes the
boot files and kernel it embeds from `JOLT_CHEZ`'s install, falling back to
whichever `chez` is first on PATH. (`make jolt-release CHEZ=...` exports it.)
Running the patched Chez without it builds a binary with stock boots.

```sh
JOLT_CHEZ=/opt/homebrew/bin/chez /opt/homebrew/bin/chez \
  --script host/chez/build-jolt.ss release target/release/jolt-stock
P=/Users/admin/.cache/jolt-chez-patched/10.4.1-p0001-0007/bin/chez
JOLT_CHEZ=$P $P --script host/chez/build-jolt.ss release target/release/jolt-patched
```

### Inputs (outside the repository)

```sh
E=/Users/admin/.cache/jolt-exp/jolt1
git archive origin/gen-research benchmarks/generated-project | tar -x -C $E
bb $E/benchmarks/generated-project/generate.bb $E/clj200k --seed 42 --lines 200k --namespace-lines 400
python3 -I /Users/admin/src/jolt/benchmarks/chez-reference/gen.py $E/scm200k
```

### The runs

```sh
PC=/Users/admin/.cache/jolt-chez-patched/10.4.1-p0001-0007/bin/chez
sh benchmarks/chez-primitives/run.sh target/release/jolt-stock /opt/homebrew/bin/chez \
  $E/clj200k $E/scm200k $E/work-stock
sh benchmarks/chez-primitives/run.sh target/release/jolt-patched $PC \
  $E/clj200k $E/scm200k $E/work-patched
JOLT_TRACE=0 sh benchmarks/chez-primitives/run.sh target/release/jolt-stock /opt/homebrew/bin/chez \
  $E/clj200k - $E/work-stock-notrace
JOLT_TRACE=0 sh benchmarks/chez-primitives/run.sh target/release/jolt-patched $PC \
  $E/clj200k - $E/work-patched-notrace
```

Each prints the table rows (stdout) and the volume reports and phase times
(stderr); the four together take about 25 minutes. `COLD_N`, `WARM_N`,
`EDIT_N`, `PHASES=0` and `VOLUME=0` shorten a run; "-" for either project skips
it. Code volume on its own:

```sh
chez --script benchmarks/chez-primitives/volume.ss --clj $E/clj200k/src \
  --cache <JOLT_CACHE_DIR> --scm $E/scm200k/src --so <reference obj dir>
```

Phase times on any jolt run: `JOLT_PHASE_TIMES=1 jolt ...` prints the table
above on stderr at exit.

## What was added

- `host/chez/compile-eval.ss`: `JOLT_PHASE_TIMES`. One clock charges the
  running phase and every phase entry interrupts and later resumes its parent,
  so a nested `require` lands in its own phases instead of being counted again
  in its requirer's `execute`. `expand` and `execute` come from wrapping Chez's
  `current-expand` and `$compile-backend` (installed only when the variable is
  set, and charging only inside jolt's `compile` phase). Main thread only. Off,
  each site is one flag test and a closure; enabled from
  `jolt-trace-init-from-env!`, which both entrypoints call at run time.
- `host/chez/loader.ss`: the loader's phases (clj-read, loader, cache-load,
  aot-write, aot-compile), through `ldr-phase`, which is a plain call in a
  no-eval app built without compile-eval.ss.
- `benchmarks/chez-primitives/run.sh`, `volume.ss`: the harness above.
