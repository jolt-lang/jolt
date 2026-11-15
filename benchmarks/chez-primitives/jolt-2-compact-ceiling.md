# jolt-2: the compile-time ceiling of compact emitted code

Question: if Clojure-friendly Chez primitives let the emitter write compact
code, how much faster would Chez compile Jolt's output, and how much smaller
would the `.so` be? Answered offline, before building any primitive. A Scheme
rewriter turns the emitted `.scm` of a cold AOT cache into the compact shape,
and every variant is compiled the way the AOT cache compiles it.

## Setup

- Jolt at `7ed36439` (base `exp/chez-primitives`), `make build` against stock
  Homebrew Chez 10.4.1 (`/opt/homebrew/bin/chez`), Apple Silicon, 24 cores.
- Clojure 200k project: `generate.bb <out> --seed 42 --lines 200k --namespace-lines 400`,
  run with `jolt -m workload.main 42 3` (checksum 374878 on every cold and warm
  run below). Cold cache `JOLT_CACHE_DIR` with tracing on: 521 namespaces,
  521 `.scm`, 65.9 MB non-whitespace, 123.5 MB of `.so`.
- **Compile measurement: Chez with Jolt's runtime loaded.** `measure.ss`
  loads `host/chez/build-boot.ss`, the full runtime preamble build-jolt.ss
  compiles the stdlib under, so runtime macros (`image-fn-form-src` among them)
  expand as in a real build. It then runs `compile-file` on every `.scm`, as
  `aot-publish-artifact!` does, under the binary's compiler parameters (Chez
  defaults: optimize-level 0, inspector information on). That setting
  reproduces the cache's `.so` byte for byte when compiled from the cache's own
  path; from the copied path the project total is 122.3 MB instead of 123.5 MB,
  because inspector information records the source path. The
  `compile-variants.ss` fallback was not needed.
- Every time is the median of 5 runs, interleaved (run 1 of every variant,
  then run 2, and so on), one fresh Chez process per measurement. Real and CPU
  time agree within 2-3%. Run-to-run spread is up to ±10%. The orig variant was
  25.5 s in a first 3-run pass and 29.0 s in the interleaved 5-run pass an hour
  later, with CPU time tracking real time both times, so the single-thread
  speed of the machine drifted between passes. That is why only numbers from
  the same interleaved pass are compared.
- Patched Chez: jolt-1's build at
  `/Users/admin/.cache/jolt-chez-patched/10.4.1-p0001-0007`, published while
  this pellet ran. It was measured afterwards over the same variant files (the
  emitted Scheme is byte-identical on both builds), with 3 interleaved runs and
  without the pairs.

## Variants

| | Rewrite (`rewrite.ss`) | Sites rewritten |
|---|---|---|
| orig | the cache's `.scm` as emitted | |
| R | read and written back unchanged (a control: it drops the `#\|L\|#` comments and re-spaces the text) | |
| A tracing | `(jolt-site-call (lambda () X))` and the inline top-level restore become `(clj-site N X)`, one opaque call per site carrying a fixnum standing in for the line. The per-fn `jolt-site-call` binding is dropped. `(jolt-site! '(fqn . line))` becomes `(clj-site! line)`. `jolt-register-callsite!` calls move into one quoted `(clj-register-callsites! '#(...))` per namespace | 143,618 wrappers, 16,082 inline restores, 17,565 bindings, 8,708 tail markers, 81,136 registrations |
| B constants | `keyword`/`jolt-symbol` with literal parts, and `jolt-vector`/`jolt-list`/`jolt-hash-map`/`jolt-hash-set`/`amap-slots->pmap` whose parts are all constant (metadata maps and arglists included), become quoted data. Constant `_kc$` bindings are substituted at their uses and dropped. `image-register-fn-form!` rows and the interned names move into one `(intern-constants! '#(names) '#(fn-forms))` per namespace | 106,424 keywords, 34,337 symbols, 60,935 collections, 122,689 bindings, 20,252 fn forms |
| C calls | `_kc$` `(jolt-var ns name)` bindings become `'ns/name` at their uses. `(jolt-invoke-tN (var-cell-deref 'v) a ...)` and `(jolt-invoke-tN (var-deref "ns" "name") a ...)` become `(clj-callN 'v a ...)`, where `clj-callN` is an opaque stub. A `let*` that binds every argument of a `jolt-invoke` to an `_a$` temporary is folded into the call | 77,130 bindings, 89,640 calls, 51,448 `let*` folded |
| D | A, then B, then C | all of the above |
| AB, AC, BC | the pairs | |
| T0 | not a rewrite: the real emitter with `JOLT_TRACE=0` (a separate cold cache) | |

The stubs (`clj-site`, `clj-callN`, `intern-constants!`, ...) are unbound top-level
procedures: the output is compiled, never run. All 521 files compile in every
variant, with no failures in any run.

## Results (stock Chez 10.4.1)

`compile-file` over all 521 files, Jolt's runtime loaded. Inspector
information on is what the AOT cache does today.

| Variant | Text, non-ws | Compile, inspector on | vs orig | `.so`, inspector on | Compile, inspector off | `.so`, inspector off |
|---|---|---|---|---|---|---|
| orig | 65.9 MB | 28.98 s | | 122.3 MB | 25.89 s | 60.2 MB |
| R (control) | 64.9 MB | 29.12 s | +0.5% | 122.2 MB | 25.78 s | 60.2 MB |
| A tracing | 52.8 MB | 23.17 s | **−20.1%** | 113.8 MB | 20.45 s | 56.1 MB |
| B constants | 59.6 MB | 24.89 s | **−14.1%** | 105.6 MB | 22.20 s | 51.9 MB |
| C calls | 58.0 MB | 25.45 s | **−12.2%** | 109.5 MB | 23.11 s | 55.1 MB |
| AB | 47.5 MB | 18.95 s | −34.6% | 97.2 MB | 16.80 s | 47.8 MB |
| AC | 45.8 MB | 19.55 s | −32.5% | 100.6 MB | 17.22 s | 50.4 MB |
| BC | 52.6 MB | 20.88 s | −28.0% | 92.3 MB | 18.66 s | 46.4 MB |
| **D all three** | **40.5 MB** | **15.57 s** | **−46.3%** | **83.7 MB** | **13.73 s** | **42.0 MB** |
| T0 (`JOLT_TRACE=0`) | 44.9 MB | 20.42 s | −29.5% | 104.3 MB | 18.07 s | 51.7 MB |
| Chez reference project (jolt-1, stock) | 4.3 MB | 8.73 s whole cold build | | 16.7 MB | | |

The same variants on **patched Chez** (median of 3, interleaved):

| Variant | Compile, inspector on | vs orig | `.so`, inspector on | Compile, inspector off | vs orig | `.so`, inspector off |
|---|---|---|---|---|---|---|
| orig | 27.86 s | | 122.8 MB | 25.05 s | | 60.2 MB |
| A tracing | 21.22 s | −23.8% | 114.3 MB | 19.12 s | −23.7% | 56.1 MB |
| B constants | 23.37 s | −16.1% | 106.1 MB | 21.29 s | −15.0% | 51.9 MB |
| C calls | 24.34 s | −12.6% | 110.1 MB | 21.63 s | −13.6% | 55.1 MB |
| D all three | 14.74 s | −47.1% | 84.2 MB | 13.03 s | −48.0% | 42.0 MB |
| T0 | 19.57 s | −29.8% | 104.8 MB | 17.34 s | −30.8% | 51.7 MB |

The two builds rank the rewrites the same, within 1–4 points of each other.
Patched writes 0.5 MB more `.so` with inspector information on, and an
identical `.so` with it off. The absolute times of the two tables come from
different passes, so compare them only within a table.

The gains add up in seconds: alone, A, B and C remove 5.8 + 4.1 + 3.5 = 13.4 s,
and together they remove 13.4 s. Each pair is likewise within 0.4 s of the
sum of its singles. The three rewrites touch disjoint code and do not interact.

### Where the compile time goes (Chez pass timing, one run each)

`CC_PASS_TIMING=1` makes `measure.ss` print `$print-pass-stats`. Pass timing
adds overhead, so read the totals relatively.

| | orig | A | B | C | D | T0 |
|---|---|---|---|---|---|---|
| all passes | 30.8 s | 23.6 s | 25.9 s | 26.7 s | 16.7 s | 20.9 s |
| fasl write (`pfasl`) | 3.78 | 3.27 | 3.19 | 3.30 | 2.53 | 2.93 |
| read | 1.11 | 0.84 | 0.96 | 0.94 | 0.67 | 0.76 |
| compile excluding read and fasl | 25.9 | 19.5 (−25%) | 21.8 (−16%) | 22.5 (−13%) | 13.5 (−48%) | 17.2 (−33%) |
| code objects compiled | 107,906 | 72,506 | 108,435 | 108,128 | 73,609 | 72,215 |

Tracing adds **35,000 code objects**: 108k with tracing on against 72k with it
off (A and T0 agree). The emitted code relies on cp0 to inline the per-fn
`jolt-site-call` helper and its `(lambda () ...)` thunk at every site
(run-trace-emit.ss case 14 pins that on small fns). On the real project it
does not. Over the first 40 files, 3,061 of 13,875 site calls still reference
the helper after `expand/optimize`, and 3,119 thunks survive. The smallest
surviving case is a nested site, `(jolt-site-call (lambda () (jolt-into k
(jolt-site-call (lambda () (jolt-range 8))))))`: cp0 inlines the outer call but
not the inner one while it is integrating the helper. Raising
`cp0-effort-limit` and `cp0-score-limit` 1000-fold does not change that. Every
`(f (g x))` in Clojure is such a nest.

## The cold build's Chez share

On macOS the AOT cache compiles in a background worker by default
(`JOLT_AOT_ASYNC`, on unless set to off). So a default cold build compiles each
form twice: once in-process when it is evaluated (the "Chez compile" phase,
21.9 s in jolt-1's phase table) and once in the worker with `compile-file`, off
the critical path but on the CPU. Measured cold builds, median of 3, same
binary, checksum 374878:

| | Cold build |
|---|---|
| default (worker compiles the `.so`) | 41.7 s (40.5–42.5) |
| `JOLT_AOT_ASYNC=0` (in-process `compile-file`) | 65.7 s (63.6–71.5) |

That difference, 24 s, is the in-process `compile-file` of the whole project,
in line with the harness's 25.5–29.0 s. The eval-path compile runs the same
passes minus the fasl write and the file read. So the "compile excluding read
and fasl" row is its best proxy. jolt-1's `JOLT_PHASE_TIMES` measured that
phase at 21.86 s, and 14.55 s with `JOLT_TRACE=0`, so 33% lower. The proxy
predicts T0 at −33%, which supports using it. Applied to the 21.9 s:

| Rewrite | Share of compile removed | Of the 21.9 s eval-path compile |
|---|---|---|
| A tracing | 20–25% | ~4.4–5.5 s |
| B constants | 14–16% | ~3.1–3.5 s |
| C calls | 12–13% | ~2.6–2.8 s |
| D all three | 46–48% | ~10.1–10.5 s |
| (T0, tracing simply off) | 30–33% | 7.3 s measured by jolt-1 |

The background `compile-file` shrinks by the same fractions: 29.0 s to 15.6 s
for D.

## Interpretation

- **Primitives are worth pursuing, but they close only half the gap.** Even
  D still compiles in 15.6 s, against the Chez reference project's whole cold
  build of 8.7 s, and it writes an 83.7 MB `.so` against 16.7 MB. D's text is
  40.5 MB: 13 MB of it is the per-namespace quoted tables (fn-form source
  strings 7.3 MB, call-site table 4.8 MB, def ordinals 1.0 MB), and about
  27 MB is code, still 6.5× the 4.1 MB source. Among the largest remaining
  constructs: 32k `fnrec` named lets (one per fn clause), 15k
  `jolt-fn-identity-probe` branches, 18k `letrec` fn wrappers, 19k
  `var-cell-deref` value reads and 6k `jolt-register-source!`. These are the
  next places to look.
- **Tracing is the largest single item**, and part of it needs no Chez change
  at all. A, with one opaque call per site, removes 20%. Turning tracing off
  (T0) removes 30%. So an opaque per-site call costs about a third of what the
  current shape costs. The 35k closures cp0 leaves at nested sites are an
  emitter artifact. Spelling the restore as a runtime macro, or inline as the
  top-level sites already are, should remove them without touching Chez.
- **Constants (B) is the largest `.so` win**: −14% with inspector information
  on and −14% off, more than A, because load-time construction code is bulky
  object code but cheap to compile. It needs a fasl-able constant
  representation (keywords and symbols as something Chez interns on fasl read,
  or a per-namespace intern step), which is the primitive part.
- **Calls (C) is the smallest win** (−12% compile, −10% `.so`). Its gain adds
  to the others, so it is worth building eventually, but nothing depends on it
  coming first.
- Inspector information costs 11% of compile time (29.0 s against 25.9 s for
  orig) and half the `.so`. That is orthogonal to all of the above.

## Recommendation

1. **Before any primitive, do the emitter-only tracing fix**: make the site
   restore a macro, or emit it inline, so that nested sites stop leaving
   closures. Measure it against A and T0. It should recover a good part of
   the 20–30% without touching Chez.
2. **First primitive to prototype: tracing.** A single per-site operation that
   Chez lowers inline (save the site pair, call, restore). This measurement
   used an opaque call per site, so it is pessimistic: −20% with an opaque
   call, −30% with no call at all.
3. **Second: constants**, as a fasl-able interned constant (B: −14% compile,
   −14% `.so`).
4. **Last: calls through vars** (C: −12%). Because the gains are additive,
   building all three takes the compile to about half (D: −46%) whatever the
   order.

## Reproduce

```sh
# cold caches (outside the repo), tracing on and off
cd <proj200k>
JOLT_CACHE_DIR=<cache-trace> <worktree>/target/release/jolt -m workload.main 42 3
JOLT_TRACE=0 JOLT_CACHE_DIR=<cache-notrace> <worktree>/target/release/jolt -m workload.main 42 3
# from the repo root: rewrite, measure, summarize (~45 min at 5 runs)
TRACE_OFF_DIR=<cache-notrace>/<version>/v1 \
  sh benchmarks/chez-primitives/compact-ceiling/run.sh <cache-trace>/<version>/v1 <work> /opt/homebrew/bin/chez 5
SUMMARY_ONLY=1 sh benchmarks/chez-primitives/compact-ceiling/run.sh - <work>   # re-summarize
# per-pass breakdown for one variant
CC_PASS_TIMING=1 chez --script benchmarks/chez-primitives/compact-ceiling/measure.ss D <work>/D <work>/so-D '#t' 1
```

Scripts: `compact-ceiling/rewrite.ss` (the rewriter, which prints the rewrite
counts), `compact-ceiling/measure.ss` (compile timing in the runtime
environment) and `compact-ceiling/run.sh` (driver).
