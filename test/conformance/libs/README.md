# Library conformance: replaying third-party test suites on jolt

`make libconformance` runs real Clojure libraries' **own** `clojure.test` suites on
jolt and compares the tallies against the standings recorded in `manifest.edn`.

This is the outer ring of jolt's conformance testing. The corpus
(`test/chez/corpus.edn`) is the inner ring: small, portable, JVM-certified
expressions that run in `make ci` with no external checkout. Library suites catch
what the corpus cannot — the combinations, the load-order effects, and the
long-tail core functions a real library leans on — but they need the libraries
present, so they are opt-in.

## Running

```sh
make libconformance                          # every library in the manifest
make libconformance LIBS="malli honeysql"    # just these
```

The library checkouts are ordinary upstream clones (**not** vendored — see
`test/conformance/README.md`), expected at `../conformance-libraries` or wherever
`JOLT_CONFORMANCE_LIBS` points. First-party jolt libraries (`time`, `xml`, `db`, …)
are expected beside this repo, or at `JOLT_SIBLING_LIBS`. The gate skips cleanly
when the checkout is absent.

Per-library output is written to `target/libconformance/<name>.log`.

## What the manifest holds

One entry per library: how to put it on the classpath, and what it scored.

| key | meaning |
| --- | --- |
| `:name` | manifest key, and the `LIBS=` selector |
| `:root` | checkout-relative directory, when it differs from `:name` (e.g. `yamlstar/core`) |
| `:paths` | source + test roots, relative to the library root |
| `:test-paths` | which of those hold test namespaces (default `["test"]`) |
| `:deps` | extra roots: `@lib/...` another checkout, `~lib/...` a first-party jolt library, `/...` absolute |
| `:extra-deps` | ordinary deps.edn coordinates, for deps with no local checkout — jolt resolves the source out of the jar |
| `:nses` | explicit namespace list; omitted means "discover every namespace under `:test-paths` that defines tests" |
| `:exclude-nses` | namespaces to skip (benchmarks, JVM-only suites) |
| `:preload` | jolt-side namespaces to require before the suites — a shim registering a host class the library reaches for through interop |
| `:expect` | the recorded tally |
| `:timeout` | seconds before the child is killed (default 300) |
| `:skip` / `:note` | why a library is not run, or why a non-zero tally is expected |

## The gate criterion

A library **regresses** when `pass` drops or `fail` / `error` / `load-fail` rise
against `:expect`. A move the other way — `pass` up, or any of the other three
down — is reported as `BETTER` and does not fail the gate: improving a library is
then a one-line manifest edit.

`:tolerance` widens both verdicts on all four counters, not just `pass`: in a
generative suite whether a drawn case fails or errors moves run to run while the
total stays put. It is a **symmetric** noise band, so it suppresses `BETTER` just
as it suppresses `WORSE` — a `pass` above the recorded one but inside the band
reports `ok`, because re-recording it would only re-centre the band on whatever
the last run drew. test.check (`:tolerance 40`) reading pass=245 against a
recorded 236 is that case. Above the band the move is real and says so, and every
library without a `:tolerance` is pinned exactly, so any rise there is `BETTER`.

`:expect` is deliberately a tally and not a per-assertion baseline. Some suites
are order- and environment-sensitive (compliment completes against the live
runtime, for instance), so pinning individual assertions would be noise. What the
tally does catch is the thing worth catching: a jolt change that silently breaks
a library that used to work.

## When a library regresses

1. Read `target/libconformance/<name>.log` — the failing assertions are in the
   suite's own output.
2. Reduce it to the smallest expression that misbehaves, and check that expression
   against reference Clojure.
3. If jolt is wrong, **the fix comes with a jolt-side test**: a corpus row when the
   expression is a single portable form, otherwise a fixture under `test/chez/`
   wired into `make ci`. The library suite is the discovery mechanism, not the
   regression test — `make ci` must fail without the external checkout too.
4. If jolt is deliberately different, record it in
   `test/conformance/known-divergences.edn` and note it on the manifest entry.

## Timing against the JVM

`make libperf` runs the same suites per test on jolt and on JVM Clojure and lists
the tests that are more than 5x slower on jolt (`JOLT_LIBPERF_THRESHOLD`). It is a
report, not a gate. Each namespace's suite runs `JOLT_LIBPERF_REPS` times (default
3) and a test's time is its minimum, measured between its `:begin-test-var` and
`:end-test-var` reports. Tests under `JOLT_LIBPERF_FLOOR_MS` (default 1ms) on jolt
are not flagged. Namespace load times are reported per library but kept apart
from the test times, since jolt compiles ahead of time at load.

The JVM classpath comes from the library's own `deps.edn` (`:deps` plus the `:dev` and
`:test` aliases) and `project.clj` (`:dependencies` plus the `:dev`/`:test`
profiles), with Clojure pinned. jolt's stand-ins (`~` libraries, `!` shims,
`:local-deps`) stay off it. A library that needs a different JVM recipe carries
`:jvm {:paths [...] :extra-deps {...} :exclude-deps [...] :skip "why"}`.

Output lands in `target/libperf/`: `report.tsv` sorted by ratio, `report.edn`
with every timed test, and `<lib>.jolt.log` / `<lib>.jvm.log`.
