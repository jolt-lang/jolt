#!/bin/sh
# bench-gate.sh — fail a release whose codegen made a benchmark dramatically slower.
#
#   ci/bench-gate.sh <baseline-jolt> <candidate-jolt> [max-ratio] [bench...]
#
# Naming benches runs only those — how a flagged row gets re-checked on its own,
# and how this script is exercised without paying for the whole suite. `startup`
# names the whole-process row, and `compile` the cold source-load row (its own
# ceiling, BENCH_GATE_COMPILE_MAX), both at the end.
#
# Both compilers build the SAME bench sources (this checkout's), so what is being
# compared is codegen, not the benchmarks. Every measurement is a RATIO between
# two binaries timed on ONE machine in ONE run, alternating between them: that is
# the only shape of timing assertion this repo allows in a gate. An absolute
# millisecond ceiling would false-fail on a slow runner and, worse, pass on a fast
# one while hiding a real regression, so there is no threshold in ms anywhere here.
#
# Why this exists: bench/arrays went 229.7 -> 1272.6ms (5.4x) when :loop became
# spliceable and array-hinted callees lost their unboxed path. `make test` (88 ci
# targets) and `make libconformance` (47 libraries) both passed the whole time --
# every answer was still correct, just 5.4x slower. Nothing but the suite catches
# that class, so the release runs it.
#
# The threshold is deliberately loose. It is a gate, not a scorecard: it must not
# false-fail, and a real perf review reads the table by hand. Observed noise on a
# quiet machine is ~1.07x per bench, with a cold first-run outlier up to ~1.65x --
# handled here by building each binary once and taking the MIN of several timed
# runs per side (noise only ever adds time), after a discarded warm-up round.
set -eu

base="${1:-}"; cand="${2:-}"; max="${3:-1.40}"
[ "$#" -gt 3 ] && { shift 3; only="$*"; } || only=""
[ -n "$base" ] && [ -n "$cand" ] || {
  echo "usage: $0 <baseline-jolt> <candidate-jolt> [max-ratio] [bench...]" >&2; exit 2; }
# Absolute, because build() runs from bench/ — a relative jolt would resolve
# against the wrong directory there and read as "the candidate cannot build it".
abspath() { case "$1" in /*) printf '%s\n' "$1" ;;
                          *) printf '%s/%s\n' "$(pwd)" "$1" ;; esac; }
base="$(abspath "$base")"; cand="$(abspath "$cand")"
for j in "$base" "$cand"; do
  [ -x "$j" ] || { echo "not executable: $j" >&2; exit 2; }
done

root="$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)"
rounds="${BENCH_GATE_ROUNDS:-3}"
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

echo "bench gate: baseline $("$base" --version 2>/dev/null || echo '?')"
echo "bench gate: candidate $("$cand" --version 2>/dev/null || echo '?')"
echo "bench gate: min of $rounds timed runs per side, fail above ${max}x"
echo

# Build one bench with one compiler. Same flags the suite uses.
build() {  # build <jolt> <ns> <out>
  ( cd "$root/bench" && JOLT_PWD="$PWD" "$1" build -m "$2" -o "$3" --direct-link --opt ) \
    >"$work/build.log" 2>&1
}

# One timed run -> milliseconds, off the bench's own `mean:` line.
timed() { "$1" "$2" 2>/dev/null | awk '/^mean:/{print $2}'; }

fails=0; rows=0; skipped=0
printf '%-16s %10s %10s %8s\n' bench baseline candidate ratio

for spec in $(sh "$root/bench/run.sh" --list); do
  ns="${spec%%:*}"; arg="${spec##*:}"
  if [ -n "$only" ]; then
    want=0; for o in $only; do [ "$o" = "$ns" ] && want=1; done
    [ "$want" = 1 ] || continue
  fi
  if ! build "$base" "$ns" "$work/$ns.base"; then
    echo "  SKIP $ns: the BASELINE jolt could not build it (a bench newer than the"
    echo "       release being compared against — not a regression)"
    skipped=$((skipped + 1)); continue
  fi
  if ! build "$cand" "$ns" "$work/$ns.cand"; then
    echo "  FAIL $ns: the candidate jolt could not build it"
    sed -n '1,20p' "$work/build.log"
    fails=$((fails + 1)); continue
  fi
  # discarded warm-up, then alternate so any drift on the runner lands on both
  timed "$work/$ns.base" "$arg" >/dev/null || true
  timed "$work/$ns.cand" "$arg" >/dev/null || true
  bmin=""; cmin=""
  i=0
  while [ "$i" -lt "$rounds" ]; do
    i=$((i + 1))
    b="$(timed "$work/$ns.base" "$arg")"; c="$(timed "$work/$ns.cand" "$arg")"
    bmin="$(awk "BEGIN{m=\"$bmin\"+0; v=\"$b\"+0; if (v>0 && (m==0 || v<m)) m=v; print m}")"
    cmin="$(awk "BEGIN{m=\"$cmin\"+0; v=\"$c\"+0; if (v>0 && (m==0 || v<m)) m=v; print m}")"
  done
  # A baseline that builds a bench but cannot RUN it — a bench that exercises
  # something the older release lacks, `printing`'s format flags on 0.8.5 —
  # is not a regression; the candidate failing to run one is.
  if [ "$(awk "BEGIN{print (\"$bmin\"+0 <= 0)}")" = 1 ]; then
    echo "  SKIP $ns: the BASELINE jolt built it but printed no mean: (a bench that"
    echo "       needs something the release being compared against lacks)"
    skipped=$((skipped + 1)); continue
  fi
  if [ "$(awk "BEGIN{print (\"$cmin\"+0 <= 0)}")" = 1 ]; then
    echo "  FAIL $ns: the candidate produced no timing (the bench printed no mean:)"
    "$work/$ns.cand" "$arg" 2>&1 | sed -n '1,10p'
    fails=$((fails + 1)); continue
  fi
  ratio="$(awk "BEGIN{printf \"%.2f\", (\"$cmin\"+0)/(\"$bmin\"+0)}")"
  over="$(awk "BEGIN{print (\"$ratio\"+0 > \"$max\"+0)}")"
  rows=$((rows + 1))
  printf '%-16s %10.1f %10.1f %7sx%s\n' "$ns" "$bmin" "$cmin" "$ratio" \
    "$([ "$over" = 1 ] && echo '  <-- REGRESSED' || true)"
  [ "$over" = 1 ] && fails=$((fails + 1)) || true
done

# The startup row. Every bench above times the compute inside a running binary,
# so none of them sees a regression in what a built program costs from exec to
# exit — the boot image's decode and the runtime's init, which is exactly what a
# boot-format change moves (0.8.5's vfasl boot, 0.8.6's --boot codecs). Both
# compilers build bench/hello.clj; best of $rounds whole-process wall clocks per
# side, alternating, same ratio and same ceiling as the rows above.
if [ -z "$only" ] || { for o in $only; do [ "$o" = startup ] && break; done; [ "$o" = startup ]; }; then
  wall_ms() {   # best-of-N whole-process wall clock in ms; noise only ever adds time
    best=""; i=0
    while [ "$i" -lt "$rounds" ]; do
      t0=$(perl -MTime::HiRes=time -e 'printf "%d", time()*1000')
      "$1" >/dev/null 2>&1
      t1=$(perl -MTime::HiRes=time -e 'printf "%d", time()*1000')
      d=$((t1 - t0)); i=$((i + 1))
      if [ -z "$best" ] || [ "$d" -lt "$best" ]; then best="$d"; fi
    done
    echo "$best"
  }
  if ! build "$base" hello "$work/hello.base"; then
    echo "  SKIP startup: the BASELINE jolt could not build bench/hello.clj"
    skipped=$((skipped + 1))
  elif ! build "$cand" hello "$work/hello.cand"; then
    echo "  FAIL startup: the candidate jolt could not build bench/hello.clj"
    sed -n '1,20p' "$work/build.log"
    fails=$((fails + 1))
  else
    "$work/hello.base" >/dev/null 2>&1 || true; "$work/hello.cand" >/dev/null 2>&1 || true
    bmin="$(wall_ms "$work/hello.base")"; cmin="$(wall_ms "$work/hello.cand")"
    if [ "$bmin" -le 0 ] || [ "$cmin" -le 0 ]; then
      echo "  FAIL startup: a side took no measurable time"; fails=$((fails + 1))
    else
      ratio="$(awk "BEGIN{printf \"%.2f\", $cmin/$bmin}")"
      over="$(awk "BEGIN{print ($ratio > \"$max\"+0)}")"
      rows=$((rows + 1))
      printf '%-16s %10.1f %10.1f %7sx%s\n' startup "$bmin" "$cmin" "$ratio" \
        "$([ "$over" = 1 ] && echo '  <-- REGRESSED' || true)"
      [ "$over" = 1 ] && fails=$((fails + 1)) || true
    fi
  fi
fi

# The compile row. Every row above times a BUILT program, so none of them sees
# what loading jolt code from source costs, before any cache exists: reading,
# analysis, emission and Chez's compile. A regression there leaves every answer
# right and every bench flat. #1645: 0.8.23's trace restore made Chez compile
# traced code ~1.57x slower than untraced, and the HoneySQL suite went from
# 10-12s to 15-17s on a cold cache; nothing at release time measured it. Both
# binaries load the same sources from this checkout (stdlib pprint and test,
# babashka.cli) with the AOT cache off and tracing on, the default, so each run
# compiles everything. Best of $rounds whole-process wall clocks per side,
# alternating. Its own ceiling, tighter than the rows above, since a compile
# regression costs every cold start of every program (BENCH_GATE_COMPILE_MAX,
# default 1.15; observed spread ~1% per side). That is still too loose for a
# trace-instrumentation regression on its own -- #1645 moved this row ~1.10x --
# which the per-PR gate `make tracecompilecost` measures exactly.
if [ -z "$only" ] || { for o in $only; do [ "$o" = compile ] && break; done; [ "$o" = compile ]; }; then
  cmax="${BENCH_GATE_COMPILE_MAX:-1.15}"
  corpus='(load-file "stdlib/clojure/pprint.clj") (load-file "stdlib/clojure/test.clj") (load-file "vendor/cli/src/babashka/cli.cljc")'
  load_ms() {   # wall clock of one cold source load in ms, 0 if it failed
    t0=$(perl -MTime::HiRes=time -e 'printf "%d", time()*1000')
    ( cd "$root" && JOLT_AOT_CACHE=0 JOLT_NO_USER_DEPS=1 "$1" -e "$corpus" ) >/dev/null 2>&1 || { echo 0; return; }
    t1=$(perl -MTime::HiRes=time -e 'printf "%d", time()*1000')
    echo $((t1 - t0))
  }
  load_ms "$base" >/dev/null; load_ms "$cand" >/dev/null   # discarded warm-up (page cache)
  bmin=""; cmin=""; i=0
  while [ "$i" -lt "$rounds" ]; do
    i=$((i + 1))
    b="$(load_ms "$base")"; c="$(load_ms "$cand")"
    if [ "$b" -le 0 ]; then bmin=0; elif [ -z "$bmin" ] || { [ "$bmin" -gt 0 ] && [ "$b" -lt "$bmin" ]; }; then bmin="$b"; fi
    if [ "$c" -le 0 ]; then cmin=0; elif [ -z "$cmin" ] || { [ "$cmin" -gt 0 ] && [ "$c" -lt "$cmin" ]; }; then cmin="$c"; fi
  done
  if [ "$bmin" -le 0 ]; then
    echo "  SKIP compile: the BASELINE jolt could not load the corpus"
    skipped=$((skipped + 1))
  elif [ "$cmin" -le 0 ]; then
    echo "  FAIL compile: the candidate jolt could not load the corpus"
    ( cd "$root" && JOLT_AOT_CACHE=0 JOLT_NO_USER_DEPS=1 "$cand" -e "$corpus" ) 2>&1 | sed -n '1,10p'
    fails=$((fails + 1))
  else
    ratio="$(awk "BEGIN{printf \"%.2f\", $cmin/$bmin}")"
    over="$(awk "BEGIN{print ($ratio > \"$cmax\"+0)}")"
    rows=$((rows + 1))
    printf '%-16s %10.1f %10.1f %7sx%s\n' compile "$bmin" "$cmin" "$ratio" \
      "$([ "$over" = 1 ] && echo "  <-- REGRESSED (ceiling ${cmax}x)" || true)"
    [ "$over" = 1 ] && fails=$((fails + 1)) || true
  fi
fi

echo
if [ "$rows" -eq 0 ]; then
  echo "bench gate: FAILED — nothing was measured (every bench skipped or errored)"
  exit 1
fi
if [ "$fails" -gt 0 ]; then
  echo "bench gate: FAILED — $fails of $rows benchmark(s) over their ceiling (${max}x; compile ${BENCH_GATE_COMPILE_MAX:-1.15}x)"
  echo "  A ratio here is candidate/baseline on one machine, so it is a real"
  echo "  codegen change, not runner noise. Re-measure the named benches alone"
  echo "  (bench/run.sh <name>) before concluding anything about the size."
  exit 1
fi
if [ "$skipped" -gt 0 ]; then
  echo "bench gate: passed — $rows benchmark(s) within ${max}x, $skipped skipped"
else
  echo "bench gate: passed — $rows benchmark(s) within ${max}x"
fi
