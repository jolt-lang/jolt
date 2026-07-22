#!/bin/sh
# Run the jolt benchmark suite against JVM Clojure and print a jolt/JVM scorecard.
#
# jolt's optimizing passes (direct-linking, inlining, scalar-replace, whole-program
# inference) fire only in an AOT BUILD — `joltc run -m` is unoptimized — so each
# benchmark is compiled to an optimized standalone binary and timed. JVM Clojure
# runs the same portable source for the absolute reference. Each benchmark prints
# `runs: [...]` and `mean: N ms`; the table shows the means and the jolt/JVM ratio.
#
#   bench/run.sh                 # full suite + JVM scorecard
#   bench/run.sh fib             # one benchmark, default size
#   bench/run.sh fib 32          # one benchmark, custom size
#   NO_JVM=1 bench/run.sh        # jolt only (skip the JVM reference)
#   MODE_A=1 bench/run.sh        # also time a plain-release build (no
#                                # --direct-link --opt) per bench — the default
#                                # `jolt build` most users get; roughly doubles
#                                # the suite's build time, so it's on demand
#
# Building needs Chez's kernel dev files (libkernel.a + scheme.h) and a C compiler,
# the same as `jolt build`; set JOLT_CHEZ_CSV to override the detected csv dir.
set -e
cd "$(dirname "$0")"
root="$(cd .. && pwd)"
joltc="$root/bin/joltc"
export JOLT_PWD="$PWD"

# Locate Chez's kernel dev files for the optimized build (as build-smoke.sh does).
csv="$JOLT_CHEZ_CSV"
if [ -z "$csv" ]; then
  chez_bin="$(command -v chez || command -v scheme || command -v petite || true)"
  if [ -n "$chez_bin" ]; then
    base="$(cd "$(dirname "$chez_bin")/.." 2>/dev/null && pwd)"
    for d in "$base"/lib/csv*/*/; do
      [ -f "${d}libkernel.a" ] && csv="${d%/}" && break
    done
  fi
fi
if [ -z "$csv" ] || [ ! -f "$csv/libkernel.a" ] || [ ! -f "$csv/scheme.h" ] || ! command -v cc >/dev/null 2>&1; then
  echo "error: the optimized build needs Chez kernel dev files (libkernel.a + scheme.h) and cc." >&2
  echo "       set JOLT_CHEZ_CSV to the csv dir, e.g. \$(brew --prefix chezscheme)/lib/csv*/<machine>." >&2
  exit 1
fi
export JOLT_CHEZ_CSV="$csv"

bindir="$(mktemp -d)"
trap 'rm -rf "$bindir"' EXIT

# name:default-arg, each sized to run in a few seconds. Axes: see README.md.
BENCHES="fib:30 tak:24 loop-recur:20000 mandelbrot:200 arrays:40000 mathfns:1000000 collections:30000 seqs:20000 transducers:20000 mono-dispatch:2000 dispatch:2000 binary-trees:14"

run_one() {
  ns="${1%%:*}"; arg="${2:-${1##*:}}"
  if ! "$joltc" build -m "$ns" -o "$bindir/$ns" --direct-link --opt >/dev/null 2>&1; then
    printf '%-16s  jolt build FAILED\n' "$ns"; return
  fi
  jmean=$("$bindir/$ns" "$arg" 2>/dev/null | awk '/^mean:/{print $2}')
  # mode A: the plain-release binary (no --direct-link --opt) — what a default
  # `jolt build` ships. Tracked so a release-mode win or regression is visible.
  rmean=""
  if [ -n "$MODE_A" ]; then
    if "$joltc" build -m "$ns" -o "$bindir/$ns-rel" >/dev/null 2>&1; then
      rmean=$("$bindir/$ns-rel" "$arg" 2>/dev/null | awk '/^mean:/{print $2}')
    fi
  fi
  if [ -z "$NO_JVM" ]; then
    vmean=$(clojure -Sdeps '{:paths ["."]}' -M -m "$ns" "$arg" 2>/dev/null | awk '/^mean:/{print $2}')
    ratio=$(awk "BEGIN{ if (\"$vmean\"+0>0 && \"$jmean\"+0>0) printf \"%.1fx\", (\"$jmean\"+0)/(\"$vmean\"+0); else printf \"-\" }")
    if [ -n "$MODE_A" ]; then
      rratio=$(awk "BEGIN{ if (\"$vmean\"+0>0 && \"$rmean\"+0>0) printf \"%.1fx\", (\"$rmean\"+0)/(\"$vmean\"+0); else printf \"-\" }")
      printf '%-16s opt %9s ms (%s)   release %9s ms (%s)   jvm %8s ms\n' \
        "$ns" "${jmean:--}" "$ratio" "${rmean:--}" "$rratio" "${vmean:--}"
    else
      printf '%-16s jolt %9s ms   jvm %8s ms   %s\n' "$ns" "${jmean:--}" "${vmean:--}" "$ratio"
    fi
  elif [ -n "$MODE_A" ]; then
    printf '%-16s opt %9s ms   release %9s ms\n' "$ns" "${jmean:--}" "${rmean:--}"
  else
    printf '%-16s jolt %9s ms\n' "$ns" "${jmean:--}"
  fi
}

if [ -n "$1" ]; then
  spec=""
  for s in $BENCHES; do [ "${s%%:*}" = "$1" ] && spec="$s"; done
  [ -n "$spec" ] || { echo "unknown benchmark: $1 (have: ${BENCHES})" >&2; exit 1; }
  run_one "$spec" "$2"
else
  echo "jolt benchmark suite — optimized AOT binaries${NO_JVM:+ }${NO_JVM:-, vs JVM Clojure}"
  for spec in $BENCHES; do run_one "$spec"; done
fi
