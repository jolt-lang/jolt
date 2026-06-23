#!/bin/sh
# Run the jolt benchmark suite and print mean ms per benchmark.
#
# Each benchmark isolates an axis the ray tracer (float-compute-bound) doesn't
# capture — see README.md. Run back-to-back against `main` to measure a pass's
# impact.
#
#   bench/run.sh                 # default sizes, whole-program optimization on
#   JOLT_WHOLE_PROGRAM=0 bench/run.sh   # compare with WP off
#   bench/run.sh binary-trees    # one benchmark
#
# Needs `jolt` on PATH (build with `jpm build`; export PATH="$PWD/build:$PATH").
set -e
cd "$(dirname "$0")"

export JOLT_DIRECT_LINK="${JOLT_DIRECT_LINK:-1}"
export JOLT_WHOLE_PROGRAM="${JOLT_WHOLE_PROGRAM:-1}"
export JOLT_APP_PATHS="$PWD"
export JOLT_PATH="$PWD"

# name:default-arg  (arg sized to run in a few seconds each). Axes: allocation
# (binary-trees), megamorphic vs monomorphic dispatch, persistent-collection
# churn (collections — now O(log n) via the HAMT, so sized up), pure
# float compute (mandelbrot), call+arith recursion (fib).
BENCHES="binary-trees:14 dispatch:2000 mono-dispatch:2000 collections:30000 mandelbrot:200 fib:30"

# JVM=1 also runs each bench on JVM Clojure and prints a jolt/JVM ratio — the
# holistic absolute-reference scorecard for the optimization work.
run_one() {
  ns="${1%%:*}"; arg="${2:-${1##*:}}"
  jmean=$(jolt -m "$ns" "$arg" 2>&1 | awk '/^mean:/{print $2}')
  if [ -n "$JVM" ]; then
    vmean=$(clojure -Sdeps '{:paths ["."]}' -M -m "$ns" "$arg" 2>&1 | awk '/^mean:/{print $2}')
    ratio=$(awk "BEGIN{ if ($vmean+0>0) printf \"%.1f\", ($jmean+0)/($vmean+0); else printf \"-\" }")
    printf '%-16s jolt %9s ms   jvm %8s ms   %sx\n' "$ns" "${jmean:--}" "${vmean:--}" "$ratio"
  else
    printf '%-16s %9s ms\n' "$ns" "${jmean:--}"
  fi
}

if [ -n "$1" ]; then
  for spec in $BENCHES; do
    [ "${spec%%:*}" = "$1" ] && run_one "$spec" "$2"
  done
else
  echo "jolt benchmark suite (WP=$JOLT_WHOLE_PROGRAM${JVM:+, vs JVM Clojure})"
  for spec in $BENCHES; do run_one "$spec"; done
fi
