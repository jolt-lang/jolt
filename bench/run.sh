#!/bin/sh
# Run the jolt benchmark suite and print mean ms per benchmark.
#
# Each benchmark isolates an axis the ray tracer (float-compute-bound) doesn't
# capture — see README.md. Run back-to-back against `main` to measure a pass's
# impact (the same protocol as test/bench/core-bench.janet).
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

# name:default-arg  (arg sized to run in a few seconds each)
# NOTE collections is small because the persistent map is O(n)/assoc (jolt-684u);
# raise it once that's fixed to a HAMT.
BENCHES="binary-trees:14 dispatch:2000 collections:1500"

run_one() {
  ns="${1%%:*}"; arg="${2:-${1##*:}}"
  printf '%-16s ' "$ns"
  jolt -m "$ns" "$arg" 2>&1 | awk '/^mean:/{print}'
}

if [ -n "$1" ]; then
  for spec in $BENCHES; do
    [ "${spec%%:*}" = "$1" ] && run_one "$spec" "$2"
  done
else
  echo "jolt benchmark suite (WP=$JOLT_WHOLE_PROGRAM)"
  for spec in $BENCHES; do run_one "$spec"; done
fi
