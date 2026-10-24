#!/bin/sh
# collect-safe-activation-test.sh — jolt's Chez kernel patch
# (host/chez/chez-patches/0001-lockfree-thread-activation.patch): collect-safe
# foreign calls without the tc mutex. Invoke from the repo root:
#   sh test/chez/collect-safe-activation-test.sh [chez]
#
# Two halves:
#   - collect-safe-stress.ss, on the Chez the gate runs: threads in
#     collect-safe calls (sleeps, a qsort whose collect-safe callback
#     allocates, freshly compiled foreign procedures whose code is young)
#     while others allocate and request collections. It passes on a stock
#     kernel too; on a patched one it is what pins the protocol (with the
#     collector's lock of a native thread's return code removed it crashes).
#   - collect-safe-cost.clj, on jolt: a :blocking call against a plain one in
#     one process. On a patched kernel the ratio must stay under 4 (it is
#     ~1.7-2.4 on most runners; a stock kernel is ~8). It is a RATIO over a
#     fixed cost, two CASes and a fence (~13ns), so it climbs as the plain call
#     gets cheaper: x86 runners whose plain call took 6ns read 3.16x and 3.25x
#     against the 3x this bound used to be, while the same commit read 2.2x on
#     a 23ns runner. 4x still sits well clear of the stock kernel.
#   - collect-safe-scaling.ss, on any kernel: the tc mutex and a collection
#     with many parked threads, so a quadratic thread walk cannot come back.
#
# JOLT_REQUIRE_LOCKFREE_CHEZ=1 (set in CI, where every Chez is built with the
# patch) fails the gate when the kernel jolt runs on does not have it, so a
# stale cache or a patch that stopped applying cannot pass as "stock".
set -eu

CHEZ=${1:-${CHEZ:-chez}}

case "$(uname -s)" in
  MINGW*|MSYS*|CYGWIN*)
    echo "collect-safe-activation: skipped (the stress rows call POSIX usleep/qsort)"
    exit 0 ;;
esac

if ! "$CHEZ" -q --script test/chez/collect-safe-stress.ss; then
  echo "FAIL: collect-safe-stress.ss" >&2
  exit 1
fi

out=$(bin/jolt run test/chez/collect-safe-cost.clj)
echo "collect-safe cost: $out"
lockfree=$(echo "$out" | sed -n 's/^LOCKFREE \([a-z]*\) .*/\1/p')
ratio=$(echo "$out" | sed -n 's/.* RATIO \([0-9.]*\)$/\1/p')
[ -n "$ratio" ] || { echo "FAIL: no ratio in collect-safe-cost output" >&2; exit 1; }

# Scaling, in one process (collect-safe-scaling.ss): the tc mutex with 2000
# parked threads over none, and a collection with 1000 parked threads over
# 100. A thread-list walk in the wrong place makes either quadratic.
scale=$("$CHEZ" -q --script test/chez/collect-safe-scaling.ss)
echo "collect-safe scaling: $scale"
mutex=$(echo "$scale" | sed -n 's/^MUTEX \([0-9.]*\) .*/\1/p')
coll=$(echo "$scale" | sed -n 's/.* COLLECT \([0-9.]*\)$/\1/p')
[ -n "$mutex" ] && [ -n "$coll" ] || { echo "FAIL: no ratios in collect-safe-scaling output" >&2; exit 1; }
if awk "BEGIN { exit !($mutex >= 20) }"; then
  echo "FAIL: the tc mutex costs ${mutex}x with 2000 parked threads (bound 20x)" >&2
  exit 1
fi
if awk "BEGIN { exit !($coll >= 60) }"; then
  echo "FAIL: a collection with 10x the parked threads costs ${coll}x (bound 60x)" >&2
  exit 1
fi
if [ "$lockfree" != true ]; then
  if [ "${JOLT_REQUIRE_LOCKFREE_CHEZ:-}" = 1 ]; then
    echo "FAIL: JOLT_REQUIRE_LOCKFREE_CHEZ=1 but this Chez kernel lacks the lockfree-activation patch" >&2
    exit 1
  fi
  echo "collect-safe-activation: stock Chez kernel, cost bound not checked"
  exit 0
fi
if awk "BEGIN { exit !($ratio >= 4) }"; then
  echo "FAIL: a :blocking call costs ${ratio}x a plain one on a patched kernel (bound 4x)" >&2
  exit 1
fi

echo "collect-safe-activation: passed (ratio ${ratio}x, mutex ${mutex}x, collect ${coll}x)"
