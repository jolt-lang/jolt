#!/bin/sh
# run.sh -- the whole compact-ceiling measurement, from a cold AOT cache's .scm.
#
#   sh benchmarks/chez-primitives/compact-ceiling/run.sh CACHE_SCM_DIR WORK [CHEZ] [RUNS]
#
# CACHE_SCM_DIR  the v1/ directory of a cold JOLT_CACHE_DIR (tracing on)
# WORK           scratch directory outside the repo; variants and .so land there
# Optional env: TRACE_OFF_DIR = the v1/ directory of a JOLT_TRACE=0 cache, measured
# as an extra row "T0" (the real emitter with tracing off).
#
# Run from the repo root. Runs are interleaved (run 1 of every variant, then
# run 2, ...) so drift over the half hour lands on every variant alike. Prints
# the text size of each variant, then per variant and inspector setting the
# median real and cpu ms over RUNS and the total .so bytes; raw lines go to
# WORK/times.txt.
set -eu
src=$1; work=$2; chez=${3:-chez}; runs=${4:-5}
here=benchmarks/chez-primitives/compact-ceiling
mkdir -p "$work/orig"
cp "$src"/*.scm "$work/orig/"
variants="orig R A B C D AB AC BC"
if [ -n "${TRACE_OFF_DIR:-}" ]; then
  mkdir -p "$work/T0"; cp "$TRACE_OFF_DIR"/*.scm "$work/T0/"; variants="$variants T0"
fi
for v in R A B C D AB AC BC; do
  mkdir -p "$work/$v"
  "$chez" --script "$here/rewrite.ss" "$v" "$work/$v" "$work"/orig/*.scm > "$work/$v.stats"
done
for v in $variants; do
  printf '%s nonws_bytes=%s\n' "$v" "$(cat "$work/$v"/*.scm | tr -d ' \t\n\r' | wc -c | tr -d ' ')"
done
: > "$work/times.txt"
i=0
while [ "$i" -lt "$runs" ]; do
  i=$((i + 1))
  for ii in '#t' '#f'; do
    for v in $variants; do
      mkdir -p "$work/so-$v"
      "$chez" --script "$here/measure.ss" "$v" "$work/$v" "$work/so-$v" "$ii" 1 | tail -1 >> "$work/times.txt"
    done
  done
done
med() { sort -n | sed -n "$(( (runs + 1) / 2 ))p"; }
for ii in '#t' '#f'; do
  for v in $variants; do
    rows=$(grep "^$v ii=$ii " "$work/times.txt")
    real=$(printf '%s\n' "$rows" | sed 's/.*real_ms=\([0-9]*\).*/\1/' | med)
    cpu=$(printf '%s\n' "$rows" | sed 's/.*cpu_ms=\([0-9]*\).*/\1/' | med)
    so=$(printf '%s\n' "$rows" | tail -1 | sed 's/.*so_bytes=\([0-9]*\).*/\1/')
    fail=$(printf '%s\n' "$rows" | grep -vc 'fail=0' || true)
    printf '%s ii=%s real_ms=%s cpu_ms=%s so_bytes=%s failed_runs=%s\n' "$v" "$ii" "$real" "$cpu" "$so" "$fail"
  done
done
