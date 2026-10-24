#!/bin/sh
# jvm-gap-gate.sh — fail a release that lets a held row fall behind JVM Clojure.
#
#   ci/jvm-gap-gate.sh <jolt> [max-ratio] [bench...]
#
# bench-gate.sh compares a release against the LAST release, so it holds whatever
# a row already was: a row that was 6x the JVM stays 6x for as long as nothing
# makes it worse. This holds a few rows against the JVM itself. Each one is a
# path that once trailed the reference by more than 2x and was brought inside it
# (bench/README.md says which); naming one here keeps it there.
#
# The measurement is the suite's own: the same bench source built as an optimized
# jolt binary and run under JVM Clojure, on ONE machine in ONE run, alternating
# between the two after a discarded warm-up round, taking the MIN of the timed
# runs per side (noise only ever adds time). It is a ratio between two programs
# timed side by side, the only shape of timing assertion this repo allows in a
# gate; there is no threshold in milliseconds anywhere here.
#
# Needs what bench/run.sh needs, plus `clojure` on PATH for the reference.
set -eu

jolt="${1:-}"; max="${2:-2.00}"
[ "$#" -gt 2 ] && { shift 2; only="$*"; } || only=""
[ -n "$jolt" ] || { echo "usage: $0 <jolt> [max-ratio] [bench...]" >&2; exit 2; }
case "$jolt" in /*) ;; *) jolt="$(pwd)/$jolt" ;; esac
[ -x "$jolt" ] || { echo "not executable: $jolt" >&2; exit 2; }
command -v clojure >/dev/null 2>&1 || { echo "jvm-gap gate: no clojure on PATH" >&2; exit 2; }

# The held rows. A row joins when it is brought inside the ceiling; it leaves
# only with a reason written beside the change that takes it out.
HELD="atom-ops watched-swap pool-tasks sorted-set-build int-map-build"

root="$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)"
rounds="${JVM_GAP_ROUNDS:-3}"
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

echo "jvm-gap gate: jolt $("$jolt" --version 2>/dev/null || echo '?')"
# the JDK the clojure launcher runs, which is JAVA_CMD when set (setup-chez
# points it at JDK 21 while an older java stays first on the runner's PATH)
echo "jvm-gap gate: $("${JAVA_CMD:-java}" -version 2>&1 | head -1)"
echo "jvm-gap gate: min of $rounds timed runs per side, fail above ${max}x the JVM"
echo

timed() { awk '/^mean:/{print $2}'; }
jolt_ms() { "$work/$1" "$2" 2>/dev/null | timed; }
jvm_ms() { ( cd "$root/bench" && clojure -Sdeps '{:paths ["."]}' -M -m "$1" "$2" 2>/dev/null ) | timed; }
minof() { awk "BEGIN{m=\"$1\"+0; v=\"$2\"+0; if (v>0 && (m==0 || v<m)) m=v; print m}"; }

fails=0; rows=0
printf '%-18s %10s %10s %8s\n' bench jolt jvm ratio
for spec in $(sh "$root/bench/run.sh" --list); do
  ns="${spec%%:*}"; arg="${spec##*:}"
  held=0; for h in $HELD; do [ "$h" = "$ns" ] && held=1; done
  [ "$held" = 1 ] || continue
  if [ -n "$only" ]; then
    want=0; for o in $only; do [ "$o" = "$ns" ] && want=1; done
    [ "$want" = 1 ] || continue
  fi
  if ! ( cd "$root/bench" && JOLT_PWD="$PWD" "$jolt" build -m "$ns" -o "$work/$ns" --direct-link --opt ) \
       >"$work/build.log" 2>&1; then
    echo "  FAIL $ns: jolt could not build it"; sed -n '1,20p' "$work/build.log"
    fails=$((fails + 1)); continue
  fi
  jolt_ms "$ns" "$arg" >/dev/null || true; jvm_ms "$ns" "$arg" >/dev/null || true
  jmin=""; vmin=""; i=0
  while [ "$i" -lt "$rounds" ]; do
    i=$((i + 1))
    jmin="$(minof "$jmin" "$(jolt_ms "$ns" "$arg")")"
    vmin="$(minof "$vmin" "$(jvm_ms "$ns" "$arg")")"
  done
  if [ "$(awk "BEGIN{print (\"$jmin\"+0 <= 0 || \"$vmin\"+0 <= 0)}")" = 1 ]; then
    echo "  FAIL $ns: a side printed no mean: (jolt $jmin, jvm $vmin)"
    "$work/$ns" "$arg" 2>&1 | sed -n '1,10p'
    fails=$((fails + 1)); continue
  fi
  ratio="$(awk "BEGIN{printf \"%.2f\", (\"$jmin\"+0)/(\"$vmin\"+0)}")"
  over="$(awk "BEGIN{print (\"$ratio\"+0 > \"$max\"+0)}")"
  rows=$((rows + 1))
  printf '%-18s %10.1f %10.1f %7sx%s\n' "$ns" "$jmin" "$vmin" "$ratio" \
    "$([ "$over" = 1 ] && echo '  <-- OVER' || true)"
  [ "$over" = 1 ] && fails=$((fails + 1)) || true
done

echo
if [ "$rows" -eq 0 ] && [ "$fails" -eq 0 ]; then
  echo "jvm-gap gate: FAILED — nothing was measured"; exit 1
fi
if [ "$fails" -gt 0 ]; then
  echo "jvm-gap gate: FAILED — $fails held row(s) over ${max}x the JVM"
  echo "  Re-measure a named row alone (bench/run.sh <name>) before concluding"
  echo "  anything about its size; a ratio here is two programs on one machine."
  exit 1
fi
echo "jvm-gap gate: passed — $rows held row(s) within ${max}x the JVM"
