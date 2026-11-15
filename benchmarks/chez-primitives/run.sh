#!/bin/sh
# The four compile-cost tests of the Chez-primitives experiments, on the
# generated Clojure 200k project (jolt) and the hand-written Scheme 200k
# reference (plain Chez), printed as one table with each run's checksum.
#
#   sh benchmarks/chez-primitives/run.sh JOLT CHEZ CLJ-PROJECT SCM-PROJECT WORK
#
#   JOLT         a built jolt binary (target/release/jolt), never bin/jolt
#   CHEZ         the Chez that runs the Scheme reference (stock or patched)
#   CLJ-PROJECT  generate.bb output (src/workload/*.clj); "-" skips it
#   SCM-PROJECT  chez-reference gen.py output (src/main.sps); "-" skips it
#   WORK         scratch directory for caches, object files and source backups;
#                keep it outside the repository
#
# Tests, each against its own fresh cache:
#   cold    empty cache: compile everything, write the cache (median of COLD_N)
#   warm    the same command again, everything cached (median of WARM_N)
#   leaf    append a new def to a namespace/library no other worker requires, run
#   common  append a new def to workload.common / (workload common), run
#
# jolt writes its .so files from a background worker after the run exits, so
# each jolt run reports two times: the run itself, and "+cache", the wall time
# until the worker has drained (no aot-jobs-*.edn left in the cache). The next
# run always waits for that drain, so it never races the previous one's cache.
#
# Every source file the edits touch is restored on exit, however the script ends.
# Environment the jolt runs see (JOLT_TRACE=0, ...) passes straight through.
#
#   COLD_N=3 WARM_N=5 EDIT_N=3   repetitions (medians; the group's minimums)
#   PHASES=1                     one extra cold run with JOLT_PHASE_TIMES=1,
#                                printing jolt's exclusive phase times
#   VOLUME=1                     report code volume (volume.ss) after the colds
#   INPUT="42 3"                 workload arguments
set -eu

[ $# -eq 5 ] || { sed -n '2,32p' "$0" >&2; exit 2; }
jolt=$1; chez=$2; clj=$3; scm=$4; work=$5
here=$(cd "$(dirname "$0")" && pwd)
COLD_N=${COLD_N:-3}; WARM_N=${WARM_N:-5}; EDIT_N=${EDIT_N:-3}
PHASES=${PHASES:-1}; VOLUME=${VOLUME:-1}; INPUT=${INPUT:-42 3}

abs() { (cd "$1" && pwd); }
mkdir -p "$work"
work=$(abs "$work")
case $jolt in /*) ;; *) jolt=$(cd "$(dirname "$jolt")" && pwd)/$(basename "$jolt") ;; esac
case $chez in /*) ;; *) chez=$(command -v "$chez") ;; esac
[ "$clj" = - ] || clj=$(abs "$clj")
[ "$scm" = - ] || scm=$(abs "$scm")

now_ms() { perl -MTime::HiRes=time -e 'printf "%.0f\n", time*1000'; }
median() { sort -n | awk '{v[NR]=$1} END {if (NR) print v[int((NR+1)/2)]}'; }
secs() { awk -v m="$1" 'BEGIN {printf "%.2f", m/1000}'; }

# --- source edits, restored on any exit -------------------------------------------
backups="$work/orig"
rm -rf "$backups"; mkdir -p "$backups"
restore() {
  for b in "$backups"/*; do
    [ -f "$b" ] || continue
    cp "$b" "$(cat "$b.path")" 2>/dev/null || true
  done
}
trap 'restore' EXIT
trap 'restore; exit 130' INT TERM
backup() {   # file
  n=$(printf '%s' "$1" | tr '/' '_')
  [ -f "$backups/$n" ] || { cp "$1" "$backups/$n"; printf '%s' "$1" > "$backups/$n.path"; }
}
# A new exported definition, unique per iteration so every edited run misses.
edit_clj() {   # file k
  backup "$1"
  printf '\n(def jolt-harness-edit-%s %s)\n' "$2" "$2" >> "$1"
}
edit_scm() {   # file k: export the name, define it before the library's last paren
  backup "$1"
  perl -0pi -e "s/\\(export /(export jolt-harness-edit-$2 /; s/\\)\\s*\\z/\n  (define jolt-harness-edit-$2 $2))\n/" "$1"
}
# A worker no other worker requires: only its group (gN) and main load it, so an
# edit recompiles it and the namespaces that chain up to main, nothing else.
leaf_of() {   # dir ext needle-prefix needle-suffix
  for f in $(ls "$1" | grep "\\.$2\$" | grep -v '^g[0-9]*\.\|^m[0-9]*\.\|^common\.\|^api\.\|^main\.' | sort); do
    n=${f%.$2}
    users=$(grep -l -F "$3$n$4" "$1"/*."$2" | grep -v "/g[0-9]*\\.$2\$\|/main\\.$2\$\|/$f\$" | wc -l)
    [ "$users" -eq 0 ] && { echo "$f"; return; }
  done
}

# --- rows -------------------------------------------------------------------------
rows="$work/rows.txt"; : > "$rows"
row() { printf '%s\n' "$*" >> "$rows"; printf '  %s\n' "$*" >&2; }

# --- jolt (Clojure 200k) ------------------------------------------------------------
jolt_cache=""
# A worker removes its manifest when done; one that died leaves it behind, so the
# wait also ends when no worker for this cache is alive.
drain() {   # wait for the background .so worker of the run that just exited
  while [ -n "$(find "$jolt_cache" -name 'aot-jobs-*.edn' 2>/dev/null | head -1)" ] \
        && pgrep -f -- "--aot-worker $jolt_cache" > /dev/null; do
    sleep 0.2
  done
}
# one run: prints "<run-ms> <until-cache-ms> <checksum>"
jolt_run() {   # cache-dir [extra env assignment]
  jolt_cache=$1; shift
  out="$work/jolt.out"
  t0=$(now_ms)
  ( cd "$clj" && env JOLT_CACHE_DIR="$jolt_cache" "$@" "$jolt" -m workload.main $INPUT ) \
    > "$out" 2> "$work/jolt.err" || { echo "jolt run failed:" >&2; tail -20 "$work/jolt.err" >&2; exit 1; }
  t1=$(now_ms)
  drain
  t2=$(now_ms)
  echo "$((t1 - t0)) $((t2 - t0)) $(tail -1 "$out" | tr -d ' ')"
}
# n runs into "<median-run> <median-cache> <checksums>"
jolt_series() {   # n setup-cmd cache-dir
  n=$1; setup=$2; cache=$3
  : > "$work/series"
  i=1
  while [ "$i" -le "$n" ]; do
    $setup "$i"
    jolt_run "$cache" >> "$work/series"
    i=$((i + 1))
  done
  r=$(cut -d' ' -f1 "$work/series" | median)
  c=$(cut -d' ' -f2 "$work/series" | median)
  sums=$(cut -d' ' -f3 "$work/series" | sort -u | tr '\n' ' ')
  echo "$r $c $sums"
}
noop() { :; }

if [ "$clj" != - ]; then
  ws="$clj/src/workload"
  [ -f "$ws/common.clj" ] || { echo "no $ws/common.clj" >&2; exit 2; }
  leaf=$(leaf_of "$ws" clj '[workload.' ' ')
  echo "jolt: $jolt" >&2
  echo "clj leaf namespace: workload.${leaf%.clj}" >&2

  : > "$work/series"
  i=1
  while [ "$i" -le "$COLD_N" ]; do
    rm -rf "$work/jolt-cache-$i"
    jolt_run "$work/jolt-cache-$i" >> "$work/series"
    i=$((i + 1))
  done
  set -- $(cut -d' ' -f1 "$work/series" | median) $(cut -d' ' -f2 "$work/series" | median)
  row "clj cold    $(secs $1) s  +cache $(secs $2) s  checksum $(cut -d' ' -f3 "$work/series" | sort -u | tr '\n' ' ')"
  cache="$work/jolt-cache-$COLD_N"
  # the colds are done with; keep the last one for the warm and edit runs
  i=1; while [ "$i" -lt "$COLD_N" ]; do rm -rf "$work/jolt-cache-$i"; i=$((i + 1)); done

  if [ "$VOLUME" = 1 ]; then
    echo "--- clj code volume (cache after cold) ---" >&2
    "$chez" --script "$here/volume.ss" --clj "$ws" --cache "$cache" > "$work/volume-clj.txt"
    cat "$work/volume-clj.txt" >&2
  fi

  set -- $(jolt_series "$WARM_N" noop "$cache")
  row "clj warm    $(secs $1) s  +cache $(secs $2) s  checksum $3"

  leaf_edit() { edit_clj "$ws/$leaf" "$1"; }
  set -- $(jolt_series "$EDIT_N" leaf_edit "$cache")
  row "clj leaf    $(secs $1) s  +cache $(secs $2) s  checksum $3"
  restore

  common_edit() { edit_clj "$ws/common.clj" "$1"; }
  set -- $(jolt_series "$EDIT_N" common_edit "$cache")
  row "clj common  $(secs $1) s  +cache $(secs $2) s  checksum $3"
  restore
  rm -rf "$cache"

  if [ "$PHASES" = 1 ]; then
    rm -rf "$work/jolt-cache-phases"
    jolt_run "$work/jolt-cache-phases" JOLT_PHASE_TIMES=1 > "$work/series"
    row "clj phases  run $(secs "$(cut -d' ' -f1 "$work/series")") s  checksum $(cut -d' ' -f3 "$work/series")"
    grep '^jolt phase-times:' "$work/jolt.err" | sed 's/^/  /' | tee -a "$rows" >&2
    rm -rf "$work/jolt-cache-phases"
  fi
fi

# --- Chez (Scheme 200k) -------------------------------------------------------------
if [ "$scm" != - ]; then
  ss="$scm/src/workload"
  [ -f "$ss/common.sls" ] || { echo "no $ss/common.sls" >&2; exit 2; }
  leaf=$(leaf_of "$ss" sls '(workload ' ')')
  obj="$work/scm-obj"
  echo "chez: $chez" >&2
  echo "scm leaf library: (workload ${leaf%.sls})" >&2
  scm_run() {   # prints "<ms> <checksum>"
    t0=$(now_ms)
    ( cd "$scm" && "$chez" --libdirs "$scm/src::$obj" --compile-imported-libraries \
        --program "$scm/src/main.sps" $INPUT ) > "$work/scm.out" 2> "$work/scm.err" \
      || { echo "chez run failed:" >&2; tail -20 "$work/scm.err" >&2; exit 1; }
    t1=$(now_ms)
    echo "$((t1 - t0)) $(tail -1 "$work/scm.out" | sed 's/^checksum //')"
  }
  scm_series() {   # n setup-cmd [fresh]
    : > "$work/series"
    i=1
    while [ "$i" -le "$1" ]; do
      $2 "$i"
      [ "${3:-}" = fresh ] && { rm -rf "$obj"; mkdir -p "$obj"; }
      scm_run >> "$work/series"
      i=$((i + 1))
    done
    echo "$(cut -d' ' -f1 "$work/series" | median) $(cut -d' ' -f2 "$work/series" | sort -u | tr '\n' ' ')"
  }
  set -- $(scm_series "$COLD_N" noop fresh)
  row "scm cold    $(secs $1) s  checksum $2"
  if [ "$VOLUME" = 1 ]; then
    echo "--- scm code volume ---" >&2
    "$chez" --script "$here/volume.ss" --scm "$scm/src" --so "$obj" > "$work/volume-scm.txt"
    cat "$work/volume-scm.txt" >&2
  fi
  set -- $(scm_series "$WARM_N" noop)
  row "scm warm    $(secs $1) s  checksum $2"
  leaf_edit_scm() { edit_scm "$ss/$leaf" "$1"; }
  set -- $(scm_series "$EDIT_N" leaf_edit_scm)
  row "scm leaf    $(secs $1) s  checksum $2"
  restore
  common_edit_scm() { edit_scm "$ss/common.sls" "$1"; }
  set -- $(scm_series "$EDIT_N" common_edit_scm)
  row "scm common  $(secs $1) s  checksum $2"
  restore
  rm -rf "$obj"
fi

echo "=== summary ===" >&2
cat "$rows"
