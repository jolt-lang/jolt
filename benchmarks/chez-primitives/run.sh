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
# Tests:
#   cold    an empty cache: compile everything, write the cache (COLD_N runs,
#           each from its own empty cache)
#   warm    the same command again, everything cached (WARM_N runs)
#   leaf    append a new def to a worker no other worker requires, run (EDIT_N
#           runs, a different def each time so every run recompiles)
#   common  the same for workload.common / (workload common), which every worker
#           requires (EDIT_N runs)
# Each row is the median, then every run's time, then the set of checksums seen.
#
# jolt compiles its .so files in a background worker that can outlive the run,
# so a jolt row has two times: the run itself and "+cache", the wall time until
# that worker has drained (it deletes its aot-jobs-*.edn manifest). Every run
# waits for the drain before the next one starts, so no run races the cache.
#
# Every source file an edit touches is restored before the next test and on exit.
# The environment passes through to jolt (JOLT_TRACE=0 measures tracing off).
#
#   COLD_N=3 WARM_N=5 EDIT_N=3   repetitions
#   PHASES=1                     one more cold run with JOLT_PHASE_TIMES=1,
#                                printing jolt's exclusive phase times
#   VOLUME=1                     code volume (volume.ss) of the first test's cache
#   INPUT="42 3"                 workload arguments
set -eu

[ $# -eq 5 ] || { sed -n '2,37p' "$0" >&2; exit 2; }
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

# Shell functions share one variable namespace, so each keeps its own prefix.
now_ms() { perl -MTime::HiRes=time -e 'printf "%.0f\n", time*1000'; }
median() { sort -n | awk '{v[NR]=$1} END {if (NR) print v[int((NR+1)/2)]}'; }
secs() { awk -v m="$1" 'BEGIN {printf "%.2f", m/1000}'; }

# --- source edits, restored on any exit -------------------------------------------
backups="$work/orig"
rm -rf "$backups"; mkdir -p "$backups"
restore() {
  for rb in "$backups"/*.path; do
    [ -f "$rb" ] || continue
    cp "${rb%.path}" "$(cat "$rb")" || echo "run.sh: could not restore $(cat "$rb")" >&2
  done
}
trap 'restore' EXIT
trap 'restore; exit 130' INT TERM
backup() {   # file
  bk=$(printf '%s' "$1" | tr '/' '_')
  [ -f "$backups/$bk" ] || { cp "$1" "$backups/$bk"; printf '%s' "$1" > "$backups/$bk.path"; }
}
# A new exported definition, unique per run so every edited run recompiles.
edit_clj() {   # file k
  backup "$1"
  printf '\n(def jolt-harness-edit-%s %s)\n' "$2" "$2" >> "$1"
}
# Chez wants a library's definitions ahead of its expressions, so the define goes
# right after the (balanced) import clause.
edit_scm() {   # file k
  backup "$1"
  JH_K=$2 perl -0pi -e 's/\(export /(export jolt-harness-edit-$ENV{JH_K} /;
    s/(\(import(?:[^()]++|(\((?:[^()]++|(?2))*\)))*\))/$1\n  (define jolt-harness-edit-$ENV{JH_K} $ENV{JH_K})/' "$1"
}
# The first worker (by name) no other worker requires: only its group (gN) and,
# through that, main load it, so an edit recompiles it and the chain up to main.
leaf_of() {   # dir ext needle-prefix needle-suffix
  for lf in $(ls "$1" | grep -E "\\.$2\$" | grep -v -E '^(g[0-9]+|m[0-9]+|common|api|main)\.' | sort); do
    lusers=$(grep -l -F "$3${lf%.$2}$4" "$1"/*."$2" \
             | grep -v -E "/(g[0-9]+|main|${lf%.$2})\\.$2\$" | wc -l)
    if [ "$lusers" -eq 0 ]; then echo "$lf"; return 0; fi
  done
  echo "run.sh: no leaf worker in $1" >&2
  return 1
}

# --- runs ---------------------------------------------------------------------------
rows="$work/rows.txt"; : > "$rows"
row() { printf '%s\n' "$*" >> "$rows"; printf '  %s\n' "$*" >&2; }

jolt_cache=""
# A worker deletes its manifest when it is done; one that died leaves it behind,
# so the wait also ends when no worker for this cache is alive.
drain() {
  while [ -n "$(find "$jolt_cache" -name 'aot-jobs-*.edn' 2>/dev/null | head -1)" ] \
        && pgrep -f -- "--aot-worker $jolt_cache" > /dev/null; do
    sleep 0.1
  done
}
# One run appended to $work/series as "<run-ms> <until-cache-ms> <checksum>".
# Called directly, never inside $(...), so a failed run stops the script.
jolt_run() {   # cache-dir [VAR=value ...]
  jolt_cache=$1; shift
  jr_t0=$(now_ms)
  ( cd "$clj" && env JOLT_CACHE_DIR="$jolt_cache" "$@" "$jolt" -m workload.main $INPUT ) \
    > "$work/jolt.out" 2> "$work/jolt.err" \
    || { echo "jolt run failed:" >&2; tail -20 "$work/jolt.err" >&2; exit 1; }
  jr_t1=$(now_ms)
  drain
  jr_t2=$(now_ms)
  echo "$((jr_t1 - jr_t0)) $((jr_t2 - jr_t0)) $(tail -1 "$work/jolt.out" | tr -d ' ')" >> "$work/series"
}
scm_run() {
  sr_t0=$(now_ms)
  ( cd "$scm" && "$chez" --libdirs "$scm/src::$obj" --compile-imported-libraries \
      --program "$scm/src/main.sps" $INPUT ) > "$work/scm.out" 2> "$work/scm.err" \
    || { echo "chez run failed:" >&2; tail -20 "$work/scm.err" >&2; exit 1; }
  sr_t1=$(now_ms)
  echo "$((sr_t1 - sr_t0)) $((sr_t1 - sr_t0)) $(tail -1 "$work/scm.out" | sed 's/^checksum //')" >> "$work/series"
}
# N runs of CMD, each after SETUP <i>, into $work/series.
series() {   # n setup cmd [args]
  se_n=$1; se_setup=$2; shift 2
  : > "$work/series"
  se_i=1
  while [ "$se_i" -le "$se_n" ]; do
    $se_setup "$se_i"
    "$@"
    se_i=$((se_i + 1))
  done
}
# One table row from $work/series. A checksum other than EXPECTED is flagged.
summarize() {   # label with-cache(yes|no) expected
  sm_run=$(cut -d' ' -f1 "$work/series" | median)
  sm_all=$(cut -d' ' -f1 "$work/series" | while read -r v; do printf '%s ' "$(secs "$v")"; done)
  sm_sums=$(cut -d' ' -f3 "$work/series" | sort -u | tr '\n' ' ' | sed 's/ $//')
  sm_flag=""
  [ -z "$3" ] || [ "$sm_sums" = "$3" ] || sm_flag="  !! CHECKSUM MISMATCH (expected $3)"
  if [ "$2" = yes ]; then
    sm_c=$(cut -d' ' -f2 "$work/series" | median)
    row "$(printf '%-11s %7s s  +cache %7s s  checksum %s  runs: %s%s' \
           "$1" "$(secs "$sm_run")" "$(secs "$sm_c")" "$sm_sums" "$sm_all" "$sm_flag")"
  else
    row "$(printf '%-11s %7s s                   checksum %s  runs: %s%s' \
           "$1" "$(secs "$sm_run")" "$sm_sums" "$sm_all" "$sm_flag")"
  fi
}
noop() { :; }

# --- jolt (Clojure 200k) ------------------------------------------------------------
if [ "$clj" != - ]; then
  ws="$clj/src/workload"
  [ -f "$ws/common.clj" ] || { echo "no $ws/common.clj" >&2; exit 2; }
  leaf=$(leaf_of "$ws" clj '[workload.' ' ')
  echo "jolt: $jolt" >&2
  echo "clj leaf namespace: workload.${leaf%.clj}" >&2

  # each cold gets its own empty cache; the last one stays for the tests after it
  cold_cache() { cache="$work/jolt-cache-$1"; rm -rf "$cache"; }
  cold_run() { jolt_run "$cache"; }
  series "$COLD_N" cold_cache cold_run
  summarize "clj cold" yes ""
  expect=$(cut -d' ' -f3 "$work/series" | head -1)
  i=1; while [ "$i" -lt "$COLD_N" ]; do rm -rf "$work/jolt-cache-$i"; i=$((i + 1)); done

  if [ "$VOLUME" = 1 ]; then
    echo "--- clj code volume (cache after cold) ---" >&2
    "$chez" --script "$here/volume.ss" --clj "$ws" --cache "$cache" > "$work/volume-clj.txt"
    cat "$work/volume-clj.txt" >&2
  fi

  series "$WARM_N" noop jolt_run "$cache"
  summarize "clj warm" yes "$expect"

  leaf_edit() { edit_clj "$ws/$leaf" "$1"; }
  series "$EDIT_N" leaf_edit jolt_run "$cache"
  summarize "clj leaf" yes "$expect"
  restore

  common_edit() { edit_clj "$ws/common.clj" "$1"; }
  series "$EDIT_N" common_edit jolt_run "$cache"
  summarize "clj common" yes "$expect"
  restore
  rm -rf "$cache"

  if [ "$PHASES" = 1 ]; then
    cache="$work/jolt-cache-phases"; rm -rf "$cache"
    series 1 noop jolt_run "$cache" JOLT_PHASE_TIMES=1
    summarize "clj phases" yes "$expect"
    grep '^jolt phase-times:' "$work/jolt.err" | sed 's/^/  /' | tee -a "$rows" >&2
    rm -rf "$cache"
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

  fresh_obj() { rm -rf "$obj"; mkdir -p "$obj"; }
  series "$COLD_N" fresh_obj scm_run
  summarize "scm cold" no ""
  expect=$(cut -d' ' -f3 "$work/series" | head -1)
  if [ "$VOLUME" = 1 ]; then
    echo "--- scm code volume ---" >&2
    "$chez" --script "$here/volume.ss" --scm "$scm/src" --so "$obj" > "$work/volume-scm.txt"
    cat "$work/volume-scm.txt" >&2
  fi
  series "$WARM_N" noop scm_run
  summarize "scm warm" no "$expect"
  leaf_edit_scm() { edit_scm "$ss/$leaf" "$1"; }
  series "$EDIT_N" leaf_edit_scm scm_run
  summarize "scm leaf" no "$expect"
  restore
  common_edit_scm() { edit_scm "$ss/common.sls" "$1"; }
  series "$EDIT_N" common_edit_scm scm_run
  summarize "scm common" no "$expect"
  restore
  rm -rf "$obj"
fi

echo "=== summary ===" >&2
cat "$rows"
