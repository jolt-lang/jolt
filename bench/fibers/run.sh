#!/bin/sh
# bench/fibers/run.sh — Fibers R6 benchmark driver (jolt-nvpr.7).
#
# Runs the whole measurement set as :thread vs :fiber pairs, wraps the memory
# phases in /usr/bin/time -l for the peak-RSS cross-check (the R0 method:
# retained objects, forced full collect, absolute live bytes, RSS from the OS),
# and prints the comparison table. Run via `make fibersbench`.
#
# Like aba.sh, this is blind to dev mode: every phase runs the runtime from
# source with chez --script (the gate boot may use the precompiled
# target/dev/gate.so when fresh, which is behavior-identical to the literal
# loads). Benchmarks are opt-in and NOT part of make test / make ci.
set -e
cd "$(dirname "$0")/../.."
root="$PWD"

# JOLT_CHEZ wins (see host/chez/selfcheck.sh).
CHEZ_BIN="${JOLT_CHEZ:-${CHEZ:-$(command -v chez || command -v scheme || true)}}"
[ -n "$CHEZ_BIN" ] || { echo "chez not found on PATH (set CHEZ)" >&2; exit 1; }
HARNESS="$root/bench/fibers/bench-fibers.ss"

phase() { # phase <args...> — run one harness phase, echo its lines
  "$CHEZ_BIN" --script "$HARNESS" "$@"
}

phase_rss() { # like phase but under /usr/bin/time -l; appends peak-rss-bytes
  out=$(/usr/bin/time -l "$CHEZ_BIN" --script "$HARNESS" "$@" 2>&1) || true
  # macOS /usr/bin/time -l prints the NUMBER FIRST, then the label, so the value
  # is $1 and not $5 — $5 is the literal word "size", which is what this printed
  # before and why the whole memory table came out as "size -". GNU time (Linux)
  # puts the label first, so accept either by taking whichever field is numeric.
  rss=$(printf '%s\n' "$out" | awk '/maximum resident set size/{
          for (i = 1; i <= NF; i++) if ($i ~ /^[0-9]+$/) { print $i; exit }
        }')
  printf '%s\n' "$out" | grep -v 'maximum resident set size' || true
  echo "peak-rss-bytes: ${rss:-NA}"
}

get() { # get <key> — extract "key: value" from stdin
  sed -n "s/^$1: //p"
}

echo "machine: $(sysctl -n machdep.cpu.brand_string 2>/dev/null) / $(sysctl -n hw.ncpu 2>/dev/null) cores / $(( $(sysctl -n hw.memsize 2>/dev/null) / 1073741824 )) GiB"
first=$(phase switch | head -1)
echo "$first"
echo

# --- 1. spawn cost -----------------------------------------------------------
echo "== 1. spawn cost (K processes that immediately park) =="
printf '%-11s %-9s %-9s %-10s %-13s %-10s %s\n' \
  backend K created create-ms per-create-us all-parked fail
SPAWN_KS="1000 10000 100000"
for backend in fiber thread; do
  for k in $SPAWN_KS; do
    out=$(phase "spawn-$backend" "$k")
    created=$(printf '%s\n' "$out" | get created)
    cms=$(printf '%s\n' "$out" | get create-ms)
    perus=$(printf '%s\n' "$out" | get per-create-us)
    parked=$(printf '%s\n' "$out" | get all-parked)
    fail=$(printf '%s\n' "$out" | get fail)
    printf '%-11s %-9s %-9s %-10s %-13s %-10s %s\n' \
      "$backend" "$k" "$created" "$cms" "$perus" "$parked" "$fail"
  done
done
echo

# --- 2. memory per parked process -------------------------------------------
echo "== 2. memory per parked process (retained, forced full collect, absolute live; RSS cross-check) =="
base_out=$(phase_rss mem-baseline)
base_rss=$(printf '%s\n' "$base_out" | get peak-rss-bytes)
echo "boot baseline peak RSS: ${base_rss:-NA} bytes"
printf '%-13s %-9s %-13s %-13s %-11s\n' backend K live-B/unit peak-RSS RSS/unit
for memphase in mem-fiber-raw mem-fiber-go mem-thread; do
  out=$(phase_rss "$memphase")
  cnt=$(printf '%s\n' "$out" | get count)
  lv=$(printf '%s\n' "$out" | get live-delta)
  pk=$(printf '%s\n' "$out" | get peak-rss-bytes)
  perlive=$(printf '%s\n' "$out" | get per-live-bytes)
  perrss=$(awk -v r="$pk" -v b="$base_rss" -v k="$cnt" \
    'BEGIN{ if (r+0>0 && b+0>0 && k+0>0) printf "%.0f", (r-b)/k; else printf "-" }')
  printf '%-13s %-9s %-13s %-13s %-11s\n' "$memphase" "$cnt" "$perlive" "$pk" "$perrss"
done
echo

# --- 3. channel throughput ---------------------------------------------------
echo "== 3. channel throughput =="
for backend in thread fiber; do
  out=$(phase "ping-$backend")
  rt=$(printf '%s\n' "$out" | get per-roundtrip-us)
  hps=$(printf '%s\n' "$out" | get handoffs-per-sec)
  echo "ping-pong ($(printf '%s\n' "$out" | get runs-ms)) $backend: $rt us/roundtrip  $hps handoffs/sec"
  [ "$backend" = thread ] && rt_thread=$rt
done
p1=$(phase ping-fiber-1)
echo "ping-pong, pool pinned to 1 carrier (runs $(printf '%s\n' "$p1" | get runs-ms)): $(printf '%s\n' "$p1" | get per-roundtrip-us) us/roundtrip  $(printf '%s\n' "$p1" | get handoffs-per-sec) handoffs/sec  — floor when both fibers share one carrier"
for backend in thread fiber; do
  out=$(phase "fanin-$backend")
  echo "fan-in 8x2500 (runs $(printf '%s\n' "$out" | get runs-ms)) $backend: $(printf '%s\n' "$out" | get values-per-sec) values/sec"
done
echo

# --- 4. context-switch cost --------------------------------------------------
echo "== 4. context-switch cost =="
sw=$(phase switch)
bare=$(printf '%s\n' "$sw" | get bare-switch-ns)
sched=$(printf '%s\n' "$sw" | get scheduler-switch-ns)
echo "bare continuation switch:   $bare ns  (R0 measured 12.5 ns)"
echo "scheduler yield + slice:    $sched ns  (R2 measured 64 ns)"
echo "OS-thread channel handoff:  $(awk -v r="$rt_thread" 'BEGIN{printf "%.1f", r*1000/2}') ns  (from ping-pong: per-roundtrip/2)"
echo

# --- 5. scaling with carriers ------------------------------------------------
echo "== 5. scaling with carriers (CPU-bound, 40 fibers x 1e7) =="
sc=$(phase scaling)
printf '%-10s %-10s %s\n' carriers ms mops/sec
printf '%s\n' "$sc" | awk '/^carriers: /{c=$2} /^ms: /{m=$2} /^mops-per-sec: /{printf "  %-8d %-10s %s\n", c, m, $2}'
un=$(printf '%s\n' "$sc" | get uneven-ms)
echo "uneven lifetimes ($(printf '%s\n' "$sc" | get uneven-carriers) carriers, one fiber does 10x): ${un} ms — fibers do not migrate, so the long fiber pins its carrier (property, not defect)"
echo
echo "done."
