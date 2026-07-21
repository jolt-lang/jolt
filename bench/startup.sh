#!/bin/sh
# Startup / small-program latency — the axis run.sh does NOT measure. run.sh
# builds each benchmark to an optimized binary and times the compute inside it;
# it says nothing about how long `joltc` itself takes to get from exec to first
# result. That fixed floor (runtime + compiler image boot, then compile the
# program) is what dominates ys-style workloads: many short `joltc prog.clj`
# invocations where the program runs for milliseconds.
#
# This times whole-process wall clock (best of N, to shed scheduler noise) for a
# built joltc against babashka on the same sources, across three sizes:
#   - version : pure boot floor, no user program
#   - trivial : boot + compile + run of a one-liner
#   - script  : boot + compile + run of a small real program (a seq pipeline)
#
#   bench/startup.sh              # default 7 reps
#   REPS=15 bench/startup.sh      # more reps
#   JOLT_BIN=/path/to/joltc bench/startup.sh
#
# joltc must be a BUILT binary (target/release/joltc or an installed joltc), not
# the dev bin/joltc source launcher — the dev script opts out of the AOT cache and
# boots from source, so it is not representative of what users run.
set -e
cd "$(dirname "$0")"
root="$(cd .. && pwd)"

joltc="${JOLT_BIN:-$root/target/release/joltc}"
[ -x "$joltc" ] || joltc="$(command -v joltc || true)"
if [ -z "$joltc" ] || [ ! -x "$joltc" ]; then
  echo "error: no built joltc found. Build one (make joltc-release) or set JOLT_BIN." >&2
  exit 1
fi
have_bb=""; command -v bb >/dev/null 2>&1 && have_bb=1

REPS="${REPS:-7}"
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

printf '(println (reduce + (map inc (filter odd? (range 1000)))))\n' > "$work/trivial.clj"
# a small lazy-seq pipeline — representative of idiomatic script code
cat > "$work/script.clj" <<'EOF'
(println
  (reduce (fn [a x] (+ a x)) 0
          (take 5000 (map (fn [x] (* x x))
                          (filter even? (iterate inc 1))))))
EOF

# best-of-N wall-clock in milliseconds for a command.
best_ms() {
  best=""
  i=0
  while [ "$i" -lt "$REPS" ]; do
    t0=$(perl -MTime::HiRes=time -e 'printf "%d", time()*1000')
    "$@" >/dev/null 2>&1 || true
    t1=$(perl -MTime::HiRes=time -e 'printf "%d", time()*1000')
    ms=$((t1 - t0))
    if [ -z "$best" ] || [ "$ms" -lt "$best" ]; then best="$ms"; fi
    i=$((i + 1))
  done
  echo "$best"
}

row() {
  label="$1"; shift
  jbin="$1"; shift
  jms=$(best_ms "$joltc" $jbin)
  if [ -n "$have_bb" ]; then
    bms=$(best_ms bb "$@")
    ratio=$(awk "BEGIN{ if ($bms>0) printf \"%.1fx\", $jms/$bms; else printf \"-\" }")
    printf '%-10s jolt %5s ms   bb %5s ms   %s\n' "$label" "$jms" "$bms" "$ratio"
  else
    printf '%-10s jolt %5s ms\n' "$label" "$jms"
  fi
}

echo "startup / small-program latency — best of $REPS  ($(basename "$joltc")${have_bb:+ vs bb})"
row "version" "--version" "--version"
row "trivial" "$work/trivial.clj" "$work/trivial.clj"
row "script"  "$work/script.clj"  "$work/script.clj"
