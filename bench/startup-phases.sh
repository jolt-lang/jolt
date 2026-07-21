#!/bin/sh
# Startup phase breakdown — where does joltc's startup floor go?
#
# bench/startup.sh times the whole process (exec to exit) against babashka, but
# it says nothing about which phase a change moved. This attributes a `joltc
# prog.clj` invocation to four phases, so the floor-reduction work can see whether
# a change lands on boot, on compilation, or on runtime:
#
#   boot     : runtime + compiler image load, then jolt.main recompiled from
#              Clojure — the fixed cost before any user code. Measured by
#              `joltc --version` (boots the image, prints, exits; no cmd-run).
#   dispatch : deps/project resolution + load-file setup that every file run pays
#              on top of boot. Measured by running a file that is just `nil`.
#   compile  : compiling the user program's forms. Measured by a compile-heavy,
#              run-trivial program (many defns, one cheap call).
#   run      : executing the compiled program. Measured by a run-heavy,
#              compile-trivial program (tiny source, one long loop).
#
# The phases are external subtractions, each isolating one cost by construction:
#
#   boot     = version
#   dispatch = empty        - version
#   compile  = compile-heavy - empty
#   run      = run-heavy      - empty
#
# They are honest approximations, not a strict partition of one program: the
# compile-heavy program also evaluates its var installs (a little runtime), and
# the run-heavy program also compiles its one loop form (a little compile). Both
# are small next to the phase they isolate. What the beads want is directional:
# speed up boot and `boot` drops; speed up the compiler and `compile` drops;
# speed up the runtime and `run` drops.
#
#   bench/startup-phases.sh            # defaults: 7 reps, 400 defns, 30M-iter loop
#   REPS=15 bench/startup-phases.sh    # more reps (best-of-N sheds scheduler noise)
#   DEFNS=800 LOOP=60000000 bench/startup-phases.sh   # heavier compile / run
#   JOLT_BIN=/path/to/joltc bench/startup-phases.sh
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

REPS="${REPS:-7}"
DEFNS="${DEFNS:-400}"
LOOP="${LOOP:-30000000}"
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

# empty: boot + dispatch, ~0 compile, ~0 run.
printf 'nil\n' > "$work/empty.clj"

# compile-heavy: DEFNS small function definitions plus one cheap call. Compiling
# the bodies dominates; evaluating them just installs vars.
awk -v n="$DEFNS" 'BEGIN{
  for (i = 0; i < n; i++)
    printf "(defn f%d [x] (+ x (* %d 2) (- %d 1)))\n", i, i, i
  print "(println (f0 1))"
}' > "$work/compile-heavy.clj"

# run-heavy: tiny source, one long accumulating loop. Compiles in a few forms,
# spends its time in the runtime.
printf '(println (loop [i 0 acc 0] (if (< i %s) (recur (inc i) (+ acc i)) acc)))\n' "$LOOP" \
  > "$work/run-heavy.clj"

# best-of-N wall clock in milliseconds for a command (min sheds scheduler noise).
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

# non-negative delta a-b (clamp to 0 so subtraction noise never prints negative).
delta() { d=$(($1 - $2)); if [ "$d" -lt 0 ]; then d=0; fi; echo "$d"; }

version=$(best_ms "$joltc" --version)
empty=$(best_ms "$joltc" "$work/empty.clj")
cheavy=$(best_ms "$joltc" "$work/compile-heavy.clj")
rheavy=$(best_ms "$joltc" "$work/run-heavy.clj")

boot="$version"
dispatch=$(delta "$empty" "$version")
compile=$(delta "$cheavy" "$empty")
run=$(delta "$rheavy" "$empty")

echo "startup phase breakdown — best of $REPS  ($(basename "$joltc"))"
printf '  %-9s %5s ms   runtime + compiler image load, jolt.main recompile\n' "boot" "$boot"
printf '  %-9s %5s ms   deps/project resolve + load-file setup\n' "dispatch" "$dispatch"
printf '  %-9s %5s ms   compile %s defns\n' "compile" "$compile" "$DEFNS"
printf '  %-9s %5s ms   run a %s-iteration loop\n' "run" "$run" "$LOOP"
echo
printf '  raw: version %s  empty %s  compile-heavy %s  run-heavy %s ms\n' \
  "$version" "$empty" "$cheavy" "$rheavy"
