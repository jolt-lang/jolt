#!/bin/sh
# bench/dyn-binding/run.sh — the dynamic-var binding stack (jolt-3bo).
#
# Runs every phase of bench-dyn.ss and prints them in order. Opt-in, NOT part of
# make test / make ci — benchmarks do not belong in a gate.
#
# Like bench/fibers/run.sh, this is blind to dev mode: the harness loads the
# runtime through gate-boot, which may use target/dev/gate.so when it is fresh.
# That is behaviour-identical to the literal loads.
#
# What to look at, and what each phase is guarding:
#
#   read     ABSENT must be flat in depth. That column is the whole point: every
#            var reference in compiled code lands there, because almost no var is
#            ever thread-bound. INNERMOST is the regression guard on the other
#            side — the flag adds a field read to a var that IS bound.
#   width    a frame's own alist is still walked, so this grows with the number of
#            vars in ONE binding form. It is small and bounded by what a program
#            writes in a single `binding`.
#   push     the cost the read path is traded against. Must not move much: the
#            back end pushes a frame per fn literal and per arity.
#   emit     N nested fn literals — the shape that made this worth measuring.
#   compile  a real namespace, which is wide and shallow. The guard that a
#            lookup fix has not been paid for out of push.
set -e
cd "$(dirname "$0")/../.."
root="$PWD"

# JOLT_CHEZ wins (see host/chez/selfcheck.sh).
CHEZ_BIN="${JOLT_CHEZ:-${CHEZ:-$(command -v chez || command -v scheme || true)}}"
[ -n "$CHEZ_BIN" ] || { echo "chez not found on PATH (set CHEZ)" >&2; exit 1; }
HARNESS="$root/bench/dyn-binding/bench-dyn.ss"

for phase in read width push emit compile; do
  echo "----------------------------------------------------------------------"
  "$CHEZ_BIN" --script "$HARNESS" "$phase"
done
