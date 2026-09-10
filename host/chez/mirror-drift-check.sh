#!/bin/sh
# mirror-drift-check.sh — hand-mirrored host file drift gate (POSIX sh).
#
# Runs host/chez/mirror-drift-check.ss under the build's Chez. With no flag
# this is the gate: exit 1 when a procedure defined in both halves of a
# mirrored pair differs without an allowlist line, or when an allowlist line
# has gone stale. Flags pass through:
#   --regen   rewrite host/chez/mirror-drift-allowlist.txt from reality
#   --list    print the tallies and the diverged names
#
# Chez resolution mirrors portability-check.sh: JOLT_CHEZ wins (the Makefile
# hands down the interpreter it selected), then a PATH search.
root="$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)"
cd "$root" || exit 1

if [ -z "${JOLT_CHEZ:-}" ]; then
  for c in chez chezscheme; do
    if command -v "$c" >/dev/null 2>&1; then
      JOLT_CHEZ="$c"
      break
    fi
  done
  if [ -z "${JOLT_CHEZ:-}" ]; then
    echo "mirror drift: no Chez Scheme executable found on PATH" >&2
    exit 1
  fi
fi

exec "$JOLT_CHEZ" --script host/chez/mirror-drift-check.ss "$@"
