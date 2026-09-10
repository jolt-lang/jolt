#!/bin/sh
# dead-host-check.sh — dead host procedure gate wrapper (POSIX sh).
#
# Runs host/chez/dead-host-check.ss under the build's Chez. The checker collects
# every top-level (define (name ...) ...) in the handwritten host files and
# fails when one is referenced nowhere in the repo. Flags pass through:
#   --list   print what it found without failing
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
    echo "dead host: no Chez Scheme executable found on PATH" >&2
    exit 1
  fi
fi

exec "$JOLT_CHEZ" --script host/chez/dead-host-check.ss "$@"
