#!/bin/sh
# error-kinds-check.sh — hold test/conformance/error-kinds.edn and the sources to
# each other, in BOTH directions.
#
# A diagnostic kind is the stable handle on an error: tooling keys on it and
# JOLT_DIAG=edn emits it, while the wording beside it stays free to improve. That
# only holds if the set of kinds is known, so this fails when:
#
#   * a kind is RAISED but not registered — an error with no documented meaning,
#     and the thing a new diagnostic forgets;
#   * a kind is REGISTERED but never raised — a registry that accumulates dead
#     entries stops describing the compiler, which is how a doc rots into
#     decoration.
#
# The second direction is the one that is easy to leave out and the one that
# keeps the file honest as diagnostics are reworded, merged or removed.

root="$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)"
cd "$root" || exit 1

reg="test/conformance/error-kinds.edn"
fail=0
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

if [ ! -f "$reg" ]; then
  echo "  FAIL: no registry at $reg"
  echo "error kinds: FAILED"
  exit 1
fi

# Registered kinds: the map's keys. A key is a namespaced keyword at the start of
# a line (the file is written one entry per stanza), which keeps this a text scan
# rather than an edn parse — the gate must run without a working jolt, since a
# broken analyzer is exactly when it matters.
grep -oE '^[{ ]*:[a-z]+/[a-z-]+' "$reg" | tr -d '{ ' | LC_ALL=C sort -u > "$tmp/registered"

# Kinds the sources actually raise. The compiler is not only jolt/: `let`'s
# destructurer and `defn`'s clause check are clojure.core macros that build a
# :jolt/error map of their own, so clojure/core is scanned too — leaving it out
# is the blind spot that would let exactly those diagnostics go unregistered.
#
# Comment lines are dropped first: a kind named in prose (this file's own
# rationale, a "raises :analyze/foo" note beside a call) is documentation, not a
# raise, and counting it would let a registry entry nothing raises survive on the
# strength of a comment mentioning it.
sed 's/;;.*//' jolt-core/jolt/*.clj jolt-core/jolt/passes/*.clj \
               jolt-core/clojure/core/*.clj 2>/dev/null \
  | grep -ohE ':(analyze|read|codegen|aot)/[a-z-]+' \
  | LC_ALL=C sort -u > "$tmp/raised"

# The registry is documentation, so it names each kind in prose too; exclude the
# registry itself from the "raised" scan by never reading it above.
unregistered="$(comm -13 "$tmp/registered" "$tmp/raised")"
if [ -n "$unregistered" ]; then
  echo "  FAIL: raised but not registered in $reg:"
  printf '%s\n' "$unregistered" | sed 's/^/    /'
  fail=1
fi

unraised="$(comm -23 "$tmp/registered" "$tmp/raised")"
if [ -n "$unraised" ]; then
  echo "  FAIL: registered but never raised (delete the entry, or raise it):"
  printf '%s\n' "$unraised" | sed 's/^/    /'
  fail=1
fi

n="$(wc -l < "$tmp/registered" | tr -d ' ')"
if [ "$n" = 0 ]; then
  echo "  FAIL: no kinds found in $reg — the scan pattern no longer matches the file"
  fail=1
fi

[ "$fail" = 0 ] && echo "error kinds: passed ($n kinds, registry and sources agree)" \
                || echo "error kinds: FAILED"
exit $fail
