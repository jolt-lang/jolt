#!/bin/sh
# mm-epoch-check.sh -- every jolt-mm-epoch bump goes through mm-epoch-bump!.
#
# The multimethod caches stamp an entry with the epoch it was read under and
# trust it while the epoch is unchanged. That holds only if a reader that sees a
# new epoch also sees the table change behind it, which on arm64 takes a release
# fence between the change and the bump (mm-epoch-bump!, multimethods.ss) and an
# acquire fence on the reader's side. A bare (set! jolt-mm-epoch ...) anywhere
# else skips the release half, and the failure is a superseded method answered
# for good, a few times in a few hundred thousand updates.
set -eu
cd "$(dirname "$0")/../.."
hits=$(grep -n 'set! jolt-mm-epoch' host/chez/*.ss host/chez/java/*.ss host/gambit/*.ss 2>/dev/null \
         | grep -v '^host/gambit/records-gambit.ss:' || true)
# the helper's own store is the one sanctioned site
bad=$(printf '%s\n' "$hits" | grep -v '^host/chez/multimethods.ss:[0-9]*:  (set! jolt-mm-epoch (fx+ jolt-mm-epoch 1)))$' | grep . || true)
n=$(printf '%s\n' "$hits" | grep -c . || true)
if [ "$n" != 1 ] || [ -n "$bad" ]; then
  echo "mmepochcheck: bump jolt-mm-epoch through mm-epoch-bump! only:" >&2
  printf '%s\n' "$bad" >&2
  exit 1
fi
# the generated Gambit copy of records-dispatch.ss must be current too
if grep -q 'set! jolt-mm-epoch' host/gambit/records-gambit.ss; then
  echo "mmepochcheck: host/gambit/records-gambit.ss bumps the epoch directly (make gambitgen)" >&2
  exit 1
fi
echo "mmepochcheck: every multimethod epoch bump is fenced"
