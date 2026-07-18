#!/bin/sh
# Re-mint the checked-in Chez seed. Run after changing a seed source — the reader
# (host/chez/reader.ss), the analyzer/IR/backend (jolt-core/jolt/*.clj), or the
# clojure.core overlay (jolt-core/clojure/core/*.clj). Iterates bootstrap.ss from the
# current seed to a byte-fixpoint and overwrites host/chez/seed/.
set -e
root="$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)"
cd "$root"
CHEZ="${CHEZ:-$(command -v chez || command -v chezscheme || command -v scheme)}"
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

cp host/chez/seed/prelude.ss "$tmp/cur-p.ss"
cp host/chez/seed/image.ss   "$tmp/cur-i.ss"
i=0
while [ "$i" -lt 8 ]; do
  i=$((i + 1))
  # capture stderr so the fixpoint pass can be checked for skipped forms; the
  # skip count is only trustworthy once converged (an earlier pass compiling off
  # an older seed may skip a form that a later pass, off the rebuilt seed, emits).
  "$CHEZ" --script host/chez/bootstrap.ss \
    "$tmp/cur-p.ss" "$tmp/cur-i.ss" "$tmp/new-p.ss" "$tmp/new-i.ss" >/dev/null 2>"$tmp/err"
  if diff -q "$tmp/cur-p.ss" "$tmp/new-p.ss" >/dev/null \
     && diff -q "$tmp/cur-i.ss" "$tmp/new-i.ss" >/dev/null; then
    skipped=$(sed -n 's/^mint: \([0-9][0-9]*\) form(s) skipped$/\1/p' "$tmp/err" | tail -1)
    if [ -n "$skipped" ] && [ "$skipped" -ne 0 ]; then
      echo "re-mint: $skipped form(s) failed to compile in the fixpoint pass:" >&2
      grep '^mint: skipped ' "$tmp/err" >&2
      exit 1
    fi
    cp "$tmp/new-p.ss" host/chez/seed/prelude.ss
    cp "$tmp/new-i.ss" host/chez/seed/image.ss
    echo "re-minted seed (converged after $i pass(es))"
    exit 0
  fi
  cp "$tmp/new-p.ss" "$tmp/cur-p.ss"
  cp "$tmp/new-i.ss" "$tmp/cur-i.ss"
done
echo "re-mint did not converge in 8 passes" >&2
exit 1
