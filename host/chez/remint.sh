#!/bin/sh
# Re-mint the checked-in Chez seed. Run after changing a seed source — the reader
# (host/chez/reader.ss), the analyzer/IR/backend (jolt-core/jolt/*.clj), or the
# clojure.core overlay (jolt-core/clojure/core/*.clj). Iterates bootstrap.ss from the
# current seed to a byte-fixpoint and overwrites host/chez/seed/.
set -e
root="$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)"
cd "$root"
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

cp host/chez/seed/prelude.ss "$tmp/cur-p.ss"
cp host/chez/seed/image.ss   "$tmp/cur-i.ss"
i=0
while [ "$i" -lt 8 ]; do
  i=$((i + 1))
  chez --script host/chez/bootstrap.ss \
    "$tmp/cur-p.ss" "$tmp/cur-i.ss" "$tmp/new-p.ss" "$tmp/new-i.ss" >/dev/null
  if diff -q "$tmp/cur-p.ss" "$tmp/new-p.ss" >/dev/null \
     && diff -q "$tmp/cur-i.ss" "$tmp/new-i.ss" >/dev/null; then
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
