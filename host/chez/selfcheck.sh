#!/bin/sh
# self-host fixpoint gate: bootstrap.ss rebuilds the prelude + compiler image from
# source on pure Chez; the rebuild must equal the checked-in seed byte-for-byte. If
# it doesn't, a seed source changed without a re-mint — run `make remint`.
set -e
root="$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)"
cd "$root"
CHEZ="${CHEZ:-$(command -v chez || command -v chezscheme || command -v scheme)}"
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT
"$CHEZ" --script host/chez/bootstrap.ss \
  host/chez/seed/prelude.ss host/chez/seed/image.ss "$tmp/p.ss" "$tmp/i.ss" >/dev/null
if diff -q host/chez/seed/prelude.ss "$tmp/p.ss" >/dev/null \
   && diff -q host/chez/seed/image.ss "$tmp/i.ss" >/dev/null; then
  echo "self-host fixpoint: rebuild == checked-in seed"
else
  echo "self-host FAILED: bootstrap rebuild != checked-in seed; run 'make remint'" >&2
  exit 1
fi
