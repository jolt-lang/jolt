#!/bin/sh
# gateboot-smoke.sh — pin the gate boot image's staleness predicate.
#
# gate-boot.ss loads a precompiled target/dev/gate.so instead of the runtime
# source when its Chez identity and input contents match. Both paths
# behave identically, so a bug here is invisible in gate OUTPUT and shows up only
# as a gate quietly testing code that is no longer on disk. This drives
# gate-boot-image-fresh? directly over synthetic input lists in a temp dir, so it
# needs no runtime boot, touches nothing in the repo, and is safe under parallel
# make. It then checks the real image end to end via the JOLT_GATEBOOT marker.
set -u
root="$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)"
cd "$root"
# JOLT_CHEZ wins (see host/chez/selfcheck.sh) — the Makefile only ever exports
# JOLT_CHEZ, not CHEZ, so this must check it first or silently re-search PATH.
CHEZ="${JOLT_CHEZ:-${CHEZ:-$(command -v chez 2>/dev/null || command -v chezscheme 2>/dev/null || command -v scheme 2>/dev/null)}}"
# Like make, identify the executable used by the isolated gate compiler.
JOLT_CHEZ="$(command -v "$CHEZ")"; export JOLT_CHEZ
pass=0; fail=0
tmp="$(mktemp -d)"; trap 'rm -rf "$tmp"' EXIT

check() { # label expected actual
  if [ "$2" = "$3" ]; then pass=$((pass+1))
  else fail=$((fail+1)); echo "  FAIL: $1 (expected $2, got $3)" >&2; fi
}

# fresh? over a fabricated (image, inputs) pair
probe() { # so inputs -> "#t" | "#f"
  "$CHEZ" --script /dev/stdin <<CHEZEOF 2>/dev/null
(import (chezscheme))
(load "host/chez/gate-boot-fresh.ss")
(display (if (gate-boot-image-fresh? "$1" "$2") "#t" "#f"))
CHEZEOF
}

# The hash of a file, from the same function the predicate and the writer use.
hash_of() {
  "$CHEZ" --script /dev/stdin <<CHEZEOF 2>/dev/null
(import (chezscheme))
(load "host/chez/gate-boot-fresh.ss")
(display (gate-boot-hash-string "$1"))
CHEZEOF
}
toolchain_of() {
  "$CHEZ" --script /dev/stdin <<'CHEZEOF'
(import (chezscheme))
(load "host/chez/gate-boot-fresh.ss")
(display (gate-boot-toolchain-line))
CHEZEOF
}
toolchain_line="$(toolchain_of)"
hash_line() { printf '%s %s\n' "$(hash_of "$1")" "$1"; }
listing() { printf '%s\n' "$toolchain_line"; hash_line "$1"; }

mkdir -p "$tmp/d"
printf 'original\n' > "$tmp/d/in"
sleep 1
: > "$tmp/image.so"                      # the image is NEWER than its input
listing "$tmp/d/in" > "$tmp/ok.inputs"

check "an input with the content it was built from is fresh" \
  "#t" "$(probe "$tmp/image.so" "$tmp/ok.inputs")"
check "blank lines in the list are skipped" "#t" \
  "$(printf '%s\n\n%s\n\n' "$toolchain_line" "$(hash_line "$tmp/d/in")" > "$tmp/blank.inputs"; probe "$tmp/image.so" "$tmp/blank.inputs")"
check "no image is not fresh"       "#f" "$(probe "$tmp/nope.so" "$tmp/ok.inputs")"
check "no input list is not fresh"  "#f" "$(probe "$tmp/image.so" "$tmp/nope.inputs")"
{ printf '%s\n' "$toolchain_line"; printf '%s %s\n' deadbeef "$tmp/d/gone"; } > "$tmp/gone.inputs"
check "a deleted input is not fresh" "#f" "$(probe "$tmp/image.so" "$tmp/gone.inputs")"

# Identity-free hashed receipts are the immediately preceding format: matching
# source hashes alone must not let a stock image hide patched capabilities.
hash_line "$tmp/d/in" > "$tmp/hash-only.inputs"
check "a hashed receipt without a toolchain identity is stale" \
  "#f" "$(probe "$tmp/image.so" "$tmp/hash-only.inputs")"
# Flip only the capability bits: version, machine and executable stay identical.
other_toolchain_line="$(printf '%s\n' "$toolchain_line" | sed 's/#t/@true@/g;s/#f/#t/g;s/@true@/#f/g')"
{ printf '%s\n' "$other_toolchain_line"; hash_line "$tmp/d/in"; } > "$tmp/other-chez.inputs"
check "matching sources with a different compiler identity are stale" \
  "#f" "$(probe "$tmp/image.so" "$tmp/other-chez.inputs")"
: > "$tmp/empty.inputs"
check "an empty receipt is stale" "#f" "$(probe "$tmp/image.so" "$tmp/empty.inputs")"
check "an undiscoverable selected executable is stale" "#f" \
  "$(JOLT_CHEZ="$tmp/no-chez" probe "$tmp/image.so" "$tmp/ok.inputs")"
# A command-name override is supported by make and build.ss, too. Resolve it to
# the same executable rather than giving up cache reuse merely for this spelling.
chez_name="$(basename "$JOLT_CHEZ")"
check "a bare selected executable reuses the same receipt" "#t" \
  "$(PATH="$(dirname "$JOLT_CHEZ"):$PATH" JOLT_CHEZ="$chez_name" probe "$tmp/image.so" "$tmp/ok.inputs")"

# Replacing a compiler at the same path must invalidate even if its version and
# exposed capabilities stay the same. Only the private copy is changed here.
cp "$JOLT_CHEZ" "$tmp/chez"
chmod u+w "$tmp/chez"
{ (JOLT_CHEZ="$tmp/chez" toolchain_of); printf '\n'; hash_line "$tmp/d/in"; } > "$tmp/compiler.inputs"
check "matching compiler bytes at the selected path are fresh" "#t" \
  "$(JOLT_CHEZ="$tmp/chez" probe "$tmp/image.so" "$tmp/compiler.inputs")"
printf 'changed compiler bytes\n' >> "$tmp/chez"
check "changed compiler bytes at the same path are stale" "#f" \
  "$(JOLT_CHEZ="$tmp/chez" probe "$tmp/image.so" "$tmp/compiler.inputs")"

# THE ONE THAT MATTERS. Content changed, mtime left OLDER than the image — which
# is what a build overlapping an edit produces, and what a preserved mtime
# (cp -p, tar -x, a restored revision) produces. The mtime predicate this
# replaced called it fresh, so the gate ran code that was no longer on disk.
printf 'changed\n' > "$tmp/d/in"
touch -t 202601010000 "$tmp/d/in"
check "changed content with an OLDER mtime is stale" \
  "#f" "$(probe "$tmp/image.so" "$tmp/ok.inputs")"

# ...and restoring the exact bytes makes it fresh again, mtime notwithstanding:
# the question is content, so an edit and its undo leave the image usable.
printf 'original\n' > "$tmp/d/in"
touch -t 202601010000 "$tmp/d/in"
check "restored content is fresh again, whatever the mtime" \
  "#t" "$(probe "$tmp/image.so" "$tmp/ok.inputs")"

# A list from an older build has bare paths and no hash to check, so it cannot
# be trusted; rebuilding is the safe reading.
printf '%s\n' "$tmp/d/in" > "$tmp/legacy.inputs"
check "a bare-path list from an older build is not fresh" \
  "#f" "$(probe "$tmp/image.so" "$tmp/legacy.inputs")"

# End to end: a gate consults the predicate and actually loads the image.
#
# Rebuild it first rather than trusting the one make built: devboot-smoke.sh
# touches host/chez/rt.ss and seed/prelude.ss to test ITS cache invalidation, and
# both are inputs here, so under `make -j` the image can go stale between
# gateboot and this check. That is correct behavior (the gate just falls back to
# source), but it is not something to assert against. Rebuild, then assert only
# if the predicate agrees the image is fresh — so a concurrent touch skips this
# case instead of failing it.
"$CHEZ" --script host/chez/make-gateboot.ss >/dev/null 2>&1
if [ "$(probe target/dev/gate.so target/dev/gate.inputs)" = "#t" ]; then
  out="$(JOLT_GATEBOOT=1 "$CHEZ" --script host/chez/run-infer.ss 2>&1)"
  case "$out" in
    *"gateboot: using target/dev/gate.so"*) pass=$((pass+1)) ;;
    *) fail=$((fail+1)); echo "  FAIL: a gate did not report using the fresh image" >&2 ;;
  esac
  case "$out" in
    *"infer gate:"*"passed"*) pass=$((pass+1)) ;;
    *) fail=$((fail+1)); echo "  FAIL: the gate did not pass off the image" >&2 ;;
  esac
else
  echo "  (end-to-end skipped: image went stale — a concurrent gate touched a runtime source)"
fi

echo "gateboot smoke: $pass passed, $fail failed"
[ "$fail" -eq 0 ]
