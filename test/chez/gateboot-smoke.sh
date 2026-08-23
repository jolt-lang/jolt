#!/bin/sh
# gateboot-smoke.sh — pin the gate boot image's staleness predicate.
#
# gate-boot.ss loads a precompiled target/dev/gate.so instead of the runtime
# source when the image is newer than everything that went into it. Both paths
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

mkdir -p "$tmp/d"
: > "$tmp/d/older"                       # an input, then the image, then a newer input
sleep 1
: > "$tmp/image.so"
printf '%s\n' "$tmp/d/older" > "$tmp/ok.inputs"
check "image newer than its inputs is fresh"      "#t" "$(probe "$tmp/image.so" "$tmp/ok.inputs")"
check "blank lines in the list are skipped"       "#t" "$(printf '\n%s\n\n' "$tmp/d/older" > "$tmp/blank.inputs"; probe "$tmp/image.so" "$tmp/blank.inputs")"
check "no image is not fresh"                     "#f" "$(probe "$tmp/nope.so" "$tmp/ok.inputs")"
check "no input list is not fresh"                "#f" "$(probe "$tmp/image.so" "$tmp/nope.inputs")"
printf '%s\n' "$tmp/d/gone" > "$tmp/gone.inputs"
check "a deleted input is not fresh"              "#f" "$(probe "$tmp/image.so" "$tmp/gone.inputs")"
sleep 1
: > "$tmp/d/newer"
printf '%s\n%s\n' "$tmp/d/older" "$tmp/d/newer" > "$tmp/stale.inputs"
check "an input newer than the image is stale"    "#f" "$(probe "$tmp/image.so" "$tmp/stale.inputs")"

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
