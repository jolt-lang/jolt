#!/bin/sh
# seed-statics.sh — every Class/member call and (new Class) the seed emits
# resolves on the Gambit boot (POSIX sh). The driver for seed-statics.ss;
# `make gambitstatics`.
#
# grep pulls the emitted names out of the two seed files (an interpreted gsi
# scan of 3 MB of text takes minutes; grep takes milliseconds) and the Scheme
# half boots and asks the registries. The name grammar is the emitter's: a
# class or member is identifier characters only, which is what keeps the
# compiler image's own format strings — where the text after
# (host-static-call " is Scheme code — out of the list. Flags pass through as
# JOLT_GAMBITSTATICS (gsi loads every argument as a file):
#   --regen   rewrite host/gambit/seed-statics-allowlist.txt
#   --list    print every emitted static and constructor and its status
#
# Resolution as in the other gambit gates: JOLT_GSI (the Makefile hands down
# $(GAMBIT_GSI)), else brew's gambit-scheme. NEVER bare gsi.
root="$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)"
cd "$root" || exit 1

if [ -z "${JOLT_GSI:-}" ]; then
  prefix="$(brew --prefix gambit-scheme 2>/dev/null)"
  JOLT_GSI="$prefix/bin/gsi"
fi
if [ ! -x "$JOLT_GSI" ]; then
  echo "gambit statics: no gsi found (JOLT_GSI or brew gambit-scheme)" >&2
  exit 1
fi

mode=""
for a in "$@"; do
  case "$a" in
    --regen) mode=regen ;;
    --list) mode=list ;;
    *) echo "gambit statics: unknown flag $a" >&2; exit 2 ;;
  esac
done

refs="$(mktemp)"
trap 'rm -f "$refs"' EXIT
name='[A-Za-z0-9_.$-]*'
{
  grep -ohE "\((host-static-call|host-static-ref) \"$name\" \"$name\"" \
    host/gambit/seed/prelude.ss host/gambit/seed/image.ss \
    | sed -E 's/^\((host-static-call|host-static-ref) "([^"]*)" "([^"]*)"$/\2\/\3/'
  grep -ohE "\(host-new \"$name\"" host/gambit/seed/prelude.ss host/gambit/seed/image.ss \
    | sed -E 's/^\(host-new "([^"]*)"$/new \1/'
} | LC_ALL=C sort -u > "$refs"

JOLT_GAMBITSTATICS="$mode" JOLT_GAMBITSTATICS_REFS="$refs" \
  "$JOLT_GSI" host/gambit/seed-statics.ss < /dev/null
