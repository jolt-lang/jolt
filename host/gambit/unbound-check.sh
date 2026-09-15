#!/bin/sh
# unbound-check.sh — every Scheme global the Gambit boot references is defined
# (POSIX sh). The driver for host/gambit/unbound-check.ss; `make gambitunbound`.
#
# Regenerates the full-profile boot, compiles it to the js target (the shipped
# web REPL's target, and the one whose base library is what the linker checks
# against), then links with -warnings, which is where Gambit reports every
# global referenced by the unit and defined nowhere. The link log goes to the
# Chez script, which subtracts what the boot defines at runtime and compares
# the rest with the allowlist. Flags pass through:
#   --regen   rewrite host/gambit/unbound-allowlist.txt from the report
#   --list    print every unbound name and its classification
#
# Compile only, no C compiler and no node: `gsc -c` writes JavaScript text and
# `-link` the link file. About 75s on the full boot — the seed is what takes
# the time.
#
# Resolution mirrors the other gambit gates: JOLT_GSC (the Makefile hands down
# $(GAMBIT_GSC)), else brew's gambit-scheme. NEVER bare gsc, which is
# Ghostscript on a brew machine. JOLT_CHEZ as in mirror-drift-check.sh.
root="$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)"
cd "$root" || exit 1

if [ -z "${JOLT_GSC:-}" ]; then
  prefix="$(brew --prefix gambit-scheme 2>/dev/null)"
  JOLT_GSC="$prefix/bin/gsc"
fi
if [ ! -x "$JOLT_GSC" ]; then
  echo "gambit unbound: no gsc found (JOLT_GSC or brew gambit-scheme)" >&2
  exit 1
fi

if [ -z "${JOLT_CHEZ:-}" ]; then
  for c in chez chezscheme; do
    if command -v "$c" >/dev/null 2>&1; then
      JOLT_CHEZ="$c"
      break
    fi
  done
  if [ -z "${JOLT_CHEZ:-}" ]; then
    echo "gambit unbound: no Chez Scheme executable found on PATH" >&2
    exit 1
  fi
fi

tmp="$(mktemp -d)" || exit 1
trap 'rm -rf "$tmp"' EXIT INT TERM

"$JOLT_CHEZ" --script host/gambit/gen-boot.ss full > "$tmp/gen-boot.log" 2>&1 || {
  cat "$tmp/gen-boot.log" >&2
  echo "gambit unbound: gen-boot.ss failed" >&2
  exit 1
}

# From host/gambit: boot-full.ss ##includes its files relative to itself, and
# irregex.scm's own includes resolve from there too.
if ! (cd host/gambit && "$JOLT_GSC" -target js -c -o "$tmp/boot.js" boot-full.ss) \
     > "$tmp/compile.log" 2>&1; then
  tail -40 "$tmp/compile.log" >&2
  echo "gambit unbound: the boot does not compile — see above" >&2
  exit 1
fi

if ! "$JOLT_GSC" -target js -warnings -link -o "$tmp/boot_.js" "$tmp/boot.js" \
     > "$tmp/link.log" 2>&1; then
  tail -40 "$tmp/link.log" >&2
  echo "gambit unbound: the link step failed — see above" >&2
  exit 1
fi

"$JOLT_CHEZ" --script host/gambit/unbound-check.ss "$tmp/link.log" "$@"
