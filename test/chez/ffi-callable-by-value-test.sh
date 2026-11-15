#!/bin/sh
# Build the by-value callback C witness outside the tree, run the
# jolt.ffi by-value callback checks against it, and retain the temporary directory on
# failure. Invoke from the repo root:
#   sh test/chez/ffi-callable-by-value-test.sh [jolt]
set -eu

JOLT=${1-bin/jolt}
C=test/chez/ffi-callable-by-value-helper.c
[ -f "$C" ] || { echo "missing $C (run from repo root)" >&2; exit 2; }
[ -x "$JOLT" ] || { echo "missing executable jolt: $JOLT" >&2; exit 2; }

OUT=$(mktemp -d) || exit 1
[ -n "$OUT" ] && [ -d "$OUT" ] || {
  echo "mktemp returned no usable ffi-callable-by-value artifact directory" >&2
  exit 1
}
case "$(uname -s)" in
  Darwin) EXT=dylib ;;
  MINGW*|MSYS*|CYGWIN*) EXT=dll ;;
  *) EXT=so ;;
esac
SO=$OUT/jolt-ffi-callable-by-value-helper.$EXT
cleanup() {
  status=$?
  if [ "$status" -eq 0 ]; then rm -f "$SO"; rmdir "$OUT"
  else echo "retained ffi-callable-by-value artifacts: $OUT" >&2
  fi
}
trap cleanup EXIT
CC_BIN=${CC:-cc}
case "$(uname -s)" in
  Darwin) "$CC_BIN" -std=c11 -dynamiclib -o "$SO" "$C" ;;
  MINGW*|MSYS*|CYGWIN*) "$CC_BIN" -std=c11 -shared -o "$SO" "$C" ;;
  *) "$CC_BIN" -std=c11 -shared -fPIC -o "$SO" "$C" ;;
esac || { echo "cc failed to build $C" >&2; exit 1; }
JOLT_NO_USER_DEPS=1 JOLT_FFI_CALLABLE_BY_VALUE_HELPER=$SO \
  "$JOLT" run test/chez/jolt-ffi-callable-by-value-test.clj
