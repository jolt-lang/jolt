#!/bin/sh
# Build the stack-argument C witness outside the tree and run
# test/chez/ffi-stack-args-test.ss on it (chez-patches/0006); the temporary
# directory is kept on failure.
set -eu

CHEZ=${1-}
[ -n "$CHEZ" ] || { echo "usage: $0 <chez>" >&2; exit 2; }
C=test/chez/ffi-stack-args-helper.c
[ -f "$C" ] || { echo "missing $C (run from repo root)" >&2; exit 2; }

OUT=$(mktemp -d) || exit 1
[ -n "$OUT" ] && [ -d "$OUT" ] || {
  echo "mktemp returned no usable ffi-stack-args artifact directory" >&2
  exit 1
}
case "$(uname -s)" in
  Darwin) EXT=dylib ;;
  MINGW*|MSYS*|CYGWIN*) EXT=dll ;;
  *) EXT=so ;;
esac
SO=$OUT/jolt-ffi-stack-args-helper.$EXT
cleanup() {
  status=$?
  if [ "$status" -eq 0 ]; then rm -f "$SO"; rmdir "$OUT"
  else echo "retained ffi-stack-args artifacts: $OUT" >&2
  fi
}
trap cleanup EXIT
CC_BIN=${CC:-cc}
case "$(uname -s)" in
  Darwin) "$CC_BIN" -std=c11 -dynamiclib -o "$SO" "$C" ;;
  MINGW*|MSYS*|CYGWIN*) "$CC_BIN" -std=c11 -shared -o "$SO" "$C" ;;
  *) "$CC_BIN" -std=c11 -shared -fPIC -o "$SO" "$C" ;;
esac || { echo "cc failed to build $C" >&2; exit 1; }
JOLT_FFI_STACK_ARGS_HELPER=$SO "$CHEZ" --script test/chez/ffi-stack-args-test.ss
