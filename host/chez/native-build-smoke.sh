#!/bin/sh

# native-build smoke: a program with jolt.native regions runs them as C in a
# binary from jolt build and as their Jolt fallback under jolt run, and the two
# print the same thing — results, coerced arguments, and the exceptions a region
# raises. The binary must also report every region linked (it took the C path),
# and jolt run none (it took the fallback).
root="$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)"
cd "$root"

jolt="${JOLT_BIN:-bin/jolt}"
case "$jolt" in /*) joltabs="$jolt" ;; *) joltabs="$root/$jolt" ;; esac

# Same preflight as static-native-smoke: the build cc-links the region object
# into a relinked launcher, which needs cc and Chez's kernel dev files.
csv="$JOLT_CHEZ_CSV"
if [ -z "$csv" ]; then
  chez_bin="${JOLT_CHEZ:-$(command -v chez || command -v chezscheme || command -v scheme || command -v petite || true)}"
  if [ -n "$chez_bin" ]; then
    base="$(cd "$(dirname "$chez_bin")/.." 2>/dev/null && pwd)"
    for d in "$base"/lib/csv*/*/; do
      [ -f "${d}libkernel.a" ] && csv="${d%/}" && break
    done
  fi
fi
if ! command -v cc >/dev/null 2>&1 || [ -z "$csv" ] || [ ! -f "$csv/scheme.h" ] || [ ! -f "$csv/libkernel.a" ]; then
  echo "native-build smoke: skipped (Chez kernel dev files or C compiler not available)"
  exit 0
fi
export JOLT_CHEZ_CSV="$csv"

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
app="$root/test/chez/native-app"
out="$work/app-bin"

want_results="5.0
[2.0 4.0 6.0]
5
integer overflow
ClassCastException
Index 1 out of bounds for length 1
6
[0 1 4 9]
9.0
gc during region true"

ran="$(cd "$app" && JOLT_PWD="$app" "$joltabs" run -m app.main 2>&1)"
if [ "$ran" != "$want_results
linked (false)" ]; then
  echo "  FAIL: jolt run output"; echo "--- want ---"; echo "$want_results"; echo "linked (false)"
  echo "--- got ----"; echo "$ran"; exit 1
fi

echo "native-build smoke: building"
if ! JOLT_PWD="$app" "$jolt" build -m app.main -o "$out" >"$work/build.log" 2>&1; then
  echo "  FAIL: jolt build exited non-zero"; cat "$work/build.log"; exit 1
fi
grep -q "compiled 7 native region(s)" "$work/build.log" || {
  echo "  FAIL: the build did not compile the seven regions"; cat "$work/build.log"; exit 1; }
got="$(cd / && "$out" 2>&1)"
if [ "$got" != "$want_results
linked (true)" ]; then
  echo "  FAIL: built binary output"; echo "--- want ---"; echo "$want_results"; echo "linked (true)"
  echo "--- got ----"; echo "$got"; exit 1
fi
echo "native-build smoke: OK"
