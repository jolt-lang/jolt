#!/bin/sh
# build-lib smoke: `jolt build --library` compiles an app into a shared object an
# embedder loads with dlopen and calls via jolt_library_init + jolt_lookup. This
# proves the managed-runtime library works as a C ABI target.
root="$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)"
cd "$root"

# Preflight: same as build-smoke — a library build needs Chez's kernel dev files
# (libkernel.a + scheme.h) and a C compiler. Skip cleanly where absent.
csv="$JOLT_CHEZ_CSV"
if [ -z "$csv" ]; then
  chez_bin="$(command -v chez || command -v scheme || command -v petite || true)"
  if [ -n "$chez_bin" ]; then
    base="$(cd "$(dirname "$chez_bin")/.." 2>/dev/null && pwd)"
    for d in "$base"/lib/csv*/*/; do
      [ -f "${d}libkernel.a" ] && csv="${d%/}" && break
    done
  fi
fi
if ! command -v cc >/dev/null 2>&1 || [ -z "$csv" ] || [ ! -f "$csv/scheme.h" ] || [ ! -f "$csv/libkernel.a" ]; then
  echo "build-lib smoke: skipped (Chez kernel dev files or C compiler not available)"
  exit 0
fi
export JOLT_CHEZ_CSV="$csv"

app="$root/test/chez/build-lib"
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

case "$(uname -s)" in
  Darwin) lib="$work/libadd.dylib" ;;
  *)      lib="$work/libadd.so" ;;
esac

echo "build-lib smoke: compiling libadd.core -> $lib"
build_out="$(JOLT_PWD="$app" bin/joltc build --library -m libadd.core -o "$lib" 2>&1)"
if [ ! -f "$lib" ]; then
  # A shared object folds Chez's libkernel.a in, so that archive must be PIC. A
  # kernel built without -fPIC (the common default, incl. a stock source build)
  # fails the -shared link with a relocation error — an environment limitation,
  # not a jolt bug, so skip like the missing-toolchain case above.
  if printf '%s' "$build_out" | grep -qiE 'recompile with .*-fPIC|can not be used when making a shared object|relocation R_'; then
    echo "build-lib smoke: skipped (Chez libkernel.a is not position-independent; a shared library needs a PIC kernel)"
    exit 0
  fi
  echo "  FAIL: jolt build --library produced no shared library"
  printf '%s\n' "$build_out"
  exit 1
fi

echo "build-lib smoke: compiling driver + calling add(2,3) through dlopen"
if ! cc -O2 "$app/driver.c" -ldl -o "$work/driver" 2>"$work/driver.err"; then
  echo "  FAIL: driver compile failed"; cat "$work/driver.err"; exit 1
fi
got="$("$work/driver" "$lib" 2>&1)"; rc=$?
if [ "$got" != "5" ] || [ "$rc" != "0" ]; then
  echo "  FAIL: add(2,3) — want '5' rc 0, got '$got' rc $rc"; exit 1
fi

echo "build-lib smoke: passed (library built, add(2,3)=5 via dlopen+jolt_lookup)"
