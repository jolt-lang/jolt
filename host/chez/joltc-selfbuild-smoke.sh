#!/bin/sh
# joltc self-build smoke (jolt-eaj): build joltc as a self-contained binary, then
# use THAT binary to compile a jolt app with Chez and cc removed from the
# environment — the whole point of the feature. The produced app must then run
# and match the same expected output as build-smoke.sh.
root="$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)"
cd "$root"

# Preflight: building joltc itself needs the Chez kernel dev files (libkernel.a +
# scheme.h) and a C compiler, same as build-smoke.sh. A distro chezscheme package
# ships neither, so skip there (CI included).
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
  echo "joltc self-build smoke: skipped (Chez kernel dev files or C compiler not available)"
  exit 0
fi
export JOLT_CHEZ_CSV="$csv"

# 1. Build joltc (debug profile — faster; the self-contained app-build mechanism
# is identical to release, only Chez compile settings differ).
joltc="$root/target/debug/joltc"
echo "joltc self-build smoke: building $joltc"
if ! chez --script host/chez/build-joltc.ss debug "$joltc" >/dev/null 2>&1; then
  echo "  FAIL: build-joltc.ss exited non-zero"
  exit 1
fi
[ -x "$joltc" ] || { echo "  FAIL: no joltc executable produced"; exit 1; }

# 2. The distributed joltc must run with no Chez install: a basic eval.
got_e="$(env -i HOME="$HOME" "$joltc" -e '(reduce + (range 10))' 2>&1)"
if [ "$got_e" != "45" ]; then
  echo "  FAIL: joltc -e under empty env gave '$got_e', want 45"
  exit 1
fi

# 3. Build an app through the distributed joltc with an EMPTY environment — no
# PATH at all, so no chez, no cc, no shell tools are reachable. This is the core
# guarantee: joltc compiles apps entirely on its own.
app="$(mktemp -d)/build-app"
cp -r "$root/test/chez/build-app" "$app"
out="$app/app"
echo "joltc self-build smoke: compiling app.core via the binary (no chez/cc on PATH)"
if ! env -i HOME="$HOME" JOLT_PWD="$app" "$joltc" build -m app.core -o "$out" >/dev/null 2>&1; then
  echo "  FAIL: self-contained jolt build exited non-zero"
  rm -rf "$(dirname "$app")"
  exit 1
fi
[ -x "$out" ] || { echo "  FAIL: no app executable produced"; rm -rf "$(dirname "$app")"; exit 1; }

# 4. The produced app runs from a neutral cwd and matches build-smoke's output.
got="$(cd / && "$out" alpha bb ccc 2>&1)"
want='embedded resource ok
HELLO FROM A BUILT BINARY!
HELLO FROM A BUILT BINARY!
args: [alpha bb ccc]
sum: 10
greet-default: greet:default
greet-loud: greet:loud
greet-soft: greet:soft'
rm -rf "$(dirname "$app")"
if [ "$got" != "$want" ]; then
  echo "  FAIL: produced app output mismatch"
  echo "--- want ---"; echo "$want"
  echo "--- got ----"; echo "$got"
  exit 1
fi
echo "joltc self-build smoke: passed (joltc runs + builds a working app with no external toolchain)"
