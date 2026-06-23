#!/bin/sh
# build smoke: `jolt build` compiles a multi-namespace app (macro + cross-ns +
# clojure.string) into a standalone binary, which then runs with no jolt source
# or Chez install on the path — args reach -main, output matches.
root="$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)"
cd "$root"

# Preflight: a standalone build needs Chez's kernel dev files (libkernel.a +
# scheme.h) and a C compiler. A distro chezscheme package ships neither, so on
# such hosts (CI included) skip — like `certify` skips without Clojure. Pin the
# csv dir we validate so the build uses exactly it.
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
  echo "build smoke: skipped (Chez kernel dev files or C compiler not available)"
  exit 0
fi
export JOLT_CHEZ_CSV="$csv"

app="$root/test/chez/build-app"
out="$(mktemp -d)/app-bin"
trap 'rm -rf "$(dirname "$out")"' EXIT

echo "build smoke: compiling app.core -> $out"
if ! JOLT_PWD="$app" bin/joltc build -m app.core -o "$out" >/dev/null 2>&1; then
  echo "  FAIL: jolt build exited non-zero"
  exit 1
fi
[ -x "$out" ] || { echo "  FAIL: no executable produced"; exit 1; }

# Run from a neutral cwd with args. The first line is an embedded resource
# (deps.edn :jolt/build :embed), proving io/resource resolves from the binary with
# no resources/ dir on disk; the rest exercise a macro, cross-ns, and args.
got="$(cd / && "$out" alpha bb ccc 2>&1)"
want='embedded resource ok
HELLO FROM A BUILT BINARY!
HELLO FROM A BUILT BINARY!
args: [alpha bb ccc]
sum: 10'
if [ "$got" != "$want" ]; then
  echo "  FAIL: binary output mismatch"
  echo "--- want ---"; echo "$want"
  echo "--- got ----"; echo "$got"
  exit 1
fi

# Optimized mode (inference + flatten + scalar-replace) must produce the same
# result — a sanity check that the passes don't miscompile this app.
if ! JOLT_PWD="$app" bin/joltc build -m app.core -o "$out" --opt >/dev/null 2>&1; then
  echo "  FAIL: jolt build --opt exited non-zero"; exit 1
fi
got_opt="$(cd / && "$out" alpha bb ccc 2>&1)"
if [ "$got_opt" != "$want" ]; then
  echo "  FAIL: --opt binary output mismatch"
  echo "--- got ----"; echo "$got_opt"
  exit 1
fi
echo "build smoke: passed (release + optimized)"
