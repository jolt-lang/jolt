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

# Closed-world direct-linking (opt-in): same result, and the cross-namespace call
# (app.core -> app.util/shout) must lower to a direct jv$ binding, not var-deref.
if ! JOLT_PWD="$app" bin/joltc build -m app.core -o "$out" --direct-link >/dev/null 2>&1; then
  echo "  FAIL: jolt build --direct-link exited non-zero"; exit 1
fi
got_dl="$(cd / && "$out" alpha bb ccc 2>&1)"
if [ "$got_dl" != "$want" ]; then
  echo "  FAIL: --direct-link binary output mismatch"
  echo "--- got ----"; echo "$got_dl"
  exit 1
fi
if ! grep -q '(jv\$app.util\$shout' "$out.build/flat.ss"; then
  echo "  FAIL: --direct-link did not emit a direct app->app call"; exit 1
fi
# Tree-shaking (opt-in): same result, and an unreachable def (the `twice` macro,
# expanded at AOT and never called at runtime) is dropped.
if ! JOLT_PWD="$app" bin/joltc build -m app.core -o "$out" --tree-shake >/dev/null 2>&1; then
  echo "  FAIL: jolt build --tree-shake exited non-zero"; exit 1
fi
got_ts="$(cd / && "$out" alpha bb ccc 2>&1)"
if [ "$got_ts" != "$want" ]; then
  echo "  FAIL: --tree-shake binary output mismatch"
  echo "--- got ----"; echo "$got_ts"
  exit 1
fi
if grep -q 'def-var! "app.util" "twice"' "$out.build/flat.ss"; then
  echo "  FAIL: --tree-shake did not drop the unreachable twice macro"; exit 1
fi
# The app never evals, so the compiler image (analyzer/back end) is dropped.
if grep -q 'def-var! "jolt.analyzer"' "$out.build/flat.ss"; then
  echo "  FAIL: --tree-shake kept the compiler image in a no-eval app"; exit 1
fi
echo "build smoke: passed (release + optimized + direct-link + tree-shake + compiler-drop)"
