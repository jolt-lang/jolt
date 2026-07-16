#!/bin/sh
# devboot cache smoke: verify the dev boot cache (target/dev/flat.so) is used
# when fresh, invalidated when source changes, and produces identical behavior.

set -e

pass=0
fails=0
root="$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)"
cd "$root"

joltc="bin/joltc"

# (a) cache used when fresh — JOLT_DEVCACHE prints a marker to stderr.
echo "=== (a) devcache fresh: cache should be used ==="
# Build the cache fresh.
make devboot
out="$(JOLT_DEVCACHE=1 $joltc -e "(+ 1 2)" 2>&1)" || true
if echo "$out" | grep -q "devcache:"; then
  echo "  PASS: cache marker found in stderr"
  pass=$((pass + 1))
else
  echo "  FAIL: cache marker not found in stderr"
  echo "    output: $out"
  fails=$((fails + 1))
fi

# (b) touching any runtime .ss invalidates the cache.
echo "=== (b) touching rt.ss invalidates cache ==="
sleep 1  # ensure timestamp changes
touch host/chez/rt.ss
out="$(JOLT_DEVCACHE=1 $joltc -e "(+ 1 2)" 2>&1)" || true
if echo "$out" | grep -q "devcache:"; then
  echo "  FAIL: cache was used after rt.ss touched"
  fails=$((fails + 1))
else
  echo "  PASS: cache not used after rt.ss touch"
  pass=$((pass + 1))
fi
# Rebuild for next tests.
make devboot

# (c) touching a seed invalidates.
echo "=== (c) touching seed/prelude.ss invalidates cache ==="
sleep 1
touch host/chez/seed/prelude.ss
out="$(JOLT_DEVCACHE=1 $joltc -e "(+ 1 2)" 2>&1)" || true
if echo "$out" | grep -q "devcache:"; then
  echo "  FAIL: cache was used after seed touched"
  fails=$((fails + 1))
else
  echo "  PASS: cache not used after seed touch"
  pass=$((pass + 1))
fi
# Rebuild for next tests.
make devboot

# (d) behavior identical source vs cached.
echo "=== (d) behavior: source vs cached ==="
exprs='
(+ 1 2)
(require (quote clojure.string)) (clojure.string/upper-case "hello")
(defn fib [n] (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))) (fib 10)
'
# Run with cache.
cached="$(JOLT_DEVCACHE=1 $joltc -e "$(echo "$exprs" | tr '\n' ' ')" 2>/dev/null | tail -1)" || true
# Force source by touching a .ss file.
sleep 1
touch host/chez/rt.ss
source_out="$(JOLT_DEVCACHE=1 $joltc -e "$(echo "$exprs" | tr '\n' ' ')" 2>/dev/null | tail -1)" || true
if [ "$cached" = "$source_out" ]; then
  echo "  PASS: cached and source output match"
  pass=$((pass + 1))
else
  echo "  FAIL: output differs"
  echo "    cached: $cached"
  echo "    source: $source_out"
  fails=$((fails + 1))
fi
# Clean up: rebuild so subsequent tests don't inherit the touched state.
make devboot

# (e) project commands under the cache: build an app from a project dir and run
# it. Guards the cached entry's source-roots/JOLT_PWD handling — a drifted
# launcher once resolved no roots and every project build failed.
echo "=== (e) cached project build ==="
projdir="$(mktemp -d)"
mkdir -p "$projdir/src/app"
cat > "$projdir/src/app/core.clj" <<'CLJ'
(ns app.core)
(defn -main [& args] (println "devboot-project-ok"))
CLJ
printf '{:paths ["src"]}' > "$projdir/deps.edn"
build_out="$( (cd "$projdir" && JOLT_DEVCACHE=1 "$root/$joltc" build -m app.core -o "$projdir/app-bin") 2>&1 )" || true
if echo "$build_out" | grep -q "devcache:"; then
  binpath="$projdir/app-bin"
  run_out="$( [ -x "$binpath" ] && "$binpath" 2>&1 )" || true
  if [ "$run_out" = "devboot-project-ok" ]; then
    echo "  PASS: cached build produced a working binary"
    pass=$((pass + 1))
  else
    echo "  FAIL: cached build binary missing or wrong output"
    echo "    build: $build_out"
    echo "    run:   $run_out"
    fails=$((fails + 1))
  fi
else
  echo "  FAIL: cache was not used for the project build"
  echo "    output: $build_out"
  fails=$((fails + 1))
fi
rm -rf "$projdir"

echo ""
echo "devboot smoke: $pass passed, $fails failed"
[ "$fails" -eq 0 ]
