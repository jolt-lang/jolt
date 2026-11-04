#!/bin/sh
# devboot cache smoke: verify the dev boot cache (target/dev/flat.so) is used
# when fresh, invalidated when source changes, and produces identical behavior.

set -e

pass=0
fails=0
root="$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)"
cd "$root"

jolt="bin/jolt"

# (a) cache used when fresh — JOLT_DEVCACHE prints a marker to stderr.
echo "=== (a) devcache fresh: cache should be used ==="
# Build the cache fresh.
make devboot
out="$(JOLT_DEVCACHE=1 $jolt -e "(+ 1 2)" 2>&1)" || true
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
out="$(JOLT_DEVCACHE=1 $jolt -e "(+ 1 2)" 2>&1)" || true
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
out="$(JOLT_DEVCACHE=1 $jolt -e "(+ 1 2)" 2>&1)" || true
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
cached="$(JOLT_DEVCACHE=1 $jolt -e "$(echo "$exprs" | tr '\n' ' ')" 2>/dev/null | tail -1)" || true
# Force source by touching a .ss file.
sleep 1
touch host/chez/rt.ss
source_out="$(JOLT_DEVCACHE=1 $jolt -e "$(echo "$exprs" | tr '\n' ' ')" 2>/dev/null | tail -1)" || true
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
build_out="$( (cd "$projdir" && JOLT_DEVCACHE=1 "$root/$jolt" build -m app.core -o "$projdir/app-bin") 2>&1 )" || true
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

# (f) guard the CLI-AOT regression. The dev boot cache must AOT the CLI closure
# (jolt.main + jolt.deps + their on-demand Clojure requires) into flat.ss as
# emitted Scheme, NOT as top-level (load-namespace …) forms — those re-execute at
# every Sbuild_heap and recompile the whole CLI closure from source on every dev
# invocation (~1.3s). The release build (build-jolt.ss) has always done this AOT;
# make-devboot must share the same bld-emit-cli-aot path. This asserts on the
# EMITTED flat.ss text so the tail coming back fails here without adding a full
# ~90s `make devboot` to the ci gate.
echo "=== (f) flat.ss CLI-AOT section ==="
flat_ss="target/dev/flat.ss"
if [ -f "$flat_ss" ]; then
  # must contain the CLI-AOT section marker bld-emit-cli-aot writes.
  if grep -q '=== AOT jolt.main + jolt.deps (emitted Scheme) ===' "$flat_ss"; then
    echo "  PASS: CLI-AOT section marker present in flat.ss"
    pass=$((pass + 1))
  else
    echo "  FAIL: CLI-AOT section marker missing from flat.ss (bld-emit-cli-aot did not run?)"
    fails=$((fails + 1))
  fi
  # must NOT contain a top-level (load-namespace "jolt.main"/"jolt.deps") line.
  # cli-core.ss's inlined copy is INDENTED (inside jolt-cli-run's body), so the
  # column-0 anchor skips it; only a regression that re-adds the tail matches.
  if grep -nE '^\(load-namespace "jolt\.(main|deps)"\)$' "$flat_ss" >/dev/null; then
    echo "  FAIL: flat.ss has a top-level (load-namespace \"jolt.main\"/\"jolt.deps\") — CLI closure not AOT'd"
    grep -nE '^\(load-namespace "jolt\.(main|deps)"\)$' "$flat_ss" | head | sed 's/^/    /'
    fails=$((fails + 1))
  else
    echo "  PASS: no top-level load-namespace for the CLI closure in flat.ss"
    pass=$((pass + 1))
  fi
else
  echo "  SKIP: $flat_ss does not exist (run 'make devboot' first); CLI-AOT text assertion not run"
fi

# (g) the image vanishing between bin/jolt's freshness check and the load. bin/jolt
# tests flat.so, then cli-devcache.ss loads it a moment later; a parallel
# `make devboot` used to delete the old image before renaming the new one in, and
# a run landing in that gap died printing nothing ("failed for target/dev/flat.so:
# no such file or directory") — aotcachesmoke's case (o) lost its warm run to it
# on CI. The loader must take source mode instead. Driven in a mirror of the
# checkout with no target/, so the image this gate shares with every other one
# is never touched.
echo "=== (g) devcache entry with the image gone falls back to source ==="
mirror="$(mktemp -d)"
for e in "$root"/* "$root"/.[!.]*; do
  [ -e "$e" ] || continue
  case "$(basename "$e")" in target|.git) ;; *) ln -s "$e" "$mirror/$(basename "$e")" ;; esac
done
chez_g="${JOLT_CHEZ:-$(cat target/dev/flat.chez 2>/dev/null)}"
gone_out="$( (cd "$mirror" && JOLT_PWD="$mirror" JOLT_QUIET=1 JOLT_VERSION="${JOLT_VERSION:-dev}" \
              "$chez_g" --script host/chez/cli-devcache.ss -e '(println (+ 1 2))') 2>&1 | tail -1)" || true
rm -rf "$mirror"
if [ "$gone_out" = "3" ]; then
  echo "  PASS: no image at load time ran from source"
  pass=$((pass + 1))
else
  echo "  FAIL: no image at load time: '$gone_out' (expected 3)"
  fails=$((fails + 1))
fi

echo ""
echo "devboot smoke: $pass passed, $fails failed"
[ "$fails" -eq 0 ]
