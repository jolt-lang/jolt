#!/bin/sh
# aot-cache-path smoke (trace-r5): a frame from a CACHED namespace must report an
# EXISTING source path.
#
# compile-file bakes the name of the file it read into the frame source objects.
# The AOT cache used to compile a pid-unique temp (base.tmp<pid>.scm) and rename
# it away, so every frame from a cache hit pointed at a path that no longer
# exists. This check drives a throwing app twice under a fresh JOLT_CACHE_DIR —
# cold (miss) then warm (hit) — and on the WARM run asserts that the path the
# RUNTIME reports for each frame exists on disk and is the final .scm, not a temp.
#
#   before:  (".../boom-74-E212836D-11.tmp12072.scm" 570)      <- missing
#   after:   (".../boom-74-E212836D-11.scm" 3 125)             <- exists
#
# Chez resolves a source object to line and column only when it can read the file,
# which is why the fixed form yields three values where the broken one yielded two.
#
# The path comes from the frame source objects (JOLT_DEBUG_FRAMES prints them),
# NOT from scraping the cached .so. Chez fasls are compressed, so `strings` over
# one returns mangled fragments: on Linux CI an earlier version of this check read
# "W.DXMDTLZWm5/…scm" for a real path of "/tmp/tmp.DXMDTLZWm5/…scm" and failed
# while the runtime was entirely correct. It passed on macOS, which is how it got
# this far. The runtime's own report is also what the trace actually resolves
# offsets against, so it is the right thing to assert on.

set -e

pass=0
fails=0
root="$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)"
cd "$root"

jolt="bin/jolt"
cache="$(mktemp -d)"
tmp="$(mktemp -d)"
mkdir -p "$tmp/src/mylib"

cat > "$tmp/src/mylib/core.clj" <<'EOF'
(ns mylib.core)
;; throw in operand position: a tail-position throw TCO-erases boom's frame from
;; the live continuation, and this check drives the continuation path.
(defn boom [] (+ 1 (throw (ex-info "boom" {}))))
EOF

# Run the throwing app, capturing stderr (the JOLT_DEBUG_FRAMES dump and the
# trace both go to current-error-port). $1 = the cache dir.
# JOLT_TRACE=0: dev mode traces by default, and the tail-frame history ring then
# preempts jolt-frame-records (the continuation walk) — this check drives it.
# JOLT_GUARDED_INLINE=0: untraced, the cold run's -e form would splice boom's body
# into itself (host-contract.ss hc-guarded-inline?) and boom has no frame to map.
run_app() {
  JOLT_AOT_CACHE=1 JOLT_CACHE_DIR="$1" JOLT_QUIET=1 JOLT_DEBUG_FRAMES=1 \
    JOLT_TRACE=0 JOLT_GUARDED_INLINE=0 \
    "$jolt" -e "
      (require 'jolt.deps)
      (jolt.deps/add-deps {:deps {'mylib/mylib {:local/root \"$tmp\"}}})
      (require 'mylib.core)
      (println (mylib.core/boom))" 2>&1 || true
}

# --- (a) cold run: the cache compiles, the app throws -------------------------
out_a="$(run_app "$cache")"
if printf '%s' "$out_a" | grep -q '\[frame\] mylib.core/boom \*MAPPED\*'; then
  echo "PASS: (a) cold run traces the cached frame (mapped)"; pass=$((pass+1))
else
  echo "FAIL: (a) no mapped boom frame in JOLT_DEBUG_FRAMES output"
  printf '%s\n' "$out_a" | head -20
  fails=$((fails+1))
fi

# --- (b) warm run: a cache hit ------------------------------------------------
out_b="$(run_app "$cache")"
if printf '%s' "$out_b" | grep -q '\[frame\] mylib.core/boom \*MAPPED\*'; then
  echo "PASS: (b) warm (cache-hit) run traces the cached frame (mapped)"; pass=$((pass+1))
else
  echo "FAIL: (b) no mapped boom frame on the warm run"
  printf '%s\n' "$out_b" | head -20
  fails=$((fails+1))
fi

# --- (c) the path the RUNTIME reports for a cached frame exists and is final ---
# Read from the frame source objects via JOLT_DEBUG_FRAMES, not by scraping the
# .so. Chez fasls are COMPRESSED, so `strings`/`grep -a` over one yields mangled
# fragments -- on Linux CI this produced "W.DXMDTLZWm5/...scm" for a real path of
# "/tmp/tmp.DXMDTLZWm5/...scm", which read as a missing file and failed the gate
# while the runtime was perfectly correct. It happened to work on macOS. The
# runtime's own report is both the honest source and the thing R3 actually
# resolves offsets against.
paths="$(printf '%s' "$out_b" \
  | sed -n 's/.*source=\([^ ]*\.scm\)@[0-9][0-9]*.*/\1/p' | sort -u)"
if [ -z "$paths" ]; then
  echo "FAIL: (c) no frame reported a .scm source path on the warm run"
  printf '%s\n' "$out_b" | grep '\[frame\]' | head -10
  fails=$((fails+1))
else
  echo "PASS: (c) warm frames report $(printf '%s\n' "$paths" | wc -l | tr -d ' ') source path(s)"
  pass=$((pass+1))
  for p in $paths; do
    case "$p" in
      /*) ;;
      *)  echo "FAIL: (c1) reported path is not absolute: $p"; fails=$((fails+1)); continue ;;
    esac
    if [ -f "$p" ]; then
      echo "PASS: (c2) reported path exists: $p"; pass=$((pass+1))
    else
      echo "FAIL: (c2) reported path does not exist: $p"; fails=$((fails+1))
    fi
    case "$p" in
      *".tmp"*) echo "FAIL: (c3) reported path is a temp: $p"; fails=$((fails+1)) ;;
      *) echo "PASS: (c3) reported path is the final .scm: $p"; pass=$((pass+1)) ;;
    esac
  done
fi

echo
echo "aot-cache-path smoke: $pass passed, $fails failed"
rm -rf "$cache" "$tmp"
[ "$fails" -eq 0 ]
