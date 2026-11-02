#!/bin/sh
# aot-cache-star-file smoke: inside a required namespace, *file* and
# *source-path* name that namespace's source, the same answer cold (compiled
# from source) and warm (loaded from the AOT cache) (#1466).
#
# A source load binds both around the namespace's forms (ldr-with-file-vars),
# as Compiler.load does. The compiled entrances — a cached .so, an embedded
# fasl, a classpath artifact — bound only the compiler-flag vars, so a warm
# namespace's top level read its REQUIRER's *file*: a (def here *file*) named
# the requiring script on every run after the first.
#
# The value is the classpath name, as on the JVM (Compiler.load gets RT.load's
# resource name): "mylib/here.clj", not the path on disk.

set -e

pass=0
fails=0
root="$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)"
jolt="$root/target/release/jolt"
cache="$(mktemp -d)"
tmp="$(cd "$(mktemp -d)" && pwd -P)"
trap 'rm -rf "$cache" "$tmp"' EXIT
mkdir -p "$tmp/proj/src/mylib"

cat > "$tmp/proj/src/mylib/here.clj" <<'EOF'
(ns mylib.here)
(def file *file*)
(def spath *source-path*)
EOF
echo '{:paths ["src"]}' > "$tmp/proj/deps.edn"
cat > "$tmp/proj/q.clj" <<'EOF'
(require 'mylib.here)
(println mylib.here/file)
(println mylib.here/spath)
EOF

# JOLT_AOT_ASYNC=0: the miss compiles in-process, so the next run is a hit
# rather than racing a detached worker.
run() {
  (cd "$tmp/proj" && JOLT_AOT_CACHE="$1" JOLT_AOT_ASYNC=0 JOLT_CACHE_DIR="$cache" \
     JOLT_QUIET=1 "$jolt" q.clj 2>&1) || true
}
hits() {
  (cd "$tmp/proj" && JOLT_AOT_CACHE=1 JOLT_AOT_ASYNC=0 JOLT_CACHE_DIR="$cache" \
     JOLT_QUIET=1 JOLT_DEBUG=1 "$jolt" q.clj 2>&1 >/dev/null) \
    | grep -c '\[jolt.aot\] hit mylib.here' || true
}

want='mylib/here.clj
here.clj'

check() {
  label="$1"; out="$2"
  if [ "$out" = "$want" ]; then
    echo "PASS: ($label) *file* and *source-path* name the namespace's source"
    pass=$((pass+1))
  else
    echo "FAIL: ($label)"
    echo "  got:";  printf '%s\n' "$out"  | sed 's/^/    /'
    echo "  want:"; printf '%s\n' "$want" | sed 's/^/    /'
    fails=$((fails+1))
  fi
}

# (a) uncached
check a "$(run 0)"
# (b) cold: the miss that fills the cache
check b "$(run 1)"
# (c) warm: served from the cache, same answer
n="$(hits)"
if [ "$n" -ge 1 ]; then
  echo "PASS: (c) the warm run is a cache hit"; pass=$((pass+1))
else
  echo "FAIL: (c) the warm run recompiled instead of hitting the cache"
  fails=$((fails+1))
fi
check c "$(run 1)"

echo
echo "aot-cache-star-file smoke: $pass passed, $fails failed"
[ "$fails" -eq 0 ]
