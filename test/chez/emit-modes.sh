#!/bin/sh
# The emission modes' own semantics checks, each under
# the modes it covers:
#   clj-call-redef-test.clj   var-call redefinition, default and global var calls
#   global-var-call-test.clj  JOLT_GLOBAL_VAR_CALL: evaluation order, remove-ns
#                             and reload, concurrent redefinition
#   lazy-meta-var-test.clj    JOLT_LAZY_META: a #'var inside def metadata
#   global-var-cache-order.sh global var calls across AOT-cached namespaces
#   lazy-quoted-data.sh       JOLT_LAZY_META / JOLT_CONST_DATA, cold and warm
# Keep-frames is the default; each .clj test also runs with JOLT_KEEP_FRAMES=0.
#   JOLT_BIN=target/release/jolt sh test/chez/emit-modes.sh
root="$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)"
jolt="${JOLT_BIN:-$root/target/release/jolt}"
case "$jolt" in /*) ;; *) jolt="$root/$jolt" ;; esac
work="$(mktemp -d)"; trap 'rm -rf "$work"' EXIT INT TERM
fails=0
run() {   # test-file env...
  t=$1; shift
  out=$(cd "$work" && env "$@" JOLT_CACHE_DIR="$work/cache" "$jolt" run "$root/test/chez/$t" 2>&1)
  st=$?
  if [ $st -eq 0 ]; then
    echo "emitmodes: $t [$*] $(printf '%s\n' "$out" | tail -n 1)"
  else
    echo "emitmodes: $t [$*] FAIL (exit $st)"; printf '%s\n' "$out" | tail -n 20
    fails=$((fails + 1))
  fi
}
for kf in 1 0; do
  run clj-call-redef-test.clj JOLT_KEEP_FRAMES=$kf
  run clj-call-redef-test.clj JOLT_KEEP_FRAMES=$kf JOLT_GLOBAL_VAR_CALL=1
  run global-var-call-test.clj JOLT_KEEP_FRAMES=$kf
  run global-var-call-test.clj JOLT_KEEP_FRAMES=$kf JOLT_GLOBAL_VAR_CALL=1
  run lazy-meta-var-test.clj JOLT_KEEP_FRAMES=$kf
  run lazy-meta-var-test.clj JOLT_KEEP_FRAMES=$kf JOLT_LAZY_META=1
  run lazy-meta-var-test.clj JOLT_KEEP_FRAMES=$kf JOLT_LAZY_META=meta
done
JOLT_BIN="$jolt" sh "$root/test/chez/global-var-cache-order.sh" || fails=$((fails + 1))
JOLT_BIN="$jolt" sh "$root/test/chez/lazy-quoted-data.sh" || fails=$((fails + 1))
if [ "$fails" -eq 0 ]; then echo "emitmodes: passed"; else echo "emitmodes: $fails failure(s)"; exit 1; fi
