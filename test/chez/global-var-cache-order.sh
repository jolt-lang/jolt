#!/bin/sh
# Global var calls across namespaces loaded from the AOT cache in a different
# order than they were compiled in (JOLT_GLOBAL_VAR_CALL, rt.ss "global vars").
# gx.main1 compiles everything with gx.a loaded first; gx.main2 then loads gx.c
# from the cache before gx.a exists. Prints the same lines with the knob on and
# off; exits non-zero on any difference from the expected output.
#   JOLT_BIN=target/release/jolt sh test/chez/global-var-cache-order.sh
root="$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)"
jolt="${JOLT_BIN:-$root/target/release/jolt}"
case "$jolt" in /*) ;; *) jolt="$root/$jolt" ;; esac
work="$(mktemp -d)"; trap 'rm -rf "$work"' EXIT INT TERM
cp -R "$root/test/chez/global-var-cache-order/." "$work/"
cd "$work" || exit 2
expected='main1 [:a-f 1] [:a-f 2] [:a-g 3] [:a-g 4]
main1 [:a-f 1] [:a-f 2] [:a-g 3] [:a-g 4]
main2 before [java.lang.IllegalStateException Attempting to call unbound fn: #'"'"'gx.a/g] clojure.lang.Var$Unbound
main2 after a [:a-g 2] [:a-g 3]
main2 after b [:a-f 4]'
fails=0
for gv in "" 1; do
  for kf in 0 1; do
    c="$work/cache-$gv-$kf"
    out=$( (JOLT_GLOBAL_VAR_CALL=$gv JOLT_KEEP_FRAMES=$kf JOLT_CACHE_DIR="$c" "$jolt" -M -m gx.main1 &&
            JOLT_GLOBAL_VAR_CALL=$gv JOLT_KEEP_FRAMES=$kf JOLT_CACHE_DIR="$c" "$jolt" -M -m gx.main1 &&
            JOLT_GLOBAL_VAR_CALL=$gv JOLT_KEEP_FRAMES=$kf JOLT_CACHE_DIR="$c" "$jolt" -M -m gx.main2) 2>&1)
    if [ "$out" = "$expected" ]; then
      echo "global-var-cache-order: GV=${gv:-off} KF=$kf ok"
    else
      echo "global-var-cache-order: GV=${gv:-off} KF=$kf FAIL"; echo "$out"; fails=$((fails + 1))
    fi
  done
done
[ "$fails" -eq 0 ]
