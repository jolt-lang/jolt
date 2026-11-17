#!/bin/sh
# Lazy quoted data (JOLT_LAZY_META, JOLT_CONST_DATA). Runs lq.main twice per mode -- a cold run that compiles
# and caches every namespace, then a warm one that loads them all from the AOT
# cache -- and requires every run to print what the default emission prints on
# its cold run: each var's metadata with its keys in order, nested metadata
# included; alter-meta!/reset-meta! before the first read; a race of first reads
# against an alter-meta!; literal identity and metadata across namespaces;
# source lookup; a caught trace; closures through a state image. Lines starting
# "lazy:" report whether metadata was still a descriptor and differ by mode by
# design: they are printed, not compared, and the warm lazy runs must say true.
#   JOLT_BIN=target/release/jolt sh test/chez/lazy-quoted-data.sh
root="$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)"
jolt="${JOLT_BIN:-$root/target/release/jolt}"
case "$jolt" in /*) ;; *) jolt="$root/$jolt" ;; esac
work="$(mktemp -d)"; trap 'rm -rf "$work"' EXIT INT TERM
cp -R "$root/test/chez/lazy-quoted-data/." "$work/"
cd "$work" || exit 2
fails=0
run() {   # label env...
  label=$1; shift
  for phase in cold warm; do
    env "$@" JOLT_CACHE_DIR="$work/cache-$label" "$jolt" -m lq.main > "$work/$label-$phase.out" 2>&1
  done
}
run default
grep -v '^lazy:' "$work/default-cold.out" > "$work/expected"
grep -q '^done$' "$work/expected" || { echo "lazy-quoted-data: default run failed"; cat "$work/default-cold.out"; exit 1; }
for mode in "default" "meta:JOLT_LAZY_META=meta" "rows:JOLT_LAZY_META=1" \
            "const:JOLT_CONST_DATA=1" "all:JOLT_LAZY_META=1 JOLT_CONST_DATA=1" \
            "all-kf-gv:JOLT_LAZY_META=1 JOLT_CONST_DATA=1 JOLT_KEEP_FRAMES=1 JOLT_GLOBAL_VAR_CALL=1"; do
  label=${mode%%:*}; envs=""; [ "$mode" = "$label" ] || envs=${mode#*:}
  # shellcheck disable=SC2086
  [ "$label" = default ] || run "$label" $envs
  for phase in cold warm; do
    if grep -v '^lazy:' "$work/$label-$phase.out" | cmp -s - "$work/expected"; then
      echo "lazy-quoted-data: $label $phase ok $(grep '^lazy:' "$work/$label-$phase.out" | tr '\n' ' ')"
    else
      echo "lazy-quoted-data: $label $phase FAIL"
      grep -v '^lazy:' "$work/$label-$phase.out" | diff "$work/expected" - | head -20
      fails=$((fails + 1))
    fi
  done
done
# with metadata lazy, a warm run must find it still unread
for label in meta rows all all-kf-gv; do
  if ! grep -q '^lazy: true true$' "$work/$label-warm.out"; then
    echo "lazy-quoted-data: $label warm read metadata the program never asked for"
    fails=$((fails + 1))
  fi
done
[ "$fails" -eq 0 ]
