#!/bin/sh
# jolt.loader's artifact cache for context-private namespaces (#1653): run
# test/chez/loader-cache-test.clj twice over one source tree and one fresh
# cache directory — phase 1 checks miss/hit/invalidation within a process,
# phase 2 that a second process compiles nothing. JOLT_AOT_ASYNC=0 so a miss
# has published its fasl before the next load looks for it.
root="$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)"
cd "$root"
jolt="${JOLT_BIN:-bin/jolt}"
tmp="$(mktemp -d)"; trap 'rm -rf "$tmp"' EXIT
mkdir -p "$tmp/src" "$tmp/cache"

for phase in 1 2; do
  JOLT_AOT_CACHE=1 JOLT_AOT_ASYNC=0 JOLT_CACHE_DIR="$tmp/cache" \
    "$jolt" run test/chez/loader-cache-test.clj "$tmp/src" "$phase" > "$tmp/out" 2>&1
  status=$?
  cat "$tmp/out"
  if [ "$status" -ne 0 ] || ! grep -q '^LOADERCACHE OK' "$tmp/out"; then
    echo "FAIL: loader-cache phase $phase (exit $status)"
    exit 1
  fi
done
echo "loadercache: OK"
