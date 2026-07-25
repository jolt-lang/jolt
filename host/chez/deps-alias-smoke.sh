#!/bin/sh
# deps-alias-smoke.sh — deps.edn alias semantics through the real CLI.
#
# Fixture projects live in test/chez/deps-alias/: `app` selects aliases over two
# local libs that define the same namespace at different "versions" (liba/libb),
# a third lib (libc), and a stand-in jolt.time lib for the roots-autoload gate.
# Asserts the tools.deps alias args-map keys jolt supports — :extra-deps /
# :extra-paths / :override-deps / :default-deps / :replace-deps / :replace-paths
# / :main-opts — plus multi-alias combination rules, alias visibility in `path`,
# -A composing with -M, an undeclared alias failing, and the java.time library
# autoload from the source roots.
#
# JOLT_BIN overrides the binary under test (defaults to bin/jolt source mode).
set -u
root="$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)"
cd "$root"
JOLT="${JOLT_BIN:-bin/jolt}"
APP="$root/test/chez/deps-alias/app"
pass=0; fail=0

check() { # label expected actual
  if [ "$2" = "$3" ]; then
    pass=$((pass+1))
  else
    fail=$((fail+1))
    echo "  FAIL: $1" >&2
    echo "    expected: $2" >&2
    echo "    got:      $3" >&2
  fi
}

run() { JOLT_PWD="$APP" JOLT_QUIET=1 "$JOLT" "$@" 2>&1 | tail -1; }
runfull() { JOLT_PWD="$APP" JOLT_QUIET=1 "$JOLT" "$@" 2>&1; }

# baseline: project deps only
check "project dep resolves (liba)" "liba A" "$(run run -m appver)"

# :extra-deps via -A adds a lib
check "-A :extra-deps adds libc" "libc C" "$(run -A:dev run -m appc)"

# without the alias the extra dep is absent (loader error, not libc C)
out="$(run run -m appc)"
[ "$out" = "libc C" ] && check "no alias => libc absent" "absent" "present" \
                      || check "no alias => libc absent" "absent" "absent"

# :override-deps replaces the coordinate wherever the lib appears
check "-A :override-deps swaps liba for libb" "liba B" "$(run -A:vb run -m appver)"

# :default-deps fills a nil coordinate
check "-A :default-deps fills nil coordinate" "libc C" "$(run -A:defd run -m appc)"

# :replace-deps: the project deps map is replaced (libc in, liba gone)
check "-A :replace-deps keeps libc" "libc C" "$(run -A:bare run -m appc)"
out="$(run -A:bare run -m appver 2>&1)"
[ "$out" = "liba A" ] && check ":replace-deps drops project deps" "dropped" "kept" \
                      || check ":replace-deps drops project deps" "dropped" "dropped"

# :replace-paths: project paths replaced (dev present, src gone)
out="$(run -A:rp path)"
case "$out" in
  *"$APP/src"*) check ":replace-paths drops src" "no src in path" "$out" ;;
  *"$APP/dev"*) check ":replace-paths drops src" ok ok ;;
  *) check ":replace-paths drops src" "dev in path" "$out" ;;
esac

# path honors -A aliases (extra-paths + extra-dep roots visible)
out="$(run -A:dev path)"
case "$out" in
  *"$APP/dev"*libc*|*libc*"$APP/dev"*) check "-A path lists alias roots" ok ok ;;
  *) check "-A path lists alias roots" "dev+libc in path" "$out" ;;
esac

# multi-alias :extra-paths append distinct (dev listed once)
out="$(run -A:dev:dev2 path)"
n="$(printf '%s' "$out" | tr ':' '\n' | grep -c "^$APP/dev$")"
check "multi-alias extra-paths distinct" "1" "$n"

# :main-opts last-wins across aliases
check "multi-alias main-opts last-wins" "main2" "$(run -M:m1:m2)"

# -A composes with -M (deps from -A, main from -M)
check "-A composes with -M" "main1" "$(run -A:dev -M:m1)"

# undeclared alias errors like tools.deps
out="$(runfull -A:nope path)"
case "$out" in
  *undeclared*) check "undeclared alias errors" ok ok ;;
  *) check "undeclared alias errors" "undeclared-alias error" "$(printf '%s' "$out" | head -1)" ;;
esac

# java.time library autoload: an unrequired java.time.ZoneId reference loads
# jolt.time from the source roots (the :time alias adds the stand-in lib)
check "java.time library autoloads from roots" "fixture-zone:UTC" "$(run -A:time run -m appzone)"

# off the roots the reference still names the dependency to add
out="$(runfull run -m appzone)"
case "$out" in
  *jolt-lang/time*) check "library miss names the dependency" ok ok ;;
  *) check "library miss names the dependency" "message naming jolt-lang/time" "$(printf '%s' "$out" | head -1)" ;;
esac

echo "deps-alias smoke: $pass passed, $fail failed"
[ "$fail" -eq 0 ]
