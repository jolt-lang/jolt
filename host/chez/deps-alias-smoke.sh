#!/bin/sh
# deps-alias-smoke.sh — deps.edn alias + CLI semantics through the real CLI.
#
# Fixture projects live in test/chez/deps-alias/: `app` selects aliases over two
# local libs that define the same namespace at different "versions" (liba/libb),
# a third lib (libc), and a stand-in jolt.time lib for the roots-autoload gate.
# Asserts the tools.deps alias args-map keys jolt supports — :extra-deps /
# :extra-paths / :override-deps / :default-deps / :replace-deps / :replace-paths
# / :main-opts — plus multi-alias combination rules, alias visibility in `path`,
# -A composing with -M, an undeclared alias failing, the java.time library
# autoload from the source roots, and the tools.deps CLI surface: -X/-T exec,
# -Sdeps, the user deps.edn chain, :local/root jars, and :git/tag + short sha.
#
# The expansion engine itself (exclusions, version selection, orphan cutting) is
# unit-tested in test/deps_expand_test.clj — see `make depsunit`.
#
# JOLT_BIN overrides the binary under test (defaults to bin/jolt source mode).
set -u
root="$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)"
cd "$root"
JOLT="${JOLT_BIN:-bin/jolt}"
APP="$root/test/chez/deps-alias/app"
pass=0; fail=0
# Hermetic: never read the developer's real ~/.clojure/deps.edn (the chain test
# below opts back in with an explicit CLJ_CONFIG).
export JOLT_NO_USER_DEPS=1
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

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

# a dep directory with no deps.edn of its own contributes its default src path
# (the programmatic add-deps path relies on this too)
check "dep without a deps.edn defaults to src" "libnoedn NOEDN" "$(run -A:noedn run -m appnoedn)"

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

# --- tools.deps CLI surface -------------------------------------------------

# -Sdeps merges an extra deps.edn map last into the chain (deps and aliases)
check "-Sdeps adds a dep" "libc C" \
      "$(run -Sdeps '{:deps {local/libc {:local/root "../libc"}}}' run -m appc)"
check "-Sdeps adds an alias" "libc C" \
      "$(run -Sdeps '{:aliases {:inj {:extra-deps {local/libc {:local/root "../libc"}}}}}' -A:inj run -m appc)"

# -X: :exec-fn / :exec-args from the alias, k v overrides, a trailing map, an
# explicit ns/fn argument, and :ns-aliases qualification
check "-X runs :exec-fn with :exec-args" 'exec: {:greeting "hi"}' "$(run -X:xbuild)"
check "-X k v overrides merge over :exec-args" 'exec: {:greeting "yo", :n 3}' \
      "$(run -X:xbuild :greeting '"yo"' :n 3)"
check "-X trailing map merges" 'exec: {:greeting "hi", :z 9}' "$(run -X:xbuild '{:z 9}')"
check "-X explicit ns/fn wins over :exec-fn" 'exec: {:greeting "hi", :a 1}' \
      "$(run -X:xbuild xtool/hello :a 1)"
check "-X :ns-aliases qualifies the fn" 'exec: {}' "$(run -X:xqual)"
out="$(runfull -X:dev)"
case "$out" in
  *"No function to execute"*) check "-X without :exec-fn errors" ok ok ;;
  *) check "-X without :exec-fn errors" "no-exec-fn error" "$(printf '%s' "$out" | head -1)" ;;
esac

# -T is -X with the project's own paths/deps replaced by the tool alias's
check "-T replaces the project basis" "tool: project-src-on-roots? false" "$(run -T:xtool)"
check "-X keeps the project basis" "tool: project-src-on-roots? true" "$(run -X:xtool)"

# user deps.edn chain: CLJ_CONFIG points at a user config whose alias is merged
# under the project's; JOLT_NO_USER_DEPS (exported above) opts out.
mkdir -p "$tmp/userconf"
sed "s|LIBC|$root/test/chez/deps-alias/libc|" \
  > "$tmp/userconf/deps.edn" <<'EOF'
{:aliases {:useralias {:extra-deps {local/libc {:local/root "LIBC"}}}}}
EOF
check "user deps.edn alias resolves" "libc C" \
      "$(JOLT_PWD="$APP" JOLT_QUIET=1 CLJ_CONFIG="$tmp/userconf" JOLT_NO_USER_DEPS= "$JOLT" -A:useralias run -m appc 2>&1 | tail -1)"
out="$(JOLT_PWD="$APP" JOLT_QUIET=1 CLJ_CONFIG="$tmp/userconf" "$JOLT" -A:useralias run -m appc 2>&1)"
case "$out" in
  *undeclared*) check "JOLT_NO_USER_DEPS opts out of the user chain" ok ok ;;
  *) check "JOLT_NO_USER_DEPS opts out of the user chain" "undeclared-alias error" "$(printf '%s' "$out" | head -1)" ;;
esac

# :local/root pointing at a jar extracts it and uses the extraction as a root
mkdir -p "$tmp/jarsrc/jarlib" "$tmp/jarproj/src"
cat > "$tmp/jarsrc/jarlib/core.clj" <<'EOF'
(ns jarlib.core)
(def version "from-jar")
EOF
( cd "$tmp/jarsrc" && zip -q -r ../mylib.jar jarlib )
cat > "$tmp/jarproj/deps.edn" <<'EOF'
{:paths ["src"] :deps {local/jarred {:local/root "../mylib.jar"}}}
EOF
cat > "$tmp/jarproj/src/japp.clj" <<'EOF'
(ns japp (:require [jarlib.core :as j]))
(defn -main [& _] (println "jar dep:" j/version))
EOF
check ":local/root jar extracts and loads" "jar dep: from-jar" \
      "$(JOLT_PWD="$tmp/jarproj" JOLT_QUIET=1 JOLT_JARLIBS="$tmp/jarlibs" "$JOLT" run -m japp 2>&1 | tail -1)"

# :git/tag + short :git/sha — the tag resolves to its commit and the short sha
# is verified as a prefix of it. Uses a local repo so the gate stays offline.
mkdir -p "$tmp/gitrepo/src/gitlib" "$tmp/gitproj/src"
# The identity is set repo-locally rather than passed per command: `git tag -a`
# needs a tagger too, and a CI runner has no global git config.
( cd "$tmp/gitrepo" \
  && git init -q . \
  && git config user.email t@example.com \
  && git config user.name t \
  && printf '{:paths ["src"]}\n' > deps.edn \
  && printf '(ns gitlib.core)\n(def version "tagged")\n' > src/gitlib/core.clj \
  && git add -A \
  && git commit -qm v1 \
  && git tag -a v1.0 -m v1.0 ) >/dev/null 2>&1
git -C "$tmp/gitrepo" rev-parse v1.0^{} >/dev/null 2>&1 || {
  echo "  FAIL: fixture git repo has no v1.0 tag (git identity/config problem)" >&2
  fail=$((fail+1)); }
short="$(git -C "$tmp/gitrepo" rev-parse --short=7 HEAD)"
cat > "$tmp/gitproj/src/gapp.clj" <<'EOF'
(ns gapp (:require [gitlib.core :as g]))
(defn -main [& _] (println "git dep:" g/version))
EOF
cat > "$tmp/gitproj/deps.edn" <<EOF
{:paths ["src"]
 :deps {local/gitdep {:git/url "file://$tmp/gitrepo" :git/tag "v1.0" :git/sha "$short"}}}
EOF
check ":git/tag + short sha resolves" "git dep: tagged" \
      "$(JOLT_PWD="$tmp/gitproj" JOLT_QUIET=1 JOLT_GITLIBS="$tmp/gitlibs" "$JOLT" run -m gapp 2>&1 | tail -1)"
cat > "$tmp/gitproj/deps.edn" <<EOF
{:paths ["src"]
 :deps {local/gitdep {:git/url "file://$tmp/gitrepo" :git/tag "v1.0" :git/sha "deadbee"}}}
EOF
out="$(JOLT_PWD="$tmp/gitproj" JOLT_QUIET=1 JOLT_GITLIBS="$tmp/gitlibs2" "$JOLT" run -m gapp 2>&1)"
case "$out" in
  *"does not match tag"*) check "short sha not matching the tag errors" ok ok ;;
  *) check "short sha not matching the tag errors" "sha/tag mismatch error" "$(printf '%s' "$out" | head -1)" ;;
esac

echo "deps-alias smoke: $pass passed, $fail failed"
[ "$fail" -eq 0 ]
