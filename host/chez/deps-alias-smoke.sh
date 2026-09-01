#!/bin/sh
# deps-alias-smoke.sh — deps.edn alias + CLI semantics through the real CLI.
#
# Fixture projects live in test/chez/deps-alias/: `app` selects aliases over two
# local libs that define the same namespace at different "versions" (liba/libb),
# a third lib (libc), and a stand-in jolt.time lib for the roots-autoload gate.
# Asserts the tools.deps alias args-map keys jolt supports — :extra-deps /
# :extra-paths / :override-deps / :default-deps / :replace-deps / :replace-paths
# / :main-opts — plus multi-alias combination rules, alias visibility in `path`,
# -A composing with -M, an undeclared alias warning and being skipped, the
# java.time library autoload from the source roots, and the tools.deps CLI
# surface: -X/-T exec, -Sdeps, the report options (-Spath / -Stree / -Strace /
# -Sdescribe / -P), -Scp, -Srepro, -Sverbose, the accepted-and-ignored options,
# the user deps.edn chain, :local/root jars, :git/tag + short sha, and git cache
# integrity (an interrupted or failed fetch is never trusted as a cached
# checkout).
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
# stdout only — for the path queries, whose answer a warning on stderr must not
# be able to displace. runall keeps every line (a tree, a describe map).
runout() { JOLT_PWD="$APP" JOLT_QUIET=1 "$JOLT" "$@" 2>/dev/null | tail -1; }
runall() { JOLT_PWD="$APP" JOLT_QUIET=1 "$JOLT" "$@" 2>/dev/null; }

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

# Path order matches `clojure -Spath`: the aliases' :extra-paths (in selection
# order), then the project's :paths — or an alias's :replace-paths, which
# :extra-paths still precedes — then the dep roots. own_paths keeps only the
# project's own directories; a :local/root dep root is under "$APP/../".
own_paths() { run "$@" path | tr ':' '\n' | grep -E "^$APP/[a-z0-9]+\$" | tr '\n' ' ' | sed 's/ $//'; }
check "extra-paths precede the project paths" "$APP/dev $APP/extra $APP/src" \
      "$(own_paths -A:dev2)"
check "extra-paths follow alias selection order" "$APP/shadow $APP/dev $APP/src" \
      "$(own_paths -A:shadow:dev)"
check "extra-paths precede replace-paths" "$APP/shadow $APP/dev" \
      "$(own_paths -A:rp:shadow)"

# and the order is load-bearing: shadow/appmain.clj and src/appmain.clj both
# define `appmain`, and the loader takes the first root that has it.
check "no alias => the project's own copy loads" "main1" "$(run run -m appmain)"
check ":extra-paths shadow the project's paths" "shadowed" "$(run -A:shadow run -m appmain)"

# :main-opts last-wins across aliases
check "multi-alias main-opts last-wins" "main2" "$(run -M:m1:m2)"

# -A composes with -M (deps from -A, main from -M)
check "-A composes with -M" "main1" "$(run -A:dev -M:m1)"

# an undeclared alias warns and is skipped, like tools.deps — an editor sends a
# fixed alias set (Calva's `-A:test:dev`) and a project that happens not to
# declare one of them still has a classpath, and a program, to run.
out="$(runfull -A:nope path)"
case "$out" in
  *undeclared*) check "undeclared alias warns" ok ok ;;
  *) check "undeclared alias warns" "undeclared-alias warning" "$(printf '%s' "$out" | head -1)" ;;
esac
check "undeclared alias still resolves the project" "$(runout path)" "$(runout -A:nope path)"
check "undeclared alias keeps the declared ones" "$(runout -A:dev path)" "$(runout -A:nope:dev path)"
check "undeclared alias still runs the program" "liba A" "$(run -A:nope run -m appver)"
JOLT_PWD="$APP" JOLT_QUIET=1 "$JOLT" -A:nope path >/dev/null 2>&1
check "undeclared alias exits 0" "0" "$?"

# -Spath — the clj CLI's "print the classpath and run nothing" option (what an
# editor asks with). The alias-selecting forms still count, in either order.
check "-Spath prints the roots" "$(runout path)" "$(runout -Spath)"
check "-A:alias -Spath sees the alias" "$(runout -A:dev path)" "$(runout -A:dev -Spath)"
check "-Spath -A:alias sees the alias" "$(runout -A:dev path)" "$(runout -Spath -A:dev)"
check "-Spath -M:alias sees the alias" "$(runout -A:dev path)" "$(runout -Spath -M:dev)"
check "-Spath -X:alias sees the alias" "$(runout -A:dev path)" "$(runout -Spath -X:dev)"
check "-Spath -Sdeps sees the extra map" "$(runout -A:dev path)" \
      "$(runout -Spath -Sdeps '{:aliases {:inj {:extra-paths ["dev"] :extra-deps {local/libc {:local/root "../libc"}}}}}' -A:inj)"
# -T replaces the project basis, so its -Spath answer does too
out="$(runout -Spath -T:xtool)"
case "$out" in
  *"$APP/src"*) check "-Spath -T replaces the project basis" "no src in path" "$out" ;;
  *tooldep*)    check "-Spath -T replaces the project basis" ok ok ;;
  *)            check "-Spath -T replaces the project basis" "tooldep in path" "$out" ;;
esac
# and nothing runs: no :main-opts, no :exec-fn, no task, no REPL
out="$(runfull -Spath -M:m1)"
case "$out" in
  *main1*) check "-Spath -M runs no program" "no program output" "$out" ;;
  *) check "-Spath -M runs no program" ok ok ;;
esac
out="$(runfull -Spath -X:xbuild)"
case "$out" in
  *exec:*) check "-Spath -X runs no exec-fn" "no exec output" "$out" ;;
  *) check "-Spath -X runs no exec-fn" ok ok ;;
esac
check "-Spath with an undeclared alias still answers" "$(runout -A:dev path)" \
      "$(runout -A:dev:nope -Spath)"

# -Sdescribe — the environment as an edn map, without resolving any dependency
out="$(runall -Sdescribe)"
case "$out" in
  *":config-project \"$APP/deps.edn\""*) check "-Sdescribe names the project deps.edn" ok ok ;;
  *) check "-Sdescribe names the project deps.edn" ":config-project entry" "$out" ;;
esac
out="$(runall -A:dev -Sdescribe -A:dev2)"
case "$out" in
  *":aliases [:dev :dev2]"*) check "-Sdescribe reports the selected aliases" ok ok ;;
  *) check "-Sdescribe reports the selected aliases" ":aliases [:dev :dev2]" "$out" ;;
esac

# -Stree — the dependency tree, tools.deps format: top deps unprefixed, the deps
# they pull in indented under them with a `.`
out="$(runall -Stree)"
case "$out" in
  "local/liba "*) check "-Stree prints the top dep" ok ok ;;
  *) check "-Stree prints the top dep" "local/liba <coord>" "$out" ;;
esac
out="$(runall -A:dev -Stree)"
case "$out" in
  *"local/libc "*) check "-Stree sees the alias's deps" ok ok ;;
  *) check "-Stree sees the alias's deps" "local/libc in the tree" "$out" ;;
esac
# a transitive dep hangs under the dep that pulled it in
mkdir -p "$tmp/treeproj/src" "$tmp/treeparent/src" "$tmp/treechild/src"
printf '{:paths ["src"]}\n' > "$tmp/treechild/deps.edn"
printf '{:paths ["src"] :deps {local/treechild {:local/root "../treechild"}}}\n' > "$tmp/treeparent/deps.edn"
printf '{:paths ["src"] :deps {local/treeparent {:local/root "../treeparent"}}}\n' > "$tmp/treeproj/deps.edn"
out="$(JOLT_PWD="$tmp/treeproj" JOLT_QUIET=1 "$JOLT" -Stree 2>/dev/null)"
case "$out" in
  "local/treeparent "*"
  . local/treechild "*) check "-Stree indents a transitive dep" ok ok ;;
  *) check "-Stree indents a transitive dep" "child under parent" "$out" ;;
esac

# accepted-and-ignored options don't change the answer or fail
check "-Sforce is accepted" "$(runout path)" "$(runout -Sforce -Spath)"
check "-Sthreads N is accepted" "$(runout path)" "$(runout -Sthreads 4 -Spath)"
check "-J is accepted (no JVM to pass it to)" "$(runout path)" "$(runout -J-Xmx512m -Spath)"

# -Sverbose says where deps come from — on stderr, so stdout stays the answer
check "-Sverbose leaves stdout alone" "$(runout path)" "$(runout -Sverbose -Spath)"
out="$(JOLT_PWD="$APP" JOLT_QUIET=1 "$JOLT" -Sverbose -Spath 2>&1 >/dev/null)"
case "$out" in
  *project_deps*gitlibs_dir*) check "-Sverbose reports the deps environment" ok ok ;;
  *) check "-Sverbose reports the deps environment" "env lines on stderr" "$out" ;;
esac

# -P resolves everything and runs nothing
check "-P prints nothing" "" "$(runout -P)"
out="$(runfull -P -M:m1)"
case "$out" in
  *main1*) check "-P runs no program" "no program output" "$out" ;;
  *) check "-P runs no program" ok ok ;;
esac

# -Sdescribe answers from the deps.edn files alone: a project whose dependency
# can't be fetched still describes, so an editor can ask cheaply and offline.
mkdir -p "$tmp/baddep/src"
cat > "$tmp/baddep/deps.edn" <<'EOF'
{:paths ["src"]
 :deps {local/nope {:git/url "file:///nonexistent-jolt-smoke-repo"
                    :git/sha "0000000000000000000000000000000000000000"}}}
EOF
JOLT_PWD="$tmp/baddep" JOLT_QUIET=1 "$JOLT" -Sdescribe >/dev/null 2>&1
check "-Sdescribe resolves no dependencies" "0" "$?"

# -Scp — these roots, no expansion. The unfetchable dep above proves the
# expansion is skipped rather than merely overridden.
check "-Scp replaces the roots" "/a:/b" "$(runout -Scp /a:/b -Spath)"
check "-Scp round-trips a recorded classpath" "$(runout -A:dev path)" \
      "$(runout -Scp "$(runout -A:dev path)" -Spath)"
check "-Scp expands no dependencies" "/a" \
      "$(JOLT_PWD="$tmp/baddep" JOLT_QUIET=1 "$JOLT" -Scp /a -Spath 2>/dev/null)"
# …and the deps.edn is still read, so an alias's :main-opts still run — against
# the given roots, which is the point: a recorded classpath drives a run offline
check "-Scp keeps the deps.edn main-opts" "main1" \
      "$(run -Scp "$APP/src" -M:m1)"
cat > "$tmp/baddep/src/badmain.clj" <<'EOF'
(ns badmain)
(defn -main [& _] (println "ran off the given roots"))
EOF
check "-Scp runs a program with the deps unfetched" "ran off the given roots" \
      "$(JOLT_PWD="$tmp/baddep" JOLT_QUIET=1 "$JOLT" -Scp "$tmp/baddep/src" run -m badmain 2>&1 | tail -1)"

# -Strace writes the expansion to trace.edn beside the deps.edn, and runs nothing
rm -f "$APP/trace.edn"
out="$(runfull -A:dev -Strace)"
case "$out" in
  *"Wrote $APP/trace.edn"*) check "-Strace says where it wrote" ok ok ;;
  *) check "-Strace says where it wrote" "Wrote …/trace.edn" "$out" ;;
esac
if [ -f "$APP/trace.edn" ]; then
  check "-Strace logs the expansion" "libc" \
        "$("$JOLT" -e "(let [t (read-string (slurp \"$APP/trace.edn\"))]
                         (println (some #(when (= 'local/libc (:lib %)) (name (:lib %)))
                                        (:log t))))" 2>/dev/null | tail -1)"
  check "-Strace records the version map" "true" \
        "$("$JOLT" -e "(println (contains? (read-string (slurp \"$APP/trace.edn\")) :vmap))" 2>/dev/null | tail -1)"
else
  fail=$((fail+2)); echo "  FAIL: -Strace wrote no trace.edn" >&2
fi
rm -f "$APP/trace.edn"

# an -S option jolt doesn't have says so, rather than reading as a bad task name
out="$(runfull -Sman)"
case "$out" in
  *"unsupported option: -Sman"*) check "unknown -S option is named as one" ok ok ;;
  *) check "unknown -S option is named as one" "unsupported-option error" "$(printf '%s' "$out" | head -1)" ;;
esac

# java.time library autoload: an unrequired java.time.ZoneId reference loads
# jolt.time from the source roots (the :time alias adds the stand-in lib)
check "java.time library autoloads from roots" "fixture-zone:UTC" "$(run -A:time run -m appzone)"

# ...and the DOT form of the same static call autoloads too. jolt evaluates the
# class name to a class token, so (. Class method args) arrives as a method call
# on that token; anything Class itself does not answer is a static of the class it
# names, and routing it that way is what picks up the autoload. Without it the
# form threw "No matching method of for class" unless an earlier slash-form call
# had already loaded the library — time-literals' data readers, (. LocalDate parse
# x), are written in this form and so could not read at all.
check "the dot form of a static autoloads too" "fixture-zone:UTC" "$(run -A:time run -m appzonedot)"

# ...and so does CONSTRUCTING a library class through an imported simple name.
# host-new does attempt the autoload, but the java.time library's name list is
# hand-maintained and had drifted from what the library actually registers:
# DateTimeFormatterBuilder was missing, so the fully-qualified form autoloaded and
# the imported simple name did not. malli's transform.cljc builds one that way.
check "constructing a library class autoloads" "fixture-builder" "$(run -A:time run -m appzonector)"

# Off the roots the reference reports that nothing provides the class — and
# deliberately does NOT name a library (RFC 0014). Which library supplies
# java.time is not the runtime's to say, and a caller may write the shim
# themselves; naming one would put the removed coupling back as a string.
out="$(runfull run -m appzone)"
case "$out" in
  *"No dependency provides"*) check "library miss reports no provider" ok ok ;;
  *) check "library miss reports no provider" "message saying no dependency provides it" "$(printf '%s' "$out" | head -1)" ;;
esac
# The first line only: the traceback below it carries absolute paths, and this
# checkout lives under a directory called jolt-lang.
case "$(printf '%s' "$out" | head -1)" in
  *jolt-lang/*) check "library miss names no library" "no library named" "$(printf '%s' "$out" | head -1)" ;;
  *) check "library miss names no library" ok ok ;;
esac

# io/resource answers an ABSOLUTE file: URL for a file on a source root, like the
# JVM classloader. The roots here are relative ("./src"), and "file:./src/x" is
# not a valid absolute URL — Selmer stores the URL from (io/resource "templates/…")
# and resolves template names against it, which threw MalformedURLException
# "no protocol" off the relative form.
check "io/resource answers an absolute file: URL" "absolute: true clean: true" \
      "$(run run -m appres)"

# A provider that IS on the roots but does not compile is not a missing
# dependency. The autoload latch is one-shot, so after the load raises every
# later miss falls back to the message — which must not send someone to edit a
# deps.edn that already declares the library.
out="$(run -A:timebroken run -m appzonebroken)"
case "$out" in
  *"failed to load"*) check "broken provider says it failed to load" ok ok ;;
  *) check "broken provider says it failed to load" "message saying failed to load" "$(printf '%s' "$out" | head -1)" ;;
esac
# The two cases must not read alike: "declared but broken" is fixed by repairing
# the library, "nothing provides it" by supplying one. Asserting the absence of
# the no-provider wording is what keeps them apart now that neither names a
# coordinate.
case "$out" in
  *"No dependency provides"*)
    check "broken provider is not reported as missing" "no missing-provider wording" "$(printf '%s' "$out" | head -1)" ;;
  *) check "broken provider is not reported as missing" ok ok ;;
esac

# org.clojure/clojure is intrinsic here — jolt IS Clojure — but on the JVM that
# artifact depends on spec.alpha and core.specs.alpha, so a project declaring only
# Clojure is terminal, while directly declared spec.alpha and core.specs.alpha
# remain ordinary Maven dependencies. This needs the network for the two small
# artifacts, so it is skipped when they cannot be fetched rather than failing.
SPECPROJ="$root/test/chez/deps-alias/specproj"
out="$(JOLT_PWD="$SPECPROJ" JOLT_QUIET=1 \
      JOLT_MAVEN_REPOSITORY="$tmp/spec-m2" \
      "$JOLT" run -m specapp 2>&1 | tail -1)"
case "$out" in
  "spec: true false") check "explicit spec.alpha dependency loads" ok ok ;;
  *"Could not locate"*|*"could not"*|*"no such"*)
    echo "  SKIP: spec.alpha transitivity (spec artifacts not fetchable offline)" >&2 ;;
  *) check "explicit spec.alpha dependency loads" "spec: true false" "$out" ;;
esac

# --- tools.deps CLI surface -------------------------------------------------

# -Sdeps merges an extra deps.edn map last into the chain (deps and aliases)
check "-Sdeps adds a dep" "libc C" \
      "$(run -Sdeps '{:deps {local/libc {:local/root "../libc"}}}' run -m appc)"
check "-Sdeps adds an alias" "libc C" \
      "$(run -Sdeps '{:aliases {:inj {:extra-deps {local/libc {:local/root "../libc"}}}}}' -A:inj run -m appc)"

# -e in a project resolves deps.edn first, so the expression can require the
# project's namespaces and its deps — and it composes with -Sdeps/-A/-M, which
# used to fail with "unknown command or task: -e".
check "-e sees the project's namespaces" "main1" "$(run -e "(require 'appmain) (appmain/-main)")"
check "-Sdeps + -e" "libc C" \
      "$(run -Sdeps '{:deps {local/libc {:local/root "../libc"}}}' -e "(require 'appc) (appc/-main)")"
check "-A + -e" "devmain" "$(run -A:dev2 -e "(require 'devmain) (devmain/-main)")"
check "bare -M -e uses the command line as main-opts" "main1" \
      "$(run -M -e "(require 'appmain) (appmain/-main)")"
check "bare -M -m uses the command line as main-opts" "main1" "$(run -M -m appmain)"
check ":main-opts may be an -e expression" "main1" "$(run -M:e1)"
check "-M:alias main-opts precede the command line" "main1" "$(run -M:m1 -m appmain2)"
check "-e passes the rest as *command-line-args*" '(a b)' \
      "$(run -e '(println *command-line-args*)' a b)"
check "-e - reads the expression from stdin" "main1" \
      "$(printf "(require 'appmain) (appmain/-main)" | JOLT_PWD="$APP" JOLT_QUIET=1 "$JOLT" -e - 2>&1 | tail -1)"
check "- runs a stdin program against the project" "main1" \
      "$(printf "(require 'appmain) (appmain/-main)" | JOLT_PWD="$APP" JOLT_QUIET=1 "$JOLT" - 2>&1 | tail -1)"
out="$(runfull -M)"
case "$out" in
  *"have no :main-opts"*) check "bare -M with nothing to run errors" ok ok ;;
  *) check "bare -M with nothing to run errors" "no-main-opts error" "$(printf '%s' "$out" | head -1)" ;;
esac

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
  *) check "JOLT_NO_USER_DEPS opts out of the user chain" "undeclared-alias warning" "$(printf '%s' "$out" | head -1)" ;;
esac
# -Srepro opts out the same way, per run rather than per environment
out="$(JOLT_PWD="$APP" JOLT_QUIET=1 CLJ_CONFIG="$tmp/userconf" JOLT_NO_USER_DEPS= "$JOLT" -Srepro -A:useralias run -m appc 2>&1)"
case "$out" in
  *undeclared*) check "-Srepro ignores the user deps.edn" ok ok ;;
  *) check "-Srepro ignores the user deps.edn" "undeclared-alias warning" "$(printf '%s' "$out" | head -1)" ;;
esac
out="$(JOLT_PWD="$APP" JOLT_QUIET=1 CLJ_CONFIG="$tmp/userconf" JOLT_NO_USER_DEPS= "$JOLT" -Srepro -Sdescribe 2>/dev/null)"
case "$out" in
  *":repro true"*) check "-Srepro reports itself in -Sdescribe" ok ok ;;
  *) check "-Srepro reports itself in -Sdescribe" ":repro true" "$out" ;;
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
      "$(JOLT_PWD="$tmp/gitproj" JOLT_QUIET=1 JOLT_GITLIBS_DIR="$tmp/gitlibs" "$JOLT" run -m gapp 2>&1 | tail -1)"

# An annotated tag carries two shas: the tag object's own, and the commit it
# peels to. `git ls-remote` prints the tag object for refs/tags/X, so that is
# what a deps.edn written from ls-remote output pins — cognitect-labs/test-runner
# v0.5.0 is exactly this, and tools.deps accepts either. Rejecting the tag object
# left an unmodified upstream library unable to resolve its own test runner.
tagobj="$(git -C "$tmp/gitrepo" rev-parse --short=7 v1.0)"
if [ "$tagobj" = "$short" ]; then
  echo "  FAIL: fixture tag is not annotated (tag object sha == commit sha)" >&2
  fail=$((fail+1))
fi
cat > "$tmp/gitproj/deps.edn" <<EOF
{:paths ["src"]
 :deps {local/gitdep {:git/url "file://$tmp/gitrepo" :git/tag "v1.0" :git/sha "$tagobj"}}}
EOF
check ":git/tag + annotated tag object sha resolves" "git dep: tagged" \
      "$(JOLT_PWD="$tmp/gitproj" JOLT_QUIET=1 JOLT_GITLIBS_DIR="$tmp/gitlibs-tagobj" "$JOLT" run -m gapp 2>&1 | tail -1)"

# The legacy tools.deps spellings, :sha and :tag, still appear in deps.edn files
# in the wild — malli's spec-alpha2 dependency is written {:git/url … :sha …} —
# and tools.deps accepts them alongside the namespaced keys. Rejecting them left
# an unmodified upstream library unable to resolve its own test dependencies.
legacy_sha="$(git -C "$tmp/gitrepo" rev-parse HEAD)"
cat > "$tmp/gitproj/deps.edn" <<EOF
{:paths ["src"]
 :deps {local/gitdep {:git/url "file://$tmp/gitrepo" :sha "$legacy_sha"}}}
EOF
check "the legacy :sha spelling resolves" "git dep: tagged" \
      "$(JOLT_PWD="$tmp/gitproj" JOLT_QUIET=1 JOLT_GITLIBS_DIR="$tmp/gitlibs-legacy" "$JOLT" run -m gapp 2>&1 | tail -1)"
cat > "$tmp/gitproj/deps.edn" <<EOF
{:paths ["src"]
 :deps {local/gitdep {:git/url "file://$tmp/gitrepo" :tag "v1.0" :sha "$short"}}}
EOF
check "the legacy :tag + :sha pair resolves" "git dep: tagged" \
      "$(JOLT_PWD="$tmp/gitproj" JOLT_QUIET=1 JOLT_GITLIBS_DIR="$tmp/gitlibs-legacy2" "$JOLT" run -m gapp 2>&1 | tail -1)"

# and not off a stale tag cache: an older jolt recorded only the commit, so a
# one-token cache file must be re-resolved rather than trusted — otherwise the
# tag object is unknown and the tag-object coordinate is rejected again. Writes
# its own deps.edn rather than inheriting the previous block's, so inserting a
# case above cannot quietly turn this into a different test.
cat > "$tmp/gitproj/deps.edn" <<EOF
{:paths ["src"]
 :deps {local/gitdep {:git/url "file://$tmp/gitrepo" :git/tag "v1.0" :git/sha "$tagobj"}}}
EOF
stalesan="$(printf '%s' "file://$tmp/gitrepo" | sed 's/[^A-Za-z0-9.-]/_/g')"
mkdir -p "$tmp/gitlibs-stale/tags/$stalesan"
git -C "$tmp/gitrepo" rev-parse "v1.0^{}" > "$tmp/gitlibs-stale/tags/$stalesan/v1.0"
check "a legacy one-token tag cache is re-resolved" "git dep: tagged" \
      "$(JOLT_PWD="$tmp/gitproj" JOLT_QUIET=1 JOLT_GITLIBS_DIR="$tmp/gitlibs-stale" "$JOLT" run -m gapp 2>&1 | tail -1)"
cat > "$tmp/gitproj/deps.edn" <<EOF
{:paths ["src"]
 :deps {local/gitdep {:git/url "file://$tmp/gitrepo" :git/tag "v1.0" :git/sha "deadbee"}}}
EOF
out="$(JOLT_PWD="$tmp/gitproj" JOLT_QUIET=1 JOLT_GITLIBS_DIR="$tmp/gitlibs2" "$JOLT" run -m gapp 2>&1)"
case "$out" in
  *"does not match tag"*) check "short sha not matching the tag errors" ok ok ;;
  *) check "short sha not matching the tag errors" "sha/tag mismatch error" "$(printf '%s' "$out" | head -1)" ;;
esac

# "the tag is not there" and "we could not go look" are different answers, and
# saying the first when the second happened sends you to the repo and the pin
# instead of to the fetch. The v0.7.7 release failed exactly this way: a
# transient `git ls-remote` failure against github reported a tag that had
# existed for two weeks as missing. Both cases are offline here — a real repo
# without the tag, and a URL that is not a repo at all.
cat > "$tmp/gitproj/deps.edn" <<EOF
{:paths ["src"]
 :deps {local/gitdep {:git/url "file://$tmp/gitrepo" :git/tag "v9.9" :git/sha "$short"}}}
EOF
out="$(JOLT_PWD="$tmp/gitproj" JOLT_QUIET=1 JOLT_GITLIBS_DIR="$tmp/gitlibs-notag" "$JOLT" run -m gapp 2>&1)"
case "$out" in
  *"tag v9.9 not found"*) check "a tag the repo does not have is reported as not found" ok ok ;;
  *) check "a tag the repo does not have is reported as not found" "tag v9.9 not found" "$(printf '%s' "$out" | head -2)" ;;
esac

mkdir -p "$tmp/notarepo"
cat > "$tmp/gitproj/deps.edn" <<EOF
{:paths ["src"]
 :deps {local/gitdep {:git/url "file://$tmp/notarepo" :git/tag "v1.0" :git/sha "$short"}}}
EOF
out="$(JOLT_PWD="$tmp/gitproj" JOLT_QUIET=1 JOLT_GITLIBS_DIR="$tmp/gitlibs-noremote" "$JOLT" run -m gapp 2>&1)"
case "$out" in
  *"not found"*) check "a failed ls-remote is not reported as a missing tag" \
                       "a fetch failure, not 'not found'" "$(printf '%s' "$out" | head -2)" ;;
  *"could not list"*) check "a failed ls-remote is not reported as a missing tag" ok ok ;;
  *) check "a failed ls-remote is not reported as a missing tag" \
           "could not list …" "$(printf '%s' "$out" | head -2)" ;;
esac

# ...and a transient one is retried rather than reported. A `git` shim ahead of
# the real one on PATH fails the first ls-remote the way a reset does, then gets
# out of the way — so the dep resolves only if the failure was retried. The
# counter file is what makes it observable: an unretried run leaves it at 1.
mkdir -p "$tmp/fakebin"
cat > "$tmp/fakebin/git" <<'SHIM'
#!/bin/sh
# Count ls-remote invocations; fail the first one, delegate the rest.
for a in "$@"; do
  if [ "$a" = "ls-remote" ]; then
    n=$(cat "$FAKE_GIT_COUNT" 2>/dev/null || echo 0)
    n=$((n+1)); printf '%s' "$n" > "$FAKE_GIT_COUNT"
    if [ "$n" -le "${FAKE_GIT_FAILS:-1}" ]; then
      echo "fatal: unable to access: Connection reset by peer" >&2
      exit 128
    fi
    break
  fi
done
exec "$REAL_GIT" "$@"
SHIM
chmod +x "$tmp/fakebin/git"
export REAL_GIT="$(command -v git)"
cat > "$tmp/gitproj/deps.edn" <<EOF
{:paths ["src"]
 :deps {local/gitdep {:git/url "file://$tmp/gitrepo" :git/tag "v1.0" :git/sha "$short"}}}
EOF
export FAKE_GIT_COUNT="$tmp/fakegit.count"; : > "$FAKE_GIT_COUNT"
check "a transient ls-remote failure is retried" "git dep: tagged" \
      "$(PATH="$tmp/fakebin:$PATH" FAKE_GIT_FAILS=1 JOLT_PWD="$tmp/gitproj" JOLT_QUIET=1 \
         JOLT_GITLIBS_DIR="$tmp/gitlibs-retry" "$JOLT" run -m gapp 2>&1 | tail -1)"
check "  and the retry actually happened (ls-remote ran twice)" "2" "$(cat "$FAKE_GIT_COUNT")"

# The cap is real: a failure that outlasts it reports, it does not spin.
: > "$FAKE_GIT_COUNT"
out="$(PATH="$tmp/fakebin:$PATH" FAKE_GIT_FAILS=99 JOLT_PWD="$tmp/gitproj" JOLT_QUIET=1 \
       JOLT_GITLIBS_DIR="$tmp/gitlibs-retry2" "$JOLT" run -m gapp 2>&1)"
case "$out" in
  *"could not list"*) check "a persistent ls-remote failure reports after the cap" ok ok ;;
  *) check "a persistent ls-remote failure reports after the cap" "could not list …" \
           "$(printf '%s' "$out" | head -2)" ;;
esac
check "  and it stopped at the attempt cap" "3" "$(cat "$FAKE_GIT_COUNT")"
unset REAL_GIT FAKE_GIT_COUNT

# git cache integrity: only a finished checkout counts as cached. An interrupted
# fetch used to leave the pre-created sha directory behind empty, and every later
# run took it for a valid checkout — the dep contributed no source root and the
# failure surfaced as a "Could not locate" on one of its namespaces.
sha="$(git -C "$tmp/gitrepo" rev-parse HEAD)"
san="$(printf '%s' "file://$tmp/gitrepo" | sed 's/[^A-Za-z0-9.-]/_/g')"
cat > "$tmp/gitproj/deps.edn" <<EOF
{:paths ["src"]
 :deps {local/gitdep {:git/url "file://$tmp/gitrepo" :git/sha "$sha"}}}
EOF
mkdir -p "$tmp/gitlibs3/$san/$sha"
check "an empty cached checkout is re-fetched" "git dep: tagged" \
      "$(JOLT_PWD="$tmp/gitproj" JOLT_QUIET=1 JOLT_GITLIBS_DIR="$tmp/gitlibs3" "$JOLT" run -m gapp 2>&1 | tail -1)"
# and the re-fetch is durable: the second run reuses it without cloning again
out="$(JOLT_PWD="$tmp/gitproj" JOLT_QUIET=1 JOLT_DEBUG=1 JOLT_GITLIBS_DIR="$tmp/gitlibs3" "$JOLT" run -m gapp 2>&1)"
case "$out" in
  *fetching*) check "a complete checkout is reused" "no re-fetch" "$(printf '%s' "$out" | grep fetching)" ;;
  *) check "a complete checkout is reused" ok ok ;;
esac

# a failed fetch leaves nothing the next run would trust
cat > "$tmp/gitproj/deps.edn" <<EOF
{:paths ["src"]
 :deps {local/gitdep {:git/url "file://$tmp/not-a-repo" :git/sha "$sha"}}}
EOF
JOLT_PWD="$tmp/gitproj" JOLT_QUIET=1 JOLT_GITLIBS_DIR="$tmp/gitlibs4" "$JOLT" run -m gapp >/dev/null 2>&1
check "a failed fetch caches no checkout" "" \
      "$(find "$tmp/gitlibs4" -mindepth 2 -maxdepth 2 2>/dev/null)"

echo "deps-alias smoke: $pass passed, $fail failed"
[ "$fail" -eq 0 ]
