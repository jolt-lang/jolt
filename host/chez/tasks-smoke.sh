#!/bin/sh
# tasks-smoke.sh — bb.edn / deps.edn task running through the real CLI.
#
# Fixture projects live in test/chez/tasks/: `bbproj` has only a bb.edn (so
# bb.edn is the whole project config), `both` has a deps.edn AND a bb.edn (the
# app resolves from deps.edn; bb.edn's paths join in for task runs), and
# `depsonly` pins jolt's own deps.edn :tasks forms.
#
# `cliproj` is the CLI-task fixture: tasks that name an :exec-fn or a :cmd tree
# and so parse their arguments with babashka.cli.
#
# Asserts the babashka task semantics jolt supports — code bodies, :doc,
# :depends (deduped, dependency-first, cycles refused), :init, :requires (global
# and per-task), :enter/:leave, :private, :extra-paths/:extra-deps,
# :override-builtin, the babashka.tasks API (shell / jolt / clojure / run /
# current-task), *command-line-args*, the `tasks` listing, `run <task>` and
# `run --parallel`, and exit-code propagation from a failed shell — plus jolt's
# own string and :main-opts forms, and which file drives which command when a
# project has both.
#
# JOLT_BIN overrides the binary under test (defaults to bin/jolt source mode).
set -u
root="$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)"
cd "$root"
JOLT="${JOLT_BIN:-bin/jolt}"
BB="$root/test/chez/tasks/bbproj"
BOTH="$root/test/chez/tasks/both"
DEPS="$root/test/chez/tasks/depsonly"
NAT="$root/test/chez/tasks/native"
CLI="$root/test/chez/tasks/cliproj"
pass=0; fail=0
export JOLT_NO_USER_DEPS=1
# babashka.tasks/jolt (and `clojure`, its babashka name) re-invokes the jolt
# CLI: point it at the one under test, not at whatever `jolt` PATH happens to
# hold.
case "$JOLT" in /*) export JOLT_EXE="$JOLT" ;; *) export JOLT_EXE="$root/$JOLT" ;; esac
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

# stdout+stderr for a command run in a fixture project; and the exit status
# on its own.
inbb()   { d="$1"; shift; JOLT_PWD="$d" JOLT_QUIET=1 "$JOLT" "$@" 2>&1; }
status() { d="$1"; shift; JOLT_PWD="$d" JOLT_QUIET=1 "$JOLT" "$@" >/dev/null 2>&1; echo $?; }

# --- a build task for a :jolt/native library that does not exist yet ----------
# A project whose native/ holds C sources names the .so/.dylib built from them in
# :jolt/native, so on a fresh checkout the library is missing until a task builds
# it. Applying the project used to load the natives strictly first, which made
# that task impossible to run: the build step needed its own output. A task run
# warns instead.
out="$(inbb "$NAT" build-native)"
case "$out" in *compiled*) r=yes ;; *) r="no: $out" ;; esac
check "a task runs when its :jolt/native library is not built yet" "yes" "$r"
check "...and exits 0" "0" "$(status "$NAT" build-native)"
case "$out" in *warning:*nope*) r=yes ;; *) r="no: $out" ;; esac
check "...having said which library was missing" "yes" "$r"

# Everything that is not a task still refuses to start without it.
check "a run still fails on a missing required native" "1" \
  "$(s=$(status "$NAT" -M:go); [ "$s" = 0 ] && echo 0 || echo 1)"

# The candidate is a PATH (it has a separator), so it belongs to the project, not
# to whatever directory jolt was started from — bin/jolt cd's to its own tree, so
# resolving against the current directory looked in the wrong place and reported
# the library missing even when it was built.
out="$(inbb "$NAT" -M:go)"
case "$out" in *"$NAT/native/libnope."*) r=yes ;; *) r="no: $out" ;; esac
check "a relative :jolt/native path resolves against the project" "yes" "$r"

# --- a dependency's :jolt/native :build task ----------------------------------
# test/chez/tasks/nativebuild: `lib` compiles native/jbt.c in its `native` task
# and names that task in its spec's :build; `app` depends on it by :local/root
# and does nothing else. Each case runs on a fresh copy, the state a gitlib
# checkout is in the first time an app uses it.
if command -v "${CC:-cc}" >/dev/null 2>&1; then
NB="$root/test/chez/tasks/nativebuild"
fresh() { rm -rf "$tmp/nb"; mkdir -p "$tmp/nb"; cp -R "$NB/." "$tmp/nb/"; }
builds() { if [ -f "$tmp/nb/lib/native/builds.log" ]; then wc -l < "$tmp/nb/lib/native/builds.log" | tr -d ' '; else echo 0; fi; }

fresh
out="$(inbb "$tmp/nb/app" -M:go)"; s=$?
case "$out" in *"answer 42"*) r=yes ;; *) r="no: $out" ;; esac
check "a dependency's missing native library is built by its :build task" "yes" "$r"
check "...and the run exits 0" "0" "$s"
case "$out" in *"building native library jbt (task native in "*"/nb/lib)"*) r=yes ;; *) r="no: $out" ;; esac
check "...having said what it built and where" "yes" "$r"
check "...running the task once" "1" "$(builds)"
case "$out" in *warning:*) r="no: $out" ;; *) r=yes ;; esac
check "...whose own process does not warn about the library it is building" "yes" "$r"
out="$(inbb "$tmp/nb/app" -M:go)"
case "$out" in "answer 42") r=yes ;; *) r="no: $out" ;; esac
check "a second run loads the built library without building" "yes" "$r"
check "...so the task ran once in all" "1" "$(builds)"
[ -d "$tmp/nb/lib/.jolt-native-build.lock" ] && r=left || r=gone
check "...and the build lock is released" "gone" "$r"

# the build's own stdout is not the program's output
fresh
out="$(JOLT_PWD="$tmp/nb/app" JOLT_QUIET=1 "$JOLT" -M:go 2>/dev/null)"
check "the build's output goes to stderr, not stdout" "answer 42" "$out"

# a consumer TASK builds a dependency's library too (only the project's own
# library is left to the project's tasks)
fresh
out="$(inbb "$tmp/nb/app" go)"
case "$out" in *"answer 42"*) r=yes ;; *) r="no: $out" ;; esac
check "a task in the consumer builds a dependency's library" "yes" "$r"

# the build task, run in the library itself, does not start a build of its own
fresh
out="$(inbb "$tmp/nb/lib" native)"; s=$?
check "the library's own build task runs once, without recursing" "0 1" "$s $(builds)"
case "$out" in *"building native library"*) r="no: $out" ;; *) r=yes ;; esac
check "...and does not build its library first" "yes" "$r"

# JOLT_NATIVE_BUILD=0 turns it off, and the error says how it would be built
fresh
out="$(export JOLT_NATIVE_BUILD=0; inbb "$tmp/nb/app" -M:go)"; s=$?
check "JOLT_NATIVE_BUILD=0 builds nothing" "0" "$(builds)"
check "...and the run fails" "1" "$([ "$s" = 0 ] && echo 0 || echo 1)"
case "$out" in *"required native library jbt not found"*"task native in "*"/nb/lib builds it (auto-build is off: JOLT_NATIVE_BUILD=0)"*) r=yes ;; *) r="no: $out" ;; esac
check "...naming the task that would build it" "yes" "$r"

# a failed build names the library, the task, the dir and the exit status
fresh
out="$(export JBT_BUILD_FAIL=1; inbb "$tmp/nb/app" -M:go)"; s=$?
check "a failed build fails the run" "1" "$([ "$s" = 0 ] && echo 0 || echo 1)"
case "$out" in *"required native library jbt not found"*"building it failed: task native in "*"/nb/lib exited with status 3"*) r=yes ;; *) r="no: $out" ;; esac
check "...naming the library, the task, the dir and the status" "yes" "$r"

# two specs naming the same task run it once per process
fresh
sed 's/:build  "native"}]/:build  "native"} {:name "jbt2" :darwin ["native\/libjbt2.dylib"] :linux ["native\/libjbt2.so"] :build "native"}]/' \
  "$NB/lib/deps.edn" > "$tmp/nb/lib/deps.edn"
out="$(inbb "$tmp/nb/app" -M:go)"
check "a task several specs name runs once per process" "1" "$(builds)"
case "$out" in *"required native library jbt2 not found"*"task native in "*"/nb/lib ran but did not produce it"*) r=yes ;; *) r="no: $out" ;; esac
check "...and a spec it did not produce says so" "yes" "$r"

# two processes racing to build the same checkout: one builds, one waits
fresh
JBT_BUILD_SLEEP_MS=1500 JOLT_PWD="$tmp/nb/app" JOLT_QUIET=1 "$JOLT" -M:go > "$tmp/nb-a.out" 2>&1 &
pa=$!
JBT_BUILD_SLEEP_MS=1500 JOLT_PWD="$tmp/nb/app" JOLT_QUIET=1 "$JOLT" -M:go > "$tmp/nb-b.out" 2>&1
wait $pa
case "$(cat "$tmp/nb-a.out") $(cat "$tmp/nb-b.out")" in *"answer 42"*"answer 42"*) r=yes ;; *) r="no: $(cat "$tmp/nb-a.out" "$tmp/nb-b.out")" ;; esac
check "two processes needing the same build both run" "yes" "$r"
check "...and the task ran once" "1" "$(builds)"

# A build killed before it could release the lock (Ctrl-C, kill -9, a crash)
# leaves .jolt-native-build.lock behind. Its holder recorded its pid and kept
# the lock's mtime fresh while it lived, so the next run sees it is dead and
# carries on at once, instead of waiting out a 30-minute staleness window.
# `bounded` runs a command with a watchdog that kills it and everything under
# it, so a regression fails rather than hanging the smoke.
killtree() { for c in $(pgrep -P "$1"); do killtree "$c"; done; kill -9 "$1" 2>/dev/null; }
bounded() { # secs cmd...
  secs="$1"; shift
  "$@" & bp=$!
  ( sleep "$secs"; killtree $bp ) >/dev/null 2>&1 & bw=$!
  wait $bp; bs=$?
  kill $bw 2>/dev/null; wait $bw 2>/dev/null
  return $bs
}
# The build is killed the way Ctrl-C kills it: the jolt holding the lock and
# the task it is running, together, mid-build.
fresh
JBT_BUILD_SLEEP_MS=6000 JOLT_PWD="$tmp/nb/app" JOLT_QUIET=1 "$JOLT" -M:go > "$tmp/nb-a.out" 2>&1 &
pa=$!
n=0; while [ ! -f "$tmp/nb/lib/native/builds.log" ] && [ $n -lt 200 ]; do sleep 0.1; n=$((n+1)); done
kill -KILL $pa $(pgrep -P $pa) 2>/dev/null; wait $pa 2>/dev/null
[ -d "$tmp/nb/lib/.jolt-native-build.lock" ] && r=left || r=gone
check "a build interrupted mid-run leaves its lock behind" "left" "$r"
t0=$(date +%s)
out="$(bounded 30 inbb "$tmp/nb/app" -M:go)"; s=$?
t1=$(date +%s)
case "$out" in *"stale native build lock for jbt (its holder, pid "*" is gone)"*"answer 42") r=yes ;; *) r="no: $out" ;; esac
check "the next run breaks the dead build's lock and builds" "yes" "$r"
check "...promptly" "yes" "$([ $((t1-t0)) -lt 20 ] && echo yes || echo "no: $((t1-t0))s")"
[ -d "$tmp/nb/lib/.jolt-native-build.lock" ] && r=left || r=gone
check "...releasing the lock it took" "gone" "$r"

# a lock with no holder recorded (an older jolt's) is stale once its mtime is
# a minute old
fresh
mkdir "$tmp/nb/lib/.jolt-native-build.lock"
touch -t 200001010000 "$tmp/nb/lib/.jolt-native-build.lock"
out="$(bounded 30 inbb "$tmp/nb/app" -M:go)"
case "$out" in *"stale native build lock for jbt (it has not been refreshed for a minute)"*"answer 42") r=yes ;; *) r="no: $out" ;; esac
check "a lock that stopped being refreshed is broken" "yes" "$r"

# a live holder's lock is waited on, and the wait names the lock to delete
fresh
mkdir "$tmp/nb/lib/.jolt-native-build.lock"
( sleep 2; rm -rf "$tmp/nb/lib/.jolt-native-build.lock" ) &
out="$(bounded 30 inbb "$tmp/nb/app" -M:go)"
wait
case "$out" in *"waiting for another jolt building jbt; if no build is running, delete "*"/nb/lib/.jolt-native-build.lock"*"answer 42") r=yes ;; *) r="no: $out" ;; esac
check "a held lock is waited on, naming the path to delete" "yes" "$r"

# the build task's stdin is closed, so a task that reads it does not hang
fresh
out="$(export JBT_BUILD_READ_STDIN=1; bounded 30 inbb "$tmp/nb/app" -M:go)"
case "$out" in *"answer 42") r=yes ;; *) r="no: $out" ;; esac
check "a build task that reads stdin sees EOF" "yes" "$r"

# `jolt build` builds it too, and the binary loads what was built
fresh
out="$(JOLT_PWD="$tmp/nb/app" JOLT_QUIET=1 "$JOLT" build -m app.go -o "$tmp/nb-app" 2>&1)"; s=$?
check "jolt build builds a dependency's missing library" "0 1" "$s $(builds)"
[ "$s" = 0 ] || echo "$out" >&2
out="$("$tmp/nb-app" 2>&1)"
check "...and the binary loads it" "answer 42" "$out"

# ...and a :static archive the task produces, which only `jolt build` reads
fresh
sed 's/:linux  \["native\/libjbt.so"\]/:static {:archive "native\/libjbt.a"}/; s/:darwin \["native\/libjbt.dylib"\]//' \
  "$NB/lib/deps.edn" > "$tmp/nb/lib/deps.edn"
out="$(export JBT_STATIC=1; JOLT_PWD="$tmp/nb/app" JOLT_QUIET=1 "$JOLT" build -m app.go -o "$tmp/nb-static" 2>&1)"; s=$?
check "jolt build builds a missing :static archive" "0 1" "$s $(builds)"
[ "$s" = 0 ] || echo "$out" >&2
out="$("$tmp/nb-static" 2>&1)"
check "...and links it into the binary" "answer 42" "$out"

# --- a dependency's natives load on its first require --------------------------
# The same fixture: the library's native is opened (and so, here, built) the
# first time a namespace from the library is required, not at startup.
# app.plain requires nothing of the library, app.later requires it at run time,
# app.direct binds its C symbol without requiring any of it.
fresh
out="$(inbb "$tmp/nb/app" -m app.plain)"; s=$?
case "$out" in "plain ["*"/lib/src]") r=yes ;; *) r="no: $out" ;; esac
check "a dependency nothing requires leaves its native pending" "yes" "$r"
check "...builds nothing and exits 0" "0 0" "$s $(builds)"

out="$(inbb "$tmp/nb/app" -m app.later)"
case "$out" in "pending before 1"*"building native library jbt"*"pending after 0"*"answer 42") r=yes ;; *) r="no: $out" ;; esac
check "the native loads on the first require of the dependency" "yes" "$r"
check "...once" "1" "$(builds)"

fresh
out="$(export JOLT_NATIVE_BUILD=0; inbb "$tmp/nb/app" -m app.later)"; s=$?
check "a missing dependency native fails the require" "1" "$([ "$s" = 0 ] && echo 0 || echo 1)"
case "$out" in "pending before 1"*"required native library jbt not found"*"(declared by dependency jbt/jbt, loaded on first require of jbt.core)"*) r=yes ;; *) r="no: $out" ;; esac
check "...after startup, naming the library, the dependency and the namespace" "yes" "$r"

# the project's own natives still load at startup
out="$(export JOLT_NATIVE_BUILD=0; inbb "$tmp/nb/lib" -e '(println :ran)')"
case "$out" in *":ran"*) r="no: $out" ;; *"required native library jbt not found"*) r=yes ;; *) r="no: $out" ;; esac
check "the project's own native still loads at startup" "yes" "$r"

# a foreign symbol nothing loaded defines loads the pending natives, building
# the library on a fresh checkout as its first require would
fresh
out="$(inbb "$tmp/nb/app" -m app.direct)"
case "$out" in *"building native library jbt"*"direct 42") r=yes ;; *) r="no: $out" ;; esac
check "binding a dependency's symbol directly loads its native" "yes" "$r"
check "...building it once" "1" "$(builds)"

# ...and when it cannot load, the call's "no entry" names the library
fresh
out="$(export JOLT_NATIVE_BUILD=0; inbb "$tmp/nb/app" -m app.direct)"
case "$out" in *'no entry for "jbt_answer"'*"required native library jbt not found"*) r=yes ;; *) r="no: $out" ;; esac
check "...and a library that cannot load is named in the call's error" "yes" "$r"

# a probe for a symbol nothing defines opens no pending library
fresh
out="$(inbb "$tmp/nb/app" -m app.probe)"
check "ffi/find-symbol of a missing symbol leaves the dependency pending" "probe nil 1" "$out"
check "...and builds nothing" "0" "$(builds)"

# a binary carries a dependency's load only with one of its namespaces. The
# build still builds the library, since the binary may bind its symbols.
fresh
out="$(JOLT_PWD="$tmp/nb/app" JOLT_QUIET=1 "$JOLT" build -m app.plain -o "$tmp/nb-plain" 2>&1)"; s=$?
check "jolt build of an app requiring nothing of the dependency builds its library" "0 1" "$s $(builds)"
[ "$s" = 0 ] || echo "$out" >&2
out="$("$tmp/nb-plain" 2>&1)"
check "...and its binary starts without the library" "plain []" "$out"

# a binary binding the symbol directly, from a fresh checkout: nothing but the
# build itself produces the library
fresh
out="$(JOLT_PWD="$tmp/nb/app" JOLT_QUIET=1 "$JOLT" build -m app.direct -o "$tmp/nb-direct" 2>&1)"; s=$?
check "jolt build of an app binding a dependency's symbol builds its library" "0 1" "$s $(builds)"
[ "$s" = 0 ] || echo "$out" >&2
out="$("$tmp/nb-direct" 2>&1)"
check "...and the binary finds the symbol" "direct 42" "$out"
rm -f "$tmp/nb/lib/native/libjbt.so" "$tmp/nb/lib/native/libjbt.dylib"
out="$("$tmp/nb-direct" 2>&1)"
case "$out" in *'no entry for "jbt_answer"'*"required native library"*"libjbt"*) r=yes ;; *) r="no: $out" ;; esac
check "...and without the library, its error names it" "yes" "$r"

# a JOLT_NATIVE_BUILD=0 build of it fails, naming the library and the task
fresh
out="$(export JOLT_NATIVE_BUILD=0; JOLT_PWD="$tmp/nb/app" JOLT_QUIET=1 "$JOLT" build -m app.direct -o "$tmp/nb-direct" 2>&1)"; s=$?
check "jolt build with a dependency's library missing and auto-build off fails" "1" "$([ "$s" = 0 ] && echo 0 || echo 1)"
case "$out" in *"required native library jbt not found"*"auto-build is off"*"(declared by dependency jbt/jbt)"*) r=yes ;; *) r="no: $out" ;; esac
check "...naming the library, the task and the dependency" "yes" "$r"
else
  echo "  (skipped :build cases: no C compiler)" >&2
fi

# --- bb.edn task bodies ------------------------------------------------------

# a code body runs, with :init's def and the global :requires alias in scope,
# wrapped by :enter/:leave (which see current-task)
check "code body + :init + :requires + :enter/:leave" \
  "enter hi
hello WORLD
leave hi" "$(inbb "$BB" hi)"

# a bare (non-map) task value is the body
check "bare body" "enter bare
bare body
leave bare" "$(inbb "$BB" bare)"

# :depends run first, and only once across the whole invocation
check ":depends dedupe + order" \
  "enter clean
cleaning
leave clean
enter build
building
leave build
enter both
both
leave both" "$(inbb "$BB" both)"

# per-task :requires
check "per-task :requires" "enter needs
from-bbproj-core
leave needs" "$(inbb "$BB" needs)"

# bb.edn :paths put every listed root on the source path
check "bb.edn :paths (script root)" "enter helper
from-script-root
leave helper" "$(inbb "$BB" helper)"

# trailing args reach the task as *command-line-args*
check "*command-line-args*" 'enter args
args: ("a" "b")
leave args' "$(inbb "$BB" args a b)"

# the first standalone "--" ends option parsing; a later one is program data
check "*command-line-args* consumes one --" 'enter args
args: ("a" "--" "b")
leave args' "$(inbb "$BB" args -- a -- b)"

# babashka.tasks/shell is referred into the task namespace
check "shell" "enter echo
from-shell
leave echo" "$(inbb "$BB" echo)"

# babashka.tasks/run invokes another task in-process
# A project file is edn that carries CODE, so a task body's reader macros —
# quote, deref, syntax-quote/unquote, #() — have to read. EDN refuses ' @ ` ~
# outright, so reading these files with the edn reader breaks every one of them.
check "reader macros in a bb.edn task body" "enter macros
macros: 41 sym [1 7] [2 4]
leave macros" "$(inbb "$BB" macros)"

check "run" "enter nested
before
enter clean
cleaning
leave clean
after
leave nested" "$(inbb "$BB" nested)"

# a string body is a shell command line (jolt's form, in either file)
check "string body is a shell command" "enter shcmd
shell-string
second
leave shcmd" "$(inbb "$BB" shcmd)"

# the `clojure` and `jolt` fns referred into the task ns must not shadow the
# clojure.* / jolt.* namespace prefixes
check "fully-qualified clojure.*/jolt.* names still resolve" "enter fq
FQ 2 true
leave fq" "$(inbb "$BB" fq)"

# --- exit codes --------------------------------------------------------------

check "failed shell exits with its code" "7" "$(status "$BB" boom)"
check "successful task exits 0"          "0" "$(status "$BB" hi)"
check "unknown task exits non-zero"      "1" "$(status "$BB" nosuchtask)"
check "failing string task exits with its code" "3" "$(status "$DEPS" fail)"
check "failing string task (bb project)"        "4" "$(status "$BOTH" dfail)"

# --- the tasks listing -------------------------------------------------------

out="$(inbb "$BB" tasks)"
echo "$out" | grep -q "^The following tasks are available:" \
  && check "tasks header" "yes" "yes" || check "tasks header" "yes" "no"
echo "$out" | grep -q "^hi *say hi$" \
  && check "tasks lists name + :doc" "yes" "yes" || check "tasks lists name + :doc" "yes" "no"
echo "$out" | grep -q "secret" \
  && check "tasks hides :private" "hidden" "shown" || check "tasks hides :private" "hidden" "hidden"
echo "$out" | grep -q "^bare$" \
  && check "tasks lists a doc-less task" "yes" "yes" || check "tasks lists a doc-less task" "yes" "no"
# A leading dash is babashka's other spelling of :private, and a bb.edn we read
# may well use it. Hidden from the listing, but still runnable.
echo "$out" | grep -q -- "-dash" \
  && check "tasks hides a -dash name" "hidden" "shown" || check "tasks hides a -dash name" "hidden" "hidden"
check "a -dash task still runs" "enter -dash
dash ran
leave -dash" "$(inbb "$BB" -dash)"
# One task per line is the listing's whole shape: the second line of a docstring
# would land in the name column and read as a task of its own.
echo "$out" | grep -q "^multi *first line$" \
  && check "tasks prints a doc's first line" "yes" "yes" || check "tasks prints a doc's first line" "yes" "no"
echo "$out" | grep -q "second line of the doc" \
  && check "tasks drops a doc's later lines" "dropped" "printed" || check "tasks drops a doc's later lines" "dropped" "dropped"

# --- run <task> --------------------------------------------------------------

check "run <task>" "enter clean
cleaning
leave clean" "$(inbb "$BB" run clean)"

# --- bb.edn as the whole project config --------------------------------------

check "bb.edn :paths drive run -m" 'bbproj main ("x")' "$(inbb "$BB" run -m bbproj.core x)"

# --- deps.edn + bb.edn side by side ------------------------------------------

check "bb.edn task sees bb.edn :paths" "bb task from-bb-paths" "$(inbb "$BOTH" btask)"
check "bb.edn task sees deps.edn :paths" "app ns reachable" "$(inbb "$BOTH" appns)"
check "deps.edn :tasks still run beside a bb.edn" "only-in-deps-edn" "$(inbb "$BOTH" donly)"
check "deps.edn :main-opts task beside a bb.edn" 'both main ("q")' "$(inbb "$BOTH" dmain q)"
check "bb.edn shadows a same-named deps.edn task" "bb.edn shadows deps.edn" "$(inbb "$BOTH" dtask)"
# a non-task command resolves from deps.edn alone: bb.edn's :paths must not
# displace the app's own source roots
out="$(inbb "$BOTH" path)"
case "$out" in
  *"/test/chez/tasks/both/src"*) check "path keeps deps.edn :paths" "yes" "yes" ;;
  *) check "path keeps deps.edn :paths" "yes" "no ($out)" ;;
esac
case "$out" in
  *"/test/chez/tasks/both/tsk"*) check "path excludes bb.edn :paths" "no" "yes ($out)" ;;
  *) check "path excludes bb.edn :paths" "no" "no" ;;
esac

# --- jolt's own deps.edn :tasks forms ----------------------------------------

check "deps.edn string task"    "deps-only-hello"           "$(inbb "$DEPS" hello)"
check "reader macros in a deps.edn task body" "macros: 41 sym [2 4]" "$(inbb "$DEPS" macros)"
check "deps.edn :main-opts task" 'depsonly main ("z")'      "$(inbb "$DEPS" main z)"
check "a :main-opts task consumes one --" 'depsonly main ("z" "--" "w")' "$(inbb "$DEPS" main -- z -- w)"

# --- per-task roots and deps -------------------------------------------------

check "task :extra-paths" "enter xp
from-task-extra-paths
leave xp" "$(inbb "$BB" xp)"
# …and only for that task: the extra root is gone again for the next one
out="$(inbb "$BB" needs)"
check "task :extra-paths don't leak" "enter needs
from-bbproj-core
leave needs" "$out"
check "bb.edn :deps reach a task" "from-bb-edn-dep" "$(inbb "$BOTH" bbdep)"

# --- babashka.tasks/jolt and clojure -----------------------------------------

check "clojure re-invokes the jolt CLI" "enter viajolt
:from-clojure-fn
leave viajolt" "$(inbb "$BB" viajolt)"
check "clojure with an options map + tokenized args" "enter viajolt2
:tokenized
leave viajolt2" "$(inbb "$BB" viajolt2)"
check "jolt is the same fn under its own name" "enter viajolt3
:from-jolt-fn
leave viajolt3" "$(inbb "$BB" viajolt3)"

# --- :override-builtin -------------------------------------------------------

check ":override-builtin takes the command" "enter path
overridden path
leave path" "$(inbb "$BB" path)"
# without it a task name that collides with a command does NOT take it: `both`
# declares a `path` task that does not claim the name, so `path` there is still
# the built-in
out="$(inbb "$BOTH" path)"
case "$out" in
  */test/chez/tasks/both/src*) check "no :override-builtin keeps the command" "yes" "yes" ;;
  *) check "no :override-builtin keeps the command" "yes" "no ($out)" ;;
esac
case "$out" in
  *not-the-builtin*) check "...and does not run the task" "no" "yes ($out)" ;;
  *) check "...and does not run the task" "no" "no" ;;
esac
# …but it does not lose the name in silence. Without this the built-in answers as
# if the task were not there — `jolt build` in a project with a `build` task
# fails with "build needs an entry: -m NS", which reads like jolt dropped the
# task (jolt-935). Both ways out of the collision are named.
case "$out" in
  *"warning: the task \`path\` is shadowed"*) check "a shadowed task is reported" "yes" "yes" ;;
  *) check "a shadowed task is reported" "yes" "no ($out)" ;;
esac
case "$out" in
  *"jolt run path"*":override-builtin"*) check "...naming both ways out" "yes" "yes" ;;
  *) check "...naming both ways out" "yes" "no ($out)" ;;
esac
# The escape hatch the warning offers has to work, and the one that claims the
# name must not be warned about.
check "run <task> reaches a shadowed task" "not-the-builtin" "$(inbb "$BOTH" run path)"
case "$(inbb "$BB" path)" in
  *shadowed*) check "an :override-builtin task is not warned about" "quiet" "warned" ;;
  *) check "an :override-builtin task is not warned about" "quiet" "quiet" ;;
esac
case "$(inbb "$BOTH" tasks)" in
  *shadowed*) check "a command with no task of its name is quiet" "quiet" "warned" ;;
  *) check "a command with no task of its name is quiet" "quiet" "quiet" ;;
esac

# --- errors ------------------------------------------------------------------

check "a dependency cycle is an error, not a hang" "1" "$(status "$BB" cyc1)"
inbb "$BB" cyc1 2>&1 | grep -q "circular task dependency" \
  && check "cycle error names the problem" "yes" "yes" \
  || check "cycle error names the problem" "yes" "no"

# The same claim under --parallel, and for a cycle that spans two sibling
# branches rather than one chain. Like the row above, a regression here HANGS
# rather than failing — that is the shape of the bug these pin (per-thread cycle
# detection let two parallel branches wait on each other forever).
check "a cycle is an error under --parallel" "1" "$(status "$BB" run --parallel cyc1)"
check "a cross-branch cycle is an error" "1" "$(status "$BB" xcyc)"
check "a cross-branch cycle is an error under --parallel" "1" \
  "$(status "$BB" run --parallel xcyc)"
inbb "$BB" run --parallel xcyc 2>&1 | grep -q "circular task dependency" \
  && check "cross-branch cycle error names the problem" "yes" "yes" \
  || check "cross-branch cycle error names the problem" "yes" "no"

printf '{:paths ["src"]}\n' > "$tmp/deps.edn"
check "tasks with none declared" "No tasks found. Add a :tasks map to bb.edn or deps.edn." \
  "$(inbb "$tmp" tasks)"

# --- run --parallel ----------------------------------------------------------

check "run --parallel exits 0" "0" "$(status "$BB" run --parallel both)"
check "run --parallel runs a shared dependency once" "1" \
  "$(inbb "$BB" run --parallel both | grep -c '^cleaning$')"
# a failed :depends reports its own status either way — under --parallel the
# future's deref wraps the throw, and reading only the top ex-data lost it
check "failed :depends exits with its code"            "7" "$(status "$BB" pfail)"
check "failed :depends exits with its code (parallel)" "7" "$(status "$BB" run --parallel pfail)"

# --- CLI tasks (:exec-fn / :cmd) ---------------------------------------------
#
# A task that names a handler or a command tree parses its arguments with
# babashka.cli before anything runs. Fixture: test/chez/tasks/cliproj, whose
# handlers print the map they were called with, so what is asserted here is the
# map the runner built rather than a side effect further down.

check "an :exec-fn task is called with the parsed options" \
  'greet {:loud true, :name "ada"}' "$(inbb "$CLI" greet --name ada --loud)"
check "...with the spec's defaults filled in" 'greet {:name "world"}' \
  "$(inbb "$CLI" greet)"
check "...and a bad value is refused" "1" "$(status "$CLI" serve --port nope)"

# :exec-args on the task wins over the one under :cli, as an exec call's does
check ":exec-args defaults, the task's own winning" 'greet {:loud true, :name "ada"}' \
  "$(inbb "$CLI" tone)"
check "...and the command line still wins over both" 'greet {:loud true, :name "eve"}' \
  "$(inbb "$CLI" tone --name eve)"

# the spec may live on the handler var instead of in bb.edn, which is where
# `bb -x` reads it from too
check "a spec on the handler var is used" "serve {:port 8080}" "$(inbb "$CLI" serve)"
check "...and parses against it" "serve {:port 9090}" "$(inbb "$CLI" serve --port 9090)"

# :cli may name a def, for options edn cannot express
check ":cli naming a def is resolved" 'greet {:name "grace"}' \
  "$(inbb "$CLI" bysym --name grace)"

# --help comes for free, and prints the task's own name
out="$(inbb "$CLI" greet --help)"
case "$out" in *"Usage: jolt greet"*) r=yes ;; *) r="no: $out" ;; esac
check "--help prints usage for the task" "yes" "$r"
case "$out" in *"who to greet"*) r=yes ;; *) r="no: $out" ;; esac
check "...including each option's :desc" "yes" "$r"
check "...and exits 0" "0" "$(status "$CLI" greet --help)"
check "-h is the same" "0" "$(status "$CLI" greet -h)"

# The reason the :depends walk moved inside the parser: asking what a task
# accepts must not run the task's dependencies.
check "--help does not run :depends" "" \
  "$(inbb "$CLI" helped --help | grep '^prep ran$')"
check "...which do run when the task does" "prep ran" \
  "$(inbb "$CLI" helped --name ada | grep '^prep ran$')"

# a :cmd tree
check "a :cmd command dispatches" "migrate {:steps 3}" "$(inbb "$CLI" db migrate --steps 3)"
check "...and a sibling command is its own" "seed {}" "$(inbb "$CLI" db seed)"
check "naming no command is an error" "1" "$(status "$CLI" db)"
case "$(inbb "$CLI" db)" in *migrate*seed*) r=yes ;; *) r="no" ;; esac
check "...listing the commands there are" "yes" "$r"
check "a task with a tree AND a body runs the body as its root" "dbx default" \
  "$(inbb "$CLI" dbx)"
check "...and the tree when a command is named" "sub {:x 1}" "$(inbb "$CLI" dbx go --x 1)"

# A CLI task named in :depends does not parse: its handler is called with what
# the target's parse produced, narrowed by its own :restrict.
check "a CLI :depends handler runs before the target" "setup {:verbose true}" \
  "$(inbb "$CLI" app --port 9000 --verbose | grep '^setup')"
check "...and :restrict narrows what it is handed" "setup {:verbose true}" \
  "$(inbb "$CLI" app --port 9000 --verbose | grep '^setup')"
check "...while the target's handler gets the whole map" \
  "serve {:port 9000, :verbose true}" \
  "$(inbb "$CLI" app --port 9000 --verbose | grep '^serve')"
# the dependency's spec merges into the target's, so the option parses at all
check "a dependency's option is accepted by the target" "0" \
  "$(status "$CLI" app --verbose)"
case "$(inbb "$CLI" app --help)" in *--verbose*) r=yes ;; *) r="no" ;; esac
check "...and shows up in the target's help" "yes" "$r"

# a :cmd task is a tree, not one thing to run
check ":depends naming a :cmd task is refused" "1" "$(status "$CLI" baddep)"
inbb "$CLI" baddep 2>&1 | grep -q "no single handler" \
  && check "...saying why" "yes" "yes" \
  || check "...saying why" "yes" "no"

# runner-level parser options, the :tasks-map :cli entry — like :init and
# :requires, it reaches every CLI task in the map
check "a runner-level :cli spec reaches a task" 'greet {:name "ada", :trace true}' \
  "$(inbb "$CLI" greet --trace --name ada)"
case "$(inbb "$CLI" greet --help)" in *"trace the run"*) r=yes ;; *) r="no" ;; esac
check "...and shows up in its help" "yes" "$r"

# a :cmd may name a def, so a large tree lives in code rather than in edn
check "a :cmd naming a def is resolved" "tree-by-sym {}" "$(inbb "$CLI" dbsym up)"

# a handler that throws fails the task, like a body that throws
check "a throwing handler fails the task" "1" "$(status "$CLI" boom)"
inbb "$CLI" boom 2>&1 | grep -q "handler said no" \
  && check "...with its own message" "yes" "yes" \
  || check "...with its own message" "yes" "no"

# babashka.tasks/run reaching a CLI task: it is a target of its own, so it parses
check "run from a body parses" 'greet {:name "eve"}' "$(inbb "$CLI" viarun --name eve)"

# the listing treats a CLI task like any other
case "$(inbb "$CLI" tasks)" in *"greet   greet someone"*) r=yes ;; *) r="no" ;; esac
check "a CLI task is listed with its :doc" "yes" "$r"

echo "tasks-smoke: $pass passed, $fail failed"
[ "$fail" -eq 0 ]
