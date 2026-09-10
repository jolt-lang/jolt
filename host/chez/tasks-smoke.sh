#!/bin/sh
# tasks-smoke.sh — bb.edn / deps.edn task running through the real CLI.
#
# Fixture projects live in test/chez/tasks/: `bbproj` has only a bb.edn (so
# bb.edn is the whole project config), `both` has a deps.edn AND a bb.edn (the
# app resolves from deps.edn; bb.edn's paths join in for task runs), and
# `depsonly` pins jolt's own deps.edn :tasks forms.
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
# has no :override-builtin task, so `path` there is still the built-in
case "$(inbb "$BOTH" path)" in
  */test/chez/tasks/both/src*) check "no :override-builtin keeps the command" "yes" "yes" ;;
  *) check "no :override-builtin keeps the command" "yes" "no" ;;
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

echo "tasks-smoke: $pass passed, $fail failed"
[ "$fail" -eq 0 ]
