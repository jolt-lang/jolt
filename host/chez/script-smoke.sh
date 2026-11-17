#!/bin/sh
# script-smoke.sh — running a FILE as a script through the real CLI, including a
# `#!/usr/bin/env jolt` shebang line executed by the kernel.
#
# The babashka shape: a file with a shebang, marked executable, run as `./tool`
# or as `jolt tool`, with no extension and no project. Everything the script
# needs from the CLI is asserted here — the shebang line being read as a comment,
# *command-line-args*, *file*, stdin left readable, exit-code propagation, and a
# project's paths being on the roots when there is a project — plus which of a
# FILE, a builtin command and a task wins when a name could be more than one.
#
# JOLT_BIN overrides the binary under test (defaults to bin/jolt source mode).
# Note that bin/jolt cd's to its own checkout, so a project-relative script path
# only resolves through JOLT_PWD: this gate runs every case that way on purpose.
set -u
root="$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)"
cd "$root"
JOLT="${JOLT_BIN:-bin/jolt}"
case "$JOLT" in /*) JOLT_ABS="$JOLT" ;; *) JOLT_ABS="$root/$JOLT" ;; esac
export JOLT_EXE="$JOLT_ABS"
export JOLT_NO_USER_DEPS=1
pass=0; fail=0
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

# A project dir the CLI is pointed at with JOLT_PWD, the way a user's cwd reaches
# it, and a second one that has a deps.edn.
bare="$tmp/bare"; proj="$tmp/proj"
mkdir -p "$bare" "$proj/src/app" "$proj/test" "$bare/sub"

# The script every case runs unless it says otherwise: no extension, executable,
# and a shebang line the reader has to treat as a comment.
cat > "$bare/hello" <<'EOF'
#!/usr/bin/env jolt
(println "hello" (pr-str *command-line-args*))
EOF
chmod +x "$bare/hello"

cat > "$bare/sub/nested" <<'EOF'
#!/usr/bin/env jolt
(println "nested ok")
EOF
chmod +x "$bare/sub/nested"

# Same file with a .clj extension: the extension path and the exists path must
# agree, and the shebang is legal in both.
cp "$bare/hello" "$bare/hello.clj"

cat > "$bare/info" <<'EOF'
#!/usr/bin/env jolt
(require '[clojure.string :as str])
(println "file" (str/ends-with? *file* "info") "args" (pr-str *command-line-args*))
EOF
chmod +x "$bare/info"

# The common bb script shape: an ns form with requires, under the shebang.
cat > "$bare/withns" <<'EOF'
#!/usr/bin/env jolt
(ns myscript
  (:require [clojure.string :as str]))

(defn -main [& args] (println (str/join "," args)))

(apply -main *command-line-args*)
EOF
chmod +x "$bare/withns"

# Not executable: the exec bit is the kernel's business, not the CLI's.
cat > "$bare/noexec" <<'EOF'
#!/usr/bin/env jolt
(println "loaded without an exec bit")
EOF

cat > "$bare/piped" <<'EOF'
#!/usr/bin/env jolt
(println "read" (read-line))
EOF
chmod +x "$bare/piped"

cat > "$bare/bye" <<'EOF'
#!/usr/bin/env jolt
(println "before exit")
(System/exit 3)
EOF
chmod +x "$bare/bye"

cat > "$bare/boom" <<'EOF'
#!/usr/bin/env jolt
(throw (ex-info "boom" {}))
EOF
chmod +x "$bare/boom"

# A script whose NAME is a builtin command, and one whose name is a task: the
# command and the file each have a claim on the argv, and the answers are pinned
# below rather than left to whichever arm happens to come first.
cat > "$bare/build" <<'EOF'
#!/usr/bin/env jolt
(println "the build script, not the build command")
EOF
chmod +x "$bare/build"

# The project: a deps.edn with :paths and a :tasks entry that shares its name
# with a file, plus a `test` DIRECTORY (every jolt project has one, and a
# directory must never be dispatched as a script).
cat > "$proj/deps.edn" <<'EOF'
{:paths ["src"]
 :tasks {greet (println "the greet TASK")}}
EOF
cat > "$proj/src/app/util.clj" <<'EOF'
(ns app.util)
(defn shout [s] (str s "!"))
EOF
cat > "$proj/greet" <<'EOF'
#!/usr/bin/env jolt
(println "the greet SCRIPT")
EOF
chmod +x "$proj/greet"
cat > "$proj/uses-project" <<'EOF'
#!/usr/bin/env jolt
(require '[app.util :as u])
(println (u/shout "project paths reached"))
EOF
chmod +x "$proj/uses-project"

# stdout+stderr of a command run with a project dir; and its exit status alone.
# (`in` is a shell keyword, hence runp.)
runp()   { d="$1"; shift; JOLT_PWD="$d" JOLT_QUIET=1 "$JOLT_ABS" "$@" 2>&1; }
status() { d="$1"; shift; JOLT_PWD="$d" JOLT_QUIET=1 "$JOLT_ABS" "$@" >/dev/null 2>&1; echo $?; }

# --- jolt FILE, where FILE is a shebang script with no extension --------------
check "jolt FILE runs an extensionless shebang script" "hello nil" \
  "$(runp "$bare" hello)"
check "...and its arguments arrive as *command-line-args*" 'hello ("a" "b")' \
  "$(runp "$bare" hello a b)"
check "...including ones that look like options" 'hello ("--foo" "-x")' \
  "$(runp "$bare" hello --foo -x)"
check "...with the first standalone -- consumed as end-of-options" 'hello ("z")' \
  "$(runp "$bare" hello -- z)"
check "...and exits 0" "0" "$(status "$bare" hello)"

check "a path into a subdirectory runs" "nested ok" "$(runp "$bare" sub/nested)"
check "the same file with a .clj extension runs the same way" "hello nil" \
  "$(runp "$bare" hello.clj)"
check "run FILE takes the same script" "hello nil" "$(runp "$bare" run hello)"

check "*file* is the script and a require works from it" 'file true args ("q")' \
  "$(runp "$bare" info q)"

check "a script may carry an ns form with requires" "a,b" \
  "$(runp "$bare" withns a b)"
check "a file with no exec bit still runs as an argument" "loaded without an exec bit" \
  "$(runp "$bare" noexec)"

# The installed-binary shape: no JOLT_PWD at all, so the path is resolved against
# the process directory. (bin/jolt sets JOLT_PWD from $PWD on its own, which is
# the same directory here, so this case is meaningful for both.)
check "jolt FILE resolves against the caller's own cwd with no JOLT_PWD" "hello nil" \
  "$(cd "$bare" && unset JOLT_PWD; JOLT_QUIET=1 "$JOLT_ABS" hello 2>&1)"

# --- the shebang line, executed by the kernel ---------------------------------
# /usr/bin/env has to find a `jolt` on PATH: a wrapper that execs the binary
# under test, so this exercises the same argv the kernel builds for any user.
mkdir -p "$tmp/bin"
printf '#!/bin/sh\nexec "%s" "$@"\n' "$JOLT_ABS" > "$tmp/bin/jolt"
chmod +x "$tmp/bin/jolt"
check "./script runs through its shebang line" 'hello ("k")' \
  "$(cd "$bare" && PATH="$tmp/bin:$PATH" ./hello k 2>&1)"
check "...from a subdirectory of the caller's cwd too" "nested ok" \
  "$(cd "$bare" && PATH="$tmp/bin:$PATH" ./sub/nested 2>&1)"

# A SYMLINK on PATH, not a wrapper: putting a checkout's launcher on PATH as
# `ln -s .../bin/jolt ~/.local/bin/jolt` is what a contributor does to make
# `#!/usr/bin/env jolt` work, and bin/jolt derives its checkout root from $0 — so
# this case names bin/jolt directly rather than $JOLT_BIN. A built binary does not
# care where it is called from and passes either way; the source launcher used to
# resolve the root to the symlink's own directory and die looking for
# tools/version.sh there.
mkdir -p "$tmp/lnbin"
ln -sf "$root/bin/jolt" "$tmp/lnbin/jolt"
ln -sf "$root/bin/joltc" "$tmp/lnbin/joltc"
check "a shebang script runs through a symlinked bin/jolt on PATH" 'hello ("s")' \
  "$(cd "$bare" && PATH="$tmp/lnbin:$PATH" ./hello s 2>&1)"
check "...and bin/joltc, the shim beside it, resolves the same way" "3" \
  "$(cd "$bare" && PATH="$tmp/lnbin:$PATH" joltc -e '(+ 1 2)' 2>&1)"

# --- what a script inherits ---------------------------------------------------
check "stdin is left readable by the script" "read piped in" \
  "$(printf 'piped in\n' | runp "$bare" piped)"
check "System/exit propagates the status" "3" "$(status "$bare" bye)"
check "...having run what came before it" "before exit" "$(runp "$bare" bye)"
check "an uncaught exception exits nonzero" "1" "$(status "$bare" boom)"

# --- a project's paths and deps ----------------------------------------------
check "a script sees the project's source roots" "project paths reached!" \
  "$(runp "$proj" uses-project)"

# --- FILE vs command vs task -------------------------------------------------
# A builtin command wins over a file of the same name: `jolt build` has to stay
# the compiler however a directory is laid out. -f is how the file is named
# unambiguously (bb's spelling), and it is the only way to run this script.
case "$(runp "$bare" build)" in
  *"the build script"*) r="ran the script" ;;
  *"build needs"*|*"-m"*) r="ran the build command" ;;
  *) r="other: $(runp "$bare" build)" ;;
esac
check "a builtin command still wins over a file of the same name" \
  "ran the build command" "$r"
check "-f FILE runs a script whose name is a builtin command" \
  "the build script, not the build command" "$(runp "$bare" -f build)"
check "--file is the same option" "the build script, not the build command" \
  "$(runp "$bare" --file build)"
check "-f passes the rest as *command-line-args*" 'hello ("x")' \
  "$(runp "$bare" -f hello x)"
check "run -f FILE works too" "hello nil" "$(runp "$bare" run -f hello)"
check "-f with no FILE is an error, not a REPL" "1" "$(status "$bare" -f)"

# A file wins over a TASK of the same name (a task is the fallback for a token
# that names nothing on disk), and a DIRECTORY is never a script.
check "a file wins over a task of the same name" "the greet SCRIPT" \
  "$(runp "$proj" greet)"
case "$(runp "$proj" test)" in
  *"is a directory"*) r="tried to load the directory" ;;
  *"unknown command or task"*) r="fell through to the task lookup" ;;
  *) r="other: $(runp "$proj" test)" ;;
esac
check "a directory is not a script" "fell through to the task lookup" "$r"

# --- stdin as the program, which is the shebang case without a file ----------
check "- runs a program read from stdin" "stdin program" \
  "$(printf '(println "stdin program")\n' | runp "$bare" -)"

# --- jolt.file: the script being run, as bb's babashka.file -----------------
# What lets a file tell being run from being loaded by another script. It names
# the top-level script, so a file the script load-files sees someone else's path.
cat > "$bare/guarded" <<'EOF'
#!/usr/bin/env jolt
(def me (.getCanonicalPath (java.io.File. *file*)))
(def running (some-> (System/getProperty "jolt.file") java.io.File. .getCanonicalPath))
(when (= me running) (println "main of guarded"))
(load-file (str (.getParent (java.io.File. *file*)) "/guarded-lib"))
EOF
cat > "$bare/guarded-lib" <<'EOF'
(if (= (.getCanonicalPath (java.io.File. *file*))
       (some-> (System/getProperty "jolt.file") java.io.File. .getCanonicalPath))
  (println "main of lib")
  (println "lib loaded"))
EOF
check "jolt.file names the script run, not a file it loads" "main of guarded
lib loaded" "$(runp "$bare" guarded)"
printf '(println (.isAbsolute (java.io.File. (System/getProperty "jolt.file"))))\n' > "$bare/abs"
check "jolt.file is absolute for a relative script path" "true" "$(runp "$bare" abs)"
check "standard input leaves jolt.file unset" "nil" \
  "$(printf '(prn (System/getProperty "jolt.file"))\n' | runp "$bare" -)"

# --- the usage text says all this exists -------------------------------------
usage="$(runp "$bare" --help)"
# The bare-FILE line, not `run FILE [args]` — which was already there and would
# make this check pass without it.
case "$usage" in *"with \`run\` left out"*) r=yes ;; *) r=no ;; esac
check "--help documents a bare FILE" "yes" "$r"
case "$usage" in *"-f FILE"*) r=yes ;; *) r=no ;; esac
check "--help documents -f" "yes" "$r"
case "$usage" in *"#!"*) r=yes ;; *) r=no ;; esac
check "--help mentions the shebang line" "yes" "$r"

echo "script smoke: $pass passed, $fail failed"
[ "$fail" -eq 0 ]
