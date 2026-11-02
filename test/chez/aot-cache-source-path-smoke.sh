#!/bin/sh
# aot-cache-source-path smoke: a namespace a require loads is named by its
# classpath name, the same answer cold or warm, from any directory and any
# spelling of its root, and a project that moves keeps its cache.
#
# A namespace's name is baked into its cached artifact at compile time: the var's
# :file metadata, and any *file* a macro read while expanding. It used to be the
# path as the root spelled it, so :paths ["src"] gave "./src/mylib/where.clj" and
# an absolute root an absolute path, and the cache (keyed on content) replayed the
# first load's spelling everywhere else. It is the classpath name now, as on the
# JVM:
#
#   cwd .../elsewhere, root /abs/proj/src
#   before:  {:file "./src/mylib/where.clj"}    <- no such file from here
#   after:   {:file "mylib/where.clj"}          <- what the JVM says; io/resource opens it
#
# load-file names a file by its absolute path, as Compiler.loadFile does.

set -e

pass=0
fails=0
root="$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)"

# The built binary, not bin/jolt: bin/jolt cd's to the repo and passes the project
# dir as JOLT_PWD, so its roots are already absolute and the relative spelling
# never arises.
jolt="$root/target/release/jolt"
cache="$(mktemp -d)"
tmp="$(cd "$(mktemp -d)" && pwd -P)"
trap 'rm -rf "$cache" "$tmp"' EXIT
mkdir -p "$tmp/proj/src/mylib" "$tmp/elsewhere"

cat > "$tmp/proj/src/mylib/where.clj" <<'EOF'
(ns mylib.where)
(defmacro compiled-in [] *file*)
(defn f [] 1)
(def baked (compiled-in))
EOF
echo '{:paths ["src"]}' > "$tmp/proj/deps.edn"
echo '{}' > "$tmp/elsewhere/deps.edn"

# Prints :file, the macro's *file*, and whether io/resource opens :file.
probe='(require (quote mylib.where) (quote clojure.java.io))
       (let [file (:file (meta (var mylib.where/f)))]
         (println file)
         (println mylib.where/baked)
         (println (some? (clojure.java.io/resource file))))'

# $1 = cwd, $2 = JOLT_AOT_CACHE, rest = extra jolt args before -M
run_in() {
  dir="$1"; aot="$2"; shift 2
  (cd "$dir" && JOLT_AOT_CACHE="$aot" JOLT_CACHE_DIR="$cache" JOLT_QUIET=1 \
     "$jolt" "$@" -M -e "$probe" 2>&1) || true
}
# The same run with the cache's report on stderr, for whether it hit.
hit_in() {
  dir="$1"; shift
  (cd "$dir" && JOLT_AOT_CACHE=1 JOLT_CACHE_DIR="$cache" JOLT_QUIET=1 JOLT_DEBUG=1 \
     "$jolt" "$@" -M -e "$probe" 2>&1 >/dev/null) > "$tmp/hit.log"
  grep -c '\[jolt.aot\] hit mylib.where' "$tmp/hit.log" || true
}

want='mylib/where.clj
mylib/where.clj
true'

check() {
  label="$1"; out="$2"
  if [ "$out" = "$want" ]; then
    echo "PASS: ($label) the classpath name, and io/resource opens it"; pass=$((pass+1))
  else
    echo "FAIL: ($label)"
    echo "  got:";  printf '%s\n' "$out"  | sed 's/^/    /'
    echo "  want:"; printf '%s\n' "$want" | sed 's/^/    /'
    fails=$((fails+1))
  fi
}
check_hit() {
  label="$1"; n="$2"
  if [ "$n" -ge 1 ]; then
    echo "PASS: ($label) a cache hit"; pass=$((pass+1))
  else
    echo "FAIL: ($label) recompiled instead of hitting the cache"; fails=$((fails+1))
    echo "  the cache's report:"; grep '\[jolt.aot\]' "$tmp/hit.log" | sed 's/^/    /'
    echo "  the cache dir:"; find "$cache" -name '*mylib*' | sed 's/^/    /'
  fi
}

abs_deps="{:paths [\"$tmp/proj/src\"]}"

# (a) uncached, in the project
check a "$(run_in "$tmp/proj" 0)"
# (b) cold, in the project: the miss that fills the cache
check b "$(run_in "$tmp/proj" 1)"
# (c) same source from another directory through an absolute root: same name,
#     and the same artifact
check c "$(run_in "$tmp/elsewhere" 1 -Sdeps "$abs_deps")"
check_hit c "$(hit_in "$tmp/elsewhere" -Sdeps "$abs_deps")"
# (d) the project moved: its cache still answers
mv "$tmp/proj" "$tmp/moved"
check d "$(run_in "$tmp/moved" 1)"
check_hit d "$(hit_in "$tmp/moved")"

# (e) load-file: the absolute path, as Compiler.loadFile gives
out="$(cd "$tmp/moved" && JOLT_CACHE_DIR="$cache" JOLT_QUIET=1 "$jolt" -M -e \
  '(load-file "src/mylib/where.clj") (println (:file (meta (var mylib.where/f)))) (println mylib.where/baked)' 2>&1 || true)"
want_lf="$tmp/moved/src/mylib/where.clj
$tmp/moved/src/mylib/where.clj"
if [ "$out" = "$want_lf" ]; then
  echo "PASS: (e) load-file names the file by its absolute path"; pass=$((pass+1))
else
  echo "FAIL: (e) load-file"
  echo "  got:";  printf '%s\n' "$out"    | sed 's/^/    /'
  echo "  want:"; printf '%s\n' "$want_lf" | sed 's/^/    /'
  fails=$((fails+1))
fi

# (f) ...and a leading ./ stays in it, as File.getAbsolutePath keeps it
out="$(cd "$tmp/moved" && JOLT_CACHE_DIR="$cache" JOLT_QUIET=1 "$jolt" -M -e \
  '(load-file "./src/mylib/where.clj") (println (:file (meta (var mylib.where/f)))) (println mylib.where/baked)' 2>&1 || true)"
want_lf="$tmp/moved/./src/mylib/where.clj
$tmp/moved/./src/mylib/where.clj"
if [ "$out" = "$want_lf" ]; then
  echo "PASS: (f) load-file keeps a leading ./"; pass=$((pass+1))
else
  echo "FAIL: (f) load-file ./"
  echo "  got:";  printf '%s\n' "$out"    | sed 's/^/    /'
  echo "  want:"; printf '%s\n' "$want_lf" | sed 's/^/    /'
  fails=$((fails+1))
fi

echo
echo "aot-cache-source-path smoke: $pass passed, $fails failed"
[ "$fails" -eq 0 ]
