#!/bin/sh
# aot-cache smoke: verify the per-namespace AOT/compile cache.
#
# The cache fasls a required namespace's emitted Scheme on first load (miss)
# and loads the .so on subsequent loads (hit), keyed by source content hash +
# jolt version. This script drives the fast dev bin/joltc (devcache mode loads
# the same loader.ss, so the hook is exercised) with a temp cache dir.
#
# Phases (added incrementally):
#   1 — core miss/hit/invalidate        (this file)
#   2 — correctness edge cases          (macro/record/data-reader/transitive)
#   3 — bypass semantics                (:reload, install-owned never cached)
#   4 — performance gate                (cold vs warm wall-clock)

set -e

pass=0
fails=0
root="$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)"
cd "$root"

joltc="bin/joltc"
cache="$(mktemp -d)"
tmp="$(mktemp -d)"
mkdir -p "$tmp/src/mylib"

# A program that requires a disk-backed ns via add-deps (the real require path)
# and prints a value computed in it. \$1 = the temp project dir.
run_prog() {
  JOLT_AOT_CACHE=1 JOLT_CACHE_DIR="$cache" JOLT_QUIET=1 "$joltc" -e "
    (require 'jolt.deps)
    (jolt.deps/add-deps {:deps {'mylib/mylib {:local/root \"$1\"}}})
    (require 'mylib.core)
    (println (mylib.core/answer))" 2>/dev/null | tail -1
}

count_cache_files() { find "$cache" -name '*.so' 2>/dev/null | wc -l | tr -d ' '; }

# --- (a) cold load writes a .so and produces correct output -------------------
cat > "$tmp/src/mylib/core.clj" <<'CLJ'
(ns mylib.core)
(defn answer [] 42)
CLJ
out_a="$(run_prog "$tmp")"
n_a="$(count_cache_files)"
if [ "$out_a" = "42" ] && [ "$n_a" -ge 1 ]; then
  echo "PASS: (a) cold load output=42, cache files=$n_a"; pass=$((pass+1))
else
  echo "FAIL: (a) cold load output='$out_a' cache files=$n_a (expected output 42, >=1 .so)"; fails=$((fails+1))
fi

# --- (b) warm load produces identical output (cache hit) ----------------------
out_b="$(run_prog "$tmp")"
if [ "$out_b" = "42" ]; then
  echo "PASS: (b) warm load output=42"; pass=$((pass+1))
else
  echo "FAIL: (b) warm load output='$out_b' (expected 42)"; fails=$((fails+1))
fi

# --- (c) editing source invalidates: recompiles, output reflects the edit -----
sleep 1  # ensure mtime advances
cat > "$tmp/src/mylib/core.clj" <<'CLJ'
(ns mylib.core)
(defn answer [] 99)
CLJ
out_c="$(run_prog "$tmp")"
n_c="$(count_cache_files)"
if [ "$out_c" = "99" ]; then
  echo "PASS: (c) after edit output=99"; pass=$((pass+1))
else
  echo "FAIL: (c) after edit output='$out_c' (expected 99 — cache did not invalidate)"; fails=$((fails+1))
fi

# --- Phase 2: correctness the tee must preserve (cold == warm == expected) ----
# case_cold_warm <label> <projdir> <expr-after-add-deps> <expected>
# projdir has src/proj/core.clj (+ siblings); expr requires proj.core and prints
# (proj.core/run). Asserts cold and warm runs both print `expected`.
case_cold_warm() {
  clabel="$1"; cproj="$2"; cexpr="$3"; cexp="$4"
  cmd="(require 'jolt.deps) (jolt.deps/add-deps {:deps {'proj/proj {:local/root \"$cproj\"}}}) $cexpr"
  ccold="$(JOLT_AOT_CACHE=1 JOLT_CACHE_DIR="$cache" JOLT_QUIET=1 "$joltc" -e "$cmd" 2>/dev/null | tail -1)"
  cwarm="$(JOLT_AOT_CACHE=1 JOLT_CACHE_DIR="$cache" JOLT_QUIET=1 "$joltc" -e "$cmd" 2>/dev/null | tail -1)"
  if [ "$ccold" = "$cexp" ] && [ "$cwarm" = "$cexp" ]; then
    echo "PASS: ($clabel) cold='$ccold' warm='$cwarm'"; pass=$((pass+1))
  else
    echo "FAIL: ($clabel) cold='$ccold' warm='$cwarm' (expected '$cexp')"; fails=$((fails+1))
  fi
}

# (d) same-file macro def-then-use — the forward ref only works because the tee
# records each form AFTER its eval has marked the macro (so the later form's emit
# has the macro already expanded; the .so replays that).
d="$tmp/d"; mkdir -p "$d/src/proj"
cat > "$d/src/proj/core.clj" <<'CLJ'
(ns proj.core)
(defmacro mx [x] (str "macro-" x))
(defn run [] (mx 42))
CLJ
case_cold_warm "d" "$d" "(require 'proj.core) (println (proj.core/run))" "macro-42"

# (e) defrecord — compile-time type registration must reproduce on cache hit.
e="$tmp/e"; mkdir -p "$e/src/proj"
cat > "$e/src/proj/core.clj" <<'CLJ'
(ns proj.core)
(defrecord Pt [x y])
(defn run [] (:x (->Pt 7 8)))
CLJ
case_cold_warm "e" "$e" "(require 'proj.core) (println (proj.core/run))" "7"

# (f) data reader #tag in a required ns — the reader rewrite is baked into the
# captured emit (post ldr-apply-readers), so the .so carries it. data_readers.clj
# at the source root registers the reader (add-deps' set-source-roots! scans it).
f="$tmp/f"; mkdir -p "$f/src/proj"
cat > "$f/src/data_readers.clj" <<'CLJ'
{greet proj.dr/foo}
CLJ
cat > "$f/src/proj/dr.clj" <<'CLJ'
(ns proj.dr)
(defn foo [v] (str "got-" v))
CLJ
cat > "$f/src/proj/core.clj" <<'CLJ'
(ns proj.core (:require [proj.dr]))
(defn run [] #greet "hi")
CLJ
case_cold_warm "f" "$f" "(require 'proj.core) (println (proj.core/run))" "got-hi"

# (g) transitive require — proj.core requires proj.sub; the cached .so for
# proj.core re-triggers the require, loading proj.sub (from its own cache entry).
g="$tmp/g"; mkdir -p "$g/src/proj"
cat > "$g/src/proj/core.clj" <<'CLJ'
(ns proj.core (:require [proj.sub :as s]))
(defn run [] (s/subval))
CLJ
cat > "$g/src/proj/sub.clj" <<'CLJ'
(ns proj.sub)
(defn subval [] :subval)
CLJ
case_cold_warm "g" "$g" "(require 'proj.core) (println (proj.core/run))" ":subval"

# --- Phase 3: bypass semantics ------------------------------------------------
# (h) :reload bypasses the cache and picks up an edit even when a fresh .so for
# the OLD content exists. The :reload sets force?=#t → aot-load-or-compile takes
# the plain load-jolt-file branch (no read, no write of the cache), so the edited
# source compiles and runs.
h="$tmp/h"; mkdir -p "$h/src/proj"
printf '(ns proj.core)\n(defn answer [] 42)\n' > "$h/src/proj/core.clj"
hcmd="(require 'jolt.deps) (jolt.deps/add-deps {:deps {'proj/proj {:local/root \"$h\"}}})"
# cold: populate the cache with the v1 .so
JOLT_AOT_CACHE=1 JOLT_CACHE_DIR="$cache" JOLT_QUIET=1 "$joltc" -e "$hcmd (require 'proj.core) (println (proj.core/answer))" >/dev/null 2>&1
# warm (no reload): cache hit → still 42
warm1="$(JOLT_AOT_CACHE=1 JOLT_CACHE_DIR="$cache" JOLT_QUIET=1 "$joltc" -e "$hcmd (require 'proj.core) (println (proj.core/answer))" 2>/dev/null | tail -1)"
# edit, then :reload — must show the edit despite the stale v1 .so in the cache
sleep 1
printf '(ns proj.core)\n(defn answer [] 99)\n' > "$h/src/proj/core.clj"
reload_out="$(JOLT_AOT_CACHE=1 JOLT_CACHE_DIR="$cache" JOLT_QUIET=1 "$joltc" -e "$hcmd (require 'proj.core :reload) (println (proj.core/answer))" 2>/dev/null | tail -1)"
if [ "$warm1" = "42" ] && [ "$reload_out" = "99" ]; then
  echo "PASS: (h) warm=$warm1, :reload-after-edit=$reload_out"; pass=$((pass+1))
else
  echo "FAIL: (h) warm='$warm1' (want 42), :reload-after-edit='$reload_out' (want 99)"; fails=$((fails+1))
fi

# (i) install-owned namespaces (stdlib/jolt-core — embedded in the binary) are
# NEVER cached. Run in SOURCE mode (chez --script cli.ss) so clojure.set actually
# loads on demand (the devcache preloads it); ldr-install-file? must bypass it.
chez_bin="$(command -v chez || command -v scheme || command -v chezscheme)"
n_i="(not cached)"
if [ -n "$chez_bin" ]; then
  cache_i="$(mktemp -d)"
  JOLT_AOT_CACHE=1 JOLT_CACHE_DIR="$cache_i" JOLT_VERSION=dev "$chez_bin" --script host/chez/cli.ss \
    -e "(require 'clojure.set) (println (clojure.set/union #{1 2} #{2 3}))" >/dev/null 2>&1
  n_i="$(find "$cache_i" -name '*.so' 2>/dev/null | wc -l | tr -d ' ')"
  rm -rf "$cache_i"
fi
if [ "$n_i" = "0" ]; then
  echo "PASS: (i) install-owned clojure.set produced 0 cache files"; pass=$((pass+1))
else
  echo "FAIL: (i) install-owned clojure.set produced $n_i cache files (expected 0)"; fails=$((fails+1))
fi

# --- (j) corrupt cache file is recovered, not fatal --------------------------
# A truncated/garbage .so (killed process, concurrent mid-write) must fall back
# to recompile and still produce correct output — not crash the program. Populate
# the cache with a good entry, then overwrite the .so with garbage and run.
j="$tmp/j"; mkdir -p "$j/src/mylib"
printf '(ns mylib.core)\n(defn answer [] 42)\n' > "$j/src/mylib/core.clj"
jrun() {
  JOLT_AOT_CACHE=1 JOLT_CACHE_DIR="$cache" JOLT_QUIET=1 "$joltc" -e "
    (require 'jolt.deps) (jolt.deps/add-deps {:deps {'mylib/mylib {:local/root \"$j\"}}})
    (require 'mylib.core) (println (mylib.core/answer))" 2>/dev/null | tail -1
}
# cold: populate
jrun >/dev/null 2>&1
# corrupt every cached .so
find "$cache" -name '*.so' -exec sh -c 'printf "GARBAGE-NOT-FASL" > "$1"' sh {} \;
jout="$(jrun)"
jso_after="$(count_cache_files)"
if [ "$jout" = "42" ] && [ "$jso_after" -ge 1 ]; then
  echo "PASS: (j) corrupt cache recovered, output=42, rebuilt .so"; pass=$((pass+1))
else
  echo "FAIL: (j) corrupt cache: output='$jout' .so-after=$jso_after (expected 42, >=1 rebuilt)"; fails=$((fails+1))
fi

# Phase 4 (cold-vs-warm speedup) lives in aot-cache-perf.sh — a timing
# measurement doesn't belong in this deterministic correctness gate.

echo ""
echo "aot-cache smoke: $pass passed, $fails failed"
rm -rf "$cache" "$tmp"
[ "$fails" -eq 0 ]
