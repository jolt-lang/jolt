#!/bin/sh
# aot-cache smoke: verify the per-namespace AOT/compile cache.
#
# The cache fasls a required namespace's emitted Scheme on first load (miss)
# and loads the .so on subsequent loads (hit), keyed by source content hash +
# jolt version. This script drives the fast dev bin/jolt (devcache mode loads
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

jolt="bin/jolt"
cache="$(mktemp -d)"
tmp="$(mktemp -d)"
mkdir -p "$tmp/src/mylib"

# A program that requires a disk-backed ns via add-deps (the real require path)
# and prints a value computed in it. \$1 = the temp project dir.
run_prog() {
  JOLT_AOT_CACHE=1 JOLT_CACHE_DIR="$cache" JOLT_QUIET=1 "$jolt" -e "
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

# --- (c2) same-length edit on a realistically-sized source --------------------
# equal-hash samples at most ~26 bytes regardless of length, so a same-length
# edit away from those sampling points produces a collision and serves stale
# code. The fixture is ~4 KB with the value in the middle — far from the small
# test above where the edit happens to land in a sampled byte. 42 -> 99.
c2="$tmp/c2"; mkdir -p "$c2/src/mylib"
# Regenerated rather than sed-edited: `sed -i ''` is BSD-only (GNU sed reads the
# '' as the script and the s/// as a filename, exits 2, and set -e kills the run),
# and both values are two digits so rewriting the file is length-preserving anyway.
gen_c2() {
  {
    # pad: ~2 KB of comment lines above the value
    i=0; while [ "$i" -lt 32 ]; do
      printf ';; padding line %s for cache key size ---------------------------------------\n' "$i"
      i=$((i + 1))
    done
    echo '(ns mylib.core)'
    printf '(defn answer [] %s)\n' "$1"
    # pad: ~2 KB of comment lines below the value
    i=0; while [ "$i" -lt 32 ]; do
      printf ';; padding line %s for cache key size ---------------------------------------\n' "$i"
      i=$((i + 1))
    done
  } > "$c2/src/mylib/core.clj"
}
gen_c2 42
c2_len=$(wc -c < "$c2/src/mylib/core.clj")
if [ "$c2_len" -lt 4096 ]; then
  echo "FAIL: (c2) fixture too small: $c2_len bytes (need >=4096)"; fails=$((fails+1))
else
  sleep 1  # ensure mtime advances
  c2_cold="$(run_prog "$c2")"
  if [ "$c2_cold" != "42" ]; then
    echo "FAIL: (c2) cold output='$c2_cold' (expected 42)"; fails=$((fails+1))
  else
    # length-preserving edit: 42 -> 99
    c2_orig_len=$(wc -c < "$c2/src/mylib/core.clj")
    gen_c2 99
    c2_new_len=$(wc -c < "$c2/src/mylib/core.clj")
    if [ "$c2_orig_len" != "$c2_new_len" ]; then
      echo "FAIL: (c2) edit changed length: $c2_orig_len -> $c2_new_len (expected equal)"; fails=$((fails+1))
    else
      sleep 1
      c2_edit="$(run_prog "$c2")"
      if [ "$c2_edit" = "99" ]; then
        echo "PASS: (c2) large-fixture edit invalidated, output=99"; pass=$((pass+1))
      else
        echo "FAIL: (c2) after edit output='$c2_edit' (expected 99 — equal-hash sampled stale)"; fails=$((fails+1))
      fi
    fi
  fi
fi

# --- Phase 2: correctness the tee must preserve (cold == warm == expected) ----
# case_cold_warm <label> <projdir> <expr-after-add-deps> <expected>
# projdir has src/proj/core.clj (+ siblings); expr requires proj.core and prints
# (proj.core/run). Asserts cold and warm runs both print `expected`.
case_cold_warm() {
  clabel="$1"; cproj="$2"; cexpr="$3"; cexp="$4"
  cmd="(require 'jolt.deps) (jolt.deps/add-deps {:deps {'proj/proj {:local/root \"$cproj\"}}}) $cexpr"
  ccold="$(JOLT_AOT_CACHE=1 JOLT_CACHE_DIR="$cache" JOLT_QUIET=1 "$jolt" -e "$cmd" 2>/dev/null | tail -1)"
  cwarm="$(JOLT_AOT_CACHE=1 JOLT_CACHE_DIR="$cache" JOLT_QUIET=1 "$jolt" -e "$cmd" 2>/dev/null | tail -1)"
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
JOLT_AOT_CACHE=1 JOLT_CACHE_DIR="$cache" JOLT_QUIET=1 "$jolt" -e "$hcmd (require 'proj.core) (println (proj.core/answer))" >/dev/null 2>&1
# warm (no reload): cache hit → still 42
warm1="$(JOLT_AOT_CACHE=1 JOLT_CACHE_DIR="$cache" JOLT_QUIET=1 "$jolt" -e "$hcmd (require 'proj.core) (println (proj.core/answer))" 2>/dev/null | tail -1)"
# edit, then :reload — must show the edit despite the stale v1 .so in the cache
sleep 1
printf '(ns proj.core)\n(defn answer [] 99)\n' > "$h/src/proj/core.clj"
reload_out="$(JOLT_AOT_CACHE=1 JOLT_CACHE_DIR="$cache" JOLT_QUIET=1 "$jolt" -e "$hcmd (require 'proj.core :reload) (println (proj.core/answer))" 2>/dev/null | tail -1)"
if [ "$warm1" = "42" ] && [ "$reload_out" = "99" ]; then
  echo "PASS: (h) warm=$warm1, :reload-after-edit=$reload_out"; pass=$((pass+1))
else
  echo "FAIL: (h) warm='$warm1' (want 42), :reload-after-edit='$reload_out' (want 99)"; fails=$((fails+1))
fi

# (i) install-owned namespaces (stdlib/jolt-core — embedded in the binary) are
# NEVER cached. Run in SOURCE mode (chez --script cli.ss) so clojure.set actually
# loads on demand (the devcache preloads it); ldr-install-file? must bypass it.
# JOLT_CHEZ wins (see host/chez/selfcheck.sh) — otherwise this runs (i) under
# whatever Chez happens to be on PATH, independent of the one the rest of this
# gate actually built with.
chez_bin="${JOLT_CHEZ:-$(command -v chez || command -v scheme || command -v chezscheme)}"
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
  JOLT_AOT_CACHE=1 JOLT_CACHE_DIR="$cache" JOLT_QUIET=1 "$jolt" -e "
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

# --- (k) two runtimes reporting one version don't share a cache namespace ----
# The fasl a namespace compiles to is only valid for the runtime that emitted it,
# and the version string does not pin one: `git describe` reports the same
# "…-dirty" for every edit in a working tree, so a rebuilt jolt used to load its
# predecessor's output. Drive the SAME namespace through two genuinely different
# runtimes — the source tree and the built binary — with the version forced equal,
# and require them to land in separate cache namespaces.
k="$tmp/k"; mkdir -p "$k/src/mylib"
printf '(ns mylib.core)\n(defn answer [] 42)\n' > "$k/src/mylib/core.clj"
cache_k="$(mktemp -d)"
kprog="(require 'jolt.deps)
       (jolt.deps/add-deps {:deps {'mylib/mylib {:local/root \"$k\"}}})
       (require 'mylib.core) (println (mylib.core/answer))"
kbin="target/release/jolt"
n_k="(skipped)"; k_out_src=""; k_out_bin=""
if [ -n "$chez_bin" ] && [ -x "$kbin" ]; then
  # the binary bakes its version, so read it back and hand it to the source run
  kver="$("$kbin" --version 2>/dev/null | sed 's/^jolt //')"
  k_out_src="$(JOLT_AOT_CACHE=1 JOLT_CACHE_DIR="$cache_k" JOLT_VERSION="$kver" JOLT_QUIET=1 \
    "$chez_bin" --script host/chez/cli.ss -e "$kprog" 2>/dev/null | tail -1)"
  k_out_bin="$(JOLT_AOT_CACHE=1 JOLT_CACHE_DIR="$cache_k" JOLT_QUIET=1 \
    "$kbin" -e "$kprog" 2>/dev/null | tail -1)"
  n_k="$(find "$cache_k" -mindepth 1 -maxdepth 1 -type d 2>/dev/null | wc -l | tr -d ' ')"
fi
rm -rf "$cache_k"
if [ "$n_k" = "(skipped)" ]; then
  echo "SKIP: (k) needs chez + $kbin (make testbin)"
elif [ "$n_k" = "2" ] && [ "$k_out_src" = "42" ] && [ "$k_out_bin" = "42" ]; then
  echo "PASS: (k) source and binary runtimes keyed separately under one version"; pass=$((pass+1))
else
  echo "FAIL: (k) cache namespaces=$n_k (expected 2), source='$k_out_src' binary='$k_out_bin' (expected 42)"; fails=$((fails+1))
fi

# --- (l) editing a REQUIRED namespace invalidates its consumers ---------------
# A namespace's fasl bakes in whatever its dependencies contributed at compile
# time — macro expansions above all — so keying only on its own source leaves it
# serving expansions from a macro definition that no longer exists. The consumer
# is untouched here; only the macro namespace changes.
l="$tmp/l"; mkdir -p "$l/src/dep"
printf '(ns dep.macros)\n(defmacro tag [] "v1")\n' > "$l/src/dep/macros.clj"
printf '(ns dep.core (:require [dep.macros :as m]))\n(defn answer [] (m/tag))\n' > "$l/src/dep/core.clj"
lrun() {
  JOLT_AOT_CACHE=1 JOLT_CACHE_DIR="$cache" JOLT_QUIET=1 "$jolt" -e "
    (require 'jolt.deps) (jolt.deps/add-deps {:deps {'dep/dep {:local/root \"$l\"}}})
    (require 'dep.core) (println (dep.core/answer))" 2>/dev/null | tail -1
}
l_cold="$(lrun)"
sleep 1
printf '(ns dep.macros)\n(defmacro tag [] "v2")\n' > "$l/src/dep/macros.clj"
l_warm="$(lrun)"
if [ "$l_cold" = "v1" ] && [ "$l_warm" = "v2" ]; then
  echo "PASS: (l) macro-ns edit invalidated its consumer (v1 -> v2)"; pass=$((pass+1))
else
  echo "FAIL: (l) cold='$l_cold' (want v1) after-macro-edit='$l_warm' (want v2)"; fails=$((fails+1))
fi

# --- (m) invalidation reaches through a chain, not just direct requires -------
# top -> mid -> low, with the macro at the bottom: top's key has to fold in the
# whole transitive closure, since mid contributes low's expansion to it.
m="$tmp/m"; mkdir -p "$m/src/chain"
printf '(ns chain.low)\n(defmacro tag [] "v1")\n' > "$m/src/chain/low.clj"
printf '(ns chain.mid (:require [chain.low :as l]))\n(defn mid-answer [] (l/tag))\n' > "$m/src/chain/mid.clj"
printf '(ns chain.top (:require [chain.mid :as mid]))\n(defn answer [] (mid/mid-answer))\n' > "$m/src/chain/top.clj"
mrun() {
  JOLT_AOT_CACHE=1 JOLT_CACHE_DIR="$cache" JOLT_QUIET=1 "$jolt" -e "
    (require 'jolt.deps) (jolt.deps/add-deps {:deps {'chain/chain {:local/root \"$m\"}}})
    (require 'chain.top) (println (chain.top/answer))" 2>/dev/null | tail -1
}
m_cold="$(mrun)"
sleep 1
printf '(ns chain.low)\n(defmacro tag [] "v2")\n' > "$m/src/chain/low.clj"
m_warm="$(mrun)"
if [ "$m_cold" = "v1" ] && [ "$m_warm" = "v2" ]; then
  echo "PASS: (m) transitive dep edit invalidated the chain (v1 -> v2)"; pass=$((pass+1))
else
  echo "FAIL: (m) cold='$m_cold' (want v1) after-transitive-edit='$m_warm' (want v2)"; fails=$((fails+1))
fi

# --- (n) superseded runtime generations are pruned ---------------------------
# The cache namespace moves whenever the runtime does, so a dev loop that
# rebuilds jolt leaves a full generation behind per build. Plant stale
# generations with old markers and require the run to collect them, keeping the
# few most recently used (the current one always among them).
cache_n="$(mktemp -d)"
i=1
while [ "$i" -le 6 ]; do
  mkdir -p "$cache_n/stale-gen-$i/v1"
  : > "$cache_n/stale-gen-$i/.used"
  touch -t "2020010100$(printf '%02d' "$i")" "$cache_n/stale-gen-$i/.used"
  i=$((i+1))
done
n_before="$(find "$cache_n" -mindepth 1 -maxdepth 1 -type d | wc -l | tr -d ' ')"
out_n="$(JOLT_AOT_CACHE=1 JOLT_CACHE_DIR="$cache_n" JOLT_QUIET=1 "$jolt" -e "
  (require 'jolt.deps) (jolt.deps/add-deps {:deps {'mylib/mylib {:local/root \"$k\"}}})
  (require 'mylib.core) (println (mylib.core/answer))" 2>/dev/null | tail -1)"
n_after="$(find "$cache_n" -mindepth 1 -maxdepth 1 -type d | wc -l | tr -d ' ')"
n_live="$(find "$cache_n" -name '*.so' | wc -l | tr -d ' ')"
rm -rf "$cache_n"
if [ "$n_before" = "6" ] && [ "$n_after" -le 3 ] && [ "$n_live" -ge 1 ] && [ "$out_n" = "42" ]; then
  echo "PASS: (n) pruned $n_before generations to $n_after, current one live"; pass=$((pass+1))
else
  echo "FAIL: (n) generations $n_before -> $n_after (want <=3), live .so=$n_live, output='$out_n'"; fails=$((fails+1))
fi

# --- (o) an artifact holds its OWN namespace, not the ones it requires --------
# The capture teed every form compiled while a namespace loaded. A cacheable
# require redirected it (the nested aot-capture-load opens its own port), but an
# install-owned one bypasses the cache entirely and so never did — the requiring
# namespace's artifact ended up carrying a whole second copy of the stdlib
# namespace, replayed AFTER the require that already loaded it properly.
#
# That is only bloat until a sibling require layers something on top of what the
# stdlib namespace registered: the baked copy replays last and undoes it. Which is
# what jolt.time did — its 14-line ns form cached as 412 KB of eight jolt/time/*
# namespaces, and on a hit jolt.time.local's ISO-only java.time.LocalDate/parse
# landed back on top of jolt.time.fmt's pattern-aware override, so a warm cache
# silently ignored a DateTimeFormatter. Same shape here with clojure.zip:
# top requires the install-owned namespace and then the sibling that overrides it.
o="$tmp/o"; mkdir -p "$o/src/olib"
printf '{}' > "$o/deps.edn"
cat > "$o/src/olib/over.clj" <<'CLJ'
(ns olib.over (:require [clojure.zip :as z]))
(alter-var-root #'clojure.zip/root (constantly (fn [_] :overridden)))
(defn probe [] (z/root nil))
CLJ
cat > "$o/src/olib/top.clj" <<'CLJ'
(ns olib.top (:require [clojure.zip] [olib.over]))
(defn probe [] (olib.over/probe))
CLJ
cache_o="$(mktemp -d)"
run_o() {
  JOLT_AOT_CACHE=1 JOLT_CACHE_DIR="$cache_o" JOLT_QUIET=1 "$jolt" -e "
    (require 'jolt.deps) (jolt.deps/add-deps {:deps {'olib/olib {:local/root \"$o\"}}})
    (require 'olib.top) (println (olib.top/probe))" 2>/dev/null | tail -1
}
o_cold="$(run_o)"
o_warm="$(run_o)"
o_scm="$(find "$cache_o" -name 'olib.top-*.scm' | head -1)"
# Each in-ns reference followed by the namespace symbol it interns. Matched
# through a window rather than adjacently, because the two are no longer
# neighbours in the emitted text: a top-level var reference now resolves through
# a per-form cache cell (jolt-g3u), so `in-ns` and its argument are separated by
# the cell's (or … (set! …)) . The window is what this check actually wants —
# which namespaces the artifact interns — independent of how a var ref is spelled.
o_defines="$(grep -o 'in-ns.\{0,200\}' "$o_scm" 2>/dev/null \
             | sed -n 's/.*jolt-symbol #f "\([^"]*\)".*/\1/p' | sort -u | tr '\n' ' ' | sed 's/ $//')"
rm -rf "$cache_o"
if [ "$o_cold" = ":overridden" ] && [ "$o_warm" = ":overridden" ] && [ "$o_defines" = "olib.top" ]; then
  echo "PASS: (o) artifact defines only olib.top; override survives the hit"; pass=$((pass+1))
else
  echo "FAIL: (o) cold='$o_cold' warm='$o_warm' (both want :overridden), artifact defines '$o_defines' (want 'olib.top')"
  fails=$((fails+1))
fi

# --- Phase 5: files read at compile time invalidate their reader --------------
# A macro that slurps an external file bakes that file's CONTENTS into the
# artifact, exactly the way it bakes a macro expansion. The key folded in the
# source and the required namespaces but nothing the compile read, so editing an
# embedded SQL migration (and nothing else) left every consumer serving the old
# statements out of the cache — silently, since the .clj is untouched and its
# hash still matches (jolt#576).

# (p) slurp of a path at macro-expansion time
p="$tmp/p"; mkdir -p "$p/src/proj"
printf 'v1' > "$p/mig.sql"
cat > "$p/src/proj/core.clj" <<CLJ
(ns proj.core)
(defmacro embed [] (slurp "$p/mig.sql"))
(defn run [] (embed))
CLJ
prun() {
  JOLT_AOT_CACHE=1 JOLT_CACHE_DIR="$cache" JOLT_QUIET=1 "$jolt" -e "
    (require 'jolt.deps) (jolt.deps/add-deps {:deps {'proj/proj {:local/root \"$p\"}}})
    (require 'proj.core) (println (proj.core/run))" 2>/dev/null | tail -1
}
p_cold="$(prun)"
printf 'v2' > "$p/mig.sql"
p_warm="$(prun)"
if [ "$p_cold" = "v1" ] && [ "$p_warm" = "v2" ]; then
  echo "PASS: (p) slurped file edit invalidated its reader (v1 -> v2)"; pass=$((pass+1))
else
  echo "FAIL: (p) cold='$p_cold' (want v1) after-resource-edit='$p_warm' (want v2)"; fails=$((fails+1))
fi

# (q) io/resource — the classpath spelling, resolved against the source roots
q="$tmp/q"; mkdir -p "$q/src/proj" "$q/resources"
printf '{:paths ["src" "resources"]}' > "$q/deps.edn"
printf 'r1' > "$q/resources/mig.sql"
cat > "$q/src/proj/core.clj" <<'CLJ'
(ns proj.core (:require [clojure.java.io :as io]))
(defmacro embed [] (slurp (io/resource "mig.sql")))
(defn run [] (embed))
CLJ
qrun() {
  JOLT_AOT_CACHE=1 JOLT_CACHE_DIR="$cache" JOLT_QUIET=1 "$jolt" -e "
    (require 'jolt.deps) (jolt.deps/add-deps {:deps {'proj/proj {:local/root \"$q\"}}})
    (require 'proj.core) (println (proj.core/run))" 2>/dev/null | tail -1
}
q_cold="$(qrun)"
printf 'r2' > "$q/resources/mig.sql"
q_warm="$(qrun)"
if [ "$q_cold" = "r1" ] && [ "$q_warm" = "r2" ]; then
  echo "PASS: (q) io/resource edit invalidated its reader (r1 -> r2)"; pass=$((pass+1))
else
  echo "FAIL: (q) cold='$q_cold' (want r1) after-resource-edit='$q_warm' (want r2)"; fails=$((fails+1))
fi

# (q2) a resource that did not exist yet — adding one (a new migration) has to
# invalidate the namespace that probed for it and found nothing.
printf '(ns proj.opt (:require [clojure.java.io :as io]))\n(defmacro embed [] (if-let [u (io/resource "opt.sql")] (slurp u) "none"))\n(defn run [] (embed))\n' > "$q/src/proj/opt.clj"
q2run() {
  JOLT_AOT_CACHE=1 JOLT_CACHE_DIR="$cache" JOLT_QUIET=1 "$jolt" -e "
    (require 'jolt.deps) (jolt.deps/add-deps {:deps {'proj/proj {:local/root \"$q\"}}})
    (require 'proj.opt) (println (proj.opt/run))" 2>/dev/null | tail -1
}
q2_cold="$(q2run)"
printf 'appeared' > "$q/resources/opt.sql"
q2_warm="$(q2run)"
if [ "$q2_cold" = "none" ] && [ "$q2_warm" = "appeared" ]; then
  echo "PASS: (q2) an added resource invalidated the ns that probed for it"; pass=$((pass+1))
else
  echo "FAIL: (q2) cold='$q2_cold' (want none) after-add='$q2_warm' (want appeared)"; fails=$((fails+1))
fi

# (r) the read happens while compiling the CONSUMER (the macro lives one
# namespace away), so the resource belongs to the consumer's key, and editing it
# has to move that key even though neither .clj changed.
r="$tmp/r"; mkdir -p "$r/src/chain"
printf 'w1' > "$r/mig.sql"
cat > "$r/src/chain/mac.clj" <<CLJ
(ns chain.mac)
(defmacro embed [] (slurp "$r/mig.sql"))
CLJ
cat > "$r/src/chain/top.clj" <<'CLJ'
(ns chain.top (:require [chain.mac :as m]))
(defn run [] (m/embed))
CLJ
rrun() {
  JOLT_AOT_CACHE=1 JOLT_CACHE_DIR="$cache" JOLT_QUIET=1 "$jolt" -e "
    (require 'jolt.deps) (jolt.deps/add-deps {:deps {'chain/chain {:local/root \"$r\"}}})
    (require 'chain.top) (println (chain.top/run))" 2>/dev/null | tail -1
}
r_cold="$(rrun)"
printf 'w2' > "$r/mig.sql"
r_warm="$(rrun)"
if [ "$r_cold" = "w1" ] && [ "$r_warm" = "w2" ]; then
  echo "PASS: (r) cross-namespace macro read invalidated the consumer (w1 -> w2)"; pass=$((pass+1))
else
  echo "FAIL: (r) cold='$r_cold' (want w1) after-resource-edit='$r_warm' (want w2)"; fails=$((fails+1))
fi

# (s) a resource baked into a REQUIRED namespace's own artifact propagates
# through the chain the way an edit to that namespace's source does: deep.low
# expands the slurp into its OWN .so, and deep.top's key has to fold that in.
s="$tmp/s"; mkdir -p "$s/src/deep"
printf 'd1' > "$s/mig.sql"
cat > "$s/src/deep/low.clj" <<CLJ
(ns deep.low)
(defmacro mig [] (slurp "$s/mig.sql"))
(def payload (mig))
CLJ
cat > "$s/src/deep/top.clj" <<'CLJ'
(ns deep.top (:require [deep.low :as l]))
(defn run [] l/payload)
CLJ
srun() {
  JOLT_AOT_CACHE=1 JOLT_CACHE_DIR="$cache" JOLT_QUIET=1 "$jolt" -e "
    (require 'jolt.deps) (jolt.deps/add-deps {:deps {'deep/deep {:local/root \"$s\"}}})
    (require 'deep.top) (println (deep.top/run))" 2>/dev/null | tail -1
}
s_cold="$(srun)"
printf 'd2' > "$s/mig.sql"
s_warm="$(srun)"
if [ "$s_cold" = "d1" ] && [ "$s_warm" = "d2" ]; then
  echo "PASS: (s) a required ns's resource edit reached the chain (d1 -> d2)"; pass=$((pass+1))
else
  echo "FAIL: (s) cold='$s_cold' (want d1) after-resource-edit='$s_warm' (want d2)"; fails=$((fails+1))
fi

# (t) a namespace that reads nothing writes no resource sidecar — the record is
# per-namespace, not a global list every artifact then keys on.
t_res="$(find "$cache" -name 'mylib.core-*.res' 2>/dev/null | wc -l | tr -d ' ')"
p_res="$(find "$cache" -name 'proj.core-*.res' 2>/dev/null | wc -l | tr -d ' ')"
if [ "$t_res" = "0" ] && [ "$p_res" -ge 1 ]; then
  echo "PASS: (t) resource sidecars are per-namespace ($p_res for readers, 0 for mylib.core)"; pass=$((pass+1))
else
  echo "FAIL: (t) mylib.core sidecars=$t_res (want 0), proj.core sidecars=$p_res (want >=1)"; fails=$((fails+1))
fi

# --- (u) a fasl cut at a form boundary is detected, not served ----------------
# `load` of a .so truncated exactly at a compiled-form boundary SUCCEEDS — a
# Chez fasl is a sequence of objects — so the raise-based guard in (j) never
# fires and a partial namespace would be served on every run while a plain
# source load works. Every published .scm ends with a completion marker the
# loader checks after a hit-load. Simulate the partial artifact by stripping
# the marker from the cached .scm, recompiling that with the same Chez, and
# planting the result as the .so.
u="$tmp/u"; mkdir -p "$u/src/ulib"
printf '(ns ulib.core)\n(defn answer [] 42)\n' > "$u/src/ulib/core.clj"
cache_u="$(mktemp -d)"
urun() {
  JOLT_DEBUG="${UDEBUG:-}" JOLT_AOT_CACHE=1 JOLT_CACHE_DIR="$cache_u" JOLT_QUIET=1 "$jolt" -e "
    (require 'jolt.deps) (jolt.deps/add-deps {:deps {'ulib/ulib {:local/root \"$u\"}}})
    (require 'ulib.core) (println (ulib.core/answer))" 2>&1
}
if [ -z "$chez_bin" ]; then
  echo "SKIP: (u) needs a chez on PATH to recompile the stripped .scm"
else
  urun >/dev/null 2>&1                                     # cold: populate
  u_scm="$(find "$cache_u" -name 'ulib.core-*.scm' | head -1)"
  u_so="${u_scm%.scm}.so"
  u_marked="$(tail -1 "$u_scm" | grep -c 'aot-mark-complete!')"
  u_warm="$(UDEBUG=1 urun)"                                # warm: real hit
  grep -v 'aot-mark-complete!' "$u_scm" > "$u_scm.cut"
  printf '(compile-file "%s" "%s")\n' "$u_scm.cut" "$u_so" | "$chez_bin" -q >/dev/null 2>&1
  u_cut="$(UDEBUG=1 urun)"                                 # partial artifact planted
  u_heal="$(UDEBUG=1 urun)"                                # must be a clean hit again
  if [ "$u_marked" = "1" ] \
     && echo "$u_warm" | grep -q '^42$' && echo "$u_warm" | grep -q 'hit ulib.core' \
     && ! echo "$u_warm" | grep -q 'incomplete cache' \
     && echo "$u_cut" | grep -q '^42$' && echo "$u_cut" | grep -q 'incomplete cache for ulib.core' \
     && echo "$u_heal" | grep -q '^42$' && echo "$u_heal" | grep -q 'hit ulib.core' \
     && ! echo "$u_heal" | grep -q 'incomplete cache'; then
    echo "PASS: (u) form-boundary-truncated fasl detected, recompiled, healed"; pass=$((pass+1))
  else
    echo "FAIL: (u) marker=$u_marked warm-hit=$(echo "$u_warm" | grep -c 'hit ulib.core') cut-detected=$(echo "$u_cut" | grep -c 'incomplete cache') cut-out=$(echo "$u_cut" | tail -1) heal-hit=$(echo "$u_heal" | grep -c 'hit ulib.core')"; fails=$((fails+1))
  fi
fi
rm -rf "$cache_u"

# Phase 4 (cold-vs-warm speedup) lives in aot-cache-perf.sh — a timing
# measurement doesn't belong in this deterministic correctness gate.

echo ""
echo "aot-cache smoke: $pass passed, $fails failed"
rm -rf "$cache" "$tmp"
[ "$fails" -eq 0 ]
