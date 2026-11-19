#!/bin/sh
# footprint-test.sh — what a jolt process holds beyond its data. Invoke from the
# repo root with the binary to check (default target/release/jolt):
#   sh test/chez/footprint-test.sh [jolt]
#
# The rows, each a leak of memory that was measured, not modeled:
#   - pinned chunks: a vfasl boot loads straight into the static generation
#     while its temporary buffers (the decompressed image, the relocations) live
#     in chunks of their own. A stock kernel hands such a chunk's spare aligned
#     segment to the next allocation, a static one there pins the whole chunk,
#     and jolt held 76MB of empty chunks for its whole life
#     (host/chez/chez-patches/0002-oversize-chunk-spare-segment.patch). On a
#     kernel without jolt's patches the row reports and passes; on a patched one
#     (the lockfree entry is the marker, both ship together) it must be clean.
#   - const boot arrays: the boot image and the other embedded files are read
#     only, and the launcher madvises the boot. Over writable data that makes
#     every page a private dirty copy on macOS (14MB per process); const puts
#     them in a read-only section, clean and shared.
#   - the GC policy's live baseline: it was read from bytes-allocated before any
#     collection, startup garbage and all (155MB against 62MB live), and sized
#     the nursery and the old-generation growth limit from that. The first
#     collection must report a baseline no bigger than the heap after it.
set -eu

J=${1:-target/release/jolt}
case "$J" in /*) ;; *) J="$(cd "$(dirname "$J")" && pwd)/$(basename "$J")" ;; esac
CHEZ=${2:-${CHEZ:-}}
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
fail=0

# --- pinned chunks ------------------------------------------------------------
cat > "$tmp/probe.clj" <<EOF
(System/gc)
(println (jolt.host/scheme-eval-string
  "(begin (define footprint-report \"$tmp/alloc.txt\")
          (load \"test/chez/footprint-chunks.ss\")
          (foreign-entry? \"(cs)lockfree_activation\"))"))
EOF
out=$(JOLT_NO_USER_DEPS=1 "$J" "$tmp/probe.clj")
line=$(echo "$out" | grep '^PINNED ' || true)
patched=$(echo "$out" | tail -n 1)
[ -n "$line" ] || { echo "FAIL: footprint probe printed no PINNED line:" >&2; echo "$out" >&2; exit 1; }
echo "footprint: $line (patched kernel: $patched)"
pinned=$(echo "$line" | awk '{print $2}')
if [ "$pinned" != 0 ]; then
  if [ "$patched" = true ] || [ "${JOLT_REQUIRE_LOCKFREE_CHEZ:-}" = 1 ]; then
    echo "FAIL: $pinned near-empty oversize chunk(s) pinned after a full collection" >&2
    fail=1
  else
    echo "footprint: stock Chez kernel, pinned chunks not checked"
  fi
fi

# --- const boot arrays --------------------------------------------------------
# Every embedded array (jolt_boot, jolt_petite_boot, ...; not the _len words)
# must sit in a read-only section: nm's D/d is writable data.
if command -v nm >/dev/null 2>&1; then
  writable=$(nm "$J" 2>/dev/null | awk '$3 ~ /^_?jolt_[A-Za-z0-9_]+$/ && $3 !~ /_len$/ && ($2 == "D" || $2 == "d") {print $3}')
  if [ -n "$writable" ]; then
    echo "FAIL: embedded arrays in writable data (dirtied by the boot prefetch):" >&2
    echo "$writable" >&2
    fail=1
  fi
  arrays=$(nm "$J" 2>/dev/null | awk '$3 ~ /^_?jolt_boot$/' | wc -l | tr -d ' ')
  [ "$arrays" -ge 1 ] || { echo "FAIL: no jolt_boot symbol in $J (nm row checks nothing)" >&2; fail=1; }
else
  echo "footprint: no nm, const-array row skipped"
fi

# --- GC live baseline ---------------------------------------------------------
first=$(JOLT_NO_USER_DEPS=1 JOLT_GC_LOG=1 "$J" -e '(println (reduce + (map count (repeatedly 400000 #(vector 1 2 3)))))' 2>&1 | grep '^gc: ' | head -n 1)
[ -n "$first" ] || { echo "FAIL: no collection logged by the GC baseline row" >&2; exit 1; }
heap=$(echo "$first" | sed -n 's/.* heap \([0-9]*\)MB .*/\1/p')
base=$(echo "$first" | sed -n 's/.* live-after-full \([0-9]*\)MB .*/\1/p')
if [ -z "$heap" ] || [ -z "$base" ]; then
  echo "FAIL: cannot read heap/live-after-full from: $first" >&2; exit 1
fi
if [ "$base" -gt $((heap + 1)) ]; then
  echo "FAIL: GC baseline ${base}MB exceeds the heap after the first collection (${heap}MB)" >&2
  fail=1
fi

# --- System/gc under a busy thread ----------------------------------------------
# A full collection is refused while another thread is active, and System/gc
# used to retry briefly and then do nothing, so with a compute loop running it
# was a no-op (and so was the collection the startup ceiling check depends on).
# It joins the collector's rendezvous now; a weakly held object must clear.
cleared=$(JOLT_NO_USER_DEPS=1 "$J" test/chez/gc-under-threads.clj 2>&1 | tail -n 1)
if [ "$cleared" != "cleared true" ]; then
  echo "FAIL: System/gc with a busy thread did not collect ($cleared)" >&2
  fail=1
fi

# The same early reading refused a ceiling well above the live heap: 150m read
# as under the runtime's 163MB (62MB live), and so would the default ceiling in
# a 512MB container (25% of it).
if ! small=$(JOLT_NO_USER_DEPS=1 JOLT_MAX_HEAP=150m "$J" -e '(println :ok)' 2>&1) || [ "$small" != ":ok" ]; then
  echo "FAIL: JOLT_MAX_HEAP=150m refused at startup:" >&2
  echo "$small" >&2
  fail=1
fi

# --- the boot's transient peak --------------------------------------------------
# A vfasl boot is decompressed into one buffer per image segment, and the buffer
# lives beside the static image it is building. One segment for the whole
# runtime put jolt's peak at 2.2x its live heap; segmented (build.ss
# bld-emit-runtime), the buffer is one segment's worth. In the heap, each
# segment's buffer left holes in the static image around it that no later one
# fit, so on a stock kernel the boot keeps several chunks of empty segments;
# jolt's kernel keeps the buffer outside the heap (chez-patches/0003). The peak
# over the live heap after boot must stay under half of it.
peak=$(echo "$line" | awk '{print $9}')
live=$(echo "$line" | awk '{print $7}')
if [ $(( (peak - live) * 2 )) -gt "$live" ]; then
  echo "FAIL: jolt's boot peaked at $((peak / 1048576))MB for $((live / 1048576))MB live (transient over half the live heap)" >&2
  fail=1
fi

# --- closed-world apps ------------------------------------------------------------
# What --closed-world ships. The shake keeps an embedded stdlib namespace's
# top-level forms only when the namespace is live (required, or one of its defs
# reached), as loading it would run them; a form whose only effect is one var's
# metadata or declaration goes with that var; and a binary with no compiler
# carries no compile-time information (macro transformers and expander data).
app=$tmp/app
mkdir -p "$app/src/fp"
echo '{:paths ["src"]}' > "$app/deps.edn"
cat > "$app/src/fp/hello.clj" <<'EOF'
(ns fp.hello)
(defn -main [& _]
  (println "hello" (count (range 1000)))
  (System/gc)
  (let [rt (Runtime/getRuntime)]
    (println "boot" (jolt.host/maximum-memory-bytes) (- (.totalMemory rt) (.freeMemory rt)))))
EOF
cat > "$app/src/fp/strs.clj" <<'EOF'
(ns fp.strs (:require [clojure.string :as s]))
(defn -main [& _]
  (println (s/upper-case "abc"))
  (clojure.pprint/pprint {:a [1 2 3]}))
EOF
cat > "$app/src/fp/loads.clj" <<'EOF'
(ns fp.loads (:require [clojure.pprint]))
(defn -main [& _] (println "loaded"))
EOF
build_app() { (cd "$app" && JOLT_NO_USER_DEPS=1 "$J" build -m "fp.$1" -o "$1" --closed-world >"$1.log" 2>&1) ||
  { echo "FAIL: closed-world build of fp.$1 failed:" >&2; tail -5 "$app/$1.log" >&2; fail=1; return 1; }; }

if build_app hello; then
  out=$("$app/hello")
  echo "$out" | grep -q '^hello 1000$' || { echo "FAIL: closed-world hello printed: $out" >&2; fail=1; }
  apeak=$(echo "$out" | awk '/^boot /{print $2}'); alive=$(echo "$out" | awk '/^boot /{print $3}')
  echo "footprint: closed-world hello boot peak $((apeak / 1048576))MB, live $((alive / 1048576))MB"
  if [ $(( (apeak - alive) * 2 )) -gt "$alive" ]; then
    echo "FAIL: the closed-world app's boot transient is over half its live heap" >&2; fail=1
  fi
  rt=$app/hello.build/runtime.ss
  if grep -q '(def-var[a-z!-]* "clojure.pprint"' "$rt"; then
    echo "FAIL: a hello app that never loads clojure.pprint ships its defs" >&2; fail=1
  fi
  # a var's doc form: (jv$clojure.core/attach-core-doc-meta! "ns" "name" ...), as
  # minted through a linked-call local (backend calls.clj linked-call):
  # (let ((_lp$N jv$...attach-core-doc-meta!) (_la$M "ns") (_la$K "name") ...
  doc_form() { grep -Eq "attach-core-doc-meta!\)? (\(_la\\\$[0-9]+ )?\"clojure.core\"\)? (\(_la\\\$[0-9]+ )?\"$1\"" "$rt"; }
  if doc_form frequencies; then
    echo "FAIL: doc metadata shipped for a var the shake dropped (frequencies)" >&2; fail=1
  fi
  doc_form println ||
    { echo "FAIL: doc metadata missing for a var the app keeps (println)" >&2; fail=1; }
  # stripped already: stripping again changes nothing
  # the runtime fasl the boot was made from: the stripped copy when there is one
  so=$app/hello.build/runtime-stripped.so
  [ -f "$so" ] || so=$app/hello.build/runtime.so
  if [ -n "${CHEZ:-}" ] && command -v "$CHEZ" >/dev/null 2>&1; then
    echo "(strip-fasl-file \"$so\" \"$tmp/again.so\" (fasl-strip-options compile-time-information inspector-source source-annotations profile-source))" | "$CHEZ" -q
    if [ "$(wc -c < "$so")" != "$(wc -c < "$tmp/again.so")" ]; then
      echo "FAIL: the closed-world runtime ($so) still carries compile-time information" >&2; fail=1
    fi
  else
    echo "footprint: no CHEZ, compile-time-information row skipped"
  fi
fi
if build_app strs; then
  out=$("$app/strs")
  [ "$out" = "$(printf 'ABC\n{:a [1 2 3]}')" ] || { echo "FAIL: closed-world clojure.string/pprint app printed: $out" >&2; fail=1; }
fi
if build_app loads; then
  [ "$("$app/loads")" = "loaded" ] || { echo "FAIL: closed-world app requiring clojure.pprint did not run" >&2; fail=1; }
  grep -q '(def-var[a-z!-]* "clojure.pprint"' "$app/loads.build/runtime.ss" ||
    { echo "FAIL: a required clojure.pprint was shaken out (its load must run)" >&2; fail=1; }
fi

[ "$fail" = 0 ] || exit 1
echo "footprint: passed"
