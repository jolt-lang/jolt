#!/bin/sh
# aot-cache perf: measure cold (recompile) vs warm (cache hit) wall-clock for a
# realistic multi-library require. Informational — prints cold/warm and the
# speedup; exits 0 unless warm is NOT faster than cold (a real regression).
#
# Not part of the default ci gate (it needs Maven jars locally and a timing
# budget). Run by hand or via: make aotcacheperf

root="$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)"
cd "$root"
joltc="bin/joltc"

m2="$HOME/.m2/repository/org/clojure/data.json/2.4.0/data.json-2.4.0.jar"
if [ ! -f "$m2" ]; then
  echo "SKIP: $m2 not present — this measurement needs the Maven jars locally."
  exit 0
fi

cmd='(require (quote jolt.deps)) (jolt.deps/add-deps (quote {:deps {org.clojure/data.json {:mvn/version "2.4.0"} org.clojure/data.csv {:mvn/version "1.1.0"} org.clojure/tools.cli {:mvn/version "1.0.219"} org.flatland/ordered {:mvn/version "1.15.11"}}})) (require (quote clojure.data.json)) (require (quote clojure.data.csv)) (require (quote clojure.tools.cli)) (require (quote flatland.ordered))'

# one timed run against cache dir $1; prints the real seconds
one_run() {
  t="/tmp/aotperf.$$"
  JOLT_AOT_CACHE=1 JOLT_CACHE_DIR="$1" JOLT_QUIET=1 /usr/bin/time -p "$joltc" -e "$cmd" >/dev/null 2>>"$t"
  out="$(grep '^real' "$t" | awk '{print $2}')"
  rm -f "$t"; echo "$out"
}
median() { echo "$1" | tr ' ' '\n' | grep -E '^[0-9]' | sort -n | sed -n '2p'; }

# pre-warm maven extractions so COLD measures compile, not download/unzip
pre="$(mktemp -d)"; JOLT_AOT_CACHE=1 JOLT_CACHE_DIR="$pre" JOLT_QUIET=1 "$joltc" -e "$cmd" >/dev/null 2>&1; rm -rf "$pre"

# cold: median of 3 runs, each with a FRESH (empty) cache (so every run recompiles)
cold=""
for i in 1 2 3; do d="$(mktemp -d)"; cold="$cold $(one_run "$d")"; rm -rf "$d"; done
cold="$(median "$cold")"

# warm: median of 3 runs against one populated cache
wcache="$(mktemp -d)"; JOLT_AOT_CACHE=1 JOLT_CACHE_DIR="$wcache" JOLT_QUIET=1 "$joltc" -e "$cmd" >/dev/null 2>&1
warm=""
for i in 1 2 3; do warm="$warm $(one_run "$wcache")"; done
rm -rf "$wcache"
warm="$(median "$warm")"

if [ -z "$cold" ] || [ -z "$warm" ]; then
  echo "ERROR: timing collection failed (cold='$cold' warm='$warm')"; exit 2
fi
saved=$(awk "BEGIN{printf \"%.2f\", $cold - $warm}")
pct=$(awk "BEGIN{printf \"%.0f\", 100 * (1 - $warm/$cold)}")
echo "cold=${cold}s  warm=${warm}s  saved=${saved}s (${pct}% faster)"
if awk "BEGIN{exit !($warm < $cold)}"; then
  echo "OK: warm faster than cold"; exit 0
else
  echo "WARN: warm not faster than cold (noise?)"; exit 1
fi
