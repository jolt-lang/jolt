#!/bin/bash
# clojure-test-suite gate: run the vendored jank-lang/clojure-test-suite
# (vendor/clojure-test-suite) against joltc, one process per test namespace (a
# hang or crash is contained), and compare per-namespace fail/error counts
# against the checked-in baseline test/chez/cts-known-failures.txt.
#
# The comparison is exact, like certify's allowlist: a namespace doing WORSE
# than the baseline fails the gate (regression), and one doing BETTER also
# fails (stale baseline — update the file in the same change that improved it).
#
#   JOLT_CTS_JOBS=N            parallel workers (default 4)
#   JOLT_CTS_TIMEOUT=SECS      per-namespace timeout (default 120)
#   JOLT_CTS_WRITE_BASELINE=1  regenerate the baseline file instead of gating
#   JOLT_CTS_NS=ns1,ns2        run only these namespaces, verbose, no gating
set -u
root="$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)"
cd "$root"

suite="vendor/clojure-test-suite/test"
baseline="test/chez/cts-known-failures.txt"
app="$root/test/chez/cts-app"
# one process per namespace; default the worker count to the CPU count
cpus="$(sysctl -n hw.ncpu 2>/dev/null || nproc 2>/dev/null || echo 4)"
jobs="${JOLT_CTS_JOBS:-$cpus}"
tmo="${JOLT_CTS_TIMEOUT:-120}"
# JOLT_BIN overrides the joltc under test (make test points it at the freshly
# built target/release/joltc — 10x faster boot than script mode)
joltc="${JOLT_BIN:-$root/bin/joltc}"

if [ ! -d "$suite/clojure" ]; then
  echo "cts: skipped (git submodule update --init vendor/clojure-test-suite)"
  exit 0
fi

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

# test namespaces from the .cljc files (portability is a helper, not a test ns)
find "$suite" -name '*.cljc' | sed "s|^$suite/||;s|\.cljc$||;s|/|.|g;s|_|-|g" \
  | grep -v '\.portability$' | sort > "$work/nses"
if [ -n "${JOLT_CTS_NS:-}" ]; then
  echo "${JOLT_CTS_NS}" | tr ',' '\n' > "$work/nses"
fi

# round-robin the namespaces over N sequential workers; each worker appends
# "ns pass fail error" lines (HUNG/CRASH in the pass column) to its own file.
awk -v j="$jobs" '{print > ("'"$work"'/chunk." (NR % j))}' "$work/nses"
run_chunk() {
  chunk="$1"; out="$2"
  while IFS= read -r ns; do
    res=$(JOLT_PWD="$app" perl -e "alarm $tmo; exec @ARGV" -- "$joltc" -M:cts "$ns" 2>&1 </dev/null)
    rc=$?
    line=$(echo "$res" | grep '^CTS-RESULT' | head -1)
    if [ -n "$line" ]; then
      echo "$line" | awk '{print $2, $3, $4, $5}' >> "$out"
      if [ -n "${JOLT_CTS_NS:-}" ]; then
        echo "$res" | grep -E 'FAIL:|ERROR:|LOAD:' | sed 's/^/    /' >> "$out"
      fi
    elif [ $rc -ge 128 ]; then
      echo "$ns HUNG 0 0" >> "$out"
    else
      echo "$ns CRASH 0 0" >> "$out"
    fi
  done < "$chunk"
}
for c in "$work"/chunk.*; do
  run_chunk "$c" "$c.res" &
done
wait
cat "$work"/chunk.*.res 2>/dev/null | sort > "$work/results"

if [ -n "${JOLT_CTS_NS:-}" ]; then
  cat "$work/results"
  exit 0
fi

summary=$(awk '$2!="HUNG" && $2!="CRASH" {p+=$2; f+=$3; e+=$4; c++}
               $2=="HUNG" {h++} $2=="CRASH" {x++}
               END {printf "%d namespaces: pass %d, fail %d, error %d, hung %d, crash %d",
                    c+h+x, p, f, e, h, x}' "$work/results")

if [ "${JOLT_CTS_WRITE_BASELINE:-0}" = "1" ]; then
  {
    echo "# clojure-test-suite known failures: <namespace> <fail> <error>"
    echo "# The gate fails on any per-namespace change, worse OR better; regenerate"
    echo "# with: JOLT_CTS_WRITE_BASELINE=1 host/chez/cts.sh"
    awk '$2=="HUNG" || $2=="CRASH" {print $1, $2, $2; next}
         $3 != 0 || $4 != 0 {print $1, $3, $4}' "$work/results"
  } > "$baseline"
  echo "cts: $summary"
  echo "cts: baseline written to $baseline ($(grep -cv '^#' "$baseline") namespaces)"
  exit 0
fi

if [ ! -f "$baseline" ]; then
  echo "cts: FAIL — no baseline; run JOLT_CTS_WRITE_BASELINE=1 host/chez/cts.sh"
  exit 1
fi

status=0
while read -r ns p f e; do
  case "$p" in HUNG|CRASH) f="$p"; e="$p" ;; esac
  bl=$(grep -v '^#' "$baseline" | awk -v n="$ns" '$1==n {print $2, $3; exit}')
  if [ -n "$bl" ]; then bf="${bl%% *}"; be="${bl##* }"; else bf=0; be=0; fi
  if [ "$f" = "$bf" ] && [ "$e" = "$be" ]; then
    continue
  elif [ "$f" = "HUNG" ] || [ "$f" = "CRASH" ] \
       || { [ "$bf" != "HUNG" ] && [ "$bf" != "CRASH" ] \
            && { [ "$f" -gt "$bf" ] || [ "$e" -gt "$be" ]; }; }; then
    echo "cts: NEW regression in $ns — fail $f error $e (baseline $bf $be)"
    status=1
  else
    echo "cts: STALE baseline for $ns — now fail $f error $e (baseline $bf $be); update $baseline"
    status=1
  fi
done < "$work/results"

# a baseline entry whose namespace no longer reports is stale too
while read -r ns bf be; do
  grep -q "^$ns " "$work/results" || { echo "cts: STALE baseline entry $ns (namespace gone)"; status=1; }
done < <(grep -v '^#' "$baseline")

echo "cts: $summary"
if [ $status -eq 0 ]; then echo "cts: passed (matches baseline)"; else echo "cts: FAILED"; fi
exit $status
