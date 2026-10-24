#!/bin/sh
# Run the jolt benchmark suite against JVM Clojure and print a jolt/JVM scorecard.
#
# jolt's optimizing passes (direct-linking, inlining, scalar-replace, whole-program
# inference) fire only in an AOT BUILD — `jolt run -m` is unoptimized — so each
# benchmark is compiled to an optimized standalone binary and timed. JVM Clojure
# runs the same portable source for the absolute reference. Each benchmark prints
# `runs: [...]` and `mean: N ms`; the table shows the means and the jolt/JVM ratio.
#
#   bench/run.sh                 # full suite + JVM scorecard
#   bench/run.sh fib             # one benchmark, default size
#   bench/run.sh fib 32          # one benchmark, custom size
#   bench/run.sh startup         # the startup row alone (whole-process wall clock)
#   NO_JVM=1 bench/run.sh        # jolt only (skip the JVM reference)
#   MODE_A=1 bench/run.sh        # also time a plain-release build (no
#                                # --direct-link --opt) per bench — the default
#                                # `jolt build` most users get; roughly doubles
#                                # the suite's build time, so it's on demand
#
# Building needs Chez's kernel dev files (libkernel.a + scheme.h) and a C compiler,
# the same as `jolt build`; set JOLT_CHEZ_CSV to override the detected csv dir.
set -e
# A relative $JOLT_BIN names a path from the caller's directory; pin it before
# the cd below moves us into bench/.
case "${JOLT_BIN:-}" in
  */*) JOLT_BIN="$(cd "$(dirname "$JOLT_BIN")" && pwd)/$(basename "$JOLT_BIN")" ;;
esac
cd "$(dirname "$0")"
root="$(cd .. && pwd)"
# $JOLT_BIN points the suite at another jolt — a released binary, or the one a
# release workflow just built. ci/bench-gate.sh drives both sides through it.
jolt="${JOLT_BIN:-$root/bin/jolt}"
export JOLT_PWD="$PWD"

# The benchmark list is the single source of truth for what the suite covers, and
# ci/bench-gate.sh reads it from here rather than keeping a second copy. Printed
# before the toolchain check below, so listing needs no Chez and no cc.
if [ "${1:-}" = "--list" ]; then
  # defined below; re-stated by name so the list lives in exactly one place
  # cd'd into bench/ above, so name the script rather than reusing $0 (which is
  # still relative to the caller's directory and no longer resolves)
  sed -n 's/^BENCHES="\(.*\)"$/\1/p' run.sh | tr ' ' '\n' | grep .
  exit 0
fi

# Locate Chez's kernel dev files for the optimized build (as build-smoke.sh does).
csv="$JOLT_CHEZ_CSV"
if [ -z "$csv" ]; then
  # JOLT_CHEZ wins (see host/chez/selfcheck.sh) — else this can pair a
  # PATH-resolved Chez's csv dir with a running interpreter built elsewhere.
  chez_bin="${JOLT_CHEZ:-$(command -v chez || command -v scheme || command -v petite || true)}"
  if [ -n "$chez_bin" ]; then
    base="$(cd "$(dirname "$chez_bin")/.." 2>/dev/null && pwd)"
    for d in "$base"/lib/csv*/*/; do
      [ -f "${d}libkernel.a" ] && csv="${d%/}" && break
    done
  fi
fi
if [ -z "$csv" ] || [ ! -f "$csv/libkernel.a" ] || [ ! -f "$csv/scheme.h" ] || ! command -v cc >/dev/null 2>&1; then
  echo "error: the optimized build needs Chez kernel dev files (libkernel.a + scheme.h) and cc." >&2
  echo "       set JOLT_CHEZ_CSV to the csv dir, e.g. \$(brew --prefix chezscheme)/lib/csv*/<machine>." >&2
  exit 1
fi
export JOLT_CHEZ_CSV="$csv"

bindir="$(mktemp -d)"
trap 'rm -rf "$bindir"' EXIT

# name:default-arg, each sized to run in a few seconds. Axes: see README.md.
BENCHES="fib:30 tak:24 loop-recur:20000 mandelbrot:200 arrays:40000 arrays-unhinted:1000 byte-arrays:400 gc-arrays:150 mathfns:1000000 mathfns-unhinted:1000000 collections:30000 vecops:600000 seqs:20000 lazy-threads:100000 apply-rest:1000000 sorted-access:40000 coll-shapes:4000 sorted-build:20000 nth-access:1000000 transducers:20000 transients:50000 keyed-lookup:3000 hash-eq:2000 literals:100000 string-build:60000 string-ops:100000 string-ops-unhinted:100000 char-scan:40000 char-scan-unhinted:40000 printing:300 mono-dispatch:2000 dispatch:2000 binary-trees:14 typed-records:100000 typed-records-unhinted:100000 stm:200000 executors:60000 compile-forms:200 host-io:200 string-scan:40 cst-format:80 coll-dispatch:200000 metadata:100000 parallel-colls:200000 atom-ops:200000 watched-swap:4000 pool-tasks:400 sorted-set-build:50000 int-map-build:50000"

# A row is one summary line, then indented detail lines the scorecard reads for
# the README's prose: every line a bench printed (its `runs:`, and any phase
# split or per-thread slowdown), per host, plus the plain-release mean under
# MODE_A and a node reference where bench/<ns>.js exists.
detail() {   # detail <host> <output>
  printf '%s\n' "$2" | grep . | sed "s/^/  $1: /"
}
run_one() {
  ns="${1%%:*}"; arg="${2:-${1##*:}}"
  if ! "$jolt" build -m "$ns" -o "$bindir/$ns" --direct-link --opt >/dev/null 2>&1; then
    printf '%-16s  jolt build FAILED\n' "$ns"; return
  fi
  jout=$("$bindir/$ns" "$arg" 2>/dev/null)
  jmean=$(printf '%s\n' "$jout" | awk '/^mean:/{print $2}')
  # mode A: the plain-release binary (no --direct-link --opt) — what a default
  # `jolt build` ships. Tracked so a release-mode win or regression is visible.
  rmean=""
  if [ -n "$MODE_A" ]; then
    if "$jolt" build -m "$ns" -o "$bindir/$ns-rel" >/dev/null 2>&1; then
      rmean=$("$bindir/$ns-rel" "$arg" 2>/dev/null | awk '/^mean:/{print $2}')
    fi
  fi
  vout=""
  if [ -z "$NO_JVM" ]; then
    vout=$(clojure -Sdeps '{:paths ["."]}' -M -m "$ns" "$arg" 2>/dev/null)
    vmean=$(printf '%s\n' "$vout" | awk '/^mean:/{print $2}')
    ratio=$(awk "BEGIN{ if (\"$vmean\"+0>0 && \"$jmean\"+0>0) printf \"%.1fx\", (\"$jmean\"+0)/(\"$vmean\"+0); else printf \"-\" }")
    printf '%-16s jolt %9s ms   jvm %8s ms   %s\n' "$ns" "${jmean:--}" "${vmean:--}" "$ratio"
  else
    printf '%-16s jolt %9s ms\n' "$ns" "${jmean:--}"
  fi
  [ -n "$MODE_A" ] && printf '  release: %s ms\n' "${rmean:--}"
  detail jolt "$jout"
  [ -n "$vout" ] && detail jvm "$vout"
  js="$(echo "$ns" | tr - _).js"
  if [ -f "$js" ] && command -v node >/dev/null 2>&1; then
    detail node "$(node "$js" "$arg" 2>/dev/null)"
  fi
  return 0
}

# The startup row: every bench above times the compute INSIDE a running binary,
# so none of them sees what a built program costs from exec to exit — the boot
# image's decode and the runtime's init. This builds bench/hello.clj and times
# the whole process, best of REPS (noise only ever adds time), against the JVM
# starting the same program: `java` on the classpath the clojure CLI resolves,
# not the CLI itself, whose own JVM would be most of the number.
STARTUP_REPS="${STARTUP_REPS:-7}"
best_ms() {   # best-of-N whole-process wall clock, in ms, for a command
  best=""; i=0
  while [ "$i" -lt "$STARTUP_REPS" ]; do
    t0=$(perl -MTime::HiRes=time -e 'printf "%d", time()*1000')
    "$@" >/dev/null 2>&1
    t1=$(perl -MTime::HiRes=time -e 'printf "%d", time()*1000')
    d=$((t1 - t0)); i=$((i + 1))
    if [ -z "$best" ] || [ "$d" -lt "$best" ]; then best="$d"; fi
  done
  echo "$best"
}
run_startup() {
  if ! "$jolt" build -m hello -o "$bindir/hello" --direct-link --opt >/dev/null 2>&1; then
    printf '%-16s  jolt build FAILED\n' startup; return
  fi
  jms=$(best_ms "$bindir/hello")
  if [ -z "$NO_JVM" ]; then
    cp=$(clojure -Sdeps '{:paths ["."]}' -Spath 2>/dev/null)
    vms=$(best_ms java -cp "$cp" clojure.main -m hello)
    ratio=$(awk "BEGIN{ if (\"$vms\"+0>0 && \"$jms\"+0>0) printf \"%.2fx\", (\"$jms\"+0)/(\"$vms\"+0); else printf \"-\" }")
    printf '%-16s jolt %9s ms   jvm %8s ms   %s\n' startup "$jms" "${vms:--}" "$ratio"
  else
    printf '%-16s jolt %9s ms\n' startup "$jms"
  fi
}

if [ "${1:-}" = "startup" ]; then
  run_startup
elif [ -n "$1" ]; then
  spec=""
  for s in $BENCHES; do [ "${s%%:*}" = "$1" ] && spec="$s"; done
  [ -n "$spec" ] || { echo "unknown benchmark: $1 (have: ${BENCHES} startup)" >&2; exit 1; }
  run_one "$spec" "$2"
else
  echo "jolt benchmark suite — optimized AOT binaries${NO_JVM:+ }${NO_JVM:-, vs JVM Clojure}"
  # What the sitting ran on, read by bench/scorecard.clj for the README's
  # "Measured …" line — recorded here because it is true of this run, not of
  # whenever the table is rendered.
  echo "env date: $(date +%Y-%m-%d)"
  if [ "$(uname)" = Darwin ]; then
    echo "env machine: $(sysctl -n machdep.cpu.brand_string), macOS $(sw_vers -productVersion)"
  else
    echo "env machine: $(awk -F': ' '/^model name/{print $2; exit}' /proc/cpuinfo), $(uname -sr)"
  fi
  echo "env jolt: $("$jolt" --version 2>/dev/null)"
  [ -z "$NO_JVM" ] && echo "env jvm: $(java -version 2>&1 | head -1)"
  echo "env chez: $("${JOLT_CHEZ:-$(command -v chez || command -v scheme || command -v petite)}" --version 2>&1)"
  command -v node >/dev/null 2>&1 && echo "env node: $(node --version)"
  echo "env mode-a: ${MODE_A:+yes}"
  for spec in $BENCHES; do run_one "$spec"; done
  run_startup
fi
