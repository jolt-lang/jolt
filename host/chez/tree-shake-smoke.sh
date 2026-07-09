#!/bin/sh
# Tree-shake soundness smoke: build each example app twice — default and
# --tree-shake — run both, and require identical output. A wrongly-dropped def
# (incl. a core fn once core-shaking lands) shows up as a diff or a crash. Covers a
# pure-compute app and several that pull libraries via deps.edn (the key risk).
#
# Skips (like build-smoke) when the example repo or the Chez kernel dev files /
# C compiler aren't available. Slow (two full binary builds per app); not in the
# default gate — run with `make shakesmoke`.
root="$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)"
cd "$root"
examples="$root/../examples"
[ -d "$examples" ] || examples="$HOME/src/jolt-lang/examples"
if [ ! -d "$examples" ]; then echo "shake smoke: skipped (examples repo not found)"; exit 0; fi

csv="$JOLT_CHEZ_CSV"
if [ -z "$csv" ]; then
  chez_bin="$(command -v chez || command -v scheme || command -v petite || true)"
  if [ -n "$chez_bin" ]; then
    base="$(cd "$(dirname "$chez_bin")/.." 2>/dev/null && pwd)"
    for d in "$base"/lib/csv*/*/; do [ -f "${d}libkernel.a" ] && csv="${d%/}" && break; done
  fi
fi
if ! command -v cc >/dev/null 2>&1 || [ -z "$csv" ] || [ ! -f "$csv/scheme.h" ]; then
  echo "shake smoke: skipped (Chez kernel dev files or C compiler not available)"; exit 0
fi
export JOLT_CHEZ_CSV="$csv"

tmp="$(mktemp -d)"; trap 'rm -rf "$tmp"' EXIT
fail=0

# app-dir | main-ns | args — looked up under $examples
run_case() {
  app="$examples/$1"; ns="$2"; args="$3"
  [ -d "$app" ] || { echo "  - $1: skipped (not present)"; return; }
  b0="$tmp/$1-plain"; b1="$tmp/$1-shake"
  if ! JOLT_PWD="$app" bin/joltc build -m "$ns" -o "$b0" >/dev/null 2>&1; then
    echo "  - $1: FAIL (default build)"; fail=1; return; fi
  if ! JOLT_PWD="$app" bin/joltc build -m "$ns" -o "$b1" --tree-shake >/dev/null 2>&1; then
    echo "  - $1: FAIL (--tree-shake build)"; fail=1; return; fi
  o0="$(cd "$app" && "$b0" $args 2>&1)"
  o1="$(cd "$app" && "$b1" $args 2>&1)"
  if [ "$o0" != "$o1" ]; then
    echo "  - $1: FAIL (output differs default vs --tree-shake)"
    echo "    --- default ---"; echo "$o0" | head -5
    echo "    --- shake -----"; echo "$o1" | head -5
    fail=1; return
  fi
  s0="$(wc -c < "$b0")"; s1="$(wc -c < "$b1")"
  echo "  - $1: ok (output identical; $((s0/1024))K -> $((s1/1024))K)"
}

# Same as run_case but looked up under the local test/chez/ directory, with
# an optional flat.ss def-pruning assertion: if ASSERT_MISSING is set, grep
# the --tree-shake .build dir for a "ns/def" string and fail if found.
# (The .build dir is the binary's build artifacts, kept alongside the binary.)
run_local_case() {
  app="$root/test/chez/$1"; ns="$2"; args="$3"; assert_missing="$4"
  [ -d "$app" ] || { echo "  - $1: skipped (not present)"; return; }
  b0="$tmp/$1-plain"; b1="$tmp/$1-shake"
  bdir="$tmp/$1-shake.build"
  if ! JOLT_PWD="$app" bin/joltc build -m "$ns" -o "$b0" >/dev/null 2>&1; then
    echo "  - $1: FAIL (default build)"; fail=1; return; fi
  if ! JOLT_PWD="$app" bin/joltc build -m "$ns" -o "$b1" --tree-shake 2>"$tmp/$1-shake-err"; then
    echo "  - $1: FAIL (--tree-shake build)"
    cat "$tmp/$1-shake-err" | head -5
    fail=1; return; fi
  o0="$(cd "$app" && "$b0" $args 2>&1)"
  o1="$(cd "$app" && "$b1" $args 2>&1)"
  if [ "$o0" != "$o1" ]; then
    echo "  - $1: FAIL (output differs default vs --tree-shake)"
    echo "    --- default ---"; echo "$o0" | head -5
    echo "    --- shake -----"; echo "$o1" | head -5
    fail=1; return
  fi
  # Check that a def that should be pruned is indeed absent from the shaken flat.ss
  if [ -n "$assert_missing" ]; then
    blddir="$tmp/$1-shake.build"
    if [ -f "$blddir/flat.ss" ] && grep -q "$assert_missing" "$blddir/flat.ss" 2>/dev/null; then
      echo "  - $1: FAIL (pruned def '$assert_missing' found in shaken flat.ss)"
      fail=1; return
    fi
  fi
  s0="$(wc -c < "$b0")"; s1="$(wc -c < "$b1")"
  echo "  - $1: ok (output identical; $((s0/1024))K -> $((s1/1024))K)"
}

# Library apps (deps.edn git deps) with deterministic stdout — the key risk is that
# tree-shaking a binary that pulled libraries drops a reachable lib (or, later, core)
# fn. A timing/benchmark app (e.g. ray-tracer) is unsuitable: its output varies.
echo "shake smoke: building each app default vs --tree-shake (output must match)"
run_case markdown-app   app.core  ""
run_case malli-app      app.core  ""
run_case commonmark-app app.core  ""
run_case hiccup-app     app.core  ""

# Tree-shake correctness fixtures: apps whose output IDENTICAL default vs --tree-shake
# verifies the fixes in jolt-2f87. The defonce-app additionally asserts a never-referenced
# def ("app.core/dead") is absent from the shaken output.
echo "shake smoke: correctness fixtures (ns-publics, defonce, data-readers)"
run_local_case ns-publics-app   app.core  ""   ""
run_local_case defonce-app      app.core  ""   "app.core/dead"
run_local_case datareader-app   app.core  ""   ""

[ "$fail" = 0 ] && echo "shake smoke: passed" || echo "shake smoke: FAILED"
exit $fail
