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

# app-dir | main-ns | args
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

# Library apps (deps.edn git deps) with deterministic stdout — the key risk is that
# tree-shaking a binary that pulled libraries drops a reachable lib (or, later, core)
# fn. A timing/benchmark app (e.g. ray-tracer) is unsuitable: its output varies.
echo "shake smoke: building each app default vs --tree-shake (output must match)"
run_case markdown-app   app.core  ""
run_case malli-app      app.core  ""
run_case commonmark-app app.core  ""
run_case hiccup-app     app.core  ""

[ "$fail" = 0 ] && echo "shake smoke: passed" || echo "shake smoke: FAILED"
exit $fail
