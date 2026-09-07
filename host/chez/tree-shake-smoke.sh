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

# JOLT_BIN overrides the jolt under test. The gate targets point it at the
# freshly built target/release/jolt: a `jolt build` costs ~2.5s through the
# prebuilt binary and ~12.5s through the source-mode driver, and this gate
# drives two per app. JOLT_BIN=bin/jolt forces script mode.
jolt="${JOLT_BIN:-bin/jolt}"
# Absolute form, for the cases that cd into a fixture directory first.
case "$jolt" in /*) joltabs="$jolt" ;; *) joltabs="$root/$jolt" ;; esac
# SHAKESMOKE_SCOPE=local runs ONLY the no-git-dep correctness fixtures under
# test/chez (ns-publics/defonce/data-reader apps) — these build in seconds and
# don't need the examples repo, so they're wired into `make shakelocal` / ci. The
# git-dep apps (markdown/malli/…) stay in the manual `make shakesmoke`.
scope="${SHAKESMOKE_SCOPE:-all}"
# Ceiling, in percent, on how much of the program a fixture's shake may KEEP.
# Every local fixture is a hello-world-sized app whose reachable set is a thin
# slice of the prelude — measured 239-242 of ~683 defs, ~35% — so 50% catches a
# shake that ran but kept nearly everything while leaving room for the handful
# of defs a fixture's own source adds. Per-case override: run_local_case's 6th
# argument.
shake_max_kept="${SHAKE_MAX_KEPT_PCT:-50}"
examples="$root/../examples"
[ -d "$examples" ] || examples="$HOME/src/jolt-lang/examples"
if [ "$scope" != "local" ] && [ ! -d "$examples" ]; then echo "shake smoke: skipped (examples repo not found)"; exit 0; fi

csv="$JOLT_CHEZ_CSV"
if [ -z "$csv" ]; then
  # JOLT_CHEZ wins (see host/chez/selfcheck.sh) — else this can pair a
  # PATH-resolved Chez's csv dir with a running interpreter built elsewhere.
  chez_bin="${JOLT_CHEZ:-$(command -v chez || command -v chezscheme || command -v scheme || command -v petite || true)}"
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
  if ! JOLT_PWD="$app" "$jolt" build -m "$ns" -o "$b0" >/dev/null 2>&1; then
    echo "  - $1: FAIL (default build)"; fail=1; return; fi
  if ! JOLT_PWD="$app" "$jolt" build -m "$ns" -o "$b1" --tree-shake >/dev/null 2>&1; then
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

# Same as run_case but looked up under the local test/chez/ directory, with two
# further checks. ASSERT_MISSING ($4): grep the --tree-shake .build dir for a
# string and fail if found — a def the shake must have pruned. EXPECT ($5):
# "shake" (the default) requires the shaken build to report `tree-shake kept`
# and fails on `tree-shake skipped`, printing the offenders jolt named; "bail"
# is for a fixture that resolves vars at runtime on purpose (ns-publics-app),
# whose point is that the keep-everything fallback still answers identically.
# Without the EXPECT check a bail passes this gate silently: it keeps every def,
# so the outputs match by construction — which is how a prelude change that
# bailed every --tree-shake build (0.7.29 to 0.8.4) went unnoticed here.
# (The .build dir is the binary's build artifacts, kept alongside the binary.)
#
# EXPECT also decides two quantitative checks, because "it shook" is not the same
# claim as "it shook anything". A regression that shakes but keeps nearly every
# def, or that stops dropping the compiler image, still prints `tree-shake kept`
# and still matches the plain build's output:
#   - kept fraction: at most $shake_max_kept percent of the defs, or the 6th
#     argument when a fixture legitimately keeps more.
#   - the compiler image: a shake that does NOT bail always drops it (every
#     dce-compile-ref is also a dce-bail-ref, so reaching no bail ref means
#     reaching no compile ref either — see dce.ss), and a bail always keeps it.
#
# EXPECT_OUT ($7): a fixed string the --tree-shake build's stdout must contain —
# for a bailing fixture, the paste-ready :allow-dynamic hint naming exactly the
# sites that remain, which is how the gate proves an allowed site next to a
# non-allowed one is neither let through nor re-suggested.
run_local_case() {
  app="$root/test/chez/$1"; ns="$2"; args="$3"; assert_missing="$4"; expect="${5:-shake}"
  max_kept="${6:-$shake_max_kept}"; expect_out="$7"
  [ -d "$app" ] || { echo "  - $1: skipped (not present)"; return; }
  b0="$tmp/$1-plain"; b1="$tmp/$1-shake"
  bdir="$tmp/$1-shake.build"
  if ! JOLT_PWD="$app" "$jolt" build -m "$ns" -o "$b0" >/dev/null 2>&1; then
    echo "  - $1: FAIL (default build)"; fail=1; return; fi
  if ! JOLT_PWD="$app" "$jolt" build -m "$ns" -o "$b1" --tree-shake >"$tmp/$1-shake-out" 2>"$tmp/$1-shake-err"; then
    echo "  - $1: FAIL (--tree-shake build)"
    cat "$tmp/$1-shake-err" | head -5
    fail=1; return; fi
  case "$expect" in
    shake)
      if grep -q '^jolt build: tree-shake skipped' "$tmp/$1-shake-out"; then
        echo "  - $1: FAIL (tree-shake skipped; the fixture must shake)"
        sed -n '/^jolt build: tree-shake skipped/,/^jolt build: compiling/p' "$tmp/$1-shake-out" | grep -v '^jolt build: compiling' | head -8
        fail=1; return
      fi
      if ! grep -q '^jolt build: tree-shake kept ' "$tmp/$1-shake-out"; then
        echo "  - $1: FAIL (no 'tree-shake kept' report in the --tree-shake build output)"
        fail=1; return
      fi
      counts="$(sed -n 's/^jolt build: tree-shake kept \([0-9]*\) of \([0-9]*\) defs.*/\1 \2/p' "$tmp/$1-shake-out" | head -1)"
      kept_n="${counts%% *}"; kept_m="${counts##* }"
      if [ -z "$kept_n" ] || [ -z "$kept_m" ] || [ "$kept_m" = 0 ]; then
        echo "  - $1: FAIL (could not read the kept/total counts off the tree-shake report)"
        fail=1; return
      fi
      kept_pct=$(( kept_n * 100 / kept_m ))
      if [ "$kept_pct" -gt "$max_kept" ]; then
        echo "  - $1: FAIL (tree-shake kept $kept_n of $kept_m defs, ${kept_pct}% > the ${max_kept}% ceiling)"
        fail=1; return
      fi
      if ! grep -q '^jolt build: dropping compiler image' "$tmp/$1-shake-out"; then
        echo "  - $1: FAIL (the shake ran but the binary kept the compiler image)"
        fail=1; return
      fi ;;
    bail)
      if ! grep -q '^jolt build: tree-shake skipped' "$tmp/$1-shake-out"; then
        echo "  - $1: FAIL (expected the keep-everything bail; the shake ran)"
        fail=1; return
      fi
      if grep -q '^jolt build: dropping compiler image' "$tmp/$1-shake-out"; then
        echo "  - $1: FAIL (a bailed build must keep the compiler image)"
        fail=1; return
      fi ;;
    *) echo "  - $1: FAIL (unknown EXPECT '$expect')"; fail=1; return ;;
  esac
  if [ -n "$expect_out" ] && ! grep -qF -- "$expect_out" "$tmp/$1-shake-out"; then
    echo "  - $1: FAIL (the --tree-shake build output lacks: $expect_out)"
    sed -n '/^jolt build: tree-shake skipped/,/^jolt build: compiling/p' "$tmp/$1-shake-out" | grep -v '^jolt build: compiling' | head -8
    fail=1; return
  fi
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
if [ "$scope" = all ]; then
run_case markdown-app   app.core  ""
run_case malli-app      app.core  ""
run_case commonmark-app app.core  ""
run_case hiccup-app     app.core  ""
fi

# Tree-shake correctness fixtures: apps whose output IDENTICAL default vs --tree-shake
# verifies the fixes in jolt-2f87. The defonce-app additionally asserts a never-referenced
# def ("app.core/dead") is absent from the shaken output. The pattern names the var
# without its def form: app defs are emitted as def-var-with-meta!, and a pattern
# pinned to def-var! matched nothing, so the assertion passed against an unshaken
# flat.ss for as long as the emitter has carried metadata.
# ns-publics-app is the one fixture that must BAIL: it enumerates its namespace at
# runtime, which the static graph cannot follow.
echo "shake smoke: correctness fixtures (ns-publics, defonce, data-readers)"
run_local_case ns-publics-app   app.core  ""   ""   bail
run_local_case defonce-app      app.core  ""   "\"app.core\" \"dead\""
run_local_case datareader-app   app.core  ""   ""
# data-reader literal rewriting: a #tag whose reader returns a FORM must splice
# identically under a plain and a --tree-shake build (the tree-shake path once
# skipped the per-form reader hook and crashed on the raw tagged literal).
run_local_case datareader-tag-app app.core "" ""
# a PROGRAMMATICALLY-registered reader (alter-var-root on *data-readers*, no
# data_readers.clj): its reader fn is reachable only through the baked data-readers
# map, so it must be a DCE root or the shake prunes it and read-string degrades.
run_local_case progreader-app     app.core "" ""
# multi-path: the same app exercised down BOTH argv branches — a def wrongly
# shaken off the non-default path diffs on the second invocation.
run_local_case multipath-app    app.core  ""     ""
run_local_case multipath-app    app.core  "alt"  ""
# duplicate-fqn regression: a twice-defined var whose first def references a
# helper referenced nowhere else — the union (not overwrite) keeps the helper alive.
run_local_case dupfqn-app      app.core  ""     ""
# spliced-callee regression (#882): a private helper that calls `resolve` and is
# reachable only through the copies the inline pass made of it. It is KEPT (an
# inlined frame still names ns/file:line) but is not reachable code, so it must
# not bail the shake — core.async's go-macro walkers have exactly this shape, and
# rooting them kept every def and the compiler image in any app that merely
# loaded core.async. Bails against the pre-#882 dce.ss. app.core/walk-body is the
# unreachable caller the helpers were spliced into, so it must be pruned.
run_local_case spliced-resolve-app app.core "" "\"app.core\" \"walk-body\""
# deps.edn :jolt/tree-shake {:allow-dynamic […]}: two reachable `resolve` callers
# on paths -main never takes — the app's own `res` (spec.alpha/res's shape) and
# a :local/root library's `dynaload` behind a delay (spec.gen's shape). The app's
# deps.edn vouches for the first, the LIBRARY's for the second, and the union
# lets the shake run: `dead` is pruned and the compiler image dropped. Bails
# against a jolt that does not read the key. Both callers are ^:redef so the
# inline pass leaves them as the defs the bail names.
run_local_case allow-dynamic-app app.core "" "\"app.core\" \"dead\""
# …and the same app with one more reachable caller nothing vouches for must
# still bail, with the hint naming that caller alone — proof the allowed sites
# were honoured (neither is listed) and the non-allowed one was not let through.
run_local_case allow-dynamic-partial-app app.core "" "" bail "" \
  ':jolt/tree-shake {:allow-dynamic [app.core/lookup]}'

[ "$fail" = 0 ] && echo "shake smoke: passed" || echo "shake smoke: FAILED"
exit $fail
