#!/bin/sh
# error-report-check.sh — golden-file gate over what jolt PRINTS when it fails.
#
# Every other gate asserts that a bad program is rejected. None of them asserts
# anything about the report the user then reads, so the report has been free to
# drift: a message can name the wrong cause, a position can disagree with itself,
# and a trace can fill with reader internals without a single gate noticing. This
# pins the whole rendered report — message, position, ex-data, trace and exit
# status — for a set of representative failures, so a change to any of it lands
# as a reviewable diff instead of silently.
#
# Layout, one directory per case:
#
#   test/errors/<group>/<case>/input.clj   a program, run as `jolt <that path>`
#   test/errors/<group>/<case>/expr        an expression, run as `jolt -e <it>`
#   test/errors/<group>/<case>/argv        a whole argv line, run as `jolt <it>`
#   test/errors/<group>/<case>/output.txt  the expected report
#
# A case supplies exactly one of input.clj, expr and argv. argv is the escape
# hatch for anything the first two cannot spell — how a path is written on the
# command line (which the report echoes back), a subcommand, a flag — and its
# contents are word-split, so it is for fixed arguments, not for quoting games.
#
# The expected file ends with an "exit: N" line, so a case that stops failing (or
# starts) shows up as a diff rather than as identical empty output.
#
# Regenerate after an intended change:
#
#   sh host/chez/error-report-check.sh generate
#
# and read the diff before committing it — the whole point of the gate is that
# these files only change deliberately.
#
# Determinism. Reports name paths and line numbers, and most of those are
# incidental to the case:
#
#   - The case's own file:line IS the thing under test and is kept verbatim.
#     Cases run from the repo root with a repo-relative argv path, so the report
#     names test/errors/... on every machine. The one exception is the leading
#     directory: bin/jolt cd's to the repo root and so reports the case by its
#     ABSOLUTE path, where the built binary reports the relative one. That is the
#     launcher's choice, not the reporter's, and pinning it would make the gate
#     answer "which driver ran" instead of "what does the report say" — so an
#     absolute case path is rewritten back to the relative form. Everything after
#     the prefix, "././" included, survives.
#   - A frame inside jolt's own sources (jolt-core/, host/, stdlib/,
#     compatibility/) has its line number replaced with LINE. Otherwise every
#     unrelated edit to main.clj would rewrite golden files across the suite,
#     which is how a gate stops being read.
#   - An absolute repo path, the home directory, and a temp directory are
#     rewritten to <root>, ~ and <tmp>. None should appear at all; they are
#     normalized rather than trusted not to.
#
# JOLT_BIN selects the jolt under test, and unlike the other gates it defaults to
# the BUILT binary rather than bin/jolt. The two drivers genuinely disagree about
# what a report says — the source-mode driver has jolt-core registered, so its
# trace reads "jolt.main/cmd-run (jolt-core/jolt/main.clj:343)" where the built
# binary shows a bare "cmd-run" — and normalizing that away would make the gate
# stop seeing the trace it exists to pin. The built binary is what users run, so
# the golden files record what IT prints, and a run needs `make testbin` first.
#
# (Those launcher frames should not be in a user-facing trace at ALL; they are
# the same plumbing noise as the reader's rdr-* frames. That is jolt-vy5i, and
# when it is fixed these golden files change — which is the point of pinning.)

root="$(CDPATH='' cd -- "$(dirname -- "$0")/../.." && pwd)"
cd "$root" || exit 1

cases_dir="test/errors"
jolt_bin="${JOLT_BIN:-target/release/jolt}"

if [ ! -x "$jolt_bin" ]; then
  echo "  FAIL: no jolt at $jolt_bin — run 'make testbin', or set JOLT_BIN"
  echo "error report check: FAILED"
  exit 1
fi

# Each case is a sub-second invocation, so a case that does not finish has hung,
# and a hung case is invisible under make until the job limit. Cap each one; the
# case then fails and names itself. Same fallback ladder smoke.sh uses, because a
# stock macOS has neither timeout nor gtimeout.
case_timeout="${JOLT_ERROR_TIMEOUT:-60}"
if [ "$case_timeout" = "0" ]; then
  run_capped=""
else
  for t in timeout gtimeout; do
    if command -v "$t" >/dev/null 2>&1 && "$t" --foreground 1 true >/dev/null 2>&1; then
      run_capped="$t --foreground $case_timeout"
      break
    fi
  done
  [ -n "${run_capped:-}" ] || run_capped="sh $root/host/chez/cap.sh $case_timeout"
fi

mode="${1:-check}"

tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

# Rewrite the incidental parts of a report. Reads stdin, writes stdout.
#
# The jolt-source line-number rule deliberately matches only a path under one of
# jolt's own source roots: a frame naming the CASE's file keeps its line, which
# is what nearly every case is actually asserting.
# HOME is only substituted when it is a real directory prefix. An UNSET HOME
# would make the expression "s##~#g", which sed reads as "reuse the last regex"
# and which would rewrite whatever the previous rule matched; a HOME of "/" would
# turn every slash into a tilde. Neither is hypothetical in a container.
if [ -n "${HOME:-}" ] && [ "$HOME" != "/" ]; then
  home_rule="s#$HOME#~#g"
else
  home_rule="s#^##"
fi

normalize() {
  sed \
    -e "s#$tmp#<tmp>#g" \
    -e "s#$root/$cases_dir/#./$cases_dir/#g" \
    -e "s#$root#<root>#g" \
    -e "$home_rule" \
    -e 's#\(jolt-core/[A-Za-z0-9_/.-]*\):[0-9][0-9]*#\1:LINE#g' \
    -e 's#\(host/[A-Za-z0-9_/.-]*\):[0-9][0-9]*#\1:LINE#g' \
    -e 's#\(stdlib/[A-Za-z0-9_/.-]*\):[0-9][0-9]*#\1:LINE#g' \
    -e 's#\(compatibility/[A-Za-z0-9_/.-]*\):[0-9][0-9]*#\1:LINE#g'
}

# Run one case, emitting its normalized report plus the exit status.
run_case() {
  _dir="$1"
  if [ -f "$_dir/input.clj" ]; then
    # shellcheck disable=SC2086
    $run_capped "$jolt_bin" "$_dir/input.clj" > "$tmp/out" 2>&1
  elif [ -f "$_dir/expr" ]; then
    # shellcheck disable=SC2086
    $run_capped "$jolt_bin" -e "$(cat "$_dir/expr")" > "$tmp/out" 2>&1
  else
    # Deliberately unquoted: argv is a whole argument LINE and word-splitting it
    # is the point of this mode.
    # shellcheck disable=SC2086
    $run_capped "$jolt_bin" $(cat "$_dir/argv") > "$tmp/out" 2>&1
  fi
  _status=$?
  normalize < "$tmp/out"
  echo "exit: $_status"
}

# Every case directory, sorted, so the log reads the same on every host.
list_cases() {
  find "$cases_dir" -mindepth 2 -maxdepth 2 -type d 2>/dev/null | LC_ALL=C sort
}

if [ ! -d "$cases_dir" ]; then
  echo "  FAIL: no case directory at $cases_dir"
  echo "error report check: FAILED"
  exit 1
fi

count=0
fail=0

# Cases live at exactly test/errors/<group>/<case>. A case file one level too
# high is invisible to the discovery glob, and an invisible case is a case that
# passes without ever running — the exact failure this suite exists to prevent,
# so it is refused rather than skipped.
for stray in $(find "$cases_dir" -mindepth 2 -maxdepth 2 -type f \
                    \( -name input.clj -o -name expr -o -name argv \) 2>/dev/null \
               | LC_ALL=C sort); do
  echo "  FAIL: $stray sits at <group>/<file>; a case is <group>/<case>/<file>"
  fail=1
done

for dir in $(list_cases); do
  name="${dir#"$cases_dir"/}"

  # A case must say what to run, exactly one way. Getting this wrong silently
  # would make the case vacuous, which is the failure mode this whole file is
  # about.
  spells=0
  for f in input.clj expr argv; do
    [ -f "$dir/$f" ] && spells=$((spells + 1))
  done
  if [ "$spells" != 1 ]; then
    echo "  FAIL: $name names $spells of input.clj/expr/argv (want exactly 1)"
    fail=1
    continue
  fi

  count=$((count + 1))
  run_case "$dir" > "$tmp/actual"

  if [ "$mode" = "generate" ]; then
    cp "$tmp/actual" "$dir/output.txt"
    echo "  wrote $name/output.txt"
    continue
  fi

  if [ ! -f "$dir/output.txt" ]; then
    echo "  FAIL: $name has no output.txt (run: sh host/chez/error-report-check.sh generate)"
    fail=1
    continue
  fi

  if ! diff -u "$dir/output.txt" "$tmp/actual" > "$tmp/diff"; then
    echo "  FAIL: $name report changed (< expected, > actual)"
    # The diff's own header names the scratch file; rewrite it so the failure
    # reads the same on every machine and in every CI log.
    sed -e "s#$tmp/actual#(actual)#" -e 's/^/    /' "$tmp/diff"
    fail=1
  fi
done

if [ "$mode" = "generate" ]; then
  echo "error report check: generated $count case(s)"
  exit 0
fi

# A golden-file suite that silently matches nothing passes, and passing on zero
# cases is exactly what a broken case-discovery path looks like.
if [ "$count" = 0 ]; then
  echo "  FAIL: no cases found under $cases_dir"
  fail=1
fi

[ "$fail" = 0 ] && echo "error report check: passed ($count cases)" \
                || echo "error report check: FAILED"
exit $fail
