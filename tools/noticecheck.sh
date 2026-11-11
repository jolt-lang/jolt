#!/bin/sh
# noticecheck.sh — NOTICE stays true to the tree.
#
# jolt ships other people's code: submodules compiled into every binary, and
# files ported from Clojure, babashka and others. NOTICE names each one with
# its copyright and license, and the license texts live in licenses/. This gate
# fails when that record drifts:
#
#   - a submodule that is not test-only and that NOTICE does not name
#   - a licenses/ file NOTICE points at that is missing, or one it never names
#   - a source file or directory NOTICE names that does not exist, or a named
#     file without a copyright line in it
#   - a release archive or Nix package that leaves NOTICE or licenses/ out
#
# Run from the repo root: sh tools/noticecheck.sh (or `make noticecheck`).
set -u
fails=0
fail() { echo "  FAIL: $*"; fails=$((fails + 1)); }

# Test-only submodules: the gates run them, no binary carries them.
test_only="vendor/sci vendor/clojure-test-suite"

for path in $(sed -n 's/^[[:space:]]*path = //p' .gitmodules); do
  case " $test_only " in *" $path "*) continue ;; esac
  grep -q "($path" NOTICE || fail "submodule $path is not named in NOTICE"
done

for lic in $(grep -oE 'licenses/[A-Za-z0-9_-]+\.[a-z]+' NOTICE | sort -u); do
  [ -f "$lic" ] || fail "NOTICE points at $lic, which does not exist"
done
for lic in licenses/*; do
  grep -q "$lic" NOTICE || fail "$lic is not referenced from NOTICE"
done

has_copyright() { grep -qiE 'copyright|\(c\)' "$1"; }
for p in $(grep -oE '(jolt-core|stdlib|compatibility|host)/[A-Za-z0-9_./-]+' NOTICE | sed 's/[.,]$//' | sort -u); do
  p=${p%/}
  if [ -d "$p" ]; then
    # a directory NOTICE names is derived throughout: every file says so
    for f in "$p"/*.clj; do
      [ -f "$f" ] || continue
      has_copyright "$f" || fail "$f (under $p in NOTICE) has no copyright line"
    done
  elif [ -f "$p" ]; then
    has_copyright "$p" || fail "$p is named in NOTICE but carries no copyright line"
  else
    fail "NOTICE names $p, which does not exist"
  fi
done

grep -q 'cp README.md LICENSE NOTICE' .github/workflows/release.yml \
  || fail "release.yml does not package NOTICE"
grep -q 'cp -R licenses' .github/workflows/release.yml \
  || fail "release.yml does not package licenses/"
grep -q 'cp LICENSE NOTICE' flake.nix || fail "flake.nix does not install NOTICE"

if [ "$fails" -eq 0 ]; then
  echo "notice check: passed"
else
  echo "notice check: $fails failure(s)"
  exit 1
fi
