#!/usr/bin/env bash
# tools/version.sh is the one definition of a checkout's version: what
# `jolt --version` says when nothing bakes one in. Three things consume it
# (bin/jolt, build-jolt.ss, the release workflow's meta job), and the property
# that matters is lost by editing any one of them back to a bare `git describe`:
# the rolling `vnightly` tag the nightly workflow moves to main's head is the
# NEAREST tag from main, so a plain `git describe --tags` answers "vnightly" on
# every clone that has fetched it — and jolt.deps reads a version with no
# numeric part as one no :jolt/min-version floor applies to.
set -euo pipefail

root="$(cd "$(dirname "$0")/.." && pwd)"
script="$root/tools/version.sh"
fail() { echo "version-smoke: $*" >&2; exit 1; }

[ -x "$script" ] || fail "tools/version.sh is missing or not executable"

tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

repo="$tmp/repo"
mkdir -p "$repo"
g() { git -C "$repo" -c user.name=t -c user.email=t@t -c init.defaultBranch=main "$@"; }
g init -q
echo one > "$repo/f"
g add f
g commit -q -m one
g tag v0.1.0
echo two > "$repo/f"
g commit -q -am two
sha="$(g rev-parse --short HEAD)"

# (a) the rolling tag at HEAD is not the version: release tags only
g tag vnightly
got="$("$script" "$repo")"
[ "$got" = "v0.1.0-1-g$sha" ] || fail "with vnightly at HEAD: got '$got', want 'v0.1.0-1-g$sha'"

# (b) edits in the tree say so (the AOT cache key rides on the version string)
echo three > "$repo/f"
got="$("$script" "$repo")"
[ "$got" = "v0.1.0-1-g$sha-dirty" ] || fail "dirty tree: got '$got', want 'v0.1.0-1-g$sha-dirty'"
g checkout -q -- f

# (c) on the release tag itself: the tag, nothing else
g checkout -q v0.1.0
got="$("$script" "$repo")"
[ "$got" = "v0.1.0" ] || fail "on the tag: got '$got', want 'v0.1.0'"
g checkout -q main

# (d) no release tag reachable (a shallow clone; a tree tagged only vnightly):
#     the sha, prefixed — a bare 0a1b2c3 would read as version 0 to every floor
g tag -d v0.1.0 >/dev/null
got="$("$script" "$repo")"
[ "$got" = "dev-g$sha" ] || fail "no release tag: got '$got', want 'dev-g$sha'"

# (e) not a git checkout at all. GIT_CEILING_DIRECTORIES is not decoration:
# make exports TMPDIR into the checkout (.cache/local/tmp, from makes'
# local.mk), so on a provisioned dev machine this mktemp directory is INSIDE
# the jolt repo — git discovery walks up out of it and version.sh answers the
# repo's own version, which is the right answer to a question this case did
# not mean to ask. The ceiling stops the walk at $tmp, so the case asks about
# no repo wherever mktemp puts it. CI never saw this: it runs `make CHEZ=...`,
# which skips provisioning and leaves TMPDIR alone.
mkdir -p "$tmp/plain"
got="$(GIT_CEILING_DIRECTORIES="$tmp" "$script" "$tmp/plain")"
[ "$got" = "dev" ] || fail "outside git: got '$got', want 'dev'"

# (f) every consumer goes through the script; none re-derives it inline
for f in bin/jolt host/chez/build-jolt.ss .github/workflows/release.yml; do
  grep -q 'tools/version.sh' "$root/$f" || fail "$f does not use tools/version.sh"
  if grep -n 'describe --' "$root/$f"; then
    fail "$f runs git describe itself; use tools/version.sh"
  fi
done

echo "version-smoke: ok (release tags only; vnightly at HEAD reads v0.1.0-1-g$sha)"
