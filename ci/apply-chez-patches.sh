#!/bin/sh
# Apply jolt's Chez Scheme patches (host/chez/chez-patches/*.patch, in name
# order) to a cisco/ChezScheme source tree before it is configured:
#
#   sh ci/apply-chez-patches.sh <chez-src>
#
# Every workflow that builds Chez from source runs this right after the clone,
# and keys its Chez cache on the patches' hash, so a cached kernel is always
# the patched one. Idempotent: a patch that is already applied is skipped, so a
# tree restored from a cache or patched by an earlier step is left alone.
#
# The patches change the C kernel and Scheme sources (s/7.ss, s/inspect.ss,
# s/vfasl.ss, s/arm64.ss, s/x86_64.ss), so the kernel and boot files must come
# from the same patched build, which configure + make + install from one tree
# guarantees. A jolt built against a stock Chez (Homebrew, a distro) still
# works; it just pays the stock price for collect-safe foreign calls (see the
# comments in c/thread.c), keeps a copy of each string literal per form, and
# keeps stock Chez's placement of C arguments past the registers.
set -eu

src=${1:?usage: apply-chez-patches.sh <chez-src>}
here=$(cd "$(dirname "$0")/.." && pwd)
[ -f "$src/c/thread.c" ] || { echo "apply-chez-patches: $src is not a Chez source tree" >&2; exit 2; }

# git apply where there is git, and the tree is its own repository or in none
# (it works outside a repository too, so a release tarball is fine). Inside
# ANOTHER repository it would apply relative to that one's root and skip
# every file, so there, and where there is no git, patch(1).
use_git=
if command -v git >/dev/null 2>&1; then
  top=$(git -C "$src" rev-parse --show-toplevel 2>/dev/null || true)
  if [ -z "$top" ] || [ "$(cd "$top" && pwd -P)" = "$(cd "$src" && pwd -P)" ]; then
    use_git=1
  fi
fi
if [ -n "$use_git" ]; then
  applied() { git -C "$src" apply --reverse --check "$1" >/dev/null 2>&1; }
  apply() { git -C "$src" apply "$1"; }
else
  # -N refuses an already-applied hunk instead of offering to reverse it, and
  # -f keeps patch from asking; a reverse dry run alone "succeeds" on an
  # unpatched tree by guessing the patch is reversed.
  applied() {
    ! patch -d "$src" -p1 -N -f --dry-run -s < "$1" >/dev/null 2>&1 &&
      patch -d "$src" -p1 -R -f --dry-run -s < "$1" >/dev/null 2>&1
  }
  apply() { patch -d "$src" -p1 -N -f -s < "$1"; }
fi

for p in "$here"/host/chez/chez-patches/*.patch; do
  [ -f "$p" ] || continue
  name=$(basename "$p")
  # A CRLF checkout (Windows, without the -text attribute in .gitattributes)
  # turns every line into a mismatch against Chez's LF sources.
  if ! tr -d '\r' < "$p" | cmp -s - "$p"; then
    echo "apply-chez-patches: $name has CRLF line endings; it must be checked out as written (.gitattributes)" >&2
    exit 1
  fi
  if applied "$p"; then
    echo "apply-chez-patches: $name already applied"
  else
    apply "$p"
    echo "apply-chez-patches: applied $name"
  fi
done
