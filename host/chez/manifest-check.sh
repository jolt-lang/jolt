#!/bin/sh
# manifest-check.sh — pin the runtime load-manifest drift.
#
# build.ss's bld-runtime-manifest is the data source of truth for the runtime
# load skeleton; build-joltc.ss already consumes it directly. Two consumers still
# hand-mirror it: cli.ss (the live runtime entry — literal (load ...) forms) and
# bootstrap.ss (the seed rebuilder's reduced set). cli.ss can't iterate a data
# manifest instead — loading the runtime through a for-each loop changes the
# visibility of the loaded files' top-level defines (verified: a loop-loaded
# runtime fails to compile), so the literal mirror stays and this guard makes any
# drift between it and the manifest fail loudly. bootstrap.ss takes its seed
# prelude/image as CLI args and needs no png/loader/ffi, so its fixed loads are
# asserted to be a subset of the manifest.
root="$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)"
cd "$root"
fail=0
tmp="$(mktemp -d)"; trap 'rm -rf "$tmp"' EXIT

# cli.ss's runtime loads, in order: the block from the first rt.ss load to the
# shared entry tail (cli-tail.ss — source roots + jolt-cli-run; it loads
# build.ss later, conditionally, which is not part of the runtime skeleton).
sed -n '/(load "host\/chez\/rt.ss")/,/(load "host\/chez\/cli-tail.ss")/p' host/chez/cli.ss \
  | grep -oE 'host/chez/[a-zA-Z0-9_/.-]+\.ss' \
  | grep -v '^host/chez/cli-tail\.ss$' > "$tmp/cli"

# build.ss's bld-runtime-manifest loads, in order, with the three tags resolved
# to the concrete seed/compile-eval loads they stand for (via bld-tagged-loads).
sed -n '/define bld-runtime-manifest/,/define bld-tagged-loads/p' host/chez/build.ss \
  | sed -e "s/'prelude/host\/chez\/seed\/prelude.ss/" \
        -e "s/'image/host\/chez\/seed\/image.ss/" \
        -e "s/'compile-eval/host\/chez\/compile-eval.ss/" \
  | grep -oE 'host/chez/[a-zA-Z0-9_/.-]+\.ss' > "$tmp/manifest"

# cli.ss must match the manifest exactly (same loads, same order).
if ! diff -u "$tmp/cli" "$tmp/manifest" > "$tmp/diff"; then
  echo "  FAIL: cli.ss runtime loads != bld-runtime-manifest"
  sed 's/^/    /' "$tmp/diff"
  fail=1
fi

# bootstrap.ss: a reduced set. Its prelude/image come from CLI args (bs-seed-*),
# so each of its FIXED loads must appear in the manifest, except emit-image.ss
# (bootstrap-only — it rebuilds the image from source, not a runtime load).
# Match only literal (load "...") forms so the script's own name in its usage
# comment isn't mistaken for a load.
grep -oE '\(load "host/chez/[a-zA-Z0-9_/.-]+\.ss"' host/chez/bootstrap.ss \
  | grep -oE 'host/chez/[a-zA-Z0-9_/.-]+\.ss' | sort -u > "$tmp/boot"
while read -r p; do
  case "$p" in
    host/chez/emit-image.ss) ;;   # bootstrap-only (rebuilds the seed image)
    *) if ! grep -qxF "$p" "$tmp/manifest"; then
         echo "  FAIL: bootstrap.ss load $p not in bld-runtime-manifest"; fail=1
       fi ;;
  esac
done < "$tmp/boot"

[ "$fail" = 0 ] && echo "manifest check: passed" || echo "manifest check: FAILED"
exit $fail
