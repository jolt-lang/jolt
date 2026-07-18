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

# --- jolt.host surface manifest ---------------------------------------------
# host/chez/jolt-host-manifest.txt is the canonical set of names bound into the
# jolt.host namespace. The compiler and the clojure.core overlay reach these by
# late-bound var-deref, so a typo or a removed def surfaces only as a runtime
# unbound-var — invisible to mint/selfcheck/load. Pin the manifest against the
# def-var! sites (an added/removed host fn must update the manifest) and against
# every jolt.host reference in jolt-core (a reference to a name not in the
# manifest would be a runtime unbound-var).
grep -vE '^[[:space:]]*(#|$)' host/chez/jolt-host-manifest.txt | LC_ALL=C sort -u > "$tmp/hostman"
grep -rhoE --include='*.ss' 'def-var! "jolt.host" "[^"]+"' host/ \
  | sed -E 's/.*"jolt.host" "([^"]+)".*/\1/' | LC_ALL=C sort -u > "$tmp/hostdef"
if ! diff -u "$tmp/hostman" "$tmp/hostdef" > "$tmp/hd"; then
  echo "  FAIL: jolt.host def-var! sites != jolt-host-manifest.txt"
  echo "        (< manifest, > actual def-var! sites; regenerate the manifest)"
  sed 's/^/    /' "$tmp/hd"
  fail=1
fi
# references from jolt-core: qualified jolt.host/NAME plus names pulled in via
# [jolt.host :refer [...]]. Strip Clojure line comments first so a `; jolt.host/x`
# mention (e.g. a prose reference to the class-* seams) isn't counted.
{
  find jolt-core -name '*.clj' -exec sed 's/;.*//' {} + \
    | grep -oE 'jolt\.host/[a-zA-Z][a-zA-Z0-9!?*<>=+.-]*' | sed 's|jolt\.host/||'
  reffiles=$(grep -rl '\[jolt.host :refer' jolt-core 2>/dev/null)
  [ -n "$reffiles" ] && perl -0777 -ne 'while (/\[jolt\.host\s+:refer\s+\[(.*?)\]/gs){ print "$1\n" }' $reffiles \
    | tr -s ' \t\n' '\n' | grep -E '^[a-zA-Z]'
} | LC_ALL=C sort -u > "$tmp/hostref"
missing=$(LC_ALL=C comm -23 "$tmp/hostref" "$tmp/hostman")
if [ -n "$missing" ]; then
  echo "  FAIL: jolt-core references jolt.host names absent from the manifest:"
  echo "$missing" | sed 's/^/    /'
  fail=1
fi

# --- host-contract primitive declares vs backend native-ops -----------------
# host-contract.ss declares the hot clojure.core primitives so the analyzer's
# resolve-global classifies them (the emitter lowers each inline, so the declared
# cell's unbound root is never deref'd). The set must mirror backend_scheme.clj's
# native-ops — op-registry entries with a :call — minus the internal
# protocol-dispatch{1,2,3} emit helpers, which are not clojure.core names. The two
# live in different layers (.ss vs .clj) and can't derive from each other in code,
# so this pins them: a new native op that isn't declared (or vice versa) fails here.
sed -n '/^(def ^:private op-registry/,/^;; Derived accessor tables/p' jolt-core/jolt/backend_scheme.clj \
  | grep -E '^[[:space:]]*\{?"[^"]+"[[:space:]]+\{.*:call' \
  | grep -oE '^[[:space:]]*\{?"[^"]+"' | grep -oE '"[^"]+"' | tr -d '"' \
  | grep -vxE 'protocol-dispatch[123]' | LC_ALL=C sort -u > "$tmp/native"
sed -n '/declare the hot clojure.core primitives/,/^;; --- install/p' host/chez/host-contract.ss \
  | grep -v 'declare-var!' | grep -oE '"[^"]+"' | tr -d '"' | LC_ALL=C sort -u > "$tmp/declare"
if ! diff -u "$tmp/native" "$tmp/declare" > "$tmp/nd"; then
  echo "  FAIL: host-contract.ss primitive declares != backend native-ops"
  echo "        (< native-ops keys, > host-contract declares)"
  sed 's/^/    /' "$tmp/nd"
  fail=1
fi

[ "$fail" = 0 ] && echo "manifest check: passed" || echo "manifest check: FAILED"
exit $fail
