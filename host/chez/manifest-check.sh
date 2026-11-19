#!/bin/sh
# manifest-check.sh — pin the runtime load-manifest drift.
#
# build.ss's bld-runtime-manifest is the data source of truth for the runtime
# load skeleton; build-jolt.ss already consumes it directly. Two consumers still
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

# cli.ss's runtime loads, in order: the block from the first runtime load
# (scheme-adapter-runtime.ss — PSL R5+R6 made the adapter load FIRST, before
# rt.ss, because rt.ss's jolt-foreign-proc-safe macro resolves sa-os-family at
# expansion time) to the shared entry tail (cli-tail.ss — source roots +
# jolt-cli-run; it loads build.ss later, conditionally, which is not part of
# the runtime skeleton).
sed -n '/(load "host\/chez\/scheme-adapter-runtime.ss")/,/(load "host\/chez\/cli-tail.ss")/p' host/chez/cli.ss \
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

# gate-boot.ss: the gate scripts' boot preamble, and the prefix make-gateboot.ss
# compiles into target/dev/gate.so. It is a THIRD hand-mirror of the manifest —
# literal for the same reason cli.ss's loads are — so pin it as an exact prefix.
# The gates stop before cli-core.ss deliberately (loader.ss would turn their
# alias-only `require` into real file loading), so compare against the manifest
# truncated at compile-eval.ss.
sed -n '/(load "host\/chez\/scheme-adapter-runtime.ss")/,/(load "host\/chez\/compile-eval.ss")/p' host/chez/gate-boot.ss \
  | grep -oE 'host/chez/[a-zA-Z0-9_/.-]+\.ss' > "$tmp/gate"
sed -n '1,/^host\/chez\/compile-eval\.ss$/p' "$tmp/manifest" > "$tmp/manifest-prefix"
if ! diff -u "$tmp/gate" "$tmp/manifest-prefix" > "$tmp/gdiff"; then
  echo "  FAIL: gate-boot.ss loads != bld-runtime-manifest prefix through compile-eval.ss"
  sed 's/^/    /' "$tmp/gdiff"
  fail=1
fi

# The remaining prologue sites. cli.ss / gate-boot.ss / bootstrap.ss are pinned
# above; make-gateboot.ss, make-devboot.ss, and build-boot.ss (build-jolt's
# preamble, for its own process and the stdlib compile it spawns) hand-mirror the
# same skeleton. The drg-2767 failure mode: a new post-prelude-*.ss added to the
# manifest and to the pinned sites but forgotten in one of these — silent,
# perf-only (the shim never loads). So: every site must load exactly the
# manifest's post-prelude set, and the two sites that mirror cli's skeleton
# (make-devboot, build-boot) must match the manifest prefix through
# compile-eval.ss exactly, like gate-boot.ss.
manifest_pp="$(grep -oE 'post-prelude[a-z-]*\.ss' "$tmp/manifest" | LC_ALL=C sort -u)"
for site in host/chez/make-gateboot.ss host/chez/make-devboot.ss host/chez/build-boot.ss; do
  site_pp="$(grep -oE 'post-prelude[a-z-]*\.ss' "$site" | LC_ALL=C sort -u)"
  if [ "$site_pp" != "$manifest_pp" ]; then
    echo "  FAIL: $site post-prelude loads != bld-runtime-manifest post-prelude set"
    echo "    manifest: $(echo $manifest_pp)"
    echo "    site:     $(echo $site_pp)"
    fail=1
  fi
done
# build-jolt boots through build-boot.ss, in its own process and in the stdlib
# compile script it writes; a literal runtime load creeping back into it would be
# a fourth, unpinned mirror.
if grep -qE 'host/chez/(scheme-adapter-runtime|rt|compile-eval|seed/prelude|seed/image|post-prelude[a-z-]*)\.ss' host/chez/build-jolt.ss; then
  echo "  FAIL: build-jolt.ss loads the runtime itself; it boots through build-boot.ss"
  fail=1
fi
for site in host/chez/make-devboot.ss host/chez/build-boot.ss; do
  sed -n '/(load "host\/chez\/scheme-adapter-runtime.ss")/,/(load "host\/chez\/compile-eval.ss")/p' "$site" \
    | grep -oE 'host/chez/[a-zA-Z0-9_/.-]+\.ss' > "$tmp/siteprefix"
  if ! diff -u "$tmp/manifest-prefix" "$tmp/siteprefix" > "$tmp/sdiff" 2>&1; then
    echo "  FAIL: $site runtime loads != bld-runtime-manifest prefix through compile-eval.ss"
    sed 's/^/    /' "$tmp/sdiff"
    fail=1
  fi
done

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
# The :refer half below reads multi-line vectors with perl; without it that half
# would come back empty and the check would pass over every referred name.
command -v perl >/dev/null 2>&1 || { echo "  FAIL: manifest check needs perl to read [jolt.host :refer [...]] vectors"; exit 1; }
# references from jolt-core: qualified jolt.host/NAME plus names pulled in via
# [jolt.host :refer [...]], less jolt.host/load-time-value (a special form the
# analyzer dispatches on that spelling, not a var). Strip Clojure line comments
# first so a `; jolt.host/x` mention (e.g. a prose reference to the class-*
# seams) isn't counted.
{
  find jolt-core -name '*.clj' -exec sed 's/;.*//' {} + \
    | grep -oE 'jolt\.host/[a-zA-Z][a-zA-Z0-9!?*<>=+.-]*' | sed 's|jolt\.host/||'
  reffiles=$(grep -rl '\[jolt.host :refer' jolt-core 2>/dev/null)
  [ -n "$reffiles" ] && perl -0777 -ne 'while (/\[jolt\.host\s+:refer\s+\[(.*?)\]/gs){ print "$1\n" }' $reffiles \
    | tr -s ' \t\n' '\n' | grep -E '^[a-zA-Z]'
} | grep -vxF load-time-value | LC_ALL=C sort -u > "$tmp/hostref"
missing=$(LC_ALL=C comm -23 "$tmp/hostref" "$tmp/hostman")
if [ -n "$missing" ]; then
  echo "  FAIL: jolt-core references jolt.host names absent from the manifest:"
  echo "$missing" | sed 's/^/    /'
  fail=1
fi

# ...and that those references actually compiled as late-bound var reads. A
# qualified ns/name the compiling runtime has not loaded yet is not late-bound:
# resolve-global cannot see the var, so the analyzer reads it as a CLASS static
# (analyze-symbol's non-var arm), and the emitted host-static-call raises on
# every call, since no such class exists and the static registry never consults
# a namespace. In the seed that raise lands inside the emitted guard, so the def
# it sat in silently never ran (jolt#879). Nothing else emits a lowercase dotted
# class name -- hc-fq-class-name? requires a capitalized final segment -- so any
# in the minted seed is exactly this mistake. The fix is to define the host fn
# before the load point that reads it, not to move the reference.
bad_static=$(grep -ohE 'host-static-(call|ref) "[a-z][a-z0-9]*(\.[a-z0-9-]+)+" "[^"]+"' \
  host/chez/seed/image.ss host/chez/seed/prelude.ss host/gambit/seed/image.ss 2>/dev/null \
  | LC_ALL=C sort -u)
if [ -n "$bad_static" ]; then
  echo "  FAIL: the seed reads a namespace-qualified var as a class static —"
  echo "        it was not loaded when the mint compiled the reference:"
  echo "$bad_static" | sed 's/^/    /'
  fail=1
fi

# --- host-contract primitive declares vs backend native-ops -----------------
# host-contract.ss declares the hot clojure.core primitives so the analyzer's
# resolve-global classifies them (the emitter lowers each inline, so the declared
# cell's unbound root is never deref'd). The set must mirror jolt.op-registry's
# native-ops — op-registry entries with a :call — minus the internal
# protocol-dispatch{1,2,3} emit helpers, which are not clojure.core names. The two
# live in different layers (.ss vs .clj) and can't derive from each other in code,
# so this pins them: a new native op that isn't declared (or vice versa) fails here.
sed -n '/^(def op-registry/,/^;; --- derived accessor tables/p' jolt-core/jolt/op_registry.clj \
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
