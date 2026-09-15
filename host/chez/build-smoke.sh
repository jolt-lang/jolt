#!/bin/sh
# build smoke: `jolt build` compiles a multi-namespace app (macro + cross-ns +
# clojure.string) into a standalone binary, which then runs with no jolt source
# or Chez install on the path — args reach -main, output matches.
root="$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)"
cd "$root"

# JOLT_BIN overrides the jolt under test. The gate targets point it at the
# freshly built target/release/jolt: a `jolt build` costs ~2.5s through the
# prebuilt binary and ~12.5s through the source-mode driver, and this gate
# drives 26 of them. JOLT_BIN=bin/jolt forces script mode.
jolt="${JOLT_BIN:-bin/jolt}"
# Absolute form, for the cases that cd into a fixture directory first.
case "$jolt" in /*) joltabs="$jolt" ;; *) joltabs="$root/$jolt" ;; esac

# Preflight: a standalone build needs Chez's kernel dev files (libkernel.a +
# scheme.h) and a C compiler. A distro chezscheme package ships neither, so on
# such hosts (CI included) skip — like `certify` skips without Clojure. Pin the
# csv dir we validate so the build uses exactly it.
csv="$JOLT_CHEZ_CSV"
if [ -z "$csv" ]; then
  # JOLT_CHEZ wins (see host/chez/selfcheck.sh) — else this can pair a
  # PATH-resolved Chez's csv dir with a running interpreter built elsewhere.
  chez_bin="${JOLT_CHEZ:-$(command -v chez || command -v chezscheme || command -v scheme || command -v petite || true)}"
  if [ -n "$chez_bin" ]; then
    base="$(cd "$(dirname "$chez_bin")/.." 2>/dev/null && pwd)"
    for d in "$base"/lib/csv*/*/; do
      [ -f "${d}libkernel.a" ] && csv="${d%/}" && break
    done
  fi
fi
if ! command -v cc >/dev/null 2>&1 || [ -z "$csv" ] || [ ! -f "$csv/scheme.h" ] || [ ! -f "$csv/libkernel.a" ]; then
  echo "build smoke: skipped (Chez kernel dev files or C compiler not available)"
  exit 0
fi
export JOLT_CHEZ_CSV="$csv"

app="$root/test/chez/build-app"
out="$(mktemp -d)/app-bin"
trap 'rm -rf "$(dirname "$out")"' EXIT

echo "build smoke: compiling app.core -> $out"
if ! JOLT_PWD="$app" "$jolt" build -m app.core -o "$out" >/dev/null 2>&1; then
  echo "  FAIL: jolt build exited non-zero"
  exit 1
fi
[ -x "$out" ] || { echo "  FAIL: no executable produced"; exit 1; }

# Run from a neutral cwd with args. The first line is an embedded resource
# (deps.edn :jolt/build :embed), proving io/resource resolves from the binary with
# no resources/ dir on disk; the rest exercise a macro, cross-ns, and args.
got="$(cd / && "$out" alpha bb ccc 2>&1)"
want='embedded resource ok
HELLO FROM A BUILT BINARY!
HELLO FROM A BUILT BINARY!
args: [alpha bb ccc]
sum: 10
greet-default: greet:default
greet-loud: greet:loud
greet-soft: greet:soft
boot-threads: :ran :ran'
if [ "$got" != "$want" ]; then
  echo "  FAIL: binary output mismatch"
  echo "--- want ---"; echo "$want"
  echo "--- got ----"; echo "$got"
  exit 1
fi

# The compiler verdict is taken by EVERY build now, not only a shaken one
# (dce-needs-compiler?): this app reads images back (jolt.image/read-image
# restores fn sources by compiling them), so its default build keeps the
# analyzer/back end resident. The negative half — a program that reaches none
# of that ships without them — is asserted on the data-reader app below.
if ! grep -Eq 'def-var[a-z!-]*! "jolt.analyzer"' "$out.build/runtime.ss"; then
  echo "  FAIL: the default build of an image-reading app dropped the compiler"; exit 1
fi

# Startup profiling is one opt-in switch on the same binary. The ordinary run
# above is exact-output proof that it stays silent by default; an enabled run
# spans the native heap loader, app namespace initialization, and -main.
profiled="$(cd / && JOLT_STARTUP_PROFILE=1 "$out" alpha bb ccc 2>&1)"
for marker in \
  'jolt startup: [profile] native pre-main (exec+ld)' \
  'jolt startup: [profile] native Sbuild_heap' \
  'jolt startup: [profile] scheme namespace app.core' \
  'jolt startup: [profile] scheme entry -main'
do
  if ! printf '%s\n' "$profiled" | grep -Fq "$marker"; then
    echo "  FAIL: startup profile missing marker: $marker"
    echo "--- got ----"; echo "$profiled"
    exit 1
  fi
done

# --- release now defaults to direct-linking + whole-program inference ------------
# A plain `jolt build` (release, no flags) must direct-link app->app calls (the
# throughput lever the perf audit identified) AND run wp-infer — both were opt-in
# (--direct-link / --opt) before. $out is still the plain release build here.

# The cross-ns app.core -> app.util/shout reference is direct-linked in the plain
# release build, not var-routed.
#
# Asserted on the BINDING, not on a (jv$app.util$shout ...) call form. Release
# inlines now (jolt-mbcm.6), and shout is small enough to be spliced into its
# caller, so there is no call left to find -- the old assertion failed on a build
# that had done MORE than it asked for. The binding is emitted for every
# direct-linked def whether or not any particular call to it survives, and it is
# absent entirely under --no-direct-link, so it still discriminates.
if ! grep -q 'define jv\$app.util\$shout' "$out.build/flat.ss"; then
  echo "  FAIL: release build did not direct-link the app->app call"; exit 1
fi
# ...and nothing reads it through its var, which is the thing direct-linking is
# for. This holds whether the call was spliced or left as a jv$ application.
if grep -q '(jolt-var "app.util" "shout")\|(var-deref "app.util" "shout")' "$out.build/flat.ss"; then
  echo "  FAIL: release build still var-routed the app->app call"; exit 1
fi

# wp-infer ran: a hintless double fn (app.util/area, called with 2.0) gets its
# param seeded :double, so its * lowers to a flonum op. The same build with
# JOLT_NO_WP_INFER=1 skips the fixpoint — the fl-op count must drop (area is the
# delta). Same numeric result either way; this is the emit-level proof it ran.
if ! JOLT_PWD="$app" JOLT_NO_WP_INFER=1 "$jolt" build -m app.core -o "$out.noop" >/dev/null 2>&1; then
  echo "  FAIL: JOLT_NO_WP_INFER build exited non-zero"; exit 1
fi
default_fl=$(grep -c '#3%fl' "$out.build/flat.ss" || true)
noop_fl=$(grep -c '#3%fl' "$out.noop.build/flat.ss" || true)
if [ "$default_fl" -le "$noop_fl" ]; then
  echo "  FAIL: wp-infer added no fl-ops to the release build (default=$default_fl noop=$noop_fl)"; exit 1
fi

# The :str stamp on interop targets: app.util/strd-prefix's (.startsWith (str x) …)
# is unhinted — the target types :str from the str-ret table (per-form inference,
# no fixpoint needed), so flat.ss carries the inline native and NO
# record-method-dispatch "startsWith" anywhere. Runtime shape is asserted below
# via --strd; this is the emit-level proof.
if ! grep -q 'str-starts-with?' "$out.build/flat.ss"; then
  echo "  FAIL: str-target .startsWith did not lower to the string native"; exit 1
fi
if grep -q 'record-method-dispatch.*startsWith' "$out.build/flat.ss"; then
  echo "  FAIL: str-target .startsWith still routes through record-method-dispatch"; exit 1
fi
if ! grep -q 'str-starts-with?' "$out.noop.build/flat.ss"; then
  echo "  FAIL: str-target lowering depended on the wp fixpoint (str-ret table is per-form)"; exit 1
fi

# The :kw stamp on interop targets: app.util/kwsym's (.sym k) is proven a keyword
# by the ^clojure.lang.Keyword param hint (honeysql's kw->sym shape), so flat.ss
# must carry the inline keyword arm for kwsym's param and NO record-method-dispatch
# "sym". The negative grep anchors "sym" right after the target so clojure.pprint's
# record-method-dispatch this "-fields" lines (which merely contain a symbol named
# sym elsewhere) don't false-positive; the positive one matches kwsym's exact
# emission (the bare (jolt-symbol (keyword-t-ns …)) shape also appears in the
# runtime section, so it alone would not prove the stamp fired).
if ! grep -qF '(jolt-symbol (keyword-t-ns k) (keyword-t-name k))' "$out.build/flat.ss"; then
  echo "  FAIL: kw-target .sym did not lower to the inline keyword arm"; exit 1
fi
if grep -qE 'record-method-dispatch [^ ()]+"sym"' "$out.build/flat.ss"; then
  echo "  FAIL: kw-target .sym still routes through record-method-dispatch"; exit 1
fi

# The :sb stamp: app.util/sbjoin binds (StringBuilder.) to a let local with NOTHING
# hinted in the source, so this proves init-proves-hint fired and not just the
# explicit-tag path. flat.ss must carry the inline sb-append!/sb-str emission for
# that local and route no "append"/"toString" on it through the jhost method table.
# The negative grep anchors the method name right after the target so unrelated
# record-method-dispatch lines elsewhere in the closure cannot false-positive.
if ! grep -qF '(sb-append! sb (render-piece' "$out.build/flat.ss"; then
  echo "  FAIL: sb-target .append did not lower to the inline sb-append!"; exit 1
fi
if ! grep -qF '(sb-str sb)' "$out.build/flat.ss"; then
  echo "  FAIL: sb-target .toString did not lower to the inline sb-str"; exit 1
fi
if grep -qE 'record-method-dispatch [^ ()]+"append"' "$out.build/flat.ss"; then
  echo "  FAIL: sb-target .append still routes through record-method-dispatch"; exit 1
fi

# Lib-provided host classes: app.util/zdt-class references java.time.ZonedDateTime
# (a jolt-lang/time class). The build scan must pull the provider's install ns —
# src-provider/jolt/time.clj is the on-roots stand-in — into flat.ss, because a
# built binary has no source roots for the runtime class-miss autoload. The
# negative control: src-provider/jolt/crypto.clj is also on the roots, but its
# classes are referenced nowhere, so that provider must stay out. The greps
# target ns EMISSION (set-chez-ns!), not bare strings — the runtime section of
# flat.ss always mentions both providers in its autoload tables.
if ! grep -q 'set-chez-ns! "jolt\.time"' "$out.build/flat.ss"; then
  echo "  FAIL: a jolt.time class ref did not pull the provider ns into flat.ss"; exit 1
fi
if grep -q 'set-chez-ns! "jolt\.crypto"' "$out.build/flat.ss"; then
  echo "  FAIL: unreferenced lib provider jolt.crypto leaked into flat.ss"; exit 1
fi

# Closure identity in a BUILT binary, on the direct-linked release default.
# Chez shares one closure object across every evaluation of a lambda with no free
# variables; Clojure allocates a fresh fn each time, and malli.impl.regex depends
# on the Clojure answer (its parked-continuation cache keys on validator
# closures, so a shared :? epsilon branch collided two states, killed
# backtracking and made m/validate answer false). The interpreter is not enough
# evidence: the release default is --direct-link with whole-program inference,
# which is where a capture the interpreter keeps could still be folded away.
# The last line is the control — a CAPTURING fn always allocated, so a fix that
# only papered over the non-capturing case would still show here.
check_fnid() {  # check_fnid <binary> <label>
  # announces itself: a check that is silent on success cannot be distinguished
  # from one that never ran, and this one was briefly BOTH (defined below its
  # first call site, which sh reports on stderr and then carries on past).
  echo "build smoke: closure identity in $2"
  got_fn="$(cd / && "$1" --fnid 2>&1)"
  for line in 'fnid-same: false' 'fnid-set: 2' 'fnid-meta: [{:t 1} nil]' \
              'fnid-call: 7' 'fnid-cap: false'; do
    if ! printf '%s\n' "$got_fn" | grep -qxF "$line"; then
      echo "  FAIL: closure identity in $2 — missing: $line"
      echo "--- got ----"; echo "$got_fn"; exit 1
    fi
  done
}
# --no-direct-link opts back out of the release default: the app->app call must
# NOT lower to a jv$ binding (stays var-routed, dynamically linked).
if ! JOLT_PWD="$app" "$jolt" build -m app.core -o "$out.nodl" --no-direct-link >/dev/null 2>&1; then
  echo "  FAIL: jolt build --no-direct-link exited non-zero"; exit 1
fi
if grep -q 'define jv\$app.util\$shout' "$out.nodl.build/flat.ss"; then
  echo "  FAIL: --no-direct-link still direct-linked the app->app call"; exit 1
fi
check_fnid "$out.nodl" "the --no-direct-link build"
# and it IS var-routed there -- without this the check above would pass on a
# build that emitted no reference to shout at all.
if ! grep -q '(jolt-var "app.util" "shout")\|(var-deref "app.util" "shout")' "$out.nodl.build/flat.ss"; then
  echo "  FAIL: --no-direct-link did not var-route the app->app call"; exit 1
fi
# An OPEN-WORLD build maps its frames too. emit-def-cached only emits a source
# registration under direct-link, so without one a built binary printed a bare
# `deep-boom` where the direct-linked build printed
# `app.util/deep-boom (…/util.clj:24)`. The build turns source-reg on when it is
# not direct-linking, the way the runtime eval path already does.
nodl_boom="$(cd / && "$out.nodl" --boom 2>&1 >/dev/null)"
for frame in 'app\.util/deep-boom .*util\.clj:[0-9]' 'app\.util/mid-boom .*util\.clj:[0-9]' 'app\.core/-main .*core\.clj:[0-9]'; do
  if ! printf '%s' "$nodl_boom" | grep -Eq "$frame"; then
    echo "  FAIL: --no-direct-link trace missing located frame $frame"
    echo "--- got ----"; echo "$nodl_boom"
    exit 1
  fi
done

# ^:redef / ^:dynamic opt out of direct-linking, so runtime redef/binding still
# take effect in the built binary even with direct-link the release default.
got_rd="$(cd / && "$out" --redef 2>&1)"
if ! printf '%s' "$got_rd" | grep -q '^redef: :patched$'    || ! printf '%s' "$got_rd" | grep -q '^dyn: :bound$'; then
  echo "  FAIL: ^:redef/:dynamic opt-out — want 'redef: :patched' and 'dyn: :bound' lines"
  echo "--- got ----"; echo "$got_rd"; exit 1
fi

# jolt#1009: a PLAIN def (no ^:redef/^:dynamic) is direct-linked, and a root write
# must reach the jv$ binding a compiled value READ goes to — or the var cell and
# the binding split and never rejoin. The first three lines print the compiled
# read and the var-cell read of the SAME var; before the fix the left half stayed
# nil/:original in a built binary while the right half moved, and only in a built
# binary. fn-direct pins the other half of the closed world, unchanged: an
# inlined direct CALL keeps the body it was compiled with, and ^:redef opts out.
got_vr="$(cd / && "$out" --varroot 2>&1)"
# grep per line, not one equality: -main prints its unconditional output around
# these, exactly as the --redef check above works around.
for line in 'same-ns: :set / :set' \
            'cross-ns: :set / :set' \
            'fn-value: :patched / :patched' \
            'fn-direct: :original' \
            'with-redefs: :bound' \
            'after-redefs: :set'; do
  if ! printf '%s\n' "$got_vr" | grep -qxF "$line"; then
    echo "  FAIL: alter-var-root/with-redefs of a plain def — want '$line'"
    echo "--- got ----"; echo "$got_vr"
    exit 1
  fi
done

check_fnid "$out" "the direct-linked release build"

# The heap ceiling, in a BUILT binary. jolt bounds its heap at 25% of RAM by
# default, the share the JVM's MaxRAMPercentage uses, because Chez has no -Xmx
# and an unbounded heap means the kernel kills the process with no diagnostic at
# all. Asserted here and not only under `jolt -e`: the install is emitted into
# the APP launcher (build.ss), a separate site from jolt's own, and only a built
# binary runs it.
echo "build smoke: heap ceiling (default / override / off / OutOfMemoryError)"
heap_max() { cd / && "$out" --heap 2>&1 | sed -n 's/^heap-max: //p'; }
# default: a real ceiling was computed, i.e. RAM detection worked in the binary
hm_def="$(heap_max)"
case "$hm_def" in
  ''|*[!0-9]*) echo "  FAIL: default ceiling not numeric: '$hm_def'"; exit 1 ;;
esac
if [ "$hm_def" = "9223372036854775807" ] || [ "$hm_def" -le 0 ] 2>/dev/null; then
  echo "  FAIL: no default heap ceiling in a built binary (got $hm_def)"; exit 1
fi
# an explicit override is honoured exactly
hm_512="$(JOLT_MAX_HEAP=512m heap_max)"
if [ "$hm_512" != "536870912" ]; then
  echo "  FAIL: JOLT_MAX_HEAP=512m gave $hm_512, want 536870912"; exit 1
fi
# off restores the pre-0.8.5 unbounded contract
hm_off="$(JOLT_MAX_HEAP=off heap_max)"
if [ "$hm_off" != "9223372036854775807" ]; then
  echo "  FAIL: JOLT_MAX_HEAP=off gave $hm_off, want Long/MAX_VALUE"; exit 1
fi
# and exceeding one is an error the program can catch, not a SIGKILL. 256m, not
# something smaller: a built binary's own baseline live heap is ~83MB here, and a
# ceiling under that cannot be satisfied at all (checked separately below).
hm_oom="$(cd / && JOLT_MAX_HEAP=256m "$out" --heap-oom 2>&1 | sed -n 's/^heap-oom: //p')"
if [ "$hm_oom" != ":caught-oom" ]; then
  echo "  FAIL: exceeding a 256m ceiling gave '$hm_oom', want :caught-oom"; exit 1
fi
# a ceiling below the runtime's own live heap is rejected AS a bad setting, named
# as such, rather than failing somewhere inside namespace initialization
hm_low="$(cd / && JOLT_MAX_HEAP=16m "$out" --heap 2>&1 | head -2)"
case "$hm_low" in
  *"smaller than the runtime's own live heap"*) : ;;
  *) echo "  FAIL: a 16m ceiling should be refused with a clear message, got: $hm_low"; exit 1 ;;
esac

# A NAMED inner fn inside a spliced callee (jolt-pzos). Two claims:
#  - the alpha-rename the splicer applies for hygiene (step-boom -> step-boom__ilN)
#    is a compiler artifact and must not reach the user;
#  - app.util/inner-boom appears ONCE. The inner fn is emitted as its own lambda
#    and has its own runtime frame, so stamping the inline chain through the fn
#    boundary made the reporter expand that frame as spliced code as well, and the
#    callee was printed twice — once for the inner fn's frame, once for its own.
got_if="$(cd / && "$out" --innerfn 2>&1)"
if printf '%s' "$got_if" | grep -q '__il'; then
  echo "  FAIL: inner-fn trace leaked the __ilN alpha-rename artifact"
  echo "--- got ----"; echo "$got_if"; exit 1
fi
if ! printf '%s' "$got_if" | grep -q 'step-boom'; then
  echo "  FAIL: inner-fn trace lost the inner fn's frame (want a step-boom frame)"
  echo "--- got ----"; echo "$got_if"; exit 1
fi
n_inner="$(printf '%s\n' "$got_if" | grep -c 'app\.util/inner-boom')"
if [ "$n_inner" != "1" ]; then
  echo "  FAIL: inner-fn trace names app.util/inner-boom $n_inner times, want 1"
  echo "--- got ----"; echo "$got_if"; exit 1
fi

# --tree-shake must not cost the trace its inlined frames. A callee whose every
# call site was spliced has no reference left in the graph, so the shake dropped
# its def -- and with it the (jolt-register-source! ...) that def's record
# carries, which is the only thing mapping an inlined frame back to ns/name
# (file:line). The shaken binary printed ONE frame where this same build prints
# three (jolt-o13s). Same assertion as the plain release build above, so the two
# cannot drift.
if ! JOLT_PWD="$app" "$jolt" build -m app.core -o "$out.ts" --tree-shake >/dev/null 2>&1; then
  echo "  FAIL: jolt build --tree-shake exited non-zero"; exit 1
fi
got_ts="$(cd / && "$out.ts" --boom 2>&1)"
for frame in 'app\.util/deep-boom .*util\.clj:[0-9]' 'app\.util/mid-boom .*util\.clj:[0-9]' 'app\.core/-main .*core\.clj:[0-9]'; do
  if ! printf '%s' "$got_ts" | grep -qE "$frame"; then
    echo "  FAIL: --tree-shake trace missing inlined frame $frame"
    echo "--- got ----"; echo "$got_ts"; exit 1
  fi
done
# ...and in that order, innermost first — a backwards chain contains every frame
# and would pass the per-frame loop above.
if ! printf '%s' "$got_ts" | tr '\n' '%' | grep -qE 'deep-boom[^%]*%[^%]*mid-boom[^%]*%[^%]*-main'; then
  echo "  FAIL: --tree-shake trace frames out of order"
  echo "--- got ----"; echo "$got_ts"; exit 1
fi

# A var defined TWICE must answer the same through every call path, and the same
# as `jolt run` (jolt-rtjm). app.util defines dd-target, then dd-caller which
# calls it, then dd-target again: the stashed inline body froze the FIRST
# definition into dd-caller, while dd-late — compiled after the second def —
# spliced the second. One binary, two answers. `apply` is in there because a
# direct (dd-caller) can fold at its own call site and mask the frozen body.
got_dd="$(cd / && "$out" --doubledef 2>&1)"
for line in 'dd-apply: second' 'dd-call:  second' 'dd-late:  second'; do
  if ! printf '%s' "$got_dd" | grep -qF "$line"; then
    echo "  FAIL: double-def — want '$line' (a stashed body froze the first def)"
    echo "--- got ----"; echo "$got_dd"; exit 1
  fi
done

# A symbol compiled BEFORE a same-ns redefinition must resolve to the
# clojure.core var in the built binary, exactly as under `jolt run` (the
# in-order load) and on the JVM. The emit walk re-analyzes app source against
# the fully-loaded process — where app.util/get already exists — and resolving
# that ns-local redef made (fwd-get m "K") call the http helper on a map
# backwards: (assoc "K" :url m ...) — "class java.lang.String cannot be cast to
# class clojure.lang.Associative" in the compiled binary, fine under `jolt run`
# (kmet's proxy code is this exact shape; jolt-lang/jolt#451). fwd-first pins
# the same for a second core fn, and fwd-late pins the complementary half: a
# caller AFTER the redefs must get the ns-local fns in BOTH modes.
got_fwd="$(cd / && "$out" --fwdref 2>&1)"
want_fwd="$(cd "$app" && JOLT_PWD="$app" "$joltabs" run -m app.core --fwdref 2>&1)"
if [ "$got_fwd" != "$want_fwd" ]; then
  echo "  FAIL: forward-reference resolution diverges between the binary and jolt run"
  echo "--- binary ----"; echo "$got_fwd"
  echo "--- jolt run --"; echo "$want_fwd"; exit 1
fi
for line in 'fwd-get:   41' 'fwd-first: 7' 'fwd-late:  [{K 5, :url K, :method :get} {:req K, :seen-first true}]'; do
  if ! printf '%s' "$got_fwd" | grep -qF "$line"; then
    echo "  FAIL: forward-ref — want '$line'"
    echo "--- got ----"; echo "$got_fwd"; exit 1
  fi
done

# ...and the same with a WARM AOT cache, which is how a user meets this: the
# cache is on by default in a built jolt, and the report that opened this said
# "jolt run works fine once aot kicks in". A cached namespace loads from its
# compiled artifact, and those defs run outside the reader walk that stamps the
# def ordinals — so pass 1 would hand the emit walk an unstamped program, every
# var would read as visible from form 0, and the binary would resolve the
# ns-local redefinition again. Pass 1 loading from SOURCE is what keeps the
# stamps (ldr-source-only? gates the cache branch, loader.ss); nothing else in
# this gate builds an app whose cache a run has already warmed, so without this
# case that gate could be removed and every check above would still pass.
# Its own cache dir (under the temp dir the trap removes) so the gate neither
# reads nor writes the user's ~/.jolt cache.
fwd_cache="$(dirname "$out")/aot-cache"
warm_out="$(dirname "$out")/app-warm"
(cd "$app" && JOLT_PWD="$app" JOLT_AOT_CACHE=1 JOLT_CACHE_DIR="$fwd_cache" \
   "$joltabs" run -m app.core --fwdref >/dev/null 2>&1)
# ...and the run has to have actually cached something, or this case proves
# nothing while still passing — the failure mode a warm-cache gate is most
# likely to rot into.
if ! ls "$fwd_cache"/*/*/app.util-*.so >/dev/null 2>&1; then
  echo "  FAIL: the warm-cache case is vacuous — no AOT artifact for app.util under $fwd_cache"
  exit 1
fi
if ! JOLT_PWD="$app" JOLT_AOT_CACHE=1 JOLT_CACHE_DIR="$fwd_cache" \
     "$jolt" build -m app.core -o "$warm_out" >/dev/null 2>&1; then
  echo "  FAIL: jolt build over a warm AOT cache exited non-zero"
  exit 1
fi
got_warm="$(cd / && "$warm_out" --fwdref 2>&1)"
if [ "$got_warm" != "$want_fwd" ]; then
  echo "  FAIL: a build over a warm AOT cache resolves forward references differently"
  echo "--- warm binary ----"; echo "$got_warm"
  echo "--- jolt run -------"; echo "$want_fwd"; exit 1
fi

# The same in-order rule across a (load) INSIDE a namespace — the multi-file
# namespace shape (clojure.core's own (load "core_deftype"), a library split
# over foo.clj + foo_impl.clj). The loaded file's defs land in the enclosing
# namespace at the ordinal of the form that loaded it, so a reference ABOVE the
# (load) still belongs to clojure.core and one below gets the ns-local name.
# The build emits the enclosing file and leaves the (load) to run in the binary,
# so pass 1 stamped the loaded file's defs under the loaded file alone and the
# enclosing file's own forward references were ungated: this app built the same
# ClassCastException shape as #451 while `jolt run` was fine. Its own fixture —
# the shared build-app is built ~30 times here, and a top-level (load) changes
# what every one of those emits.
echo "build smoke: forward reference across a (load) in the same namespace"
mfn_app="$(mktemp -d)/mfn-app"
mkdir -p "$mfn_app/src/mf"
printf '{:paths ["src"]}\n' > "$mfn_app/deps.edn"
cat > "$mfn_app/src/mf/util.clj" <<'MFN_EOF'
(ns mf.util)

;; compiled BEFORE the (load) below, whose file redefines `second`
(defn fwd-second [coll] (second coll))

(load "util_extra")

;; ...and after it: the ns-local redefinition and the loaded helper both win
(defn after-load [req] [(second req) (extra-helper 4)])
MFN_EOF
cat > "$mfn_app/src/mf/util_extra.clj" <<'MFN_EOF'
(in-ns 'mf.util)

(defn extra-helper [n] (* n 10))
(defn second [req] (assoc req :seen-second true))
MFN_EOF
cat > "$mfn_app/src/mf/core.clj" <<'MFN_EOF'
(ns mf.core (:require [mf.util :as u]))
(defn -main [& _]
  (println "mfn-second:" (u/fwd-second [7 8]))
  (println "mfn-after: " (u/after-load {:req "K"})))
MFN_EOF
mfn_out="$(dirname "$out")/mfn-bin"
if ! JOLT_PWD="$mfn_app" "$jolt" build -m mf.core -o "$mfn_out" >/dev/null 2>&1; then
  echo "  FAIL: multi-file-namespace app build exited non-zero"; exit 1
fi
# the binary runs the (load) itself, so the app source outlives this run
got_mfn="$(cd / && "$mfn_out" 2>&1)"
want_mfn="$(cd "$mfn_app" && JOLT_PWD="$mfn_app" "$joltabs" run -m mf.core 2>&1)"
rm -rf "$(dirname "$mfn_app")"
if [ "$got_mfn" != "$want_mfn" ]; then
  echo "  FAIL: a (load)ed redefinition resolves differently in the binary and under jolt run"
  echo "--- binary ----"; echo "$got_mfn"
  echo "--- jolt run --"; echo "$want_mfn"; exit 1
fi
for line in 'mfn-second: 8' 'mfn-after:  [{:req K, :seen-second true} 40]'; do
  if ! printf '%s' "$got_mfn" | grep -qF "$line"; then
    echo "  FAIL: (load)ed redefinition — want '$line'"
    echo "--- got ----"; echo "$got_mfn"; exit 1
  fi
done

# A closure returned by a SPLICED callee must still travel in a state image, and
# the built binary must agree with `jolt run` about it. Only a built binary
# splices, and the splicer used to drop the fn's source registration -- so the
# same program wrote the closure under `jolt run` and refused it here.
#
# Compared against `jolt run` rather than pinned to a literal, because the bug
# was a DIVERGENCE between the two; a literal would have to be re-derived every
# time the fixture's arithmetic changes, and would not say what it is for.
# A deftype that DECLARES clojure.lang.ILookup answers every key through that
# valAt, field-named ones included -- the JVM gives such a type no other key
# lookup, and the slot may hold exactly what valAt is there to transform. The
# runtime honours it; the BUILD folded past it in two places, because every
# deftype is registered as a record shape and nothing told the passes this one
# has a lookup of its own (jolt-fpp3.1): scalar replacement's (:k <ctor>) fold,
# and whole-program inference proving a param a struct and dropping the guard.
#
# Compared against `jolt run` because the bug is a DIVERGENCE between the two,
# then against the literal so the two cannot agree on a shared failure.
got_dt="$(cd / && "$out" --dtlookup 2>&1)"
want_dt="$(cd "$app" && JOLT_PWD="$app" "$joltabs" run -m app.core --dtlookup 2>&1)"
if [ "$got_dt" != "$want_dt" ]; then
  echo "  FAIL: a deftype's declared valAt does not answer the same as under jolt run"
  echo "--- binary ----"; echo "$got_dt"
  echo "--- jolt run --"; echo "$want_dt"; exit 1
fi
for line in 'dt-ctor:   :from-valat' 'dt-proven: :from-valat' 'dt-opaque: :from-valat :none'; do
  if ! printf '%s' "$got_dt" | grep -qF "$line"; then
    echo "  FAIL: declared valAt -- want '$line' (the field slot was read instead)"
    echo "--- got ----"; echo "$got_dt"; exit 1
  fi
done

fasl="$(dirname "$out")/closure.fasl"
got_cl="$(cd / && "$out" --closure "$fasl" 2>&1)"
want_cl="$(cd "$app" && JOLT_PWD="$app" "$joltabs" run -m app.core --closure "$fasl" 2>&1)"
if [ "$got_cl" != "$want_cl" ]; then
  echo "  FAIL: a spliced callee's closure does not travel the same as under jolt run"
  echo "--- binary ----"; echo "$got_cl"
  echo "--- jolt run --"; echo "$want_cl"; exit 1
fi
# ...and both actually wrote one, rather than agreeing on a shared failure. The
# img-* lines cover the rest of the value kinds an image carries -- a lazy seq
# built by clojure.core, one already walked part-way, a multimethod, an unkept
# promise and a namespace -- through the BUILD emit path, which is a different
# path from `jolt run` and which nothing else exercises for images.
if ! printf '%s' "$got_cl" | grep -q '^closure-scan: 0 0$'; then
  echo "  FAIL: spliced closure is not writable (scan reported refusals)"
  echo "--- got ----"; echo "$got_cl"; exit 1
fi
for line in 'closure-folded: 115 115' 'closure-live: 110 110' \
            'img-dumpable: true true' 'img-lazy: [1 2 3 4]' 'img-walked: [1 2 3]' \
            'img-multi: :got-a :dflt' 'img-misc: false true'; do
  if ! printf '%s' "$got_cl" | grep -qF "$line"; then
    echo "  FAIL: spliced closure round trip — want '$line'"
    echo "--- got ----"; echo "$got_cl"; exit 1
  fi
done

# The :str-stamped interop answers at runtime with the same values the generic
# dispatch would (the emit-level proof is the flat.ss grep above).
got_strd="$(cd / && "$out" --strd 2>&1)"
if ! printf '%s' "$got_strd" | grep -q '^strd: true false 1 true false$'; then
  echo "  FAIL: :str-stamped interop output — want 'strd: true false 1 true false'"
  echo "--- got ----"; echo "$got_strd"; exit 1
fi

# Same runtime-shape check for the :kw-stamped interop (app.util/kwsym).
got_kwsym="$(cd / && "$out" --kwsym 2>&1)"
if ! printf '%s' "$got_kwsym" | grep -q '^kwsym: ns/qual plain$'; then
  echo "  FAIL: :kw-stamped interop output — want 'kwsym: ns/qual plain'"
  echo "--- got ----"; echo "$got_kwsym"; exit 1
fi

# Same runtime-shape check for the :sb-stamped interop (app.util/sbjoin): the
# separator logic, the empty case, and the single-element case all have to survive
# the inline lowering, since append's fluent return is what the reduce threads.
got_sbjoin="$(cd / && "$out" --sbjoin 2>&1)"
if ! printf '%s' "$got_sbjoin" | grep -q '^sbjoin: a\.b\.c  x$'; then
  echo "  FAIL: :sb-stamped interop output — want 'sbjoin: a.b.c  x'"
  echo "--- got ----"; echo "$got_sbjoin"; exit 1
fi

# The ClassLoader resource surface resolves what io/resource resolves. It used to
# walk the source roots on its own and never consult the embedded table, so a
# baked-in resource answered nil through RT/baseLoader while io/resource served
# it — invisible in the source tree, and only ever wrong in a built binary, which
# is why the check lives here.
got_rl="$(cd / && "$out" --resloader 2>&1)"
if ! printf '%s' "$got_rl" | grep -q '^resloader: true true 1 true true$'; then
  echo "  FAIL: ClassLoader resource surface — want 'resloader: true true 1 true true'"
  echo "--- got ----"; echo "$got_rl"; exit 1
fi

# With no -o and JOLT_PWD unset -- the built jolt started in the project -- the
# binary is named after the project DIRECTORY, not the entry namespace: "." is
# resolved to the directory it stands for.
echo "build smoke: default binary name from the project directory"
nm_root="$(mktemp -d)"
nm_app="$nm_root/named-app"
cp -R "$app" "$nm_app"
if ! (cd "$nm_app" && env -u JOLT_PWD "$joltabs" build -m app.core --dev >/dev/null 2>&1); then
  echo "  FAIL: build with no -o and no JOLT_PWD exited non-zero"; exit 1
fi
if [ -x "$nm_app/target/debug/named-app" ]; then
  echo "  - default name: ok (target/debug/named-app)"
else
  echo "  FAIL: expected target/debug/named-app, found: $(ls "$nm_app/target/debug" 2>/dev/null | tr '\n' ' ')"; exit 1
fi
rm -rf "$nm_root"

# Portable embed: remove the build-time source tree and run from / — the
# embedded resource must still resolve (contents baked as literals, not
# read-file-string at startup).
echo "build smoke: portable-embed check"
app_copy="$(mktemp -d)/app-copy"
cp -R "$app" "$app_copy"
pe_out="$(dirname "$out")/pe-bin"
if ! JOLT_PWD="$app_copy" "$jolt" build -m app.core -o "$pe_out" >/dev/null 2>&1; then
  echo "  FAIL: portable-embed build exited non-zero"; exit 1
fi
rm -rf "$app_copy"
pe_got="$(cd / && "$pe_out" 2>&1)"
if ! printf '%s' "$pe_got" | grep -q 'embedded resource ok'; then
  echo "  FAIL: portable-embed — embedded resource not found after source tree removed"
  echo "--- got ----"; echo "$pe_got"
  exit 1
fi

# Optimized mode (inference + flatten + scalar-replace) must produce the same
# result — a sanity check that the passes don't miscompile this app.
#
# And release and --opt compile the runtime half IDENTICALLY (build.ss
# bld-runtime-chez-params). The runtime-fasl cache is keyed on the Chez
# parameters and the runtime source, so the two modes share ONE entry when
# and only when they select the same parameters — the release row growing
# inspector/procedure-source information back (the 2.3x-binary, 1.6x-startup
# cost burinc/jolt#3 was about) shows up here as a second entry. Asked of a
# fresh cache directory rather than by comparing the two build dirs' fasls:
# with the default cache both dirs are copies of one entry, so cmp could not
# tell the modes apart, and a fresh Chez compile is not byte-reproducible, so
# it could not with the cache off either. $out is still the plain release build.
modecache="$(dirname "$out")/modecache"
if ! JOLT_PWD="$app" JOLT_RUNTIME_CACHE_DIR="$modecache" "$jolt" build -m app.core -o "$out.rel" >/dev/null 2>&1; then
  echo "  FAIL: release build into a fresh runtime cache exited non-zero"; exit 1
fi
if ! JOLT_PWD="$app" JOLT_RUNTIME_CACHE_DIR="$modecache" JOLT_BUILD_PROFILE=1 "$jolt" build -m app.core -o "$out.opt" --opt 2>"$modecache/prof.log" >/dev/null; then
  echo "  FAIL: jolt build --opt exited non-zero"
  sed -n 's/^jolt build: \[profile\]/    /p' "$modecache/prof.log"
  exit 1
fi
got_opt="$(cd / && "$out.opt" alpha bb ccc 2>&1)"
if [ "$got_opt" != "$want" ]; then
  echo "  FAIL: --opt binary output mismatch"
  echo "--- got ----"; echo "$got_opt"
  exit 1
fi
n_rt="$(ls "$modecache"/*.so 2>/dev/null | wc -l | tr -d ' ')"
if [ "$n_rt" != "1" ] || ! grep -q 'runtime fasl (cached)' "$modecache/prof.log"; then
  echo "  FAIL: release and --opt compiled the runtime half differently"
  echo "        $n_rt runtime fasl(s) in a cache both modes wrote to; --opt $(grep -q 'runtime fasl (cached)' "$modecache/prof.log" && echo reused || echo did not reuse) the release entry"
  sed -n 's/^jolt build: \[profile\]/    /p' "$modecache/prof.log"
  exit 1
fi

# Closed-world direct-linking (opt-in): same result, and the cross-namespace call
# (app.core -> app.util/shout) must lower to a direct jv$ binding, not var-deref.
if ! JOLT_PWD="$app" "$jolt" build -m app.core -o "$out" --direct-link >/dev/null 2>&1; then
  echo "  FAIL: jolt build --direct-link exited non-zero"; exit 1
fi
got_dl="$(cd / && "$out" alpha bb ccc 2>&1)"
if [ "$got_dl" != "$want" ]; then
  echo "  FAIL: --direct-link binary output mismatch"
  echo "--- got ----"; echo "$got_dl"
  exit 1
fi
if ! grep -q 'define jv\$app.util\$shout' "$out.build/flat.ss"; then
  echo "  FAIL: --direct-link did not emit a direct app->app call"; exit 1
fi
# A direct-link build registers fn sources, so an uncaught throw prints a Clojure
# stack trace mapping each native frame back to ns/name (file:line).
if ! grep -q 'jolt-register-source!' "$out.build/flat.ss"; then
  echo "  FAIL: --direct-link did not emit source registrations"; exit 1
fi
boom_err="$(cd / && "$out" --boom 2>&1 >/dev/null)"
# A direct-linked build INLINES (jolt-mbcm.6): deep-boom is spliced into
# mid-boom and mid-boom into -main, so all three of these frames come out of ONE
# physical frame, reconstructed from the inline chain the splicer stamped
# (jolt-mbcm.7). The --no-direct-link loop above asserts the same three without
# splicing, so the two together are the parity claim: inlining must not change
# what a backtrace says.
for frame in 'app\.util/deep-boom .*util\.clj:[0-9]' 'app\.util/mid-boom .*util\.clj:[0-9]' 'app\.core/-main .*core\.clj:[0-9]'; do
  if ! printf '%s' "$boom_err" | grep -Eq "$frame"; then
    echo "  FAIL: stack trace missing located frame $frame"
    echo "--- got ----"; echo "$boom_err"
    exit 1
  fi
done
# ...and in that order, innermost first. A chain reconstructed backwards would
# still contain every frame and pass the loop above.
if ! printf '%s' "$boom_err" | tr '\n' '|' \
     | grep -q 'deep-boom.*mid-boom.*-main'; then
  echo "  FAIL: reconstructed frames are not innermost-first"
  echo "--- got ----"; echo "$boom_err"
  exit 1
fi

# A pure-fn fold must not discard a throwing op. scalar-replace folds
# (:a {:a 1 :b (/ 1 0)}) -> 1 under --opt --direct-link, dropping the sibling;
# / (and quot/rem/mod/even?/odd?) are NOT pure, so the divisor still evaluates,
# the ArithmeticException fires, and -main prints THROW OK, not the folded 1.
# THROW2 pins the same for a literal :throw node: safe-op? admitted :throw, so
# pure?/total? treated it as discardable and elim-let-structs dropped the whole
# map binding — the release binary printed 1 instead of throwing.
inline_throw_app="$root/test/chez/inline-throw-app"
inline_throw_out="$(dirname "$out")/inline-throw-bin"
if ! JOLT_PWD="$inline_throw_app" "$jolt" build -m app.core -o "$inline_throw_out" --opt --direct-link >/dev/null 2>&1; then
  echo "  FAIL: inline-throw --opt --direct-link build exited non-zero"; exit 1
fi
inline_throw_got="$(cd "$inline_throw_app" && "$inline_throw_out" 2>&1)"
inline_throw_want="$(printf 'THROW OK\nTHROW2 OK')"
if [ "$inline_throw_got" != "$inline_throw_want" ]; then
  echo "  FAIL: pure-fn fold discarded a throwing op — got \`$inline_throw_got\`, want \`$inline_throw_want\`"; exit 1
fi

# Under --opt inference proves a nil-bound local is :nil, so nil? folds true and
# some? folds false. The fold was inverted (nil?->false, some?->true), so a release
# --opt build printed :b / :y instead of :a / :n.
nil_fold_app="$root/test/chez/nil-fold-app"
nil_fold_out="$(dirname "$out")/nil-fold-bin"
if ! JOLT_PWD="$nil_fold_app" "$jolt" build -m app.core -o "$nil_fold_out" --opt >/dev/null 2>&1; then
  echo "  FAIL: nil-fold --opt build exited non-zero"; exit 1
fi
nil_fold_got="$(cd "$nil_fold_app" && "$nil_fold_out" 2>&1)"
nil_fold_want="$(printf ':a\n:n')"
if [ "$nil_fold_got" != "$nil_fold_want" ]; then
  echo "  FAIL: nil?/some? fold inverted — got \`$nil_fold_got\`, want \`$nil_fold_want\`"; exit 1
fi

# Only a proven-NON-NIL receiver may devirtualize. A devirt site resolves the impl
# by the static type tag and caches it, so devirtualizing a record-or-nil receiver
# served the cached impl to a later nil receiver: this printed 3 twice instead of
# raising, where Clojure raises IllegalArgumentException the second time.
nil_devirt_app="$root/test/chez/nil-devirt-app"
nil_devirt_out="$(dirname "$out")/nil-devirt-bin"
if ! JOLT_PWD="$nil_devirt_app" "$jolt" build -m app.core -o "$nil_devirt_out" --opt --direct-link >/dev/null 2>&1; then
  echo "  FAIL: nil-devirt --opt --direct-link build exited non-zero"; exit 1
fi
nil_devirt_got="$(cd "$nil_devirt_app" && "$nil_devirt_out" 2>&1)"
nil_devirt_want="$(printf '3\n:no-impl')"
if [ "$nil_devirt_got" != "$nil_devirt_want" ]; then
  echo "  FAIL: devirt on a nilable receiver — got \`$nil_devirt_got\`, want \`$nil_devirt_want\`"; exit 1
fi

# A loop var that shadows a record-typed outer local must shadow in the inference
# tenv. The bug kept the outer type, so under --opt (:x p) devirtualized to a
# record slot read that crashed on the vector [3 4]; the fix keeps the loop p :any
# so (:x p) is a generic keyword lookup -> nil. The second line carries the record
# straight through to prove field reads still devirtualize.
loop_shadow_app="$root/test/chez/loop-shadow-app"
loop_shadow_out="$(dirname "$out")/loop-shadow-bin"
if ! JOLT_PWD="$loop_shadow_app" "$jolt" build -m app.core -o "$loop_shadow_out" --opt >/dev/null 2>&1; then
  echo "  FAIL: loop-shadow --opt build exited non-zero"; exit 1
fi
loop_shadow_got="$(cd "$loop_shadow_app" && "$loop_shadow_out" 2>&1)"
loop_shadow_want="$(printf 'nil\n1.0')"
if [ "$loop_shadow_got" != "$loop_shadow_want" ]; then
  echo "  FAIL: loop var did not shadow record local — got \`$loop_shadow_got\`, want \`$loop_shadow_want\`"; exit 1
fi

# min/max return an operand unchanged. A --opt/inference double-contagion bug
# coerced the int operand to a flonum, so (min 2.5 1) printed 1.0 and (max 2.5 3)
# printed 3.0. They must preserve the int.
min_max_app="$root/test/chez/min-max-app"
min_max_out="$(dirname "$out")/min-max-bin"
if ! JOLT_PWD="$min_max_app" "$jolt" build -m app.core -o "$min_max_out" --opt >/dev/null 2>&1; then
  echo "  FAIL: min-max --opt build exited non-zero"; exit 1
fi
min_max_got="$(cd "$min_max_app" && "$min_max_out" 2>&1)"
min_max_want="$(printf '1\n3')"
if [ "$min_max_got" != "$min_max_want" ]; then
  echo "  FAIL: min/max coerced int to double — got \`$min_max_got\`, want \`$min_max_want\`"; exit 1
fi

# A built binary runs -main with *ns* = user, like clojure.main — so a runtime
# resolve of an aliased symbol is nil (the alias lives in the entry ns, not user),
# matching the JVM and interpreted jolt rather than the entry ns's alias table. A
# separate app: `resolve` defeats tree-shaking, so keep it out of the shake test's
# app above.
nsp="$(dirname "$out")/nsparity"
mkdir -p "$nsp/src/nsp"
printf '{:paths ["src"]}\n' > "$nsp/deps.edn"
printf '(ns nsp.lib)\n(defn thing [] 1)\n' > "$nsp/src/nsp/lib.clj"
printf '(ns nsp.main (:require [nsp.lib :as l]))\n(defn -main [& _]\n  (println "ns:" (str *ns*))\n  (println "resolve:" (pr-str (resolve (quote l/thing))))\n  (println "ns-resolve:" (pr-str (ns-resolve (quote nsp.lib) (quote thing)))))\n' > "$nsp/src/nsp/main.clj"
nspout="$(dirname "$out")/nsparity-bin"
if ! JOLT_PWD="$nsp" "$jolt" build -m nsp.main -o "$nspout" >/dev/null 2>&1; then
  echo "  FAIL: jolt build of the ns-parity app exited non-zero"; exit 1
fi
nsp_out="$(cd / && "$nspout" 2>&1)"
if ! printf '%s' "$nsp_out" | grep -q 'ns: user' \
   || ! printf '%s' "$nsp_out" | grep -q '^resolve: nil' \
   || ! printf '%s' "$nsp_out" | grep -q "ns-resolve: #'nsp.lib/thing"; then
  echo "  FAIL: built binary -main ns parity — want 'ns: user', 'resolve: nil', ns-resolve found"
  echo "--- got ----"; echo "$nsp_out"
  exit 1
fi
# Tree-shaking (opt-in) on THIS app bails: it restores images, and a restore
# compiles the fn sources an image carries, which can name any core var — one
# the compiled program never reached because the inline pass spliced it away
# at every site (a shaken build once restored a closure that called `update`
# and died on the unbound var its own -main had used without a trace). So
# jolt.host/image-read is a bail reference like eval: everything is kept, the
# compiler with it, the diagnostic names the reason, and the binary restores
# exactly what the unshaken one did.
if ! JOLT_PWD="$app" "$jolt" build -m app.core -o "$out" --tree-shake >"$out.ts.log" 2>&1; then
  echo "  FAIL: jolt build --tree-shake exited non-zero"; cat "$out.ts.log"; exit 1
fi
if ! grep -q 'tree-shake skipped' "$out.ts.log" || ! grep -q 'image-read' "$out.ts.log"; then
  echo "  FAIL: --tree-shake of an image-restoring app must bail, naming image-read"
  cat "$out.ts.log"; exit 1
fi
got_ts="$(cd / && "$out" alpha bb ccc 2>&1)"
if [ "$got_ts" != "$want" ]; then
  echo "  FAIL: --tree-shake binary output mismatch"
  echo "--- got ----"; echo "$got_ts"
  exit 1
fi
if ! grep -Eq 'def-var[a-z!-]*! "jolt.analyzer"' "$out.build/runtime.ss"; then
  echo "  FAIL: the bailed --tree-shake build dropped the compiler an image restore needs"; exit 1
fi
got_ts_cl="$(cd / && "$out" --closure "$fasl" 2>&1)"
for line in 'closure-folded: 115 115' 'closure-live: 110 110' 'img-lazy: [1 2 3 4]'; do
  if ! printf '%s' "$got_ts_cl" | grep -qF "$line"; then
    echo "  FAIL: the bailed --tree-shake binary could not restore an image — want '$line'"
    echo "--- got ----"; echo "$got_ts_cl"; exit 1
  fi
done
# ...and on an app that never restores one, the shake prunes: same output, the
# unreferenced def is gone from the app half, a clojure.core overlay fn the app
# never uses is gone from the shaken core — which is the runtime unit
# (runtime.ss), compiled apart from the app half so it takes the runtime's
# no-inspector parameters; flat.ss never held it — and the compiler is dropped.
doapp="$root/test/chez/defonce-app"
doout="$(dirname "$out")/defonce-bin"
if ! JOLT_PWD="$doapp" "$jolt" build -m app.core -o "$doout" --tree-shake >/dev/null 2>&1; then
  echo "  FAIL: jolt build --tree-shake of the defonce app exited non-zero"; exit 1
fi
got_do="$(cd / && "$doout" 2>&1)"
if [ "$got_do" != "$(printf '1\nalive')" ]; then
  echo "  FAIL: --tree-shake defonce binary output mismatch"
  echo "--- got ----"; echo "$got_do"; exit 1
fi
if grep -q '"app.core" "dead"' "$doout.build/flat.ss"; then
  echo "  FAIL: --tree-shake did not drop the unreferenced def app.core/dead"; exit 1
fi
[ -f "$doout.build/runtime.ss" ] || { echo "  FAIL: --tree-shake did not emit the shaken core as its own runtime unit"; exit 1; }
if grep -q 'def-var! "clojure.core" "group-by"' "$doout.build/runtime.ss"; then
  echo "  FAIL: --tree-shake kept an unreachable clojure.core fn (group-by)"; exit 1
fi
if grep -Eq 'def-var[a-z!-]*! "jolt.analyzer"' "$doout.build/runtime.ss"; then
  echo "  FAIL: --tree-shake kept the compiler image in a no-eval app"; exit 1
fi
# A registered data reader that returns a CODE form must be compiled into the
# binary (the emit path applies it too, not just the interpreted loader): the
# datareader-app's #code literal builds to 42, not the literal list.
# Also exercises transitive reader requires: #my/rev calls app.readers/reverse-str
# which requires app.util, proving the require-graph closure pulls in helper
# namespaces reachable only through the data-readers table — and the two fn-valued
# *data-readers* entries, whose results the emit path has to splice by value.
drapp="$root/test/chez/datareader-app"
drout="$(dirname "$out")/dr-bin"
if ! JOLT_PWD="$drapp" "$jolt" build -m drtest.main -o "$drout" >/dev/null 2>&1; then
  echo "  FAIL: jolt build of a data-reader app exited non-zero"; exit 1
fi
# A program that reaches no eval, load-string or image restore ships without
# the compiler by DEFAULT — no --tree-shake asked for. runtime.ss is where it
# would be; the scheme.boot compiler kernel goes with it (petite-only boot).
if grep -Eq 'def-var[a-z!-]*! "jolt.analyzer"' "$drout.build/runtime.ss"; then
  echo "  FAIL: the default build of a no-eval app kept the compiler image"; exit 1
fi
got_dr="$(cd / && "$drout" 2>&1)"
dr_want='42
olleh!
3
shout-value'
if [ "$got_dr" != "$dr_want" ]; then
  echo "  FAIL: built data-reader output mismatch"
  echo "--- want ---"; echo "$dr_want"
  echo "--- got ----"; echo "$got_dr"
  exit 1
fi

# A script namespace with no -main (just top-level side effects) must build and
# run its top-level forms, then exit cleanly — not crash calling a nil -main.
nomain="$(dirname "$out")/nomain"
mkdir -p "$nomain/src"
printf '{:paths ["src"]}\n' > "$nomain/deps.edn"
printf '(ns script)\n(println "no-main script ran")\n' > "$nomain/src/script.clj"
nmout="$(dirname "$out")/nomain-bin"
if ! JOLT_PWD="$nomain" "$jolt" build -m script -o "$nmout" >/dev/null 2>&1; then
  echo "  FAIL: jolt build of a no-main script exited non-zero"; exit 1
fi
got_nm="$(cd / && "$nmout" 2>&1)"; rc_nm=$?
if [ "$got_nm" != "no-main script ran" ] || [ "$rc_nm" != "0" ]; then
  echo "  FAIL: no-main script binary — want 'no-main script ran' rc 0, got \`$got_nm\` rc $rc_nm"
  exit 1
fi

# Optional :jolt/native with a MISSING lib: the defcfn is lazy, so the build
# succeeds and the binary runs when -main never calls it; calling it fails with
# a catchable error, not a kernel abort.
olout="$(dirname "$out")/optional-lib-bin"
if ! JOLT_PWD="$root/test/chez/optional-lib-app" "$jolt" build -m app.optional-lib -o "$olout" >/dev/null 2>&1; then
  echo "  FAIL: build with a missing optional native lib exited non-zero"; exit 1
fi
got_ol="$(cd / && "$olout" 2>&1)"
if [ "$got_ol" != "optional lib app ran successfully" ]; then
  echo "  FAIL: optional-lib binary — got \`$got_ol\`"; exit 1
fi
ocout="$(dirname "$out")/optional-call-bin"
if ! JOLT_PWD="$root/test/chez/optional-lib-call-app" "$jolt" build -m app.optional-lib-call -o "$ocout" >/dev/null 2>&1; then
  echo "  FAIL: build of optional-lib-call app exited non-zero"; exit 1
fi
got_oc="$(cd / && "$ocout" 2>&1 | tail -1)"
case "$got_oc" in
  "caught expected error:"*) : ;;
  *) echo "  FAIL: calling a missing optional-lib fn — want a caught error, got \`$got_oc\`"; exit 1 ;;
esac

# deps.edn :jolt/build {:opt true} puts the build in optimized mode without a CLI flag.
optproj="$(dirname "$out")/optproj"
mkdir -p "$optproj/src"
printf '{:paths ["src"] :jolt/build {:opt true}}\n' > "$optproj/deps.edn"
printf '(ns app)\n(defn -main [& _] (println "opt project ran"))\n' > "$optproj/src/app.clj"
opout="$(dirname "$out")/optproj-bin"
modeline="$(JOLT_PWD="$optproj" "$jolt" build -m app -o "$opout" 2>&1 | grep 'compiling app (')"
case "$modeline" in
  *"(optimized mode"*) : ;;
  *) echo "  FAIL: deps.edn :jolt/build {:opt true} did not select optimized mode — got \`$modeline\`"; exit 1 ;;
esac

# A namespace with a cljs-only reader conditional (`#?(:cljs …)`) between two clj
# defns must not truncate emission at the conditional — the fn AFTER it must be
# emitted into the binary, or a call to it crashes on an unbound var.
ccapp="$root/test/chez/cljc-cond-app"
ccout="$(dirname "$out")/cljc-cond-bin"
if ! JOLT_PWD="$ccapp" "$jolt" build -m cljccond.main -o "$ccout" >/dev/null 2>&1; then
  echo "  FAIL: jolt build of a cljs-conditional app exited non-zero"; exit 1
fi
got_cc="$(cd / && "$ccout" 2>&1 | tail -1)"
if [ "$got_cc" != "CLJC-COND :before :after" ]; then
  echo "  FAIL: cljs-only conditional truncated emission — want 'CLJC-COND :before :after', got \`$got_cc\`"; exit 1
fi

# A .jolt namespace is ordinary source — the extension only marks jolt-specific
# interop — so `build` has to resolve, emit, and macroexpand it exactly like a
# .clj. The fixture's .clj main requires a .jolt lib and uses a macro from it.
jxapp="$root/test/chez/jolt-ext-app"
jxout="$(dirname "$out")/jolt-ext-bin"
if ! JOLT_PWD="$jxapp" "$jolt" build -m jxapp.main -o "$jxout" >/dev/null 2>&1; then
  echo "  FAIL: jolt build of an app with a .jolt namespace exited non-zero"; exit 1
fi
got_jx="$(cd / && "$jxout" 2>&1 | tail -1)"
if [ "$got_jx" != "JOLT-EXT BUILT! (:x :x)" ]; then
  echo "  FAIL: .jolt namespace in a built binary — want 'JOLT-EXT BUILT! (:x :x)', got \`$got_jx\`"; exit 1
fi

# A file's top-level (set! *unchecked-math* true) must load and take effect, and
# must not escape that file. Both the loader and the AOT'd binary bracket a
# namespace's forms with a thread binding for the var (RT.load parity), so this
# runs the app from source and as a built binary and expects the same answer.
# The middle value is a widened bigint (jolt widens where the JVM throws) and
# prints with the N suffix, as the JVM prints any bigint under print.
umapp="$root/test/chez/unchecked-math-app"
umwant="UNCHECKED-MATH -9223372036854775808 9223372036854775808N false"
got_um_src="$(cd "$umapp" && JOLT_PWD="$umapp" "$joltabs" run -m umapp.main 2>&1 | tail -1)"
if [ "$got_um_src" != "$umwant" ]; then
  echo "  FAIL: top-level (set! *unchecked-math* …) from source — want \`$umwant\`, got \`$got_um_src\`"; exit 1
fi
umout="$(dirname "$out")/unchecked-math-bin"
if ! JOLT_PWD="$umapp" "$jolt" build -m umapp.main -o "$umout" >/dev/null 2>&1; then
  echo "  FAIL: jolt build of an unchecked-math app exited non-zero"; exit 1
fi
got_um_bin="$(cd / && "$umout" 2>&1 | tail -1)"
if [ "$got_um_bin" != "$umwant" ]; then
  echo "  FAIL: top-level (set! *unchecked-math* …) in a built binary — want \`$umwant\`, got \`$got_um_bin\`"; exit 1
fi

# A built binary must have the vendored babashka.fs (via jolt.fs) available and
# runnable — including functions defined after babashka.fs's cljs-only reader
# conditionals (directory?/cwd/which). Guards the vendored-namespace baking.
fsapp="$root/test/chez/fs-app"
fsout="$(dirname "$out")/fs-app-bin"
if ! JOLT_PWD="$fsapp" "$jolt" build -m fsapp.main -o "$fsout" >/dev/null 2>&1; then
  echo "  FAIL: jolt build of a jolt.fs / babashka.fs app exited non-zero"; exit 1
fi
got_fs="$(cd / && "$fsout" 2>&1 | tail -1)"
if [ "$got_fs" != "FS-APP a/b true true rw-------" ]; then
  echo "  FAIL: built binary missing vendored babashka.fs — want 'FS-APP a/b true true rw-------', got \`$got_fs\`"; exit 1
fi

# The same fs app tree-shaken: a compiler-dropped binary boots from petite alone
# (no scheme.boot), so its libc calls through jolt-foreign-proc-safe (stat &co
# under jolt.fs) must resolve as compiled foreign-procedures — an eval'd form
# would silently return #f under the interpreter and the output would change.
# Petite-only is asserted off the build's own verdict line: the self-contained
# link path writes no compile.ss, so a grep of that file passed whatever the
# build embedded.
fsshake="$(dirname "$out")/fs-app-shake-bin"
if ! JOLT_PWD="$fsapp" "$jolt" build -m fsapp.main -o "$fsshake" --tree-shake >"$fsshake.log" 2>&1; then
  echo "  FAIL: jolt build --tree-shake of the jolt.fs app exited non-zero"; exit 1
fi
if ! grep -q '^jolt build: dropping compiler image' "$fsshake.log"; then
  echo "  FAIL: tree-shaken fs app kept the compiler image (petite-only boot expected)"; exit 1
fi
got_fss="$(cd / && "$fsshake" 2>&1 | tail -1)"
if [ "$got_fss" != "FS-APP a/b true true rw-------" ]; then
  echo "  FAIL: petite-only fs binary output mismatch — want 'FS-APP a/b true true rw-------', got \`$got_fss\`"; exit 1
fi

# A built binary must have the vendored babashka.process (via jolt.process) and
# be able to spawn a real sub-process. Guards the vendored-namespace baking.
procapp="$root/test/chez/process-app"
procout="$(dirname "$out")/process-app-bin"
if ! JOLT_PWD="$procapp" "$jolt" build -m procapp.main -o "$procout" >/dev/null 2>&1; then
  echo "  FAIL: jolt build of a jolt.process / babashka.process app exited non-zero"; exit 1
fi
got_proc="$(cd / && "$procout" 2>&1 | tail -1)"
if [ "$got_proc" != "PROC-APP hi 0 143" ]; then
  echo "  FAIL: built binary missing vendored babashka.process — want 'PROC-APP hi 0 143', got \`$got_proc\`"; exit 1
fi

# The same process app tree-shaken: a compiler-dropped binary boots from petite
# alone, so its libc calls through jolt-foreign-proc-safe (waitpid / kill under
# jolt.process) must resolve as compiled foreign-procedures — an eval'd form would
# silently return #f under the interpreter and the exit codes would be lost.
procshake="$(dirname "$out")/process-app-shake-bin"
if ! JOLT_PWD="$procapp" "$jolt" build -m procapp.main -o "$procshake" --tree-shake >"$procshake.log" 2>&1; then
  echo "  FAIL: jolt build --tree-shake of the jolt.process app exited non-zero"; exit 1
fi
if ! grep -q '^jolt build: dropping compiler image' "$procshake.log"; then
  echo "  FAIL: tree-shaken process app kept the compiler image (petite-only boot expected)"; exit 1
fi
got_procs="$(cd / && "$procshake" 2>&1 | tail -1)"
if [ "$got_procs" != "PROC-APP hi 0 143" ]; then
  echo "  FAIL: petite-only process binary output mismatch — want 'PROC-APP hi 0 143', got \`$got_procs\`"; exit 1
fi

# A built binary must carry the CLOJURE half of jolt.ffi (stdlib/jolt/ffi.clj),
# not just the host primitives from java/ffi.ss. jolt.ffi sits in the CLI's own
# AOT closure and in neither the runtime image nor the stdlib-fasl manifest, so
# an app build that reads the CLI's closure as preloaded interns layout-size /
# field-offset / read-field / write-field UNBOUND and fails at the call. `jolt
# run` compiles the source at require time and masks it entirely.
ffiapp="$root/test/chez/ffi-app"
ffiout="$(dirname "$out")/ffi-app-bin"
if ! JOLT_PWD="$ffiapp" "$jolt" build -m ffiapp.main -o "$ffiout" >/dev/null 2>&1; then
  echo "  FAIL: jolt build of a jolt.ffi app exited non-zero"; exit 1
fi
got_ffi="$(cd / && "$ffiout" 2>&1 | tail -1)"
if [ "$got_ffi" != "FFI-APP 8 4 4 2.5 true" ]; then
  echo "  FAIL: built binary missing the jolt.ffi Clojure layer — want 'FFI-APP 8 4 4 2.5 true', got \`$got_ffi\`"; exit 1
fi

# The same ffi app tree-shaken: a petite-only boot has no compiler, so
# errno-message's strerror defcfn must resolve as a compiled foreign-procedure.
ffishake="$(dirname "$out")/ffi-app-shake-bin"
if ! JOLT_PWD="$ffiapp" "$jolt" build -m ffiapp.main -o "$ffishake" --tree-shake >"$ffishake.log" 2>&1; then
  echo "  FAIL: jolt build --tree-shake of the jolt.ffi app exited non-zero"; exit 1
fi
if ! grep -q '^jolt build: dropping compiler image' "$ffishake.log"; then
  echo "  FAIL: tree-shaken ffi app kept the compiler image (petite-only boot expected)"; exit 1
fi
got_ffis="$(cd / && "$ffishake" 2>&1 | tail -1)"
if [ "$got_ffis" != "FFI-APP 8 4 4 2.5 true" ]; then
  echo "  FAIL: petite-only ffi binary output mismatch — want 'FFI-APP 8 4 4 2.5 true', got \`$got_ffis\`"; exit 1
fi

# A declaration-only var and a no-root dynamic var must stay resolvable
# (find-var / resolve / ns-interns) in an AOT binary. A no-init def now carries
# source-position metadata, so it emits set-var-meta! then declare-var! —
# declare-var! must mark the already-interned cell defined?, or introspection
# tooling (spec instrument / nREPL) misses it. Its own tiny app: find-var bails
# tree-shaking, so keep it off the shake fixtures above. Plain build (no shake).
echo "build smoke: declaration-only var discoverability"
decl_app="$(mktemp -d)/decl-app"
mkdir -p "$decl_app/src/da"
printf '{:paths ["src"]}\n' > "$decl_app/deps.edn"
cat > "$decl_app/src/da/core.clj" <<'DECL_EOF'
(ns da.core)
(declare only-declared)
(def ^:dynamic *cfg*)
(defn -main [& _]
  (println "declared:" (some? (find-var 'da.core/only-declared)))
  (println "dynvar:" (some? (find-var 'da.core/*cfg*)))
  (println "interned:" (contains? (ns-interns 'da.core) 'only-declared)))
DECL_EOF
decl_out="$(dirname "$out")/decl-bin"
if ! JOLT_PWD="$decl_app" "$jolt" build -m da.core -o "$decl_out" >/dev/null 2>&1; then
  echo "  FAIL: declaration-only-var app build exited non-zero"; exit 1
fi
got_decl="$(cd / && "$decl_out" 2>&1)"
rm -rf "$(dirname "$decl_app")"
if ! printf '%s' "$got_decl" | grep -q '^declared: true$' || ! printf '%s' "$got_decl" | grep -q '^dynvar: true$' || ! printf '%s' "$got_decl" | grep -q '^interned: true$'; then
  echo "  FAIL: declaration-only / no-root var not discoverable in AOT (find-var/ns-interns)"
  echo "--- got ----"; echo "$got_decl"; exit 1
fi

# An install-owned namespace (jolt's own stdlib) that a non-entry app namespace
# calls AT LOAD TIME has to be emitted BEFORE that namespace. bld-require-closure
# drops install-owned files, so jolt.time.util reaches the app list only through
# the loader hook's walked order; appending that order last put it behind oa.lib
# and the binary died at startup with "Attempting to call unbound fn:
# #'jolt.time.util/->long". Two levels deep on purpose — the entry namespace is
# forced last either way, so a one-namespace app hides the bug.
echo "build smoke: install-owned dep ordering"
ord_app="$(mktemp -d)/ord-app"
mkdir -p "$ord_app/src/oa"
printf '{:paths ["src"]}\n' > "$ord_app/deps.edn"
cat > "$ord_app/src/oa/lib.clj" <<'ORD_LIB_EOF'
(ns oa.lib (:require [jolt.time.util :as u]))
(def n (u/->long 41))
ORD_LIB_EOF
cat > "$ord_app/src/oa/core.clj" <<'ORD_EOF'
(ns oa.core (:require [oa.lib :as lib]))
(defn -main [& _] (println "ord:" (inc lib/n)))
ORD_EOF
ord_out="$(dirname "$out")/ord-bin"
if ! JOLT_PWD="$ord_app" "$jolt" build -m oa.core -o "$ord_out" >/dev/null 2>&1; then
  echo "  FAIL: install-owned-dep app build exited non-zero"; exit 1
fi
got_ord="$(cd / && "$ord_out" 2>&1)"
rm -rf "$(dirname "$ord_app")"
if [ "$got_ord" != "ord: 42" ]; then
  echo "  FAIL: install-owned dep emitted after its caller — want 'ord: 42', got \`$got_ord\`"; exit 1
fi

# A provider the CLASS scan pulls in — never reached by the entry's own requires —
# whose install namespace calls, at load time, into a namespace the entry DID
# load. jolt.time is that shape once a project depends on the git library: the
# formatter half arrives from the dep root and registers its types with
# jolt.time.impl, which the embedded stdlib half already served when the entry's
# def touched LocalDateTime. Ordering the never-loaded set by the static graph
# alone put the caller first, and the binary died before -main with "Attempting
# to call unbound fn: #'jolt.time.impl/register-type!" (#944). Offline stand-in:
# a :local/root provider declaring com.example.Split, split the same way — the
# entry requires impl, only -main names the class, so only the scan sees install.
echo "build smoke: class-scan provider ordered after the dependency the entry loaded"
psp_app="$(mktemp -d)/psp-app"
mkdir -p "$psp_app/src/psp" "$psp_app/lib/src/provsplit"
cat > "$psp_app/deps.edn" <<'PSP_EOF'
{:paths ["src"] :deps {local/provsplit {:local/root "lib"}}}
PSP_EOF
cat > "$psp_app/lib/deps.edn" <<'PSP_EOF'
{:paths ["src"] :jolt/provides {provsplit.install ["com.example.Split"]}}
PSP_EOF
cat > "$psp_app/lib/src/provsplit/impl.clj" <<'PSP_EOF'
(ns provsplit.impl)
(def registry (atom {}))
(defn register! [k v] (swap! registry assoc k v) nil)
PSP_EOF
cat > "$psp_app/lib/src/provsplit/install.clj" <<'PSP_EOF'
(ns provsplit.install (:require [provsplit.impl :as impl]))
(impl/register! :split 42)
(__register-class-statics! "com.example.Split"
                           {"answer" (fn [] (get @impl/registry :split))})
PSP_EOF
cat > "$psp_app/src/psp/core.clj" <<'PSP_EOF'
(ns psp.core (:require [provsplit.impl :as impl]))
(defn -main [& _] (println "split:" (com.example.Split/answer)))
PSP_EOF
psp_out="$(dirname "$out")/psp-bin"
if ! JOLT_PWD="$psp_app" "$jolt" build -m psp.core -o "$psp_out" >"$psp_out.log" 2>&1; then
  echo "  FAIL: split-provider app build exited non-zero"; tail -5 "$psp_out.log"; exit 1
fi
got_psp="$(cd / && "$psp_out" 2>&1)"
rm -rf "$(dirname "$psp_app")"
if [ "$got_psp" != "split: 42" ]; then
  echo "  FAIL: class-scan provider emitted before the dependency the entry loaded — want 'split: 42', got \`$got_psp\`"; exit 1
fi

# A live VALUE a macro put in its expansion has to reach the BINARY. jolt rebuilds
# one as code rather than stashing it in a process-local table (jolt-l7tq), and
# only a built binary proves that: a table would satisfy every in-process check
# and then be empty in the app's own image. Both shapes here — a named fn, read
# back through the var that roots it, and an anonymous literal with a real
# capture, rebuilt from the source form and the captured value the fn-form
# registry recorded.
echo "build smoke: a macro-embedded live value reaches the binary"
emb_app="$(mktemp -d)/emb-app"
mkdir -p "$emb_app/src/ea"
printf '{:paths ["src"]}\n' > "$emb_app/deps.edn"
cat > "$emb_app/src/ea/lib.clj" <<'EMB_LIB_EOF'
(ns ea.lib)
(defmacro named-fn [] (deref #'clojure.core/memfn))
(defn mk-adder [n] (fn [x] (+ x n)))
(defmacro anon-fn [] (mk-adder 7))
(def a (named-fn))
(def b (anon-fn))
EMB_LIB_EOF
cat > "$emb_app/src/ea/core.clj" <<'EMB_EOF'
(ns ea.core (:require [ea.lib :as lib]))
(defn -main [& _] (println "emb:" (fn? lib/a) (lib/b 35)))
EMB_EOF
emb_out="$(dirname "$out")/emb-bin"
if ! JOLT_PWD="$emb_app" "$jolt" build -m ea.core -o "$emb_out" >/dev/null 2>&1; then
  echo "  FAIL: macro-embedded-value app build exited non-zero"; exit 1
fi
got_emb="$(cd / && "$emb_out" 2>&1)"
rm -rf "$(dirname "$emb_app")"
if [ "$got_emb" != "emb: true 42" ]; then
  echo "  FAIL: embedded value did not reach the binary — want 'emb: true 42', got \`$got_emb\`"; exit 1
fi

# `build` behind a global option that re-dispatches the rest of the argv through
# -main (-Sdeps '<edn>', -A:alias). The launcher has to load the build driver
# before jolt.main runs and used to look for "build" at argv[0] only, so this
# form reached cmd-build with jolt.host/build-binary still unbound.
echo "build smoke: -Sdeps before the build command"
sdeps_out="$(dirname "$out")/sdeps-bin"
if ! JOLT_PWD="$app" "$jolt" -Sdeps '{}' build -m app.core -o "$sdeps_out" >/dev/null 2>&1; then
  echo "  FAIL: \`-Sdeps '{}' build\` exited non-zero"; exit 1
fi
[ -x "$sdeps_out" ] || { echo "  FAIL: \`-Sdeps '{}' build\` produced no executable"; exit 1; }

# Everything above builds through $jolt, which the make target points at the
# prebuilt binary. Build through bin/jolt too, so the driver a developer actually
# runs stays gated here and not only as a side effect of devbootsmoke's
# cached-project-build case. Redundant when $jolt already is bin/jolt, so skip it
# then. The first case takes whichever image bin/jolt picks (a fresh
# target/dev/flat.so, else source); the second pins source mode, for the reason
# spelled out there.
if [ "$jolt" != "bin/jolt" ]; then
  echo "build smoke: checkout-driver check"
  srcout="$(dirname "$out")/srcmode-bin"
  if ! JOLT_PWD="$root/test/chez/jolt-ext-app" bin/jolt build -m jxapp.main -o "$srcout" >/dev/null 2>&1; then
    echo "  FAIL: bin/jolt build exited non-zero"; exit 1
  fi
  got_src="$(cd / && "$srcout" 2>&1 | tail -1)"
  if [ "$got_src" != "JOLT-EXT BUILT! (:x :x)" ]; then
    echo "  FAIL: bin/jolt build output — want 'JOLT-EXT BUILT! (:x :x)', got \`$got_src\`"; exit 1
  fi

  # The ffi-app case above, through the source-mode driver. Only this ordering
  # reaches it: both the release binary and the dev boot cache bake jolt.main
  # with bld-emit-cli-aot, which marks its closure ldr-cli-aot? and re-emits it
  # into the app regardless, while source mode loads jolt.main — and with it
  # jolt.ffi — into the driver process as an ordinary require. A boot-image
  # snapshot still emits jolt.ffi into the app; snapshotting loaded-ns at
  # build-binary time reads it as already in the app's image and leaves
  # layout-size interned but UNBOUND (#756's shape, reached the other way).
  # JOLT_NO_DEVCACHE is what keeps this honest: without it the gate passes on a
  # fresh target/dev/flat.so whether or not the bug is present.
  ffisrcout="$(dirname "$out")/ffi-app-srcmode-bin"
  if ! JOLT_NO_DEVCACHE=1 JOLT_PWD="$ffiapp" bin/jolt build -m ffiapp.main -o "$ffisrcout" >/dev/null 2>&1; then
    echo "  FAIL: source-mode (bin/jolt) build of the jolt.ffi app exited non-zero"; exit 1
  fi
  got_ffisrc="$(cd / && "$ffisrcout" 2>&1 | tail -1)"
  if [ "$got_ffisrc" != "FFI-APP 8 4 4 2.5 true" ]; then
    echo "  FAIL: source-mode build dropped the jolt.ffi Clojure layer — want 'FFI-APP 8 4 4 2.5 true', got \`$got_ffisrc\`"; exit 1
  fi
fi

# A build failure names the file it was reading.
#
# The build has three walks that process a file WITHOUT evaluating its forms — the
# require scan, the whole-program inference walk, the emit walk — and none reaches
# jolt-enter-form!, which is what records a location. A failure in one printed
# "Unhandled exception: ..." over a trace of runtime procedure names and nothing
# about which file was being read; they record it with jolt-enter-file! now.
#
# This case drives the load walk, which could always report a location — the
# no-eval walks have no fault left to reach them with, now that the reader's
# duplicate check no longer fires in scan mode, and that is the point of the fix
# below. It gates the reporting path itself, which all four share.
echo "build smoke: build failure names the source file"
badsrc="$(dirname "$out")/badread"; mkdir -p "$badsrc/src/app"
printf '{}\n' > "$badsrc/deps.edn"
printf '(ns app.core (:require [app.broke]))\n(defn -main [& _] (println :x))\n' > "$badsrc/src/app/core.clj"
printf '(ns app.broke)\n(defn f [] (+ 1 2)\n' > "$badsrc/src/app/broke.clj"
read_err="$(JOLT_PWD="$badsrc" "$joltabs" build -m app.core -o "$(dirname "$out")/badread-bin" 2>&1 || true)"
if ! printf '%s' "$read_err" | grep -qE '^  (at|-->) .*app/broke\.clj'; then
  echo "  FAIL: build failure did not name src/app/broke.clj"
  echo "--- got ---"; echo "$read_err"; exit 1
fi

# A compile error names the line of the OFFENDING form, and prints no trace.
#
# The reporter can only do either when the throw carries a :jolt.error/kind, and
# only the unresolved-symbol diagnostic built one. Everything else raised while
# analyzing — an uncompilable form, a destructuring pattern the desugarer rejects,
# a macro that threw expanding — arrived bare, so the report was the LOADER's
# per-top-level-form position (a long defn's opening line) above thirty lines of
# analyze-list/map-seq/seq->list internals naming nothing the reader can act on.
# The bad pattern below sits on line 7 of an fn opening on line 3, so the two
# positions are distinguishable.
echo "build smoke: compile error names the offending form's line"
badpos="$(dirname "$out")/badpos"; mkdir -p "$badpos/src/app"
printf '{}\n' > "$badpos/deps.edn"
{ echo '(ns app.core)'
  echo ''
  echo '(defn -main [& _]'
  echo '  (println :a)'
  echo '  (println :b)'
  echo '  (println :c)'
  echo '  (let [(a b) [1 2]]'
  echo '    (println a b)))'
} > "$badpos/src/app/core.clj"
pos_err="$(JOLT_PWD="$badpos" "$joltabs" build -m app.core -o "$(dirname "$out")/badpos-bin" 2>&1 || true)"
if ! printf '%s' "$pos_err" | grep -qE '^  (at|-->) .*app/core\.clj:7:'; then
  echo "  FAIL: compile error did not name app/core.clj line 7 (the let it is in)"
  echo "--- got ---"; echo "$pos_err"; exit 1
fi
if printf '%s' "$pos_err" | grep -q '^  trace:'; then
  echo "  FAIL: compile error printed the analyzer's own frames"
  echo "--- got ---"; echo "$pos_err"; exit 1
fi

# The build reads a namespace's forms through its own read-all, which had the same
# stray-close-delimiter blindness the loader did: the position parks on the paren,
# the loop reads that as end of input, and the image is emitted from the forms
# BEFORE it — a successful build of a program missing everything after the typo.
echo "build smoke: a stray close paren fails the build"
stray="$(dirname "$out")/stray"; mkdir -p "$stray/src/app"
printf '{}\n' > "$stray/deps.edn"
{ echo '(ns app.core)'
  echo ''
  echo '(defn -main [& _]'
  echo '  (println :a)))'
  echo ''
  echo '(defn unreachable [] :nope)'
} > "$stray/src/app/core.clj"
stray_err="$(JOLT_PWD="$stray" "$joltabs" build -m app.core -o "$(dirname "$out")/stray-bin" 2>&1 || true)"
if ! printf '%s' "$stray_err" | grep -q 'Unmatched delimiter'; then
  echo "  FAIL: build did not report the stray close paren"
  echo "--- got ---"; echo "$stray_err"; exit 1
fi
if [ -x "$(dirname "$out")/stray-bin" ]; then
  echo "  FAIL: build produced a binary from a file it could not read"; exit 1
fi

# A set literal mixing an auto keyword with a plain one of the same alias text
# must BUILD. ::o/x and :o/x are distinct, but the require scan reads in scan mode,
# where an unresolved alias keeps its text, so both read as :o/x — and the
# duplicate-literal check rejected the file. The namespace loaded fine, so this was
# a build that failed on a program that ran.
echo "build smoke: scan-mode alias placeholders do not collide"
aliasout="$(dirname "$out")/alias-set-bin"
if ! JOLT_PWD="$root/test/chez/alias-set-app" "$joltabs" build -m app.core -o "$aliasout" >/dev/null 2>&1; then
  echo "  FAIL: alias-set-app build exited non-zero"; exit 1
fi
got_alias="$(cd / && "$aliasout" 2>&1 | tail -1)"
if [ "$got_alias" != "2" ]; then
  echo "  FAIL: alias-set-app — want 2, got \`$got_alias\`"; exit 1
fi

# :as-alias through a build. clojure.core's load-lib aliases the target WITHOUT
# loading it (need-ns is `(or as use)`, falling to create-ns), for a namespace that
# may not exist yet or exists only to qualify keywords. The build has to agree: the
# require scan must not count an alias-only spec as a dependency (or the target is
# emitted and its top level runs in the binary), and the emitted ns prelude must
# still replay the alias. jolt used to get both halves wrong.
echo "build smoke: :as-alias aliases without pulling the target in"
aaout="$(dirname "$out")/as-alias-bin"
if ! JOLT_PWD="$root/test/chez/as-alias-app" "$joltabs" build -m app.core -o "$aaout" >/dev/null 2>&1; then
  echo "  FAIL: as-alias-app build exited non-zero"; exit 1
fi
if grep -q 'set-chez-ns! "app.other"' "$aaout.build/flat.ss"; then
  echo "  FAIL: :as-alias pulled app.other into the binary"; exit 1
fi
got_aa="$(cd / && "$aaout" 2>&1)"
if [ "$got_aa" != ":kw :app.other/x :map 1 :aliased true" ]; then
  echo "  FAIL: as-alias-app — want ':kw :app.other/x :map 1 :aliased true', got \`$got_aa\`"; exit 1
fi

# --- split flat source + cached runtime fasl ---------------------------------
# The runtime half of the flat source is app-independent, so it is emitted to its
# own runtime.ss, compiled once per (content, mode), and the fasl kept — most of a
# small app's build is that one compile. Three things have to hold: the split
# really happened (the runtime's defines are NOT in flat.ss), a second build reuses
# the cached fasl, and a binary from the cached fasl behaves exactly like one built
# without splitting at all. The last is the point: a stale or mismatched cached
# runtime would produce a subtly wrong binary rather than a failed build.
echo "build smoke: split flat source + runtime fasl cache"
splitout="$(dirname "$out")/split-bin"
cachedir="$(dirname "$out")/rtcache"
if ! JOLT_PWD="$app" JOLT_RUNTIME_CACHE_DIR="$cachedir" "$joltabs" build -m app.core -o "$splitout" >/dev/null 2>&1; then
  echo "  FAIL: split build exited non-zero"; exit 1
fi
[ -f "$splitout.build/runtime.ss" ] || { echo "  FAIL: no runtime.ss — the split did not happen"; exit 1; }
# clojure.core lives in the runtime half only; finding it in flat.ss means the app
# half still carries the runtime and nothing was actually separated.
if grep -q 'def-var! "clojure.core" "group-by"' "$splitout.build/flat.ss"; then
  echo "  FAIL: runtime defs still in flat.ss after the split"; exit 1
fi
if [ "$(ls "$cachedir"/*.so 2>/dev/null | wc -l | tr -d ' ')" != "1" ]; then
  echo "  FAIL: the first build cached no runtime fasl"; exit 1
fi
# second build: same cache dir, so the runtime compile must be skipped entirely
rtmtime_before="$(ls -l "$cachedir"/*.so | awk '{print $6, $7, $8}')"
splitout2="$(dirname "$out")/split-bin2"
if ! JOLT_PWD="$app" JOLT_RUNTIME_CACHE_DIR="$cachedir" JOLT_BUILD_PROFILE=1 "$joltabs" build -m app.core -o "$splitout2" 2>"$cachedir/prof.log" >/dev/null; then
  echo "  FAIL: second (cached) split build exited non-zero"; exit 1
fi
if ! grep -q 'runtime fasl (cached)' "$cachedir/prof.log"; then
  echo "  FAIL: second build did not reuse the cached runtime fasl"
  sed -n 's/^jolt build: \[profile\]/    /p' "$cachedir/prof.log"
  exit 1
fi
# JOLT_NO_FLAT_SPLIT builds the one-file form: the escape hatch has to still work,
# and its binary is the reference the split one is compared against.
nosplitout="$(dirname "$out")/nosplit-bin"
if ! JOLT_PWD="$app" JOLT_NO_FLAT_SPLIT=1 "$joltabs" build -m app.core -o "$nosplitout" >/dev/null 2>&1; then
  echo "  FAIL: JOLT_NO_FLAT_SPLIT build exited non-zero"; exit 1
fi
[ -f "$nosplitout.build/runtime.ss" ] && { echo "  FAIL: JOLT_NO_FLAT_SPLIT still split the source"; exit 1; }
got_split="$(cd / && "$splitout" alpha bb ccc 2>&1)"
got_split2="$(cd / && "$splitout2" alpha bb ccc 2>&1)"
got_nosplit="$(cd / && "$nosplitout" alpha bb ccc 2>&1)"
if [ "$got_split" != "$want" ] || [ "$got_split2" != "$want" ] || [ "$got_nosplit" != "$want" ]; then
  echo "  FAIL: split/cached/unsplit binaries disagree with the reference output"
  echo "--- want ---";        echo "$want"
  echo "--- split ---";       echo "$got_split"
  echo "--- split cached ---";echo "$got_split2"
  echo "--- unsplit ---";     echo "$got_nosplit"
  exit 1
fi

# --boot picks how the boot image is encoded (jolt-lang/jolt#886): `fast` (the
# default) is vfasl+LZ4, `small` is vfasl+gzip, `plain` skips vfasl entirely.
# --no-vfasl is the spelling the issue asked for and aliases `--boot plain`.
#
# Checked by the ARTIFACT, not by a message — a build that quietly converted
# anyway is exactly the failure `plain` exists to prevent — then by the ordering
# of the three binaries' sizes, which is what says the codec really changed and
# not just the filename, and finally by RUNNING each, since a binary that does
# not boot is the other failure.
echo "build smoke: --boot fast|small|plain"
smallout="$(dirname "$out")/small-boot-bin"
plainout="$(dirname "$out")/plain-boot-bin"
envplainout="$(dirname "$out")/envplain-boot-bin"
if ! JOLT_PWD="$app" "$joltabs" build -m app.core --boot small -o "$smallout" >/dev/null 2>&1; then
  echo "  FAIL: --boot small build exited non-zero"; exit 1
fi
[ -f "$smallout.build/jolt.boot.vfasl" ] || { echo "  FAIL: --boot small produced no vfasl boot"; exit 1; }
if ! JOLT_PWD="$app" "$joltabs" build -m app.core --boot plain -o "$plainout" >/dev/null 2>&1; then
  echo "  FAIL: --boot plain build exited non-zero"; exit 1
fi
[ -f "$plainout.build/jolt.boot.vfasl" ] && { echo "  FAIL: --boot plain still converted the boot"; exit 1; }
# --no-vfasl and JOLT_NO_VFASL are aliases for `--boot plain`; the env var
# travels a different path than the flag, so it gets its own build.
if ! JOLT_PWD="$app" JOLT_NO_VFASL=1 "$joltabs" build -m app.core -o "$envplainout" >/dev/null 2>&1; then
  echo "  FAIL: JOLT_NO_VFASL build exited non-zero"; exit 1
fi
[ -f "$envplainout.build/jolt.boot.vfasl" ] && { echo "  FAIL: JOLT_NO_VFASL still converted the boot"; exit 1; }
# the default still converts, or the `plain` checks above pass for the wrong
# reason the day something stops emitting a vfasl boot at all.
[ -f "$splitout.build/jolt.boot.vfasl" ] || { echo "  FAIL: the default build produced no vfasl boot"; exit 1; }
# A gzip image is a third smaller than either of the others; if `small` merely
# fell back to the LZ4 default this ordering is what catches it.
sz_small=$(wc -c < "$smallout"); sz_fast=$(wc -c < "$splitout"); sz_plain=$(wc -c < "$plainout")
if [ "$sz_small" -ge "$sz_fast" ] || [ "$sz_small" -ge "$sz_plain" ]; then
  echo "  FAIL: --boot small ($sz_small) is not smaller than fast ($sz_fast) and plain ($sz_plain)"
  exit 1
fi
# a bad value is rejected rather than silently building the default
if JOLT_PWD="$app" "$joltabs" build -m app.core --boot nope -o "$plainout.bad" >/dev/null 2>&1; then
  echo "  FAIL: --boot nope was accepted"; exit 1
fi
# An empty environment variable reads as UNSET. `bin/jolt` already treats
# JOLT_NO_DEVCACHE that way, and a CI matrix leg that does not fill a value in
# exports an empty one — which must not fail the build (JOLT_BOOT= used to, with
# "must be fast, small or plain (got )") nor silently change it (JOLT_NO_VFASL=
# used to force plain).
emptybootout="$(dirname "$out")/emptyboot-bin"
emptynvout="$(dirname "$out")/emptynv-bin"
if ! JOLT_PWD="$app" JOLT_BOOT= "$joltabs" build -m app.core -o "$emptybootout" >/dev/null 2>&1; then
  echo "  FAIL: an empty JOLT_BOOT failed the build"; exit 1
fi
[ -f "$emptybootout.build/jolt.boot.vfasl" ] || { echo "  FAIL: an empty JOLT_BOOT did not build the default"; exit 1; }
if ! JOLT_PWD="$app" JOLT_NO_VFASL= "$joltabs" build -m app.core -o "$emptynvout" >/dev/null 2>&1; then
  echo "  FAIL: an empty JOLT_NO_VFASL failed the build"; exit 1
fi
[ -f "$emptynvout.build/jolt.boot.vfasl" ] || { echo "  FAIL: an empty JOLT_NO_VFASL forced plain"; exit 1; }
# Within one source the explicit --boot spelling beats the --no-vfasl alias, in
# either order: a script that adds --boot small without dropping its old
# --no-vfasl is the migration #886 is on, and it used to silently get `plain`.
precout="$(dirname "$out")/prec-boot-bin"
if ! JOLT_PWD="$app" "$joltabs" build -m app.core --no-vfasl --boot small -o "$precout" >/dev/null 2>&1; then
  echo "  FAIL: --no-vfasl --boot small build exited non-zero"; exit 1
fi
[ -f "$precout.build/jolt.boot.vfasl" ] || { echo "  FAIL: --boot small lost to the --no-vfasl alias"; exit 1; }
got_small="$(cd / && "$smallout" alpha bb ccc 2>&1)"
got_plain="$(cd / && "$plainout" alpha bb ccc 2>&1)"
got_envplain="$(cd / && "$envplainout" alpha bb ccc 2>&1)"
if [ "$got_small" != "$want" ] || [ "$got_plain" != "$want" ] || [ "$got_envplain" != "$want" ]; then
  echo "  FAIL: --boot binaries disagree with the reference output"
  echo "--- want ---";        echo "$want"
  echo "--- small ---";       echo "$got_small"
  echo "--- plain ---";       echo "$got_plain"
  echo "--- JOLT_NO_VFASL ---"; echo "$got_envplain"
  exit 1
fi


# --- the compiler verdict, per program shape ---------------------------------
# Every build (not only --tree-shake) drops the compiler when nothing reaches
# it. Four shapes pin the edges of "reaches": an eval in a def -main never
# references (its init still RUNS at start, so the verdict roots every def, not
# only -main's reach); a bare :& FFI binding, which compiles a foreign-procedure
# per tail shape at the call and petite cannot; jolt.scheme/proc, a top-level
# lookup that needs no compiler and so must answer from the runtime half; and
# jolt.scheme/eval-string, which does compile. Each is its own entry namespace
# of one fixture, so no shape masks another.
echo "build smoke: compiler verdict (unreached eval def, bare :& ffi, jolt.scheme)"
verdictapp="$root/test/chez/verdict-app"
verdict_case() { # name entry-ns want-output keep|drop
  vout="$(dirname "$out")/verdict-$1"
  if ! JOLT_PWD="$verdictapp" "$jolt" build -m "$2" -o "$vout" >"$vout.log" 2>&1; then
    echo "  FAIL: verdict fixture $1 ($2) did not build"; tail -5 "$vout.log"; exit 1
  fi
  vgot="$(cd / && "$vout" 2>&1 | tail -1)"
  if [ "$vgot" != "$3" ]; then
    echo "  FAIL: verdict fixture $1 — want '$3', got \`$vgot\`"; exit 1
  fi
  if grep -q '^jolt build: dropping compiler image' "$vout.log"; then vhad=drop; else vhad=keep; fi
  if [ "$vhad" != "$4" ]; then
    echo "  FAIL: verdict fixture $1 — the build should $4 the compiler, it chose $vhad"; exit 1
  fi
}
verdict_case evaldef verdict.evaldef "VERDICT-EVALDEF ok" keep
verdict_case varargs verdict.varargs "VERDICT-VARARGS ok" keep
verdict_case sproc   verdict.sproc   "VERDICT-SPROC 42"   drop
verdict_case seval   verdict.seval   "VERDICT-SEVAL 42"   keep
# A require by a COMPUTED name loads source at run time, so the verdict must
# keep the compiler; the binary runs from the project dir, where its source
# root is, since the namespace it loads is deliberately not in the binary.
vout="$(dirname "$out")/verdict-dynreq"
if ! JOLT_PWD="$verdictapp" "$jolt" build -m verdict.dynreq -o "$vout" >"$vout.log" 2>&1; then
  echo "  FAIL: verdict fixture dynreq did not build"; tail -5 "$vout.log"; exit 1
fi
if grep -q '^jolt build: dropping compiler image' "$vout.log"; then
  echo "  FAIL: verdict fixture dynreq — a require by a computed name must keep the compiler"; exit 1
fi
vgot="$(cd "$verdictapp" && "$vout" 2>&1 | tail -1)"
if [ "$vgot" != "VERDICT-DYNREQ true" ]; then
  echo "  FAIL: verdict fixture dynreq — want 'VERDICT-DYNREQ true', got \`$vgot\`"; exit 1
fi

echo "build smoke: passed (release + optimized + direct-link + tree-shake + compiler+core shake + data-reader + no-main + optional-native + deps-opt + cljc-cond + jolt-ext + vendored-fs + petite-only-fs + vendored-process + petite-only-process + ffi-clj-layer + petite-only-ffi + declare-only-var + install-owned-order + split-provider-order + embedded-value + sdeps-before-build + source-mode-driver + build-error-location + compile-error-position + scan-alias-set + as-alias + flat-split + runtime-cache + boot-modes + compiler-verdict)"
