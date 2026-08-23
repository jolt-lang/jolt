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
greet-soft: greet:soft'
if [ "$got" != "$want" ]; then
  echo "  FAIL: binary output mismatch"
  echo "--- want ---"; echo "$want"
  echo "--- got ----"; echo "$got"
  exit 1
fi

# Startup profiling is one opt-in switch on the same binary. The ordinary run
# above is exact-output proof that it stays silent by default; an enabled run
# spans the native heap loader, app namespace initialization, and -main.
profiled="$(cd / && JOLT_STARTUP_PROFILE=1 "$out" alpha bb ccc 2>&1)"
for marker in \
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

# The cross-ns app.core -> app.util/shout call lowers to a direct jv$ binding in
# the plain release build, not var-deref.
if ! grep -q '(jv\$app.util\$shout' "$out.build/flat.ss"; then
  echo "  FAIL: release build did not direct-link the app->app call"; exit 1
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

# --no-direct-link opts back out of the release default: the app->app call must
# NOT lower to a jv$ binding (stays var-routed, dynamically linked).
if ! JOLT_PWD="$app" "$jolt" build -m app.core -o "$out.nodl" --no-direct-link >/dev/null 2>&1; then
  echo "  FAIL: jolt build --no-direct-link exited non-zero"; exit 1
fi
if grep -q '(jv\$app.util\$shout' "$out.nodl.build/flat.ss"; then
  echo "  FAIL: --no-direct-link still direct-linked the app->app call"; exit 1
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
if ! JOLT_PWD="$app" "$jolt" build -m app.core -o "$out" --opt >/dev/null 2>&1; then
  echo "  FAIL: jolt build --opt exited non-zero"; exit 1
fi
got_opt="$(cd / && "$out" alpha bb ccc 2>&1)"
if [ "$got_opt" != "$want" ]; then
  echo "  FAIL: --opt binary output mismatch"
  echo "--- got ----"; echo "$got_opt"
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
if ! grep -q '(jv\$app.util\$shout' "$out.build/flat.ss"; then
  echo "  FAIL: --direct-link did not emit a direct app->app call"; exit 1
fi
# A direct-link build registers fn sources, so an uncaught throw prints a Clojure
# stack trace mapping each native frame back to ns/name (file:line).
if ! grep -q 'jolt-register-source!' "$out.build/flat.ss"; then
  echo "  FAIL: --direct-link did not emit source registrations"; exit 1
fi
boom_err="$(cd / && "$out" --boom 2>&1 >/dev/null)"
for frame in 'app.util/deep-boom' 'app.util/mid-boom' 'app.core/-main'; do
  if ! printf '%s' "$boom_err" | grep -q "$frame"; then
    echo "  FAIL: stack trace missing frame $frame"
    echo "--- got ----"; echo "$boom_err"
    exit 1
  fi
done

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
# Tree-shaking (opt-in): same result, and an unreachable def (the `twice` macro,
# expanded at AOT and never called at runtime) is dropped.
if ! JOLT_PWD="$app" "$jolt" build -m app.core -o "$out" --tree-shake >/dev/null 2>&1; then
  echo "  FAIL: jolt build --tree-shake exited non-zero"; exit 1
fi
got_ts="$(cd / && "$out" alpha bb ccc 2>&1)"
if [ "$got_ts" != "$want" ]; then
  echo "  FAIL: --tree-shake binary output mismatch"
  echo "--- got ----"; echo "$got_ts"
  exit 1
fi
if grep -q 'def-var! "app.util" "twice"' "$out.build/flat.ss"; then
  echo "  FAIL: --tree-shake did not drop the unreachable twice macro"; exit 1
fi
# The app never evals, so the compiler image (analyzer/back end) is dropped.
if grep -q 'def-var! "jolt.analyzer"' "$out.build/flat.ss"; then
  echo "  FAIL: --tree-shake kept the compiler image in a no-eval app"; exit 1
fi
# Core is shaken: a clojure.core overlay fn this app never uses is dropped.
if grep -q 'def-var! "clojure.core" "group-by"' "$out.build/flat.ss"; then
  echo "  FAIL: --tree-shake kept an unreachable clojure.core fn (group-by)"; exit 1
fi
# A registered data reader that returns a CODE form must be compiled into the
# binary (the emit path applies it too, not just the interpreted loader): the
# datareader-app's #code literal builds to 42, not the literal list.
# Also exercises transitive reader requires: #my/rev calls app.readers/reverse-str
# which requires app.util, proving the require-graph closure pulls in helper
# namespaces reachable only through the data-readers table.
drapp="$root/test/chez/datareader-app"
drout="$(dirname "$out")/dr-bin"
if ! JOLT_PWD="$drapp" "$jolt" build -m drtest.main -o "$drout" >/dev/null 2>&1; then
  echo "  FAIL: jolt build of a data-reader app exited non-zero"; exit 1
fi
got_dr="$(cd / && "$drout" 2>&1)"
dr_want='42
olleh!'
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
fsshake="$(dirname "$out")/fs-app-shake-bin"
if ! JOLT_PWD="$fsapp" "$jolt" build -m fsapp.main -o "$fsshake" --tree-shake >/dev/null 2>&1; then
  echo "  FAIL: jolt build --tree-shake of the jolt.fs app exited non-zero"; exit 1
fi
if grep -q 'scheme.boot' "$fsshake.build/compile.ss" 2>/dev/null; then
  echo "  FAIL: tree-shaken fs app still bundles scheme.boot (petite-only boot expected)"; exit 1
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
if ! JOLT_PWD="$procapp" "$jolt" build -m procapp.main -o "$procshake" --tree-shake >/dev/null 2>&1; then
  echo "  FAIL: jolt build --tree-shake of the jolt.process app exited non-zero"; exit 1
fi
if grep -q 'scheme.boot' "$procshake.build/compile.ss" 2>/dev/null; then
  echo "  FAIL: tree-shaken process app still bundles scheme.boot (petite-only boot expected)"; exit 1
fi
got_procs="$(cd / && "$procshake" 2>&1 | tail -1)"
if [ "$got_procs" != "PROC-APP hi 0 143" ]; then
  echo "  FAIL: petite-only process binary output mismatch — want 'PROC-APP hi 0 143', got \`$got_procs\`"; exit 1
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
# prebuilt binary. Build one app through the source-mode driver too, so the
# bin/jolt path a developer actually runs stays gated here and not only as a
# side effect of devbootsmoke's cached-project-build case. Redundant when $jolt
# already is bin/jolt, so skip it then.
if [ "$jolt" != "bin/jolt" ]; then
  echo "build smoke: source-mode driver check"
  srcout="$(dirname "$out")/srcmode-bin"
  if ! JOLT_PWD="$root/test/chez/jolt-ext-app" bin/jolt build -m jxapp.main -o "$srcout" >/dev/null 2>&1; then
    echo "  FAIL: source-mode (bin/jolt) build exited non-zero"; exit 1
  fi
  got_src="$(cd / && "$srcout" 2>&1 | tail -1)"
  if [ "$got_src" != "JOLT-EXT BUILT! (:x :x)" ]; then
    echo "  FAIL: source-mode build output — want 'JOLT-EXT BUILT! (:x :x)', got \`$got_src\`"; exit 1
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
if ! printf '%s' "$read_err" | grep -q '^  at .*app/broke\.clj'; then
  echo "  FAIL: build failure did not name src/app/broke.clj"
  echo "--- got ---"; echo "$read_err"; exit 1
fi

# A compile error names the line of the OFFENDING form, and prints no trace.
#
# The reporter can only do either when the throw carries a :jolt/error map, and
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
if ! printf '%s' "$pos_err" | grep -q '^  at .*app/core\.clj:7:'; then
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
if [ "$got_aa" != ":kw :app.other/x :aliased true" ]; then
  echo "  FAIL: as-alias-app — want ':kw :app.other/x :aliased true', got \`$got_aa\`"; exit 1
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

echo "build smoke: passed (release + optimized + direct-link + tree-shake + compiler+core shake + data-reader + no-main + optional-native + deps-opt + cljc-cond + jolt-ext + vendored-fs + petite-only-fs + vendored-process + petite-only-process + declare-only-var + install-owned-order + sdeps-before-build + source-mode-driver + build-error-location + compile-error-position + scan-alias-set + as-alias + flat-split + runtime-cache)"
