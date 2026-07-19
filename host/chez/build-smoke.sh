#!/bin/sh
# build smoke: `jolt build` compiles a multi-namespace app (macro + cross-ns +
# clojure.string) into a standalone binary, which then runs with no jolt source
# or Chez install on the path — args reach -main, output matches.
root="$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)"
cd "$root"

# Preflight: a standalone build needs Chez's kernel dev files (libkernel.a +
# scheme.h) and a C compiler. A distro chezscheme package ships neither, so on
# such hosts (CI included) skip — like `certify` skips without Clojure. Pin the
# csv dir we validate so the build uses exactly it.
csv="$JOLT_CHEZ_CSV"
if [ -z "$csv" ]; then
  chez_bin="$(command -v chez || command -v chezscheme || command -v scheme || command -v petite || true)"
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
if ! JOLT_PWD="$app" bin/joltc build -m app.core -o "$out" >/dev/null 2>&1; then
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
if ! JOLT_PWD="$app" JOLT_NO_WP_INFER=1 bin/joltc build -m app.core -o "$out.noop" >/dev/null 2>&1; then
  echo "  FAIL: JOLT_NO_WP_INFER build exited non-zero"; exit 1
fi
default_fl=$(grep -c '#3%fl' "$out.build/flat.ss" || true)
noop_fl=$(grep -c '#3%fl' "$out.noop.build/flat.ss" || true)
if [ "$default_fl" -le "$noop_fl" ]; then
  echo "  FAIL: wp-infer added no fl-ops to the release build (default=$default_fl noop=$noop_fl)"; exit 1
fi

# --no-direct-link opts back out of the release default: the app->app call must
# NOT lower to a jv$ binding (stays var-routed, dynamically linked).
if ! JOLT_PWD="$app" bin/joltc build -m app.core -o "$out.nodl" --no-direct-link >/dev/null 2>&1; then
  echo "  FAIL: jolt build --no-direct-link exited non-zero"; exit 1
fi
if grep -q '(jv\$app.util\$shout' "$out.nodl.build/flat.ss"; then
  echo "  FAIL: --no-direct-link still direct-linked the app->app call"; exit 1
fi

# ^:redef / ^:dynamic opt out of direct-linking, so runtime redef/binding still
# take effect in the built binary even with direct-link the release default.
got_rd="$(cd / && "$out" --redef 2>&1)"
if ! printf '%s' "$got_rd" | grep -q '^redef: :patched$'    || ! printf '%s' "$got_rd" | grep -q '^dyn: :bound$'; then
  echo "  FAIL: ^:redef/:dynamic opt-out — want 'redef: :patched' and 'dyn: :bound' lines"
  echo "--- got ----"; echo "$got_rd"; exit 1
fi

# Portable embed: remove the build-time source tree and run from / — the
# embedded resource must still resolve (contents baked as literals, not
# read-file-string at startup).
echo "build smoke: portable-embed check"
app_copy="$(mktemp -d)/app-copy"
cp -R "$app" "$app_copy"
pe_out="$(dirname "$out")/pe-bin"
if ! JOLT_PWD="$app_copy" bin/joltc build -m app.core -o "$pe_out" >/dev/null 2>&1; then
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
if ! JOLT_PWD="$app" bin/joltc build -m app.core -o "$out" --opt >/dev/null 2>&1; then
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
if ! JOLT_PWD="$app" bin/joltc build -m app.core -o "$out" --direct-link >/dev/null 2>&1; then
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
inline_throw_app="$root/test/chez/inline-throw-app"
inline_throw_out="$(dirname "$out")/inline-throw-bin"
if ! JOLT_PWD="$inline_throw_app" bin/joltc build -m app.core -o "$inline_throw_out" --opt --direct-link >/dev/null 2>&1; then
  echo "  FAIL: inline-throw --opt --direct-link build exited non-zero"; exit 1
fi
inline_throw_got="$(cd "$inline_throw_app" && "$inline_throw_out" 2>&1)"
if [ "$inline_throw_got" != "THROW OK" ]; then
  echo "  FAIL: pure-fn fold discarded a throwing op — got \`$inline_throw_got\`, want THROW OK"; exit 1
fi

# Under --opt inference proves a nil-bound local is :nil, so nil? folds true and
# some? folds false. The fold was inverted (nil?->false, some?->true), so a release
# --opt build printed :b / :y instead of :a / :n.
nil_fold_app="$root/test/chez/nil-fold-app"
nil_fold_out="$(dirname "$out")/nil-fold-bin"
if ! JOLT_PWD="$nil_fold_app" bin/joltc build -m app.core -o "$nil_fold_out" --opt >/dev/null 2>&1; then
  echo "  FAIL: nil-fold --opt build exited non-zero"; exit 1
fi
nil_fold_got="$(cd "$nil_fold_app" && "$nil_fold_out" 2>&1)"
nil_fold_want="$(printf ':a\n:n')"
if [ "$nil_fold_got" != "$nil_fold_want" ]; then
  echo "  FAIL: nil?/some? fold inverted — got \`$nil_fold_got\`, want \`$nil_fold_want\`"; exit 1
fi

# A loop var that shadows a record-typed outer local must shadow in the inference
# tenv. The bug kept the outer type, so under --opt (:x p) devirtualized to a
# record slot read that crashed on the vector [3 4]; the fix keeps the loop p :any
# so (:x p) is a generic keyword lookup -> nil. The second line carries the record
# straight through to prove field reads still devirtualize.
loop_shadow_app="$root/test/chez/loop-shadow-app"
loop_shadow_out="$(dirname "$out")/loop-shadow-bin"
if ! JOLT_PWD="$loop_shadow_app" bin/joltc build -m app.core -o "$loop_shadow_out" --opt >/dev/null 2>&1; then
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
if ! JOLT_PWD="$min_max_app" bin/joltc build -m app.core -o "$min_max_out" --opt >/dev/null 2>&1; then
  echo "  FAIL: min-max --opt build exited non-zero"; exit 1
fi
min_max_got="$(cd "$min_max_app" && "$min_max_out" 2>&1)"
min_max_want="$(printf '1\n3')"
if [ "$min_max_got" != "$min_max_want" ]; then
  echo "  FAIL: min/max coerced int to double — got \`$min_max_got\`, want \`$min_max_want\`"; exit 1
fi

# A built binary runs -main with *ns* = user, like clojure.main — so a runtime
# resolve of an aliased symbol is nil (the alias lives in the entry ns, not user),
# matching the JVM and interpreted joltc rather than the entry ns's alias table. A
# separate app: `resolve` defeats tree-shaking, so keep it out of the shake test's
# app above.
nsp="$(dirname "$out")/nsparity"
mkdir -p "$nsp/src/nsp"
printf '{:paths ["src"]}\n' > "$nsp/deps.edn"
printf '(ns nsp.lib)\n(defn thing [] 1)\n' > "$nsp/src/nsp/lib.clj"
printf '(ns nsp.main (:require [nsp.lib :as l]))\n(defn -main [& _]\n  (println "ns:" (str *ns*))\n  (println "resolve:" (pr-str (resolve (quote l/thing))))\n  (println "ns-resolve:" (pr-str (ns-resolve (quote nsp.lib) (quote thing)))))\n' > "$nsp/src/nsp/main.clj"
nspout="$(dirname "$out")/nsparity-bin"
if ! JOLT_PWD="$nsp" bin/joltc build -m nsp.main -o "$nspout" >/dev/null 2>&1; then
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
if ! JOLT_PWD="$app" bin/joltc build -m app.core -o "$out" --tree-shake >/dev/null 2>&1; then
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
if ! JOLT_PWD="$drapp" bin/joltc build -m drtest.main -o "$drout" >/dev/null 2>&1; then
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
if ! JOLT_PWD="$nomain" bin/joltc build -m script -o "$nmout" >/dev/null 2>&1; then
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
if ! JOLT_PWD="$root/test/chez/optional-lib-app" bin/joltc build -m app.optional-lib -o "$olout" >/dev/null 2>&1; then
  echo "  FAIL: build with a missing optional native lib exited non-zero"; exit 1
fi
got_ol="$(cd / && "$olout" 2>&1)"
if [ "$got_ol" != "optional lib app ran successfully" ]; then
  echo "  FAIL: optional-lib binary — got \`$got_ol\`"; exit 1
fi
ocout="$(dirname "$out")/optional-call-bin"
if ! JOLT_PWD="$root/test/chez/optional-lib-call-app" bin/joltc build -m app.optional-lib-call -o "$ocout" >/dev/null 2>&1; then
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
modeline="$(JOLT_PWD="$optproj" bin/joltc build -m app -o "$opout" 2>&1 | grep 'compiling app (')"
case "$modeline" in
  *"(optimized mode"*) : ;;
  *) echo "  FAIL: deps.edn :jolt/build {:opt true} did not select optimized mode — got \`$modeline\`"; exit 1 ;;
esac

# A namespace with a cljs-only reader conditional (`#?(:cljs …)`) between two clj
# defns must not truncate emission at the conditional — the fn AFTER it must be
# emitted into the binary, or a call to it crashes on an unbound var.
ccapp="$root/test/chez/cljc-cond-app"
ccout="$(dirname "$out")/cljc-cond-bin"
if ! JOLT_PWD="$ccapp" bin/joltc build -m cljccond.main -o "$ccout" >/dev/null 2>&1; then
  echo "  FAIL: jolt build of a cljs-conditional app exited non-zero"; exit 1
fi
got_cc="$(cd / && "$ccout" 2>&1 | tail -1)"
if [ "$got_cc" != "CLJC-COND :before :after" ]; then
  echo "  FAIL: cljs-only conditional truncated emission — want 'CLJC-COND :before :after', got \`$got_cc\`"; exit 1
fi

# A built binary must have the vendored babashka.fs (via jolt.fs) available and
# runnable — including functions defined after babashka.fs's cljs-only reader
# conditionals (directory?/cwd/which). Guards the vendored-namespace baking.
fsapp="$root/test/chez/fs-app"
fsout="$(dirname "$out")/fs-app-bin"
if ! JOLT_PWD="$fsapp" bin/joltc build -m fsapp.main -o "$fsout" >/dev/null 2>&1; then
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
if ! JOLT_PWD="$fsapp" bin/joltc build -m fsapp.main -o "$fsshake" --tree-shake >/dev/null 2>&1; then
  echo "  FAIL: jolt build --tree-shake of the jolt.fs app exited non-zero"; exit 1
fi
if grep -q 'scheme.boot' "$fsshake.build/compile.ss" 2>/dev/null; then
  echo "  FAIL: tree-shaken fs app still bundles scheme.boot (petite-only boot expected)"; exit 1
fi
got_fss="$(cd / && "$fsshake" 2>&1 | tail -1)"
if [ "$got_fss" != "FS-APP a/b true true rw-------" ]; then
  echo "  FAIL: petite-only fs binary output mismatch — want 'FS-APP a/b true true rw-------', got \`$got_fss\`"; exit 1
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
if ! JOLT_PWD="$decl_app" bin/joltc build -m da.core -o "$decl_out" >/dev/null 2>&1; then
  echo "  FAIL: declaration-only-var app build exited non-zero"; exit 1
fi
got_decl="$(cd / && "$decl_out" 2>&1)"
rm -rf "$(dirname "$decl_app")"
if ! printf '%s' "$got_decl" | grep -q '^declared: true$' || ! printf '%s' "$got_decl" | grep -q '^dynvar: true$' || ! printf '%s' "$got_decl" | grep -q '^interned: true$'; then
  echo "  FAIL: declaration-only / no-root var not discoverable in AOT (find-var/ns-interns)"
  echo "--- got ----"; echo "$got_decl"; exit 1
fi

echo "build smoke: passed (release + optimized + direct-link + tree-shake + compiler+core shake + data-reader + no-main + optional-native + deps-opt + cljc-cond + vendored-fs + petite-only-fs + declare-only-var)"
