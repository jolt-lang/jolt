#!/bin/sh
# compile-path smoke: clojure.core/compile writes an artifact under *compile-path*
# and a LATER PROCESS loads it instead of the source.
#
# The unit gate covers compile in-process; what it cannot reach is the point of
# the feature — compile once, then run without recompiling, or without the source
# present at all. Each phase is a separate jolt process.
#
# Layout, mirroring the JVM's "that directory too must be in the classpath": the
# sources live in $tmp (root $tmp/src) and the artifacts go to $tmp/out/src, a
# second :local/root project so add-deps puts it on the source roots.

set -e

pass=0
fails=0
root="$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)"
cd "$root"

jolt="${JOLT_BIN:-target/release/jolt}"
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT
mkdir -p "$tmp/src/cpsmoke" "$tmp/out"
printf '{}' > "$tmp/deps.edn"
printf '{}' > "$tmp/out/deps.edn"

# The AOT cache would answer a second load on its own; keep it off so every
# result here is attributable to the compile-path artifact.
run() {
  JOLT_AOT_CACHE=0 JOLT_QUIET=1 "$jolt" -e "
    (require 'jolt.deps)
    (jolt.deps/add-deps {:deps {'cpsmoke/cpsmoke {:local/root \"$tmp\"}}})
    (jolt.deps/add-deps {:deps {'cpout/cpout {:local/root \"$tmp/out\"}}})
    $1" 2>&1
}

check() { # label expected actual
  if [ "$2" = "$3" ]; then
    echo "PASS: $1"; pass=$((pass+1))
  else
    echo "FAIL: $1 — expected '$2', got '$3'"; fails=$((fails+1))
  fi
}

cat > "$tmp/src/cpsmoke/util.clj" <<'CLJ'
(ns cpsmoke.util)
(defmacro scale [x] `(* 2 ~x))
CLJ
cat > "$tmp/src/cpsmoke/core.clj" <<'CLJ'
(ns cpsmoke.core (:require [cpsmoke.util :as u]))
(defn v [] (u/scale 21))
CLJ

# --- (a) compile writes the artifact and returns the lib ----------------------
out_a="$(run "(println (binding [*compile-path* \"$tmp/out/src\"] (compile 'cpsmoke.core)))" | tail -1)"
check "(a) compile returns the lib symbol" "cpsmoke.core" "$out_a"
if [ -f "$tmp/out/src/cpsmoke/core.so" ] && [ -f "$tmp/out/src/cpsmoke/core.meta" ]; then
  echo "PASS: (a2) .so and .meta written under *compile-path*"; pass=$((pass+1))
else
  echo "FAIL: (a2) no artifact under $tmp/out/src/cpsmoke"; fails=$((fails+1))
fi

# --- (b) a later process loads the artifact, not the source -------------------
out_b="$(run "(require 'cpsmoke.core :verbose) (println (cpsmoke.core/v))" | tail -1)"
check "(b) later process gets the compiled value" "42" "$out_b"
loaded_from="$(run "(require 'cpsmoke.core :verbose)" | grep 'Loading cpsmoke.core' || true)"
case "$loaded_from" in
  *out/src/cpsmoke/core) echo "PASS: (b2) loaded from the artifact"; pass=$((pass+1)) ;;
  *) echo "FAIL: (b2) expected a load from out/src, got '$loaded_from'"; fails=$((fails+1)) ;;
esac

# --- (c) with no source at all, the artifact still loads ----------------------
mv "$tmp/src" "$tmp/src-hidden"
out_c="$(run "(require 'cpsmoke.core) (println (cpsmoke.core/v))" | tail -1)"
check "(c) artifact loads with the source removed" "42" "$out_c"
mv "$tmp/src-hidden" "$tmp/src"

# --- (d) an edited source beats the artifact ----------------------------------
cat > "$tmp/src/cpsmoke/core.clj" <<'CLJ'
(ns cpsmoke.core (:require [cpsmoke.util :as u]))
(defn v [] (u/scale 50))
CLJ
out_d="$(run "(require 'cpsmoke.core) (println (cpsmoke.core/v))" | tail -1)"
check "(d) edited source wins over the stale artifact" "100" "$out_d"

# --- (e) editing a REQUIRED namespace invalidates too -------------------------
# The artifact baked cpsmoke.util's macro expansion, so its own source hash does
# not describe it; .meta records the direct requires for exactly this case.
cat > "$tmp/src/cpsmoke/core.clj" <<'CLJ'
(ns cpsmoke.core (:require [cpsmoke.util :as u]))
(defn v [] (u/scale 21))
CLJ
run "(binding [*compile-path* \"$tmp/out/src\"] (compile 'cpsmoke.core))" >/dev/null
cat > "$tmp/src/cpsmoke/util.clj" <<'CLJ'
(ns cpsmoke.util)
(defmacro scale [x] `(* 3 ~x))
CLJ
out_e="$(run "(require 'cpsmoke.core) (println (cpsmoke.core/v))" | tail -1)"
check "(e) edited dependency invalidates the artifact" "63" "$out_e"

# --- (f) an artifact from another runtime is refused, and says so -------------
run "(binding [*compile-path* \"$tmp/out/src\"] (compile 'cpsmoke.core))" >/dev/null
# rewrite the fingerprint line (2) to something no runtime produces
awk 'NR==2 {print "DEADBEEF-00000000"; next} {print}' \
  "$tmp/out/src/cpsmoke/core.meta" > "$tmp/out/src/cpsmoke/core.meta.new"
mv "$tmp/out/src/cpsmoke/core.meta.new" "$tmp/out/src/cpsmoke/core.meta"
out_f="$(run "(require 'cpsmoke.core) (println (cpsmoke.core/v))" | tail -1)"
check "(f) refused artifact falls back to source" "63" "$out_f"
mv "$tmp/src" "$tmp/src-hidden"
err_f="$(run "(require 'cpsmoke.core)" || true)"
case "$err_f" in
  *"recompile it"*) echo "PASS: (f2) source-less refused artifact explains itself"; pass=$((pass+1)) ;;
  *) echo "FAIL: (f2) expected a recompile hint, got '$err_f'"; fails=$((fails+1)) ;;
esac
mv "$tmp/src-hidden" "$tmp/src"

# --- (g) a nil *compile-path* is an error, like Compiler.compile --------------
err_g="$(run "(binding [*compile-path* nil] (compile 'cpsmoke.core))" || true)"
case "$err_g" in
  *"*compile-path* not set"*) echo "PASS: (g) nil *compile-path* reports it"; pass=$((pass+1)) ;;
  *) echo "FAIL: (g) expected '*compile-path* not set', got '$err_g'"; fails=$((fails+1)) ;;
esac

# --- (h) `jolt build` still works in a project that has been compiled ---------
# The build driver emits each namespace from its SOURCE. It merges a static
# require scan with the ns-loaded-hook's load order, and the hook is the ONLY
# source of a path for a namespace the scan can't see — a require inside a fn. If
# a compiled artifact answered that load, the emitter would be handed a path with
# no source behind it, so the loader suppresses artifacts for a build walk
# (ldr-source-only?). cpsmoke.dyn below is exactly that namespace.
# Needs a C toolchain; skipped like build-smoke without one.
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
  echo "SKIP: (h) jolt build over a compiled project (no Chez kernel dev files or cc)"
else
  export JOLT_CHEZ_CSV="$csv"
  cat > "$tmp/src/cpsmoke/dyn.clj" <<'CLJ'
(ns cpsmoke.dyn)
(defn w [] :dynamically-required)
CLJ
  cat >> "$tmp/src/cpsmoke/core.clj" <<'CLJ'
(defn- boot [] (require 'cpsmoke.dyn))
(boot)
(defn -main [& _] (println (v) ((resolve 'cpsmoke.dyn/w))))
CLJ
  run "(binding [*compile-path* \"$tmp/out/src\"] (compile 'cpsmoke.core))" >/dev/null
  if [ ! -f "$tmp/out/src/cpsmoke/dyn.so" ]; then
    echo "FAIL: (h0) compile did not carry through to the dynamically required ns"
    fails=$((fails+1))
  else
    echo "PASS: (h0) compile reached the dynamically required ns"; pass=$((pass+1))
  fi
  cat > "$tmp/deps.edn" <<EDN
{:paths ["src" "out/src"]}
EDN
  bin="$tmp/app-bin"
  if JOLT_PWD="$tmp" "$jolt" build -m cpsmoke.core -o "$bin" >/dev/null 2>&1 && [ -x "$bin" ]; then
    # Run it with BOTH the sources and the artifacts moved away. A namespace the
    # build silently dropped would otherwise be reloaded off disk at run time and
    # the binary would look fine.
    mv "$tmp/src" "$tmp/src-away"; mv "$tmp/out" "$tmp/out-away"
    out_h="$("$bin" 2>&1 | tail -1)"
    mv "$tmp/src-away" "$tmp/src"; mv "$tmp/out-away" "$tmp/out"
    check "(h) built binary is standalone" "63 :dynamically-required" "$out_h"
  else
    echo "FAIL: (h) jolt build failed in a project with compiled artifacts on a path"
    fails=$((fails+1))
  fi

  # --- (i) a build over a source-less compiled dep fails clearly --------------
  # ldr-source-only? is what turns this into a loader error naming the namespace;
  # without it the artifact answers the load, the emitter is handed a path with no
  # source at it, and the build dies inside read-file-string.
  rm -f "$tmp/src/cpsmoke/dyn.clj"
  err_i="$(JOLT_PWD="$tmp" "$jolt" build -m cpsmoke.core -o "$tmp/app-bin2" 2>&1 || true)"
  case "$err_i" in
    *"cpsmoke/dyn"*"a build emits from source"*)
      echo "PASS: (i) source-less compiled dep fails the build with a clear message"; pass=$((pass+1)) ;;
    *) echo "FAIL: (i) expected a loader error naming cpsmoke/dyn, got '$err_i'"; fails=$((fails+1)) ;;
  esac
fi

echo
echo "compile-path smoke: $pass passed, $fails failed"
[ "$fails" -eq 0 ] || exit 1
