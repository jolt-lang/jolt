#!/bin/sh
# static-native smoke: a project's :jolt/native lib with a :static archive is
# LINKED INTO the built binary (the default), so the binary calls the C function
# with no shared object on disk at runtime. --dynamic keeps the old behavior —
# load a shared object at runtime.
root="$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)"
cd "$root"

# JOLT_BIN overrides the jolt under test. The gate targets point it at the
# freshly built target/release/jolt: a `jolt build` costs ~2.5s through the
# prebuilt binary and ~12.5s through the source-mode driver, and this gate
# drives two of them. JOLT_BIN=bin/jolt forces script mode.
jolt="${JOLT_BIN:-bin/jolt}"
# Absolute form, for the cases that cd into a fixture directory first.
case "$jolt" in /*) joltabs="$jolt" ;; *) joltabs="$root/$jolt" ;; esac

# Preflight: needs cc (to build the test libs AND to cc-link the app) + Chez's
# kernel dev files, same as build-smoke. Skip otherwise (CI on a distro package).
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
  echo "static-native smoke: skipped (Chez kernel dev files or C compiler not available)"
  exit 0
fi
export JOLT_CHEZ_CSV="$csv"

case "$(uname -s)" in
  Darwin) plat=":darwin"; soext="dylib"; shared="-dynamiclib" ;;
  *)      plat=":linux";  soext="so";    shared="-shared" ;;
esac

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
app="$work/app"
mkdir -p "$app/src/app"

# 1. a trivial C library, built BOTH as a static archive and a shared object.
cat > "$work/greet.c" <<'EOF'
int jolt_static_answer(void) { return 42; }
EOF
cc -c "$work/greet.c" -o "$work/greet.o"
ar rcs "$work/libgreet.a" "$work/greet.o"
cc $shared "$work/greet.c" -o "$work/libgreet.$soext"

# 2. an app that binds that symbol via FFI.
cat > "$app/src/app/core.clj" <<'EOF'
(ns app.core
  (:require [jolt.ffi :as ffi]))
(ffi/defcfn answer "jolt_static_answer" [] :int)
(defn -main [& _]
  (println "answer:" (answer)))
EOF

out="$work/app-bin"

# --- default: static link ---------------------------------------------------
# A static-only spec (no runtime candidate): the build resolves the symbol by
# preloading the archive, and the binary links it in — nothing to load at runtime.
cat > "$app/deps.edn" <<EOF
{:paths ["src"]
 :jolt/native [{:name "greet" :static {:archive "$work/libgreet.a"}}]}
EOF
echo "static-native smoke: building (default: static link)"
if ! JOLT_PWD="$app" "$jolt" build -m app.core -o "$out" >"$work/build.log" 2>&1; then
  echo "  FAIL: jolt build (static) exited non-zero"; cat "$work/build.log"; exit 1
fi
[ -x "$out" ] || { echo "  FAIL: no executable produced"; exit 1; }
# A static lib emits a process-symbol load (its archive is in-process), not a
# dlopen of the shared object.
if ! grep -q "jolt-build-load-native '() #f #t" "$out.build/flat.ss"; then
  echo "  FAIL: static native did not emit a process-symbol load"; exit 1
fi
if grep -q "libgreet.$soext" "$out.build/flat.ss"; then
  echo "  FAIL: static native baked a runtime shared-object load"; exit 1
fi
# Remove BOTH libs: a static-linked symbol lives in the binary, nothing to load.
rm -f "$work/libgreet.a" "$work/libgreet.$soext" "$work/greet.o"
got="$(cd / && "$out" 2>&1)"
if [ "$got" != "answer: 42" ]; then
  echo "  FAIL: static-linked binary output mismatch"
  echo "--- want ---"; echo "answer: 42"; echo "--- got ----"; echo "$got"; exit 1
fi

# --- a static archive that is not position-independent (jolt#1060) ----------
# gcc on most Linux distributions links PIE by default, and an archive compiled
# without -fPIC cannot go into a position-independent executable:
#
#   relocation R_X86_64_32 against `.rodata' can not be used when making a PIE
#   object; recompile with -fPIE
#
# Two archives in this link can be in that state and neither is the app's doing:
# the Chez kernel the self-contained jolt carries (built on an image whose gcc
# had no PIE default) and a :static native compiled the same way. The link falls
# back to -no-pie (build.ss bld-link-executable), so the build must succeed.
# Linux only: -fno-pie means nothing where there is no PIE default to undo, and
# arm64 macOS has no non-PIC form at all.
if [ "$(uname -s)" = Linux ] && cc -no-pie -E -x c /dev/null -o /dev/null 2>/dev/null; then
  # A leaf function is position-independent by accident — take the address of
  # static data, which is what gets the absolute relocation.
  cat > "$work/nopic.c" <<'EOF'
static const char greeting[] = "static";
const char *jolt_static_greeting(void) { return greeting; }
int jolt_static_answer(void) { return 42; }
EOF
  cc -fno-pie -fno-PIC -c "$work/nopic.c" -o "$work/nopic.o"
  ar rcs "$work/libnopic.a" "$work/nopic.o"
  cat > "$app/deps.edn" <<EOF
{:paths ["src"]
 :jolt/native [{:name "nopic" :static {:archive "$work/libnopic.a"}}]}
EOF
  rm -rf "$app/.jolt"
  echo "static-native smoke: building (non-PIC static archive)"
  if ! JOLT_PWD="$app" "$jolt" build -m app.core -o "$out" >"$work/build.log" 2>&1; then
    echo "  FAIL: jolt build with a non-PIC static archive exited non-zero (jolt#1060)"
    cat "$work/build.log"; exit 1
  fi
  # The archive cannot be preloaded as a shared object either (no flag makes an
  # absolute relocation work in a library the loader maps anywhere), so the build
  # says what it gave up rather than failing.
  if ! grep -q "symbols cannot be resolved while the build runs" "$work/build.log"; then
    echo "  FAIL: the build did not report the archive it could not preload"
    cat "$work/build.log"; exit 1
  fi
  # Where the compiler links PIE by default (__PIE__), the first link must have
  # failed and the -no-pie retry must be what produced the binary.
  if echo | cc -E -dM -x c - 2>/dev/null | grep -q '__PIE__'; then
    if ! grep -q 'relinking with -no-pie' "$work/build.log"; then
      echo "  FAIL: a PIE-by-default toolchain linked a non-PIC archive without the -no-pie retry"
      cat "$work/build.log"; exit 1
    fi
  fi
  got="$(cd / && "$out" 2>&1)"
  if [ "$got" != "answer: 42" ]; then
    echo "  FAIL: non-PIC static archive binary output mismatch"
    echo "--- got ----"; echo "$got"; exit 1
  fi
  rm -rf "$app/.jolt"
fi

# --- --dynamic: runtime load ------------------------------------------------
# Rebuild the shared object (static phase deleted it) and give the spec a runtime
# candidate; --dynamic loads it at startup instead of linking the archive.
cc $shared "$work/greet.c" -o "$work/libgreet.$soext"
cat > "$app/deps.edn" <<EOF
{:paths ["src"]
 :jolt/native [{:name "greet"
                :static {:archive "$work/libgreet.a"}
                $plat ["$work/libgreet.$soext"]}]}
EOF
echo "static-native smoke: building (--dynamic: runtime load)"
if ! JOLT_PWD="$app" "$jolt" build -m app.core -o "$out" --dynamic >"$work/build.log" 2>&1; then
  echo "  FAIL: jolt build --dynamic exited non-zero"; cat "$work/build.log"; exit 1
fi
# --dynamic loads the shared object at runtime.
if ! grep -q "libgreet.$soext" "$out.build/flat.ss"; then
  echo "  FAIL: --dynamic did not emit a runtime shared-object load"; exit 1
fi
got="$(cd / && "$out" 2>&1)"
if [ "$got" != "answer: 42" ]; then
  echo "  FAIL: --dynamic binary output mismatch (shared object present)"
  echo "--- got ----"; echo "$got"; exit 1
fi
# With the shared object gone, a --dynamic binary must FAIL — proving the symbol
# was loaded at runtime, not baked in.
rm -f "$work/libgreet.$soext"
rc=0; { (cd / && exec "$out"); } >/dev/null 2>&1 || rc=$?
if [ "$rc" -eq 0 ]; then
  echo "  FAIL: --dynamic binary still ran with its shared object removed"; exit 1
fi

# --- a relative :static archive resolves against the DECLARING deps.edn -----
# A project ships its archive beside its own sources ("native/libfoo.a", which is
# what a build task produces), and a DEPENDENCY does the same in its own tree.
# Both paths went to cc verbatim, so they resolved against the build's cwd: a
# dependency's could never work, and the project's own only worked when the build
# happened to run from the project dir — which bin/jolt, which cd's to the jolt
# tree, never does (jolt-9a8). Built from / here so a cwd-relative resolution
# cannot pass by accident.
cc -c "$work/greet.c" -o "$work/greet.o"

# 1. the PROJECT's own, relative.
mkdir -p "$app/native"
ar rcs "$app/native/libgreet.a" "$work/greet.o"
cat > "$app/deps.edn" <<'EOF'
{:paths ["src"]
 :jolt/native [{:name "greet" :static {:archive "native/libgreet.a"}}]}
EOF
echo "static-native smoke: building (project-relative archive, foreign cwd)"
if ! (cd / && JOLT_PWD="$app" "$joltabs" build -m app.core -o "$out") >"$work/build.log" 2>&1; then
  echo "  FAIL: jolt build with a project-relative archive exited non-zero"
  cat "$work/build.log"; exit 1
fi
got="$(cd / && "$out" 2>&1)"
if [ "$got" != "answer: 42" ]; then
  echo "  FAIL: project-relative archive binary output mismatch"
  echo "--- got ----"; echo "$got"; exit 1
fi

# 2. a TRANSITIVE dependency's, relative to that dependency's own root. app -> mid
#    -> leaf, and only leaf declares the native: nothing on the app's side names
#    the archive, so the root has to travel with the spec.
dep="$work/leaf"
mkdir -p "$dep/native" "$dep/src/leaf" "$work/mid/src/mid"
ar rcs "$dep/native/libgreet.a" "$work/greet.o"
cat > "$dep/deps.edn" <<'EOF'
{:paths ["src"]
 :jolt/native [{:name "greet" :static {:archive "native/libgreet.a"}}]}
EOF
cat > "$dep/src/leaf/core.clj" <<'EOF'
(ns leaf.core (:require [jolt.ffi :as ffi]))
(ffi/defcfn answer "jolt_static_answer" [] :int)
EOF
cat > "$work/mid/deps.edn" <<EOF
{:paths ["src"]
 :deps {org.example/leaf {:local/root "$dep"}}}
EOF
cat > "$work/mid/src/mid/core.clj" <<'EOF'
(ns mid.core (:require [leaf.core :as l]))
(defn go [] (l/answer))
EOF
rm -rf "$app/native" "$app/.jolt"
cat > "$app/deps.edn" <<EOF
{:paths ["src"]
 :deps {org.example/mid {:local/root "$work/mid"}}}
EOF
cat > "$app/src/app/core.clj" <<'EOF'
(ns app.core (:require [mid.core :as m]))
(defn -main [& _] (println "answer:" (m/go)))
EOF
echo "static-native smoke: building (transitive dep's relative archive)"
if ! (cd / && JOLT_PWD="$app" "$joltabs" build -m app.core -o "$out") >"$work/build.log" 2>&1; then
  echo "  FAIL: jolt build with a transitive dep's relative archive exited non-zero"
  cat "$work/build.log"; exit 1
fi
# the archive is linked in, so the binary runs with the dep tree gone
rm -rf "$dep/native"
got="$(cd / && "$out" 2>&1)"
if [ "$got" != "answer: 42" ]; then
  echo "  FAIL: transitive-dep archive binary output mismatch"
  echo "--- got ----"; echo "$got"; exit 1
fi

# --- a build says which natives stay dynamic --------------------------------
# Everything else a `jolt build` produces is IN the binary, so a lib that stayed
# dynamic is the one reason it is not the dependency-free artifact a static build
# is taken to be. The build names those rather than leaving it to be discovered
# on the target host; a fully static build says nothing.
# a PATH (it has a separator), so it resolves against the declaring dep's root —
# a bare name would be a soname for the loader to search for, which is dlopen's
# rule and deliberately left alone.
mkdir -p "$dep/native"
cc $shared "$work/greet.c" -o "$dep/native/libgreet.$soext"
cat > "$dep/deps.edn" <<EOF
{:paths ["src"]
 :jolt/native [{:name "greet" $plat ["native/libgreet.$soext"]}]}
EOF
rm -rf "$app/.jolt"
if ! (cd / && JOLT_PWD="$app" "$joltabs" build -m app.core -o "$out") >"$work/build.log" 2>&1; then
  echo "  FAIL: jolt build with a dynamic dep native exited non-zero"
  cat "$work/build.log"; exit 1
fi
if ! grep -q "loaded at runtime" "$work/build.log"; then
  echo "  FAIL: a build with a runtime-loaded native said nothing about it"
  cat "$work/build.log"; exit 1
fi
# The interpreted path resolves the same spec the same way: a DEPENDENCY's
# relative candidate is relative to that dependency, not to the app that pulled
# it in. It was resolved against the app's dir, where it is not.
echo "static-native smoke: running (transitive dep's relative shared object)"
got="$(cd / && JOLT_PWD="$app" "$joltabs" run -m app.core 2>&1)"
if [ "$got" != "answer: 42" ]; then
  echo "  FAIL: jolt run did not resolve a dep's relative shared object"
  echo "--- got ----"; echo "$got"; exit 1
fi
# and the fully static build above must NOT have said it
rm -rf "$app/.jolt"
cat > "$dep/deps.edn" <<'EOF'
{:paths ["src"]
 :jolt/native [{:name "greet" :static {:archive "native/libgreet.a"}}]}
EOF
mkdir -p "$dep/native"; ar rcs "$dep/native/libgreet.a" "$work/greet.o"
if ! (cd / && JOLT_PWD="$app" "$joltabs" build -m app.core -o "$out") >"$work/build.log" 2>&1; then
  echo "  FAIL: jolt build (static, re-check) exited non-zero"; cat "$work/build.log"; exit 1
fi
if grep -q "loaded at runtime" "$work/build.log"; then
  echo "  FAIL: a fully static build claimed a native is loaded at runtime"
  cat "$work/build.log"; exit 1
fi

# --- structural: link order (GNU ld left-to-right) --------------------------
# Static archives that reference system symbols (libm, libpthread) must appear
# BEFORE the -l flags for those libraries. grep build.ss for the pattern that
# indicates the OPPOSITE (syslibs before archives — bad on Linux).
if grep -qn 'bld-link-libs.*native-link' host/chez/build.ss; then
  echo "  FAIL: native-link appears after (bld-link-libs) in build.ss — GNU ld would get undefined references"
  exit 1
fi

echo "static-native smoke: passed (static default + non-PIC archive + --dynamic runtime load + project-relative archive + transitive-dep relative archive + runtime-native report + link order)"
