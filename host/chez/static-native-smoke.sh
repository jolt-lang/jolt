#!/bin/sh

# static-native smoke: a project's :jolt/native lib with a :static archive is
# LINKED INTO the built binary (the default), so the binary calls the C function
# with no shared object on disk at runtime. --dynamic keeps the old behavior —
# load a shared object at runtime.
root="$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)"
cd "$root"

# The app's emitted Scheme is several files since the build compiles (and
# caches) one unit per namespace: flat.ss is the prologue, app-N.ss each
# namespace, app-post.ss the launcher. A check about what the app emitted reads
# all of them — one over flat.ss alone passes an absence check vacuously.
appsrc() { cat "$1/flat.ss" "$1"/app-[0-9]*.ss "$1/app-post.ss" 2>/dev/null; }

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
if ! appsrc "$out.build" | grep -q "jolt-build-load-native '() #f #t"; then
  echo "  FAIL: static native did not emit a process-symbol load"; exit 1
fi
if appsrc "$out.build" | grep -q "libgreet.$soext"; then
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
  # static data, which is what gets the absolute relocation. The data is a
  # NON-static (preemptible) global on purpose: on aarch64 a reference to a
  # local symbol still lowers to adrp/add, which links into a shared object
  # fine, so only a global the loader could interpose forces the non-PIC
  # relocation this case is about (R_X86_64_32 on x86_64, and
  # R_AARCH64_ADR_PREL_PG_HI21 / R_AARCH64_ABS64 on aarch64).
  cat > "$work/nopic.c" <<'EOF'
const char greeting[] = "static";
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
  # failed and the -no-pie retry must be what produced the binary. NOT on
  # bionic: build.ss bld-no-pie-supported? refuses -no-pie there by design —
  # Android's loader requires a PIE executable, so the first link has to
  # succeed, and on aarch64 it does (the adrp/add pair in the archive resolves
  # against the definition in the executable).
  if echo | cc -E -dM -x c - 2>/dev/null | grep -q '__PIE__' \
     && ! cc -dumpmachine 2>/dev/null | grep -q android; then
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

# --- a :static-only spec loads nothing while the build runs ------------------
# The build resolves a :static native through the preloaded archive. Loading the
# project's natives into the build process derived conventional shared-object
# names from the spec's :name ("greet" -> libgreet.dylib) for a spec that
# declares no candidates, :static ones included — so whatever the loader found
# under that name answered the build's calls instead of the archive being
# linked, and for {:name "crypto"} on macOS the loader found Apple's
# libcrypto.dylib, which aborts the process that opens it. A decoy libgreet on
# the loader's path answers 99 where the archive answers 42; a macro calls the
# native while the build runs and bakes the answer in.
cc -c "$work/greet.c" -o "$work/greet.o" && ar rcs "$work/libgreet.a" "$work/greet.o"
mkdir -p "$work/decoy"
printf 'int jolt_static_answer(void) { return 99; }\n' > "$work/decoy.c"
cc $shared "$work/decoy.c" -o "$work/decoy/libgreet.$soext"
cat > "$app/deps.edn" <<EOF
{:paths ["src"]
 :jolt/native [{:name "greet" :static {:archive "$work/libgreet.a"}}]}
EOF
cp "$app/src/app/core.clj" "$work/core.clj.saved"
cat > "$app/src/app/core.clj" <<'EOF'
(ns app.core
  (:require [jolt.ffi :as ffi]))
(ffi/defcfn answer "jolt_static_answer" [] :int)
(defmacro answer-while-building [] (answer))
(defn -main [& _]
  (println "answer:" (answer) (answer-while-building)))
EOF
rm -rf "$app/.jolt" "$out.build"
echo "static-native smoke: building (a :static-only spec with a same-named shared object on the loader path)"
if ! DYLD_LIBRARY_PATH="$work/decoy" LD_LIBRARY_PATH="$work/decoy" JOLT_PWD="$app" \
     "$jolt" build -m app.core -o "$out" >"$work/build.log" 2>&1; then
  echo "  FAIL: jolt build with a decoy shared object exited non-zero"
  cat "$work/build.log"; exit 1
fi
got="$(cd / && "$out" 2>&1)"
if [ "$got" != "answer: 42 42" ]; then
  echo "  FAIL: the build resolved a :static native through a derived shared-object name"
  echo "--- want ---"; echo "answer: 42 42"; echo "--- got ----"; echo "$got"; exit 1
fi
cp "$work/core.clj.saved" "$app/src/app/core.clj"
rm -rf "$app/.jolt" "$work/decoy"

# --- an archive's own system library, from :link-libs -------------------------
# An archive that calls into a system library jolt does not otherwise link —
# sqlite3 on macOS, libcrypt on Linux — links only when its spec declares it.
# Without the declaration the build fails (the control: the library really is
# missing from the line); with it, the binary runs.
case "$(uname -s)" in
  Darwin) syslib=sqlite3
          printf 'int sqlite3_libversion_number(void);\nint jolt_needs(void) { return sqlite3_libversion_number() > 0 ? 7 : 0; }\n' > "$work/needs.c" ;;
  *)      syslib=crypt
          printf 'char *crypt(const char *, const char *);\nint jolt_needs(void) { return crypt("a", "ab") ? 7 : 0; }\n' > "$work/needs.c" ;;
esac
if printf 'int main(void){return 0;}' | cc -x c - -l"$syslib" -o "$work/probe" 2>/dev/null; then
  cc -fPIC -c "$work/needs.c" -o "$work/needs.o" && ar rcs "$work/libneeds.a" "$work/needs.o"
  cp "$app/src/app/core.clj" "$work/core.clj.saved"
  cat > "$app/src/app/core.clj" <<'EOF'
(ns app.core
  (:require [jolt.ffi :as ffi]))
(ffi/defcfn needs "jolt_needs" [] :int)
(def at-load (needs))
(defn -main [& _]
  (println "needs:" at-load (needs)))
EOF
  cat > "$app/deps.edn" <<EOF
{:paths ["src"]
 :jolt/native [{:name "needs" :static {:archive "$work/libneeds.a"}}]}
EOF
  rm -rf "$app/.jolt" "$out.build"
  echo "static-native smoke: building (an archive needing -l$syslib, undeclared)"
  if JOLT_PWD="$app" "$jolt" build -m app.core -o "$out" >"$work/build.log" 2>&1; then
    echo "  FAIL: an archive needing -l$syslib linked without it — the case proves nothing"
    cat "$work/build.log"; exit 1
  fi
  cat > "$app/deps.edn" <<EOF
{:paths ["src"]
 :jolt/native [{:name "needs" :static {:archive "$work/libneeds.a"}
                :link-libs {:darwin ["sqlite3"] :linux ["crypt"]}}]}
EOF
  rm -rf "$app/.jolt" "$out.build"
  echo "static-native smoke: building (an archive needing -l$syslib, declared in :link-libs)"
  if ! JOLT_PWD="$app" "$jolt" build -m app.core -o "$out" >"$work/build.log" 2>&1; then
    echo "  FAIL: jolt build with :link-libs exited non-zero"; cat "$work/build.log"; exit 1
  fi
  got="$(cd / && "$out" 2>&1)"
  if [ "$got" != "needs: 7 7" ]; then
    echo "  FAIL: :link-libs binary output mismatch"; echo "--- got ----"; echo "$got"; exit 1
  fi
  cp "$work/core.clj.saved" "$app/src/app/core.clj"
  rm -rf "$app/.jolt"
else
  echo "static-native smoke: :link-libs case skipped (cc cannot link -l$syslib here)"
fi

# --- one archive calling into another (jolt-lang/jolt#1205) ------------------
# libssl.a calls into libcrypto.a; here libdep.a calls into libbase.a, and the
# dependent one is declared FIRST. The namespace calls it while it loads, so the
# build process itself has to resolve it — which it could not while each archive
# was preloaded as a shared object of its own: the dependent one's reference to
# the other stayed undefined and its load was refused.
cat > "$work/base.c" <<'EOF'
int jolt_static_base(void) { return 40; }
EOF
cat > "$work/dep.c" <<'EOF'
int jolt_static_base(void);
int jolt_static_dep(void) { return jolt_static_base() + 2; }
EOF
cc -fPIC -c "$work/base.c" -o "$work/base.o" && ar rcs "$work/libbase.a" "$work/base.o"
cc -fPIC -c "$work/dep.c" -o "$work/dep.o" && ar rcs "$work/libdep.a" "$work/dep.o"
cat > "$app/deps.edn" <<EOF
{:paths ["src"]
 :jolt/native [{:name "dep"  :static {:archive "$work/libdep.a"}}
               {:name "base" :static {:archive "$work/libbase.a"}}]}
EOF
cp "$app/src/app/core.clj" "$work/core.clj.saved"
cat > "$app/src/app/core.clj" <<'EOF'
(ns app.core
  (:require [jolt.ffi :as ffi]))
(ffi/defcfn dep-answer "jolt_static_dep" [] :int)
(def at-load (dep-answer))
(defn -main [& _]
  (println "answer:" at-load (dep-answer)))
EOF
rm -rf "$app/.jolt" "$out.build"
echo "static-native smoke: building (an archive that calls into another)"
if ! JOLT_PWD="$app" "$jolt" build -m app.core -o "$out" >"$work/build.log" 2>&1; then
  echo "  FAIL: jolt build with dependent static archives exited non-zero (jolt#1205)"
  cat "$work/build.log"; exit 1
fi
got="$(cd / && "$out" 2>&1)"
if [ "$got" != "answer: 42 42" ]; then
  echo "  FAIL: dependent static archives binary output mismatch"
  echo "--- got ----"; echo "$got"; exit 1
fi
cp "$work/core.clj.saved" "$app/src/app/core.clj"
rm -rf "$app/.jolt"

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
if ! appsrc "$out.build" | grep -q "libgreet.$soext"; then
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
# (bld-link-libs-after native-link) names native-link as its argument, after it.
if grep -n 'bld-link-libs.*native-link' host/chez/build.ss | grep -qv 'native-link " " (bld-link-libs-after native-link)'; then
  echo "  FAIL: native-link appears after (bld-link-libs) in build.ss — GNU ld would get undefined references"
  exit 1
fi

echo "static-native smoke: passed (static default + non-PIC archive + dependent archives + :link-libs + --dynamic runtime load + project-relative archive + transitive-dep relative archive + runtime-native report + link order)"
