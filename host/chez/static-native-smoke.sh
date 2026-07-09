#!/bin/sh
# static-native smoke: a project's :jolt/native lib with a :static archive is
# LINKED INTO the built binary (the default), so the binary calls the C function
# with no shared object on disk at runtime. --dynamic keeps the old behavior —
# load a shared object at runtime.
root="$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)"
cd "$root"

# Preflight: needs cc (to build the test libs AND to cc-link the app) + Chez's
# kernel dev files, same as build-smoke. Skip otherwise (CI on a distro package).
csv="$JOLT_CHEZ_CSV"
if [ -z "$csv" ]; then
  chez_bin="$(command -v chez || command -v scheme || command -v petite || true)"
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
if ! JOLT_PWD="$app" bin/joltc build -m app.core -o "$out" >"$work/build.log" 2>&1; then
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
if ! JOLT_PWD="$app" bin/joltc build -m app.core -o "$out" --dynamic >"$work/build.log" 2>&1; then
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

# --- structural: link order (GNU ld left-to-right) --------------------------
# Static archives that reference system symbols (libm, libpthread) must appear
# BEFORE the -l flags for those libraries. grep build.ss for the pattern that
# indicates the OPPOSITE (syslibs before archives — bad on Linux).
if grep -qn 'bld-link-libs.*native-link' host/chez/build.ss; then
  echo "  FAIL: native-link appears after (bld-link-libs) in build.ss — GNU ld would get undefined references"
  exit 1
fi

echo "static-native smoke: passed (static default + --dynamic runtime load + link order)"
