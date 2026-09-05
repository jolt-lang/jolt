#!/bin/sh
# jolt self-build smoke (jolt-eaj): build jolt as a self-contained binary, then
# use THAT binary to compile a jolt app with Chez and cc removed from the
# environment — the whole point of the feature. The produced app must then run
# and match the same expected output as build-smoke.sh.
root="$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)"
cd "$root"

# Preflight: building jolt itself needs the Chez kernel dev files (libkernel.a +
# scheme.h) and a C compiler, same as build-smoke.sh. A distro chezscheme package
# ships neither, so skip there (CI included).
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
  echo "jolt self-build smoke: skipped (Chez kernel dev files or C compiler not available)"
  exit 0
fi
export JOLT_CHEZ_CSV="$csv"

# 1. Build jolt (debug profile — faster; the self-contained app-build mechanism
# is identical to release, only Chez compile settings differ).
jolt="$root/target/debug/jolt"
echo "jolt self-build smoke: building $jolt"
if ! "$chez_bin" --script host/chez/build-jolt.ss debug "$jolt" >/dev/null 2>&1; then
  echo "  FAIL: build-jolt.ss exited non-zero"
  exit 1
fi
[ -x "$jolt" ] || { echo "  FAIL: no jolt executable produced"; exit 1; }

# 2. The distributed jolt must run with no Chez install: a basic eval.
got_e="$(env -i HOME="$HOME" "$jolt" -e '(reduce + (range 10))' 2>&1)"
if [ "$got_e" != "45" ]; then
  echo "  FAIL: jolt -e under empty env gave '$got_e', want 45"
  exit 1
fi

# 2b. JOLT_TRACE must take effect in the BUILT binary. The env check runs at
# runtime (the launcher), NOT at heap-build where JOLT_TRACE is always unset — so
# an uncaught error shows a tail-frame trace recovering the TCO-elided chain, and
# exactly ONE trace block (the launcher must not double-print it).
got_tr="$(env -i HOME="$HOME" JOLT_TRACE=1 "$jolt" -e '(defn a [x] (+ x 1)) (defn b [x] (a x)) (b :x)' 2>&1)"
if ! printf '%s' "$got_tr" | grep -q '  trace:' || ! printf '%s' "$got_tr" | grep -q 'b'; then
  echo "  FAIL: JOLT_TRACE=1 in the built jolt produced no tail-frame trace"
  echo "--- got ---"; echo "$got_tr"; exit 1
fi
if [ "$(printf '%s' "$got_tr" | grep -c '  trace:')" != "1" ]; then
  echo "  FAIL: built jolt double-printed the trace block"
  echo "--- got ---"; echo "$got_tr"; exit 1
fi

# 3. Build an app through the distributed jolt with an EMPTY environment — no
# PATH at all, so no chez, no cc, no shell tools are reachable. This is the core
# guarantee: jolt compiles apps entirely on its own.
app="$(mktemp -d)/build-app"
cp -r "$root/test/chez/build-app" "$app"
out="$app/app"
echo "jolt self-build smoke: compiling app.core via the binary (no chez/cc on PATH)"
if ! env -i HOME="$HOME" JOLT_PWD="$app" "$jolt" build -m app.core -o "$out" >/dev/null 2>&1; then
  echo "  FAIL: self-contained jolt build exited non-zero"
  rm -rf "$(dirname "$app")"
  exit 1
fi
[ -x "$out" ] || { echo "  FAIL: no app executable produced"; rm -rf "$(dirname "$app")"; exit 1; }

# 4. The produced app runs from a neutral cwd and matches build-smoke's output.
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
rm -rf "$(dirname "$app")"
if [ "$got" != "$want" ]; then
  echo "  FAIL: produced app output mismatch"
  echo "--- want ---"; echo "$want"
  echo "--- got ----"; echo "$got"
  exit 1
fi

# 5. Static native linking through the distributed jolt: it bundles the Chez
# kernel AND the static liblz4.a / libz.a that kernel was built against, so with
# the system cc (but still no external Chez) it re-links a stub that bakes a
# :jolt/native :static archive into the app. The app then calls the C function
# with the archive removed from disk. Uses the normal PATH so cc and the kernel's
# remaining link deps are found, but Chez stays out of the build — and the
# compression libraries come from the bundle, not from this machine, which the
# dependency check below pins.
napp="$(mktemp -d)/native-app"
mkdir -p "$napp/src/app"
printf 'int jolt_static_answer(void){return 42;}\n' > "$napp/greet.c"
cc -c "$napp/greet.c" -o "$napp/greet.o" && ar rcs "$napp/libgreet.a" "$napp/greet.o"
cat > "$napp/src/app/core.clj" <<'EOF'
(ns app.core (:require [jolt.ffi :as ffi]))
(ffi/defcfn answer "jolt_static_answer" [] :int)
(defn -main [& _] (println "answer:" (answer)))
EOF
cat > "$napp/deps.edn" <<EOF
{:paths ["src"]
 :jolt/native [{:name "greet" :static {:archive "$napp/libgreet.a"}}]}
EOF
nout="$napp/app"
echo "jolt self-build smoke: static-linking a native lib via the binary (no external Chez)"
if ! JOLT_PWD="$napp" "$jolt" build -m app.core -o "$nout" > "$napp/build.log" 2>&1; then
  echo "  FAIL: static native build via distributed jolt exited non-zero"
  cat "$napp/build.log"
  rm -rf "$(dirname "$napp")"; exit 1
fi

# The relink is the one link the distributed jolt performs, and it must take lz4
# and zlib from the archives the binary carries rather than from this machine: an
# app built on a box with them installed has to run on one without them. Skipped
# when this jolt is missing one — it says so at link time — because then there is
# nothing to take, and the fallback is the pre-existing behaviour.
if grep -q "no static lib" "$napp/build.log"; then
  echo "  (compression dependency check skipped: this jolt bundles no archive)"
else
  ndeps=""
  if command -v otool >/dev/null 2>&1; then ndeps="$(otool -L "$nout" 2>/dev/null)"
  elif command -v readelf >/dev/null 2>&1; then ndeps="$(readelf -d "$nout" 2>/dev/null)"
  elif command -v ldd >/dev/null 2>&1; then ndeps="$(ldd "$nout" 2>/dev/null)"
  fi
  if printf '%s' "$ndeps" | grep -qEi 'lz4|libz\.'; then
    echo "  FAIL: the static-native app needs a compression library at runtime:"
    printf '%s\n' "$ndeps" | grep -Ei 'lz4|libz\.'
    rm -rf "$(dirname "$napp")"; exit 1
  fi
fi
rm -f "$napp/libgreet.a" "$napp/greet.o"     # nothing to load at runtime
got_n="$(cd / && "$nout" 2>&1)"
rm -rf "$(dirname "$napp")"
if [ "$got_n" != "answer: 42" ]; then
  echo "  FAIL: static-linked app (via distributed jolt) output mismatch"
  echo "--- got ----"; echo "$got_n"; exit 1
fi
echo "jolt self-build smoke: passed (jolt runs + builds a working app with no external toolchain, incl. static native linking)"
