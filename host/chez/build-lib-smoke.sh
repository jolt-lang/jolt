#!/bin/sh
# build-lib smoke: `jolt build --library` compiles an app into a shared object an
# embedder loads with dlopen and calls via jolt_library_init + jolt_lookup. This
# proves the managed-runtime library works as a C ABI target.
root="$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)"
cd "$root"

# JOLT_BIN overrides the jolt under test. The gate targets point it at the
# freshly built target/release/jolt: a `jolt build` costs ~2.5s through the
# prebuilt binary and ~12.5s through the source-mode driver, and this gate
# drives one. JOLT_BIN=bin/jolt forces script mode.
jolt="${JOLT_BIN:-bin/jolt}"
# Absolute form, for the cases that cd into a fixture directory first.
case "$jolt" in /*) joltabs="$jolt" ;; *) joltabs="$root/$jolt" ;; esac

app="$root/test/chez/build-lib"
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

# --library composes with --target: the real cross build is the manual-dispatch
# cross-smoke workflow, so pin the wiring it rests on here, where no cross
# toolchain is needed. Both cases fail before any compile, so they also run on a
# machine the preflight below skips.
#
# The target must be a machine OTHER than this host, or bld-cross? is #f and
# the cross path (where both messages live) is never entered: on an aarch64
# host, --target tarm64le built a native library and the second check below
# saw no hint. Pick the other architecture's real tag.
case "$(uname -m)" in
  aarch64|arm64) cross_target=ta6le ;;
  *)             cross_target=tarm64le ;;
esac
echo "build-lib smoke: --library composes with --target"
# 1. the CLI forwards --target through a library build (it used to reject the
#    combination outright), so the missing pack is what it complains about.
out="$( (unset JOLT_TARGET_PACK; JOLT_PWD="$app" "$jolt" build --library \
          -m libadd.core -o "$work/never" --target "$cross_target") 2>&1 )"
case "$out" in
  *"needs a target pack"*) ;;
  *) echo "  FAIL: --library --target should ask for a target pack, got:"
     printf '%s\n' "$out"; exit 1 ;;
esac
# 2. target + pack reach build.ss's cross path inside a library build — the
#    "Provide a target pack" hint is emitted only when bld-cross? is true.
mkdir -p "$work/emptypack"
out="$(JOLT_PWD="$app" "$jolt" build --library -m libadd.core -o "$work/never" \
        --target "$cross_target" --target-pack "$work/emptypack" 2>&1 )"
case "$out" in
  *"Provide a target pack"*) ;;
  *) echo "  FAIL: --library --target-pack should reach the cross toolchain check, got:"
     printf '%s\n' "$out"; exit 1 ;;
esac

# Preflight: same as build-smoke — a library build needs Chez's kernel dev files
# (libkernel.a + scheme.h) and a C compiler. Skip cleanly where absent.
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
# A skip here is an environment limitation, not a pass — but it exits 0, so a CI
# job whose Chez cannot build a shared library would report this gate green while
# testing nothing. JOLT_REQUIRE_BUILDLIB=1 (set by .github/workflows/tests.yml)
# turns every skip below into a failure, so the gate is either run or loud.
skip_or_fail() {
  if [ -n "${JOLT_REQUIRE_BUILDLIB:-}" ]; then
    echo "  FAIL: $1" >&2
    echo "  (JOLT_REQUIRE_BUILDLIB is set, so this environment was expected to run the gate)" >&2
    exit 1
  fi
  echo "build-lib smoke: skipped ($1)"
  exit 0
}

if ! command -v cc >/dev/null 2>&1 || [ -z "$csv" ] || [ ! -f "$csv/scheme.h" ] || [ ! -f "$csv/libkernel.a" ]; then
  skip_or_fail "Chez kernel dev files or C compiler not available"
fi
export JOLT_CHEZ_CSV="$csv"

case "$(uname -s)" in
  Darwin) lib="$work/libadd.dylib" ;;
  *)      lib="$work/libadd.so" ;;
esac

# -o names the library WITHOUT a suffix, the README's form: the build appends the
# platform's .so/.dylib (jolt#1235), so "$lib" is where it must land.
echo "build-lib smoke: compiling libadd.core -o $work/libadd -> $lib"
build_out="$(JOLT_PWD="$app" "$jolt" build --library -m libadd.core -o "$work/libadd" 2>&1)"
build_rc=$?
if [ -f "$work/libadd" ]; then
  echo "  FAIL: -o $work/libadd wrote the library with no suffix, want $lib"; exit 1
fi
if [ ! -f "$lib" ]; then
  # A shared object folds Chez's libkernel.a in, so that archive must be PIC. A
  # kernel built without -fPIC (the common default, incl. a stock source build)
  # fails the -shared link with a relocation error — an environment limitation,
  # not a jolt bug, so skip like the missing-toolchain case above.
  if printf '%s' "$build_out" | grep -qiE 'recompile with .*-fPIC|can not be used when making a shared object|relocation R_'; then
    skip_or_fail "Chez libkernel.a is not position-independent; a shared library needs a PIC kernel (configure Chez with CFLAGS+=-fPIC)"
  fi
  # A build killed from outside prints no error, so the status is all there is
  # to tell it from a failed one (143 is SIGTERM, 137 SIGKILL).
  echo "  FAIL: jolt build --library produced no shared library (exit $build_rc)"
  printf '%s\n' "$build_out"
  exit 1
fi

# The install name follows the file name, suffix included.
if [ "$(uname -s)" = Darwin ] && command -v otool >/dev/null 2>&1; then
  case "$(otool -D "$lib")" in
    *"@rpath/libadd.dylib"*) ;;
    *) echo "  FAIL: install name, want @rpath/libadd.dylib, got:"; otool -D "$lib"; exit 1 ;;
  esac
fi

echo "build-lib smoke: compiling driver + calling add(2,3) through dlopen"
if ! cc -O2 "$app/driver.c" -ldl -o "$work/driver" 2>"$work/driver.err"; then
  echo "  FAIL: driver compile failed"; cat "$work/driver.err"; exit 1
fi
got="$("$work/driver" "$lib" 2>&1)"; rc=$?
if [ "$got" != "5 8 1" ] || [ "$rc" != "0" ]; then
  echo "  FAIL: exports — want '5 8 1' rc 0, got '$got' rc $rc"; exit 1
fi

# Code that is compiled and then dropped frees code chunks, and new code reuses
# the space. On macOS arm64 the library keeps code in one aligned region
# (host/chez/stub/jolt_code_region.h, jolt#1246) through its own hidden mmap and
# munmap, so this is the path that recycles region ranges.
echo "build-lib smoke: compiling and dropping code inside the library"
if ! cc -O2 "$app/driver-churn.c" -ldl -o "$work/driver-churn" 2>"$work/driver-churn.err"; then
  echo "  FAIL: churn driver compile failed"; cat "$work/driver-churn.err"; exit 1
fi
got="$("$work/driver-churn" "$lib" 2>&1)"; rc=$?
if [ "$got" != "1" ] || [ "$rc" != "0" ]; then
  echo "  FAIL: code churn, want '1' rc 0, got '$got' rc $rc"; exit 1
fi
# The region's mmap/munmap must bind the kernel's calls (defined in the image)
# without being exported, or they would stand in for the host's own.
if [ "$(uname -s)" = Darwin ] && [ "$(uname -m)" = arm64 ] && command -v nm >/dev/null 2>&1; then
  if ! nm -m "$lib" | grep -qE 'non-external .* _mmap$'; then
    echo "  FAIL: no library-local _mmap; the code region is not linked in"; exit 1
  fi
  if nm -gU "$lib" | grep -qE ' _(mmap|munmap)$'; then
    echo "  FAIL: the library exports mmap/munmap"; nm -gU "$lib" | grep -E ' _(mmap|munmap)$'; exit 1
  fi
fi

# The #1234 handoff: release the init thread, park it in host code while a
# thread the embedder started calls a :collect-safe export that allocates, then
# shut down from the released thread. Before jolt_library_release_thread the
# worker's first collection waited forever on the parked init thread. The driver
# arms alarm(30), so a hang comes back as a SIGALRM death. POSIX threads only.
case "$(uname -s)" in
  MINGW*|MSYS*|CYGWIN*)
    echo "build-lib smoke: release-thread handoff skipped (POSIX threads driver)" ;;
  *)
    echo "build-lib smoke: init, release the thread, call in from a worker, shut down"
    if ! cc -O2 "$app/driver-release.c" -ldl -lpthread -o "$work/driver-release" 2>"$work/driver-release.err"; then
      echo "  FAIL: release driver compile failed"; cat "$work/driver-release.err"; exit 1
    fi
    got="$("$work/driver-release" "$lib" 2>&1)"; rc=$?
    if [ "$got" != "20255 20000" ] || [ "$rc" != "0" ]; then
      echo "  FAIL: release-thread handoff, want '20255 20000' rc 0, got '$got' rc $rc"; exit 1
    fi ;;
esac

# A host that handles its own faults (a .NET or Java runtime does) keeps them
# (#1277). Chez installs process-wide SIGSEGV/SIGBUS/SIGFPE/SIGILL handlers at
# init and treats every fault as Scheme's; the library stub chains a fault on a
# thread not running Scheme code to the handler installed before Chez's, runs
# it as the kernel would have (the host's alternate stack, sa_mask and
# SA_RESETHAND), and gives the signals back at jolt_library_shutdown. A fault in
# Scheme code is still jolt's exception. SIGINT and SIGQUIT stay the host's, and
# SIGPIPE is ignored only while the library is loaded. The driver prints what
# each stage saw.
case "$(uname -s)" in
  MINGW*|MSYS*|CYGWIN*)
    echo "build-lib smoke: host fault handling skipped (POSIX signals driver)" ;;
  *)
    echo "build-lib smoke: a host's own fault after init goes to the host's handler"
    if ! cc -O0 "$app/driver-signals.c" -ldl -lpthread -o "$work/driver-signals" 2>"$work/driver-signals.err"; then
      echo "  FAIL: signals driver compile failed"; cat "$work/driver-signals.err"; exit 1
    fi
    got="$("$work/driver-signals" "$lib" 2>&1)"; rc=$?
    # 11, or 10 where a null dereference raises SIGBUS
    sig="$(printf '%s\n' "$got" | sed -n 's/^before load: \([0-9]*\) .*/\1/p')"
    # macOS hides SA_RESETHAND from sigaction, and keeps a thread "on its
    # alternate stack" after Chez's _longjmp out of a Scheme fault taken there
    # (driver-signals.c)
    case "$(uname -s)" in
      Darwin) oneshot=0; alt_after_scheme=0 ;;
      *)      oneshot="$sig"; alt_after_scheme=1 ;;
    esac
    want="one-shot: 1 $oneshot
before load: $sig 1 1
scheme fault: 1
after load: $sig 1 1
signals after load: host host ign 1
worker: $sig 1 1, 1, $sig $alt_after_scheme 1
after shutdown: $sig 1 1
signals after shutdown: host host dfl 1
after reinit: $sig 1 1
signals after reinit: host host ign 1"
    if [ "$rc" != "0" ] || { [ "$sig" != "11" ] && [ "$sig" != "10" ]; } || [ "$got" != "$want" ]; then
      echo "  FAIL: host fault handling, want rc 0 and:"; printf '%s\n' "$want"
      echo "  got rc $rc:"; printf '%s\n' "$got"; exit 1
    fi ;;
esac

echo "build-lib smoke: passed (add(2,3)=5 + jolt.ffi layout-size=8 + gzip_ok()=1 via dlopen+jolt_lookup, code churn, and a released init thread)"
