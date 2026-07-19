#!/bin/sh
# cross-build-poc.sh — proof of concept for jolt-lang/jolt#375: cross-compile
# a jolt app on one platform into a native executable for another.
#
# `jolt build` splits cleanly at the machine boundary:
#   steps 1-3: Clojure -> IR -> Scheme emission -> <out>.build/flat.ss
#              (machine-neutral text: no host paths, no host machine-type
#              literals; every (machine-type) call is a runtime call)
#   step 4:    compile-file -> make-boot-file -> cc link   (machine-specific)
#
# This script re-runs ONLY step 4 under Chez's cross compiler. Chez's
# `make bootquick XM=<target>` produces target boot files AND an "xpatch"
# that retargets exactly the two functions step 4 calls: compile-file and
# make-boot-file (see ChezScheme/BUILDING, "CROSS COMPILING SCHEME PROGRAMS").
#
# One-time prereqs, in a ChezScheme checkout (with submodules) at the version
# jolt pins (v10.4.1):
#   ./configure -m=<host>  && make        # host compiler        (~5-8 min)
#   make bootquick XM=<target>            # target boots + xpatch (~2 min)
#   ./configure --cross --force -m=<target> CFLAGS="<target cc flags>" CC_FOR_BUILD=cc
#   make                                  # target libkernel.a    (~1 min)
#
# Defaults below are the macOS arm64 -> macOS x86_64 (tarm64osx -> ta6osx)
# case, which is verifiable on one machine via Rosetta 2 — and is exactly the
# release artifact jolt CI dropped when GitHub retired Intel runners.
#
# Usage:
#   bin/joltc build -m app.core -o myapp        # host build; leaves myapp.build/
#   CHEZ_SRC=~/dev/ChezScheme tools/cross-compile/cross-build-poc.sh myapp.build myapp-x86
#   file myapp-x86        # => Mach-O 64-bit executable x86_64
set -e

CHEZ_SRC="${CHEZ_SRC:?set CHEZ_SRC to a ChezScheme checkout prepared per the header comment}"
HOST_M="${HOST_M:-tarm64osx}"
TARGET_M="${TARGET_M:-ta6osx}"
# "-" (not ":-") so an explicit ARCH_FLAG="" survives — Linux cross targets
# select the arch via CC (aarch64-linux-gnu-gcc), not an -arch flag.
ARCH_FLAG="${ARCH_FLAG--arch x86_64}"
CC_BIN="${CC:-cc}"

# link libs for the TARGET OS; *osx = macOS, *le = Linux machine types.
# The Linux set matches a cross kernel configured with --disable-curses
# --disable-x11 (a cross sysroot has no ncurses/X11); it mirrors Chez's own
# scheme-executable link. Override with LINK_LIBS for anything else.
case "$TARGET_M" in
  *osx) DEFAULT_LIBS="-lncurses -liconv -lm -framework Foundation" ;;
  *le)  DEFAULT_LIBS="-lm -ldl -lrt -lpthread" ;;
  *)    DEFAULT_LIBS="" ;;
esac
LINK_LIBS="${LINK_LIBS:-$DEFAULT_LIBS}"

FLAT_DIR="$1"; OUT="$2"
[ -n "$OUT" ] || { echo "usage: cross-build-poc.sh <dir-with-flat.ss> <out-binary>"; exit 1; }
[ -f "$FLAT_DIR/flat.ss" ] || { echo "no flat.ss in $FLAT_DIR (run jolt build first)"; exit 1; }

HOST_SCHEME="$CHEZ_SRC/$HOST_M/bin/$HOST_M/scheme"
HOST_BOOT="$CHEZ_SRC/$HOST_M/boot/$HOST_M"
XPATCH="$CHEZ_SRC/xc-$TARGET_M/s/xpatch"
TARGET_CSV="${TARGET_CSV:-$CHEZ_SRC/$TARGET_M/boot/$TARGET_M}"   # libkernel.a, scheme.h, *.boot
TARGET_LZ4="$CHEZ_SRC/$TARGET_M/lz4/lib"                          # static lz4 built for target
TARGET_ZLIB="$CHEZ_SRC/$TARGET_M/zlib"                            # static zlib built for target

for f in "$HOST_SCHEME" "$XPATCH" "$TARGET_CSV/libkernel.a" "$TARGET_CSV/scheme.h" \
         "$TARGET_CSV/petite.boot" "$TARGET_CSV/scheme.boot"; do
  [ -e "$f" ] || { echo "missing prereq: $f"; exit 1; }
done

WORK="$OUT.build"
mkdir -p "$WORK"

# --- jolt build step 4a, retargeted: compile + boot under the xpatch --------
# Same Chez parameters as build.ss's release mode (bld-chez-params).
cat > "$WORK/cross-compile.ss" <<EOF
(import (chezscheme))
(load "$XPATCH")
(optimize-level 2)
(generate-inspector-information #t)
(generate-procedure-source-information #t)
(fasl-compressed #t)
(compile-file "$FLAT_DIR/flat.ss" "$WORK/flat.so")
(make-boot-file "$WORK/jolt.boot" (list)
  "$TARGET_CSV/petite.boot"
  "$TARGET_CSV/scheme.boot"
  "$WORK/flat.so")
EOF
echo "cross-build: compile-file + make-boot-file under xpatch ($TARGET_M)"
SCHEMEHEAPDIRS="$HOST_BOOT" "$HOST_SCHEME" --script "$WORK/cross-compile.ss"

# --- jolt build step 4b, retargeted: link the target-arch launcher ----------
# Same main.c + xxd embedding as build.ss's build-with-cc, but cc targets the
# other architecture and links the target-arch kernel + static lz4/zlib.
xxd -i "$WORK/jolt.boot" > "$WORK/boot_data.h"
sed -i.bak -E 's/unsigned char [A-Za-z0-9_]+\[\]/unsigned char jolt_boot[]/; s/unsigned int [A-Za-z0-9_]+_len/unsigned int jolt_boot_len/' "$WORK/boot_data.h"
cat > "$WORK/main.c" <<'EOF'
#include "scheme.h"
#include "boot_data.h"
int main(int argc, char *argv[]) {
  Sscheme_init(0);
  Sregister_boot_file_bytes("jolt", jolt_boot, jolt_boot_len);
  Sbuild_heap(0, 0);
  int status = Sscheme_start(argc, (const char **)argv);
  Sscheme_deinit();
  return status;
}
EOF
echo "cross-build: $CC_BIN $ARCH_FLAG link against $TARGET_CSV/libkernel.a"
$CC_BIN $ARCH_FLAG -O2 -I"$TARGET_CSV" -I"$WORK" "$WORK/main.c" "$TARGET_CSV/libkernel.a" \
   -o "$OUT" -L"$TARGET_LZ4" -L"$TARGET_ZLIB" -llz4 -lz $LINK_LIBS
echo "cross-build: wrote $OUT"
file "$OUT"
