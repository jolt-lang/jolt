#!/bin/sh
# make-pack.sh — assemble a jolt "target pack" from a prepared ChezScheme cross
# checkout, for `jolt build --target <machine> --target-pack <pack>`.
#
# The one-time ChezScheme setup (see README.md) leaves the target's boots,
# kernel, xpatch and static lz4/zlib scattered under the checkout; this collects
# them into one directory in the layout build.ss expects:
#
#   <pack>/petite.boot  scheme.boot  libkernel.a  scheme.h   (the csv)
#   <pack>/xpatch                                            (retargets step 4)
#   <pack>/link-libs                                         (target -l flags)
#   <pack>/lib/liblz4.a  lib/libz.a                          (static, if present)
#
# Usage:
#   CHEZ_SRC=~/dev/ChezScheme tools/cross-compile/make-pack.sh <target-machine> <pack-dir>
# e.g. … make-pack.sh ta6osx /tmp/pack-ta6osx
set -e

CHEZ_SRC="${CHEZ_SRC:?set CHEZ_SRC to a ChezScheme checkout prepared per README.md}"
TARGET_M="${1:?usage: make-pack.sh <target-machine> <pack-dir>}"
PACK="${2:?usage: make-pack.sh <target-machine> <pack-dir>}"

CSV="$CHEZ_SRC/$TARGET_M/boot/$TARGET_M"
XPATCH="$CHEZ_SRC/xc-$TARGET_M/s/xpatch"
for f in "$CSV/petite.boot" "$CSV/scheme.boot" "$CSV/libkernel.a" "$CSV/scheme.h" "$XPATCH"; do
  [ -e "$f" ] || { echo "missing: $f (did the ChezScheme cross build run? see README.md)"; exit 1; }
done

mkdir -p "$PACK/lib"
cp "$CSV/petite.boot" "$CSV/scheme.boot" "$CSV/libkernel.a" "$CSV/scheme.h" "$PACK/"
cp "$XPATCH" "$PACK/xpatch"
# static lz4/zlib built for the target (present for non-macOS targets; on macOS
# they come from the system, so absence is fine).
cp "$CHEZ_SRC/$TARGET_M/lz4/lib/liblz4.a" "$PACK/lib/" 2>/dev/null || true
cp "$CHEZ_SRC/$TARGET_M/zlib/libz.a"      "$PACK/lib/" 2>/dev/null || true

# The remaining -l/-framework flags for the target OS (build.ss prepends
# -L<pack>/lib). *osx = macOS, *le = Linux, *nt = Windows. The Linux set matches a
# cross kernel configured --disable-curses --disable-x11 (no ncurses/X11 in a
# cross sysroot), mirroring the POC and Chez's own scheme-executable link.
case "$TARGET_M" in
  *osx) LIBS="-llz4 -lz -lncurses -framework Foundation -liconv -lm" ;;
  *le)  LIBS="-llz4 -lz -lm -ldl -lrt -lpthread" ;;
  *nt)  LIBS="-static -llz4 -lz -lws2_32 -lrpcrt4 -lole32 -luuid -ladvapi32 -luser32 -lshell32 -lm" ;;
  *)    LIBS="-llz4 -lz -lm" ;;
esac
printf '%s\n' "$LIBS" > "$PACK/link-libs"

echo "wrote target pack: $PACK"
echo "  jolt build -m <ns> -o <out> --target $TARGET_M --target-pack $PACK"
