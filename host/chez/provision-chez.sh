#!/bin/sh
# Provision the pinned Chez Scheme locally, with jolt's kernel patches.
#
# Every Chez jolt builds from source carries host/chez/chez-patches (CI, the
# release binaries, and this script), so a from-source `make build` runs the
# same kernel the releases do. makes' chezscheme.mk cannot do that: its one
# recipe extracts, configures and installs with nothing in between, so this
# script replaces it on macOS, Linux and bionic (Android/Termux).
#
# The build is staged by hand rather than with Chez's `make install`, which
# hard-links petite and scheme-script to scheme: Android's SELinux policy
# refuses link() under app data (verified on Termux: EACCES). The layout is
# the one make and bin/jolt expect: bin/scheme (with petite and
# scheme-script), lib/csv<version>/<machine> holding the boot files,
# libkernel.a and scheme.h, and the static libz.a / liblz4.a beside the
# kernel so bld-compression-lib bakes compression in rather than falling back
# to dynamic -lz -llz4. On bionic LIBS=-liconv is load-bearing: Chez's Linux
# sources compile their iconv support unconditionally and bionic has no iconv
# of its own, so the kernel's libiconv_open/close need the library Termux
# packages separately.
#
# Usage: provision-chez.sh PREFIX VERSION [CC]
#        provision-chez.sh --hash
#
#   PREFIX   install prefix, a chezscheme-<version> directory
#   VERSION  Chez release to build (no leading v)
#   CC       C compiler to build with (default: $CC, else cc)
#   --hash   print the patch-set hash the prefix is keyed on
#
# The prefix is keyed on the patches: it holds jolt-patches-<hash>, and a
# prefix without the current one (an unpatched Chez from before this script,
# or one built with an older patch set) is rebuilt from a fresh extract.
#
# The download and extract are cached in the prefix's parent, matching makes'
# layout: <parent>/cache/csv<version>.tar.gz, <parent>/tmp/csv<version>.

set -eu

here=$(cd "$(dirname "$0")" && pwd)
patches_hash() {
  cat "$here"/chez-patches/*.patch | cksum | awk '{print $1 "-" $2}'
}

if [ "${1-}" = --hash ]; then
  patches_hash
  exit 0
fi

prefix=$1
version=$2
cc=${3:-${CC:-cc}}
root=$(dirname "$prefix")
cache=$root/cache
tmp=$root/tmp
tar=$cache/csv$version.tar.gz
src=$tmp/csv$version
url=https://github.com/cisco/ChezScheme/releases/download/v$version/csv$version.tar.gz
stamp=$prefix/jolt-patches-$(patches_hash)

if [ -x "$prefix/bin/scheme" ] && [ -f "$stamp" ]; then
  echo "provision-chez: $prefix/bin/scheme is already provisioned"
  exit 0
fi

# Anything else in the prefix is a stale kernel, and its extract was
# configured (and maybe patched) for it, so neither is reused.
if [ -e "$prefix" ]; then
  echo "provision-chez: $prefix is not built with the current chez-patches; rebuilding"
  rm -rf "$prefix" "$src"
fi

libs=
case "$("$cc" -dumpmachine 2>/dev/null)" in *android*) libs=-liconv ;; esac

mkdir -p "$cache" "$tmp"

if [ ! -s "$tar" ]; then
  echo "provision-chez: downloading csv$version.tar.gz"
  if command -v curl >/dev/null 2>&1; then
    curl -fL --retry 3 --connect-timeout 20 -o "$tar.tmp" "$url"
  elif command -v wget >/dev/null 2>&1; then
    wget -q -O "$tar.tmp" "$url"
  else
    echo "provision-chez: need curl or wget to fetch $url" >&2
    exit 1
  fi
  mv "$tar.tmp" "$tar"
fi

# Reuse an existing extract: configure and make are incremental, so a retry
# after a failure resumes instead of building the bytecode bootstrap again.
if [ ! -f "$src/Makefile" ]; then
  rm -rf "$src"
  tar -C "$tmp" -xzf "$tar"
fi
# jolt's kernel patches, as every CI Chez build applies them (idempotent, so
# a resumed extract is fine).
sh "$here/../../ci/apply-chez-patches.sh" "$src"

jobs=$(nproc 2>/dev/null || getconf _NPROCESSORS_ONLN 2>/dev/null || echo 2)

echo "provision-chez: building Chez Scheme $version with $cc"
(
  cd "$src"
  # CFLAGS+=-fPIC, same as ci/glibc-floor-build.sh: libkernel.a (and the
  # liblz4.a/libz.a built beside it) is bundled INTO the jolt that is built with
  # this Chez, and `jolt build --library` folds it into a shared object, which
  # an archive compiled without -fPIC cannot go into (buildlibsmoke: ld.lld
  # "relocation R_AARCH64_ADR_PREL_PG_HI21 ... recompile with -fPIC). The
  # relink falls back to -no-pie for an archive it cannot fix, but the one jolt
  # carries is jolt's to get right.
  ./configure \
    --installprefix="$prefix" \
    --disable-x11 \
    --disable-curses \
    --threads \
    CFLAGS+=-fPIC \
    CC="$cc" \
    ${libs:+LIBS=$libs}
  make -j"$jobs"
)

# The workarea directory is named after the machine (tarm64le, ta6le, ...);
# read it from the generated Makefile rather than guessing.
m=$(sed -n 's/^workarea=//p' "$src/Makefile" | head -n1)
if [ -z "$m" ]; then
  echo "provision-chez: no workarea= in $src/Makefile" >&2
  exit 1
fi
wa=$src/$m
csv=$prefix/lib/csv$version/$m

mkdir -p "$prefix/bin" "$csv"
cp -f "$wa/bin/$m/scheme" "$csv/scheme"
chmod 755 "$csv/scheme"
for f in petite.boot scheme.boot libkernel.a scheme.h; do
  cp -f "$wa/boot/$m/$f" "$csv/$f"
done
cp -f "$wa/zlib/libz.a" "$csv/libz.a"
cp -f "$wa/lz4/lib/liblz4.a" "$csv/liblz4.a"
# Symlinks, not Chez's hard links — the filesystem refuses those. Chez picks
# its boot file by the executable's name, so scheme-script needs a
# scheme-script.boot beside scheme.boot, as Chez's own install lays it out.
ln -sf scheme.boot "$csv/scheme-script.boot"
ln -sf scheme "$csv/petite"
ln -sf "../lib/csv$version/$m/scheme" "$prefix/bin/scheme"
ln -sf "../lib/csv$version/$m/petite" "$prefix/bin/petite"
ln -sf "../lib/csv$version/$m/scheme" "$prefix/bin/scheme-script"

if [ ! -x "$prefix/bin/scheme" ]; then
  echo "provision-chez: $prefix/bin/scheme is not executable" >&2
  exit 1
fi
: > "$stamp"
echo "provision-chez: $("$prefix/bin/scheme" --version 2>/dev/null || echo version-unknown) staged at $prefix"
