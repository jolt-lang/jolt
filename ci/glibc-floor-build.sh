#!/bin/sh
# Build joltc inside an old-glibc container to probe the lowest build
# environment that still produces a working binary (issue #452). Run by
# .github/workflows/glibc-floor.yml as:
#
#   docker run -v <workspace>:/src -v <chez-src>:/chez-src:ro -v <chez-cache>:/opt/chez \
#     -w /src -e JOLT_VERSION=glibc-probe <image> sh ci/glibc-floor-build.sh
#
# The Chez source tree is cloned on the host and mounted at /chez-src. The
# joltc link line (build.ss bld-link-libs) needs the shared dev libs:
# -llz4 -lz -lncurses -ltinfo -luuid — so their dev packages are installed
# here. glibc symbol versions are stamped from THIS image's glibc, which is
# the whole point of building in an old one.
set -eux

# --- toolchain + link deps per distro family ---------------------------------
if command -v apt-get >/dev/null 2>&1; then
  export DEBIAN_FRONTEND=noninteractive
  apt-get update
  apt-get install -y --no-install-recommends \
    build-essential git ca-certificates file xxd \
    liblz4-dev zlib1g-dev libncurses-dev uuid-dev
else
  pkg="$(command -v dnf || command -v yum)"
  # lz4-devel lives in EPEL on the centos7 family (manylinux2014); the pypa
  # images ship a working epel repo. gcc: the manylinux images already carry a
  # modern devtoolset gcc on PATH — only install one if none is present.
  "$pkg" install -y epel-release || true
  "$pkg" install -y make git file lz4-devel zlib-devel ncurses-devel libuuid-devel
  # xxd (build-joltc embeds the boot as C bytes) ships in vim-common on el-family
  command -v xxd >/dev/null 2>&1 || "$pkg" install -y vim-common
  command -v gcc >/dev/null 2>&1 || "$pkg" install -y gcc gcc-c++
  [ -n "${STATIC_DEPS:-}" ] && { "$pkg" install -y ncurses-static zlib-static || true; \
                                 "$pkg" install -y lz4-static libuuid-static || true; }
fi

# STATIC_DEPS=1: make -llz4/-lz/-lncurses/-ltinfo/-luuid resolve to the static
# archives by removing the .so linker symlinks (only where a .a exists, so a
# missing static package degrades to the shared lib instead of a link error).
# The result should need nothing at runtime beyond glibc itself.
if [ -n "${STATIC_DEPS:-}" ]; then
  for lib in lz4 z ncurses tinfo uuid; do
    for d in /usr/lib64 /usr/lib /usr/lib/x86_64-linux-gnu; do
      if [ -f "$d/lib$lib.a" ] && [ -e "$d/lib$lib.so" ]; then
        rm -f "$d/lib$lib.so"; echo "static-deps: lib$lib -> $d/lib$lib.a"
      fi
    done
  done
fi
gcc --version | head -1
ldd --version | head -1

# --- Chez 10.4.1 (cached across runs via the mounted /opt/chez) --------------
if [ ! -x /opt/chez/bin/scheme ]; then
  # configure writes into the source tree; copy so the read-only host mount
  # stays clean.
  cp -a /chez-src /tmp/chez-build
  cd /tmp/chez-build
  ./configure --installprefix=/opt/chez --threads --disable-x11
  make -j"$(nproc)"
  make install
fi
# the jolt build invokes `chez`; keep argv0 = scheme so boot files resolve.
printf '#!/bin/sh\nexec /opt/chez/bin/scheme "$@"\n' > /opt/chez/bin/chez
chmod +x /opt/chez/bin/chez
export PATH="/opt/chez/bin:$PATH"
chez --version

# --- joltc release build ------------------------------------------------------
cd /src
make joltc-release

# --- report + in-container smoke ---------------------------------------------
b=target/release/joltc
{
  echo "build image: ${GLIBC_FLOOR_IMAGE:-unknown}"
  ldd --version | head -1
  echo "NEEDED:"; readelf -d "$b" | grep NEEDED || true
  echo "glibc versions:"; readelf -V "$b" | grep -o 'GLIBC_[0-9.]*' | sort -Vu
} | tee target/release/glibc-report.txt
file "$b"
out="$("$b" -e '(reduce + (range 10))')"
test "$out" = "45"
echo "in-container smoke: PASS"
