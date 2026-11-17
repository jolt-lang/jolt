#!/data/data/com.termux/files/usr/bin/sh
# Build and check jolt on bionic (Android), inside termux/termux-docker (#943).
# Run by .github/workflows/release.yml and tests.yml on an arm64 runner as:
#
#   docker run -v <workspace>:/src <termux-image> \
#     env JOLT_VERSION=<v> sh /src/ci/termux-build.sh [GATES...]
#
# (env inside the command: the image's entrypoint drops to its user with env -i,
# so a `docker run -e` variable never arrives.)
#
# The workspace is copied into $HOME first: Termux runs as an unprivileged user
# that cannot write the host mount, and its filesystem refuses hard links. The
# Makefile's bionic path provisions the pinned Chez itself
# (host/chez/provision-chez.sh), so nothing Chez-shaped is installed here.
# Results (the binary, the report) are copied back to /src/target/release.
#
# GATES are extra make targets to run after the build (tests.yml passes one
# shard of the CI gate, `ci-shard SHARD-SET=ci SHARD=i SHARDS=n`; the release
# passes none). Every check here runs in the container,
# since the host cannot execute a bionic binary.
set -eu

# The Termux app starts every shell with termux-exec preloaded, which is what
# makes a `#!/bin/sh` script run on a system with no /bin (Chez's configure is
# one). termux-docker's entrypoint does not, so do what the app does.
if [ -z "${LD_PRELOAD:-}" ] && [ -f "$PREFIX/lib/libtermux-exec-ld-preload.so" ]; then
  export LD_PRELOAD="$PREFIX/lib/libtermux-exec-ld-preload.so"
fi

# The official mirror, not whichever one termux-docker picked at random: the
# random one can be slow enough to dominate the job.
echo "deb https://packages-cf.termux.dev/apt/termux-main stable main" > "$PREFIX/etc/apt/sources.list"
apt-get update
# Termux does not support partial upgrades: a new package against an old libc++
# or openssl fails at load time, so bring the image current first.
apt-get -y -o Dpkg::Options::=--force-confnew upgrade
# clang: cc. make/git/curl: the build and Chez provisioning. which: makes'
# init.mk looks bash up with it. xxd: build-jolt embeds the boot as C bytes.
# binutils: readelf for the dependency report. zip, perl: tools the gates use
# (a :local/root jar fixture, the manifest check). libsqlite: libsqlite3.so.0 is
# the fixture smoke's per-OS map names (Termux's sqlite package is only the
# statically linked CLI).
# The rest are the link line's libraries (build.ss bld-link-libs, bionic arm):
# ncurses for the expression editor, libuuid, and libiconv (bionic has no iconv).
apt-get install -y clang make git curl which xxd binutils zip perl libsqlite \
  ncurses libuuid libiconv

# Termux has no /tmp.
export TMPDIR="${TMPDIR:-$PREFIX/tmp}"
mkdir -p "$TMPDIR"

work="$HOME/jolt"
rm -rf "$work"
cp -R /src "$work"
cd "$work"
rm -rf target

make jolt-release

# The Chez that make provisioned, handed back for tests.yml to cache: only the
# installed prefix, not the tarball and build tree beside it. Skipped when the
# host didn't open target/ to this user (release.yml doesn't cache it).
if [ -w /src/target ]; then
  rm -rf /src/target/bionic-chez
  mkdir -p /src/target/bionic-chez
  cp -R .cache/local/chezscheme-* /src/target/bionic-chez/
fi

b=target/release/jolt
{
  echo "machine: $(cc -dumpmachine)"
  echo "NEEDED:"; readelf -d "$b" | sed -n 's/.*Shared library: \[\(.*\)\]/  \1/p'
} | tee "$TMPDIR/bionic-report.txt"

# The binary needs bionic plus the Termux packages installed above, and nothing
# else. An allowlist, so a link-line change that picks up a new dynamic
# dependency fails here rather than on a phone.
bad="$(readelf -d "$b" | sed -n 's/.*Shared library: \[\(.*\)\]/\1/p' \
       | grep -Ev '^(libc|libm|libdl|libiconv|libuuid)\.so(\.[0-9]+)?$|^libncursesw?\.so(\.[0-9]+)?$' || true)"
if [ -n "$bad" ]; then
  echo "termux-build: $b needs libraries outside the documented Termux set:"
  echo "$bad"
  exit 1
fi

"$b" --version
out="$("$b" -e '(reduce + (range 10))')"
[ "$out" = "45" ] || { echo "termux-build: jolt -e gave '$out', want 45"; exit 1; }
gz="$("$b" -e '(let [b (java.io.ByteArrayOutputStream.)] (with-open [o (java.util.zip.GZIPOutputStream. b)] (.write o (.getBytes (apply str (repeat 1000 "zip")) "UTF-8"))) [(count (slurp (java.util.zip.GZIPInputStream. (java.io.ByteArrayInputStream. (.toByteArray b))))) (< (.size b) 100)])')"
[ "$gz" = "[3000 true]" ] || { echo "termux-build: gzip round trip gave '$gz', want [3000 true]"; exit 1; }

# The binary is a self-contained compiler: build an app from a directory with
# no jolt source in it, and run it.
app="$TMPDIR/termux-app"
rm -rf "$app"
mkdir -p "$app/src/app"
printf '{:paths ["src"]}\n' > "$app/deps.edn"
printf '(ns app.core)\n(defn -main [& _] (println "built:" (reduce + (range 10))))\n' > "$app/src/app/core.clj"
( cd "$app" && "$work/$b" build -m app.core -o app )
out="$("$app/app")"
[ "$out" = "built: 45" ] || { echo "termux-build: built app ran '$out', want 'built: 45'"; exit 1; }
echo "termux-build: smoke passed"

if [ "$#" -gt 0 ]; then
  make -j"$(nproc)" "$@"
fi

mkdir -p /src/target/release
cp "$b" /src/target/release/jolt
cp "$TMPDIR/bionic-report.txt" /src/target/release/bionic-report.txt
