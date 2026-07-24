#!/bin/sh
# Smoke a built joltc across runtime containers, bare first (out-of-the-box)
# and, when bare fails, again after installing the runtime shared libs
# (liblz4/zlib/libtinfo) — so the summary distinguishes "glibc too old, will
# never run" from "runs once the usual libs are present" (container images are
# more minimal than real installs: on desktop/server distros systemd drags in
# liblz4 and bash drags in libtinfo, so 'with deps' approximates a real box).
# Usage: ci/glibc-floor-smoke.sh <path-to-joltc> <summary-md-out>
# Runs ON THE HOST; drives `docker run` per runtime image.
set -eu
joltc="$1"; summary="$2"
dir="$(cd "$(dirname "$joltc")" && pwd)"

DEB='export DEBIAN_FRONTEND=noninteractive; apt-get update >/dev/null 2>&1; apt-get install -y -qq liblz4-1 zlib1g libtinfo6 >/dev/null 2>&1'
RPM='( dnf install -y -q lz4-libs ncurses-libs zlib || dnf install -y -q lz4-libs ncurses-libs zlib-ng-compat || yum install -y -q lz4 ncurses-libs zlib ) >/dev/null 2>&1'

# image | glibc note | dep install for the retry
IMAGES="
ubuntu:20.04|2.31|$DEB
ubuntu:22.04|2.35|$DEB
ubuntu:24.04|2.39|$DEB
debian:11|2.31|$DEB
debian:12|2.36|$DEB
rockylinux:8|2.28|$RPM
rockylinux:9|2.34|$RPM
amazonlinux:2|2.26|$RPM
amazonlinux:2023|2.34|$RPM
fedora:latest|current|$RPM
alpine:3.20|musl|true
"

echo "| runtime image | glibc | bare | with deps |" >> "$summary"
echo "|---|---|---|---|" >> "$summary"

run_smoke() { # $1=image $2=setup-cmd ("" for bare)
  docker run --rm -v "$dir":/probe:ro "$1" sh -c \
    "${2:+$2; }out=\$(/probe/joltc -e '(reduce + (range 10))' 2>&1); if [ \"\$out\" = 45 ]; then echo SMOKE-PASS; else echo \"SMOKE-FAIL: \$(echo \"\$out\" | head -1)\"; fi" \
    2>&1 | grep '^SMOKE-' | tail -1
}

echo "$IMAGES" | while IFS='|' read -r img glibc inst; do
  [ -n "$img" ] || continue
  bare="$(run_smoke "$img" "" || echo "SMOKE-FAIL: docker run failed")"
  if [ "$bare" = "SMOKE-PASS" ]; then
    bcell="PASS"; dcell="-"
  else
    bcell="FAIL \`$(echo "$bare" | sed 's/^SMOKE-FAIL: //' | cut -c1-80 | sed 's/|/\\|/g')\`"
    deps="$(run_smoke "$img" "$inst" || echo SMOKE-FAIL)"
    case "$deps" in SMOKE-PASS) dcell="PASS";; *) dcell="FAIL";; esac
  fi
  echo "| $img | $glibc | $bcell | $dcell |" >> "$summary"
done
