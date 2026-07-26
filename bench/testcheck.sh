#!/bin/sh
# 64-bit integer arithmetic + test.check generator machinery, jolt vs JVM Clojure.
#
# Separate from bench/run.sh because these run in RUN mode (`jolt run`) rather
# than as AOT binaries — this is library code reached through the normal require
# path, which is how a test suite hits it.
#
#   bench/testcheck.sh           # both hosts, print the scorecard
#   NO_JVM=1 bench/testcheck.sh  # jolt only
#
# Needs the test.check jar in ~/.m2 (or network on first run) for both hosts.
set -e
cd "$(dirname "$0")"
root="$(cd .. && pwd)"
jolt="$root/bin/jolt"
TC_VERSION="1.1.3"

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
mkdir -p "$work/src"
cp testcheck.clj "$work/src/"
printf '{:paths ["src"] :deps {org.clojure/test.check {:mvn/version "%s"}}}\n' \
  "$TC_VERSION" > "$work/deps.edn"

echo "test.check / 64-bit arithmetic — run mode${NO_JVM:+ }${NO_JVM:-, vs JVM Clojure}"

jout="$work/jolt.txt"
( cd "$work" && JOLT_PWD="$work" "$jolt" run -m testcheck ) > "$jout" 2>/dev/null || {
  echo "error: the jolt run failed" >&2
  ( cd "$work" && JOLT_PWD="$work" "$jolt" run -m testcheck ) >&2 || true
  exit 1
}

vout="$work/jvm.txt"
if [ -z "$NO_JVM" ]; then
  ( cd "$work" && clojure -M -m testcheck ) > "$vout" 2>/dev/null || {
    echo "error: the JVM Clojure run failed" >&2; exit 1; }
fi

# label|n|ms per line, in a fixed order from both hosts — join on the label.
while IFS='|' read -r label n jms; do
  [ -n "$label" ] || continue
  if [ -z "$NO_JVM" ]; then
    vms="$(awk -F'|' -v l="$label" '$1==l{print $3}' "$vout")"
    ratio=$(awk "BEGIN{ if (\"$vms\"+0>0 && \"$jms\"+0>0) printf \"%.1fx\", (\"$jms\"+0)/(\"$vms\"+0); else printf \"-\" }")
    printf '%-32s x%-7s jolt %8s ms   jvm %8s ms   %s\n' "$label" "$n" "$jms" "${vms:--}" "$ratio"
  else
    printf '%-32s x%-7s jolt %8s ms\n' "$label" "$n" "$jms"
  fi
done < "$jout"
