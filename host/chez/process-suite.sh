#!/bin/bash
# babashka/process upstream suite runner (manual conformance harness, like cts.sh
# — the CI gate is test/chez/process-test.clj). Runs the vendored
# babashka.process-test against joltc with the program-resolution fixtures staged
# and bb isolated, so the portable (non-bb) tests run to a clean result.
#
# Two things the bare `bin/joltc -M:proc babashka.process-test` can't do on its own:
#   1. The program-resolution tests copy test-resources/print-dirs.sh into
#      target/test/{on-path,cwd,workdir} and resolve a bare name via PATH — so the
#      fixtures must be present and the on-path dir must be on PATH.
#   2. Most other tests shell out to `babashka` itself; with bb on PATH they run
#      (and one can hang). We drop the dir holding bb from PATH so find-bb returns
#      nil and those tests skip (BABASHKA_TEST_ENV=jvm) instead.
#
# Everything is staged in a temp dir (no writes into the repo). Baseline result:
# 27 assertions pass, 0 fail, 0 errors; the bb-gated tests skip.
set -u
root="$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)"
vend="$root/vendor/process"
joltc="${JOLT_BIN:-$root/bin/joltc}"

if [ ! -f "$vend/test-resources/print-dirs.sh" ]; then
  echo "process-suite: skipped (git submodule update --init vendor/process)"
  exit 0
fi

chez="$(command -v chez 2>/dev/null || command -v chezscheme 2>/dev/null || command -v scheme 2>/dev/null)"
if [ -z "$chez" ]; then echo "process-suite: no chez on PATH"; exit 1; fi

# Canonicalize the temp dir: on macOS mktemp returns /var/… (a symlink to
# /private/var/…). The program-resolution tests compare a canonicalized expected
# path against the program's reported real path, so the staging root must already
# be symlink-free or those two disagree.
work="$(cd "$(mktemp -d)" && pwd -P)"
trap 'rm -rf "$work"' EXIT

# A minimal tool PATH: chez (for joltc) + the standard system dirs, but NOT the
# dir that holds bb (typically a homebrew/coursier dir) — so bb-dependent tests
# skip rather than run/hang.
toolbin="$work/bin"; mkdir -p "$toolbin"
ln -s "$chez" "$toolbin/chez"

# Staging: JOLT_PWD is a temp project whose deps.edn points (absolutely) at the
# vendored source + this suite's runner. Under it, a process/ subdir holds the
# fixtures (BABASHKA_TEST_ENV=jvm makes test-utils use the bb-submodule layout,
# i.e. "process/…" paths), with the source fixtures symlinked and target/ writable.
proj="$work/proj"
mkdir -p "$proj/process/target/test/on-path" "$proj/process/target/test/workdir" "$proj/process/target/test/cwd"
ln -s "$vend/test-resources" "$proj/process/test-resources"
ln -s "$vend/script" "$proj/process/script"
cat > "$proj/deps.edn" <<EOF
{:paths ["$root/test/chez/process-suite/src" "$vend/test" "$vend/src" "$root/vendor/fs/src"]
 :aliases {:proc {:main-opts ["-m" "process-run"]}}}
EOF
onpath="$(cd "$proj/process/target/test/on-path" && pwd -P)"

# perl for a portable timeout (the coreutils `timeout` may not be on the tool PATH).
out="$(JOLT_PWD="$proj" BABASHKA_TEST_ENV=jvm PATH="$onpath:$toolbin:/usr/bin:/bin" \
       perl -e 'alarm shift; exec @ARGV' 300 "$joltc" -M:proc babashka.process-test 2>&1)"

echo "$out" | grep -E 'PROC-RESULT|Ran '
echo "$out" | grep -E 'FAIL:|ERROR:' | head -20
res="$(echo "$out" | grep '^PROC-RESULT' | head -1)"
fails="$(echo "$res" | awk '{print $4}')"
errs="$(echo "$res" | awk '{print $5}')"
if [ -n "$res" ] && [ "${fails:-1}" = "0" ] && [ "${errs:-1}" = "0" ]; then
  echo "process-suite: clean ($(echo "$res" | awk '{print $3}') assertions passed; bb-gated tests skipped)"
  exit 0
fi
echo "process-suite: FAILED (${fails:-?} failures / ${errs:-?} errors)"
exit 1
