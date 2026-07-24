#!/bin/sh
# Isolated A/B/A bench: A = parent compiler (HEAD~1), B = current (mine).
# Order A1 -> B -> A2 to detect drift. Restores the working tree at the end.
set -e
cd "$(dirname "$0")/.."
root="$PWD"
benchdir="$root/bench"
joltc="$root/bin/joltc"

# Chez csv for the optimized build (nested <csv>/<machine> dir, e.g. csv10.4.1/tarm64osx)
csv="$JOLT_CHEZ_CSV"
if [ -z "$csv" ] || [ ! -f "$csv/libkernel.a" ]; then
  for d in "$(brew --prefix chezscheme 2>/dev/null)/lib"/csv*/*/; do
    [ -f "${d}libkernel.a" ] && csv="${d%/}" && break
  done
fi
export JOLT_CHEZ_CSV="$csv"
[ -f "$csv/libkernel.a" ] || { echo "no chez csv"; exit 1; }

# All three are overridable so a stage can be A/B'd against an exact commit pair
# (ABA_BASE/ABA_TARGET) and include the runtime .ss files it touches (COMPILER_FILES).
COMPILER_FILES="${COMPILER_FILES:-host/chez/seed/image.ss jolt-core/jolt/passes/types.clj}"
BENCHES="${BENCHES:-fib:30 mandelbrot:200 collections:30000 mono-dispatch:2000 dispatch:2000 binary-trees:14}"
RUNS=5
BASE="${ABA_BASE:-HEAD~1}"
TARGET="${ABA_TARGET:-HEAD}"

stash_compiler () { # $1 = ref
  git checkout "$1" -- $COMPILER_FILES
}

time_bench () { # echoes "mean" value (ms) averaged over RUNS builds... no: one build, RUNS runs
  spec="$1"; ns="${spec%%:*}"; arg="${spec##*:}"
  bindir="$(mktemp -d)"
  if ! ( cd "$benchdir" && export JOLT_PWD="$PWD" && "$joltc" build -m "$ns" -o "$bindir/$ns" --direct-link --opt ) >/tmp/aba_build.log 2>&1; then
    echo "BUILD_FAIL"; rm -rf "$bindir"; cat /tmp/aba_build.log | tail -3; return
  fi
  sum=0; n=0
  i=0; while [ $i -lt $RUNS ]; do
    m=$("$bindir/$ns" "$arg" 2>/dev/null | awk '/^mean:/{print $2}')
    [ -n "$m" ] && { sum=$(awk "BEGIN{print $sum+$m}"); n=$((n+1)); }
    i=$((i+1))
  done
  rm -rf "$bindir"
  if [ $n -gt 0 ]; then awk "BEGIN{printf \"%.1f\", $sum/$n}"; else echo "RUN_FAIL"; fi
}

echo "compiler files: $COMPILER_FILES"
echo "A = $BASE  (parent);  B = $TARGET  (mine)"
echo "bench order: A1 -> B -> A2, $RUNS runs each"

echo; echo "=== A1 ($BASE) ==="
stash_compiler "$BASE"
for s in $BENCHES; do printf "%-16s %s ms\n" "${s%%:*}" "$(time_bench "$s")"; done

echo; echo "=== B ($TARGET) ==="
stash_compiler "$TARGET"
for s in $BENCHES; do printf "%-16s %s ms\n" "${s%%:*}" "$(time_bench "$s")"; done

echo; echo "=== A2 ($BASE) ==="
stash_compiler "$BASE"
for s in $BENCHES; do printf "%-16s %s ms\n" "${s%%:*}" "$(time_bench "$s")"; done

echo; echo "=== restoring working tree to $TARGET ==="
stash_compiler "$TARGET"
git status --short
