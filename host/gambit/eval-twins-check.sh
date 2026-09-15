#!/bin/sh
# eval-twins-check.sh — every macro the emitter can put in call position has a
# function twin in host/gambit/eval-fns.ss (POSIX sh, no Gambit needed;
# `make gambittwins`).
#
# Compiled code the Gambit boot evals at run time cannot see the unit's macros
# (eval-fns.ss says why), so a call-position macro — jolt-nil?, jolt-n+,
# jolt-identical? — has to exist as a same-named FUNCTION in the interaction
# environment, or the first eval'd use raises "Unbound variable". The set is
# derived, not remembered: the :call names of jolt-core/jolt/op_registry.clj and
# the numeric op names backend_scheme.clj emits (its jolt-l* / jolt-n* tables),
# intersected with the names host/chez/*.ss and the Gambit kernel define as
# syntax. identical? was in the registry and not in eval-fns.ss, and
# (identical? a b) died in every eval'd row while the gates stayed green.
root="$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)"
cd "$root" || exit 1
tmp="$(mktemp -d)"; trap 'rm -rf "$tmp"' EXIT

{
  grep -oE ':call "[^"]+"' jolt-core/jolt/op_registry.clj | sed 's/:call "//; s/"$//'
  grep -oE '"jolt-[ln]-?[^" ]*"' jolt-core/jolt/backend_scheme.clj | tr -d '"'
} | LC_ALL=C sort -u > "$tmp/heads"

grep -ohE '^\(define-syntax [^ )]+' host/chez/*.ss host/gambit/rt-core.ss host/gambit/hasheq.ss host/gambit/records-gambit.ss \
  | sed 's/(define-syntax //' | LC_ALL=C sort -u > "$tmp/macros"

grep -oE '\(%eval-fn \(define \(?[^ )]+' host/gambit/eval-fns.ss \
  | sed -E 's/.*\(define \(?//' | LC_ALL=C sort -u > "$tmp/twins"

LC_ALL=C comm -12 "$tmp/heads" "$tmp/macros" > "$tmp/call-macros"
LC_ALL=C comm -23 "$tmp/call-macros" "$tmp/twins" > "$tmp/missing"

n="$(grep -c . "$tmp/call-macros")"
if [ -s "$tmp/missing" ]; then
  echo "gambit eval twins: $n call-position macro(s); these have NO function twin in host/gambit/eval-fns.ss:" >&2
  sed 's/^/  /' "$tmp/missing" >&2
  echo "Add a (%eval-fn (define (NAME …) …)) for each, with the macro's own fallback body." >&2
  exit 1
fi
echo "gambit eval twins: $n call-position macro(s), every one twinned in eval-fns.ss"
