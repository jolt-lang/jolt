#!/bin/sh
# Compile a spread of forms exercising every IR op with JOLT_IR_VALIDATE=1, and
# fail if the jolt.ir schema validator (jolt.ir/tree-problems, hooked in
# run-passes) reports any problem — i.e. the analyzer or a pass produced a node
# with an unknown :op or a missing required key. Pins the schema in
# jolt-core/jolt/ir.clj against what the compiler actually builds.
root="$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)"
cd "$root"
JOLTC="${JOLTC:-bin/joltc}"

# One program covering the op vocabulary: const/vector/map/set literals, if/do,
# let/loop/recur, fn multi-arity + variadic, invoke, def/declare/defmacro,
# quote, throw/try-catch-finally, host-static / host-new / host-call / set-field,
# regex/inst/uuid/bigdec reader literals, and a coerce via a ^double hint.
prog='(do
  (def cs {:a [1 2] :b #{3 4} :c "s"})
  (defn arith [a b] (+ (* a b) (- a b) (/ a 2) (mod a 3) (bit-and a b)))
  (defn ^double dsq [^double x] (* x x))
  (defn multi ([x] x) ([x y] [x y]) ([x y & zs] (count zs)))
  (defn ctrl [n]
    (loop [i 0 acc 0]
      (if (< i n) (recur (inc i) (+ acc i)) acc)))
  (defn exc [x]
    (try (throw (ex-info "e" {}))
         (catch RuntimeException e (.getMessage e))
         (finally (do (quote done)))))
  (def sb (StringBuilder.))
  (defn host [s] (.append sb s))
  (def ^:dynamic *dv* 1)
  (defn setdv [] (binding [*dv* 0] (set! *dv* 5)))
  (def mx (Math/sqrt 2.0))
  (defmacro twice [x] (list (quote do) x x))
  (declare later)
  (def rx #"ab+c")
  (def dt #inst "2021-06-01")
  (def uu #uuid "12345678-1234-1234-1234-123456789abc")
  (def bd 1.5M)
  :ok)'

out=$(JOLT_IR_VALIDATE=1 "$JOLTC" -e "$prog" 2>&1)
problems=$(printf '%s\n' "$out" | grep 'IR-VALIDATE')
if [ -n "$problems" ]; then
  echo "ir-validate: FAILED — schema problems (fix ir.clj node-ops/required-node-keys or the offending pass):"
  printf '%s\n' "$problems" | sed 's/^/    /'
  exit 1
fi
# sanity: the program itself compiled+ran (no stray error swallowed the output)
if ! printf '%s\n' "$out" | grep -q ':ok'; then
  echo "ir-validate: FAILED — program did not evaluate to :ok:"
  printf '%s\n' "$out" | tail -5 | sed 's/^/    /'
  exit 1
fi
echo "ir-validate: passed"
