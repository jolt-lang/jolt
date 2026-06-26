#!/bin/sh
# CLI smoke: exercise the real bin/joltc process end to end — core eval, runtime
# eval/load-string, runtime defmacro, futures, and the numeric tower. The in-process
# corpus/unit gates cover semantics in depth; this confirms the CLI entry itself.
root="$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)"
cd "$root"

fails=0
check() {
  got="$(bin/joltc -e "$1" 2>/dev/null | tail -1)"
  if [ "$got" = "$2" ]; then
    pass=$((pass + 1))
  else
    echo "  FAIL: $1"
    echo "    want \`$2\` got \`$got\`"
    fails=$((fails + 1))
  fi
}
pass=0

# An uncaught error reports the source location of the top-level form (stderr).
check_loc() {
  err="$(bin/joltc -e "$1" 2>&1 >/dev/null)"
  if printf '%s' "$err" | grep -q "$2"; then
    pass=$((pass + 1))
  else
    echo "  FAIL (loc): $1"
    echo "    want stderr to contain \`$2\`, got \`$err\`"
    fails=$((fails + 1))
  fi
}

check '(+ 1 2)' '3'
check '(defn fib [n] (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))) (fib 15)' '610'
check '(->> (range 10) (filter even?) (map (fn [x] (* x x))) (reduce +))' '120'
check '(let [{:keys [a b] :or {b 99}} {:a 1}] [a b])' '[1 99]'
check '(map inc [1 2 3])' '(2 3 4)'
check '(require [clojure.string :as s]) (s/upper-case "hello")' 'HELLO'
check '(eval (quote (+ 1 2)))' '3'
check '(load-string "(def y 5) (* y y)")' '25'
check '(defmacro add1 [x] (list (quote +) x 1)) (add1 10)' '11'
check '(deref (future (+ 1 2)))' '3'
check '(/ 1 2)' '1/2'
check '(= 3 3.0)' 'false'
check '(== 3 3.0)' 'true'
check_loc '(throw (ex-info "boom" {}))' '  at 1:'
check_loc '(do (+ 1 1) (/ 1 0))' '  at 1:'

echo "cli smoke: $pass passed, $fails failed"
[ "$fails" -eq 0 ]
