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

# A throw that crosses the eval boundary (eval / load-string) must surface its
# ex-info :message, not Chez's "attempt to apply non-procedure" noise from
# re-wrapping a raw value raised through `eval`.
check '(try (eval (read-string "(throw (ex-info \"boom\" {}))")) (catch :default e (ex-message e)))' 'boom'
check '(try (load-string "(+") (catch :default e (ex-message e)))' 'EOF while reading'
# An uncaught throw prints the ex-info message alongside its source location.
check_loc '(throw (ex-info "boom" {}))' 'boom'
check_loc '(do (+ 1 1) (/ 1 0))' '  at 1:'

# --help prints usage, and lists the nREPL server under its real flag name.
help_out="$(bin/joltc --help 2>/dev/null)"
if printf '%s' "$help_out" | grep -q -- '--nrepl-server'; then
  pass=$((pass + 1))
else
  echo "  FAIL: --help should list --nrepl-server"
  fails=$((fails + 1))
fi

# clojure.test extension points (assert-expr / do-report / report) need separate
# top-level forms — assert-expr must register before `is` expands — so this is a
# multi-form `joltc run`, not an -e one-liner. The file self-checks its tallies.
ct_out="$(bin/joltc run test/chez/clojure-test.clj 2>/dev/null)"
if printf '%s' "$ct_out" | grep -q 'CLOJURE-TEST OK'; then
  pass=$((pass + 1))
else
  echo "  FAIL: clojure.test extension points"
  echo "    $(printf '%s' "$ct_out" | grep CLOJURE-TEST | tail -1)"
  fails=$((fails + 1))
fi

# A data reader that returns a CODE form (deps.edn data_readers.clj -> reader fn)
# must have its result spliced in and COMPILED, like Clojure — #code [:x] becomes
# (+ 40 2) and evaluates to 42, not the literal list. A project run so the source
# root's data_readers.clj is picked up.
dr_out="$(JOLT_PWD="$root/test/chez/datareader-app" bin/joltc run -m drtest.main 2>/dev/null | tail -1)"
if [ "$dr_out" = "42" ]; then
  pass=$((pass + 1))
else
  echo "  FAIL: code-returning data reader (#code) not compiled — got \`$dr_out\`, want 42"
  fails=$((fails + 1))
fi

# Unit-checks the REPL read-until-complete predicate over balanced/unbalanced,
# string, comment and regex-literal inputs. A multi-form `joltc run` so jolt.main
# is loaded and its private var resolves; the file self-checks and prints a sentinel.
rr_out="$(bin/joltc run test/chez/repl-reader-test.clj 2>/dev/null)"
if printf '%s' "$rr_out" | grep -q 'REPL-READER OK'; then
  pass=$((pass + 1))
else
  echo "  FAIL: repl-form-complete? predicate"
  echo "    $(printf '%s' "$rr_out" | grep REPL-READER | tail -1)"
  fails=$((fails + 1))
fi

# REPL must exit on :repl/quit / :exit — a reliable exit that works in any
# terminal, unlike ^D (which some terminals/editors don't deliver as EOF).
# Pipe: an evaluable form, the quit keyword, then a sentinel that must NOT run.
repl_out="$(printf '(+ 1000 23)\n:repl/quit\n(* 999 9)\n' | bin/joltc repl 2>/dev/null)"
if printf '%s' "$repl_out" | grep -q '1023' && ! printf '%s' "$repl_out" | grep -q '8991'; then
  pass=$((pass + 1))
else
  echo "  FAIL: repl should exit on :repl/quit before later forms"
  printf '%s\n' "$repl_out" | sed 's/^/    | /'
  fails=$((fails + 1))
fi

repl_out="$(printf '(- 2024 1)\n:exit\n(* 999 9)\n' | bin/joltc repl 2>/dev/null)"
if printf '%s' "$repl_out" | grep -q '2023' && ! printf '%s' "$repl_out" | grep -q '8991'; then
  pass=$((pass + 1))
else
  echo "  FAIL: repl should exit on :exit before later forms"
  printf '%s\n' "$repl_out" | sed 's/^/    | /'
  fails=$((fails + 1))
fi

# A form split across lines is accumulated and evaluated once complete, with a
# secondary continuation prompt before each continued line.
repl_out="$(printf '(+ 1\n2)\n:exit\n' | bin/joltc repl 2>/dev/null)"
if printf '%s' "$repl_out" | grep -q '3' && ! printf '%s' "$repl_out" | grep -q 'error'; then
  pass=$((pass + 1))
else
  echo "  FAIL: repl should accumulate multi-line forms to 3"
  printf '%s\n' "$repl_out" | sed 's/^/    | /'
  fails=$((fails + 1))
fi

# A single-line regex literal is complete on its own — the #" opens a regex whose
# body (delimiters, quotes and all) must not be miscounted as unbalanced parens.
repl_out="$(printf '(re-find #"(a)(b)" "ab")\n:exit\n' | bin/joltc repl 2>/dev/null)"
if printf '%s' "$repl_out" | grep -q 'ab' && ! printf '%s' "$repl_out" | grep -q 'error'; then
  pass=$((pass + 1))
else
  echo "  FAIL: repl should evaluate a one-line regex literal, not wait for more input"
  printf '%s\n' "$repl_out" | sed 's/^/    | /'
  fails=$((fails + 1))
fi

echo "cli smoke: $pass passed, $fails failed"
[ "$fails" -eq 0 ]
