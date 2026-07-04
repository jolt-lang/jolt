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

# An uncaught error's stack trace must name the runtime-eval'd fn frames that
# survive TCO (the non-tail spine), even though the eval path registers no source
# map — "print what is available". Asserts a substring appears under "  trace:".
check_trace() {
  err="$(bin/joltc -e "$1" 2>&1 >/dev/null)"
  if printf '%s' "$err" | grep -q '  trace:' && printf '%s' "$err" | grep -q "$2"; then
    pass=$((pass + 1))
  else
    echo "  FAIL (trace): $1"
    echo "    want stderr trace to contain \`$2\`, got \`$err\`"
    fails=$((fails + 1))
  fi
}

# JOLT_TRACE opts into the tail-frame history (the ring of rings): every $2 (an
# ERE) must match the "  trace:" block. Used to assert TCO-elided frames are
# recovered and non-tail caller context survives a tail loop.
check_trace_on() {
  err="$(JOLT_TRACE=1 bin/joltc -e "$1" 2>&1 >/dev/null)"
  ok=1
  printf '%s' "$err" | grep -q '  trace:' || ok=0
  shift
  for want in "$@"; do
    printf '%s' "$err" | grep -Eq "$want" || ok=0
  done
  if [ "$ok" = 1 ]; then
    pass=$((pass + 1))
  else
    echo "  FAIL (trace-on): want [$*] in trace, got \`$err\`"
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
# a deftype whose simple name collides with a built-in host class must not shadow
# the java class: (java.io.PushbackReader. …) still builds the java reader (has
# .read), while the bare name in the deftype's own ns is the deftype. (Fresh -e
# process per check, so the deftype doesn't leak.)
check '(do (deftype PushbackReader [x]) (.read (java.io.PushbackReader. (java.io.StringReader. "A") 1)))' '65'
check '(do (deftype PushbackReader [x]) (.-x (PushbackReader. 42)))' '42'
check_loc '(throw (ex-info "boom" {}))' '  at 1:'

# A throw that crosses the eval boundary (eval / load-string) must surface its
# ex-info :message, not Chez's "attempt to apply non-procedure" noise from
# re-wrapping a raw value raised through `eval`.
check '(try (eval (read-string "(throw (ex-info \"boom\" {}))")) (catch :default e (ex-message e)))' 'boom'
check '(try (load-string "(+") (catch :default e (ex-message e)))' 'EOF while reading'
# An uncaught throw prints the ex-info message alongside its source location.
check_loc '(throw (ex-info "boom" {}))' 'boom'
check_loc '(do (+ 1 1) (/ 1 0))' '  at 1:'

# Runtime-eval'd fns aren't source-mapped, but their native frame names survive on
# the non-tail spine; the trace must show them. deepest/+ are tail calls (erased);
# middle and outer wait on a non-tail (inc …) so their frames are live at the throw.
trace_prog='(defn deepest [x] (+ x 1)) (defn middle [x] (inc (deepest x))) (defn outer [x] (inc (middle x))) (outer :nan)'
check_trace "$trace_prog" 'middle'
check_trace "$trace_prog" 'outer'

# JOLT_TRACE (tail-frame history / ring of rings). An all-tail chain is entirely
# TCO-erased from the continuation, but the history recovers every frame — incl.
# `deepest`, the actual error site.
check_trace_on '(defn deepest [x] (+ x 1)) (defn middle [x] (deepest x)) (defn outer [x] (middle x)) (outer :nan)' \
  'deepest' 'middle' 'outer'
# A tail loop (a<->b) under a NON-tail caller: the loop is confined to one rib's
# bounded inner ring, so the caller context (`driver`, `top`) is NOT flushed out —
# the point of the ring of rings.
check_trace_on '(declare b) (defn a [n] (if (zero? n) (+ :x 1) (b (dec n)))) (defn b [n] (a n)) (defn driver [] (inc (a 6))) (defn top [] (inc (driver))) (top)' \
  'driver' 'top'
# A ^long/^double return hint wraps the body in a coercion, so the hinted fn's call
# is NOT a tail call — its own frame is still live and must appear (not be elided).
check_trace_on '(defn g [n] (+ :x n)) (defn ^long f [n] (g n)) (f 3)' 'f' 'g'
# History is per top-level form: a later form's error trace shows its own frames
# (h2/u2), not frames from an earlier, already-returned form (h1/u1).
check_trace_on '(defn h1 [x] (inc x)) (defn u1 [] (inc (h1 5))) (u1) (defn h2 [x] (+ :x x)) (defn u2 [] (inc (h2 5))) (u2)' \
  'h2' 'u2'
err_stale="$(JOLT_TRACE=1 bin/joltc -e '(defn h1 [x] (inc x)) (defn u1 [] (inc (h1 5))) (u1) (defn h2 [x] (+ :x x)) (defn u2 [] (inc (h2 5))) (u2)' 2>&1 >/dev/null)"
if printf '%s' "$err_stale" | grep -q 'h1'; then
  echo "  FAIL (trace-on): stale frame h1 from an earlier form leaked into the trace"
  fails=$((fails + 1))
else
  pass=$((pass + 1))
fi

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

# A required namespace's own :as aliases must not leak into the requirer: fix.main
# aliases clojure.string as ss and requires fix.lib (which aliases clojure.set as
# ss); (ss/upper-case "hi") in main must stay clojure.string -> "HI #{1 2}".
al_out="$(JOLT_PWD="$root/test/chez/alias-leak-app" bin/joltc run -m fix.main 2>/dev/null | tail -1)"
if [ "$al_out" = "HI #{1 2}" ]; then
  pass=$((pass + 1))
else
  echo "  FAIL: a loaded ns's alias leaked into its requirer — got \`$al_out\`, want \`HI #{1 2}\`"
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

# REPL-driven development traces by default: an error in an evaluated form shows a
# tail-frame backtrace with no JOLT_TRACE set. rb tail-calls ra tail-calls +, all
# TCO-elided from the continuation — only the history recovers them.
repl_err="$(printf '(defn ra [x] (+ x 1))\n(defn rb [x] (ra x))\n(rb :nan)\n:exit\n' | bin/joltc repl 2>&1)"
if printf '%s' "$repl_err" | grep -q '  trace:' && printf '%s' "$repl_err" | grep -q 'rb'; then
  pass=$((pass + 1))
else
  echo "  FAIL: a REPL error should show a tail-frame trace by default"
  printf '%s\n' "$repl_err" | sed 's/^/    | /'
  fails=$((fails + 1))
fi
# JOLT_TRACE=0 opts out — no trace in the REPL.
repl_off="$(printf '(defn ra [x] (+ x 1))\n(defn rb [x] (ra x))\n(rb :nan)\n:exit\n' | JOLT_TRACE=0 bin/joltc repl 2>&1)"
if printf '%s' "$repl_off" | grep -q '  trace:'; then
  echo "  FAIL: JOLT_TRACE=0 should suppress the REPL trace"
  fails=$((fails + 1))
else
  pass=$((pass + 1))
fi

echo "cli smoke: $pass passed, $fails failed"
[ "$fails" -eq 0 ]
