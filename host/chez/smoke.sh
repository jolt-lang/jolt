#!/bin/sh
# CLI smoke: exercise the real joltc process end to end — core eval, runtime
# eval/load-string, runtime defmacro, futures, and the numeric tower. The in-process
# corpus/unit gates cover semantics in depth; this confirms the CLI entry itself.
root="$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)"
cd "$root"

# JOLT_BIN overrides the joltc under test (make test points it at the freshly
# built target/release/joltc — 10x faster boot than script mode; the explicit
# script-mode case below keeps the source-load path covered).
joltc="${JOLT_BIN:-bin/joltc}"

fails=0
check() {
  got="$($joltc -e "$1" 2>/dev/null | tail -1)"
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
  err="$($joltc -e "$1" 2>&1 >/dev/null)"
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
  err="$($joltc -e "$1" 2>&1 >/dev/null)"
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
  err="$(JOLT_TRACE=1 $joltc -e "$1" 2>&1 >/dev/null)"
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
err_stale="$(JOLT_TRACE=1 $joltc -e '(defn h1 [x] (inc x)) (defn u1 [] (inc (h1 5))) (u1) (defn h2 [x] (+ :x x)) (defn u2 [] (inc (h2 5))) (u2)' 2>&1 >/dev/null)"
if printf '%s' "$err_stale" | grep -q 'h1'; then
  echo "  FAIL (trace-on): stale frame h1 from an earlier form leaked into the trace"
  fails=$((fails + 1))
else
  pass=$((pass + 1))
fi
# A file-backed project run maps each runtime-compiled frame to ns/name (file:line)
# — the eval path registers source in trace mode, so the trace isn't bare names.
tr_proj="$(mktemp -d)"
mkdir -p "$tr_proj/src/tp"
printf '{:paths ["src"] :aliases {:run {:main-opts ["-m" "tp.core"]}}}\n' > "$tr_proj/deps.edn"
printf '(ns tp.core)\n(defn deep [x] (+ x 1))\n(defn mid [x] (inc (deep x)))\n(defn -main [& _] (mid :nan))\n' > "$tr_proj/src/tp/core.clj"
tr_out="$(JOLT_TRACE=1 JOLT_PWD="$tr_proj" $joltc -M:run 2>&1)"
if printf '%s' "$tr_out" | grep -Eq 'tp\.core/deep \(.*/tp/core\.clj:2\)'; then
  pass=$((pass + 1))
else
  echo "  FAIL: JOLT_TRACE trace should map a frame to ns/name (file:line)"
  printf '%s\n' "$tr_out" | sed 's/^/    | /'
  fails=$((fails + 1))
fi
rm -rf "$tr_proj"

# CLI trailing-args / POSIX end-of-options. After -e EXPR the remaining argv are
# *command-line-args* (nil when empty); a leading "--" terminates option parsing
# and is consumed, so everything after it is literal program data.
cla_check() {
  out="$(eval "$1" 2>/dev/null | tail -1)"
  if [ "$out" = "$2" ]; then pass=$((pass + 1))
  else echo "  FAIL: $1"; echo "    want \`$2\` got \`$out\`"; fails=$((fails + 1)); fi
}
cla_check "$joltc -e '(println *command-line-args*)' one two three" '(one two three)'
cla_check "$joltc -e '(println *command-line-args*)' -- one two"      '(one two)'
cla_check "$joltc -e '(println *command-line-args*)' -- -e"           '(-e)'
cla_check "$joltc -e '(println *command-line-args*)' a -- b -- c"     '(a b -- c)'
cla_check "$joltc -e '(println *command-line-args*)'"                 'nil'
# run FILE -- ... : the "--" is consumed, "-e" stays a program arg.
rc_dir="$(mktemp -d)"; rc="$rc_dir/rc.clj"; printf '(prn *command-line-args*)\n' > "$rc"
cla_check "$joltc run \"$rc\" -- -e x" '("-e" "x")'
rm -rf "$rc_dir"
# -m NS -- ... : same end-of-options rule for a namespace -main.
mp="$(mktemp -d)"; mkdir -p "$mp/src"
printf '{:paths ["src"]}\n' > "$mp/deps.edn"
printf '(ns mcmd) (defn -main [& a] (prn *command-line-args*))\n' > "$mp/src/mcmd.clj"
cla_check "JOLT_PWD=\"$mp\" $joltc -m mcmd -- a b" '("a" "b")'
rm -rf "$mp"

# help prints usage (bare `help` and --help/-h are synonyms) and lists the
# nREPL server as a bare command.
help_out="$($joltc help 2>/dev/null)"
if printf '%s' "$help_out" | grep -q 'nrepl-server'; then
  pass=$((pass + 1))
else
  echo "  FAIL: help should list nrepl-server"
  fails=$((fails + 1))
fi
if [ "$($joltc --help 2>/dev/null)" = "$help_out" ]; then
  pass=$((pass + 1))
else
  echo "  FAIL: --help should print the same usage as help"
  fails=$((fails + 1))
fi
# version / --version are synonyms and name the version.
if $joltc version 2>/dev/null | grep -q '^jolt ' \
   && [ "$($joltc version 2>/dev/null)" = "$($joltc --version 2>/dev/null)" ]; then
  pass=$((pass + 1))
else
  echo "  FAIL: version / --version"
  fails=$((fails + 1))
fi
# bare joltc starts a REPL (bb/clj parity): piped stdin evaluates and exits.
repl_out="$(printf '(+ 1 2)\n' | $joltc 2>/dev/null)"
if printf '%s' "$repl_out" | grep -q '3'; then
  pass=$((pass + 1))
else
  echo "  FAIL: bare joltc should start a REPL (got \`$repl_out\`)"
  fails=$((fails + 1))
fi

# clojure.test extension points (assert-expr / do-report / report) need separate
# top-level forms — assert-expr must register before `is` expands — so this is a
# multi-form `joltc run`, not an -e one-liner. The file self-checks its tallies.
ct_out="$($joltc run test/chez/clojure-test.clj 2>/dev/null)"
if printf '%s' "$ct_out" | grep -q 'CLOJURE-TEST OK'; then
  pass=$((pass + 1))
else
  echo "  FAIL: clojure.test extension points"
  echo "    $(printf '%s' "$ct_out" | grep CLOJURE-TEST | tail -1)"
  fails=$((fails + 1))
fi

# clojure.pprint cl-format: a representative, JVM-certified subset of the upstream
# test_cl_format suite (~A ~S ~D ~F ~$ ~% ~& ~C ~( ~) ~{ ~} ~[ ~] ~< ~> ~T ~* ~R).
# The file tallies per-case pass/fail and emits a PPRINT OK / PPRINT FAIL sentinel.
pp_out="$($joltc run test/chez/pprint-test.clj 2>/dev/null)"
if printf '%s' "$pp_out" | grep -q 'PPRINT OK'; then
  pass=$((pass + 1))
else
  echo "  FAIL: clojure.pprint cl-format suite"
  echo "    $(printf '%s' "$pp_out" | grep PPRINT-RESULT | tail -1)"
  printf '%s' "$pp_out" | grep 'pprint FAIL' | sed 's/^/    /'
  fails=$((fails + 1))
fi

# A throwing go/thread body reports to stderr (the JVM's uncaught-exception
# handler behavior) while the channel still just closes: <!! stays nil.
thr_out="$($joltc -e "(do (require '[clojure.core.async :as a]) (pr (a/<!! (a/thread (/ 1 0)))))" 2>/tmp/jolt-smoke-thr-err)"
if [ "$thr_out" = "nil" ] && grep -q "Exception in go/thread body" /tmp/jolt-smoke-thr-err; then
  pass=$((pass + 1))
else
  echo "  FAIL: throwing (thread ...) should print an uncaught report and <!! nil"
  echo "    stdout \`$thr_out\`; stderr: $(head -1 /tmp/jolt-smoke-thr-err)"
  fails=$((fails + 1))
fi
# Same for a raw Thread body.
$joltc -e '(do (.start (Thread. (fn [] (throw (ex-info "boom" {}))))) (Thread/sleep 200))' 2>/tmp/jolt-smoke-thr2-err >/dev/null
if grep -q "Exception in Thread body" /tmp/jolt-smoke-thr2-err; then
  pass=$((pass + 1))
else
  echo "  FAIL: a throwing Thread body should print an uncaught report"
  fails=$((fails + 1))
fi

# A reader error in a required source file names the file and position.
rp="$(mktemp -d)/rproj"; mkdir -p "$rp/src"
printf '{:paths ["src"]}\n' > "$rp/deps.edn"
printf '(ns app)\n(def broken "unterminated\n' > "$rp/src/app.clj"
rerr="$(JOLT_PWD="$rp" $joltc run -m app 2>&1)"
if printf '%s' "$rerr" | grep -q 'src/app.clj:'; then
  pass=$((pass + 1))
else
  echo "  FAIL: a reader error in a file should name file:line:col"
  echo "    got: $(printf '%s' "$rerr" | head -1)"
  fails=$((fails + 1))
fi

# A malformed PROJECT deps.edn is a hard error naming the file; a git dep
# without :git/sha names the coordinate.
bp="$(mktemp -d)/badproj"; mkdir -p "$bp/src"
printf '{:paths ["src" :oops\n' > "$bp/deps.edn"
berr="$(JOLT_PWD="$bp" $joltc run -m app 2>&1)"
if printf '%s' "$berr" | grep -q 'deps.edn'; then
  pass=$((pass + 1))
else
  echo "  FAIL: malformed project deps.edn should be a hard error naming the file"
  fails=$((fails + 1))
fi
gp="$(mktemp -d)/gitproj"; mkdir -p "$gp/src"
printf '{:paths ["src"] :deps {some/dep {:git/url "https://example.com/x.git"}}}\n' > "$gp/deps.edn"
printf '(ns app)\n(defn -main [& _] (println :ok))\n' > "$gp/src/app.clj"
gerr="$(JOLT_PWD="$gp" $joltc run -m app 2>&1)"
if printf '%s' "$gerr" | grep -q 'needs :git/sha'; then
  pass=$((pass + 1))
else
  echo "  FAIL: a git dep without :git/sha should say so"
  echo "    got: $(printf '%s' "$gerr" | head -1)"
  fails=$((fails + 1))
fi

# context-bound dynamic vars: *file*/*source-path* during a load,
# *command-line-args*, *agent* inside an action, ns-map/ns-refers visibility.
ctx_out="$($joltc run test/chez/ctxvars-test.clj a1 a2 2>/dev/null)"
if printf '%s' "$ctx_out" | grep -q 'CTXVARS OK'; then
  pass=$((pass + 1))
else
  echo "  FAIL: context vars"
  printf '%s\n' "$ctx_out" | grep FAIL | head -5 | sed 's/^/    /'
  fails=$((fails + 1))
fi

# STM (refs) threaded tests: isolation, txn-leak, io! in future.
stm_out="$($joltc run test/chez/stm-test.clj 2>/dev/null)"
if printf '%s' "$stm_out" | grep -q 'STM OK'; then
  pass=$((pass + 1))
else
  echo "  FAIL: STM threaded tests"
  printf '%s\n' "$stm_out" | grep FAIL | head -5 | sed 's/^/    /'
  fails=$((fails + 1))
fi

# tap system + agent API: async delivery, error modes, held nested sends.
ta_out="$($joltc run test/chez/tap-agents-test.clj 2>/dev/null)"
if printf '%s' "$ta_out" | grep -q 'TAP-AGENTS OK'; then
  pass=$((pass + 1))
else
  echo "  FAIL: tap/agent threaded tests"
  printf '%s\n' "$ta_out" | grep FAIL | head -5 | sed 's/^/    /'
  fails=$((fails + 1))
fi

# jolt.fs — the stdlib file-system API against a scratch temp dir (glob, copy-tree,
# move, mtime round-trip, which). The file self-checks and prints one marker.
fs_out="$($joltc run test/chez/fs-test.clj 2>/dev/null)"
if printf '%s' "$fs_out" | grep -q 'FS-TEST OK'; then
  pass=$((pass + 1))
else
  echo "  FAIL: jolt.fs"
  printf '%s\n' "$fs_out" | grep FAIL | head -5 | sed 's/^/    /'
  fails=$((fails + 1))
fi

# jolt.parser — the general parser-combinator core, running rm-hull/jasentaa's
# own suite for the adopted pieces plus jolt's added combinators. Self-checks.
parser_out="$($joltc run test/chez/parser-test.clj 2>/dev/null)"
if printf '%s' "$parser_out" | grep -q 'PARSER OK'; then
  pass=$((pass + 1))
else
  echo "  FAIL: jolt.parser"
  printf '%s\n' "$parser_out" | grep FAIL | head -5 | sed 's/^/    /'
  fails=$((fails + 1))
fi

# jolt.infix — jolt's built-in infix math notation, running rm-hull/infix's own
# suite (macros/grammar/core tests). The file self-checks and prints one marker.
infix_out="$($joltc run test/chez/infix-test.clj 2>/dev/null)"
if printf '%s' "$infix_out" | grep -q 'INFIX OK'; then
  pass=$((pass + 1))
else
  echo "  FAIL: infix"
  printf '%s\n' "$infix_out" | grep FAIL | head -5 | sed 's/^/    /'
  fails=$((fails + 1))
fi

# A data reader that returns a CODE form (deps.edn data_readers.clj -> reader fn)
# must have its result spliced in and COMPILED, like Clojure — #code [:x] becomes
# (+ 40 2) and evaluates to 42, not the literal list. A project run so the source
# root's data_readers.clj is picked up.
dr_out="$(JOLT_PWD="$root/test/chez/datareader-app" $joltc run -m drtest.main 2>/dev/null)"
dr_want="42
olleh!"
if [ "$dr_out" = "$dr_want" ]; then
  pass=$((pass + 1))
else
  echo "  FAIL: data readers — got \`$dr_out\`, want 42 + olleh! (#code compiled; transitive reader-ns require)"
  fails=$((fails + 1))
fi

# A required namespace's own :as aliases must not leak into the requirer: fix.main
# aliases clojure.string as ss and requires fix.lib (which aliases clojure.set as
# ss); (ss/upper-case "hi") in main must stay clojure.string -> "HI #{1 2}".
al_out="$(JOLT_PWD="$root/test/chez/alias-leak-app" $joltc run -m fix.main 2>/dev/null | tail -1)"
if [ "$al_out" = "HI #{1 2}" ]; then
  pass=$((pass + 1))
else
  echo "  FAIL: a loaded ns's alias leaked into its requirer — got \`$al_out\`, want \`HI #{1 2}\`"
  fails=$((fails + 1))
fi

# Loader: require :reload / :reload-all, failed-load rollback, a data-reader fn
# whose var resolves surfaces a throw (not silently degraded), the LIST-libspec
# superset (use '(ns :only [x])), and the prefix-list form ((require '(pfx [c :as s]))).
# The fixture writes its own scratch ns files under a temp dir and requires them.
loader_out="$($joltc run test/chez/loader-test.clj 2>/dev/null)"
if printf '%s' "$loader_out" | grep -q 'LOADER OK'; then
  pass=$((pass + 1))
else
  echo "  FAIL: loader reload/rollback/reader-throw"
  printf '%s\n' "$loader_out" | grep FAIL | head -8 | sed 's/^/    /'
  fails=$((fails + 1))
fi

# Unit-checks the REPL read-until-complete predicate over balanced/unbalanced,
# string, comment and regex-literal inputs. A multi-form `joltc run` so jolt.main
# is loaded and its private var resolves; the file self-checks and prints a sentinel.
rr_out="$($joltc run test/chez/repl-reader-test.clj 2>/dev/null)"
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
repl_out="$(printf '(+ 1000 23)\n:repl/quit\n(* 999 9)\n' | $joltc repl 2>/dev/null)"
if printf '%s' "$repl_out" | grep -q '1023' && ! printf '%s' "$repl_out" | grep -q '8991'; then
  pass=$((pass + 1))
else
  echo "  FAIL: repl should exit on :repl/quit before later forms"
  printf '%s\n' "$repl_out" | sed 's/^/    | /'
  fails=$((fails + 1))
fi

repl_out="$(printf '(- 2024 1)\n:exit\n(* 999 9)\n' | $joltc repl 2>/dev/null)"
if printf '%s' "$repl_out" | grep -q '2023' && ! printf '%s' "$repl_out" | grep -q '8991'; then
  pass=$((pass + 1))
else
  echo "  FAIL: repl should exit on :exit before later forms"
  printf '%s\n' "$repl_out" | sed 's/^/    | /'
  fails=$((fails + 1))
fi

# A form split across lines is accumulated and evaluated once complete, with a
# secondary continuation prompt before each continued line.
repl_out="$(printf '(+ 1\n2)\n:exit\n' | $joltc repl 2>/dev/null)"
if printf '%s' "$repl_out" | grep -q '3' && ! printf '%s' "$repl_out" | grep -q 'error'; then
  pass=$((pass + 1))
else
  echo "  FAIL: repl should accumulate multi-line forms to 3"
  printf '%s\n' "$repl_out" | sed 's/^/    | /'
  fails=$((fails + 1))
fi

# A single-line regex literal is complete on its own — the #" opens a regex whose
# body (delimiters, quotes and all) must not be miscounted as unbalanced parens.
repl_out="$(printf '(re-find #"(a)(b)" "ab")\n:exit\n' | $joltc repl 2>/dev/null)"
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
repl_err="$(printf '(defn ra [x] (+ x 1))\n(defn rb [x] (ra x))\n(rb :nan)\n:exit\n' | $joltc repl 2>&1)"
if printf '%s' "$repl_err" | grep -q '  trace:' && printf '%s' "$repl_err" | grep -q 'rb'; then
  pass=$((pass + 1))
else
  echo "  FAIL: a REPL error should show a tail-frame trace by default"
  printf '%s\n' "$repl_err" | sed 's/^/    | /'
  fails=$((fails + 1))
fi
# JOLT_TRACE=0 opts out — no trace in the REPL.
repl_off="$(printf '(defn ra [x] (+ x 1))\n(defn rb [x] (ra x))\n(rb :nan)\n:exit\n' | JOLT_TRACE=0 $joltc repl 2>&1)"
if printf '%s' "$repl_off" | grep -q '  trace:'; then
  echo "  FAIL: JOLT_TRACE=0 should suppress the REPL trace"
  fails=$((fails + 1))
else
  pass=$((pass + 1))
fi

# script-mode boot: bin/joltc (chez --script over the seed source) must still
# work even when the rest of the smoke runs against a prebuilt JOLT_BIN.
if [ "$(bin/joltc -e '(+ 20 22)' 2>/dev/null | tail -1)" = "42" ]; then
  pass=$((pass + 1))
else
  echo "  FAIL: script-mode bin/joltc boot"
  fails=$((fails + 1))
fi

echo "cli smoke: $pass passed, $fails failed"
[ "$fails" -eq 0 ]
