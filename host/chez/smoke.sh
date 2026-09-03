#!/bin/sh
# CLI smoke: exercise the real jolt process end to end — core eval, runtime
# eval/load-string, runtime defmacro, futures, and the numeric tower. The in-process
# corpus/unit gates cover semantics in depth; this confirms the CLI entry itself.
root="$(CDPATH='' cd -- "$(dirname -- "$0")/../.." && pwd)"
cd "$root" || exit 1

# JOLT_BIN overrides the jolt under test (make test points it at the freshly
# built target/release/jolt — 10x faster boot than script mode; the explicit
# script-mode case below keeps the source-load path covered).
jolt="${JOLT_BIN:-bin/jolt}"
# The same default, kept unwrapped: the cases that need an ABSOLUTE path to the
# binary (they cd elsewhere first) resolved $JOLT_BIN directly, so running this
# script without JOLT_BIN set built "<repo>/" — a directory — and failed three
# cases that the make gate never saw, because the gate always sets JOLT_BIN.
jolt_bin="${JOLT_BIN:-bin/jolt}"

# Every case here is a sub-second jolt invocation, so a case that does not finish
# has hung — and a hung case is invisible: `make -Oline` shows nothing for a target
# until it completes, so a CI gate sits silent until the 6-hour job limit with no
# clue which case it was. Cap each invocation instead: the case then FAILS and names
# itself. JOLT_SMOKE_TIMEOUT=0 disables the cap (for a debugger on a live case).
#
# --foreground matters, it is not a detail: without it GNU timeout runs the command
# in its OWN PROCESS GROUP so it can signal the whole group. The jolt.process case
# spawns children and asserts on destroy / alive? / a 143 SIGTERM exit, all of which
# read the process group — under a plain `timeout` that case dies with no output on
# Linux while still passing on macOS. --foreground leaves the group alone.
#
# coreutils' timeout is not on a stock macOS, and falling back to running UNCAPPED
# there meant the one host most likely to run this by hand was the one host where a
# hang could not report itself: a `make test` here sat on the poller-registration
# case for over ninety minutes behind a one-line note nobody was watching for
# (jolt-8tma). cap.sh is the same cap in POSIX sh, so every host has one.
smoke_timeout="${JOLT_SMOKE_TIMEOUT:-120}"
if [ "$smoke_timeout" = "0" ]; then
  jolt_timeout=""
else
  for t in timeout gtimeout; do
    if command -v "$t" >/dev/null 2>&1 && "$t" --foreground 1 true >/dev/null 2>&1; then
      jolt_timeout="$t --foreground $smoke_timeout"
      break
    fi
  done
  if [ -z "${jolt_timeout:-}" ]; then
    jolt_timeout="sh $root/host/chez/cap.sh $smoke_timeout"
  fi
fi
jolt="$jolt_timeout $jolt"

fails=0
check() {
  got="$($jolt -e "$1" 2>/dev/null | tail -1)"
  if [ "$got" = "$2" ]; then
    pass=$((pass + 1))
  else
    echo "  FAIL: $1"
    echo "    want \`$2\` got \`$got\`"
    fails=$((fails + 1))
  fi
}
# Two separate jolt processes must not produce the same random draw. Chez seeds
# its PRNG identically at every process start, so anything built on `random` —
# rand, rand-int, Math/random, random-uuid — replayed one fixed stream: every
# process agreed on "random" and every UUID a fleet minted collided.
check_varies() {
  a="$($jolt -e "$1" 2>/dev/null | tail -1)"
  b="$($jolt -e "$1" 2>/dev/null | tail -1)"
  if [ "$a" != "$b" ] && [ -n "$a" ]; then
    pass=$((pass + 1))
  else
    echo "  FAIL (varies): $1"
    echo "    two processes both produced \`$a\`"
    fails=$((fails + 1))
  fi
}
pass=0

# An uncaught error reports the source location of the top-level form (stderr).
check_loc() {
  err="$($jolt -e "$1" 2>&1 >/dev/null)"
  if printf '%s' "$err" | grep -q "$2"; then
    pass=$((pass + 1))
  else
    echo "  FAIL (loc): $1"
    echo "    want stderr to contain \`$2\`, got \`$err\`"
    fails=$((fails + 1))
  fi
}

# The complement of check_loc: stderr must NOT contain $2. For diagnostics whose
# value is in what they leave out.
check_no() {
  err="$($jolt -e "$1" 2>&1 >/dev/null)"
  if printf '%s' "$err" | grep -q "$2"; then
    echo "  FAIL (no): $1"
    echo "    want stderr NOT to contain \`$2\`, got \`$err\`"
    fails=$((fails + 1))
  else
    pass=$((pass + 1))
  fi
}

# An uncaught error's stack trace must name the runtime-eval'd fn frames that
# survive TCO (the non-tail spine), even though the eval path registers no source
# map — "print what is available". Asserts a substring appears under "  trace:".
check_trace() {
  err="$($jolt -e "$1" 2>&1 >/dev/null)"
  if printf '%s' "$err" | grep -q '  trace:' && printf '%s' "$err" | grep -q "$2"; then
    pass=$((pass + 1))
  else
    echo "  FAIL (trace): $1"
    echo "    want stderr trace to contain \`$2\`, got \`$err\`"
    fails=$((fails + 1))
  fi
}

# A frame whose fn was loaded from a FILE must resolve to ns/name (file:line), not
# a bare name — the runtime eval path registers each def's source, so a trace off
# the live continuation reads the same as one from an AOT build. Every $2 (an ERE)
# must match. Tracing stays OFF here: this is the default `jolt run` experience.
check_trace_src() {
  err="$($jolt -e "$1" 2>&1 >/dev/null)"
  ok=1
  printf '%s' "$err" | grep -q '  trace:' || ok=0
  shift
  for want in "$@"; do
    printf '%s' "$err" | grep -Eq "$want" || ok=0
  done
  if [ "$ok" = 1 ]; then
    pass=$((pass + 1))
  else
    echo "  FAIL (trace-src): want [$*] in trace, got \`$err\`"
    fails=$((fails + 1))
  fi
}

# JOLT_TRACE opts into the tail-frame history (the ring of rings): every $2 (an
# ERE) must match the "  trace:" block. Used to assert TCO-elided frames are
# recovered and non-tail caller context survives a tail loop.
check_trace_on() {
  err="$(JOLT_TRACE=1 $jolt -e "$1" 2>&1 >/dev/null)"
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
check '(require [clojure.string :as s]) (s/upper-case "hello")' '"HELLO"'
# reader conditionals match :bb ahead of :clj (like babashka); clause order wins
check '#?(:bb :bb-branch :clj :clj-branch)' ':bb-branch'
check '#?(:clj :clj-first :bb :bb-second)' ':clj-first'
# -M with a bare script path runs it the way clojure.main does: the file loads,
# the remaining args become *command-line-args* (jolt-s9zc).
mfile_dir="$(mktemp -d)"
printf '(println (str "script-ran:" (pr-str *command-line-args*)))' > "$mfile_dir/script.clj"
mfile_out="$($jolt -M "$mfile_dir/script.clj" a b 2>&1 | tail -1)"
if [ "$mfile_out" = 'script-ran:("a" "b")' ]; then
  pass=$((pass + 1))
else
  echo "  FAIL: -M script path"
  echo "    want \`script-ran:(\"a\" \"b\")\` got \`$mfile_out\`"
  fails=$((fails + 1))
fi
rm -rf "$mfile_dir"
# resolve returns the class for a class mapping, the var for a var
check '[(class? (resolve (quote String))) (= (resolve (quote java.util.Map)) java.util.Map) (var? (resolve (quote map)))]' '[true true true]'

# Cold-process static-field access through an IMPORTED simple name: import only
# interns a Class token, so the analyzer cannot classify the name as a class and
# the access reaches the runtime class-token arm — which must strip the -field
# spelling, fire the provider autoload, and answer a FIELD value without
# applying it as a procedure. Each check is a fresh process, which is the point:
# any earlier slash-form touch warms the registry and hides the bug (jolt-o3sw.1).
check '(import (quote (java.time LocalDate))) (str (. LocalDate -MIN))' '"-999999999-01-01"'
check '(import (quote (java.time LocalDate))) (str (. LocalDate MIN))' '"-999999999-01-01"'
check '(let [c java.time.LocalDate] (str (. c -MIN)))' '"-999999999-01-01"'
check '(let [c java.time.LocalDate] (str (. c MIN)))' '"-999999999-01-01"'

# X/from over the CORE java.time value types (no time library on deps — the
# base autoload alone must serve these; the jolt-lang/time library re-registers
# them with the zoned/offset types added). Certified on OpenJDK 20.
check '(let [ldt (java.time.LocalDateTime/parse "2020-01-02T03:04")] [(str (java.time.LocalDate/from ldt)) (str (java.time.LocalTime/from ldt)) (str (java.time.LocalDateTime/from ldt))])' '["2020-01-02" "03:04" "2020-01-02T03:04"]'
check '(try (java.time.LocalDate/from (java.time.Instant/parse "2020-01-01T00:00:00Z")) (catch java.time.DateTimeException e :dte))' ':dte'
# The source a binary serves for a namespace is jolt's own, not a same-named one
# from a later install root — the vendored Grenadine ships a jolt.deps facade for
# embedders. Roots are first-wins and the bake matches, but only for source: the
# ns itself is AOT-emitted and would keep working, so what a swap actually breaks
# is io/resource, which is how orchard resolves a namespace to a file. Get it
# backwards and an editor jumps to the wrong jolt/deps.clj.
check '(require [clojure.java.io :as io] [clojure.string :as s])
       (s/includes? (slurp (io/resource "jolt/deps.clj")) "defn resolve-project")' 'true'
# (The URL io/resource answers for a file on a source root must be ABSOLUTE; that
# is asserted in deps-alias-smoke.sh, which has fixture projects with real roots.
# Here jolt-core/stdlib are baked into the binary, so this takes the embedded-
# resource branch instead and never builds a file: URL at all.)
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
check '(try (eval (read-string "(throw (ex-info \"boom\" {}))")) (catch :default e (ex-message e)))' '"boom"'
check '(try (load-string "(+") (catch :default e (ex-message e)))' '"EOF while reading"'
# An uncaught throw prints the ex-info message alongside its source location.
check_loc '(throw (ex-info "boom" {}))' 'boom'
check_loc '(do (+ 1 1) (/ 1 0))' '  at 1:'

# An unresolved symbol offers the nearest in-scope names ("did you mean?").
check_loc '(prinltn 1)' 'did you mean'
check_loc '(prinltn 1)' 'println'
# A symbol with no close match gets the bare message, no spurious suggestion.
check_loc '(zzzptqx 1)' 'Unable to resolve symbol: zzzptqx'

# A compile-time diagnostic names the line of the OFFENDING EXPRESSION, not of the
# enclosing top-level form. `bogusxyz` is on line 3; the defn opens on line 1, and
# reporting 1:1 for it is useless in a long fn (a real case pointed 280 lines up).
nested_unresolved='(defn nestedf []
  (let [a 1]
    (bogusxyz a)))'
check_loc "$nested_unresolved" '  at 3:'
# The position must reach the machine-readable diagnostic too, not just the text.
diag_nested="$(JOLT_DIAG=edn $jolt -e "$nested_unresolved" 2>&1 >/dev/null)"
if printf '%s' "$diag_nested" | grep -q ':line 3'; then
  pass=$((pass + 1))
else
  echo "  FAIL: JOLT_DIAG=edn carries the nested line"
  echo "    got \`$diag_nested\`"
  fails=$((fails + 1))
fi

# ...and it does not dump the analyzer's own recursion as a "stack trace". Those
# frames are jolt compiling the form, never the user's program: the error is raised
# while ANALYZING, so there is no user call stack to show.
check_no "$nested_unresolved" '  trace:'
check_no "$nested_unresolved" 'analyze-list'
check_no '(prinltn 1)' '  trace:'
# A RUNTIME error still keeps its trace — only compile-time diagnostics drop it.
check_trace '(do (defn keepstrace [x] (inc (/ x 0))) (keepstrace 1))' 'keepstrace'

# JOLT_DIAG=edn emits one machine-readable EDN diagnostic line (valid EDN with
# quoted strings) carrying the structured :type/:suggestions plus source position.
diag_out="$(JOLT_DIAG=edn $jolt -e '(prinltn 1)' 2>&1 >/dev/null)"
if printf '%s' "$diag_out" | grep -q ':type :unresolved-symbol' \
   && printf '%s' "$diag_out" | grep -q ':suggestions \[' \
   && printf '%s' "$diag_out" | grep -q '"println"' \
   && printf '%s' "$diag_out" | grep -q ':line 1'; then
  pass=$((pass + 1))
else
  echo "  FAIL: JOLT_DIAG=edn structured diagnostic"
  echo "    got \`$diag_out\`"
  fails=$((fails + 1))
fi

# JOLT_CHECK surfaces the success-type checker as located warnings; off by
# default it must stay silent.
chk_on="$(JOLT_CHECK=1 $jolt -e '(first 42)' 2>&1 >/dev/null)"
if printf '%s' "$chk_on" | grep -q 'warning:' && printf '%s' "$chk_on" | grep -q 'first'; then
  pass=$((pass + 1))
else
  echo "  FAIL: JOLT_CHECK success-type warning"
  echo "    got \`$chk_on\`"
  fails=$((fails + 1))
fi
chk_off="$($jolt -e '(first 42)' 2>&1 >/dev/null)"
if printf '%s' "$chk_off" | grep -q 'warning:'; then
  echo "  FAIL: success-type warning leaked with JOLT_CHECK unset"
  fails=$((fails + 1))
else
  pass=$((pass + 1))
fi

# A missing dependency required in an ns form is blamed on the requiring file,
# not the last form of a sibling dependency that loaded first: the loader
# restores the source position after each nested load, and a throw keeps the
# failing form's own position. `outer` requires `depa` (loads) then `missing.dep`
# (fails); the error location must be outer.clj, never depa.clj.
check_loc '(do (require (quote [jolt.fs :as fs])) (def r (str (fs/create-temp-dir))) (spit (str r "/depa.clj") "(ns depa)\n(def a 1)\n(def b 2)\n(def z 9)\n") (spit (str r "/outer.clj") "(ns outer (:require depa missing.dep))\n") (jolt.host/set-source-roots! (vec (distinct (concat [r] (jolt.host/source-roots))))) (require (quote outer)))' 'outer.clj:1'

# Runtime-eval'd fns aren't source-mapped, but their native frame names survive on
# the non-tail spine; the trace must show them. deepest/+ are tail calls (erased);
# middle and outer wait on a non-tail (inc …) so their frames are live at the throw.
# clojure.core.protocols is the extension point libraries reach for when they
# implement reduce/reduce-kv over their own types (instaparse :refer's IKVReduce),
# so the namespace has to be loadable and its protocol vars resolvable.
check '(do (require (quote clojure.core.protocols)) [(some? (resolve (quote clojure.core.protocols/coll-reduce))) (some? (resolve (quote clojure.core.protocols/kv-reduce))) (some? (resolve (quote clojure.core.protocols/datafy)))])' '[true true true]'

trace_prog='(defn deepest [x] (+ x 1)) (defn middle [x] (inc (deepest x))) (defn outer [x] (inc (middle x))) (outer :nan)'
check_trace "$trace_prog" 'middle'
check_trace "$trace_prog" 'outer'

# The same spine, but loaded from a file: each frame must carry its namespace and
# the def's file:line, with tracing off. A `jolt run` trace used to print bare
# frame names because nothing registered a source map outside AOT/trace builds.
trace_src_prog='(do (require (quote [jolt.fs :as fs])) (def r (str (fs/create-temp-dir))) (spit (str r "/tracesrc.clj") "(ns tracesrc)\n(declare mx)\n(defn deepest [x] (+ x mx))\n(defn middle [x] (inc (deepest x)))\n(defn outer [x] (inc (middle x)))\n") (jolt.host/set-source-roots! (vec (distinct (concat [r] (jolt.host/source-roots))))) (require (quote tracesrc)) (tracesrc/outer 1))'
check_trace_src "$trace_src_prog" 'tracesrc/middle \(.*tracesrc\.clj:4\)' 'tracesrc/outer \(.*tracesrc\.clj:5\)'

# Two namespaces defining the same short fn name used to key the source registry
# to 'ambiguous, so the colliding frame printed a bare name instead of a location.
# Every project hit this on -main, since jolt.main has one too. Each def now
# registers under a per-var key, so collideb/middle resolves to its OWN file:line
# and never to collidea/middle.
collide_prog='(do (require (quote [jolt.fs :as fs])) (def r (str (fs/create-temp-dir))) (spit (str r "/collidea.clj") "(ns collidea)\n(defn middle [x] (inc x))\n") (spit (str r "/collideb.clj") "(ns collideb)\n(defn deepest [x] (+ x :boom))\n(defn middle [x] (inc (deepest x)))\n(defn outer [x] (inc (middle x)))\n") (jolt.host/set-source-roots! (vec (distinct (concat [r] (jolt.host/source-roots))))) (require (quote collidea)) (require (quote collideb)) (collideb/outer 1))'
check_trace_src "$collide_prog" 'collideb/middle \(.*collideb\.clj:3\)' 'collideb/outer \(.*collideb\.clj:4\)'
# The same collision with the tail-frame history on. (Whether the TCO-erased
# `deepest` shows depends on ring-vs-continuation, which the ring tests below
# cover; what matters here is that the colliding frame still resolves.)
check_trace_on "$collide_prog" 'collideb/middle'

# Ambiguity fallback. A per-var key removes the ordinary collision, but two
# registrations under ONE key must still degrade to the bare frame name rather
# than a guessed location. Both fns wrap their call in (inc ...) so the frames
# survive TCO. The ns differs by host (user under bin/jolt, jolt.main under a
# built binary), so match the frame SHAPE: a bare line with no " (file:line)".
ambig_prog='(do (defn amb [x] (inc (+ x :boom))) (defn wrap [x] (inc (amb x))) (def k (str (ns-name *ns*) "/amb")) (jolt.host/register-source! k "ns1" "amb" "a.clj" 10) (jolt.host/register-source! k "ns2" "amb" "b.clj" 20) (inc (wrap 1)))'
ambig_err="$($jolt -e "$ambig_prog" 2>&1 >/dev/null)"
if printf '%s' "$ambig_err" | grep -Eq '^    [A-Za-z0-9._-]+/amb$' && ! printf '%s' "$ambig_err" | grep -q 'a[.]clj' && ! printf '%s' "$ambig_err" | grep -q 'b[.]clj'; then
  pass=$((pass + 1))
else
  echo "  FAIL (trace ambiguity): want a bare <ns>/amb frame citing no file"
  fails=$((fails + 1))
fi

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
err_stale="$(JOLT_TRACE=1 $jolt -e '(defn h1 [x] (inc x)) (defn u1 [] (inc (h1 5))) (u1) (defn h2 [x] (+ :x x)) (defn u2 [] (inc (h2 5))) (u2)' 2>&1 >/dev/null)"
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
tr_out="$(JOLT_TRACE=1 JOLT_PWD="$tr_proj" $jolt -M:run 2>&1)"
if printf '%s' "$tr_out" | grep -Eq 'tp\.core/deep \(.*/tp/core\.clj:2\)'; then
  pass=$((pass + 1))
else
  echo "  FAIL: JOLT_TRACE trace should map a frame to ns/name (file:line)"
  printf '%s\n' "$tr_out" | sed 's/^/    | /'
  fails=$((fails + 1))
fi
rm -rf "$tr_proj"

# .jolt is a source extension alongside .clj/.cljc — the same language, marking a
# file that uses jolt-specific interop instead of portable Clojure. It resolves
# first, so a .jolt shadows a .clj of the same namespace, and it works everywhere
# a .clj does: require, a bare FILE argument, clojure.core/load, data_readers.
jx="$(mktemp -d)"
mkdir -p "$jx/src/jx"
printf '{:paths ["src"]}\n' > "$jx/deps.edn"
printf '(ns jx.util)\n(defn greet [n] (str "jolt " n))\n' > "$jx/src/jx/util.jolt"
printf '(ns jx.core (:require [jx.util :as u]))\n(defn -main [& _] (println (u/greet "ok")))\n' > "$jx/src/jx/core.jolt"
jx_check() { # label expected actual
  if [ "$2" = "$3" ]; then pass=$((pass + 1))
  else echo "  FAIL: $1"; echo "    want \`$2\` got \`$3\`"; fails=$((fails + 1)); fi
}
jx_check "require resolves a .jolt namespace" "jolt ok" \
         "$(JOLT_PWD="$jx" $jolt run -m jx.core 2>&1 | tail -1)"
# a .clj of the same namespace loses to the .jolt
printf '(ns jx.util)\n(defn greet [n] (str "clj " n))\n' > "$jx/src/jx/util.clj"
jx_check ".jolt wins over a .clj of the same ns" "jolt ok" \
         "$(JOLT_PWD="$jx" $jolt run -m jx.core 2>&1 | tail -1)"
rm "$jx/src/jx/util.jolt"
jx_check "falls back to .clj when there is no .jolt" "clj ok" \
         "$(JOLT_PWD="$jx" $jolt run -m jx.core 2>&1 | tail -1)"
# a bare FILE.jolt runs without `run`, like a bare FILE.clj
printf '(println "script" (+ 1 2))\n' > "$jx/s.jolt"
jx_check "a bare FILE.jolt runs as a script" "script 3" \
         "$(JOLT_PWD="$jx" $jolt "$jx/s.jolt" 2>&1 | tail -1)"
# clojure.core/load finds a .jolt sibling
printf "(in-ns 'jx.ld)\n(def n 7)\n" > "$jx/src/jx/incl.jolt"
printf '(ns jx.ld)\n(load "incl")\n(defn -main [& _] (println "loaded" n))\n' > "$jx/src/jx/ld.clj"
jx_check "clojure.core/load finds a .jolt" "loaded 7" \
         "$(JOLT_PWD="$jx" $jolt run -m jx.ld 2>&1 | tail -1)"
# data_readers.jolt registers tags like data_readers.clj
printf '(ns jx.rdrs (:require [clojure.string :as s]))\n(defn up [x] (s/upper-case x))\n' > "$jx/src/jx/rdrs.jolt"
printf '{jx/up jx.rdrs/up}\n' > "$jx/src/data_readers.jolt"
printf '(ns jx.rd (:require [jx.rdrs]))\n(defn -main [& _] (println #jx/up "shout"))\n' > "$jx/src/jx/rd.clj"
jx_check "data_readers.jolt registers a tag" "SHOUT" \
         "$(JOLT_PWD="$jx" $jolt run -m jx.rd 2>&1 | tail -1)"
# A RELATIVE file: URL resolves against the project directory, like the JVM
# resolving one against user.dir. Only visible with JOLT_PWD set: the launcher
# cd's to the jolt root, so a bare relative path here used to read from THERE and
# the URL came back FileNotFoundException for a file sitting in the project.
printf 'relative-url-ok\n' > "$jx/rel-probe.txt"
jx_check "a relative file: URL resolves against the project dir" "relative-url-ok" \
         "$(JOLT_PWD="$jx" $jolt -e '(print (slurp (java.net.URL. "file:rel-probe.txt")))' 2>&1 | tail -1)"
jx_check "and through openStream, which is a byte stream" "relative-url-ok" \
         "$(JOLT_PWD="$jx" $jolt -e '(print (.readLine (java.io.BufferedReader. (java.io.InputStreamReader. (.openStream (java.net.URL. "file:rel-probe.txt")) "UTF-8"))))' 2>&1 | tail -1)"
# a missing namespace names .jolt in the error, so the extension is discoverable
miss="$(JOLT_PWD="$jx" $jolt -e "(require 'jx.nope)" 2>&1 | head -1)"
case "$miss" in
  *jx/nope.jolt*) pass=$((pass + 1)) ;;
  *) echo "  FAIL: a missing ns should name .jolt in the error"
     echo "    got \`$miss\`"; fails=$((fails + 1)) ;;
esac
rm -rf "$jx"

# CLI trailing-args / POSIX end-of-options. After -e EXPR the remaining argv are
# *command-line-args* (nil when empty); a leading "--" terminates option parsing
# and is consumed, so everything after it is literal program data.
cla_check() {
  out="$(eval "$1" 2>/dev/null | tail -1)"
  if [ "$out" = "$2" ]; then pass=$((pass + 1))
  else echo "  FAIL: $1"; echo "    want \`$2\` got \`$out\`"; fails=$((fails + 1)); fi
}
cla_check "$jolt -e '(println *command-line-args*)' one two three" '(one two three)'
cla_check "$jolt -e '(println *command-line-args*)' -- one two"      '(one two)'
cla_check "$jolt -e '(println *command-line-args*)' -- -e"           '(-e)'
cla_check "$jolt -e '(println *command-line-args*)' a -- b -- c"     '(a b -- c)'
cla_check "$jolt -e '(println *command-line-args*)'"                 'nil'
# run FILE -- ... : the "--" is consumed, "-e" stays a program arg.
rc_dir="$(mktemp -d)"; rc="$rc_dir/rc.clj"; printf '(prn *command-line-args*)\n' > "$rc"
cla_check "$jolt run \"$rc\" -- -e x" '("-e" "x")'
rm -rf "$rc_dir"
# -m NS -- ... : same end-of-options rule for a namespace -main.
mp="$(mktemp -d)"; mkdir -p "$mp/src"
printf '{:paths ["src"]}\n' > "$mp/deps.edn"
printf '(ns mcmd) (defn -main [& a] (prn *command-line-args*))\n' > "$mp/src/mcmd.clj"
cla_check "JOLT_PWD=\"$mp\" $jolt -m mcmd -- a b" '("a" "b")'
rm -rf "$mp"

# stdin `-`: an (ns …) switch is honored for later forms, so a runtime refer
# into the switched-to ns is visible to a following form's analysis — the same
# as loading a file. Was lost when `-`/-e compiled every form in a hardcoded
# "user" ns (broke `ys -T jolt prog.ys | jolt -`).
ns_dir="$(mktemp -d)"; ns_prog="$ns_dir/p.clj"
printf "(intern (create-ns 'aux) 'AAA 42)\n(ns main)\n(refer 'aux :only '[AAA])\n(println AAA)\n" > "$ns_prog"
cla_check "$jolt - < \"$ns_prog\"" '42'
rm -rf "$ns_dir"

# A dialect-defined use macro owns the quoting of its arguments. The stdin
# evaluator must not quote them first and turn one module spec into (quote ...).
use_dir="$(mktemp -d)"; use_prog="$use_dir/p.clj"
printf '%s\n' \
  '(defmacro use [& specs] `(println (quote ~specs)))' \
  '(use (demo :as d))' > "$use_prog"
cla_check "$jolt - < \"$use_prog\"" '((demo :as d))'
# ... and the same for a dialect-defined require, which shadows core's just as
# a use macro does.
printf '%s\n' \
  '(defmacro require [& specs] `(println (quote ~specs)))' \
  '(require (demo :as d))' > "$use_prog"
cla_check "$jolt - < \"$use_prog\"" '((demo :as d))'
# Shadowing is what turns the convenience off, not the name: an UNshadowed
# top-level use still gets its args auto-quoted, the way require does.
printf '%s\n' \
  '(use [clojure.set :only [union]])' \
  '(prn (union #{1} #{2}))' > "$use_prog"
cla_check "$jolt - < \"$use_prog\"" '#{1 2}'
rm -rf "$use_dir"

# help prints usage (bare `help` and --help/-h are synonyms) and lists the
# nREPL server as a bare command.
help_out="$($jolt help 2>/dev/null)"
if printf '%s' "$help_out" | grep -q 'nrepl-server'; then
  pass=$((pass + 1))
else
  echo "  FAIL: help should list nrepl-server"
  fails=$((fails + 1))
fi
if [ "$($jolt --help 2>/dev/null)" = "$help_out" ]; then
  pass=$((pass + 1))
else
  echo "  FAIL: --help should print the same usage as help"
  fails=$((fails + 1))
fi
# version / --version are synonyms and name the version.
if $jolt version 2>/dev/null | grep -q '^jolt ' \
   && [ "$($jolt version 2>/dev/null)" = "$($jolt --version 2>/dev/null)" ]; then
  pass=$((pass + 1))
else
  echo "  FAIL: version / --version"
  fails=$((fails + 1))
fi
# bare jolt starts a REPL (bb/clj parity): piped stdin evaluates and exits.
repl_out="$(printf '(+ 1 2)\n' | $jolt 2>/dev/null)"
if printf '%s' "$repl_out" | grep -q '3'; then
  pass=$((pass + 1))
else
  echo "  FAIL: bare jolt should start a REPL (got \`$repl_out\`)"
  fails=$((fails + 1))
fi

# Every CLI entry starts in user, like clojure.main's. The image bakes jolt.main
# at heap build, and loading a namespace leaves it current, so -e, the REPL and a
# run's -main all evaluated in jolt.main: a REPL def landed as #'jolt.main/x
# under a prompt that said user.
ns_e="$($jolt -e '(defn h [] 1) (println (str *ns*) (str #'"'"'h))' 2>/dev/null)"
if printf '%s' "$ns_e" | grep -q "^user #'user/h"; then
  pass=$((pass + 1))
else
  echo "  FAIL: -e should evaluate in user (got \`$ns_e\`)"
  fails=$((fails + 1))
fi
repl_ns="$(printf '(in-ns (quote foo))\n(str *ns*)\n' | $jolt repl 2>/dev/null)"
if printf '%s' "$repl_ns" | grep -q '^foo=> "foo"'; then
  pass=$((pass + 1))
else
  echo "  FAIL: the REPL prompt should name the current namespace (got \`$repl_ns\`)"
  fails=$((fails + 1))
fi
nsm="$(mktemp -d)"
mkdir -p "$nsm/src/nsm"
printf '{:paths ["src"]}\n' > "$nsm/deps.edn"
printf '(ns nsm.core)\n(defn -main [& _] (println (str *ns*)))\n' > "$nsm/src/nsm/core.clj"
nsm_out="$(JOLT_PWD="$nsm" $jolt run -m nsm.core 2>/dev/null | tail -1)"
if [ "$nsm_out" = "user" ]; then
  pass=$((pass + 1))
else
  echo "  FAIL: run -m should invoke -main in user (got \`$nsm_out\`)"
  fails=$((fails + 1))
fi
rm -rf "$nsm"

# The runtime's shared side-tables (metadata, the variadic fixed-arity registry)
# must survive concurrent access: a Chez hashtable is not thread-safe, and
# unsynchronized mutation corrupts it into a SIGSEGV inside the collector or a
# hang. This runs a core.async pipeline sweep with a hand-rolled VARIADIC
# transducer, which is what actually reproduced — the built-in (map f) does not.
tt_out="$($jolt run test/chez/thread-tables.clj 2>&1)"
if printf '%s' "$tt_out" | grep -q 'THREAD-TABLES OK'; then
  pass=$((pass + 1))
else
  echo "  FAIL: shared side-tables under concurrent access"
  echo "    $(printf '%s' "$tt_out" | tail -1)"
  fails=$((fails + 1))
fi

# System/exit ends the PROCESS from whichever thread calls it, with that thread's
# status, the way System.exit does on the JVM (checked against Clojure 1.12.3).
# Chez's exit unwinds only the calling thread, so this used to be a silent no-op
# off the main thread: the program carried on and nothing said a thread had asked
# it to stop. A watchdog is the shape that wants it — it could name the hang it
# found and not end it (jolt-7xls).
ex_dir="$(mktemp -d)"
{ echo '(.start (Thread. (fn [] (Thread/sleep 100) (println "child exiting") (System/exit 3))))'
  echo '(Thread/sleep 5000)'
  echo '(println "MAIN STILL ALIVE")'
} > "$ex_dir/x.clj"
ex_out="$($jolt run "$ex_dir/x.clj" 2>&1)"; ex_st=$?
if [ "$ex_st" = "3" ] && ! printf '%s' "$ex_out" | grep -q 'MAIN STILL ALIVE'; then
  pass=$((pass + 1))
else
  echo "  FAIL: System/exit on a spawned thread should end the process with its status"
  echo "    exit $ex_st: $(printf '%s' "$ex_out" | tail -1)"
  fails=$((fails + 1))
fi
# Its own buffered output survives: the hard exit does not run Chez's exit
# handlers, so it owes the flush they would have done.
printf '(.start (Thread. (fn [] (print "partial-line") (System/exit 4))))\n(Thread/sleep 5000)\n' > "$ex_dir/y.clj"
exf_out="$($jolt run "$ex_dir/y.clj" 2>&1)"; exf_st=$?
if [ "$exf_st" = "4" ] && [ "$exf_out" = "partial-line" ]; then
  pass=$((pass + 1))
else
  echo "  FAIL: unflushed output from an exiting thread was lost (exit $exf_st, got \`$exf_out\`)"
  fails=$((fails + 1))
fi
rm -rf "$ex_dir"

# A readiness registration must never be lost. jolt.io-poller drained its pending
# registrations in two critical sections, so one landing in between was erased
# before reaching the kqueue/epoll set and the fiber waiting on that fd never
# resumed — unfixed this workload loses ~11% of its registrations.
pr_out="$($jolt run test/chez/poller-registration.clj 2>&1)"
if printf '%s' "$pr_out" | grep -q 'POLLER-REGISTRATION OK'; then
  pass=$((pass + 1))
else
  echo "  FAIL: readiness registrations lost under concurrent parking"
  # Print every POLLER line, not just the verdict. The case deliberately prints
  # POLLER-DEBUG — jolt.io-poller/debug-state captured BEFORE the round's closes
  # tear the evidence down — to say which stage dropped the wakeup: still :pending
  # (never drained), waiters parked with no event (kernel set), or ready with no
  # waiters (resume lost). `tail -1` kept only the verdict and discarded exactly
  # that, which is why the first occurrence on record could not be attributed to
  # a stage.
  #
  # WHAT THIS ACTUALLY CAUGHT (2026-08-30). Not a lost registration: the poller
  # was innocent every time. jolt.socket's io-call read errno AFTER the syscall,
  # and twice -- once for EINTR, once for EAGAIN. errno survives only until the
  # next thing that can set it, and reading it is itself a foreign call, so under
  # load recv's EAGAIN (35) read back as ENOMEM (12): the retry branch was missed,
  # the -1 fell through, and do-recv answered EOF on a live connection. The go
  # block then threw on (String. b 0 -1 "UTF-8"), its channel closed empty, and
  # alts!! returned nil instantly -- which this case reports as a lost
  # registration. 13 of 60 runs under load; 0 of 100 with errno captured at the
  # syscall. Guarded now by host/chez/errno-check.sh.
  #
  # Two things worth keeping from the hunt. The failing run is FASTER than a
  # passing one (it stops at the losing round) and no timeout elapses -- that is
  # what says "channel closed empty", not "wakeup lost". And the exception was in
  # the captured output all along; the tail -1 that this block replaced is what
  # hid it.
  #
  # To reproduce the class: saturate the CPU (eight background
  # `jolt -e '(reduce + (map inc (range 3000000)))'` loops) and run this case in a
  # loop. On an idle machine it is ~0 in 145 runs, which is why it only ever
  # appeared inside a parallel `make test`. Instrumenting the case itself changes
  # the answer -- a probe in one-round took 6-of-60 to 0-of-60 -- so measure the
  # rate before concluding anything from a quiet run, and probe the runtime
  # rather than the workload.

  # EVERYTHING, not just the POLLER lines. The cause turned out to be an
  # exception printed by the go block ("Exception in go/fiber body (channel
  # closed)"), which a ^POLLER filter drops on the floor — the same mistake as
  # the tail -1 this replaced, one level up.
  printf '%s\n' "$pr_out" | sed 's/^/    /' 
  fails=$((fails + 1))
fi

# Retiring a registration must not cost the poller its ability to report readiness.
# Two holes, one per platform, both invisible to the case above (which is read-only
# on eight distinct fds, so no fd carries two filters and none closes with a delete
# for it in flight): on kqueue a refused EV_DELETE was re-issued forever, and since
# kevent returns as soon as a changelist entry errors and reports only the error, one
# closed socket stopped every readiness report in the process; on epoll, which has no
# per-direction delete, retiring one direction of an fd took the other one with it.
pt_out="$($jolt run test/chez/poller-retirement.clj 2>&1)"
if printf '%s' "$pt_out" | grep -q 'POLLER-RETIREMENT OK'; then
  pass=$((pass + 1))
else
  echo "  FAIL: retiring a registration cost the poller a readiness report"
  # Same reasoning as the case above: print every POLLER line, since the
  # POLLER-DEBUG table says which direction of which fd was left parked. NO-SHAPE
  # is its own verdict — the round could not build the two-directions-parked state
  # it exists to test, so a pass would have meant nothing.
  if printf '%s' "$pt_out" | grep -q '^POLLER'; then
    printf '%s\n' "$pt_out" | grep '^POLLER' | sed 's/^/    /'
  else
    printf '%s\n' "$pt_out" | tail -3 | sed 's/^/    /'
  fi
  fails=$((fails + 1))
fi

# One monitor, contended by a real thread and by fibers at once (jolt-dfuo). This
# case lives HERE rather than in `make fibers` because a regression wedges every
# thread in the process, so the report has to come from outside it: the per-case
# cap above turns the wedge into a named failing case, which is the whole reason
# cap.sh exists (jolt-8tma). The case checks exclusion as well as liveness — its
# counter is a plain array element, so a lost increment is a lost monitor.
mc_out="$($jolt run test/chez/monitor-contention.clj 2>&1)"
if printf '%s' "$mc_out" | grep -q 'MONITOR-CONTENTION OK'; then
  pass=$((pass + 1))
else
  echo "  FAIL: a monitor contended by a thread and fibers did not complete"
  echo "    $(printf '%s' "$mc_out" | tail -1)"
  fails=$((fails + 1))
fi

# A fiber must never block or occupy its carrier (jolt-x1no). Fourteen checks —
# promise and future deref, Thread.join, CountDownLatch.await, a task Future's get,
# awaitTermination, a piped stream read, agent await, .waitFor on a subprocess and
# read-line — each exercised by two fibers on ONE carrier: a waiter and, behind it in
# the queue, the releaser or sibling that only runs if the waiter gave the carrier
# up. If the waiter keeps it, that second fiber never runs at all, so every case is a
# hang rather than a stall. Three have no releaser and check the other half, that a
# fiber parked with a DEADLINE is woken at it. Unfixed this reports 12 of 12.
fb_out="$($jolt run test/chez/fiber-blocking.clj 2>&1)"
if printf '%s' "$fb_out" | grep -q 'FIBER-BLOCKING OK'; then
  pass=$((pass + 1))
else
  echo "  FAIL: a fiber blocked its carrier on a condition variable"
  echo "    $(printf '%s' "$fb_out" | tail -1)"
  fails=$((fails + 1))
fi

# The same shape one lock over: a namespace whose load PARKS, required at once by
# fibers spread across eight carriers and by a thread (jolt-04ee). Here for the same
# reason as the case above — a regression used to wedge the process, so the cap is
# the report — and at eight carriers for the same reason too, since the hazard needs
# several of them rewinding parked waiters at once. concurrent-require.clj covers the
# parked load in depth but pins ONE carrier, which is the configuration where this
# cannot happen. Now that the rule is checked at the switch, a regression fails this
# deterministically (16 of 17 askers, verified by reverting the loader) rather than
# on some fraction of runs.
rc_out="$($jolt run test/chez/require-contention.clj 2>&1)"
if printf '%s' "$rc_out" | grep -q 'REQUIRE-CONTENTION OK'; then
  pass=$((pass + 1))
else
  echo "  FAIL: a parked load contended by fibers on many carriers and a thread"
  echo "    $(printf '%s' "$rc_out" | tail -1)"
  fails=$((fails + 1))
fi

# One paren too many must FAIL the file, not truncate it. The loader read forms
# with the non-top-level reader, which parks on a stray `)` rather than raising,
# and the loop read a parked position as end of input: everything after the paren
# was dropped and the run exited 0. A test file that lost its entire body that way
# still looked like a pass, which is how this was found (jolt-3amm).
stray_dir="$(mktemp -d)"
printf '(println "before")\n)\n(println "after")\n' > "$stray_dir/stray.clj"
stray_out="$($jolt run "$stray_dir/stray.clj" 2>&1)"; stray_st=$?
if [ "$stray_st" != "0" ] && printf '%s' "$stray_out" | grep -q 'Unmatched delimiter'; then
  pass=$((pass + 1))
else
  echo "  FAIL: a stray close paren should fail the load, not drop the rest of the file"
  echo "    exit $stray_st: $(printf '%s' "$stray_out" | tail -1)"
  fails=$((fails + 1))
fi
if printf '%s' "$stray_out" | grep -q 'after'; then
  echo "  FAIL: forms after the stray paren ran anyway"
  fails=$((fails + 1))
else
  pass=$((pass + 1))
fi
rm -rf "$stray_dir"

# Two threads requiring one namespace must load it once, and neither may return
# from require until it is whole. Nothing serialized load-namespace* before, so
# both ran the target's top level, and the mark-before-load that terminates a
# require cycle told the second thread "loaded" while the file was still running.
# The loader follows JLS 12.4.2 per namespace now; unfixed this reports 7 of 8
# threads returning early.
cr_out="$($jolt run test/chez/concurrent-require.clj 2>&1)"
if printf '%s' "$cr_out" | grep -q 'CONCURRENT-REQUIRE OK'; then
  pass=$((pass + 1))
else
  echo "  FAIL: concurrent require of one namespace"
  echo "    $(printf '%s' "$cr_out" | tail -1)"
  fails=$((fails + 1))
fi

# A dynamic var must still read its binding after the read path learned to skip
# the walk for vars nobody binds (jolt-3bo). The gate is the two push sites that
# do NOT go through push-thread-bindings — the loader's per-file vars and the
# agent's *agent* — because the ordinary binding path keeps working when those
# forget to flag the cell, and the var then silently reads its root.
db_out="$($jolt run test/chez/dyn-binding.clj 2>&1)"
if printf '%s' "$db_out" | grep -q 'DYN-BINDING OK'; then
  pass=$((pass + 1))
else
  echo "  FAIL: dynamic var binding semantics"
  printf '%s\n' "$db_out" | tail -4 | sed 's/^/    /'
  fails=$((fails + 1))
fi

# clojure.test extension points (assert-expr / do-report / report) need separate
# top-level forms — assert-expr must register before `is` expands — so this is a
# multi-form `jolt run`, not an -e one-liner. The file self-checks its tallies.
ct_out="$($jolt run test/chez/clojure-test.clj 2>/dev/null)"
if printf '%s' "$ct_out" | grep -q 'CLOJURE-TEST OK'; then
  pass=$((pass + 1))
else
  echo "  FAIL: clojure.test extension points"
  echo "    $(printf '%s' "$ct_out" | grep CLOJURE-TEST | tail -1)"
  fails=$((fails + 1))
fi

# The rest of clojure.test's public surface — *test-out*/with-test-out,
# successful?, compose/join-fixtures, assert-predicate/assert-any/try-expr,
# function?, *load-tests*, set-test/deftest-, test-ns/test-all-vars/run-test-var.
# Test runners and reporters reach for these; every expectation is certified
# against reference clojure.test.
cta_out="$($jolt run test/chez/clojure-test-api.clj 2>/dev/null)"
if printf '%s' "$cta_out" | grep -q 'CLOJURE-TEST-API OK'; then
  pass=$((pass + 1))
else
  echo "  FAIL: clojure.test public surface"
  echo "    $(printf '%s' "$cta_out" | grep CLOJURE-TEST-API-RESULT | tail -1)"
  printf '%s' "$cta_out" | grep 'clojure-test-api FAIL' | sed 's/^/    /'
  fails=$((fails + 1))
fi

# clojure.zip + clojure.data: the surface data.zip and cider-nrepl drive, including
# the SHAPE of a diff result (its map arm is a seq, its vector/set arms are
# vectors). Both load on require, so they cannot be corpus rows.
zd_out="$($jolt run test/chez/zip-data-test.clj 2>/dev/null)"
if printf '%s' "$zd_out" | grep -q 'ZIP-DATA OK'; then
  pass=$((pass + 1))
else
  echo "  FAIL: clojure.zip / clojure.data"
  echo "    $(printf '%s' "$zd_out" | grep ZIP-DATA-RESULT | tail -1)"
  printf '%s' "$zd_out" | grep 'zip-data FAIL' | sed 's/^/    /'
  fails=$((fails + 1))
fi

# clojure.datafy: cannot be corpus rows (a qualified reference in the same form
# as its require compiles before the require runs), and the arms only fire if the
# value reports a class more specific than Object — which is what an atom, a
# throwable and a namespace now do. The file runs unchanged on reference Clojure
# and prints the same DATAFY OK there.
df_out="$($jolt run test/chez/datafy-test.clj 2>/dev/null)"
if printf '%s' "$df_out" | grep -q 'DATAFY OK'; then
  pass=$((pass + 1))
else
  echo "  FAIL: clojure.datafy"
  printf '%s' "$df_out" | grep 'datafy FAIL' | sed 's/^/    /'
  fails=$((fails + 1))
fi

# Protocol identity across namespaces. Two namespaces each define a protocol named
# Greet and extend it to Object; they are distinct interfaces, so each namespace
# keeps its own impl, and a record in one namespace does not answer instance? for
# a same-named protocol in the other. Keying by the SIMPLE name made the second
# extend replace the first — a silent wrong answer rather than a crash, and it
# needs two namespaces, so it is a fixture rather than a corpus row.
pc_want="A=:from-A B=:from-B reified=:reified-A cross-instance=false own-instance=true satisfies=true"
pc_jolt="$(cd "$(dirname "$jolt_bin")" && pwd)/$(basename "$jolt_bin")"
pc_out="$(cd "$root/test/chez/proto-collision-app" && "$pc_jolt" -M -m app.core 2>/dev/null)"
if [ "$pc_out" = "$pc_want" ]; then
  pass=$((pass + 1))
else
  echo "  FAIL: protocol identity fixture"
  echo "    want: $pc_want"
  echo "    got:  $pc_out"
  fails=$((fails + 1))
fi

# clojure.stacktrace: the surface test runners print an errored test with (kaocha
# calls print-cause-trace, clojure.test's reporter print-stack-trace). Loads on
# require, so it cannot be a corpus row either. The frame list is empty on jolt —
# tail calls leave nothing to report — but the throwable line, the ex-data and the
# Caused by chain match reference Clojure exactly, and that is what is pinned.
st_out="$($jolt run test/chez/stacktrace-test.clj 2>/dev/null)"
if printf '%s' "$st_out" | grep -q 'STACKTRACE OK'; then
  pass=$((pass + 1))
else
  echo "  FAIL: clojure.stacktrace"
  echo "    $(printf '%s' "$st_out" | grep STACKTRACE-RESULT | tail -1)"
  printf '%s' "$st_out" | grep 'stacktrace FAIL' | sed 's/^/    /'
  fails=$((fails + 1))
fi

# clojure.pprint cl-format: a representative, JVM-certified subset of the upstream
# test_cl_format suite (~A ~S ~D ~F ~$ ~% ~& ~C ~( ~) ~{ ~} ~[ ~] ~< ~> ~T ~* ~R).
# The file tallies per-case pass/fail and emits a PPRINT OK / PPRINT FAIL sentinel.
pp_out="$($jolt run test/chez/pprint-test.clj 2>/dev/null)"
if printf '%s' "$pp_out" | grep -q 'PPRINT OK'; then
  pass=$((pass + 1))
else
  echo "  FAIL: clojure.pprint cl-format suite"
  echo "    $(printf '%s' "$pp_out" | grep PPRINT-RESULT | tail -1)"
  printf '%s' "$pp_out" | grep 'pprint FAIL' | sed 's/^/    /'
  fails=$((fails + 1))
fi

# clojure.instant: RFC3339 parsing + the #inst reader constructors. Gated here
# rather than in the corpus because the corpus runner loads no loader, so a
# load-on-require namespace cannot be required there. Every expectation in the
# file is certified against reference Clojure.
inst_out="$($jolt run test/chez/instant-test.clj 2>/dev/null)"
if printf '%s' "$inst_out" | grep -q 'INSTANT OK'; then
  pass=$((pass + 1))
else
  echo "  FAIL: clojure.instant suite"
  echo "    $(printf '%s' "$inst_out" | grep INSTANT-RESULT | tail -1)"
  printf '%s' "$inst_out" | grep 'instant FAIL' | sed 's/^/    /'
  fails=$((fails + 1))
fi

# clojure.pprint dispatch-fn gate: simple-dispatch and code-dispatch must be real
# multimethods on class so libraries can extend them (core.logic's nominal
# namespace does (. clojure.pprint/simple-dispatch addMethod …)). Gated here for
# the same reason clojure.instant is: it requires loading clojure.pprint and
# registering methods, which the corpus runner never does.
pdm_out="$($jolt run test/chez/pprint-dispatch-test.clj 2>/dev/null)"
if printf '%s' "$pdm_out" | grep -q 'PPRINT-DISPATCH OK'; then
  pass=$((pass + 1))
else
  echo "  FAIL: clojure.pprint dispatch-fn suite"
  echo "    $(printf '%s' "$pdm_out" | grep PPRINT-DISPATCH-RESULT | tail -1)"
  printf '%s' "$pdm_out" | grep 'pprint-dispatch FAIL' | sed 's/^/    /'
  fails=$((fails + 1))
fi

# A channel transducer's ex-handler receives the ORIGINAL throwable, so ex-data
# and ex-message survive. It used to be handed the raw raised condition, which
# reaches jolt code as an opaque #object[:object] with all three nil.
exh_out="$($jolt -e "(do (require '[clojure.core.async :as a]) (let [seen (atom nil) c (a/chan 1 (map (fn [x] (throw (ex-info \"err\" {:data x})))) (fn [e] (reset! seen e) :err))] (a/>!! c 3) (pr [(ex-data @seen) (ex-message @seen)])))" 2>/dev/null)"
if [ "$exh_out" = '[{:data 3} "err"]' ]; then
  pass=$((pass + 1))
else
  echo "  FAIL: transducer ex-handler should get the original ex-info"
  echo "    got \`$exh_out\`"
  fails=$((fails + 1))
fi

# A throwing go/thread body reports to stderr (the JVM's uncaught-exception
# handler behavior) while the channel still just closes: <!! stays nil.
thr_out="$($jolt -e "(do (require '[clojure.core.async :as a]) (pr (a/<!! (a/thread (/ 1 0)))))" 2>/tmp/jolt-smoke-thr-err)"
if [ "$thr_out" = "nil" ] && grep -q "Exception in go/thread body" /tmp/jolt-smoke-thr-err; then
  pass=$((pass + 1))
else
  echo "  FAIL: throwing (thread ...) should print an uncaught report and <!! nil"
  echo "    stdout \`$thr_out\`; stderr: $(head -1 /tmp/jolt-smoke-thr-err)"
  fails=$((fails + 1))
fi
# Same for a raw Thread body.
$jolt -e '(do (.start (Thread. (fn [] (throw (ex-info "boom" {}))))) (Thread/sleep 200))' 2>/tmp/jolt-smoke-thr2-err >/dev/null
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
rerr="$(JOLT_PWD="$rp" $jolt run -m app 2>&1)"
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
berr="$(JOLT_PWD="$bp" $jolt run -m app 2>&1)"
if printf '%s' "$berr" | grep -q 'deps.edn'; then
  pass=$((pass + 1))
else
  echo "  FAIL: malformed project deps.edn should be a hard error naming the file"
  fails=$((fails + 1))
fi
gp="$(mktemp -d)/gitproj"; mkdir -p "$gp/src"
printf '{:paths ["src"] :deps {some/dep {:git/url "https://example.com/x.git"}}}\n' > "$gp/deps.edn"
printf '(ns app)\n(defn -main [& _] (println :ok))\n' > "$gp/src/app.clj"
gerr="$(JOLT_PWD="$gp" $jolt run -m app 2>&1)"
if printf '%s' "$gerr" | grep -q 'needs :git/sha'; then
  pass=$((pass + 1))
else
  echo "  FAIL: a git dep without :git/sha should say so"
  echo "    got: $(printf '%s' "$gerr" | head -1)"
  fails=$((fails + 1))
fi

# context-bound dynamic vars: *file*/*source-path* during a load,
# *command-line-args*, *agent* inside an action, ns-map/ns-refers visibility.
ctx_out="$($jolt run test/chez/ctxvars-test.clj a1 a2 2>/dev/null)"
if printf '%s' "$ctx_out" | grep -q 'CTXVARS OK'; then
  pass=$((pass + 1))
else
  echo "  FAIL: context vars"
  printf '%s\n' "$ctx_out" | grep FAIL | head -5 | sed 's/^/    /'
  fails=$((fails + 1))
fi

# STM (refs) threaded tests: isolation, txn-leak, io! in future.
stm_out="$($jolt run test/chez/stm-test.clj 2>/dev/null)"
if printf '%s' "$stm_out" | grep -q 'STM OK'; then
  pass=$((pass + 1))
else
  echo "  FAIL: STM threaded tests"
  printf '%s\n' "$stm_out" | grep FAIL | head -5 | sed 's/^/    /'
  fails=$((fails + 1))
fi

# tap system + agent API: async delivery, error modes, held nested sends.
ta_out="$($jolt run test/chez/tap-agents-test.clj 2>/dev/null)"
if printf '%s' "$ta_out" | grep -q 'TAP-AGENTS OK'; then
  pass=$((pass + 1))
else
  echo "  FAIL: tap/agent threaded tests"
  printf '%s\n' "$ta_out" | grep FAIL | head -5 | sed 's/^/    /'
  fails=$((fails + 1))
fi

# jolt.fs — the stdlib file-system API against a scratch temp dir (glob, copy-tree,
# move, mtime round-trip, which). The file self-checks and prints one marker.
fs_out="$($jolt run test/chez/fs-test.clj 2>/dev/null)"
if printf '%s' "$fs_out" | grep -q 'FS-TEST OK'; then
  pass=$((pass + 1))
else
  echo "  FAIL: jolt.fs"
  printf '%s\n' "$fs_out" | grep FAIL | head -5 | sed 's/^/    /'
  fails=$((fails + 1))
fi

# java.io.File/getCanonicalPath — realpath semantics: symlinks, "." and ".."
# resolve, and a path that does not exist still canonicalizes. Self-checks, one
# marker. stderr is captured so a run that dies before its first check says why.
canon_out="$($jolt run test/chez/canonical-path-test.clj 2>&1)"
if printf '%s' "$canon_out" | grep -q 'CANONICAL-PATH OK'; then
  pass=$((pass + 1))
else
  echo "  FAIL: File.getCanonicalPath"
  printf '%s\n' "$canon_out" | tail -8 | sed 's/^/    /'
  fails=$((fails + 1))
fi

# java.io.File path normalization — every JVM constructor collapses duplicate
# separators and drops a trailing one, so a File's path is always normalized.
# "." and ".." are left alone; resolving those is getCanonicalPath's job above.
norm_out="$($jolt run test/chez/path-normalize-test.clj 2>&1)"
if printf '%s' "$norm_out" | grep -q 'PATH-NORMALIZE OK'; then
  pass=$((pass + 1))
else
  echo "  FAIL: File path normalization"
  printf '%s\n' "$norm_out" | tail -8 | sed 's/^/    /'
  fails=$((fails + 1))
fi

# java.net autoloads jolt.socket, in a FRESH process with no require: a program
# reaching for InetAddress or NetworkInterface should not have to know which
# namespace installs them, any more than it does on the JVM. Each of these is a
# cold first touch of the class in its own process.
check '(boolean (re-matches #"\d+\.\d+\.\d+\.\d+" (.getHostAddress (java.net.InetAddress/getLocalHost))))' 'true'
check '(pos? (count (enumeration-seq (java.net.NetworkInterface/getNetworkInterfaces))))' 'true'
check '(= "class java.net.Inet4Address" (str (class (java.net.InetAddress/getLoopbackAddress))))' 'true'
# System properties read through the Properties API, not just as a map.
check '(= (.getProperty (System/getProperties) "os.name") (System/getProperty "os.name"))' 'true'

# jolt.socket — the java.net.Socket/ServerSocket surface over real loopback TCP
# (roundtrip, EOF, broken pipe, ephemeral ports, class model). Self-checks, one marker.
# stderr goes into the capture: a run that dies before its first check must leave
# the exception, not an empty log.
sock_out="$($jolt run test/chez/socket-test.clj 2>&1)"
if printf '%s' "$sock_out" | grep -q 'SOCKET-TEST OK'; then
  pass=$((pass + 1))
else
  echo "  FAIL: jolt.socket"
  if printf '%s\n' "$sock_out" | grep -q '^FAIL'; then
    printf '%s\n' "$sock_out" | grep '^FAIL' | head -5 | sed 's/^/    /'
  elif [ -n "$sock_out" ]; then
    echo "    (no verdict; last check reached was:)"
    printf '%s\n' "$sock_out" | tail -3 | sed 's/^/    /'
  fi
  fails=$((fails + 1))
fi

# jolt.scheme — the Scheme escape hatch (call/proc/eval-string/defsfn, raw
# value contract, catchable errors). Self-checks, one marker.
scm_out="$($jolt run test/chez/jolt-scheme-test.clj 2>&1)"
if printf '%s' "$scm_out" | grep -q 'JOLT-SCHEME-TEST OK'; then
  pass=$((pass + 1))
else
  echo "  FAIL: jolt.scheme"
  if printf '%s\n' "$scm_out" | grep -q '^FAIL'; then
    printf '%s\n' "$scm_out" | grep '^FAIL' | head -5 | sed 's/^/    /'
  elif [ -n "$scm_out" ]; then
    echo "    (no verdict; last check reached was:)"
    printf '%s\n' "$scm_out" | tail -3 | sed 's/^/    /'
  fi
  fails=$((fails + 1))
fi

# System properties — the JVM-standard keys (os.arch in JVM spelling,
# user.name, os.version) plus the deliberate nils. Self-checks, one marker.
props_out="$($jolt run test/chez/sysprops-test.clj 2>&1)"
if printf '%s' "$props_out" | grep -q 'SYSPROPS-TEST OK'; then
  pass=$((pass + 1))
else
  echo "  FAIL: system properties"
  if printf '%s\n' "$props_out" | grep -q '^FAIL'; then
    printf '%s\n' "$props_out" | grep '^FAIL' | head -5 | sed 's/^/    /'
  elif [ -n "$props_out" ]; then
    echo "    (no verdict; last check reached was:)"
    printf '%s\n' "$props_out" | tail -3 | sed 's/^/    /'
  fi
  fails=$((fails + 1))
fi

# Class reflection — getSuperclass/getInterfaces/isAssignableFrom/isInterface
# over the jch graph, and the static-miss fallback to Class instance methods
# (JVM Clojure's (.getName String) shape). Self-checks, one marker.
refl_out="$($jolt run test/chez/class-reflect-test.clj 2>&1)"
if printf '%s' "$refl_out" | grep -q 'CLASS-REFLECT-TEST OK'; then
  pass=$((pass + 1))
else
  echo "  FAIL: class reflection"
  if printf '%s\n' "$refl_out" | grep -q '^FAIL'; then
    printf '%s\n' "$refl_out" | grep '^FAIL' | head -5 | sed 's/^/    /'
  elif [ -n "$refl_out" ]; then
    echo "    (no verdict; last check reached was:)"
    printf '%s\n' "$refl_out" | tail -3 | sed 's/^/    /'
  fi
  fails=$((fails + 1))
fi

# jolt.ffi errno — the public thread-correct errno accessor (per-platform
# thread-local slot; ENOENT/EBADF after failing syscalls, from threads and
# fibers). Self-checks, one marker; same capture rules as the socket gate.
errno_out="$($jolt run test/chez/jolt-ffi-errno-test.clj 2>&1)"
if printf '%s' "$errno_out" | grep -q 'JOLT-FFI-ERRNO-TEST OK'; then
  pass=$((pass + 1))
else
  echo "  FAIL: jolt.ffi errno"
  if printf '%s\n' "$errno_out" | grep -q '^FAIL'; then
    printf '%s\n' "$errno_out" | grep '^FAIL' | head -5 | sed 's/^/    /'
  elif [ -n "$errno_out" ]; then
    echo "    (no verdict; last check reached was:)"
    printf '%s\n' "$errno_out" | tail -3 | sed 's/^/    /'
  fi
  fails=$((fails + 1))
fi

# jolt.host's libc timezone probe must not leave TZ set. It is process global,
# and the capability check at boot ends on "UTC", so an unrestored write left
# every jolt process answering UTC from localtime() while the machine was
# somewhere else. Self-checks, one marker; same capture rules as the gates above.
# TZ is scrubbed for this one: the gate's first check is that the BOOT probe left
# no TZ behind, and an inherited TZ (a container's ENV TZ=UTC, an exporting shell)
# is indistinguishable from that leak from inside the process — the test skips the
# check rather than fail on it, so unsetting here is what keeps it covered. The
# subshell is the command substitution's own, so nothing below sees the unset.
tzprobe_out="$(unset TZ; $jolt run test/chez/jolt-tz-probe-test.clj 2>&1)"
if printf '%s' "$tzprobe_out" | grep -q 'JOLT-TZ-PROBE-TEST OK'; then
  pass=$((pass + 1))
else
  echo "  FAIL: jolt.host tz probe"
  if printf '%s\n' "$tzprobe_out" | grep -q '^FAIL'; then
    printf '%s\n' "$tzprobe_out" | grep '^FAIL' | head -5 | sed 's/^/    /'
  elif [ -n "$tzprobe_out" ]; then
    echo "    (no verdict; last check reached was:)"
    printf '%s\n' "$tzprobe_out" | tail -3 | sed 's/^/    /'
  fi
  fails=$((fails + 1))
fi

# The default zone is the machine's, as TimeZone.getDefault finds it (TZ,
# /etc/localtime, /etc/timezone): under a TZ the deprecated Date getters,
# SimpleDateFormat and Calendar read that zone's clock, its short name and
# offset render, and a zone-less parse lands on the instant it names there.
tzdef_out="$(TZ=America/New_York $jolt -e '[(.format (java.text.SimpleDateFormat. "yyyy-MM-dd HH:mm zzz Z") (java.util.Date. 1393632000000)) (.getHours (java.util.Date. 1393632000000)) (.getID (java.util.TimeZone/getDefault)) (.get (doto (java.util.Calendar/getInstance) (.setTime (java.util.Date. 1393632000000))) java.util.Calendar/HOUR_OF_DAY) (.getTime (.parse (java.text.SimpleDateFormat. "yyyy-MM-dd HH:mm") "2014-02-28 19:00")) (.getDate (java.util.Date. 114 2 1))]' 2>&1 | tail -1)"
if [ "$tzdef_out" = '["2014-02-28 19:00 EST -0500" 19 "America/New_York" 19 1393632000000 1]' ]; then
  pass=$((pass + 1))
else
  echo "  FAIL: default zone under TZ=America/New_York: $tzdef_out"
  fails=$((fails + 1))
fi

# The same file's OTHER libc probe: LC_TIME is process global just like TZ, and
# both the boot capability check and locale-name itself write it. An unrestored
# write left every jolt process in en_US.UTF-8 and moved to "C" on the first
# format call. Also gates that a locale the OS lacks reads as nil rather than
# borrowing a name from whichever locale was current.
localeprobe_out="$($jolt run test/chez/jolt-locale-probe-test.clj 2>&1)"
if printf '%s' "$localeprobe_out" | grep -q 'JOLT-LOCALE-PROBE-TEST OK'; then
  pass=$((pass + 1))
else
  echo "  FAIL: jolt.host locale probe"
  if printf '%s\n' "$localeprobe_out" | grep -q '^FAIL'; then
    printf '%s\n' "$localeprobe_out" | grep '^FAIL' | head -5 | sed 's/^/    /'
  elif [ -n "$localeprobe_out" ]; then
    echo "    (no verdict; last check reached was:)"
    printf '%s\n' "$localeprobe_out" | tail -3 | sed 's/^/    /'
  fi
  fails=$((fails + 1))
fi

# jolt.ffi :string <-> NULL. Chez's `string` type carries NULL as #f in both
# directions; these gates prove jolt's nil translates to it and back, so a C
# API where NULL is a real argument (setlocale) or a real result (getenv of an
# unset name) reads as nil in Clojure rather than raising or answering false.
strnull_out="$($jolt run test/chez/jolt-ffi-string-null-test.clj 2>&1)"
if printf '%s' "$strnull_out" | grep -q 'JOLT-FFI-STRING-NULL-TEST OK'; then
  pass=$((pass + 1))
else
  echo "  FAIL: jolt.ffi :string NULL"
  if printf '%s\n' "$strnull_out" | grep -q '^FAIL'; then
    printf '%s\n' "$strnull_out" | grep '^FAIL' | head -5 | sed 's/^/    /'
  elif [ -n "$strnull_out" ]; then
    echo "    (no verdict; last check reached was:)"
    printf '%s\n' "$strnull_out" | tail -3 | sed 's/^/    /'
  fi
  fails=$((fails + 1))
fi

# InputStream's fuller surface: readNBytes (both arities), transferTo, and
# mark/reset that actually mark. Every value checked against JVM Clojure,
# including the edges the JVM is specific about — readNBytes clamps to what
# remains and throws on a negative count, transferTo answers 0 at EOF, and
# markSupported is TRUE for ByteArrayInputStream but FALSE for FileInputStream
# (where reset() throws rather than silently not rewinding, which is what made a
# caller believe it had gone back to the start).
check '(seq (.readNBytes (java.io.ByteArrayInputStream. (byte-array [1 2 3])) 2))' '(1 2)'
check '(seq (.readNBytes (java.io.ByteArrayInputStream. (byte-array [1 2])) 9))' '(1 2)'
check '(try (.readNBytes (java.io.ByteArrayInputStream. (byte-array [1])) -1) :no-throw (catch Throwable _ :threw))' ':threw'
check '(let [b (byte-array 4)] [(.readNBytes (java.io.ByteArrayInputStream. (byte-array [1 2 3])) b 1 2) (seq b)])' '[2 (0 1 2 0)]'
check '(let [o (java.io.ByteArrayOutputStream.)] [(.transferTo (java.io.ByteArrayInputStream. (byte-array [4 5 6])) o) (seq (.toByteArray o))])' '[3 (4 5 6)]'
check '(.transferTo (java.io.ByteArrayInputStream. (byte-array 0)) (java.io.ByteArrayOutputStream.))' '0'
check '(.markSupported (java.io.ByteArrayInputStream. (byte-array [1])))' 'true'
check '(let [s (java.io.ByteArrayInputStream. (byte-array [7 8 9]))] (.read s) (.mark s 0) (.read s) (.reset s) (.read s))' '8'
check '(.markSupported (java.io.FileInputStream. "Makefile"))' 'false'
check '(let [s (java.io.FileInputStream. "Makefile")] (try (.reset s) :no-throw (catch Throwable _ :threw)))' ':threw'

# A library replacing a HOST class constructor is a process-wide substitution
# every other namespace inherits, and the symptoms land far from the cause
# (jolt-lang/http-client swaps its shim in for java.io.ByteArrayInputStream, and
# an unrelated namespace then finds .readAllBytes unresolvable). JOLT_DEBUG must
# name the class that was replaced. Registering a class the host does NOT model
# is the intended use and must stay silent.
override_out="$(JOLT_DEBUG=1 $jolt -e '(do (clojure.core/__register-class-ctor! "java.io.ByteArrayInputStream" (fn [& _] :shim)) (clojure.core/__register-class-ctor! "com.example.NotAHostClass" (fn [& _] :ok)) nil)' 2>&1)"
if printf '%s' "$override_out" | grep -q 'replaced the host constructor for java.io.ByteArrayInputStream' &&
   ! printf '%s' "$override_out" | grep -q 'com.example.NotAHostClass'; then
  pass=$((pass + 1))
else
  echo "  FAIL: host-class override is not reported under JOLT_DEBUG"
  printf '%s\n' "$override_out" | tail -3 | sed 's/^/    /'
  fails=$((fails + 1))
fi

# jolt.host/extend-class! (jolt#575) end to end, in a real process: the extend
# tier fills a gap in the shim jolt already has for java.io.File, does NOT take a
# method the shim answers, and :override does. The whole point of the seam is
# that the class keeps its OTHER methods, so each case reads a shim method back
# alongside the extension.
check '(do (jolt.host/extend-class! "java.io.File" {:methods {"shout" (fn [self] (.toUpperCase (.getName self)))}}) [(.shout (java.io.File. "a/b.txt")) (.getName (java.io.File. "a/b.txt"))])' '["B.TXT" "b.txt"]'
check '(do (jolt.host/extend-class! "java.io.File" {:methods {"getName" (fn [_] "hijacked")}}) (.getName (java.io.File. "a/b.txt")))' '"b.txt"'
check '(do (jolt.host/extend-class! "java.io.File" {:methods {"getName" (fn [_] "overridden")} :override true}) [(.getName (java.io.File. "a/b.txt")) (.getPath (java.io.File. "a/b.txt"))])' '["overridden" "a/b.txt"]'
# file-seq lowers (.isDirectory f) through jolt-host-call rather than the arm
# chain, so an override has to reach that spelling too or one method would have
# two behaviours in the same program.
check '(do (jolt.host/extend-class! "java.io.File" {:methods {"isDirectory" (fn [_] false)} :override true}) (count (file-seq (java.io.File. "host/chez/java"))))' '1'

# An override is a process-wide substitution the same way a replaced constructor
# is, so it is reported under JOLT_DEBUG and stays silent for the additive tier.
extend_out="$(JOLT_DEBUG=1 $jolt -e '(do (jolt.host/extend-class! "java.io.File" {:methods {"getName" (fn [_] "x")} :override true}) (jolt.host/extend-class! "java.io.File" {:methods {"quietlyAdded" (fn [_] :ok)}}) nil)' 2>&1)"
if printf '%s' "$extend_out" | grep -q 'overrode java.io.File/getName' &&
   ! printf '%s' "$extend_out" | grep -q 'quietlyAdded'; then
  pass=$((pass + 1))
else
  echo "  FAIL: a class-method override is not reported under JOLT_DEBUG"
  printf '%s\n' "$extend_out" | tail -3 | sed 's/^/    /'
  fails=$((fails + 1))
fi

# jolt.ffi bulk byte buffers — the foreign<->bytevector block moves under
# read-array / read-into! / write-array / read-bytes / write-bytes. Binary
# faithfulness across the unsigned-octet/signed-byte fold, offsets, and bounds.
# Self-checks, one marker; same capture rules as the errno gate.
ffibytes_out="$($jolt run test/chez/jolt-ffi-bytes-test.clj 2>&1)"
if printf '%s' "$ffibytes_out" | grep -q 'JOLT-FFI-BYTES-TEST OK'; then
  pass=$((pass + 1))
else
  echo "  FAIL: jolt.ffi bulk byte buffers"
  if printf '%s\n' "$ffibytes_out" | grep -q '^FAIL'; then
    printf '%s\n' "$ffibytes_out" | grep '^FAIL' | head -5 | sed 's/^/    /'
  elif [ -n "$ffibytes_out" ]; then
    echo "    (no verdict; last check reached was:)"
    printf '%s\n' "$ffibytes_out" | tail -3 | sed 's/^/    /'
  fi
  fails=$((fails + 1))
fi

# jolt.fibers — the public lower-level fiber API (spawn/join/monitor!, states,
# knobs) over the carrier pool. Self-checks, one marker; same capture rules as
# the socket gate above.
fib_out="$($jolt run test/chez/jolt-fibers-test.clj 2>&1)"
if printf '%s' "$fib_out" | grep -q 'JOLT-FIBERS-TEST OK'; then
  pass=$((pass + 1))
else
  echo "  FAIL: jolt.fibers"
  if printf '%s\n' "$fib_out" | grep -q '^FAIL'; then
    printf '%s\n' "$fib_out" | grep '^FAIL' | head -5 | sed 's/^/    /'
  elif [ -n "$fib_out" ]; then
    echo "    (no verdict; last check reached was:)"
    printf '%s\n' "$fib_out" | tail -3 | sed 's/^/    /'
  fi
  fails=$((fails + 1))
fi

# jolt.process — the stdlib sub-process API against real programs (capture, pipes,
# stdin, :dir/:env, exit codes, signals). The file self-checks and prints a marker.
#
# Captured to a FILE, not with $(…), unlike every other case here: this is the one
# case that spawns child processes, and a command substitution does not return until
# every writer has closed the pipe — not just until the command exits. A child that
# inherits that pipe and outlives jolt (the destroy/signal case leaves a `sleep`
# behind if destroy does not take) therefore blocks the read forever, which is how
# this case hung the gate for hours with nothing to show for it. Redirecting to a
# file gives the children a file descriptor with no reader to wait on.
# stderr goes INTO the log, not to /dev/null: if the run dies before it can print a
# single check, "FAIL: jolt.process" with nothing under it is all the gate says, and
# the reason — the exception — is exactly what was thrown away.
process_log="$(mktemp)"
# JOLT_EXE names the jolt under test for the cases that spawn a child jolt, so a
# built binary tests itself rather than whatever `jolt` is on PATH.
JOLT_EXE="$(cd "$(dirname "$jolt_bin")" && pwd)/$(basename "$jolt_bin")" \
  $jolt run test/chez/process-test.clj >"$process_log" 2>&1 || true
process_out="$(cat "$process_log")"
if printf '%s' "$process_out" | grep -q 'PROCESS-TEST OK'; then
  pass=$((pass + 1))
else
  echo "  FAIL: jolt.process"
  # the case's own FAILs if it got that far, else the last of whatever it did say —
  # an exception, or the "  .. <label>" the test prints before each check, whose LAST
  # line then names the check that blocked. Never nothing.
  if printf '%s\n' "$process_out" | grep -q '^FAIL'; then
    printf '%s\n' "$process_out" | grep '^FAIL' | head -5 | sed 's/^/    /'
  elif [ -n "$process_out" ]; then
    echo "    (no verdict; last check reached was:)"
    printf '%s\n' "$process_out" | tail -6 | sed 's/^/    /'
  else
    echo "    (no output at all — died or was killed before its first check)"
  fi
  fails=$((fails + 1))
fi
rm -f "$process_log"

# A jolt that spawns has to restore SIG_DFL for SIGCHLD before its first spawn: the
# disposition survives exec, so jolt can arrive with SIG_IGN through no choice of
# its own (a CI runner, a supervisor), and with it in place the kernel reaps every
# child and no exit status is knowable. The case sets SIG_IGN itself rather than
# inheriting it from a `trap ''` here — a shell may keep SIGCHLD for its own job
# control and not pass the ignore through exec, which is the difference between
# macOS and Linux and made the shell version vacuous in CI. Its own process because
# the restore is once-per-process, on the first spawn: process-test.clj sets
# SIG_IGN AFTER a spawn on purpose, so it cannot reach this path at all.
sigchld_log="$(mktemp)"
$jolt run test/chez/sigchld-test.clj >"$sigchld_log" 2>&1 || true
if grep -q 'SIGCHLD-TEST OK' "$sigchld_log"; then
  pass=$((pass + 1))
else
  echo "  FAIL: SIGCHLD restored before the first spawn"
  tail -3 "$sigchld_log" | sed 's/^/    /'
  fails=$((fails + 1))
fi
rm -f "$sigchld_log"

# Runtime.availableProcessors reports the host's real usable CPU count. The value
# is machine-dependent, so the expectation comes from the OS rather than a literal
# — nproc first, because it honours the CPU affinity mask the way this does (and
# the way the JVM does), where sysctl only counts the cores that exist. A wrong
# answer here is invisible to any value-comparing gate: it returned a plausible
# 1 for as long as it was hardcoded, which silently serialised pmap.
# nproc also honours OMP_NUM_THREADS / OMP_THREAD_LIMIT, which the affinity mask
# does not — unset them here or a CI runner that exports either makes this disagree
# with a perfectly correct answer.
cpu_want="$(env -u OMP_NUM_THREADS -u OMP_THREAD_LIMIT nproc 2>/dev/null \
            || sysctl -n hw.logicalcpu 2>/dev/null || echo '')"
if [ -n "$cpu_want" ]; then
  check '(.availableProcessors (Runtime/getRuntime))' "$cpu_want"
  check '(jolt.host/available-processors)' "$cpu_want"
  # pmap sizes its look-ahead window from it, so a broken count degrades pmap
  # rather than failing it — assert the seam is wired, not just present.
  check '(count (pmap inc (range 100)))' '100'
fi

# jolt.parser — the general parser-combinator core, running rm-hull/jasentaa's
# own suite for the adopted pieces plus jolt's added combinators. Self-checks.
parser_out="$($jolt run test/chez/parser-test.clj 2>/dev/null)"
if printf '%s' "$parser_out" | grep -q 'PARSER OK'; then
  pass=$((pass + 1))
else
  echo "  FAIL: jolt.parser"
  printf '%s\n' "$parser_out" | grep FAIL | head -5 | sed 's/^/    /'
  fails=$((fails + 1))
fi

# jolt.infix — jolt's built-in infix math notation, running rm-hull/infix's own
# suite (macros/grammar/core tests). The file self-checks and prints one marker.
infix_out="$($jolt run test/chez/infix-test.clj 2>/dev/null)"
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
# root's data_readers.clj is picked up. The last two lines cover a *data-readers*
# entry that holds the reader FUNCTION rather than a symbol naming it (the JVM's
# own table shape): a form result compiles, a value result is spliced.
dr_out="$(JOLT_PWD="$root/test/chez/datareader-app" $jolt run -m drtest.main 2>/dev/null)"
dr_want="42
olleh!
3
shout-value"
if [ "$dr_out" = "$dr_want" ]; then
  pass=$((pass + 1))
else
  echo "  FAIL: data readers — got \`$dr_out\`, want \`$dr_want\` (#code compiled; transitive reader-ns require; fn-valued table entries)"
  fails=$((fails + 1))
fi

# Reader macros (jolt.reader): a source file registers a #<char> reader and uses
# it BELOW in the same file, which only works because jolt reads and evaluates a
# file one top-level form at a time. Covers both tiers (form and :raw), the
# baked-in #$ interpolation, and clojure.core.strint's << over the same grammar.
rm_out="$(JOLT_PWD="$root/test/chez/readermacro-app" $jolt run -m rmtest.main 2>/dev/null)"
rm_want="[3 3]
C:\\new
interp 3 4
strint 3 4
($ % |)"
if [ "$rm_out" = "$rm_want" ]; then
  pass=$((pass + 1))
else
  echo "  FAIL: reader macros — got \`$rm_out\`, want \`$rm_want\`"
  fails=$((fails + 1))
fi

# A required namespace's own :as aliases must not leak into the requirer: fix.main
# aliases clojure.string as ss and requires fix.lib (which aliases clojure.set as
# ss); (ss/upper-case "hi") in main must stay clojure.string -> "HI #{1 2}".
al_out="$(JOLT_PWD="$root/test/chez/alias-leak-app" $jolt run -m fix.main 2>/dev/null | tail -1)"
if [ "$al_out" = "HI #{1 2}" ]; then
  pass=$((pass + 1))
else
  echo "  FAIL: a loaded ns's alias leaked into its requirer — got \`$al_out\`, want \`HI #{1 2}\`"
  fails=$((fails + 1))
fi

# Loader: require :reload / :reload-all, failed-load rollback, a data-reader fn
# whose var resolves surfaces a throw (not silently degraded), the LIST-libspec
# superset (use '(ns :only [x])), the prefix-list form ((require '(pfx [c :as s])))
# and that a require fired under a (binding [*ns* ..]) still interns each loaded
# file's defs into its own namespace.
# The fixture writes its own scratch ns files under a temp dir and requires them.
loader_out="$($jolt run test/chez/loader-test.clj 2>/dev/null)"
if printf '%s' "$loader_out" | grep -q 'LOADER OK'; then
  pass=$((pass + 1))
else
  echo "  FAIL: loader reload/rollback/reader-throw"
  printf '%s\n' "$loader_out" | grep FAIL | head -8 | sed 's/^/    /'
  fails=$((fails + 1))
fi

# Unit-checks the REPL read-until-complete predicate over balanced/unbalanced,
# string, comment and regex-literal inputs. A multi-form `jolt run` so jolt.main
# is loaded and its private var resolves; the file self-checks and prints a sentinel.
rr_out="$($jolt run test/chez/repl-reader-test.clj 2>/dev/null)"
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
repl_out="$(printf '(+ 1000 23)\n:repl/quit\n(* 999 9)\n' | $jolt repl 2>/dev/null)"
if printf '%s' "$repl_out" | grep -q '1023' && ! printf '%s' "$repl_out" | grep -q '8991'; then
  pass=$((pass + 1))
else
  echo "  FAIL: repl should exit on :repl/quit before later forms"
  printf '%s\n' "$repl_out" | sed 's/^/    | /'
  fails=$((fails + 1))
fi

repl_out="$(printf '(- 2024 1)\n:exit\n(* 999 9)\n' | $jolt repl 2>/dev/null)"
if printf '%s' "$repl_out" | grep -q '2023' && ! printf '%s' "$repl_out" | grep -q '8991'; then
  pass=$((pass + 1))
else
  echo "  FAIL: repl should exit on :exit before later forms"
  printf '%s\n' "$repl_out" | sed 's/^/    | /'
  fails=$((fails + 1))
fi

# A form split across lines is accumulated and evaluated once complete, with a
# secondary continuation prompt before each continued line.
repl_out="$(printf '(+ 1\n2)\n:exit\n' | $jolt repl 2>/dev/null)"
if printf '%s' "$repl_out" | grep -q '3' && ! printf '%s' "$repl_out" | grep -q 'error'; then
  pass=$((pass + 1))
else
  echo "  FAIL: repl should accumulate multi-line forms to 3"
  printf '%s\n' "$repl_out" | sed 's/^/    | /'
  fails=$((fails + 1))
fi

# A single-line regex literal is complete on its own — the #" opens a regex whose
# body (delimiters, quotes and all) must not be miscounted as unbalanced parens.
repl_out="$(printf '(re-find #"(a)(b)" "ab")\n:exit\n' | $jolt repl 2>/dev/null)"
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
repl_err="$(printf '(defn ra [x] (+ x 1))\n(defn rb [x] (ra x))\n(rb :nan)\n:exit\n' | $jolt repl 2>&1)"
if printf '%s' "$repl_err" | grep -q '  trace:' && printf '%s' "$repl_err" | grep -q 'rb'; then
  pass=$((pass + 1))
else
  echo "  FAIL: a REPL error should show a tail-frame trace by default"
  printf '%s\n' "$repl_err" | sed 's/^/    | /'
  fails=$((fails + 1))
fi
# JOLT_TRACE=0 opts out — no trace in the REPL.
repl_off="$(printf '(defn ra [x] (+ x 1))\n(defn rb [x] (ra x))\n(rb :nan)\n:exit\n' | JOLT_TRACE=0 $jolt repl 2>&1)"
if printf '%s' "$repl_off" | grep -q '  trace:'; then
  echo "  FAIL: JOLT_TRACE=0 should suppress the REPL trace"
  fails=$((fails + 1))
else
  pass=$((pass + 1))
fi

# -A:alias adds paths/deps then dispatches the remaining argv. An alias that
# adds a source root must NOT be clobbered by a later cmd-run re-resolving the
# project without aliases — *aliased-resolve* guards against that.
ad="$(mktemp -d)"
mkdir -p "$ad/src/app" "$ad/dev-src/app"
printf '{:paths ["src"] :aliases {:dev {:extra-paths ["dev-src"]}}}\n' > "$ad/deps.edn"
printf '(ns app.core)\n' > "$ad/src/app/core.clj"
printf '(ns app.devtool)\n(defn -main [& _] (println "adev-ok"))\n' > "$ad/dev-src/app/devtool.clj"
# run -m via -A:dev
ad_out="$(JOLT_PWD="$ad" $jolt -A:dev run -m app.devtool 2>/dev/null | tail -1)"
if [ "$ad_out" = "adev-ok" ]; then
  pass=$((pass + 1))
else
  echo "  FAIL: -A:dev run -m app.devtool — got \`$ad_out\`, want \`adev-ok\`"
  fails=$((fails + 1))
fi
# file dispatch via -A:dev — loading a file runs its top-level forms only (JVM
# semantics), so the script prints at load; it requires the alias-added ns to
# prove the alias roots survived into the load.
printf '(require (quote app.devtool))\n(app.devtool/-main)\n' > "$ad/run.clj"
ad_out2="$(JOLT_PWD="$ad" $jolt -A:dev "$ad/run.clj" 2>/dev/null | tail -1)"
if [ "$ad_out2" = "adev-ok" ]; then
  pass=$((pass + 1))
else
  echo "  FAIL: -A:dev <file> — got \`$ad_out2\`, want \`adev-ok\`"
  fails=$((fails + 1))
fi
rm -rf "$ad"

# script-mode boot: bin/jolt (chez --script over the seed source) must still
# work even when the rest of the smoke runs against a prebuilt JOLT_BIN.
if [ "$(bin/jolt -e '(+ 20 22)' 2>/dev/null | tail -1)" = "42" ]; then
  pass=$((pass + 1))
else
  echo "  FAIL: script-mode bin/jolt boot"
  fails=$((fails + 1))
fi

# bin/jolt changes to the checkout before loading the CLI. A relative script
# still names a file in the directory from which the user invoked it, carried
# across that chdir as JOLT_PWD.
relative_script_dir="$(mktemp -d)"
printf '(println "relative-script-ok")\n' > "$relative_script_dir/probe.clj"
relative_script_out="$(cd "$relative_script_dir" && "$root/bin/jolt" probe.clj 2>/dev/null | tail -1)"
if [ "$relative_script_out" = "relative-script-ok" ]; then
  pass=$((pass + 1))
else
  echo "  FAIL: script-mode bin/jolt relative file — got \`$relative_script_out\`"
  fails=$((fails + 1))
fi
rm -rf "$relative_script_dir"

# bare-directory build: the standalone binary must build an app with NO jolt
# checkout on disk (embedded runtime sources only). The v0.4.0 release smoke
# failed on all three platforms because the flat.ss inliner missed the
# bytevector-embed arm and fell through to a disk read — every other gate runs
# inside the repo, where the disk read accidentally works.
bare="$(mktemp -d)"
mkdir -p "$bare/app/src/app"
printf '{:paths ["src"]}\n' > "$bare/app/deps.edn"
printf '(ns app.core)\n(defn -main [& _] (println "bare:" (+ 40 2)))\n' > "$bare/app/src/app/core.clj"
abs_jolt="$(cd "$(dirname "$jolt_bin")" && pwd)/$(basename "$jolt_bin")"
if ( cd "$bare/app" && "$abs_jolt" build -m app.core -o app >/dev/null 2>&1 )    && [ "$("$bare/app/app" 2>/dev/null | tail -1)" = "bare: 42" ]; then
  pass=$((pass + 1))
else
  echo "  FAIL: bare-directory standalone build (embedded runtime sources)"
  fails=$((fails + 1))
fi
rm -rf "$bare"

# A stale PWD does not decide where relative paths resolve. PWD is a shell
# convention, not the process's working directory: a child started elsewhere (a
# jolt.process :dir, or any parent that chdirs) inherits the parent's PWD, and
# trusting it sent every relative path — the project's own deps.edn included —
# to the parent's directory. This is what broke `make libconformance`, which runs
# each library's suite with :dir set to that library's root.
stale="$(mktemp -d)"
mkdir -p "$stale/proj/src/sp"
printf '{:paths ["src"] :aliases {:run {:main-opts ["-m" "sp.core"]}}}\n' > "$stale/proj/deps.edn"
printf '(ns sp.core)\n(defn -main [& _] (println "cwd-wins:" (System/getProperty "user.dir")))\n' > "$stale/proj/src/sp/core.clj"
abs_jolt2="$(cd "$(dirname "$jolt_bin")" && pwd)/$(basename "$jolt_bin")"
stale_out="$( cd "$stale/proj" && PWD=/definitely/not/here "$abs_jolt2" -M:run 2>&1 )"
if printf '%s' "$stale_out" | grep -q "cwd-wins:.*/proj"; then
  pass=$((pass + 1))
else
  echo "  FAIL: a stale PWD should not override the process's own working directory"
  printf '%s\n' "$stale_out" | sed 's/^/    | /'
  fails=$((fails + 1))
fi
# JOLT_PWD still wins — that is how bin/jolt reports the user's cwd after cd'ing
# to the repo root.
jp_out="$( cd "$stale" && JOLT_PWD="$stale/proj" "$abs_jolt2" -M:run 2>&1 )"
if printf '%s' "$jp_out" | grep -q "cwd-wins:.*/proj"; then
  pass=$((pass + 1))
else
  echo "  FAIL: JOLT_PWD should still name the project directory"
  printf '%s\n' "$jp_out" | sed 's/^/    | /'
  fails=$((fails + 1))
fi
rm -rf "$stale"

# --- randomness is per-process and per-thread -------------------------------
# Clojure on the JVM seeds Math/random from the clock, so every process draws a
# different stream. jolt must match: a fixed seed makes rand-int, random-uuid
# and Math/random agree across unrelated processes.
check_varies '(rand-int 1000000000)'
check_varies '(rand)'
check_varies '(Math/random)'
check_varies '(str (random-uuid))'
check_varies '(str (java.util.UUID/randomUUID))'
# Chez keeps the PRNG state per thread, and a forked thread started from the same
# default seed — so two threads in ONE process drew identical "random" values.
check '(let [t (atom nil) th (Thread. (fn [] (reset! t (rand-int 1000000000))))] (.start th) (.join th) (= @t (rand-int 1000000000)))' 'false'
check '(let [n 8 out (atom []) ths (doall (map (fn [_] (Thread. (fn [] (swap! out conj (str (random-uuid)))))) (range n)))] (doseq [t ths] (.start t)) (doseq [t ths] (.join t)) (count (distinct @out)))' '8'
# (java.util.Random.) with no seed fed a time OBJECT to truncate and always threw.
check '(int? (.nextInt (java.util.Random.) 1000))' 'true'
check_varies '(.nextInt (java.util.Random.) 1000000000)'
# An explicit seed must still reproduce the JVM's exact LCG stream (values taken
# from a real JVM, not derived from jolt's own implementation).
check '(let [r (java.util.Random. 42)] [(.nextInt r 100) (.nextInt r 100) (.nextInt r 100)])' '[30 63 48]'
check '(let [r (java.util.Random. 42)] [(.nextInt r 64) (.nextInt r 64)])' '[46 3]'
check '(.nextLong (java.util.Random. 12345))' '6674089274190705457'
# random-uuid draws from the OS CSPRNG, not the seeded PRNG. The fallback is
# deliberately loud, so its absence is the assertion that entropy was reached —
# a silent degradation here ships guessable session ids.
check_no '(dotimes [_ 100] (random-uuid))' 'no OS entropy'
check '(let [u (str (random-uuid))] [(count u) (nth u 14) (contains? #{\8 \9 \a \b} (nth u 19))])' '[36 \4 true]'

# The nREPL listen socket must be close-on-exec. Without it every subprocess
# spawned from a process running an nREPL inherits a duplicate, and the port
# stays bound for as long as any of them lives — so killing the server leaves the
# next start unable to bind, against a server that is already gone. Nothing else
# checked it, and the failure surfaces much later as a port that outlived its
# server, which nobody attributes to the right change.
#
# Asks the descriptor itself — fcntl(F_GETFD) & FD_CLOEXEC — on an ephemeral
# port, so no subprocess and no fixed port number are involved. It asserts the
# PROPERTY, not one mechanism: macOS reaches it through the fcntl in
# close-on-exec! (verified: removing that call turns this red here), Linux
# through SOCK_CLOEXEC on socket() as well, so each platform checks the path it
# actually relies on. POSIX only — Windows has no FD_CLOEXEC (it controls
# inheritance with HANDLE_FLAG_INHERIT), so there this asserts nothing rather
# than asserting the wrong thing.
check '(do (require (quote jolt.nrepl)) (if jolt.nrepl/windows? :close-on-exec (let [fd (jolt.nrepl/listen-socket 0) flags (jolt.nrepl/c-fcntl fd 1 0)] (jolt.nrepl/c-close fd) (if (pos? (bit-and flags 1)) :close-on-exec :inheritable))))' ':close-on-exec'

# jolt.ffi/load-library's per-OS map form — documented since the FFI docs
# existed, implemented only in 0.7.10 (it rendered the map to a string and
# dlopen'd garbage). The spec map names a library present on both gate
# platforms (libsqlite3 ships with macOS and the Ubuntu runners); a map with
# no entry for the running platform must raise naming the missing key, not
# silently load nothing.
check '(do (require (quote jolt.ffi)) (jolt.ffi/load-library {:darwin "libsqlite3.0.dylib" :linux "libsqlite3.so.0" :windows "winsqlite3.dll"}) :map-form-ok)' ':map-form-ok'
check '(do (require (quote jolt.ffi)) (try (jolt.ffi/load-library {:no-such-os "x.so"}) :no-raise (catch Exception e (if (clojure.string/includes? (ex-message e) "entry in the per-OS spec") :named-raise :wrong-message))))' ':named-raise'

# clojure.main wraps repl / -e / -m in with-bindings, so a top-level
# (set! *warn-on-reflection* true) — the standard idiom in ported libraries —
# has a thread-local slot to write. jolt's REPL bound them and -e did not, so
# the same expression worked from a file and raised "Can't change/establish
# root binding" here. Each of the three flags, and the write must be visible to
# a later form in the same -e.
check '(do (set! *warn-on-reflection* true) *warn-on-reflection*)' 'true'
check '(do (set! *unchecked-math* true) *unchecked-math*)' 'true'
check '(do (set! *assert* false) *assert*)' 'false'
# ...and load-string scopes its own set! the way a file load does. This listed
# two of the three vars by hand, so *unchecked-math* escaped into the caller.
check '(do (load-string "(set! *unchecked-math* true) :x") *unchecked-math*)' 'false'
check '(do (load-string "(set! *warn-on-reflection* true) :x") *warn-on-reflection*)' 'false'
check '(do (load-string "(set! *assert* false) :x") *assert*)' 'true'

echo "cli smoke: $pass passed, $fails failed"
[ "$fails" -eq 0 ]
