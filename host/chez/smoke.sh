#!/bin/sh
# CLI smoke: exercise the real jolt process end to end — core eval, runtime
# eval/load-string, runtime defmacro, futures, and the numeric tower. The in-process
# corpus/unit gates cover semantics in depth; this confirms the CLI entry itself.
root="$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)"
cd "$root"

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
# coreutils' timeout is not on a stock macOS, so fall back to running uncapped rather
# than skipping the case or hard-failing the gate on a missing tool.
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
    jolt_timeout=""
    echo "  note: no timeout(1) with --foreground — a hung case will block instead of failing"
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
# root's data_readers.clj is picked up.
dr_out="$(JOLT_PWD="$root/test/chez/datareader-app" $jolt run -m drtest.main 2>/dev/null)"
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
al_out="$(JOLT_PWD="$root/test/chez/alias-leak-app" $jolt run -m fix.main 2>/dev/null | tail -1)"
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

echo "cli smoke: $pass passed, $fails failed"
[ "$fails" -eq 0 ]
