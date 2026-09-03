#!/bin/sh
# Runtime error reporting: a throw that escapes names the Clojure fn it came from,
# with file and line, and java.lang.Throwable's surface is inherited by every
# exception class rather than restated per shim.
#
# Both regressions this gates were reported from the same three-line program:
#
#   (defn -main [& args] (println "Hello") (try (/ 1 0) (catch Exception e (.printStackTrace e))))
#
# 1. THE TRACE WAS EMPTY FOR A TAIL CALL. The reporter walks Chez's live
#    continuation, but TCO erases a tail-called frame from it — and (-main tail-calls
#    a fn that throws) is the ordinary shape, so the whole trace came back empty and
#    the error printed with NO location at all. The tail-frame history that survives
#    TCO existed but was opt-in behind JOLT_TRACE, so nobody saw it. It is on by
#    default on the source-run path now. A NON-tail call kept working throughout,
#    which is why this looked fixed each time it was checked with a nested call.
#    A frame also reported the line its function was DEFINED on rather than the
#    line reached inside it, so even a working trace pointed at a defn dozens of
#    lines above the fault.
#
# 2. .printStackTrace DID NOT EXIST on the value a catch binds. The Throwable
#    methods were duplicated between the raw-condition arm (records.ss) and
#    dot-object-method (dot-forms.ss); printStackTrace was in the first only, so
#    every ex-info and typed host throwable — which is what a catch actually binds —
#    answered "No matching method printStackTrace found for java.lang.ArithmeticException".
#
# Kept out of smoke.sh deliberately: these cases assert on STDERR and on a project
# run (-m), not on `-e` stdout, which is the shape every check there is built around.
root="$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)"
cd "$root"

jolt="${JOLT_BIN:-bin/jolt}"
case "$jolt" in /*) joltabs="$jolt" ;; *) joltabs="$root/$jolt" ;; esac

fails=0
pass=0

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT INT TERM
mkdir -p "$work/src/app"
printf '{}\n' > "$work/deps.edn"

# -main tail-calls boom, and boom's body is itself a tail call into `/`. Both
# frames are gone from the live continuation; only the history has them.
cat > "$work/src/app/tail.clj" <<'EOF'
(ns app.tail)

(defn boom [x]
  (/ x 0))

(defn -main [& _]
  (println "before")
  (boom 1))
EOF

# Same shape, but the callee is ^:redef so it stays var-routed and is never
# spliced. A built binary must still name ITS frame -- that is what keeps the
# baked tail-site instrumentation under test now that an ordinary callee is
# inlined away (see the built-binary block below).
mkdir -p "$work/src/app"
cat > "$work/src/app/tailredef.clj" <<'EOF'
(ns app.tailredef)

(defn ^:redef boom [x]
  (/ x 0))

(defn -main [& _]
  (println "before")
  (boom 1))
EOF

run_app() {   # run_app <ns> [env-assignment]; prints combined output
  ( cd "$work" && env $2 "$joltabs" run -m "$1" 2>&1 )
}

expect_match() {   # expect_match <label> <output> <grep-pattern>
  if printf '%s' "$2" | grep -q "$3"; then
    pass=$((pass + 1))
  else
    echo "  FAIL: $1"
    echo "    no line matching: $3"
    echo "--- got ---"; printf '%s\n' "$2"
    fails=$((fails + 1))
  fi
}

expect_no_match() {
  if printf '%s' "$2" | grep -q "$3"; then
    echo "  FAIL: $1"
    echo "    unexpected line matching: $3"
    echo "--- got ---"; printf '%s\n' "$2"
    fails=$((fails + 1))
  else
    pass=$((pass + 1))
  fi
}

echo "trace smoke: an uncaught tail-call throw names fn, file and exact line"
out="$(run_app app.tail)"
expect_match "uncaught throw reports the message" "$out" 'Unhandled exception (ArithmeticException): Divide by zero'
# The erroring fn is a tail call from a tail call: the whole point of the case.
# EXACT lines, not the line each fn was DEFINED on: boom opens on line 3 and
# divides on line 4; -main opens on line 6 and calls boom on line 8. A frame that
# reported its defn line would match 3 and 6 here, so these two assertions are
# what tell the difference.
expect_match "innermost frame is boom, at the dividing line" "$out" 'app\.tail/boom (.*src/app/tail\.clj:4)'
expect_match "caller frame -main, at the call site" "$out" 'app\.tail/-main (.*src/app/tail\.clj:8)'
expect_no_match "no frame reports a defn line" "$out" 'tail\.clj:[36])'

# 90-deep NON-TAIL recursion must report the TRUE depth and the outermost caller.
# The ring's outer capacity is 64 ribs: if the reporter ever read the spine off
# the ring, this would print ~64 `down` frames (the head wraps, and the wrap
# overwrites -main's own rib) and lose -main entirely — the R3 regression this
# round must not reintroduce. The live continuation holds all 90 frames, and
# -main must survive in it too, so its call into `down` is NON-TAIL (a tail
# call would TCO-erase -main's frame, and the wrapped ring no longer has it).
cat > "$work/src/app/deep.clj" <<'EOF'
(ns app.deep)
(defn down [n]
  (if (zero? n)
    (throw (ex-info "deep" {:n n}))
    (+ 1 (down (dec n)))))
(defn -main [& _]
  (let [r (down 90)]
    (println "sum" r)))
EOF
echo "trace smoke: 90-deep non-tail recursion reports the true depth"
out_deep="$(run_app app.deep)"
expect_match "deep: outermost caller -main present" "$out_deep" 'app\.deep/-main (.*src/app/deep\.clj:7)'
expect_match "deep: all 90 down frames reported (true depth)" "$out_deep" 'app\.deep/down.*(x90)'

# A call that RETURNED must leave nothing behind. The ring's outer head only
# advances on entry, so without the save/restore the emitter pairs around every
# non-tail call, the ribs of a completed (noisy) call were still there at the next
# throw and printed UNDER the frames that actually threw — and pushed the real
# caller out of the ring entirely. `noise` here recurses 40 deep and returns long
# before `boom` divides, so any noise/* frame in this trace is stale history.
cat > "$work/src/app/stale.clj" <<'EOF'
(ns app.stale)
(defn noise [n] (if (zero? n) 0 (+ 1 (noise (dec n)))))
(defn boom [x] (/ x 0))
(defn caller [x] (+ 1 (boom x)))
(defn -main [& _]
  (println "noise" (noise 40))
  (println (caller 1)))
EOF
echo "trace smoke: a returned call leaves no frames behind"
out_stale="$(run_app app.stale)"
expect_match "throwing frame is first" "$out_stale" 'app\.stale/boom (.*src/app/stale\.clj:3)'
expect_match "its caller is next" "$out_stale" 'app\.stale/caller (.*src/app/stale\.clj:4)'
expect_match "and -main survives the noisy call" "$out_stale" 'app\.stale/-main (.*src/app/stale\.clj:7)'
expect_no_match "no frames from the call that already returned" "$out_stale" 'app\.stale/noise'

# R1 (bead jolt-pfhc): the ring's rib-advance-on-entry invariant kept a returned
# call's rib behind the outer head, so NO fixture could gate a tail-recursive call
# that RETURNS before a later throw — app.stale's recursion is non-tail, it never
# enters a rib at all. Continuation marks die with their frame, so staleness is
# structural now: ping/pong mutually tail-recurse to completion (NOT self-calls —
# those are elided), then -main calls a thrower. The trace must NOT resurrect
# ping/pong and MUST name the thrower.
cat > "$work/src/app/tailstale.clj" <<'EOF'
(ns app.tailstale)
(declare pong)
(defn ping [n]
  (if (zero? n) :done (pong (dec n))))
(defn pong [n]
  (if (zero? n) :done (ping (dec n))))
(defn thrower [] (throw (ex-info "stalestale" {:a 1})))
(defn -main [& _]
  (println "ping" (ping 5))
  (thrower))
EOF
echo "trace smoke: a returned mutual-tail recursion leaves no frames behind"
out_ts="$(run_app app.tailstale)"
expect_match "mutual-tail: the thrower is named" "$out_ts" 'app\.tailstale/thrower'
expect_no_match "mutual-tail: ping left the trace" "$out_ts" 'app\.tailstale/ping'
expect_no_match "mutual-tail: pong left the trace" "$out_ts" 'app\.tailstale/pong'
# the positive dual: a NON-TAIL entry into a mutual tail chain that throws mid-
# chain — the hops between the entry and the throw must appear, with their lines.
cat > "$work/src/app/tailchain.clj" <<'EOF'
(ns app.tailchain)
(declare q)
(defn p [n]
  (if (zero? n) (throw (ex-info "chain" {:n n})) (q (dec n))))
(defn q [n]
  (if (zero? n) (throw (ex-info "chain" {:n n})) (p (dec n))))
(defn -main [& _]
  (println "chain" (p 3)))
EOF
echo "trace smoke: a live mutual tail chain reports its hops"
out_tc="$(run_app app.tailchain)"
expect_match "mutual-chain: p named" "$out_tc" 'app\.tailchain/p'
expect_match "mutual-chain: p at its own call line" "$out_tc" 'app\.tailchain/p (.*src/app/tailchain\.clj:4)'
expect_match "mutual-chain: q named" "$out_tc" 'app\.tailchain/q'
expect_match "mutual-chain: q at its own call line" "$out_tc" 'app\.tailchain/q (.*src/app/tailchain\.clj:6)'
expect_no_match "mutual-chain: no defn lines" "$out_tc" 'tailchain\.clj:[35])'

# R2 (bead jolt-knn8): only tail sites write the site vreg, so a pair can
# outlive its chain: g ends in a native-op tail call, returns normally, then a
# DIFFERENT fn throws from a non-tail position. Nothing refreshes the vreg in
# between, so the raise-time stash is g's stale pair — the callsite-table
# validator must reject it (the innermost context's static callee is h, not g).
# Without the validator this trace grows a phantom "g" frame.
cat > "$work/src/app/sitestale.clj" <<'EOF'
(ns app.sitestale)
(defn g [x]
  (+ x 1))
(defn h []
  (let [x (throw (ex-info "late" {:x 1}))]
    x))
(defn -main [& _]
  (println "g" (g 41))
  (h))
EOF
echo "trace smoke: a returned native tail site cannot haunt a later throw"
out_ss="$(run_app app.sitestale)"
expect_match "sitestale: the thrower is named at its line" "$out_ss" 'app\.sitestale/h (.*src/app/sitestale\.clj:5)'
expect_no_match "sitestale: the returned fn's site is rejected" "$out_ss" 'app\.sitestale/g'

# A top-level form is a root. The site vreg holds the LAST tail site executed,
# and only tail sites write it — so when a loaded file throws at its top level,
# which nothing tail-called, the raise-time stash is whatever the previous call
# left behind: helper's `str` tail here, long returned. The validator cannot
# reject it either, because the innermost live frame is the loader itself, a
# host fn that registers no callees. Each form-at-a-time entry — the loader,
# -e, load-string, the REPL — clears the slot when a form starts (jolt-site-reset!).
cat > "$work/src/app/topres.clj" <<'EOF'
(ns app.topres)
(defn helper [] (str "a" "b"))
(defn -main [& _]
  (helper)
  (require 'app.topboom)
  (println "unreachable"))
EOF
cat > "$work/src/app/topboom.clj" <<'EOF'
(ns app.topboom)
(throw (ex-info "top" {}))
EOF
echo "trace smoke: a returned tail site cannot haunt a throw at a loaded file's top level"
out_tr="$(run_app app.topres)"
expect_match "topres: the location is the throwing form" "$out_tr" 'at .*src/app/topboom\.clj:2:1'
expect_match "topres: the loading caller is named at its require line" "$out_tr" 'app\.topres/-main (.*src/app/topres\.clj:5)'
expect_no_match "topres: the returned helper is not resurrected" "$out_tr" 'app\.topres/helper'
# The form's COMPILE is a root too: macroexpansion runs user code, and mk's
# `apply` tail is what the slot held when the expanded throw ran — cleared again
# between the expansion and the run.
cat > "$work/src/app/macboom.clj" <<'EOF'
(ns app.macboom)
(defn mk [] (apply list ['throw (list 'ex-info "mac" {})]))
(defmacro m [] (mk))
(m)
EOF
cat > "$work/src/app/macmain.clj" <<'EOF'
(ns app.macmain)
(defn -main [& _]
  (require 'app.macboom)
  (println "unreachable"))
EOF
echo "trace smoke: macroexpansion's tail sites cannot haunt the expanded form's run"
out_mr="$(run_app app.macmain)"
expect_match "macres: the loading caller is named" "$out_mr" 'app\.macmain/-main (.*src/app/macmain\.clj:3)'
expect_no_match "macres: the expander's helper is not resurrected" "$out_mr" 'app\.macboom/mk'
# The same rule at the other form-at-a-time entries.
run_e() { ( cd "$work" && "$joltabs" -e "$1" 2>&1 ); }
out_e="$(run_e '(defn h [] (str 1)) (h) (throw (ex-info "x" {}))')"
expect_match "-e: the throw is reported" "$out_e" 'Unhandled exception: x'
expect_no_match "-e: a returned fn is not resurrected" "$out_e" '^ *[a-z.]*/h$'
out_ls="$(run_e '(load-string "(defn h2 [] (str 1)) (h2) (throw (ex-info \"y\" {}))")')"
expect_match "load-string: the throw is reported" "$out_ls" 'Unhandled exception: y'
expect_no_match "load-string: a returned fn is not resurrected" "$out_ls" '^ *[a-z.]*/h2$'
out_repl="$( ( cd "$work" && printf '(defn h3 [] (str 1))\n(h3)\n(throw (ex-info "z" {}))\n' | "$joltabs" repl 2>&1 ) )"
expect_match "repl: the throw is reported" "$out_repl" 'error: z'
expect_no_match "repl: a returned fn is not resurrected" "$out_repl" '^ *[a-z.]*/h3$'
# and the positive dual: a throw INSIDE an evaluated fn still names it
out_repl2="$( ( cd "$work" && printf '(defn t [] (throw (ex-info "q" {})))\n(t)\n' | "$joltabs" repl 2>&1 ) )"
expect_match "repl: a throw inside an evaluated fn names it" "$out_repl2" '^ *[a-z.]*/t$'

# R2: a HOST fault (no jolt-throw anywhere — a bad primitive-array index) is
# captured by the cli's with-exception-handler while the frames are live, so
# the trace still names the fn and its exact line.
cat > "$work/src/app/hostfault.clj" <<'EOF'
(ns app.hostfault)
(defn kaboom [n]
  (aget (double-array 3) n))
(defn -main [& _]
  (println "before")
  (println (kaboom 10)))
EOF
echo "trace smoke: a host fault still maps fn and line"
out_hf="$(run_app app.hostfault)"
expect_match "hostfault: the faulting fn is named at its line" "$out_hf" 'app\.hostfault/kaboom (.*src/app/hostfault\.clj:3)'
expect_match "hostfault: the caller is present" "$out_hf" 'app\.hostfault/-main'

# R3 (bead jolt-230w): a chain at EVERY spine level, not just the innermost.
# dive tail-calls mid (rib on -main's callee frame), mid calls hop non-tail
# (new frame), hop tail-calls thrower (inner rib), thrower throws tail (site
# pair). Before R3 the outer rib was invisible: dive was simply missing.
cat > "$work/src/app/twolevel.clj" <<'EOF'
(ns app.twolevel)
(defn thrower [n] (throw (ex-info "deep" {:n n})))
(defn hop [n] (thrower n))
(defn mid [n] (+ 1 (hop n)))
(defn dive [n] (mid n))
(defn -main [& _] (println (dive 3)))
EOF
echo "trace smoke: chains recover at every spine level"
out_tl="$(run_app app.twolevel)"
expect_match "twolevel: thrower at its line" "$out_tl" 'app\.twolevel/thrower (.*src/app/twolevel\.clj:2)'
expect_match "twolevel: hop at its tail-call line" "$out_tl" 'app\.twolevel/hop (.*src/app/twolevel\.clj:3)'
expect_match "twolevel: mid at its call line" "$out_tl" 'app\.twolevel/mid (.*src/app/twolevel\.clj:4)'
expect_match "twolevel: dive recovered from the OUTER rib" "$out_tl" 'app\.twolevel/dive (.*src/app/twolevel\.clj:5)'
expect_match "twolevel: -main at its call line" "$out_tl" 'app\.twolevel/-main (.*src/app/twolevel\.clj:6)'
flat_tl="$(printf '%s' "$out_tl" | tr '\n' ' ')"
expect_match "twolevel: frames in stack order" "$flat_tl" 'thrower .*twolevel/hop .*twolevel/mid .*twolevel/dive .*twolevel/-main'

echo "trace smoke: JOLT_TRACE=0 opts out"
out_off="$(run_app app.tail JOLT_TRACE=0)"
expect_match "still reports the message" "$out_off" 'Unhandled exception (ArithmeticException): Divide by zero'
expect_no_match "no history frames when opted out" "$out_off" 'app\.tail/boom'

# Whether tracing is on changes the code the emitter produces — the entry prologue
# and the per-call save/restore are baked in at compile time. So a cached fasl is
# only valid for the trace mode it was compiled under, and the cache generation has
# to say which. It did not, so the two modes shared a generation and each loaded the
# other's artifacts: a JOLT_TRACE=0 run first left untraced fasls that a later traced
# run happily reused, silently reporting NO history frames — the feature turned off
# by a cache hit. The reverse direction cost speed instead of frames (a traced fasl
# reused under JOLT_TRACE=0), which is how this surfaced.
echo "trace smoke: the AOT cache does not share artifacts across trace modes"
cache="$work/tracecache"
rm -rf "$cache"
# cold, tracing OFF: compiles and caches untraced fasls
run_app app.tail "JOLT_CACHE_DIR=$cache JOLT_TRACE=0" > /dev/null
# warm, tracing ON, same cache: must NOT serve the untraced artifacts
out_flip="$(run_app app.tail "JOLT_CACHE_DIR=$cache")"
expect_match "tracing on after an untraced run still names the throwing fn" "$out_flip" 'app\.tail/boom (.*src/app/tail\.clj:4)'
expect_match "...and its caller" "$out_flip" 'app\.tail/-main (.*src/app/tail\.clj:8)'
# and the other direction: a traced generation must not be served to an opted-out run
cache2="$work/tracecache2"
rm -rf "$cache2"
run_app app.tail "JOLT_CACHE_DIR=$cache2" > /dev/null
out_flip2="$(run_app app.tail "JOLT_CACHE_DIR=$cache2 JOLT_TRACE=0")"
expect_no_match "JOLT_TRACE=0 after a traced run has no history frames" "$out_flip2" 'app\.tail/boom'

# .printStackTrace over each shape a catch can bind: a host-raised arithmetic
# error, an ex-info, and a constructed exception. Each must print "class: message"
# and then the frames — the same rendering the uncaught reporter uses.
cat > "$work/src/app/pst.clj" <<'EOF'
(ns app.pst)

(defn inner [x]
  (/ x 0))

(defn outer [x]
  (inner x))

(defn -main [& _]
  (try (outer 5) (catch Exception e (.printStackTrace e)))
  (try (throw (ex-info "boom" {:a 1})) (catch Exception e (.printStackTrace e)))
  (try (throw (Exception. "plain")) (catch Exception e (.printStackTrace e)))
  ;; the 1-arg overload writes to a PrintWriter/StringWriter instead of stderr
  (let [w (java.io.StringWriter.)]
    (try (outer 5) (catch Exception e (.printStackTrace e w)))
    (when (re-find #"app\.pst/inner" (str w)) (println "WRITER-OK")))
  ;; the rest of the Throwable surface, inherited rather than per-class
  (try (/ 1 0)
       (catch Exception e
         (println "SURFACE"
                  (.getLocalizedMessage e)
                  (count (.getSuppressed e))
                  (identical? e (.fillInStackTrace e)))))
  (println "DONE"))
EOF

# A fault the host itself raised — string-append handed nil — is a typed
# throwable by the time a catch binds it (java/host-faults.ss): printStackTrace
# prints its class and message, and the frames that led to it, off the
# continuation Chez attached to the raw condition. Uncaught, the report names
# the class the same way.
cat > "$work/src/app/fault.clj" <<'EOF'
(ns app.fault)

(defn faulty [s]
  (.concat s nil))

(defn fouter [s]
  (faulty s))

(defn -main [& args]
  (let [w (java.io.StringWriter.)]
    (try (fouter "a") (catch Exception e (.printStackTrace e w)))
    (when (re-find #"^java\.lang\.NullPointerException: string-append: nil is not a string" (str w))
      (println "FAULT-HEADER-OK"))
    (when (re-find #"app\.fault/faulty" (str w))
      (println "FAULT-FRAME-OK")))
  (when (seq args) (fouter "b"))
  (println "FAULT-DONE"))
EOF
echo "trace smoke: a host fault a catch binds is a typed throwable with a trace"
fault="$(run_app app.fault)"
expect_match "host fault: printStackTrace names class and message" "$fault" 'FAULT-HEADER-OK'
expect_match "host fault: printStackTrace reaches the faulting fn" "$fault" 'FAULT-FRAME-OK'
expect_match "host fault: -main completes" "$fault" 'FAULT-DONE'
fault_u="$( cd "$work" && "$joltabs" -m app.fault again 2>&1 )"
expect_match "host fault uncaught: the report names the class" "$fault_u" 'Unhandled exception (NullPointerException): string-append: nil is not a string'
expect_match "host fault uncaught: the trace reaches the faulting fn" "$fault_u" 'app\.fault/faulty'

echo "trace smoke: .printStackTrace works on every throwable a catch binds"
pst="$(run_app app.pst)"
expect_match "arithmetic error prints class and message" "$pst" 'java\.lang\.ArithmeticException: Divide by zero'
# inner divides on 4 (defn on 3), outer calls inner on 7 (defn on 6), -main calls
# outer on 10 — and a catch clause must report the THROWING line, not the line the
# handler itself is on, which is what the snapshot at catch entry is for.
expect_match "printStackTrace: innermost frame at the dividing line" "$pst" 'app\.pst/inner (.*src/app/pst\.clj:4)'
expect_match "printStackTrace: caller frame at its call site" "$pst" 'app\.pst/outer (.*src/app/pst\.clj:7)'
expect_match "printStackTrace: outermost frame at its call site" "$pst" 'app\.pst/-main (.*src/app/pst\.clj:10)'
expect_match "ex-info prints class and message" "$pst" 'clojure\.lang\.ExceptionInfo: boom'
expect_match "constructed exception prints class and message" "$pst" 'java\.lang\.Exception: plain'
expect_match "1-arg overload writes to the given writer" "$pst" 'WRITER-OK'
expect_match "getLocalizedMessage/getSuppressed/fillInStackTrace" "$pst" 'SURFACE Divide by zero 0 true'
expect_match "execution continued past every catch" "$pst" 'DONE'

# Built binaries trace by default (0.6.2): jolt build bakes the tail-site
# instrumentation in, so a deployed binary's uncaught error still names the
# TCO-erased frame with its exact line — no marker files needed, the site
# literals carry the lines. JOLT_TRACE=0 at BUILD time opts the binary out.
#
# A built binary also INLINES (jolt-mbcm.6: splicing follows direct-linking, which
# every non-dev build sets), and a spliced callee has no procedure left to name a
# frame after. The trace reads the same anyway: the splicer stamps each copied
# node with the chain of fns it came through, the emitter folds that into the
# marker and the tail-site pair, and the reporter expands one physical frame back
# into the logical ones (jolt-mbcm.7). These rows are that parity, so they assert
# the SAME two frames the `jolt run` rows above do -- if inline attribution
# regresses, the built binary reports app.tail/-main at line 4 and both fail.
echo "trace smoke: a built binary traces by default"
( cd "$work" && "$joltabs" build -m app.tail -o tailbin >/dev/null 2>&1 )
out_bt="$("$work/tailbin" 2>&1)"
expect_match "built: the spliced callee is named at its own line" "$out_bt" 'app\.tail/boom (.*src/app/tail\.clj:4)'
expect_match "built: and its caller at the call site" "$out_bt" 'app\.tail/-main (.*src/app/tail\.clj:8)'

# The same shape where the callee CANNOT be spliced (^:redef stays var-routed):
# a real frame, not a reconstructed one. Without this row the block above would
# pass on a build that had stopped inlining altogether.
( cd "$work" && "$joltabs" build -m app.tailredef -o tailredefbin >/dev/null 2>&1 )
out_rd="$("$work/tailredefbin" 2>&1)"
expect_match "built: a ^:redef callee keeps its own frame at its line" "$out_rd" 'app\.tailredef/boom (.*src/app/tailredef\.clj:4)'
expect_match "built: and its caller at the call site" "$out_rd" 'app\.tailredef/-main (.*src/app/tailredef\.clj:8)'
echo "trace smoke: JOLT_TRACE=0 at build time opts the binary out"
( cd "$work" && env JOLT_TRACE=0 "$joltabs" build -m app.tail -o tailbin0 >/dev/null 2>&1 )
out_bt0="$("$work/tailbin0" 2>&1)"
expect_match "untraced build: still reports the message" "$out_bt0" 'Unhandled exception (ArithmeticException): Divide by zero'
expect_no_match "untraced build: no baked trace frames" "$out_bt0" 'app\.tail/boom (.*:4)'

if [ "$fails" -gt 0 ]; then
  echo "trace smoke: $fails failed, $pass passed"
  exit 1
fi
echo "trace smoke: $pass passed (exact per-frame lines, JOLT_TRACE=0 opt-out, Throwable surface)"
