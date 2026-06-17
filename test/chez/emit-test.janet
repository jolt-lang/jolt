# Phase 1 (jolt-cf1q.2) — REAL pipeline end to end: actual Clojure source ->
# Janet-hosted analyzer -> host-neutral IR -> Scheme emitter -> run on Chez.
# Correctness is checked by parity against the SAME program evaluated by the
# Janet host (jolt's own oracle), so a divergence is the back end's, not the
# program's.
#   janet test/chez/emit-test.janet      (from repo root)
(import ../../src/jolt/api :as api)
(import ../../src/jolt/backend :as backend)
(import ../../src/jolt/reader :as r)
(import ../../host/chez/driver :as d)
(import ../../host/chez/emit :as emit)

(unless (d/chez-available?)
  (print "skip: chez not on PATH")
  (os/exit 0))

(var total 0) (var fails 0)
(defn ok [name pred &opt extra]
  (++ total)
  (if pred (printf "ok: %s" name)
    (do (++ fails) (printf "FAIL: %s   %s" name (or extra "")))))

# Janet-host oracle: evaluate the same program, stringify its value the way jolt
# prints it at the CLI (so "832040" not "832040.0", "0.5" not 1/2, etc.).
(def oracle-ctx (api/init {:compile? true}))
(defn oracle [src] (string (api/load-string oracle-ctx src)))

# Canonical CLI oracle (the run-corpus gate's boundary): collection values don't
# round-trip through (string value) — they need jolt's real `-e` printer. Take
# the last non-empty stdout line, exactly like run-corpus.janet.
(defn cli-oracle [src]
  (def proc (os/spawn ["build/jolt" "-e" src] :p {:out :pipe :err :pipe}))
  (def out (ev/read (proc :out) 0x100000))
  (ev/read (proc :err) 0x100000)
  (os/proc-wait proc)
  (def lines (filter (fn [l] (not (empty? l))) (string/split "\n" (string/trim (if out (string out) "")))))
  (if (empty? lines) "" (last lines)))

(def ctx (d/make-ctx))

# 1) constant-folded arithmetic: (+ 1 2) -> the analyzer folds to const 3.
(let [[code out err] (d/run-on-chez ctx "(+ 1 2)")]
  (ok "(+ 1 2) = 3" (and (= code 0) (= out "3") (= out (oracle "(+ 1 2)"))) (string out " | " err)))

# 2) fib: var-cell def + named-fn self-recursion + native arith, via real IR.
(let [src "(defn fib [n] (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))) (fib 30)"
      [code out err] (d/run-on-chez ctx src)]
  (ok "(fib 30) = 832040" (and (= code 0) (= out "832040") (= out (oracle src))) (string out " | " err)))

# 3) mandelbrot kernel: loop/recur, let, or-expansion, cross-var call
#    (run -> count-point), flonum compute. Parity vs the Janet host on run(40).
(def mandel-defs ``
(defn count-point [cr ci cap]
  (loop [i 0 zr 0.0 zi 0.0]
    (if (or (>= i cap) (> (+ (* zr zr) (* zi zi)) 4.0))
      i
      (recur (inc i)
             (+ (- (* zr zr) (* zi zi)) cr)
             (+ (* 2.0 (* zr zi)) ci)))))
(defn run [n]
  (let [cap 200
        nd (* 1.0 n)]
    (loop [y 0 acc 0]
      (if (< y n)
        (let [ci (- (/ (* 2.0 y) nd) 1.0)
              row (loop [x 0 a 0]
                    (if (< x n)
                      (let [cr (- (/ (* 2.0 x) nd) 1.5)]
                        (recur (inc x) (+ a (count-point cr ci cap))))
                      a))]
          (recur (inc y) (+ acc row)))
        acc))))
``)
(let [src (string mandel-defs "\n(run 40)")
      [code out err] (d/run-on-chez ctx src)]
  (ok "mandelbrot run(40) parity" (and (= code 0) (= out (oracle src)))
      (string "chez=" out " janet=" (oracle src) " | " err)))

# 3b) regressions found via the corpus probe:
#   - loop binds SEQUENTIALLY (Scheme named-let is parallel); b must see a.
#   - #(...) shorthand gensyms params with a trailing `#` (invalid in Scheme).
(each [label src] [["loop sequential init" "(loop [a 1 b (+ a 10)] (+ a b))"]
                   ["#() shorthand" "(#(+ %1 %2) 1 2)"]]
  (let [[code out err] (d/run-on-chez ctx src)]
    (ok label (and (= code 0) (= out (oracle src))) (string "chez=" out " janet=" (oracle src) " | " err))))

# 3c) persistent collections (jolt-wgbz): vector/map/set literals + leaf ops.
#   Maps/sets print in jolt's INTERNAL hash order, which a Scheme HAMT won't
#   reproduce — so unordered cases are checked via `(= ...)` (prints true/false,
#   exactly how the run-corpus gate compares them), and only ORDERED vectors are
#   compared by printed form. Parity is still vs the Janet oracle in both shapes.
(each src [# ordered: direct printed-form parity
           "[1 2 3]"
           "(conj [1 2] 3)"
           "(count [1 2 3])"
           "(nth [10 20 30] 1)"
           "(get [10 20 30] 0)"
           "(peek [1 2 3])"
           "(pop [1 2 3])"
           # unordered / boolean: equality-wrapped, order-independent
           "(= {:a 1 :b 2} {:b 2 :a 1})"
           "(= {:a 1 :b 2} (assoc {:a 1} :b 2))"
           "(= 1 (get {:a 1} :a))"
           "(= 2 (count {:a 1 :b 2}))"
           "(= 99 (get {:a 1} :z 99))"
           "(= {:a 1} (dissoc {:a 1 :b 2} :b))"
           "(= #{1 2 3} (conj #{1 2} 3))"
           "(= #{1 2} (conj #{1 2} 2))"
           "(contains? #{1 2} 1)"
           "(contains? #{1 2} 9)"
           "(contains? {:a 1} :a)"
           "(empty? [])"
           "(empty? [1])"
           "(empty? {})"
           "(= [1 2] [1 2])"
           "(= [1 2] [1 3])"
           "(= #{1 2} #{2 1})"
           "(= {1 2} {1 3})"]
  (let [[code out err] (d/run-on-chez ctx src)
        want (cli-oracle src)]
    (ok (string "coll: " src) (and (= code 0) (= out want))
        (string "chez=" out " janet=" want " | " err))))

# 3d) dynamic IFn dispatch (inc 3b): a keyword/vector/coll held in a LOCAL (let
#   binding or fn param) and called as a fn. The 3 ex-known-divergences. The
#   callee is a :local that's NOT the fn's self-name, so emit routes it through
#   the jolt-invoke fallback (procedure? -> apply; keyword/coll -> lookup).
(each [src want] [["(let [v [10 20 30]] (v 1))" "20"]
                  ["(let [k :a] (k {:a 7}))" "7"]
                  ["((fn [f] (f {:a 1})) :a)" "1"]]
  (let [[code out err] (d/run-on-chez ctx src)]
    (ok (string "ifn: " src) (and (= code 0) (= out want))
        (string "chez=" out " want=" want " | " err))))

# 3e) seq tier (inc 3b): jolt list type, first/rest/next/seq/cons/list, lazy-seq
#   (range/take over an infinite seq), map/filter/reduce/into/remove, keys/vals.
#   Lists and lazy seqs print as (...) and are sequential-= to vectors. Ordered
#   shapes -> printed-form parity vs the CLI oracle.
(each src ["(first [1 2 3])"
           "(rest [1 2 3])"
           "(rest [1])"
           "(rest [])"
           "(next [1 2 3])"
           "(next [1])"
           "(cons 0 [1 2 3])"
           "(cons 1 nil)"
           "(list 1 2 3)"
           "(list)"
           "(seq [])"
           "(conj (list 2 3) 1)"
           "(conj nil 1 2)"
           "(map inc [1 2 3])"
           "(map + [1 2 3] [10 20 30])"
           "(map :a [{:a 1} {:a 2}])"
           "(filter even? [1 2 3 4])"
           "(remove even? [1 2 3 4])"
           "(reduce + 0 [1 2 3])"
           "(reduce + [1 2 3])"
           "(reduce + (map inc (range 4)))"
           "(into [] [1 2 3])"
           "(into [1] (list 2 3))"
           "(take 3 (range))"
           "(reverse [1 2 3])"
           "(apply + [1 2 3])"
           "(count (map inc [1 2 3]))"]
  (let [[code out err] (d/run-on-chez ctx src)
        want (cli-oracle src)]
    (ok (string "seq: " src) (and (= code 0) (= out want))
        (string "chez=" out " janet=" want " | " err))))

# 3f) seq tier — unordered / cross-type, equality-wrapped (prints true/false):
#   keys/vals order is HAMT order, into-map / into-set unordered; sequential =
#   across vector and list.
(each src ["(= 2 (count (keys {:a 1 :b 2})))"
           "(= 3 (reduce + (vals {:a 1 :b 2})))"
           "(= {:a 1 :b 2} (into {} [[:a 1] [:b 2]]))"
           "(= #{1 2 3} (into #{} [1 2 3]))"
           "(= [1 2 3] (list 1 2 3))"
           "(= [1 2 3] (map inc [0 1 2]))"
           # jolt returns a vector for (seq vec) / bounded (range); Chez returns a
           # Clojure-canonical lazy seq. Values are sequential-=, printed forms differ.
           "(= [1 2 3] (seq [1 2 3]))"
           "(= [0 1 2 3 4] (range 5))"]
  (let [[code out err] (d/run-on-chez ctx src)]
    (ok (string "seq=: " src) (and (= code 0) (= out "true"))
        (string "chez=" out " | " err))))

# 3g) multi-arity + variadic fns (inc 3c): case-lambda dispatch, a Scheme rest
#   arg collected into a jolt seq (nil when empty), recur within an arity and a
#   self-call across arities. Value parity vs the CLI oracle.
(each src ["((fn ([x] (* x 2)) ([x y] (+ x y))) 5)"
           "((fn ([x] (* x 2)) ([x y] (+ x y))) 3 4)"
           "(defn g ([x] x) ([x y] (+ x y))) (g 10)"
           "(defn g ([x] x) ([x y] (+ x y))) (g 10 20)"
           "(defn h [a & more] (count more)) (h 1 2 3 4)"
           # empty rest is nil (Clojure): count 0, first nil (prints "")
           "(defn h [a & more] (count more)) (h 1)"
           "(defn h [a & more] (first more)) (h 1)"
           "(defn h [a & more] (first more)) (h 1 2 3)"
           "(defn h [a & more] (reduce + a more)) (h 1 2 3 4)"
           "(defn h [a & more] (reduce + a more)) (apply h [1 2 3 4])"
           # self-call from one arity to another, then recur within it
           "(defn f ([n] (f n 0)) ([n acc] (if (zero? n) acc (recur (- n 1) (+ acc n))))) (f 5)"
           "((fn r [& xs] (if (seq xs) (+ (first xs) (apply r (rest xs))) 0)) 1 2 3)"]
  (let [[code out err] (d/run-on-chez ctx src)
        want (cli-oracle src)]
    (ok (string "arity: " src) (and (= code 0) (= out want))
        (string "chez=" out " janet=" want " | " err))))

# 3i) throw / try / catch / finally + ex-info (inc 3e). Value parity vs the CLI
#   oracle for caught throws; an uncaught throw must exit non-zero.
(each src [# jolt catch syntax is (catch Class binding body); the class is dropped
           # in the IR (catch-all). catch binds the thrown value raw.
           "(try (throw 42) (catch Exception e e))"
           "(try (+ 1 (throw 7)) (catch Exception e (* e 10)))"
           # finally runs and its value is discarded (try returns the body value)
           "(try 5 (finally 99))"
           "(try (throw 3) (catch Exception e (+ e 1)) (finally 99))"
           # body value passes through when nothing throws
           "(try (+ 2 3) (catch Exception e :nope))"
           # ex-info builds a real jolt map: read message/data via get (native-op)
           "(get (ex-info \"boom\" {:a 1}) :message)"
           "(get (ex-info \"boom\" {:a 1}) :data)"
           "(try (throw (ex-info \"boom\" {:a 1})) (catch Exception e (get e :message)))"
           "(try (throw (ex-info \"boom\" {:a 7})) (catch Exception e (get (get e :data) :a)))"
           # nested try: inner rethrows, outer catches
           "(try (try (throw 1) (catch Exception e (throw (+ e 1)))) (catch Exception e (* e 100)))"]
  (let [[code out err] (d/run-on-chez ctx src)
        want (cli-oracle src)]
    (ok (string "throw/try: " src) (and (= code 0) (= out want))
        (string "chez=" out " janet=" want " | " err))))

# an uncaught throw aborts the program (non-zero exit) — matches the corpus
# `:throws` semantics (interpret/compile both bail).
(let [[code out err] (d/run-on-chez ctx "(throw (ex-info \"unhandled\" {}))")]
  (ok "throw: uncaught exits non-zero" (not= code 0)
      (string "code=" code " out=" out)))

# 3j) quoted literals (inc 3f): a :quote node reconstructs the reader form as RT
#   values — symbols, lists, vectors, maps, sets, nested. Value parity vs the CLI.
(each src ["'foo"
           "'foo/bar"
           "':kw"
           "'(1 2 3)"
           "'[1 2 3]"
           "'(a b c)"
           "'{:a 1}"
           "'(1 (2 3) 4)"
           "(first '(10 20 30))"
           "(count '[1 2 3])"
           "(rest '(1 2 3))"
           "(= 'foo 'foo)"
           "(= 'a 'b)"
           "(map inc '(1 2 3))"
           "(conj '[1 2] 3)"
           "(get '{:a 7} :a)"]
  (let [[code out err] (d/run-on-chez ctx src)
        want (cli-oracle src)]
    (ok (string "quote: " src) (and (= code 0) (= out want))
        (string "chez=" out " janet=" want " | " err))))

# 3k) letfn + declare/def-no-init (inc 3g). letfn lowers to a Scheme `letrec*`
#   (mutual recursion between the named local fns — a plain let* can't forward-
#   ref a sibling). declare/(def x) with no init pre-creates the var cell so a
#   forward reference resolves; the real def runs before any call.
(each src [# single local fn
           "(letfn [(twice [x] (* x 2))] (twice 5))"
           # self-recursion within a local fn
           "(letfn [(fact [n] (if (zero? n) 1 (* n (fact (dec n)))))] (fact 5))"
           # MUTUAL recursion — the letrec semantics a sequential let* lacks
           "(letfn [(ev? [n] (if (zero? n) true (od? (dec n)))) (od? [n] (if (zero? n) false (ev? (dec n))))] (ev? 10))"
           "(letfn [(ev? [n] (if (zero? n) true (od? (dec n)))) (od? [n] (if (zero? n) false (ev? (dec n))))] (od? 7))"
           # local fn passed to a higher-order fn
           "(letfn [(sq [x] (* x x))] (map sq [1 2 3]))"
           # declare + forward reference (the canonical mutually-recursive top-level use)
           "(declare is-ev) (defn is-od [n] (if (zero? n) false (is-ev (dec n)))) (defn is-ev [n] (if (zero? n) true (is-od (dec n)))) (is-ev 10)"
           # declare then redefine: the real def overwrites the reserved cell
           "(declare foo) (def foo 10) foo"]
  (let [[code out err] (d/run-on-chez ctx src)
        want (cli-oracle src)]
    (ok (string "letfn/declare: " src) (and (= code 0) (= out want))
        (string "chez=" out " janet=" want " | " err))))

# 3l) host interop method calls (inc 3h). (.method target arg*) analyzes to a
#   :host-call IR node and lowers to a jolt-host-call dispatch. The Janet back end
#   PUNTS these (no interop model -> interpreter); the Chez RT shims the methods
#   jolt-core's io tier uses: .write -> display to a port, .isDirectory ->
#   file-directory?, .listFiles -> directory-list. Interop has no portable oracle
#   (the Janet host models it differently), so these are emit-shape checks plus one
#   deterministic runtime probe (the root "/" is always a directory).
(each [label src needle]
  [["emit .write -> jolt-host-call" "(fn [w x] (.write w x))" "jolt-host-call"]
   ["emit .write keeps method name" "(fn [w x] (.write w x))" "\"write\""]
   ["emit .isDirectory -> jolt-host-call" "(fn [f] (.isDirectory f))" "isDirectory"]
   ["emit .listFiles -> jolt-host-call" "(fn [f] (.listFiles f))" "listFiles"]]
  (let [scm (protect (emit/emit (backend/analyze-form ctx (in (r/parse-next src) 0))))]
    (ok label (and (scm 0) (string/find needle (scm 1))) (string/format "%p" scm))))

(let [[code out err] (d/run-on-chez ctx "(.isDirectory \"/\")")]
  (ok "runtime .isDirectory \"/\" = true" (and (= code 0) (= out "true"))
      (string "chez=" out " | " err)))

# 3m) regex (jolt-i0s3): the #"…" literal lowers to a jolt-regex value over the
#   vendored irregex; re-pattern/re-matches/re-find/re-seq/regex? are def-var!'d
#   into clojure.core (not subset native-ops — irregex's Unicode/property
#   semantics differ from the seed's byte-PEG), so they resolve in PRELUDE mode,
#   the path the assembled prelude takes. Parity vs the CLI oracle on standard
#   PCRE patterns both engines agree on.
(defn run-prelude [src]
  (emit/set-prelude-mode! true)
  (def r (protect (emit/emit (backend/analyze-form ctx (in (r/parse-next src) 0)))))
  (emit/set-prelude-mode! false)
  (if (not (r 0)) [:emit-err (r 1) ""]
    (do
      (spit "/tmp/chez-regex-prelude.ss" (emit/program @[] (r 1)))
      (def proc (os/spawn ["chez" "--script" "/tmp/chez-regex-prelude.ss"] :p {:out :pipe :err :pipe}))
      (def out (ev/read (proc :out) 0x100000))
      (def err (ev/read (proc :err) 0x100000))
      [(os/proc-wait proc) (string/trim (if out (string out) "")) (string/trim (if err (string err) ""))])))

# bare #"…" literal runs in plain subset mode (the :regex node needs no core fn).
(each src ["#\"\\d+\"" "(do #\"a.c\")"]
  (let [[code out err] (d/run-on-chez ctx src) want (cli-oracle src)]
    (ok (string "regex literal: " src) (and (= code 0) (= out want))
        (string "chez=" out " janet=" want " | " err))))

# re-* surface via prelude mode (def-var!'d fns), parity vs the CLI oracle.
(each src ["(re-matches #\"\\d+\" \"123\")"
           "(re-matches #\"\\d+\" \"12a\")"
           "(re-find #\"\\d+\" \"abc123def\")"
           "(re-find #\"([a-z])(\\d)\" \"--a1--\")"
           "(re-seq #\"\\d+\" \"a1b22c333\")"
           "(regex? #\"\\d+\")"
           "(re-matches #\"[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}\" \"550e8400-e29b-41d4-a716-446655440000\")"]
  (let [[code out err] (run-prelude src) want (cli-oracle src)]
    (ok (string "regex: " src) (and (= code 0) (= out want))
        (string "chez=" out " janet=" want " | " err))))

# 3h) prelude mode (inc 3d): emitting clojure.core ITSELF, a core->core ref must
#   lower to a runtime var-deref instead of being rejected as "out of subset".
#   `frequencies` is a core fn but not a native-op, so it exercises the switch.
(let [ir (backend/analyze-form ctx (in (r/parse-next "(fn [x] (frequencies x))") 0))]
  # subset mode (the default): a non-native core ref is rejected at emit time.
  (ok "prelude: subset mode rejects non-native core ref"
      (let [r (protect (emit/emit ir))] (not (r 0))))
  # prelude mode: the same ref lowers to (var-deref "clojure.core" "frequencies").
  (emit/set-prelude-mode! true)
  (def scm (protect (emit/emit ir)))
  (emit/set-prelude-mode! false)
  (ok "prelude: mode lowers non-native core ref to var-deref"
      (and (scm 0)
           (string/find "var-deref" (scm 1))
           (string/find "frequencies" (scm 1)))
      (string/format "%p" scm)))

# 4) perf signal: emitted fib(30) in-Scheme timing (excludes Chez startup), to
#    track against the spike ceiling (hand-Scheme fib ~5ms). Informational — the
#    jolt-truthy? wrapper (~3x) and flonum modeling are known Phase-4 levers.
(let [fib-ir (backend/analyze-form ctx (in (r/parse-next "(defn fib [n] (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))") 0))
      fib-scm (emit/emit fib-ir)
      timed (string "(import (chezscheme))\n(load \"host/chez/rt.ss\")\n"
                    fib-scm "\n"
                    "(define fib (var-deref \"user\" \"fib\"))\n"
                    "(define (now-ns) (let ((t (current-time 'time-monotonic))) (+ (* (time-second t) 1000000000) (time-nanosecond t))))\n"
                    "(fib 24)(fib 24)\n"
                    "(let* ((t0 (now-ns)) (r (fib 30)) (ms (/ (- (now-ns) t0) 1000000.0)))\n"
                    "  (printf \"~a ~a\\n\" (jolt-pr-str r) (exact->inexact ms)))")]
  (spit "/tmp/chez-jolt-fib-timed.ss" timed)
  (def proc (os/spawn ["chez" "--script" "/tmp/chez-jolt-fib-timed.ss"] :p {:out :pipe :err :pipe}))
  (def out (string/trim (string (ev/read (proc :out) 0x100000))))
  (def err (string/trim (string (or (ev/read (proc :err) 0x100000) ""))))
  (def code (os/proc-wait proc))
  (def parts (string/split " " out))
  (def result (get parts 0))
  (def ms (scan-number (or (get parts 1) "999")))
  (ok "timed fib(30) correct" (and (= code 0) (= result "832040")) (string out " | " err))
  (printf "  emitted fib(30): %s in %.2f ms (hand-Scheme spike ~5ms)" result ms))

(printf "\nemit-test: %d/%d passed" (- total fails) total)
(os/exit (if (> fails 0) 1 0))
