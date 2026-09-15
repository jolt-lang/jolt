;; eval-test.ss — the G3 gate for the Gambit compiler path (jolt-mj95.4).
;;
;; Run via `make gambiteval` (detection-gated, from the repo root). Boots the
;; full manifest + the cross-minted :gambit seed, then drives real jolt SOURCE
;; through jolt-compile-eval and compares the jolt-pr-readable rendering
;; against values captured from the Chez build (bin/jolt -e "(pr-str EXPR)").
;; The same rows run compiled-to-js under node (manual; see the demo recipe in
;; the site docs) — this gate pins the gsi half.

(##include "boot.ss")

(define failures 0)

(define (check src expected)
  (let ((actual (guard (e (#t "THREW"))
                  (jolt-pr-readable (jolt-compile-eval src "user")))))
    (if (string=? actual expected)
        (printf "  ok     ~a => ~a\n" src actual)
        (begin (printf "  FAIL   ~a: got ~s expected ~s\n" src actual expected)
               (set! failures (+ failures 1))))))

(printf "== jolt source through jolt-compile-eval on gsi ==\n")

;; arithmetic + tower
(check "(+ 1 2)" "3")
(check "(* 2/3 6)" "4")
(check "(+ 9007199254740993 1)" "9007199254740994")
;; collections + rendering
(check "{:a 1 :b 2}" "{:a 1, :b 2}")
(check "(conj [1 2] 3)" "[1 2 3]")
(check "(assoc {:a 1} :b 2)" "{:a 1, :b 2}")
;; lazy seqs + HOFs through the prelude
(check "(map inc [1 2 3])" "(2 3 4)")
(check "(->> (range 100) (filter odd?) (map #(* % %)) (reduce +))" "166650")
(check "(->> (range 20) (filter even?) (map inc) (take 5) vec)" "[1 3 5 7 9]")
;; def + cross-row invocation + recursion
(check "(defn fib [n] (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))" "#'user/fib")
(check "(fib 20)" "6765")
;; let/loop
(check "(let [x 10] (* x x))" "100")
(check "(loop [i 0 acc 0] (if (< i 5) (recur (inc i) (+ acc i)) acc))" "10")
;; strings + regex
(check "(str \"jolt on \" \"gambit\")" "\"jolt on gambit\"")
(check "(re-seq #\"[a-z]+\" \"jolt on gambit js\")" "(\"jolt\" \"on\" \"gambit\" \"js\")")
;; defmacro then use in a later row
(check "(defmacro unless2 [c a b] (list 'if c b a))" "nil")
(check "(unless2 false :yes :no)" ":yes")
;; try/catch
(check "(try (/ 1 0) (catch Exception e :caught))" ":caught")
;; sort with comparator
(check "(sort > [3 1 4 1 5 9 2 6])" "(9 6 5 4 3 2 1 1)")
;; records + protocols
(check "(defrecord Pt [x y])" "#'user/map->Pt")
(check "(:x (->Pt 3 4))" "3")
(check "(pr-str (->Pt 1 2))" "\"#user.Pt{:x 1, :y 2}\"")
(check "(defprotocol Shape (perim [s]))" "#'user/perim")
(check "(defrecord Sq [w] Shape (perim [s] (* 4 (:w s))))" "nil")
(check "(perim (->Sq 5))" "20")
;; multimethods
(check "(defmulti area :shape)" "#'user/area")
(check "(area {:shape :circle :r 21})" "THREW")   ; no method yet -> throws
(check "(defmethod area :circle [c] (* 2 (:r c)))" "#object[clojure.lang.MultiFn 0x0 \"area\"]")
(check "(area {:shape :circle :r 21})" "42")
;; anonymous fns + map results
(check "((fn [a b] {:sum (+ a b) :prod (* a b)}) 3 4)" "{:sum 7, :prod 12}")
;; deftype interface surface: a CharSequence window answers count/seq/nth and the
;; regex entry points, and a collection deftype prints in its collection's shape.
;; These live in records.ss, so they reach here only through the GENERATED
;; records-gambit.ss — a stale generation shows up as a failure on these rows.
(check (string-append "(do (deftype Sg [s cnt] CharSequence (length [_] cnt)"
                      " (charAt [_ i] (.charAt s i)) (subSequence [_ a b] (subs s a b))"
                      " (toString [_] (subs s 0 cnt)))"
                      " [(count (->Sg \"abcd\" 3)) (vec (seq (->Sg \"abcd\" 3))) (nth (->Sg \"abcd\" 3) 1)])")
       "[3 [\\a \\b \\c] \\b]")
(check (string-append "(do (deftype Sgr [s cnt] CharSequence (length [_] cnt)"
                      " (charAt [_ i] (.charAt s i)) (subSequence [_ a b] (subs s a b))"
                      " (toString [_] (subs s 0 cnt)))"
                      " (re-matches #\"a+\" (->Sgr \"aaa\" 3)))")
       "\"aaa\"")
(check (string-append "(do (deftype Psq [v] clojure.lang.ISeq (seq [_] (seq v))"
                      " (first [_] (first v)) (next [_] (next (seq v))) (more [_] (rest (seq v)))"
                      " (count [_] (count v)) (equiv [_ o] (= (seq v) o)) (empty [_] nil)"
                      " (cons [_ o] (cons o (seq v))))"
                      " [(pr-str (->Psq [1 2 3])) (str (->Psq [1 2 3]))])")
       "[\"(1 2 3)\" \"(1 2 3)\"]")
;; the runtime target must never emit chez unsafe spellings
(check "(count \"gambit\")" "6")

;; ---- host tier: the names the excluded java/ tree owns on Chez ---------------
;; `time` reaches current-time-ms and the printer's host-writer probe; both were
;; unbound names before host-vars.ss, so the macro died with a bare Gambit error.
(check "(time 1)" "1")
(check "(time (reduce + (range 100)))" "4950")
(check "(> (current-time-ms) 1700000000000)" "true")
(check "(do (flush) :ok)" ":ok")

;; A type this target cannot construct answers the question instead of raising.
(check "(delay? 1)" "false")
(check "(queue? 1)" "false")
(check "(tap> 1)" "false")

;; An absent capability raises a catchable, named error rather than crashing on
;; an unbound global.
(check "(try (future 1) (catch Exception e (str (class e))))"
       "\"class java.lang.UnsupportedOperationException\"")

;; Gambit has no arity introspection, so its own runtime raises on a mismatch;
;; those exceptions must still carry a class and a message (they rendered as
;; "#object[:object]" before).
(check "(try ((fn [x] x)) (catch Exception e (str (class e))))"
       "\"class clojure.lang.ArityException\"")
(check "(str (class 1))" "\"class java.lang.Long\"")

;; ---- jolt-cw2p: the names the boot had lost since 2026-09-02 ------------------
;; Every row here died on an unbound global or an unbound var while the gate sat
;; outside ci. gambitunbound / gambitvars pin the names statically; these pin
;; the paths.

;; Gambit's case-lambda miscompiles the two-clause (fixed n / variadic n) fn —
;; the emitter merges it on this target (backend_scheme.clj emit-fn), in the
;; seed (bit-and) and in eval'd code (f2), under jolt-apply's boxed rest too.
(check "(bit-and 12 10)" "8")
(check "(bit-or 1 2 4)" "7")
(check "(do (defn f2 ([x y] [x y]) ([x y & more] [x y more])) [(f2 1 2) (f2 1 2 3) (apply f2 1 2 [3 4]) (apply f2 1 [2])])"
       "[[1 2] [1 2 (3)] [1 2 (3 4)] [1 2]]")
;; for-all / real->flonum (prelude-shims), the chunk builder (natives-transduce.ss)
(check "(seq (chunk-first (seq (vec (range 40)))))"
       "(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31)")
(check "(* 2 1.5)" "3.0")
;; the hash caches (hasheq.ss mirrors) and Object.hashCode parity (natives-misc.ss)
(check "(hash [1 2 3])" "736442005")
(check "(hash (map inc [1 2]))" "-1504369821")
(check "[(.hashCode :a) (.hashCode (quote a/b)) (.hashCode \"ab\")]" "[1013910569 -1640525104 3105]")
(check "(defrecord Q [a])" "#'user/map->Q")
(check "(= (hash (->Q 1)) (hash (->Q 1)))" "true")
;; rt-core: indexOf/lastIndexOf on the kernel's own search, object monitors, the
;; ns-cells index ns.ss reads, proc-name-of for (class a-fn)
(check "[(.indexOf \"hello\" \"l\") (.indexOf \"hello\" \"l\" 3) (.lastIndexOf \"hello\" \"l\") (.indexOf \"hello\" \\l)]" "[2 3 3 2]")
(check "(locking :x 1)" "1")
(check "(count (ns-publics (quote clojure.set)))" "12")
(check "(str (find-var (quote clojure.core/print-method)))" "\"#'clojure.core/print-method\"")
;; class-objects.ss: the class model core's predicates and isa? read
(check "[(isa? Long Number) (seqable? 1) (ifn? :a) (class? String) (instance? String \"a\")]"
       "[true false true true true]")
(check "(count (supers Long))" "4")
(check "(do (defmulti m1 (fn [x] (class x))) (defmethod m1 Number [x] :num) (m1 1))" ":num")
;; the prelude's own (import '(clojure.lang … Sequential …)) binds under
;; clojure.core now that the boot brackets the prelude in that namespace
(check "(instance? Sequential (eduction (map inc) [1 2]))" "true")
(check "(seq (eduction (map inc) [1 2]))" "(2 3)")
;; host-new builds the typed throwable every core throw site constructs
(check "(try (zero? \"a\") (catch ClassCastException e (ex-message e)))"
       "\"class java.lang.String cannot be cast to class java.lang.Number\"")
;; with-open's close seam, the printer's *out* default, the eval'd-code twins of
;; the spliced predicates (a catch clause reads jolt-truthy? as a function)
(check "(with-open [r (reify java.io.Closeable (close [_] nil))] 1)" "1")
(check "(with-out-str (print \"hi\"))" "\"hi\"")
(check "(try (throw (ex-info \"x\" {})) (catch Exception e :caught))" ":caught")
;; DIVERGENCES this target documents rather than hides: a \\p{…} class is a
;; PatternSyntaxException (no Unicode categories here; Chez matches), and a
;; host class this boot has no shim for names itself.
(check "(try (re-pattern \"\\\\p{L}\") (catch Exception e (str (class e))))"
       "\"class java.util.regex.PatternSyntaxException\"")
(check "(try (clojure.pprint/pprint 1) (catch UnsupportedOperationException e (ex-message e)))"
       "\"(new StringBuilder) is unsupported on the gambit target: there are no JVM class shims\"")

;; a ^double-hinted fn compiles WITHOUT #3% in the emitted text (the R9
;; target-prims table at :gambit maps the unsafe prefix to "")
(let ((scm (jolt-analyze-emit-form
             (jolt-ce-read "(defn dd [^double x] (* x x))") "user")))
  (if (let loop ((i 0))
        (cond ((> (+ i 3) (string-length scm)) #f)
              ((string=? (substring scm i (+ i 3)) "#3%") #t)
              (else (loop (+ i 1)))))
      (begin (printf "  FAIL   ^double emission contains #3%\n")
             (set! failures (+ failures 1)))
      (printf "  ok     ^double emission carries no #3% (target-prims :gambit)\n")))

(printf "eval-test: ~a failure(s)\n" failures)
(if (= failures 0)
    (begin (display "eval-test: PASS — jolt compiles and evaluates on native gsi\n") (exit 0))
    (exit 1))
