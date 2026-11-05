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
                      " [(pr-str (->Psq [1 2 3])) (clojure.string/replace (str (->Psq [1 2 3])) #\"@[0-9a-f]+\" \"@H\")])")
       ;; str is toString, which a deftype implementing ISeq does not override:
       ;; Object's Class@hash, as on the JVM
       "[\"(1 2 3)\" \"user.Psq@H\"]")
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
;; the same pair declared variadic-first: the emitter orders the fixed clause
;; ahead of the variadic one before the merge reads them by position
(check "(do (defn f3 ([x y & more] [x y more]) ([x y] [x y])) [(f3 1 2) (f3 1 2 3) (apply f3 1 [2]) (apply f3 1 2 [3 4])])"
       "[[1 2] [1 2 (3)] [1 2] [1 2 (3 4)]]")
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
;; java/class-model.ss (shared with Chez): the class model core's predicates and isa? read
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
;; A DIVERGENCE this target documents rather than hides: a \\p{…} class is a
;; PatternSyntaxException (no Unicode categories here; Chez matches).
(check "(try (re-pattern \"\\\\p{L}\") (catch Exception e (str (class e))))"
       "\"class java.util.regex.PatternSyntaxException\"")

;; ---- the seed's own statics and constructors (host-statics.ss) ---------------
;; Every Class/member the seed emits resolves (make gambitstatics); these rows
;; run the paths behind them. parse-long is Long/parseLong with the throw
;; caught, munge/demunge are the Compiler statics compile-eval.ss registers,
;; pprint and cl-format build into a StringBuilder (java/string-builder.ss,
;; shared with Chez), and (Object.) is the fresh-identity sentinel.
(check "[(parse-long \"42\") (parse-long \"x\") (parse-long \"-7\")]" "[42 nil -7]")
(check "(try (Long/parseLong \"zz\") (catch NumberFormatException e (ex-message e)))"
       "\"For input string: \\\"zz\\\"\"")
(check "[(munge \"a-b?\") (clojure.lang.Compiler/demunge \"a_b_QMARK_\")]" "[\"a_b_QMARK_\" \"a-b?\"]")
(check "[(Math/floor 2.5) (Math/abs -3) (String/join \",\" [\"a\" \"b\"]) (Character/isWhitespace \\space) (clojure.lang.Util/equiv 1 1)]"
       "[2.0 3 \"a,b\" true true]")
;; java.lang.Math and clojure.math are math.ss, shared with Chez; number
;; printing is number-print.ss, also shared (a subnormal prints its two nearest
;; digits, as Double.toString does).
(check "[(clojure.math/sqrt 16) (clojure.math/next-up 1.0) (clojure.math/ulp 1.0) (str (clojure.math/copy-sign 1.0 -0.0)) (clojure.math/IEEE-remainder 11.0 3.0) (clojure.math/get-exponent 1024.5)]"
       "[4.0 1.0000000000000002 2.220446049250313E-16 \"-1.0\" -1.0 10]")
(check "[(try (clojure.math/add-exact 9223372036854775807 1) (catch ArithmeticException e (ex-message e))) (Math/multiplyExact 6 7) (str (Math/min -0.0 0.0)) (Math/signum -2.5)]"
       "[\"long overflow\" 42 \"-0.0\" -1.0]")
(check "[(Double/doubleToLongBits 1.5) (str (Double/longBitsToDouble 1)) (Float/floatToIntBits 0.1) (str 9.9E-324)]"
       "[4609434218613702656 \"4.9E-324\" 1036831949 \"9.9E-324\"]")
(check "[(= (class (Object.)) Object) (identical? (Object.) (Object.)) (System/getProperty \"line.separator\")]"
       "[true false \"\\n\"]")
(check "(let [sb (StringBuilder.)] (.append sb \"a\") (.append sb 1) (.append sb \\c) (str sb))" "\"a1c\"")
(check "(let [sb (StringBuilder. \"abc\")] [(.length sb) (.charAt sb 1) (str (.reverse sb)) (count sb) (instance? CharSequence sb)])"
       "[3 \\b \"cba\" 3 true]")
(check "(with-out-str (clojure.pprint/pprint {:a [1 2] :b \"x\"}))" "\"{:a [1 2], :b \\\"x\\\"}\\n\"")
(check "(clojure.pprint/cl-format nil \"~5d|~a|~{~a~^,~}\" 42 :x [1 2 3])" "\"   42|:x|1,2,3\"")
;; code-dispatch's table: pprint-if is a top-level formatter with a numeric
;; param (~1I), compiled while the seed prelude loads; an instance? miss there
;; once hit an unbound host fn and the def was dropped
(check "(binding [clojure.pprint/*print-right-margin* 24] (with-out-str (clojure.pprint/with-pprint-dispatch clojure.pprint/code-dispatch (clojure.pprint/pprint '(defn f [x] (if (pos? x) (inc x) (dec x)))))))" "\"(defn \\n  f\\n  [x]\\n  (if\\n    (pos? x)\\n    (inc x)\\n    (dec x)))\\n\"")
(check "(clojure.pprint/cl-format nil \"~,2f\" 3.14159)" "\"3.14\"")
(check "(with-out-str (clojure.pprint/print-table [{:a 1 :b 2}]))"
       "\"\\n| :a | :b |\\n|----+----|\\n|  1 |  2 |\\n\"")
;; java/class-model.ss (shared with Chez): the Class object's own methods and
;; the JVM's three spellings of a class
(check "[(.getName (class 1)) (.getSimpleName String) (.isInterface Sequential) (str (.getSuperclass Long))]"
       "[\"java.lang.Long\" \"String\" true \"class java.lang.Number\"]")
(check "[(str (class [])) (str Sequential) (pr-str Long)]"
       "[\"class clojure.lang.PersistentVector\" \"interface clojure.lang.Sequential\" \"java.lang.Long\"]")
;; java/byte-buffer.ss (shared with Chez): ByteBuffer over a bytevector — the
;; widths, byte order, the exact-arithmetic float codecs, views, the JDK's
;; exceptions and value semantics, and compact's in-place block move
(check "(let [b (java.nio.ByteBuffer/allocate 16)] (.putInt b 258) (.putDouble b 1.5) (.flip b) [(.getInt b) (.getDouble b) (.remaining b)])"
       "[258 1.5 0]")
(check "(let [b (.order (java.nio.ByteBuffer/allocate 4) java.nio.ByteOrder/LITTLE_ENDIAN)] (.putInt b 1) (.rewind b) [(.get b) (.get b 1) (str (.order b))])"
       "[1 0 \"LITTLE_ENDIAN\"]")
(check "(let [b (java.nio.ByteBuffer/allocate 4)] (.putFloat b 0.1) [(.getFloat b 0) (.getInt b 0)])"
       "[0.1 1036831949]")
(check "[(str (java.nio.ByteBuffer/allocate 2)) (.getName (class (java.nio.ByteBuffer/allocateDirect 1))) (instance? java.nio.Buffer (java.nio.ByteBuffer/allocate 1))]"
       "[\"java.nio.HeapByteBuffer[pos=0 lim=2 cap=2]\" \"java.nio.DirectByteBuffer\" true]")
(check "(let [b (java.nio.ByteBuffer/allocate 8) i (.asIntBuffer b)] (.put i 1 7) [(.getInt b 4) (str (.asCharBuffer (doto (java.nio.ByteBuffer/allocate 4) (.putChar \\h) (.putChar \\i) .flip)))])"
       "[7 \"hi\"]")
(check "[(try (.getInt (java.nio.ByteBuffer/allocate 2)) (catch java.nio.BufferUnderflowException e :underflow)) (try (.put (.asReadOnlyBuffer (java.nio.ByteBuffer/allocate 1)) (byte 1)) (catch java.nio.ReadOnlyBufferException e :read-only))]"
       "[:underflow :read-only]")
(check "[(= (java.nio.ByteBuffer/allocate 2) (java.nio.ByteBuffer/allocate 2)) (.hashCode (doto (java.nio.ByteBuffer/allocate 2) (.put (byte 1)) .flip)) (compare (doto (java.nio.ByteBuffer/allocate 1) (.put 0 (byte 5))) (java.nio.ByteBuffer/allocate 1))]"
       "[true 32 5]")
(check "(let [b (java.nio.ByteBuffer/allocate 4)] (.putInt b 16909060) (.flip b) (.get b) (.compact b) [(.position b) (.get b 0) (.get b 2)])"
       "[3 2 4]")
;; a StringCharBuffer needs no array, so CharBuffer/wrap of a string is here too
(check "(let [s (java.nio.CharBuffer/wrap \"hi\") v (.asCharBuffer (doto (java.nio.ByteBuffer/allocate 4) (.putChar \\h) (.putChar \\i) .flip))] [(= s v) (.hashCode s) (str (.subSequence s 1 2)) (.getName (class s)) (.isReadOnly s)])"
       "[true 4320 \"i\" \"java.nio.StringCharBuffer\" true]")

;; java/tree-map.ss + java/jutil-colls.ss (shared with Chez): a TreeMap over
;; clojure.core's sorted map, its live views, TreeSet, the Comparator statics,
;; and the java.util.Map seam (=, pr, str, reduce-kv). Values from the Chez build.
(check "(let [m (java.util.TreeMap. {:c 3 :a 1 :b 2})] [(vec (keys m)) (str m) (pr-str m) (.firstKey m) (.floorKey m :bb) (vec (keys (.headMap m :b true)))])"
       "[[:a :b :c] \"{:a=1, :b=2, :c=3}\" \"{:a 1, :b 2, :c 3}\" :a :b [:a :b]]")
(check "(let [m (java.util.TreeMap. >) d (.descendingMap m)] (.put m 1 :a) (.put d 3 :c) [(vec (keys m)) (vec (keys d)) (count m) (get m 3) (try (.put (.headMap m 2) 0 :x) (catch IllegalArgumentException e :iae))])"
       "[[3 1] [1 3] 2 :c :iae]")
(check "(let [s (java.util.TreeSet. (java.util.Comparator/reverseOrder))] (.addAll s [1 3 2]) [(vec s) (.first s) (vec (.headSet s 2)) (= s #{1 2 3}) (str s)])"
       "[[3 2 1] 3 [3] true \"[3, 2, 1]\"]")
(check "[(try (.put (java.util.TreeMap.) nil 1) (catch NullPointerException e :npe)) (try (let [m (java.util.TreeMap.)] (.put m :a 1) (.put m \"b\" 2)) (catch ClassCastException e :cce)) (= (java.util.TreeMap. {:a 1}) {:a 1}) (reduce-kv (fn [a k v] (conj a k v)) [] (java.util.TreeMap. {2 :b 1 :a}))]"
       "[:npe :cce true [1 :a 2 :b]]")

(check "(let [m (java.util.TreeMap. {:a 1 :b 2})] (doseq [e (.entrySet m)] (.setValue e (* 10 (val e)))) [(str m) (str (first m)) (into {} m) (try (.setValue (.firstEntry m) 0) (catch UnsupportedOperationException e :uoe))])"
       "[\"{:a=10, :b=20}\" \":a=10\" {:a 10, :b 20} :uoe]")

;; ---- library value seams: __register-compare! / __register-class! ------------
;; jolt-lang/time registers its java.time values through these at load; both
;; were unbound here, so its install-seams! died on the first one. A tagged
;; table stands in for a library value: it orders through the compare arm,
;; answers (class x) through the class arm, and dispatches a protocol extended
;; to a class its tags-fn reports.
(check "[(jolt.host/profile-instrument! true) (jolt.host/profile-instrument! false) (jolt.host/profile-instrumenting?) (jolt.host/profile-sites) (count (jolt.host/profile-stats))]" "[false true false [] 7]")
(check "[(jolt.host/sample-start! 1000000 8) (jolt.host/sampling?) (jolt.host/sample-stop!) (jolt.host/sample-read)]" "[false false nil [0 []]]")
(check "(do (defn spt? [x] (= :spt (jolt.host/ref-get x :tag))) (defn spt [n] (doto (jolt.host/tagged-table :spt) (jolt.host/ref-put! :tag :spt) (jolt.host/ref-put! :n n))) :ok)" ":ok")
(check "(clojure.core/__register-compare! (fn [a b] (or (spt? a) (spt? b))) (fn [a b] (compare (jolt.host/ref-get a :n) (jolt.host/ref-get b :n))))" "nil")
(check "[(compare (spt 1) (spt 2)) (compare (spt 2) (spt 2)) (mapv #(jolt.host/ref-get % :n) (sort [(spt 3) (spt 1) (spt 2)]))]"
       "[-1 0 [1 2 3]]")
(check "(clojure.core/__register-class! spt? (fn [_] \"my.Spt\") (fn [_] [\"my.Spt\" \"my.Shape\"]))" "nil")
(check "(do (defprotocol SptName (spt-name [x])) (extend-protocol SptName my.Shape (spt-name [x] :shape)) [(str (class (spt 1))) (spt-name (spt 1))])"
       "[\"class my.Spt\" :shape]")
;; the ctor/statics halves of the same library surface (jolt/time/*.clj call
;; __register-class-statics! at the top level)
(check "[(clojure.core/__register-class-statics! \"my.SptUtil\" {\"twice\" (fn [x] (* 2 x)) \"ZERO\" 0}) (clojure.core/__register-class-ctor! \"my.SptBox\" (fn [x] [:box x]))]" "[nil nil]")
(check "[(my.SptUtil/twice 21) my.SptUtil/ZERO (my.SptBox. 1)]" "[42 0 [:box 1]]")

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

;; a cast on a proven double takes the inline window + flonum->fixnum (the
;; back end's :fl-arg branch); the emitted fl>=?/fl<=? and flonum->fixnum
;; are prelude-shims names on this host. Edges: the window keeps the cast's
;; results, out of it the runtime cast still answers.
(check "(let [f (fn [^double d] (long d)) i (fn [^double d] (int d)) c (fn [^double d] (char d)) u (fn [^double d] (unchecked-int d)) m (fn [t] (try (t) (catch IllegalArgumentException e (ex-message e))))] [(f 2.9) (f -2.9) (f 1.152921504606847E18) (f ##NaN) (i 2147483647.0) (m (fn [] (i 2147483647.9))) (int (c 55295.0)) (int (c 55296.0)) (u 4294967296.5)])"
       "[2 -2 1152921504606846976 0 2147483647 \"Value out of range for int: 2.1474836479E9\" 55295 55296 2147483647]")

;; outside the window a primitive double takes the cast's -prim entry and a boxed
;; one the helper: RT's two overloads differ on NaN and on a fractional edge.
(check "(let [b (fn [^double d] (byte d)) c (fn [^double d] (char d)) m (fn [t] (try (t) (catch IllegalArgumentException e (ex-message e))))] [(m (fn [] (b ##NaN))) (m (fn [] (b 127.9))) (m (fn [] (c ##NaN))) (byte (identity ##NaN)) (byte (identity 127.9)) (m (fn [] (byte (identity ##Inf)))) (m (fn [] (byte (float ##NaN))))])"
       "[\"Value out of range for byte: NaN\" \"Value out of range for byte: 127.9\" \"Value out of range for char: NaN\" 0 127 \"Value out of range for long: Infinity\" \"Value out of range for byte: NaN\"]")

(printf "eval-test: ~a failure(s)\n" failures)
(if (= failures 0)
    (begin (display "eval-test: PASS — jolt compiles and evaluates on native gsi\n") (exit 0))
    (exit 1))
