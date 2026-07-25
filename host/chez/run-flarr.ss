;; run-flarr.ss — (aget ^doubles a i) lowers to an unboxed flvector read
;; (jolt-flaget), typed :double, so surrounding arithmetic unboxes to fl+.
;; Covers both ^doubles PARAMS and ^doubles LET bindings.
(import (chezscheme))
(load "host/chez/run-gate-harness.ss")
(define analyze (var-deref "jolt.analyzer" "analyze"))
(define numeric-annotate (var-deref "jolt.passes.numeric" "annotate"))
(define emit (var-deref "jolt.backend-scheme" "emit"))
(define U ((var-deref "jolt.passes.types" "new-unit")))
((var-deref "jolt.backend-scheme" "set-emit-unit!") U)
((var-deref "jolt.backend-scheme" "set-prelude-mode!") #t)
(define (anode src) (analyze (make-analyze-ctx "user") (jolt-ce-read src)))
(define (emit-num src) (emit (numeric-annotate (anode src))))
(let ((e (emit-num "(def _ (fn [^doubles a ^long i] (aget a i)))")))
  (gate-check "(1) aget ^doubles param -> jolt-flaget" (gate-sub? e "jolt-flaget") #t)
  (gate-check "(1) ...not the generic jolt-nth" (gate-sub? e "jolt-nth") #f))
(let ((e (emit-num "(def _ (fn [^doubles a ^long i] (+ (aget a i) (aget a i))))")))
  (gate-check "(2) arithmetic over ^doubles param reads -> fl+" (gate-sub? e "fl+") #t)
  (gate-check "(2) ...reading via jolt-flaget" (gate-sub? e "jolt-flaget") #t))
(let ((e (emit-num "(def _ (fn [a i] (aget a i)))")))
  (gate-check "(3) untyped aget stays native jolt-nth (not flaget)" (gate-sub? e "jolt-flaget") #f)
  (gate-check "(3) ...uses jolt-nth" (gate-sub? e "jolt-nth") #t))
(let ((e (emit-num "(def _ (fn [m ^long i] (let [^doubles a (:pixels m)] (aget a i))))")))
  (gate-check "(4) ^doubles LET-binding aget -> jolt-flaget" (gate-sub? e "jolt-flaget") #t))
(let ((e (emit-num "(def _ (fn [m ^long i] (let [^doubles a (:pixels m)] (+ (aget a i) (aget a i)))))")))
  (gate-check "(5) arithmetic over ^doubles let read -> fl+" (gate-sub? e "fl+") #t))

;; --- runtime value semantics of jolt-flaget/jolt-flaset ---------------------------
;; Pin both index paths: the fixnum fast path (the hot case — loop counters are
;; :long) and the coercing slow path (flonum index floors via na-idx). Guards the
;; fast-path change against behavior drift: aset returns the stored value (JVM
;; contract), an int value stores as its double, and an out-of-range or negative
;; index raises (flvector-ref's range check = the array bounds contract).
(define (ev s) (jolt-compile-eval s "user"))
(gate-check "(6) aget fixnum index reads the stored double"
            (ev "(let [^doubles a (double-array 3)] (aset a 1 7.25) (aget a 1))") 7.25)
(gate-check "(6) aset returns the stored value"
            (ev "(let [^doubles a (double-array 3)] (aset a 1 7.25))") 7.25)
(gate-check "(6) aset of an int value stores its double"
            (ev "(let [^doubles a (double-array 3)] (aset a 0 4) (aget a 0))") 4.0)
(gate-check "(6) aget flonum index 1.0 floors to slot 1"
            (ev "(let [^doubles a (double-array 3)] (aset a 1 7.25) (aget a 1.0))") 7.25)
(gate-check "(6) aget flonum index 1.5 floors to slot 1"
            (ev "(let [^doubles a (double-array 3)] (aset a 1 7.25) (aget a 1.5))") 7.25)
(gate-check "(6) aget out-of-range fixnum index raises (catchable)"
            (jolt-truthy? (ev "(try (let [^doubles a (double-array 3)] (aget a 5) false) (catch Throwable e true))")) #t)
(gate-check "(6) aget negative index raises (catchable)"
            (jolt-truthy? (ev "(try (let [^doubles a (double-array 3)] (aget a -1) false) (catch Throwable e true))")) #t)
(gate-check "(6) aset out-of-range index raises (catchable)"
            (jolt-truthy? (ev "(try (let [^doubles a (double-array 3)] (aset a 9 1.0) false) (catch Throwable e true))")) #t)

;; --- OOB exception class (JVM: ArrayIndexOutOfBoundsException) --------------
;; The proven ^doubles path relies on flvector-ref's own range check (a pre-check
;; costs ~1ns/access); the escaping Chez condition classifies at inspection time
;; as java.lang.ArrayIndexOutOfBoundsException. The generic path throws typed
;; with the JVM message. Both dispatch precisely: the parent class catches, an
;; unrelated exception class does not.
(gate-check "(7) typed-path OOB class is AIOOBE"
            (ev "(try (let [^doubles a (double-array 3)] (aget a 5)) (catch Throwable e (str (class e))))")
            "class java.lang.ArrayIndexOutOfBoundsException")
(gate-check "(7) typed-path OOB caught by (catch ArrayIndexOutOfBoundsException ...)"
            (ev "(try (let [^doubles a (double-array 3)] (aget a 5) :no) (catch ArrayIndexOutOfBoundsException e :aioobe))")
            (keyword #f "aioobe"))
(gate-check "(7) typed-path OOB caught by parent IndexOutOfBoundsException"
            (ev "(try (let [^doubles a (double-array 3)] (aset a 9 1.0) :no) (catch IndexOutOfBoundsException e :ioobe))")
            (keyword #f "ioobe"))
(gate-check "(7) typed-path OOB NOT caught by NullPointerException"
            (ev "(try (try (let [^doubles a (double-array 3)] (aget a 5) :no) (catch NullPointerException e :npe)) (catch Throwable t :outer))")
            (keyword #f "outer"))
(gate-check "(8) generic-path OOB class is AIOOBE"
            (ev "(try (aget (int-array 3) 5) (catch Throwable e (str (class e))))")
            "class java.lang.ArrayIndexOutOfBoundsException")
(gate-check "(8) generic-path OOB carries the JVM message"
            (ev "(try (aget (int-array 3) 5) (catch ArrayIndexOutOfBoundsException e (ex-message e)))")
            "Index 5 out of bounds for length 3")
(gate-check "(8) generic-path negative index"
            (ev "(try (aget (object-array 2) -1) (catch ArrayIndexOutOfBoundsException e (ex-message e)))")
            "Index -1 out of bounds for length 2")
(gate-check "(8) generic aset OOB throws AIOOBE"
            (ev "(try (aset (long-array 2) 9 1) (catch ArrayIndexOutOfBoundsException e :aioobe))")
            (keyword #f "aioobe"))
(gate-check "(8) byte-array aset OOB throws AIOOBE"
            (ev "(try (aset (byte-array 2) 5 (byte 1)) (catch ArrayIndexOutOfBoundsException e :aioobe))")
            (keyword #f "aioobe"))
(gate-check "(8) get on an array stays non-throwing OOB (returns default)"
            (ev "(get (int-array 3) 99 :d)") (keyword #f "d"))
(gate-summary "flarr")
