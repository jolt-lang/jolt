;; run-flarr.ss — (aget ^doubles a i) reads the array's backing flvector unboxed
;; and typed :double, so surrounding arithmetic unboxes to fl+. A PROVEN-:long (or
;; fixnum-literal) index is emitted INLINE — (flvector-ref (jolt-array-vec a) i) —
;; so the value stays unboxed across the procedure boundary; an unproven index
;; keeps the (jolt-flaget a i) call, which owns the fixnum?/na-idx coercion. aset
;; mirrors it: a proven index AND a :double value inline (flvector-set! ... v),
;; returning the stored value (JVM contract); an int value keeps (jolt-flaset ...)
;; (it owns exact->inexact). Covers both ^doubles PARAMS and ^doubles LET bindings.
;;
;; The tail of the file gates the OTHER primitive kinds' accesses: the direct
;; backing read (jolt-vaget) they share, and the byte store's own helper
;; (jolt-baset), which exists because that kind narrows at the store.
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
;; how many times sub occurs in s (non-overlapping)
(define (gate-count s sub)
  (let ((n (string-length s)) (m (string-length sub)))
    (let loop ((i 0) (c 0))
      (cond ((> (+ i m) n) c)
            ((string=? (substring s i (+ i m)) sub) (loop (+ i m) (+ c 1)))
            (else (loop (+ i 1) c))))))
;; A ^doubles PARAM's backing flvector is bound once at the arity's entry
;; ((_av$N (jolt-array-vec-of a)), inside the named let so a fn-level recur
;; rebinds it) and every proven read/write indexes THAT — the accessor is not
;; re-read per access. A ^doubles LET binding (row 4) keeps the accessor.
(let ((e (emit-num "(def _ (fn [^doubles a ^long i] (aget a i)))")))
  (gate-check "(1) aget ^doubles,^long idx -> inline flvector-ref on the hoisted vector" (gate-sub? e "(flvector-ref _av$") #t)
  (gate-check "(1) ...the vector is bound at entry from the param" (gate-sub? e "(jolt-array-vec-of a)") #t)
  (gate-check "(1) ...and not re-read per access" (gate-sub? e "(flvector-ref (jolt-array-vec") #f)
  (gate-check "(1) ...proven idx NOT the jolt-flaget call" (gate-sub? e "jolt-flaget") #f)
  (gate-check "(1) ...not the generic jolt-nth" (gate-sub? e "jolt-nth") #f))
(let ((e (emit-num "(def _ (fn [^doubles a ^long i] (+ (aget a i) (aget a i))))")))
  (gate-check "(2) arithmetic over ^doubles param reads -> fl+" (gate-sub? e "fl+") #t)
  (gate-check "(2) ...reading via the hoisted vector" (gate-sub? e "(flvector-ref _av$") #t))
;; shadowing: a let, a loop var or a nested fn param reusing the name reads ITS
;; array through the accessor, never the outer param's hoisted vector.
(let ((e (emit-num "(def _ (fn [^doubles a ^long i] (let [^doubles a (double-array 4)] (aget a i))))")))
  (gate-check "(2a) a ^doubles let shadowing the param reads its own array" (gate-sub? e "(flvector-ref (jolt-array-vec a)") #t)
  (gate-check "(2a) ...and not the hoisted vector" (gate-sub? e "(flvector-ref _av$") #f))
(let ((e (emit-num "(def _ (fn [^doubles a ^long i] (loop [^doubles a (double-array 4) j 0] (if (< j 1) (recur a (inc j)) (aget a i)))))")))
  ;; the numeric pass does not type a ^doubles LOOP var, so this read is the
  ;; generic one; the property is that it never reaches the hoisted vector.
  (gate-check "(2b) a loop var shadowing the param never reads the hoisted vector" (gate-sub? e "(flvector-ref _av$") #f))
(let ((e (emit-num "(def _ (fn [^doubles a ^long i] ((fn [^doubles a] (aget a i)) (double-array 4))))")))
  (gate-check "(2c) a nested fn's own ^doubles param gets its own hoist" (gate-sub? e "(flvector-ref _av$") #t)
  (gate-check "(2c) ...bound from ITS param, twice in all" (gate-count e "(jolt-array-vec-of a)") 2))
(let ((e (emit-num "(def _ (fn [a i] (aget a i)))")))
  (gate-check "(3) untyped aget stays native jolt-nth (not flaget)" (gate-sub? e "jolt-flaget") #f)
  (gate-check "(3) ...uses jolt-nth" (gate-sub? e "jolt-nth") #t))
(let ((e (emit-num "(def _ (fn [^doubles a i] (aget a i)))")))
  (gate-check "(3b) ^doubles aget, UNTYPED idx keeps jolt-flaget call" (gate-sub? e "jolt-flaget") #t)
  (gate-check "(3b) ...untyped idx NOT inlined to flvector-ref" (gate-sub? e "(flvector-ref (jolt-array-vec") #f))
(let ((e (emit-num "(def _ (fn [m ^long i] (let [^doubles a (:pixels m)] (aget a i))))")))
  (gate-check "(4) ^doubles LET-binding aget,^long idx -> inline flvector-ref" (gate-sub? e "(flvector-ref (jolt-array-vec") #t))
(let ((e (emit-num "(def _ (fn [m ^long i] (let [^doubles a (:pixels m)] (+ (aget a i) (aget a i)))))")))
  (gate-check "(5) arithmetic over ^doubles let read -> fl+" (gate-sub? e "fl+") #t))
(let ((e (emit-num "(def _ (fn [^doubles a ^long i] (aset a i 7.25)))")))
  (gate-check "(5a) aset ^doubles,^long idx,double val -> inline flvector-set! on the hoisted vector" (gate-sub? e "(flvector-set! _av$") #t)
  (gate-check "(5a) ...NOT the jolt-flaset call" (gate-sub? e "jolt-flaset") #f))
(let ((e (emit-num "(def _ (fn [m ^long i] (let [^doubles a (:pixels m)] (aset a i 7.25))))")))
  (gate-check "(5a-let) aset on a ^doubles LET binding keeps the accessor" (gate-sub? e "(flvector-set! (jolt-array-vec") #t))
(let ((e (emit-num "(def _ (fn [^doubles a ^long i] (aset a i 4)))")))
  (gate-check "(5b) aset ^doubles,int val keeps jolt-flaset (exact->inexact)" (gate-sub? e "jolt-flaset") #t))

;; --- the other primitive kinds: the direct backing read, and the byte STORE ----
;; ^longs/^ints/^bytes/^objects have no flvector to unbox, so their win is
;; skipping jolt-nth's dispatch walk (jolt-vaget) and jolt-aset3's generic seam.
;; The byte kind is the one that stores through its OWN helper: it narrows to
;; signed 8 bits and must answer what it stored, which jolt-vaset (answering its
;; argument) cannot do. Before jolt-baset existed the hint bought a byte store
;; nothing — it emitted the same jolt-aset3 an unhinted one does.
(let ((e (emit-num "(def _ (fn [^bytes a ^long i] (aset a i 7)))")))
  (gate-check "(7) aset ^bytes -> jolt-baset" (gate-sub? e "(jolt-baset a i") #t)
  (gate-check "(7) ...NOT the generic jolt-aset3" (gate-sub? e "jolt-aset3") #f)
  (gate-check "(7) ...NOT jolt-vaset, which would answer the un-narrowed value" (gate-sub? e "jolt-vaset") #f))
(let ((e (emit-num "(def _ (fn [^bytes a ^long i] (aget a i)))")))
  (gate-check "(7a) aget ^bytes stays the direct backing read" (gate-sub? e "jolt-vaget") #t))
(let ((e (emit-num "(def _ (fn [^longs a ^long i] (aset a i 7)))")))
  (gate-check "(7b) aset ^longs keeps jolt-vaset" (gate-sub? e "jolt-vaset") #t)
  (gate-check "(7b) ...and does not take the byte helper" (gate-sub? e "jolt-baset") #f))
(let ((e (emit-num "(def _ (fn [^objects a ^long i] (aset a i 7)))")))
  (gate-check "(7c) aset ^objects keeps jolt-vaset" (gate-sub? e "jolt-vaset") #t))
(let ((e (emit-num "(def _ (fn [a i] (aset a i 7)))")))
  (gate-check "(7d) an UNHINTED aset is untouched — still jolt-aset3" (gate-sub? e "jolt-aset3") #t)
  (gate-check "(7d) ...and never the byte helper" (gate-sub? e "jolt-baset") #f))
;; the bfill loop as it is actually written: the cast and the store are both
;; direct calls now, so nothing in the body goes through a var.
(let ((e (emit-num "(def _ (fn [^bytes a ^long n] (loop [i 0] (when (< i n) (aset a i (byte (bit-and i 127))) (recur (inc i))))))")))
  (gate-check "(7e) a byte-fill loop is jolt-baset over jolt-byte-cast" (gate-sub? e "(jolt-baset a i (jolt-byte-cast") #t)
  (gate-check "(7e) ...with no var-deref invoke left in it" (gate-sub? e "jolt-invoke1") #f))

;; --- runtime value semantics of jolt-flaget/jolt-flaset ---------------------------
;; Pin both index paths: the fixnum fast path (the hot case — loop counters are
;; :long) and the coercing slow path (flonum index floors via na-idx). Guards the
;; fast-path change against behavior drift: aset returns the stored value (JVM
;; contract), an int value stores as its double, and an out-of-range or negative
;; index raises (flvector-ref's range check = the array bounds contract).
(define (ev s) (jolt-compile-eval s "user"))
(gate-check "(6) aget fixnum index reads the stored double"
            (ev "(let [^doubles a (double-array 3)] (aset a 1 7.25) (aget a 1))") 7.25)
;; the hoisted vector follows a fn-level recur that passes a different array,
;; and a non-array ^doubles argument raises the JVM's checkcast on entry.
(ev "(defn flarr-recur [^doubles a ^long i] (if (< i 1) (recur (double-array 2 3.5) (inc i)) (aget a 0)))")
(gate-check "(6a) fn-level recur rebinds the hoisted vector" (ev "(flarr-recur (double-array 2 1.0) 0)") 3.5)
(gate-check "(6b) a non-array ^doubles arg raises ClassCastException on entry"
            (ev "(try (flarr-recur [1.0 2.0] 5) (catch ClassCastException e :cce))") (keyword #f "cce"))
(gate-check "(6c) a ^doubles param shadowed by a let reads the let's array"
            (ev "(let [f (fn [^doubles a ^long i] (let [^doubles a (double-array 2 9.0)] (aget a i)))] (f (double-array 2 1.0) 1))") 9.0)
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
;; ...and the unboxed paths, whose bounds check is the backing primitive's own:
;; the condition is classed AIOOBE in host-faults.ss, and its message has to be
;; the JVM's too, not Chez's "flvector-ref: 5 is not a valid index for #vfl(...)".
(gate-check "(8) ^doubles read OOB carries the JVM message"
            (ev "(try (let [^doubles a (double-array 3)] (aget a 5)) (catch ArrayIndexOutOfBoundsException e (ex-message e)))")
            "Index 5 out of bounds for length 3")
(gate-check "(8) ^doubles write OOB carries the JVM message"
            (ev "(try (let [^doubles a (double-array 3)] (aset a 7 1.0)) (catch ArrayIndexOutOfBoundsException e (ex-message e)))")
            "Index 7 out of bounds for length 3")
(gate-check "(8) ^longs read OOB carries the JVM message"
            (ev "(try (let [^longs a (long-array 2)] (aget a 4)) (catch ArrayIndexOutOfBoundsException e (ex-message e)))")
            "Index 4 out of bounds for length 2")
(gate-check "(8) ^bytes read OOB carries the JVM message"
            (ev "(try (let [^bytes a (byte-array 4)] (aget a 9)) (catch ArrayIndexOutOfBoundsException e (ex-message e)))")
            "Index 9 out of bounds for length 4")
(gate-check "(8) ^doubles read past the int range is RT.intCast's overflow"
            (ev "(try (let [^doubles a (double-array 4)] (aget a 9223372036854775807)) (catch ArithmeticException e (ex-message e)))")
            "integer overflow")
(gate-check "(8) get on an array stays non-throwing OOB (returns default)"
            (ev "(get (int-array 3) 99 :d)") (keyword #f "d"))
;; --- the array kind survives the optimizing passes ---------------------------
;; The rows above emit straight from the analyzer. A `jolt build` runs the
;; inline fixpoint first, which rebuilds let bindings: flatten hoists a
;; let-valued init, a splice replaces an invoke init. The kind used to ride on
;; the init node as a key, so a rebuilt init dropped it and every (aget a i)
;; fell back to jolt-nth. standard-clojure-style's format loop binds its kind
;; array exactly this way (an init that fills the array, then returns it).
(define run-passes (var-deref "jolt.passes" "run-passes"))
(define (emit-opt src)
  (emit (run-passes (anode src) (make-analyze-ctx "user") U)))
(set-optimize! #t)
(set-direct-link-flag! #t)
(let ((e (emit-opt "(def _ (fn [n i] (let [^longs a (let [b (long-array n)] (aset b 0 1) b)] (aget a i))))")))
  (gate-check "(9) ^longs local over a let init, flattened: still jolt-vaget" (gate-sub? e "(jolt-vaget a i)") #t)
  (gate-check "(9) ...and no jolt-nth" (gate-sub? e "jolt-nth") #f))
(let ((e (emit-opt "(def _ (fn [n i] (let [^objects a (let [b (object-array n)] b)] (fn [] (aget a i)))))")))
  (gate-check "(9a) ^objects local captured by a closure, flattened: jolt-vaget" (gate-sub? e "(jolt-vaget a i)") #t))
(let ((e (emit-opt "(def _ (fn [n ^long i] (let [^doubles a (let [b (double-array n)] b)] (+ 1.0 (aget a i)))))")))
  (gate-check "(9b) ^doubles local over a let init, flattened: unboxed read" (gate-sub? e "flvector-ref") #t))
;; A spliced callee takes its array params through the same carrier: the
;; splice binds each hinted param once, wrapped in a :coerce of its kind, as it
;; does a ^double/^long one. Before, an array-hinted fn was refused as an inline
;; candidate outright, because the copy came out on jolt-nth.
(jolt-compile-eval "(defn flarr-cell [^longs a i] (aget a i))" "user")
(jolt-compile-eval "(defn flarr-dot [^doubles a ^doubles b ^long n] (loop [i 0 s 0.0] (if (< i n) (recur (inc i) (+ s (* (aget a i) (aget b i)))) s)))" "user")
(emit-opt "(defn flarr-cell [^longs a i] (aget a i))")
(emit-opt "(defn flarr-dot [^doubles a ^doubles b ^long n] (loop [i 0 s 0.0] (if (< i n) (recur (inc i) (+ s (* (aget a i) (aget b i)))) s)))")
(let ((e (emit-opt "(defn flarr-use-1 [x j] (inc (flarr-cell x j)))")))
  (gate-check "(9c) an array-hinted callee is spliced" (gate-sub? e "flarr-cell") #f)
  (gate-check "(9c) ...and its copy reads with jolt-vaget" (gate-sub? e "jolt-vaget") #t)
  (gate-check "(9c) ...not jolt-nth" (gate-sub? e "jolt-nth") #f))
(let ((e (emit-opt "(defn flarr-use-2 [x y] (flarr-dot x y 3))")))
  (gate-check "(9d) a ^doubles loop callee is spliced" (gate-sub? e "flarr-dot") #f)
  (gate-check "(9d) ...and its copy stays unboxed" (gate-sub? e "flvector-ref") #t)
  (gate-check "(9d) ...not jolt-nth" (gate-sub? e "jolt-nth") #f))
(gate-check "(9e) the spliced copy answers what the call did"
            (ev "(let [a (double-array [1.0 2.0 3.0])] (flarr-dot a a 3))") 14.0)
;; (10) An array a constructor made is that kind of array with no hint: the
;; local bound to (double-array n) read through jolt-nth / jolt-aset3 / jolt-n+,
;; 30 ns an access where the hinted copy is 2.
(let ((e (emit-num "(def _ (fn [^long n ^long i] (let [a (double-array n)] (aset a i 1.5) (+ 1.0 (aget a i)))))")))
  (gate-check "(10) unhinted (double-array n) local: unboxed read" (gate-sub? e "flvector-ref") #t)
  (gate-check "(10) ...and unboxed write" (gate-sub? e "flvector-set!") #t)
  (gate-check "(10) ...no jolt-nth" (gate-sub? e "jolt-nth") #f)
  (gate-check "(10) ...no generic aset" (gate-sub? e "jolt-aset") #f))
(let ((e (emit-num "(def _ (fn [^long n ^long i] (let [a (float-array n)] (aget a i))))")))
  (gate-check "(10a) float-array local: the flvector read" (gate-sub? e "flvector-ref") #t))
(let ((e (emit-num "(def _ (fn [n i] (let [a (long-array n)] (aget a i))))")))
  (gate-check "(10b) long-array local: jolt-vaget" (gate-sub? e "(jolt-vaget a i)") #t))
(let ((e (emit-num "(def _ (fn [n i] (let [a (int-array n)] (aget a i))))")))
  (gate-check "(10c) int-array local: jolt-vaget" (gate-sub? e "(jolt-vaget a i)") #t))
(let ((e (emit-num "(def _ (fn [n i] (let [a (object-array n)] (aget a i))))")))
  (gate-check "(10d) object-array local: jolt-vaget" (gate-sub? e "(jolt-vaget a i)") #t))
(let ((e (emit-num "(def _ (fn [n i v] (let [a (byte-array n)] (aset a i v))))")))
  (gate-check "(10e) byte-array local: the narrowing store" (gate-sub? e "jolt-baset") #t))
(let ((e (emit-num "(def _ (fn [xs i] (let [a (double-array xs)] (+ 1.0 (aget a i)))))")))
  (gate-check "(10f) (double-array coll) is the same kind" (gate-sub? e "jolt-nth") #f))
;; a local of the same name that is NOT the constructor's result stays generic
(let ((e (emit-num "(def _ (fn [n i] (let [a (double-array n) a (vec a)] (aget a i))))")))
  (gate-check "(10g) a rebound local is not the array" (gate-sub? e "flvector-ref") #f))
;; ...and a loop that carries the array keeps the kind while every recur passes one
(let ((e (emit-num "(def _ (fn [^long n] (loop [a (double-array n) i 0] (if (< i n) (do (aset a i 2.0) (recur a (inc i))) (aget a 0)))))")))
  (gate-check "(10h) a loop-carried array local stays unboxed" (gate-sub? e "flvector-set!") #t))
;; a masked index -- (bit-and i 1023) over a :long i -- is a :long, so the access
;; is the inline flvector-ref rather than the jolt-flaget call
(let ((e (emit-num "(def _ (fn [^long n ^long i] (let [a (double-array n) j (bit-and i 1023)] (aset a j (+ (aget a j) 1.5)))))")))
  (gate-check "(10m) a bit-and of longs indexes inline" (gate-sub? e "jolt-flaget") #f)
  (gate-check "(10m) ...and stores inline" (gate-sub? e "jolt-flaset") #f))
(gate-check "(10n) values: bit-and index"
            (ev "(let [a (double-array 8) j (bit-and 13 7)] (aset a j 2.5) (aget a 5))") 2.5)
(gate-check "(10i) values: int stored, double read back"
            (ev "(let [a (double-array 3)] (aset a 1 2) (aget a 1))") 2.0)
(gate-check "(10j) values: long-array"
            (ev "(let [a (long-array 3)] (aset a 2 7) (aget a 2))") 7)
(gate-check "(10k) values: byte-array narrows"
            (ev "(let [a (byte-array 2)] (aset a 0 (unchecked-byte 200)) (aget a 0))") -56)
(let ((e (emit-opt "(def _ (fn [^long n] (let [a (double-array 1024)] (loop [i 0] (when (< i n) (let [j (bit-and i 1023)] (aset a j (+ (aget a j) 1.5))) (recur (inc i)))) (aget a 7))))")))
  (gate-check "(10l) the --opt pipeline keeps the constructed kind" (gate-sub? e "flvector-ref") #t)
  (gate-check "(10l) ...no jolt-nth" (gate-sub? e "jolt-nth") #f))
(set-direct-link-flag! #f)
(set-optimize! #f)

(gate-summary "flarr")
