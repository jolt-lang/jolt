;; Hint-directed fast arithmetic (jolt.passes.numeric). A ^double/^long param hint
;; (or a float literal) drives Chez fl*/fx* emission instead of generic arithmetic;
;; un-hinted integer code stays generic (arbitrary-precision preserved). The pass
;; runs in run-passes with optimization OFF, so this is the open-build path. Run:
;;   chez --script test/chez/numeric-test.ss

(import (chezscheme))
(load "host/chez/rt.ss")
(set-chez-ns! "clojure.core")
(load "host/chez/seed/prelude.ss")
(load "host/chez/post-prelude.ss")
(set-chez-ns! "user")
(load "host/chez/host-contract.ss")
(load "host/chez/seed/image.ss")
(load "host/chez/compile-eval.ss")

(define total 0) (define fails 0)
(define (ok name pred) (set! total (+ total 1)) (unless pred (set! fails (+ fails 1)) (printf "FAIL: ~a\n" name)))

(define (has? s sub)
  (let ((ns (string-length s)) (nsub (string-length sub)))
    (let loop ((i 0))
      (cond ((> (+ i nsub) ns) #f)
            ((string=? (substring s i (+ i nsub)) sub) #t)
            (else (loop (+ i 1)))))))

;; analyze + run-passes (optimization OFF — the always-on numeric pass still runs)
;; + emit one form to a Scheme string.
(define (emitf ns str)
  (let-values (((f j) (rdr-read-form str 0 (string-length str))))
    (let ((ctx (make-analyze-ctx ns)))
      (jolt-ce-emit (jolt-ce-run-passes (jolt-ce-analyze ctx f) ctx)))))
(define (ev s) (jolt-compile-eval s "u"))

;; --- emission: ^double -> fl-ops, ^long -> fx-ops ---
(let ((e (emitf "u" "(fn* ([^double a ^double b] (+ (* a a) (* b b))))")))
  (ok "double + lowers to fl+" (has? e "(#3%fl+"))
  (ok "double * lowers to fl*" (has? e "(#3%fl*"))
  (ok "double arith is NOT generic +" (not (has? e "(jolt-invoke"))))

(ok "long + lowers to fx+"        (has? (emitf "u" "(fn* ([^long a ^long b] (+ a b)))") "(fx+"))
(ok "long * lowers to fx*"        (has? (emitf "u" "(fn* ([^long a ^long b] (* a b)))") "(fx*"))
(ok "double < lowers to fl<?"     (has? (emitf "u" "(fn* ([^double x] (< x 1.0)))") "(#3%fl<?"))
;; ^long comparisons / inc / dec / quot use the jolt-l* fast-path-with-fallback
;; helpers so a full 64-bit operand (past the 61-bit fixnum range) is handled.
(ok "long < lowers to jolt-l<"    (has? (emitf "u" "(fn* ([^long a ^long b] (< a b)))") "(jolt-l<"))
(ok "long inc lowers to jolt-l-inc" (has? (emitf "u" "(fn* ([^long n] (inc n)))") "(jolt-l-inc"))
(ok "double inc lowers to fl+ 1.0" (has? (emitf "u" "(fn* ([^double x] (inc x)))") "(#3%fl+"))
(ok "long dec lowers to jolt-l-dec" (has? (emitf "u" "(fn* ([^long n] (dec n)))") "(jolt-l-dec"))
;; unchecked-* WRAP to signed 64 bits (Java long), so they emit the wrapping
;; jolt-unc* helpers, not the raising fx ops.
(ok "unchecked-add lowers to jolt-uncadd2" (has? (emitf "u" "(fn* ([^long n] (unchecked-add n 1)))") "(jolt-uncadd2"))
(ok "long quot lowers to jolt-l-quot" (has? (emitf "u" "(fn* ([^long a ^long b] (quot a b)))") "(jolt-l-quot"))
(ok "double == lowers to fl=?"    (has? (emitf "u" "(fn* ([^double a ^double b] (== a b)))") "(#3%fl=?"))

;; integer literal in a double op is coerced to a flonum (fl+ never sees an exact int)
(let ((e (emitf "u" "(fn* ([^double x] (+ x 1)))")))
  (ok "double op with int literal coerces it to 1.0" (and (has? e "(#3%fl+") (has? e "1.0"))))

;; let init kind propagates: d is double from (* x x)
(let ((e (emitf "u" "(fn* ([^double x] (let [d (* x x)] (+ d 1.0))))")))
  (ok "let-bound double propagates (fl* then fl+)" (and (has? e "(#3%fl*") (has? e "(#3%fl+"))))

;; --- loop-carried variable typing (round 2) ---
;; a double accumulator types via fixpoint, so its recur arithmetic is fl-ops.
(let ((e (emitf "u" "(fn* ([] (loop [acc 0.0 i 0] (if (< i 5) (recur (+ acc 1.5) (inc i)) acc))))")))
  (ok "loop double accumulator lowers (+ acc 1.5) to fl+" (has? e "(#3%fl+")))
;; a literal-init integer accumulator is now :long-typed (its init fits the fixnum
;; range), so (* acc i) -> fx* — which raises on overflow rather than promoting.
(let ((e (emitf "u" "(fn* ([] (loop [acc 1 i 1] (if (< i 25) (recur (* acc i) (inc i)) acc))))")))
  (ok "loop integer accumulator IS fx-specialized" (has? e "(fx*")))
;; a literal-init increment counter is now :long-typed — JVM loop semantics: a
;; literal-init loop var is a primitive long (when the literal fits the fixnum
;; range), so (inc i) -> jolt-l-inc and the counter's compare -> jolt-l<.
(let ((e (emitf "u" "(fn* ([] (loop [i 0] (if (< i 5) (recur (inc i)) i))))")))
  (ok "literal-init increment counter IS long-typed" (has? e "(jolt-l-inc"))
  (ok "literal-init counter compare lowers to jolt-l<" (has? e "(jolt-l<")))
;; a NEGATIVE literal-init loop var is also :long-typed: the fixnum range is
;; asymmetric ([-2^60, 2^60-1]), so a negative int literal in range seeds :long and
;; (inc i) -> jolt-l-inc exactly as a positive literal does. Guards fixnum-min, which
;; was once (- fixnum-max (inc fixnum-max)) = -1 and silently kept negative inits generic.
(let ((e (emitf "u" "(fn* ([] (loop [i -5] (if (< i 0) (recur (inc i)) i))))")))
  (ok "negative literal-init counter IS long-typed" (has? e "(jolt-l-inc"))
  (ok "negative literal-init counter compare lowers to jolt-l<" (has? e "(jolt-l<"))
  ;; runtime via a BARE top-level loop (an immediate `((fn* …))` call wraps in
  ;; jolt-invoke0 and de-specializes; a bare loop seeds :long and exercises the path).
  (ok "negative literal-init counter runtime: counts -5..0 to 0"
      (= 0 (jnum->exact (ev "(loop [i -5] (if (< i 0) (recur (inc i)) i))")))))
;; a literal-init multiplicative accumulator (and its counter) are now :long too:
;; (* acc i) -> fx*, which RAISES on overflow rather than promoting to bignum.
(let ((e (emitf "u" "(fn* ([] (loop [acc 1 i 0] (if (< i 100) (recur (* acc i) (inc i)) acc))))")))
  (ok "counter beside a * accumulator: counter IS long-typed" (has? e "(jolt-l-inc"))
  (ok "the * accumulator IS fx-specialized" (has? e "(fx*")))
;; an immediately-INVOKED ((fn* ([] (loop …)))) now specializes too: an-invoke descends
;; into its :fn child (the loop lives there), so the literal-init slots seed :long and
;; (* acc i) -> fx*. This was the WP-B fixpoint gap — a 2-var accumulator loop wrapped in
;; a ((fn* …)) stayed generic while the bare/non-invoked form specialized.
(let ((e (emitf "u" "((fn* ([] (loop [acc 1 i 1] (if (< i 100) (recur (* acc i) (inc i)) acc)))))")))
  (ok "INVOKED 2-var accumulator: the * IS fx-specialized (acc1 i1, exact failing shape)" (has? e "(fx*"))
  (ok "INVOKED 2-var accumulator: the counter IS jolt-l-inc" (has? e "(jolt-l-inc")))
(let ((e (emitf "u" "((fn* ([] (loop [acc 2 i 1] (if (< i 100) (recur (* acc i) (inc i)) acc)))))")))
  (ok "INVOKED 2-var accumulator: the * IS fx-specialized (acc2 i1, the discriminating probe)" (has? e "(fx*")))
;; a literal-init accumulator whose product leaves the fixnum range throws a CATCHABLE
;; ArithmeticException (JVM loop semantics: a primitive-long loop var overflows rather
;; than promoting to bignum). Asserted with a jolt-level try/catch INSIDE the evaluated
;; form: the raw fx condition converts to ArithmeticException on jolt's normal exception
;; path, but escapes a Scheme-level (guard ...) around ev — the harness's eval runs the
;; compiled body outside the converter, so an outer guard never unwinds (process exit).
(ok "literal-init * accumulator overflow throws catchable ArithmeticException"
    (eq? (ev "(try (loop [acc 1 i 1] (if (< i 100) (recur (* acc i) (inc i)) acc)) (catch ArithmeticException e :overflow))")
         (keyword #f "overflow")))
;; same loop wrapped in an immediately-invoked ((fn* …)) now ALSO overflows — before the
;; an-invoke fix this stayed generic, promoted to bignum, and returned 99!.
(ok "INVOKED * accumulator overflow throws (exact failing shape)"
    (eq? (ev "(try ((fn* ([] (loop [acc 1 i 1] (if (< i 100) (recur (* acc i) (inc i)) acc))))) (catch ArithmeticException e :overflow))")
         (keyword #f "overflow")))
;; the discriminating probe shape: same structure, acc seeded 2 instead of 1. Same result.
(ok "INVOKED * accumulator overflow throws (acc2 i1 probe shape)"
    (eq? (ev "(try ((fn* ([] (loop [acc 2 i 1] (if (< i 100) (recur (* acc i) (inc i)) acc))))) (catch ArithmeticException e :overflow))")
         (keyword #f "overflow")))
(ok "increment counter runtime: counts to 1000"
    (= 1000 (jnum->exact (ev "((fn* ([] (loop [i 0] (if (< i 1000) (recur (inc i)) i)))))"))))
;; a literal-init loop var beside a BIGNUM literal operand stays generic: the fx ops
;; take only fixnums, so a bignum-literal operand blocks :long and arbitrary precision
;; is preserved ((+ 5 Long/MAX) is a bignum, exactly as (let [i 5] ...) gives).
(let ((e (emitf "u" "(fn* ([] (loop [i 5] (+ i 9223372036854775807))))")))
  (ok "loop var + bignum literal: the + is NOT fx-typed" (not (has? e "(fx+"))))
(ok "loop var + bignum literal stays exact (bignum)"
    (jolt-truthy? (ev "(< 9223372036854775807 ((fn* ([] (loop [i 5] (+ i 9223372036854775807))))))")))
;; overflow near the fixnum max throws catchably (host-pinned: the JVM overflows a long
;; at 2^63, not jolt's 2^60, so this is not a corpus row).
(ok "literal-init counter overflow near fixnum max throws catchable ArithmeticException"
    (eq? (ev "(try (loop [i 1152921504606846974 k 0] (if (= k 2) i (recur (inc i) (inc k)))) (catch ArithmeticException e :overflow))")
         (keyword #f "overflow")))

;; a ^long-seeded loop accumulator IS fx-typed (the hint is a fixnum promise, and
;; the value flows from a coerced ^long param).
(let ((e (emitf "u" "(fn* ([^long start] (loop [acc start] (if (< acc 100) (recur (inc acc)) acc))))")))
  (ok "long-seeded loop accumulator lowers (inc acc) to jolt-l-inc" (has? e "(jolt-l-inc"))
  (ok "long-seeded loop comparison lowers to jolt-l<" (has? e "(jolt-l<")))

;; --- soundness: un-hinted / integer-literal code stays generic ---
(let ((e (emitf "u" "(fn* ([a b] (+ a b)))")))
  (ok "un-hinted + stays generic (no fl/fx)" (and (not (has? e "(#3%fl+")) (not (has? e "(fx+")))))
(let ((e (emitf "u" "(+ 1 2)")))
  (ok "bare integer literals stay generic (arbitrary precision)" (not (has? e "(fx+"))))
;; a constant float op like (+ 1.0 2.0) is const-folded to 3.0 (no op at all); a
;; float-literal-bound local is double-typed and its body op isn't foldable (a
;; local operand), so numeric specializes it.
(ok "float-literal-bound local specializes to fl+"
    (has? (emitf "u" "(fn* ([] (let [a 2.0] (+ a 3.0))))") "(#3%fl+"))
;; (/ ^long ^long) is a Ratio in Clojure, not a long -> must NOT lower to a fixnum op
(let ((e (emitf "u" "(fn* ([^long a ^long b] (/ a b)))")))
  (ok "long division is NOT specialized (stays generic /)" (not (has? e "(fx"))))

;; --- runtime values match the generic result ---
(ok "double dot: 3^2+4^2 = 25" (= 25 (jnum->exact (ev "((fn* ([^double a ^double b] (+ (* a a) (* b b)))) 3.0 4.0)"))))
(ok "long sum: 2+3 = 5"        (= 5  (jnum->exact (ev "((fn* ([^long a ^long b] (+ a b))) 2 3)"))))
(ok "double compare true"      (jolt-truthy? (ev "((fn* ([^double x] (< x 5.0))) 3.0)")))
(ok "double unary negate"      (= -5 (jnum->exact (ev "((fn* ([^double x] (- x))) 5.0)"))))
(ok "long unary negate"        (= -5 (jnum->exact (ev "((fn* ([^long a] (- a))) 5)"))))
(ok "long quot 7/2 = 3"        (= 3  (jnum->exact (ev "((fn* ([^long a ^long b] (quot a b))) 7 2)"))))
(ok "double + int literal = 4.5" (= 9 (jnum->exact (ev "((fn* ([^double x] (* (+ x 1) 2))) 3.5)"))))
(ok "loop double accumulator: 10*1.5 = 15"
    (= 15 (jnum->exact (ev "((fn* ([] (loop [acc 0.0 i 0] (if (< i 10) (recur (+ acc 1.5) (inc i)) acc)))))"))))
;; JVM loop semantics: a literal-init factorial loop is primitive-long arithmetic and
;; overflows past the fixnum range (this asserted bignum preservation before literal
;; inits seeded :long; a bignum-preserving loop now needs a non-literal generic init).
(ok "loop integer factorial overflows (literal-init loop vars are primitive longs)"
    (eq? (ev "(try ((fn* ([] (loop [acc 1 i 1] (if (< i 25) (recur (* acc i) (inc i)) acc))))) (catch ArithmeticException e :overflow))")
         (keyword #f "overflow")))
;; the bignum-preserving path still exists: an init the pass can't prove (via identity)
;; keeps the loop generic and the product exact.
(ok "factorial with unproven init stays exact (bignum preserved)"
    (jolt-truthy? (ev "(< 1000000000000000000000 ((fn* ([] (loop [acc (identity 1) i 1] (if (< i 25) (recur (* acc i) (inc i)) acc)))) ))")))
(ok "long-seeded loop accumulator counts to 100"
    (= 100 (jnum->exact (ev "((fn* ([^long start] (loop [acc start] (if (< acc 100) (recur (inc acc)) acc)))) 0)"))))

;; --- numeric return hints (round 3) ---
;; a ^double / ^long return hint coerces the fn's value on the way out (the contract).
(jolt-compile-eval "(def ^double dsq (fn* ([x] (* x x))))" "u")
(ok "^double return coerces value to a flonum" (flonum? (ev "(dsq 3)")))
(jolt-compile-eval "(def ^long ldbl (fn* ([x] (+ x x))))" "u")
(ok "^long return coerces value to a fixnum" (let ((r (ev "(ldbl 5)"))) (and (fixnum? r) (= r 10))))
;; the defn macro must carry the name's return hint through to the def.
(jolt-compile-eval "(defn ^double dnsq [x] (* x x))" "u")
(ok "defn ^double return coerces to flonum" (flonum? (ev "(dnsq 4)")))

;; caller propagation: a call to a ^double-returning fn types an accumulator over it.
(let ((e (emitf "u" "(fn* ([] (loop [acc 0.0 i 0] (if (< i 3) (recur (+ acc (dsq 2.0)) (inc i)) acc))))")))
  (ok "accumulator over a ^double-returning call lowers to fl+" (has? e "(#3%fl+")))
(ok "accumulator over ^double call runtime: 3 * (2*2) = 12.0"
    (= 12 (jnum->exact (ev "((fn* ([] (loop [acc 0.0 i 0] (if (< i 3) (recur (+ acc (dsq 2.0)) (inc i)) acc)))))"))))
;; a ^double call result also specializes a straight-line op
(let ((e (emitf "u" "(fn* ([^double y] (+ y (dsq 2.0))))")))
  (ok "straight-line op over ^double call lowers to fl+" (has? e "(#3%fl+")))

;; --- Part 1 (jolt-30q9): (double x)/(long x)/(int x)/(float x) casts ---
;; A non-shadowed clojure.core cast becomes a :coerce node carrying the checked
;; runtime helper, so it feeds the numeric lattice like a ^double/^long hint:
;; (* (double x) 2.0) emits fl*, (+ (long x) 1) emits fx+.

;; (double x) in arithmetic yields fl-ops AND the checked helper.
(let ((e (emitf "u" "(fn* ([x] (* (double x) 2.0)))")))
  (ok "(double x) operand lowers * to fl*" (has? e "(#3%fl*"))
  (ok "(double x) lowers to jolt-double helper" (has? e "(jolt-double")))
;; (long x) in arithmetic yields fx-ops AND the checked helper.
(let ((e (emitf "u" "(fn* ([x] (+ (long x) 1)))")))
  (ok "(long x) operand lowers + to fx+" (has? e "(fx+"))
  (ok "(long x) lowers to jolt-long-cast helper" (has? e "(jolt-long-cast")))
;; (int x) is long-kind (feeds fx) but routes to jolt-int-cast (JVM int range).
(let ((e (emitf "u" "(fn* ([x] (+ (int x) 1)))")))
  (ok "(int x) operand lowers + to fx+" (has? e "(fx+"))
  (ok "(int x) lowers to jolt-int-cast helper" (has? e "(jolt-int-cast")))
;; (float x) is double-kind but routes to jolt-float (Float range check).
(let ((e (emitf "u" "(fn* ([x] (* (float x) 2.0)))")))
  (ok "(float x) operand lowers * to fl*" (has? e "(#3%fl*"))
  (ok "(float x) lowers to jolt-float helper" (has? e "(jolt-float")))
;; a (double x) accumulator loop runs on fl+ (the headline use case).
(let ((e (emitf "u" "(fn* ([f] (loop [acc (double 0) i 0] (if (< i 3) (recur (+ acc (double (f i))) (inc i)) acc))))")))
  (ok "(double x) accumulator loop lowers to fl+" (has? e "(#3%fl+")))
;; a shadowing local named `double` does NOT trigger the cast: (double double)
;; is a call to the local fn, emitting a normal invoke (no jolt-double helper).
(let ((e (emitf "u" "(fn* ([double] (+ (double double) 1)))")))
  (ok "shadowing local `double` does NOT lower to jolt-double" (not (has? e "(jolt-double"))))
;; a clojure.core-qualified cast (from syntax-quote) also specializes.
(let ((e (emitf "u" "(fn* ([x] (* (clojure.core/double x) 2.0)))")))
  (ok "clojure.core/double operand lowers * to fl*" (has? e "(#3%fl*")))

;; --- cast runtime semantics (JVM-certified corpus rows) ---
(ok "(double 5) => 5.0 flonum" (let ((r (ev "(double 5)"))) (and (flonum? r) (fl= r 5.0))))
(ok "(double 1/2) => 0.5" (fl= (ev "(double 1/2)") 0.5))
(ok "(double 5M) => 5.0 flonum (bigdec->double)" (let ((r (ev "(double 5M)"))) (and (flonum? r) (fl= r 5.0))))
(ok "(double \"s\") throws" (guard (e (#t #t)) (ev "(double \"s\")") #f))
(ok "(double nil) throws" (guard (e (#t #t)) (ev "(double nil)") #f))
(ok "(long 1.5) => 1 (truncate toward zero)" (= (ev "(long 1.5)") 1))
(ok "(long -1.5) => -1 (truncate toward zero)" (= (ev "(long -1.5)") -1))
(ok "(long ##NaN) => 0 (JVM (long)NaN)" (= (ev "(long ##NaN)") 0))
(ok "(long ##Inf) throws (out of range)" (guard (e (#t #t)) (ev "(long ##Inf)") #f))
(ok "(long 5/2) => 2 (ratio truncate)" (= (ev "(long 5/2)") 2))
(ok "(long \"s\") throws" (guard (e (#t #t)) (ev "(long \"s\")") #f))
(ok "(int 5.7) => 5" (= (ev "(int 5.7)") 5))
(ok "(int -5.7) => -5" (= (ev "(int -5.7)") -5))
;; a cast result composes with arithmetic at runtime.
(ok "(* (double 3) 2.0) => 6.0" (fl= (ev "(* (double 3) 2.0)") 6.0))
(ok "(+ (long 7.9) 1) => 8" (= (ev "(+ (long 7.9) 1)") 8))

;; --- Part 2 (jolt-isgk): bigdec call-position contagion ---
;; JVM: (+ 1.5M 2.0) => 3.5 Double (flonum wins); (+ 1.5M 1) => 2.5M bigdec.
(ok "(+ 1.5M 2.0) => 3.5 Double contagion"
    (let ((r (ev "(+ 1.5M 2.0)"))) (and (flonum? r) (fl= r 3.5))))
(ok "(+ 1.5M 1) => 2.5M bigdec"
    (let ((r (ev "(+ 1.5M 1)"))) (and (jbigdec? r) (jolt= r (ev "2.5M")))))
(ok "(* 1.5M 2.0) => 3.0 Double contagion"
    (let ((r (ev "(* 1.5M 2.0)"))) (and (flonum? r) (fl= r 3.0))))
(ok "(- 1.5M 2.0) => -0.5 Double contagion"
    (let ((r (ev "(- 1.5M 2.0)"))) (and (flonum? r) (fl= r -0.5))))
;; SPECIALIZATION (the Part 2 coverage goal): a static bigdec + a :double operand
;; lowers to the flonum path (bigdec coerced to flonum), not the generic jolt op.
(let ((e (emitf "u" "(fn* ([^double y] (+ 1.5M y)))")))
  (ok "bigdec+double operand lowers + to fl+" (has? e "(#3%fl+")))
;; a statically-bigdec operand mixed with an untyped (:any) value routes through
;; the generic bigdec-aware jolt op, not the raw Chez op (de-opt to correct).
(ok "(+ 1.5M x) with x untyped => 4.5M bigdec"
    (let ((r ((ev "(fn* ([x] (+ 1.5M x)))") 3))) (and (jbigdec? r) (jolt= r (ev "4.5M")))))

(printf "~a/~a passed~n" (- total fails) total)
(exit (if (zero? fails) 0 1))
