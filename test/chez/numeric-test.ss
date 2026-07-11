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
  (ok "double + lowers to fl+" (has? e "(fl+"))
  (ok "double * lowers to fl*" (has? e "(fl*"))
  (ok "double arith is NOT generic +" (not (has? e "(jolt-invoke"))))

(ok "long + lowers to fx+"        (has? (emitf "u" "(fn* ([^long a ^long b] (+ a b)))") "(fx+"))
(ok "long * lowers to fx*"        (has? (emitf "u" "(fn* ([^long a ^long b] (* a b)))") "(fx*"))
(ok "double < lowers to fl<?"     (has? (emitf "u" "(fn* ([^double x] (< x 1.0)))") "(fl<?"))
;; ^long comparisons / inc / dec / quot use the jolt-l* fast-path-with-fallback
;; helpers so a full 64-bit operand (past the 61-bit fixnum range) is handled.
(ok "long < lowers to jolt-l<"    (has? (emitf "u" "(fn* ([^long a ^long b] (< a b)))") "(jolt-l<"))
(ok "long inc lowers to jolt-l-inc" (has? (emitf "u" "(fn* ([^long n] (inc n)))") "(jolt-l-inc"))
(ok "double inc lowers to fl+ 1.0" (has? (emitf "u" "(fn* ([^double x] (inc x)))") "(fl+"))
(ok "long dec lowers to jolt-l-dec" (has? (emitf "u" "(fn* ([^long n] (dec n)))") "(jolt-l-dec"))
;; unchecked-* WRAP to signed 64 bits (Java long), so they emit the wrapping
;; jolt-unc* helpers, not the raising fx ops.
(ok "unchecked-add lowers to jolt-uncadd2" (has? (emitf "u" "(fn* ([^long n] (unchecked-add n 1)))") "(jolt-uncadd2"))
(ok "long quot lowers to jolt-l-quot" (has? (emitf "u" "(fn* ([^long a ^long b] (quot a b)))") "(jolt-l-quot"))
(ok "double == lowers to fl=?"    (has? (emitf "u" "(fn* ([^double a ^double b] (== a b)))") "(fl=?"))

;; integer literal in a double op is coerced to a flonum (fl+ never sees an exact int)
(let ((e (emitf "u" "(fn* ([^double x] (+ x 1)))")))
  (ok "double op with int literal coerces it to 1.0" (and (has? e "(fl+") (has? e "1.0"))))

;; let init kind propagates: d is double from (* x x)
(let ((e (emitf "u" "(fn* ([^double x] (let [d (* x x)] (+ d 1.0))))")))
  (ok "let-bound double propagates (fl* then fl+)" (and (has? e "(fl*") (has? e "(fl+"))))

;; --- loop-carried variable typing (round 2) ---
;; a double accumulator types via fixpoint, so its recur arithmetic is fl-ops.
(let ((e (emitf "u" "(fn* ([] (loop [acc 0.0 i 0] (if (< i 5) (recur (+ acc 1.5) (inc i)) acc))))")))
  (ok "loop double accumulator lowers (+ acc 1.5) to fl+" (has? e "(fl+")))
;; an integer accumulator stays generic — a bignum-producing loop keeps arbitrary
;; precision (no fx* overflow).
(let ((e (emitf "u" "(fn* ([] (loop [acc 1 i 1] (if (< i 25) (recur (* acc i) (inc i)) acc))))")))
  (ok "loop integer accumulator is NOT fx-specialized" (not (has? e "(fx*"))))
;; a literal-init increment counter stays generic — :long flows only from a ^long
;; hint, never a bare int literal, so an un-hinted integer counter keeps arbitrary
;; precision (no fixnum overflow surprise).
(let ((e (emitf "u" "(fn* ([] (loop [i 0] (if (< i 5) (recur (inc i)) i))))")))
  (ok "literal-init increment counter is NOT fx/long-typed" (and (not (has? e "(fx")) (not (has? e "(jolt-l-inc")))))
;; a multiplicative accumulator in the SAME loop also stays generic (bignum-safe).
(let ((e (emitf "u" "(fn* ([] (loop [acc 1 i 0] (if (< i 100) (recur (* acc i) (inc i)) acc))))")))
  (ok "counter beside a * accumulator: counter is NOT long-typed" (not (has? e "(jolt-l-inc")))
  (ok "the * accumulator is NOT fx-specialized (bignum-safe)" (not (has? e "(fx*"))))
(ok "counter+bignum-accumulator stays exact (1*2*...*99 is a bignum)"
    (jolt-truthy? (ev "(< 1000000000000000000000 ((fn* ([] (loop [acc 1 i 1] (if (< i 100) (recur (* acc i) (inc i)) acc))))))")))
(ok "increment counter runtime: counts to 1000"
    (= 1000 (jnum->exact (ev "((fn* ([] (loop [i 0] (if (< i 1000) (recur (inc i)) i)))))"))))
;; a recur-less loop is a let: its int-literal binding stays generic (no fx), so
;; arbitrary precision is preserved (matches (let [i 5] ...)).
(let ((e (emitf "u" "(fn* ([] (loop [i 5] (+ i 9223372036854775807))))")))
  (ok "recur-less loop int binding is NOT fx-typed" (not (has? e "(fx+"))))
(ok "recur-less loop with a big add stays exact (bignum)"
    (jolt-truthy? (ev "(< 9223372036854775807 ((fn* ([] (loop [i 5] (+ i 9223372036854775807))))))")))

;; a ^long-seeded loop accumulator IS fx-typed (the hint is a fixnum promise, and
;; the value flows from a coerced ^long param).
(let ((e (emitf "u" "(fn* ([^long start] (loop [acc start] (if (< acc 100) (recur (inc acc)) acc))))")))
  (ok "long-seeded loop accumulator lowers (inc acc) to jolt-l-inc" (has? e "(jolt-l-inc"))
  (ok "long-seeded loop comparison lowers to jolt-l<" (has? e "(jolt-l<")))

;; --- soundness: un-hinted / integer-literal code stays generic ---
(let ((e (emitf "u" "(fn* ([a b] (+ a b)))")))
  (ok "un-hinted + stays generic (no fl/fx)" (and (not (has? e "(fl+")) (not (has? e "(fx+")))))
(let ((e (emitf "u" "(+ 1 2)")))
  (ok "bare integer literals stay generic (arbitrary precision)" (not (has? e "(fx+"))))
;; a constant float op like (+ 1.0 2.0) is const-folded to 3.0 (no op at all); a
;; float-literal-bound local is double-typed and its body op isn't foldable (a
;; local operand), so numeric specializes it.
(ok "float-literal-bound local specializes to fl+"
    (has? (emitf "u" "(fn* ([] (let [a 2.0] (+ a 3.0))))") "(fl+"))
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
(ok "loop integer factorial stays exact (bignum preserved)"
    (jolt-truthy? (ev "(< 1000000000000000000000 ((fn* ([] (loop [acc 1 i 1] (if (< i 25) (recur (* acc i) (inc i)) acc)))) ))")))
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
  (ok "accumulator over a ^double-returning call lowers to fl+" (has? e "(fl+")))
(ok "accumulator over ^double call runtime: 3 * (2*2) = 12.0"
    (= 12 (jnum->exact (ev "((fn* ([] (loop [acc 0.0 i 0] (if (< i 3) (recur (+ acc (dsq 2.0)) (inc i)) acc)))))"))))
;; a ^double call result also specializes a straight-line op
(let ((e (emitf "u" "(fn* ([^double y] (+ y (dsq 2.0))))")))
  (ok "straight-line op over ^double call lowers to fl+" (has? e "(fl+")))

;; --- Part 1 (jolt-30q9): (double x)/(long x)/(int x)/(float x) casts ---
;; A non-shadowed clojure.core cast becomes a :coerce node carrying the checked
;; runtime helper, so it feeds the numeric lattice like a ^double/^long hint:
;; (* (double x) 2.0) emits fl*, (+ (long x) 1) emits fx+.

;; (double x) in arithmetic yields fl-ops AND the checked helper.
(let ((e (emitf "u" "(fn* ([x] (* (double x) 2.0)))")))
  (ok "(double x) operand lowers * to fl*" (has? e "(fl*"))
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
  (ok "(float x) operand lowers * to fl*" (has? e "(fl*"))
  (ok "(float x) lowers to jolt-float helper" (has? e "(jolt-float")))
;; a (double x) accumulator loop runs on fl+ (the headline use case).
(let ((e (emitf "u" "(fn* ([f] (loop [acc (double 0) i 0] (if (< i 3) (recur (+ acc (double (f i))) (inc i)) acc))))")))
  (ok "(double x) accumulator loop lowers to fl+" (has? e "(fl+")))
;; a shadowing local named `double` does NOT trigger the cast: (double double)
;; is a call to the local fn, emitting a normal invoke (no jolt-double helper).
(let ((e (emitf "u" "(fn* ([double] (+ (double double) 1)))")))
  (ok "shadowing local `double` does NOT lower to jolt-double" (not (has? e "(jolt-double"))))
;; a clojure.core-qualified cast (from syntax-quote) also specializes.
(let ((e (emitf "u" "(fn* ([x] (* (clojure.core/double x) 2.0)))")))
  (ok "clojure.core/double operand lowers * to fl*" (has? e "(fl*")))

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
  (ok "bigdec+double operand lowers + to fl+" (has? e "(fl+")))
;; a statically-bigdec operand mixed with an untyped (:any) value routes through
;; the generic bigdec-aware jolt op, not the raw Chez op (de-opt to correct).
(ok "(+ 1.5M x) with x untyped => 4.5M bigdec"
    (let ((r ((ev "(fn* ([x] (+ 1.5M x)))") 3))) (and (jbigdec? r) (jolt= r (ev "4.5M")))))

(printf "~a/~a passed~n" (- total fails) total)
(exit (if (zero? fails) 0 1))
