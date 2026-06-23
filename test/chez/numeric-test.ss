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

;; --- emission: ^double -> fl-ops, ^long -> fx-ops ---
(let ((e (emitf "u" "(fn* ([^double a ^double b] (+ (* a a) (* b b))))")))
  (ok "double + lowers to fl+" (has? e "(fl+"))
  (ok "double * lowers to fl*" (has? e "(fl*"))
  (ok "double arith is NOT generic +" (not (has? e "(jolt-invoke"))))

(ok "long + lowers to fx+"        (has? (emitf "u" "(fn* ([^long a ^long b] (+ a b)))") "(fx+"))
(ok "long * lowers to fx*"        (has? (emitf "u" "(fn* ([^long a ^long b] (* a b)))") "(fx*"))
(ok "double < lowers to fl<?"     (has? (emitf "u" "(fn* ([^double x] (< x 1.0)))") "(fl<?"))
(ok "long < lowers to fx<?"       (has? (emitf "u" "(fn* ([^long a ^long b] (< a b)))") "(fx<?"))
(ok "long inc lowers to fx1+"     (has? (emitf "u" "(fn* ([^long n] (inc n)))") "(fx1+"))
(ok "double inc lowers to fl+ 1.0" (has? (emitf "u" "(fn* ([^double x] (inc x)))") "(fl+"))
(ok "long dec lowers to fx1-"     (has? (emitf "u" "(fn* ([^long n] (dec n)))") "(fx1-"))
(ok "unchecked-add lowers to fx+" (has? (emitf "u" "(fn* ([^long n] (unchecked-add n 1)))") "(fx+"))
(ok "long quot lowers to fxquotient" (has? (emitf "u" "(fn* ([^long a ^long b] (quot a b)))") "(fxquotient"))
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
(define (ev s) (jolt-compile-eval s "u"))
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

(printf "~a/~a passed~n" (- total fails) total)
(exit (if (zero? fails) 0 1))
