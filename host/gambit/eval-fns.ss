;; eval-fns.ss — FUNCTION equivalents of seq.ss's call-position numeric macros,
;; registered into the interaction environment for runtime-eval'd compiled code.
;;
;; WHY NOT eval-syntax.ss's define-syntax approach: Gambit's eval of a
;; define-syntax needs the _define-library/define-library-expand module, which
;; a `gsc -target js -exe` bundle does not embed (Module not found at the
;; first eval). Plain defines eval fine on both targets (G0 pin), and every
;; macro in this set is a STRICT fast-path wrapper over runtime helpers that
;; are unit globals visible from eval'd code — so same-named functions are
;; semantically identical (the macro buys inlining, which the interpretation
;; path forgoes anyway). Hand-maintained against seq.ss's macro region
;; (~lines 440-612); the gambitkernel/gambiteval gates catch drift.
;;
;; Registration is (eval '(define ...) (interaction-environment)) per form —
;; eval'd, not unit-defined, so the definitions land where eval'd emitted code
;; resolves its free variables.

(define-syntax %eval-fn
  (syntax-rules ()
    ((_ form) (eval 'form (interaction-environment)))))

;; checked arithmetic: fold the 2-arg fast path (+/-/* when both are numbers,
;; else the jolt-add/sub/mul dispatch) exactly like the macro chains
(%eval-fn (define (jolt-n+ . xs)
            (cond ((null? xs) 0)
                  ((null? (cdr xs)) (jolt-add (car xs)))
                  (else (let loop ((acc (car xs)) (r (cdr xs)))
                          (if (null? r) acc
                              (loop (let ((a acc) (b (car r)))
                                      (if (and (number? a) (number? b)) (+ a b) (jolt-add a b)))
                                    (cdr r))))))))
(%eval-fn (define (jolt-n- . xs)
            (cond ((null? xs) (jolt-sub))
                  ((null? (cdr xs)) (jolt-sub (car xs)))
                  (else (let loop ((acc (car xs)) (r (cdr xs)))
                          (if (null? r) acc
                              (loop (let ((a acc) (b (car r)))
                                      (if (and (number? a) (number? b)) (- a b) (jolt-sub a b)))
                                    (cdr r))))))))
(%eval-fn (define (jolt-n* . xs)
            (cond ((null? xs) 1)
                  ((null? (cdr xs)) (jolt-mul (car xs)))
                  (else (let loop ((acc (car xs)) (r (cdr xs)))
                          (if (null? r) acc
                              (loop (let ((a acc) (b (car r)))
                                      (if (and (number? a) (number? b))
                                          (if (or (flonum? a) (flonum? b))
                                              (fl* (real->flonum a) (real->flonum b))
                                              (* a b))
                                          (jolt-mul a b)))
                                    (cdr r))))))))
(%eval-fn (define (jolt-n-div . xs)
            (cond ((null? xs) (jolt-div))
                  ((null? (cdr xs)) (jolt-div (car xs)))
                  (else (let loop ((acc (car xs)) (r (cdr xs)))
                          (if (null? r) acc (loop (jolt-div2 acc (car r)) (cdr r))))))))

;; chained comparisons: (n< a b c) = (and (n< a b) (n< b c)); operands strict
(%eval-fn (define (jolt-n< . xs)
            (or (null? xs) (null? (cdr xs))
                (let loop ((a (car xs)) (r (cdr xs)))
                  (or (null? r)
                      (and (let ((b (car r)))
                             (if (and (number? a) (number? b)) (< a b) (jolt-lt2 a b)))
                           (loop (car r) (cdr r))))))))
(%eval-fn (define (jolt-n> . xs)
            (or (null? xs) (null? (cdr xs))
                (let loop ((a (car xs)) (r (cdr xs)))
                  (or (null? r)
                      (and (let ((b (car r)))
                             (if (and (number? a) (number? b)) (> a b) (jolt-gt2 a b)))
                           (loop (car r) (cdr r))))))))
(%eval-fn (define (jolt-n<= . xs)
            (or (null? xs) (null? (cdr xs))
                (let loop ((a (car xs)) (r (cdr xs)))
                  (or (null? r)
                      (and (let ((b (car r)))
                             (if (and (number? a) (number? b)) (<= a b) (jolt-le2 a b)))
                           (loop (car r) (cdr r))))))))
(%eval-fn (define (jolt-n>= . xs)
            (or (null? xs) (null? (cdr xs))
                (let loop ((a (car xs)) (r (cdr xs)))
                  (or (null? r)
                      (and (let ((b (car r)))
                             (if (and (number? a) (number? b)) (>= a b) (jolt-ge2 a b)))
                           (loop (car r) (cdr r))))))))

(%eval-fn (define (jolt-n-min . xs)
            (cond ((null? xs) (jolt-min))
                  ((null? (cdr xs)) (jolt-min2 (car xs) (car xs)))
                  (else (let loop ((acc (car xs)) (r (cdr xs)))
                          (if (null? r) acc (loop (jolt-min2 acc (car r)) (cdr r))))))))
(%eval-fn (define (jolt-n-max . xs)
            (cond ((null? xs) (jolt-max))
                  ((null? (cdr xs)) (jolt-max2 (car xs) (car xs)))
                  (else (let loop ((acc (car xs)) (r (cdr xs)))
                          (if (null? r) acc (loop (jolt-max2 acc (car r)) (cdr r))))))))
(%eval-fn (define (jolt-n-inc a) (if (number? a) (+ a 1) (jolt-inc a))))
(%eval-fn (define (jolt-n-dec a) (if (number? a) (- a 1) (jolt-dec a))))

;; long ops: the range check calls the same unit globals the macro used
(%eval-fn (define (jolt-l-check r)
            (if (fixnum? r) r
                (if (and (>= r l-long-min) (<= r l-long-max)) r (jolt-l-overflow)))))
(%eval-fn (define (jolt-l+ a b) (jolt-l-check (+ a b))))
(%eval-fn (define (jolt-l- a . b) (jolt-l-check (if (null? b) (- a) (- a (car b))))))
(%eval-fn (define (jolt-l* a b) (jolt-l-check (* a b))))
(%eval-fn (define (jolt-l-inc a) (jolt-l-check (+ a 1))))
(%eval-fn (define (jolt-l-dec a) (jolt-l-check (- a 1))))
(%eval-fn (define (jolt-l< a b) (< a b)))
(%eval-fn (define (jolt-l> a b) (> a b)))
(%eval-fn (define (jolt-l<= a b) (<= a b)))
(%eval-fn (define (jolt-l>= a b) (>= a b)))
(%eval-fn (define (jolt-l= a b) (= a b)))
(%eval-fn (define (jolt-l-min a b) (min a b)))
(%eval-fn (define (jolt-l-max a b) (max a b)))
(%eval-fn (define (jolt-l-quot a b) (quotient a b)))
(%eval-fn (define (jolt-l-rem a b) (remainder a b)))
(%eval-fn (define (jolt-l-mod a b) (modulo a b)))
;; java.lang.Math over proven longs (jolt.passes.numeric math-lng-ops). Mirrors
;; seq.ss: floorMod reuses jolt-l-mod, floorDiv needs floor semantics, abs is the
;; generic one there too.
(%eval-fn (define (jolt-l-abs a) (abs a)))
(%eval-fn (define (jolt-floor-quotient a b)
            (let ((q (quotient a b)))
              (if (and (not (eqv? 0 (remainder a b)))
                       (if (negative? b) (positive? a) (negative? a)))
                  (- q 1)
                  q))))
