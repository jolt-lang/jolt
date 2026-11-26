;; vfasl-negative-bignum-test.ss — negative bignum constants keep their value in
;; a vfasl image (host/chez/chez-patches/0012-vfasl-negative-bignum.patch).
;;
;; Stock Chez 10.4.1's vfasl writer (s/vfasl.ss exact-integer-copy) filled a
;; negative bignum's bigits from the two's-complement bits of the signed value
;; and sized it by integer-length of the signed value, so every negative bignum
;; constant came back as -(2^(32k) - |v|): -1152921504606846977 (-2^60-1, the
;; first negative long that is not a fixnum) loaded as -17293822569102704639.
;; -2^63 survived only by coincidence (its low 64 two's-complement bits are its
;; own magnitude). Plain compile-file/load was always right; `jolt build`
;; converts every app unit and the boot to vfasl, so a built binary printed the
;; wrong number for a literal that `jolt -M` printed right.
;;
;; The rows compile one unit holding the constants, convert it the way build.ss
;; converts an app unit, and compare the plain fasl and the vfasl image against
;; values computed at run time (never constants, so the expectations cannot be
;; corrupted the same way). They cover both signs, every bigit-boundary size
;; around 2^60, 2^63, 2^64, 2^96, 2^128, and bignums inside a ratnum, an
;; exactnum and a vector.
;;
;; The vfasl rows need the patched kernel. On a stock one they report and pass,
;; unless JOLT_REQUIRE_LOCKFREE_CHEZ=1 says the kernel must carry jolt's patches.
;;
;;   chez --script test/chez/vfasl-negative-bignum-test.ss
(import (chezscheme))

(define total 0) (define fails 0)
(define (ok name pred)
  (set! total (+ total 1))
  (if pred (printf "PASS: ~a\n" name)
      (begin (set! fails (+ fails 1)) (printf "FAIL: ~a\n" name))))

(define tmp "target/vfasl-negative-bignum-gate")
(unless (file-exists? "target") (mkdir "target"))
(unless (file-exists? tmp) (mkdir tmp))
(define (at name) (string-append tmp "/" name))

(define src (at "probe.ss"))
(define so (at "probe.so"))
(define vso (at "probe.vso"))
(for-each (lambda (f) (when (file-exists? f) (delete-file f))) (list src so vso))

;; Each entry: a label and an expression the probe unit evaluates at read time
;; (so the unit holds the result as a literal constant) and the gate evaluates
;; at run time.
(define cases
  '(("-2^60-1" (- (- (expt 2 60)) 1))
    ("-7046029254386353131" (- 7046029254386353131))
    ("-2^63+1" (+ (- (expt 2 63)) 1))
    ("-2^63" (- (expt 2 63)))
    ("-2^63-1" (- (- (expt 2 63)) 1))
    ("-2^64+1" (+ (- (expt 2 64)) 1))
    ("-2^64" (- (expt 2 64)))
    ("-2^64-5" (- (- (expt 2 64)) 5))
    ("-2^96" (- (expt 2 96)))
    ("-2^128-12345" (- (- (expt 2 128)) 12345))
    ("2^60" (expt 2 60))
    ("2^63" (expt 2 63))
    ("2^64+5" (+ (expt 2 64) 5))
    ("-(2^64+5)/3" (/ (- (+ (expt 2 64) 5)) 3))
    ("-(2^61+1)/(2^62+3)" (/ (- (+ (expt 2 61) 1)) (+ (expt 2 62) 3)))
    ("-2^70+3i" (make-rectangular (- (expt 2 70)) 3))
    ("vector of negatives" (vector (- (- (expt 2 60)) 1) (- (expt 2 80)) -5))))

(define (case-name i) (string->symbol (format "vnb-~a" i)))

(define (case-value c) (eval (cadr c)))

(with-output-to-file src
  (lambda ()
    (let loop ([cs cases] [i 0])
      (unless (null? cs)
        ;; the value goes into the source as a quoted literal
        (write `(define ,(case-name i) ',(case-value (car cs))))
        (newline)
        (loop (cdr cs) (+ i 1))))))

(parameterize ((current-output-port (open-output-string)))
  (compile-file src so))
(vfasl-convert-file so vso #f)

(define (check-all label)
  (let loop ([cs cases] [i 0] [bad '()])
    (if (null? cs)
        (reverse bad)
        (let* ([want (case-value (car cs))]
               [got (top-level-value (case-name i))])
          (loop (cdr cs) (+ i 1)
                (if (equal? want got) bad (cons (list (caar cs) got) bad)))))))

(define (report label bad)
  (for-each (lambda (b) (printf "  ~a: ~a loaded as ~s\n" label (car b) (cadr b))) bad))

;; --- the plain fasl: always right ------------------------------------------
(load so)
(let ([bad (check-all "fasl")])
  (report "fasl" bad)
  (ok "plain fasl: every constant keeps its value" (null? bad)))

;; --- the vfasl image ---------------------------------------------------------
(load vso)
(define patched? (foreign-entry? "(cs)lockfree_activation"))
(define required? (equal? (getenv "JOLT_REQUIRE_LOCKFREE_CHEZ") "1"))
(let ([bad (check-all "vfasl")])
  (report "vfasl" bad)
  (cond
    ((or patched? required?)
     (ok "vfasl: every constant keeps its value" (null? bad)))
    (else
     (printf "vfasl-negative-bignum: stock Chez kernel (~a wrong), not checked\n" (length bad)))))

(printf "vfasl-negative-bignum: ~a/~a passed\n" (- total fails) total)
(exit (if (= fails 0) 0 1))
