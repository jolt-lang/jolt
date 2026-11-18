;; top-level-inline-test.ss -- inlining through a top-level definition
;; (host/chez/chez-patches/0010-top-level-inline-info.patch, #1713).
;;
;; sa-define-inlinable! (scheme-adapter-runtime.ss) defines a symbol whose value
;; Chez records for inlining, as it records a library export's, so a caller
;; compiled afterward that calls it behind a method guard gets its body. This is
;; the mechanism a runtime specialization of a protocol call site inlines the
;; receiver's observed method with.
;;
;; The rows:
;;   a. the kernel records a definition's inlining information
;;   b. a guarded call compiled after the definition is the callee's body
;;   c. ...also for a body that reads a record field and a runtime top-level
;;      (interaction-environment labels are gensyms, which the inliner otherwise
;;      refuses as library exports)
;;   d. the guard falls back to the call when the procedure is another one, and
;;      the answers agree with the callee's on every kernel
;;   e. a plain definition records nothing and its call is not inlined
;;   f. a symbol is defined once: a second definition through
;;      sa-define-inlinable! is refused
;;   g. a body past cp0's default size is recorded under the size the caller
;;      gives, and inlined
;;
;; (a)-(c) and (g) need the patched kernel. On a stock one they report and pass, unless
;; JOLT_REQUIRE_LOCKFREE_CHEZ=1 says the kernel must carry jolt's patches (CI).
;;
;;   chez --script test/chez/top-level-inline-test.ss
(import (chezscheme))
(load "host/chez/gate-boot.ss")

(define total 0) (define fails 0)
(define (ok name pred)
  (set! total (+ total 1))
  (if pred (printf "PASS: ~a\n" name)
      (begin (set! fails (+ fails 1)) (printf "FAIL: ~a\n" name))))

(define required? (equal? (getenv "JOLT_REQUIRE_LOCKFREE_CHEZ") "1"))

;; the optimized form of a caller, as a datum to search
(define (optimized form)
  (parameterize ([print-gensym #f])
    (let ((p (open-output-string)))
      (write (expand/optimize form) p)
      (get-output-string p))))
(define (contains? s sub)
  (let ((n (string-length s)) (m (string-length sub)))
    (let loop ((i 0))
      (and (<= (+ i m) n)
           (or (string=? (substring s i (+ i m)) sub) (loop (+ i 1)))))))

;; --- a. the kernel records it -----------------------------------------------
(sa-define-inlinable! 'tli$add1 '(lambda (x) (fx+ x 1)))
(define patched? (sa-inline-recorded? 'tli$add1))
(if (or patched? required?)
    (ok "a. a definition through sa-define-inlinable! records its inlining information" patched?)
    (printf "SKIP: a-c need the patched Chez (chez-patches/0010); this one is stock\n"))

;; --- b. a guarded call is the body -------------------------------------------
(define caller-b '(lambda (f x) (if (eq? f tli$add1) (tli$add1 x) (f x))))
(when patched?
  (let ((s (optimized caller-b)))
    (ok "b. the guarded call compiled afterward is the callee's body"
        (and (contains? s "fx+") (not (contains? s "(tli$add1 x)"))))))

;; --- c. a record field and a runtime top-level ------------------------------
(define-record-type tli-box (fields v) (nongenerative tli-box-v1))
(define tli-offset 41)
(sa-define-inlinable! 'tli$read '(lambda (b) (fx+ (tli-box-v b) tli-offset)))
(define caller-c '(lambda (f b) (if (eq? f tli$read) (tli$read b) (f b))))
(when patched?
  (ok "c. a body reading runtime top-levels records its inlining information"
      (sa-inline-recorded? 'tli$read))
  (let ((s (optimized caller-c)))
    (ok "c. ...and its guarded call is the body"
        (and (contains? s "tli-offset") (not (contains? s "(tli$read b)"))))))

;; --- d. the guard, on every kernel -------------------------------------------
(let ((cb (eval caller-b)) (cc (eval caller-c)))
  (ok "d. the guarded call answers the callee's answer"
      (and (= (cb tli$add1 1) 2) (= (cc tli$read (make-tli-box 1)) 42)))
  (ok "d. another procedure takes the call"
      (and (eq? (cb (lambda (x) 'other) 1) 'other)
           (eq? (cc (lambda (b) 'other) (make-tli-box 1)) 'other))))

;; --- e. a plain definition ---------------------------------------------------
(eval '(define tli-plain (lambda (x) (fx+ x 1))))
(ok "e. a plain definition records nothing"
    (not (#%$sgetprop 'tli-plain '*clo* #f)))
(ok "e. ...and its call is not inlined"
    (contains? (optimized '(lambda (x) (tli-plain x))) "tli-plain"))

;; --- g. the size limit ------------------------------------------------------
(define big-body
  '(lambda (v) (fx+ (vector-ref v 0) (vector-ref v 1) (vector-ref v 2) (vector-ref v 3)
                    (vector-ref v 4) (vector-ref v 5) (vector-ref v 6) (vector-ref v 7))))
(sa-define-inlinable! 'tli$small-limit big-body)
(sa-define-inlinable! 'tli$big-limit big-body 100)
(when patched?
  (ok "g. a body past the default size is not recorded under it"
      (not (sa-inline-recorded? 'tli$small-limit)))
  (ok "g. ...and is recorded under the size given"
      (sa-inline-recorded? 'tli$big-limit))
  (let ((s (optimized '(lambda (f v) (if (eq? f tli$big-limit) (tli$big-limit v) (f v))))))
    (ok "g. ...and its guarded call is the body"
        (and (contains? s "vector-ref") (not (contains? s "(tli$big-limit v)"))))))
(ok "g. the answer is the body's on every kernel"
    (= ((top-level-value 'tli$big-limit) (vector 1 1 1 1 1 1 1 1)) 8))

;; --- f. defined once ---------------------------------------------------------
(ok "f. a second definition of the same symbol is refused"
    (guard (e (#t #t)) (sa-define-inlinable! 'tli$add1 '(lambda (x) x)) #f))
(ok "f. ...and the first definition stands" (= ((top-level-value 'tli$add1) 1) 2))

(printf "top-level-inline: ~a/~a passed~a\n" (- total fails) total
        (if patched? "" " (stock kernel: the inlining rows skipped)"))
(exit (if (= fails 0) 0 1))
