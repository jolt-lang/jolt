;; pool-shape-test.ss — a constant pool's scope nesting does not grow with the
;; pool.
;;
;; Chez compiles a let* as one nested scope per binding, and that is quadratic in
;; the number of bindings. A def's or a bare fn's constant pool used to be one
;; let*, so a (fn [] …) of 400 statements took 1.4 s of Chez compile where flat
;; let layers take 850 ms (backend_scheme.clj, pool-layers). compilescaling timed
;; that, and on a busy CI runner its healthy readings overlapped the regressed
;; ones. This pins the emitted shape instead, which no runner can disturb: the
;; nesting depth of the emitted code is the same for a 50-statement body as for a
;; 200-statement one, with plain constants and with constants that reference
;; other constants (a hoisted vector over its keywords, two layers).
;;   chez --script test/chez/pool-shape-test.ss

(import (chezscheme))
(load "host/chez/gate-boot.ss")

(define total 0) (define fails 0)
(define (ok name pred)
  (set! total (+ total 1))
  (unless pred (set! fails (+ fails 1)) (printf "FAIL: ~a\n" name)))

(define (emitf ns str)
  (let-values (((f j) (rdr-read-form str 0 (string-length str))))
    (let ((ctx (make-analyze-ctx ns)))
      (jolt-ce-emit (jolt-ce-run-passes (jolt-ce-analyze ctx f) ctx)))))

;; Scope nesting: a let* counts one level per binding, as Chez compiles it; let,
;; letrec, named let and lambda count one.
(define (bindings-of x)
  (cond ((and (pair? (cdr x)) (symbol? (cadr x)) (pair? (cddr x))) (caddr x))   ; named let
        ((pair? (cdr x)) (cadr x))
        (else '())))
(define (depth x)
  (cond
    ((not (pair? x)) 0)
    ((eq? (car x) 'quote) 0)
    ((memq (car x) '(let* letrec* let letrec lambda case-lambda))
     (let* ((bs (if (memq (car x) '(lambda case-lambda)) '() (bindings-of x)))
            (own (if (eq? (car x) 'let*) (max 1 (length bs)) 1)))
       (+ own (fold-left (lambda (m e) (max m (depth e))) 0 (if (list? x) (cdr x) '())))))
    (else (let loop ((x x) (m 0))
            (cond ((pair? x) (loop (cdr x) (max m (depth (car x)))))
                  (else m))))))

(define (pool-size s)   ; how many _kc$ constants the emission binds
  (let ((n (string-length s)))
    (let loop ((i 0) (c 0))
      (cond ((> (+ i 6) n) c)
            ((and (string=? (substring s i (+ i 5)) "(_kc$")) (loop (+ i 5) (+ c 1)))
            (else (loop (+ i 1) c))))))

(define (fn-src n stmt)
  (let loop ((i 0) (acc "(clojure.core/fn [] "))
    (if (= i n) (string-append acc ")")
        (loop (+ i 1) (string-append acc (stmt i) " ")))))

(define (plain i) (format "(clojure.core/println :k~a (quote s~a))" i i))
(define (layered i) (format "(clojure.core/println [:a~a :b~a])" i i))

(define (check label stmt)
  (let* ((small (emitf "user" (fn-src 50 stmt)))
         (big (emitf "user" (fn-src 200 stmt)))
         (ds (depth (read (open-input-string small))))
         (db (depth (read (open-input-string big)))))
    (printf "  ~a: depth ~a at 50 statements, ~a at 200 (pool ~a)\n"
            label ds db (pool-size big))
    (ok (format "~a: the pool is exercised" label) (>= (pool-size big) 400))
    (ok (format "~a: nesting does not grow with the pool" label) (= ds db))))

(printf "pool-shape:\n")
(check "plain constants" plain)
(check "constants over constants" layered)

(printf "pool-shape: ~a checks, ~a failed\n" total fails)
(exit (if (= fails 0) 0 1))
