;; lazy-hoist-test.ss — every lazy host shim may be loaded definitions-first.
;;
;; lazy-shims.ss loads a lazy file's definition forms before its other top-level
;; forms (lazy-shim-hoist), so that no registration a thread can hit names a
;; definition the load has not reached (burinc/jolt#24). Moving a definition
;; above a registration is only sound when the definition's value does not
;; depend on that registration. This checks it of every file rt.ss declares with
;; (lazy-load …):
;;
;;   - a definition that comes after the file's first non-definition form is
;;     either a procedure (lambda / case-lambda / (define (f …) …)), syntax or a
;;     record type, or its initializer calls only the pure constructors below or
;;     procedures the same file defines;
;;   - no top-level wrapper (begin, when, eval-when, …) hides a definition from
;;     the hoist.
;;
;; A failure names the file, the definition and the call. Either move the
;; definition above the registrations in the source, or, when the call really is
;; pure, add it to pure-calls with the reason.

(import (chezscheme))
;; the hoist itself (lazy-shim-hoist and its helpers) as the runtime defines it;
;; the file's top level only defines, so it loads without the rest of the runtime
(load "host/chez/lazy-shims.ss")

(define total 0) (define fails 0)
(define (ok name pred)
  (set! total (+ total 1))
  (unless pred (set! fails (+ fails 1)) (printf "FAIL: ~a\n" name)))

;; Calls with no effect on, and no dependence on, the runtime's registries.
(define pure-calls
  '(cons list list* vector append + - * / quotient remainder expt ash logor logand
    eq? eqv? equal? not and or if let let* cond case
    make-mutex make-condition make-hashtable make-eq-hashtable make-eqv-hashtable
    make-weak-eq-hashtable make-guardian box string-append string->symbol
    ;; foreign entry points and the adapter queries behind them: a lookup in
    ;; the C symbol table, the same answer whenever it is asked
    jolt-foreign-proc-safe jolt-foreign-proc-blocking sa-os-family sa-foreign-sizeof
    ;; record constructors of host records loaded before any lazy file
    make-jhost
    ;; higher-order calls: what they run is checked as well (calling-ops)
    map for-each filter
    ;; jutil-coll-views.ss: answers a toArray closure over its argument, touching
    ;; nothing until that closure is called
    jxc-to-array))

(define (lazy-paths)
  (call-with-input-file "host/chez/rt.ss"
    (lambda (p)
      (let loop ((acc '()))
        (let ((f (read p)))
          (cond ((eof-object? f) (reverse acc))
                ((and (pair? f) (eq? (car f) 'lazy-load) (string? (cadr f)))
                 (loop (cons (cadr f) acc)))
                (else (loop acc))))))))

(define (procedure-definition? f)
  (or (memq (car f) '(define-syntax define-record-type define-ftype define-condition-type define-enumeration))
      (and (eq? (car f) 'define)
           (or (pair? (cadr f))
               (and (pair? (cddr f)) (pair? (caddr f))
                    (memq (car (caddr f)) '(lambda case-lambda)))))))

(define (defined-name f) (if (pair? (cadr f)) (caadr f) (cadr f)))

;; operator-position symbols of X that run when X is evaluated: outside quoted
;; data, and outside a lambda's body -- except a lambda handed to an operator that
;; calls it then and there (map, for-each, apply, ...), whose body runs too.
(define calling-ops '(map for-each apply vector-map vector-for-each fold-left fold-right filter))
(define (lambda-body-calls l)
  (if (and (pair? l) (eq? (car l) 'lambda) (pair? (cdr l)))
      (apply append (map calls (cddr l)))
      '()))
(define (calls x)
  (cond ((not (pair? x)) '())
        ((memq (car x) '(lambda case-lambda quote quasiquote)) '())
        ((not (list? x)) '())
        (else (append (if (symbol? (car x)) (list (car x)) '())
                      (if (memq (car x) calling-ops)
                          (apply append (map lambda-body-calls (cdr x)))
                          '())
                      (apply append (map calls (cdr x)))))))

(for-each
  (lambda (path)
    (let* ((forms (lazy-shim-read-forms path))
           (procs (filter (lambda (s) s)
                          (map (lambda (f) (and (lazy-shim-definition? f) (procedure-definition? f)
                                                (defined-name f)))
                               forms))))
      (let loop ((fs forms) (past-stmt #f))
        (unless (null? fs)
          (let ((f (car fs)))
            (when (and (pair? f) (memq (car f) '(begin when unless eval-when module meta)))
              (ok (format "~a: a top-level ~a wraps forms the hoist cannot see into" path (car f)) #f))
            (when (and past-stmt (lazy-shim-definition? f) (not (procedure-definition? f))
                       (pair? (cddr f)))
              (for-each
                (lambda (c)
                  (ok (format "~a: (define ~a …) is hoisted above a registration but calls ~a"
                              path (defined-name f) c)
                      (or (memq c pure-calls) (memq c procs))))
                (calls (caddr f))))
            (loop (cdr fs) (or past-stmt (not (lazy-shim-definition? f)))))))
      ;; and the hoist itself: every definition ahead of every other form
      (let ((h (lazy-shim-hoist forms)))
        (ok (format "~a: hoisted forms put every definition first" path)
            (let scan ((h h) (seen-stmt #f))
              (cond ((null? h) #t)
                    ((lazy-shim-definition? (car h)) (and (not seen-stmt) (scan (cdr h) #f)))
                    (else (scan (cdr h) #t)))))
        (ok (format "~a: hoisting keeps every form" path) (= (length h) (length forms))))))
  (lazy-paths))

(printf "lazy hoist: ~a/~a passed\n" (- total fails) total)
(exit (if (= fails 0) 0 1))
