;; run-unit.ss — host-specific unit gate.
;;
;; Loads the checked-in seed + spine, reads test/chez/unit.edn, and for each case
;; evaluates :expr (wrapped in (do ...), as `joltc -e` does) and compares its PRINTED
;; value (jolt-final-str) to the literal :expected string. :expected :throws asserts
;; the case raises. These cover host-specific behavior (dot-forms, java statics, io,
;; reader, walk, …) that isn't in the JVM-portable corpus. Global state is reset
;; between cases for per-case isolation.
;;
;;   chez --script host/chez/run-unit.ss
(import (chezscheme))

(load "host/chez/rt.ss")
(set-chez-ns! "clojure.core")
(load "host/chez/seed/prelude.ss")
(load "host/chez/post-prelude.ss")
(set-chez-ns! "user")
(load "host/chez/host-contract.ss")
(load "host/chez/seed/image.ss")
(load "host/chez/compile-eval.ss")

(define (slurp path)
  (call-with-input-file path
    (lambda (p)
      (let loop ((cs '()) (c (read-char p)))
        (if (eof-object? c) (list->string (reverse cs))
            (loop (cons c cs) (read-char p)))))))

(define cases (jolt-read-string (slurp "test/chez/unit.edn")))
(define kw-suite    (keyword #f "suite"))
(define kw-expr     (keyword #f "expr"))
(define kw-expected (keyword #f "expected"))
(define kw-throws   (keyword #f "throws"))

;; --- per-case isolation (snapshot the world after setup, restore each case) -------
(define zj-base (let ((h (make-hashtable string-hash string=?)))
  (vector-for-each (lambda (k) (hashtable-set! h k #t)) (hashtable-keys var-table)) h))
(define zj-roots '())
(vector-for-each (lambda (k) (let ((c (hashtable-ref var-table k #f)))
                   (when c (set! zj-roots (cons (cons c (var-cell-root c)) zj-roots)))))
                 (hashtable-keys var-table))
(define (zj-snap ht) (let ((h (make-hashtable string-hash string=?)))
  (vector-for-each (lambda (k) (hashtable-set! h k #t)) (hashtable-keys ht)) h))
(define (zj-prune! ht base) (vector-for-each
  (lambda (k) (unless (hashtable-ref base k #f) (hashtable-delete! ht k))) (hashtable-keys ht)))
(define zj-ns-base (zj-snap ns-registry))
(define zj-type-base (zj-snap type-registry))
(define zj-ghier (var-cell-lookup "clojure.core" "global-hierarchy"))
(define (zj-reset!)
  (vector-for-each (lambda (k) (unless (hashtable-ref zj-base k #f) (hashtable-delete! var-table k)))
                   (hashtable-keys var-table))
  (for-each (lambda (cr) (unless (eq? (var-cell-root (car cr)) (cdr cr))
                           (var-cell-root-set! (car cr) (cdr cr)))) zj-roots)
  (zj-prune! ns-registry zj-ns-base)
  (zj-prune! type-registry zj-type-base)
  (hashtable-clear! ns-alias-table)
  (hashtable-clear! ns-refer-table)
  (hashtable-clear! ns-refer-all-table)
  (when zj-ghier (jolt-invoke (var-deref "clojure.core" "reset!")
                   (var-cell-root zj-ghier) (jolt-invoke (var-deref "clojure.core" "make-hierarchy"))))
  (set-chez-ns! "user"))

;; --- run ------------------------------------------------------------------------
(define pass 0)
(define fails '())              ; (suite expr msg)
(define suite-pass (make-hashtable string-hash string=?))
(define suite-total (make-hashtable string-hash string=?))
(define (bump! ht k) (hashtable-set! ht k (+ 1 (hashtable-ref ht k 0))))

(let loop ((i 0))
  (when (< i (pvec-count cases))
    (let* ((row (pvec-nth-d cases i jolt-nil))
           (suite (jolt-get row kw-suite))
           (expr (jolt-get row kw-expr))
           (expected (jolt-get row kw-expected))
           (throws? (eq? expected kw-throws))
           (sink (open-output-string)))
      (bump! suite-total suite)
      (guard (e (#t (if throws?
                        (begin (set! pass (+ pass 1)) (bump! suite-pass suite))
                        (set! fails (cons (list suite expr "raised") fails)))))
        (let ((got (jolt-final-str
                     (parameterize ((current-output-port sink))
                       (jolt-compile-eval (string-append "(do " expr ")") "user")))))
          (cond
            (throws? (set! fails (cons (list suite expr (string-append "expected throw; got " got)) fails)))
            ((string=? got expected) (begin (set! pass (+ pass 1)) (bump! suite-pass suite)))
            (else (set! fails (cons (list suite expr
                    (string-append "want `" expected "` got `" got "`")) fails))))))
      (zj-reset!))
    (loop (+ i 1))))

(printf "\nunit gate: ~a/~a passed\n" pass (pvec-count cases))
(let-values (((ks vs) (hashtable-entries suite-total)))
  (for-each (lambda (p)
              (printf "  ~a/~a  ~a\n" (hashtable-ref suite-pass (car p) 0) (cdr p) (car p)))
            (list-sort (lambda (a b) (string<? (car a) (car b)))
                       (vector->list (vector-map cons ks vs)))))
(when (> (length fails) 0)
  (printf "\n~a FAIL(s):\n" (length fails))
  (for-each (lambda (f) (printf "  [~a] ~a\n    ~a\n" (car f) (caddr f) (cadr f)))
            (list-head (reverse fails) (min 40 (length fails)))))
(flush-output-port)
(exit (if (> (length fails) 0) 1 0))
