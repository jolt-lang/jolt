;; Every host member answers the JVM's arities, so the invocation layer's one bit
;; test (host-static.ss host-arity-ok?) can refuse a count the JVM has no
;; overload for. A member whose procedure takes any count passes that test for
;; every call, and an extra argument is silently dropped: (Integer/parseInt "1"
;; 10 3) answered 1, (Thread/sleep 1 0 1) slept and answered nil (jolt#1020).
;;
;; What this pins, over the registries as the runtime leaves them:
;;
;;   - an open member (arity mask < 0) is a JVM varargs member with a `varargs`
;;     row in host-static.ss, and nothing else: a new `(lambda (self . args))`
;;     entry fails here until it states its arities
;;   - every arity row names a member that is registered, so the table cannot
;;     rot into rows for members that moved or were renamed
;;   - a fixed row took effect: the stored procedure answers no count the row
;;     leaves out
;;
;; Loads the runtime through compile-eval.ss (gate-boot.ss), which registers
;; the last of the host classes (clojure.lang.Compiler).
;;   chez --script test/chez/host-arity-test.ss
(import (chezscheme))
(load "host/chez/gate-boot.ss")
;; this audits the whole registry, so the host shims that load on first reference
;; (lazy-shims.ss) load now, as they would in a boot that had them all
(lazy-shims-load-all!)

(define total 0) (define fails 0)
(define (ok name pred)
  (set! total (+ total 1))
  (unless pred (set! fails (+ fails 1)) (printf "FAIL: ~a\n" name)))

(define (open? f) (and (procedure? f) (< (procedure-arity-mask f) 0)))
(define (row table key member) (hashtable-ref table (string-append key "/" member) #f))

;; Each member table once: a static class's FQN and simple name share one table,
;; and an aliased tag shares its original's.
(define (each-member tbl key-of proc)
  (let ((seen (make-eq-hashtable)))
    (let-values (((ks hs) (hashtable-entries tbl)))
      (vector-for-each
        (lambda (k h)
          (unless (hashtable-ref seen h #f)
            (hashtable-set! seen h #t)
            (let-values (((ms fs) (hashtable-entries h)))
              (vector-for-each (lambda (m f) (proc (key-of k) m f)) ms fs))))
        ks hs))))

(define (check-open kind table tbl key-of)
  (each-member tbl key-of
    (lambda (key m f)
      (when (open? f)
        (let ((r (row table key m)))
          (ok (format "~a ~a/~a takes any count: add its JVM arities to host-static.ss" kind key m)
              (and r (eq? (car r) 'varargs))))))))

(check-open "static" host-static-arities class-statics-tbl short-class-name)
(check-open "method" host-method-arities host-methods-tbl (lambda (t) t))

(define (member-of kind key m)
  (if (string=? kind "static")
      (let ((h (hashtable-ref class-statics-tbl key #f)))
        (and h (hashtable-ref h m #f)))
      (host-method-ref key m)))

(define (check-rows kind rows)
  (for-each
    (lambda (r)
      (let* ((key (car r)) (m (cadr r)) (f (member-of kind key m)))
        (ok (format "~a row ~a/~a names nothing registered" kind key m) (procedure? f))
        (when (and (procedure? f) (not (eq? (caddr r) 'varargs)))
          (ok (format "~a row ~a/~a did not narrow the member" kind key m)
              (let ((allowed (host-arities->mask (cddr r) (string=? kind "method"))))
                (zero? (bitwise-and (procedure-arity-mask f) (bitwise-not allowed))))))))
    rows))

(check-rows "static" host-static-arity-rows)
(check-rows "method" host-method-arity-rows)

;; the rows the report named, read back as the invocation layer reads them
(define (static-ok? class member n)
  (host-arity-ok? (host-static-ref class member) n #f))
(ok "Integer/parseInt has no 3-argument overload" (not (static-ok? "Integer" "parseInt" 3)))
(ok "Integer/parseInt keeps its (s, radix) overload" (static-ok? "Integer" "parseInt" 2))
(ok "Thread/sleep has no 3-argument overload" (not (static-ok? "Thread" "sleep" 3)))
(ok "System/getProperty has no 3-argument overload" (not (static-ok? "System" "getProperty" 3)))
(ok "Math/abs has no 2-argument overload" (not (static-ok? "Math" "abs" 2)))
(ok "a varargs member stays open" (static-ok? "String" "format" 5))

(printf "host arity gate: ~a/~a passed\n" (- total fails) total)
(exit (if (zero? fails) 0 1))
