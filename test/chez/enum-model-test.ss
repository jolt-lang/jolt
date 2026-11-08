;; Every enum the runtime models is declared through the enum model
;; (java/enums.ss register-enum-class!), so each one answers values/valueOf,
;; Class.isEnum, Enum/valueOf, Enum's instance methods and the JVM print form.
;; A class that reaches java.lang.Enum through a plain class-graph row, or is
;; marked ENUM for its modifiers, without that declaration has none of those
;; and nothing else fails for it — this does.
;;
;; What this pins, over the tables as the runtime leaves them:
;;
;;   - every class whose direct supers include java.lang.Enum is a declared enum
;;   - every class the modifier table marks ENUM is a declared enum
;;   - each declared enum's constants are distinct and know their own ordinal,
;;     values() is a fresh array of the class's array type holding them in
;;     order, valueOf answers each constant by name, and a jhost constant's tag
;;     reports the enum's class
;;
;;   chez --script test/chez/enum-model-test.ss
(import (chezscheme))
(load "host/chez/gate-boot.ss")
;; this audits the whole registry, so the host shims that load on first reference
;; (lazy-shims.ss) load now, as they would in a boot that had them all
(lazy-shims-load-all!)

(define total 0) (define fails 0)
(define (ok name pred)
  (set! total (+ total 1))
  (unless pred (set! fails (+ fails 1)) (printf "FAIL: ~a\n" name)))

(define (declared? fqn)
  (let ((info (hashtable-ref enum-class-tbl fqn #f)))
    (and info (string=? (enum-info-fqn info) fqn))))

(let-values (((names supers) (hashtable-entries jvm-class-parents)))
  (vector-for-each
    (lambda (n ss)
      (when (member "java.lang.Enum" ss)
        (ok (format "~a extends java.lang.Enum but is not declared through register-enum-class! (java/enums.ss)" n)
            (declared? n))))
    names supers))

(vector-for-each
  (lambda (n)
    (ok (format "~a is marked ENUM but is not declared through register-enum-class! (java/enums.ss)" n)
        (declared? n)))
  (hashtable-keys jch-enum-set))

(define (each-enum proc)
  (let-values (((ks infos) (hashtable-entries enum-class-tbl)))
    (vector-for-each (lambda (k info) (when (string=? k (enum-info-fqn info)) (proc info))) ks infos)))

(each-enum
  (lambda (info)
    (let* ((fqn (enum-info-fqn info))
           (consts (enum-info-consts info))
           (names (enum-info-names info))
           (n (vector-length consts)))
      (ok (format "~a declares at least one constant" fqn) (fx>? n 0))
      (ok (format "~a's supers include java.lang.Enum" fqn)
          (member "java.lang.Enum" (jch-direct-supers fqn)))
      (do ((i 0 (fx+ i 1))) ((fx=? i n))
        (let ((c (vector-ref consts i)) (nm (vector-ref names i)))
          (ok (format "~a.~a knows its ordinal ~a" fqn nm i)
              (let ((e (enum-of c))) (and e (eq? (car e) info) (fx=? (cdr e) i))))
          (ok (format "~a/valueOf answers ~a" fqn nm)
              (eq? (enum-value-of info nm) c))
          (ok (format "~a/~a is the declared constant" fqn nm)
              (eq? (host-static-ref fqn nm) c))
          (when (jhost? c)
            (ok (format "~a.~a's tag ~a reports the enum's class" fqn nm (jhost-tag c))
                (equal? (hashtable-ref jhost-tag->fqn (jhost-tag c) #f) fqn)))))
      (let ((a ((host-static-ref fqn "values"))) (b ((host-static-ref fqn "values"))))
        (ok (format "~a/values is a fresh array per call" fqn) (not (eq? a b)))
        (ok (format "~a/values is a ~a array" fqn fqn)
            (equal? (jolt-class-name a) (string-append "[L" fqn ";")))
        (ok (format "~a/values holds the constants in order" fqn)
            (let loop ((i 0))
              (or (fx=? i n)
                  (and (eq? (ja-ref a i) (vector-ref consts i)) (loop (fx+ i 1))))))))))

(printf "enum model gate: ~a/~a passed\n" (- total fails) total)
(exit (if (zero? fails) 0 1))
