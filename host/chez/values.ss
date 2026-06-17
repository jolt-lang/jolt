;; Jolt value model on Chez Scheme — Phase 0a (jolt-cf1q.1).
;;
;; The irreducible value layer the self-hosted RT rests on. Maps Clojure's value
;; types onto Chez natives where possible, and adds records only where Chez lacks
;; a distinct type (nil sentinel, keywords, ns-bearing symbols). Loaded into an
;; env that has already (import (chezscheme)); becomes a real library in Phase 1.
;;
;; Design notes:
;; - nil is a UNIQUE sentinel, distinct from #f and '() (the classic Lisp-on-Lisp
;;   trap). jolt false -> Chez #f, jolt true -> #t.
;; - Chez's numeric tower IS Clojure's: long->exact integer, double->flonum,
;;   ratio->exact rational, bigint->bignum. A windfall vs Janet (ratios/bignums
;;   for free). Clojure `=` is exactness-aware: (= 1 1.0) is FALSE.

;; --- nil ---------------------------------------------------------------------
(define-record-type jolt-nil-t (fields) (nongenerative jolt-nil-v1))
(define jolt-nil (make-jolt-nil-t))
(define (jolt-nil? x) (jolt-nil-t? x))

;; --- truthiness: only nil and false are falsey -------------------------------
(define (jolt-truthy? x) (not (or (jolt-nil? x) (eq? x #f))))

;; --- keywords: interned so identity works; optional namespace ----------------
(define-record-type keyword-t (fields ns name khash) (nongenerative keyword-v1))
(define keyword-table (make-hashtable string-hash string=?))
;; NUL separator can't occur in a keyword ns/name, so the intern key is
;; unambiguous (a "/" separator would collide ns="a" name="b/c" with ns="a/b").
(define (keyword-intern-key ns name) (string-append (or ns "") "\x0;" name))
(define (keyword ns name)
  (let ((k (keyword-intern-key ns name)))
    (or (hashtable-ref keyword-table k #f)
        (let ((kw (make-keyword-t ns name (equal-hash k))))
          (hashtable-set! keyword-table k kw)
          kw))))
(define (keyword? x) (keyword-t? x))

;; --- symbols: ns + name + meta; NOT interned (meta varies), = by ns/name ------
(define-record-type symbol-t (fields ns name meta) (nongenerative symbol-v1))
(define (jolt-symbol ns name) (make-symbol-t ns name jolt-nil))
(define (jolt-symbol/meta ns name meta) (make-symbol-t ns name meta))
(define (jolt-symbol? x) (symbol-t? x))

;; chars/strings: Chez natives (strings treated immutable).

;; --- jolt equality (Clojure =) — scalars; collections land in Phase 2 --------
(define (jolt=2 a b)
  (cond
    ((and (jolt-nil? a) (jolt-nil? b)) #t)
    ((or  (jolt-nil? a) (jolt-nil? b)) #f)
    ((and (number? a) (number? b))                 ; exactness-aware
     (and (eq? (exact? a) (exact? b)) (= a b)))
    ((and (keyword-t? a) (keyword-t? b)) (eq? a b)) ; interned
    ((and (symbol-t? a) (symbol-t? b))
     (and (equal? (symbol-t-ns a) (symbol-t-ns b))
          (string=? (symbol-t-name a) (symbol-t-name b))))
    ((and (char? a) (char? b)) (char=? a b))
    ((and (string? a) (string? b)) (string=? a b))
    ((and (boolean? a) (boolean? b)) (eq? a b))
    ;; collections: forward to collections.ss (loaded after this file by rt.ss).
    ((and (jolt-coll? a) (jolt-coll? b)) (jolt-coll=? a b))
    (else (eq? a b))))
(define (jolt= a . rest)
  (let loop ((a a) (rest rest))
    (cond ((null? rest) #t)
          ((jolt=2 a (car rest)) (loop (car rest) (cdr rest)))
          (else #f))))

;; --- jolt hash — consistent with jolt= (for the HAMT in 0c / Phase 2) ---------
(define (jolt-hash x)
  (cond
    ((jolt-nil? x) 0)
    ((keyword-t? x) (keyword-t-khash x))
    ((symbol-t? x) (equal-hash (cons (symbol-t-ns x) (symbol-t-name x))))
    ;; distinguish inexact from exact (1 and 1.0 are not jolt=); guard non-finite
    ;; (inexact->exact would error on NaN/inf)
    ((number? x) (if (exact? x) (equal-hash x)
                     (if (and (flonum? x) (or (nan? x) (infinite? x)))
                         (equal-hash (cons 'inexact (number->string x)))
                         (equal-hash (cons 'inexact (inexact->exact x))))))
    ((string? x) (string-hash x))
    ((char? x) (char->integer x))
    ((boolean? x) (if x 1 2))
    ((jolt-coll? x) (jolt-coll-hash x))   ; forward to collections.ss
    (else (equal-hash x))))
