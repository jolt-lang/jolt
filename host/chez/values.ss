;; Jolt value model on Chez Scheme.
;;
;; The irreducible value layer the self-hosted RT rests on. Maps Clojure's value
;; types onto Chez natives where possible, and adds records only where Chez lacks
;; a distinct type (nil sentinel, keywords, ns-bearing symbols). Loaded into an
;; env that has already (import (chezscheme)).
;;
;; Design notes:
;; - nil is a UNIQUE sentinel, distinct from #f and '() (the classic Lisp-on-Lisp
;;   trap). jolt false -> Chez #f, jolt true -> #t.
;; - Chez's numeric tower IS Clojure's: long->exact integer, double->flonum,
;;   ratio->exact rational, bigint->bignum. Clojure `=` is exactness-aware:
;;   (= 1 1.0) is FALSE.

;; --- nil ---------------------------------------------------------------------
(define-record-type jolt-nil-t (fields) (nongenerative jolt-nil-v1))
(define jolt-nil (make-jolt-nil-t))
(define (jolt-nil? x) (jolt-nil-t? x))
(define (jolt-some? x) (not (jolt-nil-t? x)))

;; --- truthiness: only nil and false are falsey -------------------------------
(define (jolt-truthy? x) (not (or (jolt-nil? x) (eq? x #f))))

;; --- keywords: interned so identity works; optional namespace ----------------
(define-record-type keyword-t (fields ns name khash) (nongenerative keyword-v1))
(define keyword-table (make-hashtable string-hash string=?))
;; The common no-ns keyword is interned in a table keyed by NAME directly, so a
;; lookup of an already-interned :kw (the hot case — every (:kw x), map literal,
;; keyword arg) is one hashtable-ref with NO allocation. The ns table keeps the
;; combined key. Both share the keyword-t khash (equal-hash of the combined key),
;; so hash values are unchanged.
(define keyword-table-bare (make-hashtable string-hash string=?))
;; NUL separator can't occur in a keyword ns/name, so the intern key is
;; unambiguous (a "/" separator would collide ns="a" name="b/c" with ns="a/b").
(define (keyword-intern-key ns name) (string-append (or ns "") "\x0;" name))
(define (keyword ns name)
  (if ns
      (let ((k (keyword-intern-key ns name)))
        (or (hashtable-ref keyword-table k #f)
            (let ((kw (make-keyword-t ns name (equal-hash k))))
              (hashtable-set! keyword-table k kw)
              kw)))
      (or (hashtable-ref keyword-table-bare name #f)
          (let ((kw (make-keyword-t #f name (equal-hash (keyword-intern-key #f name)))))
            (hashtable-set! keyword-table-bare name kw)
            kw))))
(define (keyword? x) (keyword-t? x))

;; --- symbols: ns + name + meta; NOT interned (meta varies), = by ns/name ------
;; The ns/name STRINGS are pooled (like JVM Symbol.intern, which .intern()s them):
;; two separately-read `?a` symbols share one name-string object, so code that
;; compares symbol names by identity (core.logic's non-unique lvar equality, via
;; (str sym)) behaves like the JVM.
(define symbol-string-pool (make-hashtable string-hash string=?))
(define (intern-symbol-string s)
  (if (string? s)
      (or (hashtable-ref symbol-string-pool s #f)
          (begin (hashtable-set! symbol-string-pool s s) s))
      s))
(define-record-type symbol-t (fields ns name meta) (nongenerative symbol-v1))
(define (jolt-symbol ns name)
  (make-symbol-t (intern-symbol-string ns) (intern-symbol-string name) jolt-nil))
(define (jolt-symbol/meta ns name meta)
  (make-symbol-t (intern-symbol-string ns) (intern-symbol-string name) meta))
(define (jolt-symbol? x) (symbol-t? x))

;; chars/strings: Chez natives (strings treated immutable).

;; --- jolt equality (Clojure =) — scalars + collections ----------------------
;; A host shim registers a type's equality via register-eq-arm! instead of
;; set!-wrapping jolt=2 (cf. register-hash-arm!). An arm is (pred . handler), both
;; (a b): the arm applies when pred holds (typically either arg is the type), and
;; handler returns the #t/#f result. Arms are checked before the base scalar/coll
;; cases; the entry is stable.
(define jolt-eq-arms '())
(define (register-eq-arm! pred handler)
  (set! jolt-eq-arms (cons (cons pred handler) jolt-eq-arms)))
(define (jolt=2-base a b)
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
    ;; sequential (vector / list / lazy seq) compare element-wise, cross-type:
    ;; (= [1 2 3] (list 1 2 3)) is true. Forward to seq.ss (loaded by rt.ss).
    ((and (jolt-sequential? a) (jolt-sequential? b)) (seq=? a b))
    ((or (jolt-sequential? a) (jolt-sequential? b)) #f)
    ;; other collections (map/set): forward to collections.ss.
    ((and (jolt-coll? a) (jolt-coll? b)) (jolt-coll=? a b))
    (else (eq? a b))))
(define (jolt=2 a b)
  (let loop ((as jolt-eq-arms))
    (cond ((null? as) (jolt=2-base a b))
          (((caar as) a b) ((cdar as) a b))
          (else (loop (cdr as))))))
(define (jolt= a . rest)
  (let loop ((a a) (rest rest))
    (cond ((null? rest) #t)
          ((jolt=2 a (car rest)) (loop (car rest) (cdr rest)))
          (else #f))))

;; --- jolt hash — consistent with jolt= (for the HAMT) -----------------------
;; A host shim (records, host-table, inst-time, …) registers its type's hash via
;; register-hash-arm! instead of set!-wrapping jolt-hash — the arms are disjoint
;; types, checked before the base cases, so the full behavior is gathered here plus
;; the registry rather than scattered across a set! chain (cf. register-str-render!).
(define jolt-hash-arms '())
(define (register-hash-arm! pred handler)
  (set! jolt-hash-arms (cons (cons pred handler) jolt-hash-arms)))
(define (jolt-hash-base x)
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
    ((jolt-sequential? x) (seq-hash x)) ; vector/list/seq hash alike (forward to seq.ss)
    ((jolt-coll? x) (jolt-coll-hash x))   ; map/set; forward to collections.ss
    (else (equal-hash x))))
(define (jolt-hash x)
  (let loop ((as jolt-hash-arms))
    (cond ((null? as) (jolt-hash-base x))
          (((caar as) x) ((cdar as) x))
          (else (loop (cdr as))))))
