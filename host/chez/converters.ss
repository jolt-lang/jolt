;; converters + string ops (jolt-t6cr) — host-coupled seed natives the Chez host
;; must provide; def-var!'d into clojure.core, resolved in prelude mode. Loaded
;; last (after jolt-pr-str), since `str` reuses the printer. Semantics match the
;; Janet seed (core_print.janet str-render-one, core_io.janet core-compare,
;; core_refs.janet int/double). jolt is all-flonum, so numeric results are
;; flonums (int truncates toward zero, compare returns -1.0/0.0/1.0).

;; str: nil -> "", string raw, char bare (not \c), regex -> raw source, else the
;; printer (which renders collections with readable elements).
(define (jolt-str-render-one v)
  (cond
    ((jolt-nil? v) "")
    ((string? v) v)
    ((char? v) (string v))
    ((regex-t? v) (regex-t-source v))
    ;; str/print render the infinities and NaN long-form (Clojure .toString),
    ;; unlike the -e printer's inf/-inf/nan.
    ((and (flonum? v) (fl= v +inf.0)) "Infinity")
    ((and (flonum? v) (fl= v -inf.0)) "-Infinity")
    ((and (flonum? v) (not (fl= v v))) "NaN")
    (else (jolt-pr-str v))))
(define (jolt-str . xs)
  (let loop ((xs xs) (acc '()))
    (if (null? xs)
        (apply string-append (reverse acc))
        (loop (cdr xs) (cons (jolt-str-render-one (car xs)) acc)))))

;; jolt indices are flonums; substring etc. need exact ints.
(define (jolt->idx n) (exact (truncate n)))

(define (jolt-subs s start . end)
  (substring s (jolt->idx start)
             (if (null? end) (string-length s) (jolt->idx (car end)))))

;; vec: a pvec from any seqable (already-pvec returns itself).
(define (jolt-vec coll)
  (cond
    ((jolt-nil? coll) (jolt-vector))
    ((pvec? coll) coll)
    ((string? coll) (apply jolt-vector (string->list coll)))
    (else (apply jolt-vector (seq->list coll)))))

(define (jolt-keyword . args)
  (cond
    ((= (length args) 1)
     (let ((a (car args)))
       (cond
         ((jolt-nil? a) jolt-nil)
         ((keyword? a) a)
         ;; a 1-arg string splits on the FIRST "/" into ns/name, like the seed
         ;; (keyword "x/y") => :x/y with ns "x" — destructure's {:keys [x/y]} builds
         ;; the key this way, so without the split the namespaced key never matches.
         ((string? a)
          (let ((si (let loop ((i 0))
                      (cond ((>= i (string-length a)) #f)
                            ((char=? (string-ref a i) #\/) i)
                            (else (loop (+ i 1)))))))
            (if (and si (> si 0) (< si (- (string-length a) 1)))
                (keyword (substring a 0 si) (substring a (+ si 1) (string-length a)))
                (keyword #f a))))
         ((jolt-symbol? a)
          (let ((ns (symbol-t-ns a)))
            (keyword (if (or (jolt-nil? ns) (not ns) (eq? ns '())) #f ns) (symbol-t-name a))))
         (else (error #f "keyword: requires string/symbol/keyword" a)))))
    ((= (length args) 2)
     (keyword (let ((ns (car args))) (if (jolt-nil? ns) #f ns)) (cadr args)))
    (else (error #f "keyword: wrong arity"))))

(define (jolt-symbol-new . args)
  (cond
    ((= (length args) 1)
     (let ((a (car args)))
       (cond
         ((jolt-symbol? a) a)
         ;; no-ns sentinel is #f — matches emit's quoted-symbol lowering
         ;; (jolt-symbol #f "x"), so (= 'x (symbol "x")) holds (jolt= compares ns
         ;; with strict equal?; jolt-nil vs #f would otherwise differ).
         ((string? a) (jolt-symbol #f a))
         ((keyword? a) (jolt-symbol (keyword-t-ns a) (keyword-t-name a)))
         (else (error #f "symbol: requires string/symbol" a)))))
    ((= (length args) 2) (jolt-symbol (car args) (cadr args)))
    (else (error #f "symbol: wrong arity"))))

;; gensym: per-process counter, like the seed's gensym_counter.
(define jolt-gensym-counter 0)
(define (jolt-gensym . prefix)
  (let ((p (if (null? prefix) "G__" (car prefix))))
    (set! jolt-gensym-counter (+ jolt-gensym-counter 1))
    (jolt-symbol #f
                 (string-append (if (string? p) p (jolt-str-render-one p))
                                (number->string jolt-gensym-counter)))))

;; int/long: truncate toward zero to an EXACT integer (= JVM long). char -> code
;; point (exact). double: always a flonum (= JVM double).
(define (jolt-int x) (if (char? x) (char->integer x) (exact (truncate x))))
(define (jolt-double x) (if (char? x) (exact->inexact (char->integer x)) (exact->inexact x)))

;; compare: 3-way, returns an EXACT integer (= JVM compare -> int).
(define (jolt-cmp3 x y) (cond ((< x y) -1) ((> x y) 1) (else 0)))
(define (jolt-strcmp a b) (cond ((string<? a b) -1) ((string>? a b) 1) (else 0)))
(define (jolt-kw->string k)
  (let ((ns (keyword-t-ns k))) (if ns (string-append ns "/" (keyword-t-name k)) (keyword-t-name k))))
(define (jolt-sym-ns-string s)
  (let ((n (symbol-t-ns s))) (if (or (jolt-nil? n) (not n) (eq? n '())) "" n)))
;; compare returns an EXACT integer -1/0/1 (= JVM compare -> int).
(define (jolt-compare a b)
  (cond
    ((and (jolt-nil? a) (jolt-nil? b)) 0)
    ((jolt-nil? a) -1)
    ((jolt-nil? b) 1)
    ((and (number? a) (number? b)) (jolt-cmp3 a b))
    ((and (string? a) (string? b)) (jolt-strcmp a b))
    ((and (keyword? a) (keyword? b)) (jolt-strcmp (jolt-kw->string a) (jolt-kw->string b)))
    ((and (jolt-symbol? a) (jolt-symbol? b))
     (let ((r (jolt-strcmp (jolt-sym-ns-string a) (jolt-sym-ns-string b))))
       (if (= r 0) (jolt-strcmp (symbol-t-name a) (symbol-t-name b)) r)))
    ((and (boolean? a) (boolean? b)) (cond ((eq? a b) 0) ((eq? a #f) -1) (else 1)))
    ((and (char? a) (char? b)) (jolt-cmp3 (char->integer a) (char->integer b)))
    ((and (pvec? a) (pvec? b))
     (let ((la (pvec-count a)) (lb (pvec-count b)))
       (if (not (= la lb))
           (jolt-cmp3 la lb)
           (let loop ((i 0))
             (if (>= i la)
                 0
                 (let ((r (jolt-compare (pvec-nth-d a i jolt-nil) (pvec-nth-d b i jolt-nil))))
                   (if (= r 0) (loop (+ i 1)) r)))))))
    (else (error #f "compare: cannot compare these types" a b))))

(def-var! "clojure.core" "str" jolt-str)
(def-var! "clojure.core" "subs" jolt-subs)
(def-var! "clojure.core" "vec" jolt-vec)
(def-var! "clojure.core" "keyword" jolt-keyword)
(def-var! "clojure.core" "symbol" jolt-symbol-new)
(def-var! "clojure.core" "gensym" jolt-gensym)
(def-var! "clojure.core" "int" jolt-int)
;; char: coerce a code point (jolt's all-flonum number) to a Chez char; pass a
;; char through. Inverse of int on chars. (Missing on Chez before jolt-hs9n — the
;; cross-compiled emitter's chez-str-lit needs it for printable-ASCII escaping.)
(define (jolt-char x) (if (char? x) x (integer->char (exact (round x)))))
(def-var! "clojure.core" "char" jolt-char)
;; long: same truncation as int in jolt's all-flonum model (seed core-long =
;; math/trunc; char -> code point). Distinct cell so (long ...) resolves.
(def-var! "clojure.core" "long" jolt-int)
(def-var! "clojure.core" "double" jolt-double)
(def-var! "clojure.core" "compare" jolt-compare)
