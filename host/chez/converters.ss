;; converters + string ops — host-coupled natives def-var!'d into clojure.core,
;; resolved in prelude mode. Loaded last (after jolt-pr-str), since `str` reuses
;; the printer. int/long truncate toward zero to an exact integer; compare returns
;; an exact -1/0/1; double yields a flonum.

;; str rendering for the value types not handled by the fast arms below. A host
;; shim loaded later (records, host-table, inst-time, …) registers an arm with
;; register-str-render! instead of set!-wrapping jolt-str-render-one — the arms
;; are type-disjoint, so the full behavior is the base arms here plus the
;; registry, gathered in one place rather than scattered across a set! chain.
;; Newest registration is checked first (matches the old outermost-wins order).
(define str-render-registry '())   ; list of (pred . render), checked front-to-back
(define (register-str-render! pred render)
  (set! str-render-registry (cons (cons pred render) str-render-registry)))

;; str: nil -> "", string raw, char bare (not \c), regex -> raw source, a
;; registered host type via its arm, else the printer (which renders collections
;; with readable elements).
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
    ;; a symbol stringifies to its name (JVM Symbol.toString returns the interned
    ;; name), so (str sym) of a no-ns symbol is the SAME string object the symbol
    ;; holds — code that compares those by identity (core.logic's non-unique lvar
    ;; equality) depends on it.
    ((symbol-t? v)
     (let ((ns (symbol-t-ns v)))
       (if (or (not ns) (jolt-nil? ns))
           (symbol-t-name v)
           (string-append ns "/" (symbol-t-name v)))))
    (else
     (let loop ((rs str-render-registry))
       (cond
         ((null? rs) (jolt-pr-str v))
         (((caar rs) v) ((cdar rs) v))
         (else (loop (cdr rs))))))))
;; print/println render non-readably: a nested string is raw. jolt-str-render-one
;; is exactly that (collections fall through to jolt-pr-str). The print family
;; uses this seam, NOT the str fn — which renders readably (below). A top-level nil
;; prints "nil" (str renders it ""), so the seam special-cases it.
(define (jolt-print-one v) (if (jolt-nil? v) "nil" (jolt-str-render-one v)))
(def-var! "clojure.core" "__print1" jolt-print-one)

;; str: a top-level string/scalar renders as jolt-str-render-one (raw string,
;; "Infinity"…), but a COLLECTION renders as its readable form — nested strings
;; are QUOTED ((str ["x"]) => "[\"x\"]"), matching the JVM (a collection's
;; toString is readable). jolt-pr-readable resolves at call time.
(define (jolt-str-one v)
  (if (or (pvec? v) (pmap? v) (pset? v) (cseq? v) (empty-list-t? v) (jolt-lazyseq? v))
      (jolt-pr-readable v)
      (jolt-str-render-one v)))
(define (jolt-str . xs)
  (cond
    ((null? xs) "")
    ;; single arg returns its rendering directly (no string-append copy), so
    ;; (str sym) hands back the symbol's own name string — JVM (str x) is
    ;; x.toString(), and core.logic's non-unique lvar equality compares those by
    ;; identity.
    ((null? (cdr xs)) (jolt-str-one (car xs)))
    (else (let loop ((xs xs) (acc '()))
            (if (null? xs)
                (apply string-append (reverse acc))
                (loop (cdr xs) (cons (jolt-str-one (car xs)) acc)))))))

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
         ;; a 1-arg string splits on the FIRST "/" into ns/name:
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
         ;; (symbol "ns/name") splits the namespace at the FIRST "/" (JVM
         ;; Symbol.intern), so (namespace (symbol "foo/bar/baz")) => "foo" with
         ;; name "bar/baz". A lone "/" or a leading slash has no namespace. The
         ;; no-ns sentinel is #f — matches emit's quoted-symbol lowering
         ;; (jolt-symbol #f "x"), so (= 'x (symbol "x")) holds (jolt= compares
         ;; ns with strict equal?).
         ((string? a)
          (let ((slen (string-length a)))
            (if (string=? a "/")
                (jolt-symbol #f "/")
                (let loop ((i 1))
                  (cond ((>= i slen) (jolt-symbol #f a))
                        ((char=? (string-ref a i) #\/)
                         (jolt-symbol (substring a 0 i) (substring a (+ i 1) slen)))
                        (else (loop (+ i 1))))))))
         ((keyword? a) (jolt-symbol (keyword-t-ns a) (keyword-t-name a)))
         ;; (symbol a-var) -> the var's qualified symbol (clojure.spec.alpha/->sym).
         ((var-cell? a) (jolt-symbol (var-cell-ns a) (var-cell-name a)))
         (else (error #f "symbol: requires string/symbol" a)))))
    ;; (symbol ns name): a nil namespace is the no-ns sentinel #f (NOT jolt-nil),
    ;; so (symbol nil "x") equals (symbol "x") and the reader literal 'x — jolt=
    ;; compares ns with strict equal?, so a jolt-nil ns would differ from #f.
    ((= (length args) 2)
     (let ((ns (car args)))
       (jolt-symbol (if (jolt-nil? ns) #f ns) (cadr args))))
    (else (error #f "symbol: wrong arity"))))

;; gensym: per-process counter.
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
;; a numeric type outside Chez's tower converts through this hook (bigdec).
(define (jolt-double-slow x) (jolt-num-cast-throw x))
(define (jolt-double x)
  (cond ((char? x) (exact->inexact (char->integer x)))
        ((number? x) (exact->inexact x))
        (else (jolt-double-slow x))))

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
    ;; keywords order like symbols: a nil namespace sorts before any namespace,
    ;; then by namespace, then by name (Keyword.compareTo -> Symbol.compareTo)
    ((and (keyword? a) (keyword? b))
     (let ((r (jolt-strcmp (or (keyword-t-ns a) "") (or (keyword-t-ns b) ""))))
       (if (= r 0) (jolt-strcmp (keyword-t-name a) (keyword-t-name b)) r)))
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
;; --- checked narrow casts (RT.byteCast/shortCast/intCast/longCast/charCast) --
;; One helper carries the JVM ranges: truncate toward zero, then range-check.
;; NaN casts to 0 (Java (long)NaN); an out-of-range value (including a float
;; infinity) is IllegalArgumentException "Value out of range for <type>: x".
;; A non-numeric operand is the usual ClassCastException. Numeric types outside
;; Chez's tower truncate through a hook the shim extends (BigDecimal).
(define (jolt-cast-range-throw name x)
  (jolt-throw (jolt-host-throwable
               "java.lang.IllegalArgumentException"
               (string-append "Value out of range for " name ": " (jolt-str x)))))
(define (jolt-cast-truncate-slow x) (jolt-num-cast-throw x))
(define (jolt-checked-cast name lo hi x)
  (let ((n (cond ((char? x) (char->integer x))
                 ((and (number? x) (exact? x)) (truncate x))
                 ;; a double range-checks ITSELF (before truncation): (byte
                 ;; 127.000001) throws, (byte 1.1) is 1; NaN casts to 0; an
                 ;; infinity always fails the compare.
                 ((flonum? x) (cond ((nan? x) 0)
                                    ((or (< x lo) (> x hi)) (+ hi 1))
                                    (else (exact (truncate x)))))
                 (else (jolt-cast-truncate-slow x)))))
    (if (and (>= n lo) (<= n hi)) n (jolt-cast-range-throw name x))))
(define (jolt-byte-cast x)  (jolt-checked-cast "byte" -128 127 x))
(define (jolt-short-cast x) (jolt-checked-cast "short" -32768 32767 x))
(define (jolt-int-cast x)   (jolt-checked-cast "int" -2147483648 2147483647 x))
(define (jolt-long-cast x)  (jolt-checked-cast "long" -9223372036854775808 9223372036854775807 x))
(def-var! "clojure.core" "int" jolt-int-cast)
(def-var! "clojure.core" "long" jolt-long-cast)
(def-var! "clojure.core" "byte" jolt-byte-cast)
(def-var! "clojure.core" "short" jolt-short-cast)
;; char: pass a char through; a code point must be in [0, 0xFFFF] (charCast).
(define (jolt-char x)
  (if (char? x) x (integer->char (jolt-checked-cast "char" 0 65535 x))))
(def-var! "clojure.core" "char" jolt-char)
;; unchecked-long: truncate + wrap to 64 bits (RT.uncheckedLongCast — a float
;; infinity saturates, NaN is 0). unchecked-int wraps and sign-folds to 32.
(define (jolt-cast-saturate n lo hi) (cond ((< n lo) lo) ((> n hi) hi) (else n)))
(define (jolt-unchecked-long x)
  (cond ((char? x) (char->integer x))
        ;; an exact integer wraps (long narrowing); a double SATURATES (Java's
        ;; double->long conversion clamps at the bounds, NaN is 0).
        ((and (number? x) (exact? x)) (jolt-wrap64 (truncate x)))
        ((flonum? x) (if (nan? x) 0
                         (jolt-cast-saturate (if (infinite? x) (if (> x 0.0) unc-2^63 (- unc-2^63)) (exact (truncate x)))
                                             -9223372036854775808 9223372036854775807)))
        (else (jolt-wrap64 (jolt-cast-truncate-slow x)))))
(define (jolt-unchecked-int x)
  (if (flonum? x)
      ;; double->int clamps like Java
      (if (nan? x) 0
          (jolt-cast-saturate (if (infinite? x) (if (> x 0.0) #x80000000 (- #x80000000)) (exact (truncate x)))
                              -2147483648 2147483647))
      (let ((i (bitwise-and (jolt-unchecked-long x) #xffffffff)))
        (if (>= i #x80000000) (- i #x100000000) i))))
(def-var! "clojure.core" "unchecked-long" jolt-unchecked-long)
(def-var! "clojure.core" "unchecked-int" jolt-unchecked-int)
(def-var! "clojure.core" "double" jolt-double)
;; float: Chez has no single-float type, so the value stays a flonum — but the
;; cast range-checks against Float/MAX_VALUE like RT.floatCast (an infinity is
;; out of range; NaN passes).
(define fl-float-max 3.4028234663852886e38)
(define (jolt-float x)
  (let ((d (jolt-double x)))
    (if (and (flonum? d) (not (nan? d))
             (or (< d (- fl-float-max)) (> d fl-float-max)))
        (jolt-cast-range-throw "float" x)
        d)))
(def-var! "clojure.core" "float" jolt-float)
;; numerator/denominator: jolt ratios are Chez exact rationals; a non-ratio is
;; the JVM's Ratio cast failure.
(define (jolt-ratio-part name f)
  (lambda (x)
    (if (and (number? x) (exact? x) (rational? x) (not (integer? x)))
        (f x)
        (jolt-throw (jolt-host-throwable
                     "java.lang.ClassCastException"
                     (string-append "class " (guard (e (#t "?")) (jolt-class-name x))
                                    " cannot be cast to class clojure.lang.Ratio"))))))
(def-var! "clojure.core" "numerator" (jolt-ratio-part "numerator" numerator))
(def-var! "clojure.core" "denominator" (jolt-ratio-part "denominator" denominator))
(def-var! "clojure.core" "compare" jolt-compare)
