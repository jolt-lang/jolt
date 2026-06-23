;; type predicates + simple accessors — host-coupled natives.
;;
;; These are host primitives (not clojure.core overlay fns), so they're never
;; def-var!'d by the assembled prelude; the Chez host must provide them.
;; map?/vector?/set? are STRICT over the persistent-collection records, seq? is
;; true only for real sequences, coll? is the union. Record arms are added by
;; records.ss, which extends these dispatchers.

(define (jolt-map? x) (pmap? x))
;; a map entry is a pvec under the hood AND is vector? — Clojure's MapEntry
;; implements IPersistentVector, so (vector? (first {:a 1})) is true.
(define (jolt-vector? x) (pvec? x))
(define (jolt-set? x) (pset? x))
(define (jolt-seq? x) (or (cseq? x) (empty-list-t? x)))
;; (list? x): a list-marked cseq node or the empty list (). A lazy/vector-backed
;; seq, (rest list), (seq coll), (map …) are seqs but not lists.
(define (jolt-list-pred? x) (or (and (cseq? x) (cseq-list? x)) (empty-list-t? x)))
(define (jolt-coll-pred? x)
  (or (pvec? x) (pmap? x) (pset? x) (cseq? x) (empty-list-t? x)))
(define (jolt-number? x) (number? x))
(define (jolt-string? x) (string? x))
(define (jolt-char-pred? x) (char? x))
;; JVM-parity number-type predicates over the Chez numeric tower. integer? is the
;; INTEGER TYPE (exact integer = Long/BigInt), NOT integer-VALUED: (integer? 3.0)
;; is false on the JVM (3.0 is a Double). float? = flonum (double). ratio? = exact
;; non-integer (= JVM Ratio). rational? = exact (integer or ratio; jolt has no
;; BigDecimal). decimal? is always false (no BigDecimal type).
(define (jolt-integer? x) (and (number? x) (exact? x) (integer? x)))
(define (jolt-float? x) (and (number? x) (flonum? x)))
(define (jolt-ratio? x) (and (number? x) (exact? x) (rational? x) (not (integer? x))))
(define (jolt-rational? x) (and (number? x) (exact? x)))
(define (jolt-decimal? x) #f)
(define (jolt-fn? x) (procedure? x))
(define (jolt-boolean-pred? x) (boolean? x))

;; (boolean x) coerces truthiness (nil/false -> false, else true).
(define (jolt-boolean x) (if (jolt-truthy? x) #t #f))

;; (name x): keyword/symbol -> name string; string -> itself.
(define (jolt-name x)
  (cond
    ((keyword? x) (keyword-t-name x))
    ((symbol-t? x) (symbol-t-name x))
    ((string? x) x)
    (else (error #f "name: expected string/symbol/keyword" x))))

;; (namespace x): keyword/symbol ns string, or nil when unqualified.
(define (jolt-namespace x)
  (let ((ns (cond ((keyword? x) (keyword-t-ns x))
                  ((symbol-t? x) (symbol-t-ns x))
                  (else (error #f "namespace: expected symbol/keyword" x)))))
    (if (or (jolt-nil? ns) (not ns) (eq? ns '())) jolt-nil ns)))

(def-var! "clojure.core" "nil?" jolt-nil?)
(def-var! "clojure.core" "number?" jolt-number?)
(def-var! "clojure.core" "string?" jolt-string?)
(def-var! "clojure.core" "char?" jolt-char-pred?)
(def-var! "clojure.core" "integer?" jolt-integer?)
(def-var! "clojure.core" "float?" jolt-float?)
(def-var! "clojure.core" "ratio?" jolt-ratio?)
(def-var! "clojure.core" "rational?" jolt-rational?)
(def-var! "clojure.core" "decimal?" jolt-decimal?)
;; == numeric value-equality (ignores exactness, unlike =): (== 3 3.0) -> true.
;; 1-arity is trivially true; 2+ args must all be numbers (Numbers.equiv throws
;; otherwise). Uses Scheme = (value across the tower), not jolt= (category-aware).
(define (jolt-num-equiv . xs)
  ;; 1-arity short-circuits to true for ANY value (Clojure's == 1-arg returns true
  ;; before the number check); 2+ args must all be numbers.
  (if (and (pair? xs) (null? (cdr xs)))
      #t
      (let all-num? ((ys xs))
        (cond
          ((null? ys) (or (null? xs) (apply = xs)))
          ((number? (car ys)) (all-num? (cdr ys)))
          (else (error #f "== requires numbers" xs))))))
(def-var! "clojure.core" "==" jolt-num-equiv)
(def-var! "clojure.core" "symbol?" jolt-symbol?)
(def-var! "clojure.core" "keyword?" keyword?)
(def-var! "clojure.core" "map?" jolt-map?)
(def-var! "clojure.core" "vector?" jolt-vector?)
(def-var! "clojure.core" "set?" jolt-set?)
(def-var! "clojure.core" "seq?" jolt-seq?)
(def-var! "clojure.core" "list?" jolt-list-pred?)
(def-var! "clojure.core" "coll?" jolt-coll-pred?)
(def-var! "clojure.core" "fn?" jolt-fn?)
(def-var! "clojure.core" "boolean?" jolt-boolean-pred?)
(def-var! "clojure.core" "boolean" jolt-boolean)
(def-var! "clojure.core" "name" jolt-name)
(def-var! "clojure.core" "namespace" jolt-namespace)
