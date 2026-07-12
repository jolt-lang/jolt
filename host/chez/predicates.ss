;; type predicates + simple accessors — host-coupled natives.
;;
;; These are host primitives (not clojure.core overlay fns), so they're never
;; def-var!'d by the assembled prelude; the Chez host must provide them.
;; map?/vector?/set? are STRICT over the persistent-collection records, seq? is
;; true only for real sequences, coll? is the union. Record arms are added by
;; records.ss, which extends these dispatchers.

;; ---- map? arms: host types register here instead of set!-wrapping jolt-map? ---
;; Each entry is just a predicate (returns truthy → map? is true).
(define jolt-map-pred-arms '())
(define (register-map-pred-arm! pred)
  (set! jolt-map-pred-arms (cons pred jolt-map-pred-arms)))

(define (jolt-map? x)
  (or (pmap? x)
      (let loop ((as jolt-map-pred-arms))
        (and (pair? as)
             (or ((car as) x)
                 (loop (cdr as)))))))
;; a map entry is a pvec under the hood AND is vector? — Clojure's MapEntry
;; implements IPersistentVector, so (vector? (first {:a 1})) is true.
(define (jolt-vector? x) (pvec? x))
(define (jolt-set? x) (pset? x))
(define (jolt-seq? x) (or (cseq? x) (empty-list-t? x)))
;; list? lives in the overlay (clojure/core/20-coll.clj) — see jolt.host/cseq? etc.
(define (jolt-coll-pred? x)
  (or (pvec? x) (pmap? x) (pset? x) (cseq? x) (empty-list-t? x) (jolt-lazyseq? x)))
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
;; ratio?/rational? live in the overlay (clojure/core/20-coll.clj), built on the
;; jolt.host tower tests. decimal? stays native: the optional bigdec module
;; (java/bigdec.ss) re-binds it to jbigdec?, so it can't be a static overlay const.
(define (jolt-decimal? x) #f)
(define (jolt-fn? x) (procedure? x))
(define (jolt-boolean-pred? x) (boolean? x))

;; (boolean x) coerces truthiness (nil/false -> false, else true). MUST stay native:
;; the backend's emit path calls clojure.core/boolean for every :if node
;; (backend_scheme.clj bool tracking), so it has to exist before ANY compilation,
;; including the kernel overlay tier (whose own fns contain `if`). Migrating it even
;; to the kernel tier deadlocks: compiling the tier that defines boolean needs boolean.
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
(def-var! "clojure.core" "coll?" jolt-coll-pred?)
(def-var! "clojure.core" "fn?" jolt-fn?)
(def-var! "clojure.core" "boolean?" jolt-boolean-pred?)
(def-var! "clojure.core" "boolean" jolt-boolean)
(def-var! "clojure.core" "name" jolt-name)
(def-var! "clojure.core" "namespace" jolt-namespace)

;; --- jolt.host raw type-test primitives -------------------------------------
;; Some clojure.core predicates bottom out at host tests overlay Clojure can't
;; reach. Expose the ones the migratable predicates need so the overlay versions
;; lower to exactly these calls — no perf loss. rational-type? is the Chez TYPE
;; test (exact rational), distinct from clojure.core/rational? (which gates on
;; number? first). exact? is wrapped TOTAL (Chez's raw exact? errors on a
;; non-number); rational-type? already returns #f for a non-match.
;;
;; Only the tests consumed by the migrated predicates (ratio?/rational? -> exact?,
;; rational-type?; list? -> cseq?/cseq-list?/empty-list?) are exposed. The rest of
;; the predicate web stays native and is NOT exposed: map?/set?/seq?/coll? are
;; extended at runtime with sorted/record/lazy arms, decimal? is extended by the
;; optional bigdec module, integer?/float? are on the compiler emit/inference path,
;; and vector? is reached by the kernel-tier peek during bootstrap.
(define (jh-exact? x) (and (number? x) (exact? x)))
(def-var! "jolt.host" "exact?" jh-exact?)
(def-var! "jolt.host" "rational-type?" rational?)
(def-var! "jolt.host" "cseq?" cseq?)
(def-var! "jolt.host" "empty-list?" empty-list-t?)
(def-var! "jolt.host" "cseq-list?" cseq-list?)
