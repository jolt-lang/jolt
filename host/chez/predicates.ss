;; type predicates + simple accessors (jolt-9ziu) — host-coupled seed natives.
;;
;; These are seed primitives (not clojure.core overlay fns), so they're never
;; def-var!'d by the assembled prelude; the Chez host must provide them. Semantics
;; match the Janet seed (src/jolt/core_types.janet): map?/vector?/set? are STRICT
;; over the persistent-collection records, seq? is true only for real sequences,
;; coll? is the union. Records (shape-recs) are Phase 2, so the record arms of the
;; seed predicates are simply absent here for now.

(define (jolt-map? x) (pmap? x))
(define (jolt-vector? x) (pvec? x))
(define (jolt-set? x) (pset? x))
(define (jolt-seq? x) (or (cseq? x) (empty-list-t? x)))
(define (jolt-coll-pred? x)
  (or (pvec? x) (pmap? x) (pset? x) (cseq? x) (empty-list-t? x)))
(define (jolt-number? x) (number? x))
(define (jolt-string? x) (string? x))
(define (jolt-char-pred? x) (char? x))
;; finite integral number — Chez integer? already rejects the infinities and NaN.
(define (jolt-integer? x) (and (number? x) (integer? x)))
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
