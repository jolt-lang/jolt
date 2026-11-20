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
(define (jolt-char-pred? x) (or (char? x) (jsurr? x)))   ; a surrogate unit is a Character too
;; JVM-parity number-type predicates over the Chez numeric tower. integer? is the
;; INTEGER TYPE (exact integer = Long/BigInt), NOT integer-VALUED: (integer? 3.0)
;; is false on the JVM (3.0 is a Double). float? = flonum (double). ratio? = exact
;; non-integer (= JVM Ratio). rational? = exact (integer or ratio; jolt has no
;; BigDecimal). decimal? is always false (no BigDecimal type).
(define (jolt-integer? x) (and (number? x) (exact? x) (integer? x)))
;; int? is a fixed-precision integer, Long/Integer/Short/Byte on the JVM, which
;; here is any integer in the signed 64-bit range: past it the value classes as
;; clojure.lang.BigInt. Its own procedure, not integer?'s — the JVM's two are
;; distinct fns, and a map or set literal keyed by both (spec.alpha's generator
;; table) must not see one key twice.
(define (jolt-int? x)
  (or (fixnum? x)
      (and (integer? x) (exact? x)
           (<= -9223372036854775808 x 9223372036854775807))))
;; float? is Double or Float; double? only a Double (a jfloat is a Float).
(define (jolt-float? x) (or (flonum? x) (jfloat? x)))
(define (jolt-double? x) (flonum? x))
;; ratio?/rational? live in the overlay (clojure/core/20-coll.clj), built on the
;; jolt.host tower tests. decimal? stays native: the optional bigdec module
;; (java/bigdec.ss) re-binds it to jbigdec?, so it can't be a static overlay const.
(define (jolt-decimal? x) #f)
;; fn? is (instance? clojure.lang.Fn x) on the JVM, so a deftype or reify that
;; declares the Fn marker answers true too, as instance? already does here.
;; own-type-declares? is records-interop.ss's, loaded later; it is reached at
;; call time, and only for a value that is not already a procedure.
(define (jolt-fn? x) (or (procedure? x) (own-type-declares? "clojure.lang.Fn" x)))
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
    (else (jolt-cast-throw x "clojure.lang.Named"))))

;; (namespace x): keyword/symbol ns string, or nil when unqualified.
(define (jolt-namespace x)
  (let ((ns (cond ((keyword? x) (keyword-t-ns x))
                  ((symbol-t? x) (symbol-t-ns x))
                  (else (jolt-cast-throw x "clojure.lang.Named")))))
    (if (or (jolt-nil? ns) (not ns) (eq? ns '())) jolt-nil ns)))

(def-var! "clojure.core" "nil?" jolt-nil?-fn)
(def-var! "clojure.core" "number?" jolt-number?)
(def-var! "clojure.core" "string?" jolt-string?)
(def-var! "clojure.core" "char?" jolt-char-pred?)
(def-var! "clojure.core" "integer?" jolt-integer?)
(def-var! "clojure.core" "float?" jolt-float?)
(def-var! "clojure.core" "decimal?" jolt-decimal?)
;; == numeric value-equality (ignores category/exactness, unlike =): (== 3 3.0),
;; (== 3M 3) -> true. 1-arity is trivially true; 2+ args must all be numbers
;; (Numbers.equiv throws otherwise). A BigDecimal operand compares by value across
;; the tower — mirroring compare: when a double is present both sides compare as
;; doubles, otherwise as bigdec — so (== 3M 3) is true while (= 3M 3) stays false.
;; That value-equality lives with the shim (jbd-equiv2 in java/bigdec.ss),
;; registered through this hook (register-num-arm! 'num-equiv-slow). The pairwise
;; loop below runs once ANY operand is a shim number, so individual pairs can
;; still be two plain numbers ((== 1 1 1M) compares 1 vs 1 first) and the shim's
;; arm declines those to this base — it handles them with the same
;; inexact-contaminates rule the all-plain branch uses, and throws only when an
;; operand is neither a number nor a shim number.
(define (jolt-num-equiv-slow a b)
  (cond
    ((and (number? a) (number? b))
     (if (or (inexact? a) (inexact? b))
         (= (exact->inexact a) (exact->inexact b))
         (= a b)))
    (else (throw-jvm (quote ClassCastException) "== requires numbers"))))
(define (jolt-num-equiv-numeric? x) (or (number? x) (jolt-num-slow? x)))
;; One pair, in the reference's order (Numbers.equiv(Object, Object) casts x and
;; then y to Number, then dispatches on their categories): a non-number on
;; either side is the ClassCastException, left first, and only then is a nil the
;; NullPointerException -- (== :a nil) is the cast error, (== 1 nil) the NPE.
(define (jolt-num-equiv-pair a b)
  (unless (or (jolt-num-equiv-numeric? a) (jolt-nil? a)) (jolt-num-cast-throw a))
  (unless (or (jolt-num-equiv-numeric? b) (jolt-nil? b)) (jolt-num-cast-throw b))
  (when (jolt-nil? a) (jolt-num-cast-throw a))
  (when (jolt-nil? b) (jolt-num-cast-throw b))
  (cond
    ((or (jolt-num-slow? a) (jolt-num-slow? b))
     ;; a shim-number operand (BigDecimal): by value through its slow arm
     (jolt-num-equiv-slow a b))
    ;; JVM == on a long and a double compares as doubles (the category ops
    ;; promote), so it agrees with the fl=? the numeric pass emits in call
    ;; position: (== 9007199254740993 9e15-ish) is true because the long rounds
    ;; to the same double. Chez = would promote the double to exact instead.
    ((or (inexact? a) (inexact? b)) (= (exact->inexact a) (exact->inexact b)))
    (else (= a b))))
(define (jolt-num-equiv x0 . more)
  ;; 1-arity short-circuits to true for ANY value (Clojure's == 1-arg returns true
  ;; before the number check). More than one: pairwise, left to right, stopping
  ;; at the first unequal pair as the reference's variadic arity does, so
  ;; (== 1 2 :a) is false and never looks at :a. (==) is an arity error, as on
  ;; the JVM: x0 is required.
  (let loop ((a x0) (ys more))
    (cond ((null? ys) #t)
          ((jolt-num-equiv-pair a (car ys)) (loop (car ys) (cdr ys)))
          (else #f))))
;; Two-arg == when operand types aren't proven: fixnum and flonum pairs answer
;; directly, everything else goes to jolt-num-equiv above.
(define (jolt-num-equiv2 a b)
  (cond ((and (fixnum? a) (fixnum? b)) (fx=? a b))
        ((and (flonum? a) (flonum? b)) (fl=? a b))
        (else (jolt-num-equiv a b))))
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
;; rational-type?; list? -> list?) are exposed. The rest of
;; the predicate web stays native and is NOT exposed: map?/set?/seq?/coll? are
;; extended at runtime with sorted/record/lazy arms, decimal? is extended by the
;; optional bigdec module, integer?/float? are on the compiler emit/inference path,
;; and vector? is reached by the kernel-tier peek during bootstrap.
(define (jh-exact? x) (and (number? x) (exact? x)))
(def-var! "jolt.host" "exact?" jh-exact?)
(def-var! "jolt.host" "rational-type?" rational?)
;; list?: a list-marked cseq node or the empty list. The cseq/empty-list-t
;; representation is an internal detail — the overlay asks one semantic question
;; rather than reaching for the concrete-rep predicates.
(define (jh-list? x) (or (and (cseq? x) (cseq-list? x)) (empty-list-t? x)))
(def-var! "jolt.host" "list?" jh-list?)
