;; metadata (jolt-cf1q.3 Phase 2 inc E) — meta / with-meta. Chez values don't
;; carry metadata, so collections use an identity-keyed side-table: with-meta
;; returns a fresh COPY of the value (new identity) and records its meta there, so
;; the original is unchanged (Clojure's immutable-with-meta) and a copy made by a
;; later op (conj/assoc) drops the meta. Symbols carry meta in their own field.
;; meta on a non-metadatable value (number/string/keyword) is nil.
;;
;; Loaded after records.ss (jrec) + collections/seq/values (the ctors it copies).

(define meta-table (make-eq-hashtable))

(define (jolt-meta x)
  (cond
    ((symbol-t? x) (let ((m (symbol-t-meta x))) (if (jolt-nil? m) jolt-nil m)))
    ;; a var's meta is {:ns :name} (derived from the cell) + any def-time user
    ;; meta from rt.ss's var-meta-table (jolt-zikh).
    ((var-cell? x)
     (let ((user (hashtable-ref var-meta-table x #f)))
       (jolt-assoc (if user user (jolt-hash-map))
                   jolt-kw-var-ns (var-cell-ns x)
                   jolt-kw-var-name (var-cell-name x))))
    ((or (pvec? x) (pmap? x) (pset? x) (cseq? x) (empty-list-t? x) (jrec? x) (procedure? x))
     (hashtable-ref meta-table x jolt-nil))
    (else jolt-nil)))

;; fresh-identity copy of a metadatable value (so attaching meta doesn't mutate
;; the original). cseq/procedure can't be copied meaningfully — keyed in place.
(define (meta-copy x)
  (cond
    ((pvec? x) (make-pvec (pvec-v x) (pvec-ent x)))
    ((pmap? x) (make-pmap (pmap-root x) (pmap-cnt x)))
    ((pset? x) (make-pset (pset-m x)))
    ((jrec? x) (make-jrec (jrec-tag x) (jrec-pairs x)))
    (else x)))                          ; cseq / empty-list / procedure

(define (jolt-with-meta x m)
  (cond
    ((symbol-t? x) (make-symbol-t (symbol-t-ns x) (symbol-t-name x) m))
    ((or (pvec? x) (pmap? x) (pset? x) (cseq? x) (empty-list-t? x) (jrec? x) (procedure? x))
     (let ((c (meta-copy x)))
       (if (jolt-nil? m) (hashtable-delete! meta-table c) (hashtable-set! meta-table c m))
       c))
    (else (error #f "with-meta: value does not support metadata" x))))

(def-var! "clojure.core" "meta" jolt-meta)
(def-var! "clojure.core" "with-meta" jolt-with-meta)

;; (type x) — Clojure's (or (:type (meta x)) (class x)). With no JVM classes the
;; "class" is a host taxonomy: a record yields its ns-qualified class-name SYMBOL
;; (user.TyR), everything else a keyword (:number/:vector/:seq/…).
;; MUST be total — a non-record value
;; falling through to a crash would read as a divergence, not the right keyword.
;; Forward refs (jolt-lazyseq?, the sorted-htable / wrapper predicates) all bind by
;; call time (every host .ss loads before any user expr runs).
(define ty-kw-type (keyword #f "type"))           ; the :type meta key
(define ty-kw-jtype (keyword "jolt" "type"))       ; tagged-map discriminator (ex-info)
(define ty-number (keyword #f "number"))
(define ty-string (keyword #f "string"))
(define ty-keyword (keyword #f "keyword"))
(define ty-symbol (keyword #f "symbol"))
(define ty-boolean (keyword #f "boolean"))
(define ty-char (keyword #f "char"))
(define ty-vector (keyword #f "vector"))
(define ty-map (keyword #f "map"))
(define ty-set (keyword #f "set"))
(define ty-seq (keyword #f "seq"))
(define ty-fn (keyword #f "fn"))
(define ty-atom (keyword "jolt" "atom"))
(define ty-volatile (keyword "jolt" "volatile"))
(define ty-regex (keyword "jolt" "regex"))
(define ty-var (keyword "jolt" "var"))
(define ty-transient (keyword "jolt" "transient"))
(define ty-uuid (keyword "jolt" "uuid"))
(define ty-sorted-set (keyword "jolt" "sorted-set"))
(define ty-object (keyword #f "object"))

(define (jolt-type x)
  (let* ((m (jolt-meta x))
         (override (if (jolt-nil? m) jolt-nil (jolt-get m ty-kw-type jolt-nil))))
    (cond
      ((not (jolt-nil? override)) override)            ; :type meta wins
      ;; record -> ns.Name symbol. No-ns sentinel is #f (not jolt-nil) so it = the
      ;; overlay's (symbol (str t)) — jolt= compares the ns field with equal?.
      ((jrec? x) (jolt-symbol #f (jrec-tag x)))
      ((jolt-nil? x) jolt-nil)
      ((boolean? x) ty-boolean)
      ((number? x) ty-number)
      ((string? x) ty-string)
      ((keyword? x) ty-keyword)
      ((symbol-t? x) ty-symbol)
      ((char? x) ty-char)
      ;; host wrappers — keyed by their :jolt/* tags (checked before the
      ;; collection arms; none of these are pvec/pmap/pset).
      ((jolt-atom? x) ty-atom)
      ((jvol? x) ty-volatile)
      ((jolt-regex? x) ty-regex)
      ((var-cell? x) ty-var)
      ((jolt-transient? x) ty-transient)
      ((juuid? x) ty-uuid)
      ((htable-sorted-set? x) ty-sorted-set)
      ((htable-sorted-map? x) ty-map)
      ;; collections — pvec INCLUDES map entries (:vector).
      ((pvec? x) ty-vector)
      ((pmap? x)                                        ; a :jolt/type-tagged map (ex-info) -> its tag
       (let ((t (jolt-get x ty-kw-jtype jolt-nil))) (if (jolt-nil? t) ty-map t)))
      ((pset? x) ty-set)
      ((or (cseq? x) (empty-list-t? x) (jolt-lazyseq? x)) ty-seq)
      ((procedure? x) ty-fn)
      (else ty-object))))

(def-var! "clojure.core" "type" jolt-type)
