;; metadata — meta / with-meta. Chez values don't
;; carry metadata, so collections use an identity-keyed side-table: with-meta
;; returns a fresh COPY of the value (new identity) and records its meta there, so
;; the original is unchanged (Clojure's immutable-with-meta) and a copy made by a
;; later op (conj/assoc) drops the meta. Symbols carry meta in their own field.
;; meta on a non-metadatable value (number/string/keyword) is nil.
;;
;; Loaded after records.ss (jrec) + collections/seq/values (the ctors it copies).

;; Weak so a collection's metadata is reclaimed with the collection — collection
;; ops (conj/assoc/into) carry meta forward onto fresh values, so a strong table
;; would retain every meta-bearing intermediate.
(define meta-table (make-weak-eq-hashtable))

(define (jolt-meta x)
  (cond
    ((symbol-t? x) (let ((m (symbol-t-meta x))) (if (jolt-nil? m) jolt-nil m)))
    ;; a var's meta is {:ns :name} (derived from the cell) + :macro true for a
    ;; macro var (derived from var-macro-table, like Var.isMacro reading meta on
    ;; the JVM) + any def-time user meta from rt.ss's var-meta-table.
    ((var-cell? x)
     (let* ((user (hashtable-ref var-meta-table x #f))
            (base (jolt-assoc (if user user (jolt-hash-map))
                              jolt-kw-var-ns (var-cell-ns x)
                              jolt-kw-var-name (var-cell-name x))))
       (if (macro-var? x)
           (jolt-assoc base jolt-kw-var-macro #t)
           base)))
    ;; a deftype implementing clojure.lang.IObj stores meta in a field and threads
    ;; it through its own assoc/withMeta (core.logic's Substitutions/LVar/LCons),
    ;; so dispatch to its meta method rather than the identity side-table — which
    ;; the deftype's reconstructed instances would not share.
    ((and (jrec? x) (jrec-cl x "meta")) => (lambda (m) (jolt-invoke m x)))
    ;; everything else (collections, fns, reify, atoms/agents and any reference
    ;; type) reads the identity side-table; a value with no entry is nil meta.
    (else (hashtable-ref meta-table x jolt-nil))))

;; fresh-identity copy of a metadatable value (so attaching meta doesn't mutate
;; the original). cseq/procedure can't be copied meaningfully — keyed in place.
(define (meta-copy x)
  (cond
    ((pvec? x) (make-pvec (pvec-v x) (pvec-ent x)))
    ((pmap? x) (make-pmap (pmap-root x) (pmap-cnt x) (pmap-order x)))
    ((pset? x) (make-pset (pset-m x)))
    ((jrec? x) (make-jrec (jrec-desc x) (jrec-vec-copy (jrec-vals x)) (jrec-ext x)))
    ;; a reify shares its (read-only) method table + protos but gets a fresh
    ;; identity, so attaching meta leaves the original's meta untouched. Every
    ;; Clojure reify implements IObj.
    ((jreify? x) (make-jreify (jreify-methods x) (jreify-protos x)))
    ;; () is a shared singleton — a fresh instance keeps meta off every other ().
    ((empty-list-t? x) (fresh-empty-list))
    ;; a list/seq node gets a fresh identity too (Clojure's PersistentList is
    ;; immutable — (with-meta a-list m) returns a NEW list). Keying meta on the
    ;; original mutated it, so (with-meta xs {:k xs}) built a self-referential
    ;; cycle that loops *print-meta* printing.
    ((cseq? x) (make-cseq (cseq-head x) (cseq-tail x) (cseq-forced? x)
                          (cseq-list? x) (cseq-cvec x) (cseq-ci x) (cseq-crest x)))
    ((jolt-lazyseq? x) (make-jolt-lazyseq (jolt-lazyseq-thunk x) (jolt-lazyseq-val x)
                                          (jolt-lazyseq-realized? x)))
    (else x)))                          ; procedure

(define (jolt-with-meta x m)
  (cond
    ((symbol-t? x) (make-symbol-t (symbol-t-ns x) (symbol-t-name x) m))
    ;; a deftype with an explicit clojure.lang.IObj withMeta carries meta in a
    ;; field; dispatch to it (see jolt-meta) so the meta survives reconstruction.
    ((and (jrec? x) (jrec-cl x "withMeta")) => (lambda (meth) (jolt-invoke meth x m)))
    ((or (pvec? x) (pmap? x) (pset? x) (cseq? x) (empty-list-t? x) (jolt-lazyseq? x) (jrec? x) (jreify? x) (procedure? x))
     (let ((c (meta-copy x)))
       (if (jolt-nil? m) (hashtable-delete! meta-table c) (hashtable-set! meta-table c m))
       c))
    (else (error #f "with-meta: value does not support metadata" x))))

(def-var! "clojure.core" "meta" jolt-meta)
(def-var! "clojure.core" "with-meta" jolt-with-meta)

;; Carry SRC's collection metadata onto DST (a freshly-built collection of the
;; same kind), as Clojure's ops do — each new collection threads its receiver's
;; meta() forward. Returns DST. The size check is the fast path: programs that
;; never attach collection metadata pay one O(1) check per op, no lookup.
(define (meta-carry src dst)
  (if (fx=? 0 (hashtable-size meta-table))
      dst
      (let ((m (hashtable-ref meta-table src #f)))
        (if m
            ;; never attach to the shared () singleton — use a fresh instance
            (let ((d (if (empty-list-t? dst) (fresh-empty-list) dst)))
              (hashtable-set! meta-table d m) d)
            dst))))

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

;; Arm registry for host-type extensions (jinst, jolt-array, jfile, etc.)
;; A host shim registers its type's tag via register-type-arm! instead of
;; set!-wrapping jolt-type — disjoint types, checked before the base cases,
;; so the full behavior is gathered here plus the registry rather than
;; scattered across a set! chain (cf. register-hash-arm!).
;; Arms dispatch newest-registration-first: a later-loaded type's predicate
;; wins when predicates overlap (transients/records both answer some ops).
(define jolt-type-arms '())
(define (register-type-arm! pred handler)
  (set! jolt-type-arms (cons (cons pred handler) jolt-type-arms)))
(define (jolt-type-base x)
  (let* ((m (jolt-meta x))
         (override (if (jolt-nil? m) jolt-nil (jolt-get m ty-kw-type jolt-nil))))
    (cond
      ((not (jolt-nil? override)) override)            ; :type meta wins
      ;; record -> its ns-qualified class-name STRING (= (class x)). jolt models
      ;; classes as strings, so (symbol (str (type r))) is NOT (type r) — as on the
      ;; JVM where type is a Class, not a Symbol.
      ((jrec? x) (jrec-tag x))
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
(define (jolt-type x)
  (let* ((m (jolt-meta x))
         (override (if (jolt-nil? m) jolt-nil (jolt-get m ty-kw-type jolt-nil))))
    (cond
      ((not (jolt-nil? override)) override)             ; :type meta wins
      (else (let loop ((as jolt-type-arms))
              (cond ((null? as) (jolt-type-base x))
                    (((caar as) x) ((cdar as) x))
                    (else (loop (cdr as)))))))))

;; jolt-type is the keyword TAXONOMY (:string/:set/:jolt/inst/…) — jolt's native
;; value model, with no JVM in it. print-method/print-dup dispatch on it (via
;; __type-tag). The PUBLIC clojure.core/type is Clojure's (or (:type meta) (class
;; x)) — a JVM class — but that mapping belongs to the java host layer (host-class.ss
;; rebinds `type` next to `class`), so this core layer stays JVM-free.
(def-var! "clojure.core" "__type-tag" jolt-type)
(def-var! "clojure.core" "type" jolt-type)
