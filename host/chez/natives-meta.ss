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
