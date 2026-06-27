;; dot-forms.ss — generic dispatch for the `.` special-form / `.-field` desugar.
;; The analyzer lowers (. target member arg*) and (.-field target)
;; to a :host-call; the Chez emit routes a non-shimmed :host-call through
;; record-method-dispatch. This file extends that dispatcher with the collection
;; arms the interpreter's dispatch-member covers but the record/string base does
;; not, with this precedence:
;;
;;   * collection interop wins first — count/seq/nth/get/valAt/containsKey on a
;;     vector/map/set/seq/record (so (. {:count 9} count) is the entry count, 1,
;;     NOT the :count field).
;;   * field access — a "-name" member reads the field (records and maps).
;;   * map member   — a stored fn is a method (called with self + args); any
;;                    other value is returned as a field.
;;
;; Anything not recognized falls through to the previous dispatcher (jhost /
;; number / regex / jrec protocol / string). Loaded LAST (after host-static.ss).
;; A record (jrec) is jolt-map? here (records.ss makes it so) and a collection,
;; so its protocol method (no dash, not a coll method) lands in the base.

(define %dot-rmd record-method-dispatch)

;; Vectors / maps / sets only (records are jolt-map? here). Raw seqs are excluded:
;; coll-interop accepts some seq representations and not others (a
;; plain (seq v) returns nil from .count, a lazy-seq returns the count), an
;; inconsistency Chez's normalized cseq can't mirror — so a raw seq target falls
;; through to the base dispatcher rather than risk a divergence the corpus would
;; never exercise but a future case might.
(define (dot-coll? obj)
  (or (jolt-vector? obj) (jolt-map? obj) (pset? obj)))

;; Mirror coll-interop: return a one-element list boxing the result (so a jolt-nil
;; result is still distinguishable from "not a collection method"), or #f.
(define (dot-coll-method obj name args)
  (cond
    ((string=? name "count") (list (jolt-count obj)))
    ((string=? name "seq")   (list (jolt-seq obj)))
    ((string=? name "nth")   (list (apply jolt-nth obj args)))
    ((or (string=? name "get") (string=? name "valAt"))
     (list (apply jolt-get obj args)))
    ((string=? name "containsKey") (list (jolt-contains? obj (car args))))
    ;; java.util.Collection.contains(o): VALUE membership (a set is O(1) via
    ;; contains?; a list/vector/seq is a linear scan — contains? on a vector tests
    ;; an index, so it is wrong here).
    ((string=? name "contains")
     (list (if (pset? obj)
               (jolt-contains? obj (car args))
               (let ((x (car args)))
                 (let loop ((s (jolt-seq obj)))
                   (cond ((jolt-nil? s) #f)
                         ((jolt=2 (seq-first s) x) #t)
                         (else (loop (jolt-seq (seq-more s))))))))))
    ((string=? name "size")    (list (jolt-count obj)))
    ((string=? name "isEmpty") (list (jolt-empty? obj)))
    ;; java.util.Map views: keySet (a Set), values (a Collection), entrySet.
    ((and (jolt-map? obj) (string=? name "keySet"))
     (list (apply jolt-hash-set (seq->list (jolt-keys obj)))))
    ((and (jolt-map? obj) (string=? name "values"))
     (list (apply jolt-vector (seq->list (jolt-vals obj)))))
    ((and (jolt-map? obj) (string=? name "entrySet")) (list (jolt-seq obj)))
    ;; (.iterator coll): a java.util.Iterator over the seq — for a map this is the
    ;; entry iterator. Without this a map's .iterator falls into the map-as-object
    ;; branch and is mis-read as a missing :iterator key (nil). Some libraries
    ;; (e.g. malli's -vmap) iterate a map this way.
    ((string=? name "iterator") (list (make-jiterator (jolt-seq obj))))
    (else #f)))

;; Universal object-methods: on a
;; non-record map these win OVER a field lookup, like dispatch-member. getMessage
;; on an ex-info reads its :message (the one the corpus exercises); getCause reads
;; :cause; toString/hashCode/equals round out the set. Returns a boxed result or
;; #f. Strings/numbers/records/jhost keep the base dispatcher (it shims them).
(define (dot-object-method obj name args)
  (cond
    ((string=? name "getMessage")
     (list (if (jolt=2 (jolt-get obj jolt-kw-ex-type jolt-nil) jolt-kw-ex-info)
               (jolt-get obj jolt-kw-message jolt-nil)
               (jolt-str-render-one obj))))
    ((string=? name "getCause")  (list (jolt-get obj jolt-kw-cause jolt-nil)))
    ;; java.sql.SQLException chaining — ex-info / host throwables don't chain.
    ((string=? name "getNextException") (list jolt-nil))
    ((string=? name "getStackTrace") (list (jolt-vector)))
    ((string=? name "toString")  (list (jolt-str-render-one obj)))
    ((string=? name "hashCode")  (list (jolt-hash obj)))
    ((string=? name "equals")    (list (if (jolt= obj (car args)) #t #f)))
    (else #f)))

(set! record-method-dispatch
  (lambda (obj method-name rest-args)
    (let* ((rest (if (jolt-nil? rest-args) '() (seq->list rest-args)))
           (field? (and (> (string-length method-name) 0)
                        (char=? (string-ref method-name 0) #\-)))
           (mname (if field?
                      (substring method-name 1 (string-length method-name))
                      method-name)))
      (cond
        ;; (.getClass x) universal — the class token for any value, before the
        ;; collection/map field-lookup arms below would read it as a missing key.
        ((string=? method-name "getClass") (jolt-class obj))
        ;; collection interop first (entry count / seq / nth / get / containsKey).
        ((and (dot-coll? obj) (dot-coll-method obj mname rest))
         => (lambda (box) (car box)))
        ;; (.-field obj) / (. obj -field): field read on a record or map.
        (field? (jolt-get obj (keyword #f mname) jolt-nil))
        ;; non-record map: a universal object-method (getMessage/...) wins first,
        ;; then a stored procedure is a method (call with self), else the field.
        ((and (jolt-map? obj) (not (jrec? obj)))
         (cond
           ((dot-object-method obj mname rest) => car)
           (else
            (let ((v (jolt-get obj (keyword #f mname) jolt-nil)))
              (if (procedure? v) (apply jolt-invoke v obj rest) v)))))
        (else (%dot-rmd obj method-name rest-args))))))
