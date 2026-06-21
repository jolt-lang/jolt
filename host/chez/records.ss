;; records + protocols (jolt-cf1q.3 Phase 2 inc D) — the deftype/defrecord +
;; defprotocol/extend-type subsystem. These are ctx-capturing natives
;; that resolved to jolt-nil on the prelude, so every record
;; case hit the apply-jolt-nil crash bucket.
;;
;; A record is a `jrec`: a type tag ("ns.Name") + an alist of (kw . val) in
;; declared field order. It is map?/coll?, equal to another jrec of the same tag
;; with equal fields (never equal to a plain map), and prints as #ns.Name{...}.
;; The collection dispatchers (jolt-get/count/keys/vals/seq/assoc/contains?/=/
;; hash/conj + the printers) are set!-extended with a jrec arm that delegates to
;; the original — the transients.ss pattern — so all record logic lives here and
;; the hot collection paths are untouched. (get r :jolt/deftype) returns the tag,
;; so the overlay record? predicate works unchanged.
;;
;; Loaded after collections/seq/values/converters/printing/transients/multimethods
;; (the dispatchers it wraps + chez-current-ns).

(define-record-type jrec (fields tag pairs) (nongenerative chez-jrec-v1))
(define jolt-deftype-kw (keyword "jolt" "deftype"))

(define (jrec-lookup r k d)
  (if (jolt=2 k jolt-deftype-kw)
      (jrec-tag r)
      (let loop ((ps (jrec-pairs r)))
        (cond ((null? ps) d)
              ((jolt=2 (caar ps) k) (cdar ps))
              (else (loop (cdr ps)))))))
(define (jrec-has? r k)
  (let loop ((ps (jrec-pairs r)))
    (cond ((null? ps) #f) ((jolt=2 (caar ps) k) #t) (else (loop (cdr ps))))))
(define (jrec-replace pairs k v)        ; replace existing field (keep order) or append
  (let loop ((ps pairs) (acc '()) (hit #f))
    (cond ((null? ps) (reverse (if hit acc (cons (cons k v) acc))))
          ((jolt=2 (caar ps) k) (loop (cdr ps) (cons (cons k v) acc) #t))
          (else (loop (cdr ps) (cons (car ps) acc) hit)))))
(define (jrec=? a b)
  (and (string=? (jrec-tag a) (jrec-tag b))
       (= (length (jrec-pairs a)) (length (jrec-pairs b)))
       (let loop ((ps (jrec-pairs a)))
         (or (null? ps)
             (and (jrec-has? b (caar ps))
                  (jolt=2 (cdar ps) (jrec-lookup b (caar ps) jolt-nil))
                  (loop (cdr ps)))))))
(define (jrec-hash r)
  (fold-left (lambda (acc p) (+ acc (jolt-hash (car p)) (jolt-hash (cdr p))))
             (string-hash (jrec-tag r)) (jrec-pairs r)))
(define (jrec-pr r)                      ; #ns.Name{:k v, :k v}
  (string-append "#" (jrec-tag r) "{"
    (let loop ((ps (jrec-pairs r)) (first #t) (acc ""))
      (if (null? ps) acc
          (loop (cdr ps) #f
                (string-append acc (if first "" ", ")
                  (jolt-pr-readable (caar ps)) " " (jolt-pr-readable (cdar ps))))))
    "}"))

;; ---- extend the collection dispatchers with a jrec arm ----------------------
(define %r-jolt=2 jolt=2)
(set! jolt=2 (lambda (a b)
  (cond ((jrec? a) (and (jrec? b) (jrec=? a b)))
        ((jrec? b) #f)
        (else (%r-jolt=2 a b)))))
(define %r-jolt-hash jolt-hash)
(set! jolt-hash (lambda (x) (if (jrec? x) (jrec-hash x) (%r-jolt-hash x))))
(define %r-jolt-get jolt-get)
(set! jolt-get (case-lambda
  ((coll k)   (if (jrec? coll) (jrec-lookup coll k jolt-nil) (%r-jolt-get coll k)))
  ((coll k d) (if (jrec? coll) (jrec-lookup coll k d) (%r-jolt-get coll k d)))))
(define %r-jolt-count jolt-count)
(set! jolt-count (lambda (coll) (if (jrec? coll) (length (jrec-pairs coll)) (%r-jolt-count coll))))
(define %r-jolt-contains? jolt-contains?)
(set! jolt-contains? (lambda (coll k) (if (jrec? coll) (jrec-has? coll k) (%r-jolt-contains? coll k))))
(define %r-jolt-assoc1 jolt-assoc1)
(set! jolt-assoc1 (lambda (coll k v)
  (if (jrec? coll) (make-jrec (jrec-tag coll) (jrec-replace (jrec-pairs coll) k v)) (%r-jolt-assoc1 coll k v))))
(define %r-jolt-keys jolt-keys)
(set! jolt-keys (lambda (m) (if (jrec? m) (list->cseq (map car (jrec-pairs m))) (%r-jolt-keys m))))
(define %r-jolt-vals jolt-vals)
(set! jolt-vals (lambda (m) (if (jrec? m) (list->cseq (map cdr (jrec-pairs m))) (%r-jolt-vals m))))
(define %r-jolt-seq jolt-seq)
(set! jolt-seq (lambda (x)
  (if (jrec? x) (list->cseq (map (lambda (p) (make-map-entry (car p) (cdr p))) (jrec-pairs x))) (%r-jolt-seq x))))
(define %r-jolt-conj1 jolt-conj1)
(set! jolt-conj1 (lambda (coll x)
  (if (jrec? coll) (jolt-assoc1 coll (jolt-nth x 0) (jolt-nth x 1)) (%r-jolt-conj1 coll x))))
(define %r-jolt-pr-str jolt-pr-str)
(set! jolt-pr-str (lambda (x) (if (jrec? x) (jrec-pr x) (%r-jolt-pr-str x))))
(define %r-jolt-pr-readable jolt-pr-readable)
(set! jolt-pr-readable (lambda (x) (if (jrec? x) (jrec-pr x) (%r-jolt-pr-readable x))))

;; records are map? and coll? (Clojure: a record IS an associative map). Override
;; the public predicates to include jrec.
;; records are map? and coll? (Clojure: a record IS an associative map). The
;; predicates.ss vars hold a snapshot, so re-def-var! after extending. record? is
;; the overlay's (some? (get x :jolt/deftype)) — works for free since the get
;; override returns the tag for that key.
(define %r-jolt-map? jolt-map?)
(set! jolt-map? (lambda (x) (or (jrec? x) (%r-jolt-map? x))))
(def-var! "clojure.core" "map?" jolt-map?)
(def-var! "clojure.core" "coll?" (lambda (x) (or (jrec? x) (jolt-coll-pred? x))))

;; ---- protocol registry ------------------------------------------------------
;; type-tag -> (proto-name -> (method-name -> fn))
(define type-registry (make-hashtable string-hash string=?))
(define (register-protocol-method type-tag proto method fn)
  (let* ((ti (or (hashtable-ref type-registry type-tag #f)
                 (let ((h (make-hashtable string-hash string=?))) (hashtable-set! type-registry type-tag h) h)))
         (pi (or (hashtable-ref ti proto #f)
                 (let ((h (make-hashtable string-hash string=?))) (hashtable-set! ti proto h) h))))
    (hashtable-set! pi method fn)))
(define (find-protocol-method type-tag proto method)
  (let ((ti (hashtable-ref type-registry type-tag #f)))
    (and ti (let ((pi (hashtable-ref ti proto #f))) (and pi (hashtable-ref pi method #f))))))
(define (find-method-any-protocol type-tag method)
  (let ((ti (hashtable-ref type-registry type-tag #f)))
    (and ti (let loop ((protos (vector->list (hashtable-keys ti))))
              (and (pair? protos)
                   (let ((f (hashtable-ref (hashtable-ref ti (car protos) #f) method #f)))
                     (or f (loop (cdr protos)))))))))
(define (type-satisfies? type-tag proto)
  (let ((ti (hashtable-ref type-registry type-tag #f)))
    (and ti (hashtable-ref ti proto #f) #t)))

;; host type-tag candidates for a non-record value (extend-protocol on builtins).
(define (value-host-tags obj)
  (cond ((number? obj) '("Long" "Integer" "Number" "Double" "Object"))
        ((string? obj) '("String" "CharSequence" "Object"))
        ((boolean? obj) '("Boolean" "Object"))
        ((keyword? obj) '("Keyword" "Object"))
        ((jolt-symbol? obj) '("Symbol" "Object"))
        ((pvec? obj) '("PersistentVector" "IPersistentVector" "IPersistentCollection"
                       "List" "java.util.List" "Sequential" "Collection" "Object"))
        ((pmap? obj) '("PersistentArrayMap" "IPersistentMap" "Associative"
                       "Map" "java.util.Map" "Object"))
        ((pset? obj) '("PersistentHashSet" "IPersistentSet" "Set" "java.util.Set" "Collection" "Object"))
        ((or (cseq? obj) (empty-list-t? obj)) '("ISeq" "IPersistentCollection" "Sequential" "Collection" "Object"))
        ((jolt-nil? obj) '("nil"))
        (else '("Object"))))

(define (record-tag obj) (and (jrec? obj) (jrec-tag obj)))

;; ---- the native that handles the analyzer/overlay call ----------------------
;; make-deftype-ctor: (name-sym field-kws field-tags field-muts) -> ctor closure.
;; The tag is baked at definition time in the type's ns (chez-current-ns).
(define (make-deftype-ctor name-sym field-kws . _ignored)
  (let ((tag (string-append (chez-current-ns) "." (symbol-t-name name-sym)))
        (kws (seq->list field-kws)))
    (lambda args
      (make-jrec tag (let loop ((ks kws) (as args) (acc '()))
                       (if (null? ks) (reverse acc)
                           (loop (cdr ks) (if (null? as) '() (cdr as))
                                 (cons (cons (car ks) (if (null? as) jolt-nil (car as))) acc))))))))

;; make-protocol: a protocol value the overlay reads via (get p :name)/(get p :methods).
(define (make-protocol name-str methods)
  (jolt-hash-map (keyword #f "jolt/type") (keyword #f "jolt/protocol")
                 (keyword #f "name") (jolt-symbol jolt-nil name-str)
                 (keyword #f "methods") methods))

;; register-protocol-methods!: a no-op for Chez dispatch.
(define (register-protocol-methods! proto-name method-names) jolt-nil)

;; register-method: extend-type/extend register an impl. Host type names keep a
;; bare canonical tag; record names qualify to the current ns.
(define host-type-set
  (let ((h (make-hashtable string-hash string=?)))
    (for-each (lambda (n) (hashtable-set! h n #t))
              '("Long" "Integer" "Number" "Double" "String" "CharSequence" "Boolean"
                "Keyword" "Symbol" "Object" "nil" "PersistentVector" "IPersistentVector"
                "PersistentArrayMap" "IPersistentMap" "PersistentHashSet" "IPersistentSet"
                "ISeq" "IPersistentCollection" "Associative" "Sequential"
                "Map" "java.util.Map" "List" "java.util.List" "Set" "java.util.Set"
                "Collection" "java.util.Collection"))
    h))
(define (canonical-host-tag type-name)
  (let ((base (cond ((and (> (string-length type-name) 10) (string=? (substring type-name 0 10) "java.lang.")) (substring type-name 10 (string-length type-name)))
                    ((and (> (string-length type-name) 10) (string=? (substring type-name 0 10) "java.util.")) (substring type-name 10 (string-length type-name)))
                    (else type-name))))
    (and (hashtable-ref host-type-set base #f) base)))
(define (register-method type-name proto-name method-name fn)
  (let ((host (canonical-host-tag type-name)))
    (register-protocol-method (or host (string-append (chez-current-ns) "." type-name)) proto-name method-name fn)
    jolt-nil))

;; protocol-dispatch: look up the impl by the value's type tag (record) or host
;; candidates, invoke it; reified objects carry instance-local methods.
(define (protocol-dispatch proto-name method-name obj rest-args)
  (let ((rest (if (jolt-nil? rest-args) '() (seq->list rest-args))))
    (cond
      ((and (jrec? obj) (find-protocol-method (jrec-tag obj) proto-name method-name))
       => (lambda (f) (apply jolt-invoke f obj rest)))
      ((reified-methods obj)
       => (lambda (rm) (let ((f (hashtable-ref rm method-name #f)))
                         (if f (apply jolt-invoke f obj rest)
                             (error #f (string-append "No reified method " method-name))))))
      (else
       (let loop ((tags (value-host-tags obj)))
         (cond ((null? tags) (error #f (string-append "No method " method-name " in " proto-name)))
               ((find-protocol-method (car tags) proto-name method-name)
                => (lambda (f) (apply jolt-invoke f obj rest)))
               (else (loop (cdr tags)))))))))

;; dot-dispatch fallback used by emit for (.method record args): find the method
;; in ANY protocol the record's type implements.
(define (record-method-dispatch obj method-name rest-args)
  (let ((rest (if (jolt-nil? rest-args) '() (seq->list rest-args))))
    (cond
      ((and (jrec? obj) (find-method-any-protocol (jrec-tag obj) method-name))
       => (lambda (f) (apply jolt-invoke f obj rest)))
      ((reified-methods obj)
       => (lambda (rm) (let ((f (hashtable-ref rm method-name #f)))
                         (if f (apply jolt-invoke f obj rest) (error #f (string-append "No method " method-name))))))
      ;; java.lang.String interop (jolt-nfca): defined in natives-str.ss, loaded
      ;; after this file (free reference, resolved at call time).
      ((string? obj) (jolt-string-method method-name obj rest))
      (else (error #f (string-append "No method " method-name " for value"))))))

;; reify: instance-local method table. obj is a jreify carrying a method ht +
;; the protocol short-names it implements (for satisfies?/instance?).
(define-record-type jreify (fields methods protos) (nongenerative chez-jreify-v1))
(define (reified-methods obj) (and (jreify? obj) (jreify-methods obj)))
(define (make-reified methods-map . proto-names)
  (let ((ht (make-hashtable string-hash string=?))
        (protos (if (and (pair? proto-names) (null? (cdr proto-names)) (jolt-coll-pred? (car proto-names)))
                    (seq->list (car proto-names)) proto-names)))
    (for-each (lambda (p) (hashtable-set! ht (if (keyword? p) (keyword-t-name p) p)
                                          (jolt-get methods-map p jolt-nil)))
              (seq->list (jolt-keys methods-map)))
    (make-jreify ht (map (lambda (p) (if (symbol-t? p) (symbol-t-name p) p)) protos))))

;; satisfies?: does obj's type implement the protocol?
(define (jolt-satisfies? proto obj)
  (let* ((pn (jolt-get proto (keyword #f "name") jolt-nil))
         (pn-str (if (symbol-t? pn) (symbol-t-name pn) pn)))
    (cond
      ((jrec? obj) (type-satisfies? (jrec-tag obj) pn-str))
      ((jreify? obj)
       (let ((short (last-dot pn-str)))
         (and (memp (lambda (p) (string=? (last-dot p) short)) (jreify-protos obj)) #t)))
      (else (let loop ((tags (value-host-tags obj)))
              (cond ((null? tags) #f)
                    ((type-satisfies? (car tags) pn-str) #t)
                    (else (loop (cdr tags)))))))))
(define (last-dot s)
  (let loop ((i (- (string-length s) 1)))
    (cond ((< i 0) s) ((char=? (string-ref s i) #\.) (substring s (+ i 1) (string-length s))) (else (loop (- i 1))))))
(define (memp pred lst) (cond ((null? lst) #f) ((pred (car lst)) lst) (else (memp pred (cdr lst)))))

;; extenders: type-tags implementing a protocol, as symbols (extends? reads this).
(define (extenders proto)
  (let* ((pn (jolt-get proto (keyword #f "name") jolt-nil))
         (pn-str (if (symbol-t? pn) (symbol-t-name pn) pn))
         (out '()))
    (vector-for-each
      (lambda (tag) (when (let ((ti (hashtable-ref type-registry tag #f))) (and ti (hashtable-ref ti pn-str #f)))
                      (set! out (cons (jolt-symbol jolt-nil tag) out))))
      (hashtable-keys type-registry))
    (if (null? out) jolt-nil (list->cseq out))))

;; instance-check: (type-sym val) — type/protocol membership.
(define (instance-check type-sym val)
  (let ((tname (symbol-t-name type-sym)))
    (cond
      ((jrec? val)
       (let ((tag (jrec-tag val)))
         (or (string=? tag tname)
             (and (> (string-length tag) (string-length tname))
                  (string=? (substring tag (- (string-length tag) (string-length tname)) (string-length tag)) tname)))))
      ((jreify? val) (let ((short (last-dot tname)))
                       (and (memp (lambda (p) (string=? (last-dot p) short)) (jreify-protos val)) #t)))
      (else (case-string tname val)))))
(define (case-string tname val)
  (cond
    ((member tname '("Number" "java.lang.Number" "Long" "java.lang.Long" "Integer" "Double")) (number? val))
    ((member tname '("String" "java.lang.String" "CharSequence" "java.lang.CharSequence")) (string? val))
    ((member tname '("Boolean" "java.lang.Boolean")) (boolean? val))
    ((member tname '("Keyword")) (keyword? val))
    (else #f)))

;; str of a record uses a custom (Object toString) impl if the type defines one
;; (deftype with no default toString relies on this); otherwise the map form
;; without the leading # (Clojure's record .toString). converters.ss loads before
;; records.ss, so this set! sees the registry — forward refs resolve at call time.
(define %r-str-render-one jolt-str-render-one)
(set! jolt-str-render-one
  (lambda (v)
    (if (jrec? v)
        (let ((f (find-protocol-method (jrec-tag v) "Object" "toString")))
          (if f (jolt-invoke f v)
              (let ((s (jrec-pr v))) (substring s 1 (string-length s)))))
        (%r-str-render-one v))))

;; `type` lives in natives-meta.ss (jolt-fmm4): it needs jolt-meta for the :type
;; override and a total value->taxonomy mapping, so it sits with meta — a record
;; yields (jolt-symbol #f (jrec-tag x)), the ns.Name class-name symbol.

(def-var! "clojure.core" "make-deftype-ctor" make-deftype-ctor)
(def-var! "clojure.core" "make-protocol" make-protocol)
(def-var! "clojure.core" "register-protocol-methods!" register-protocol-methods!)
(def-var! "clojure.core" "register-method" register-method)
(def-var! "clojure.core" "protocol-dispatch" (lambda (pn mn obj rest) (protocol-dispatch pn mn obj rest)))
(def-var! "clojure.core" "satisfies?" jolt-satisfies?)
(def-var! "clojure.core" "extenders" extenders)
(def-var! "clojure.core" "instance-check" instance-check)
(def-var! "clojure.core" "make-reified" (lambda (mm . rest) (apply make-reified mm rest)))
(def-var! "clojure.core" "record-method-dispatch" (lambda (obj m rest) (record-method-dispatch obj m rest)))
