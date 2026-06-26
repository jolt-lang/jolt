;; records + protocols — the deftype/defrecord + defprotocol/extend-type
;; subsystem.
;;
;; A record is a `jrec`: a shared per-type descriptor + a flat vector of field
;; values in declared order, plus an extension map for any non-field keys assoc'd
;; on (jolt-nil when there are none — the common case). This lays fields out like a
;; native struct: construction allocates one vector, not a chain of cons cells, and
;; a field read is an index lookup, not a list scan. It is map?/coll?, equal to
;; another jrec of the same type with equal fields (never equal to a plain map),
;; and prints as #ns.Name{...}.
;; The collection dispatchers (jolt-get/count/keys/vals/seq/assoc/contains?/=/
;; hash/conj + the printers) are set!-extended with a jrec arm that delegates to
;; the original — the transients.ss pattern — so all record logic lives here and
;; the hot collection paths are untouched. (get r :jolt/deftype) returns the tag,
;; so the overlay record? predicate works unchanged.
;;
;; Loaded after collections/seq/values/converters/printing/transients/multimethods
;; (the dispatchers it wraps + chez-current-ns).

;; The per-type descriptor: built once at deftype/defrecord definition and shared
;; by every instance. Holds the tag, the field keywords in declared order, and an
;; eq?-keyed keyword->index table (field keys are interned, so identity lookup).
(define-record-type (jrdesc make-jrdesc-rec jrdesc?)
  (fields tag fkeys index) (nongenerative chez-jrdesc-v1))
(define (make-jrdesc tag fkey-list)
  (let ((index (make-eq-hashtable)))
    (let loop ((ks fkey-list) (i 0))
      (unless (null? ks) (hashtable-set! index (car ks) i) (loop (cdr ks) (+ i 1))))
    (make-jrdesc-rec tag (list->vector fkey-list) index)))
;; An instance: the shared descriptor, the field-value vector, and an extension
;; map (jolt-nil unless non-field keys have been assoc'd on).
(define-record-type (jrec make-jrec jrec?) (fields desc vals ext) (nongenerative chez-jrec-v2))
(define (jrec-tag r) (jrdesc-tag (jrec-desc r)))
(define jolt-deftype-kw (keyword "jolt" "deftype"))
;; unique present-vs-absent sentinel for extension-map lookups (so a present nil
;; in the extension map is distinguished from a genuine miss).
(define jrec-absent (list 'jrec-absent))

;; --- whole-program inference registries -------------------------------------
;; Populated at definition/load time (deftype/defrecord and defprotocol forms run
;; before `jolt build` re-emits), read by the inference driver to seed record and
;; protocol-method types across fn boundaries. A no-op for the runtime itself; the
;; tables just accumulate. jolt.host/record-shapes and /protocol-methods (host-
;; contract.ss) materialize them into the shape jolt.passes.types expects.

;; ctor-key "ns/->Name" -> (vector field-kw-list field-tag-list type-tag).
;; field-tag-list parallels the fields: "num", a record simple-name string, or #f.
(define chez-record-shapes-tbl (make-hashtable string-hash string=?))
;; method var-key "ns/method" -> (cons proto-name method-name).
(define chez-protocol-methods-tbl (make-hashtable string-hash string=?))

(define (register-record-shape! ctor-key field-kws field-tags type-tag)
  (hashtable-set! chez-record-shapes-tbl ctor-key
                  (vector field-kws field-tags type-tag)))

;; simple name of a dotted/slashed string: the segment after the last . or /.
(define (chez-shape-simple-name s)
  (let loop ((i (- (string-length s) 1)))
    (cond ((< i 0) s)
          ((or (char=? (string-ref s i) #\.) (char=? (string-ref s i) #\/))
           (substring s (+ i 1) (string-length s)))
          (else (loop (- i 1))))))

;; resolve a field's declared type tag to what jolt.passes.types wants: "num"
;; passes through; a record name (simple "Vec3" or qualified "ns.Vec3") resolves
;; to its ctor-key (so the field reads back as that record); anything else -> nil.
(define (chez-resolve-field-tag tag by-name)
  (cond ((or (not tag) (jolt-nil-t? tag)) jolt-nil)
        ((string=? tag "num") "num")
        (else (let ((ck (hashtable-ref by-name (chez-shape-simple-name tag) #f)))
                (if ck ck jolt-nil)))))

;; materialize chez-record-shapes-tbl into "ns/->Name" -> {:fields :tags :type},
;; the shape record-type-from-entry consumes.
(define (chez-record-shapes-map)
  (let ((by-name (make-hashtable string-hash string=?))
        (kw-fields (keyword #f "fields")) (kw-tags (keyword #f "tags")) (kw-type (keyword #f "type"))
        (out (jolt-hash-map)))
    ;; index simple record name (from the type tag "ns.Name") -> ctor-key for
    ;; nested-field-tag resolution.
    (let-values (((ks vs) (hashtable-entries chez-record-shapes-tbl)))
      (vector-for-each
        (lambda (k v) (hashtable-set! by-name (chez-shape-simple-name (vector-ref v 2)) k)) ks vs)
      (vector-for-each
        (lambda (k v)
          (let* ((fields (vector-ref v 0)) (tags (vector-ref v 1)) (type-tag (vector-ref v 2))
                 (rtags (map (lambda (t) (chez-resolve-field-tag t by-name)) tags)))
            (set! out (jolt-assoc out k
                                  (jolt-hash-map kw-fields (apply jolt-vector fields)
                                                 kw-tags   (apply jolt-vector rtags)
                                                 kw-type   type-tag)))))
        ks vs))
    out))

;; materialize chez-protocol-methods-tbl into "ns/method" -> [proto method].
(define (chez-protocol-methods-map)
  (let ((out (jolt-hash-map)))
    (let-values (((ks vs) (hashtable-entries chez-protocol-methods-tbl)))
      (vector-for-each
        (lambda (k v) (set! out (jolt-assoc out k (jolt-vector (car v) (cdr v)))))
        ks vs))
    out))

;; index of a declared field key, or #f (only an interned keyword can be one).
(define (jrec-field-index r k) (hashtable-ref (jrdesc-index (jrec-desc r)) k #f))
;; a vector-copy that doesn't depend on the optional rnrs vector-copy being present.
(define (jrec-vec-copy v)
  (let* ((n (vector-length v)) (out (make-vector n)))
    (let loop ((i 0)) (when (< i n) (vector-set! out i (vector-ref v i)) (loop (+ i 1))))
    out))
;; extension-map entries as an (k . v) alist in iteration order.
(define (jrec-ext-pairs ext)
  (let loop ((s (jolt-seq ext)) (acc '()))
    (if (jolt-nil? s) (reverse acc)
        (let ((e (seq-first s)))
          (loop (jolt-seq (seq-more s)) (cons (cons (jolt-nth e 0) (jolt-nth e 1)) acc))))))

;; lookup with default d: a declared field reads index+vector-ref (a present nil
;; returns nil), then the extension map, then d.
(define (jrec-lookup r k d)
  (if (eq? k jolt-deftype-kw)
      (jrec-tag r)
      (let ((i (jrec-field-index r k)))
        (if i (vector-ref (jrec-vals r) i)
            (let ((ext (jrec-ext r)))
              (if (jolt-nil? ext) d
                  (let ((v (jolt-get ext k jrec-absent)))
                    (if (eq? v jrec-absent) d v))))))))
(define (jrec-has? r k)
  (and (not (eq? k jolt-deftype-kw))
       (or (and (jrec-field-index r k) #t)
           (let ((ext (jrec-ext r)))
             (and (not (jolt-nil? ext))
                  (not (eq? jrec-absent (jolt-get ext k jrec-absent))))))))
;; The get path: like jrec-lookup, but a deftype's ILookup valAt runs when a key
;; is genuinely missing from both the fields and the extension map.
(define (jrec-ref coll k d)
  (if (eq? k jolt-deftype-kw)
      (jrec-tag coll)
      (let ((i (jrec-field-index coll k)))
        (if i (vector-ref (jrec-vals coll) i)
            (let* ((ext (jrec-ext coll))
                   (v (if (jolt-nil? ext) jrec-absent (jolt-get ext k jrec-absent))))
              (if (eq? v jrec-absent)
                  (cond ((find-method-any-protocol (jrec-tag coll) "valAt")
                         => (lambda (m) (jolt-invoke m coll k d)))
                        (else d))
                  v))))))
;; bare-index field read for a statically-known record field — emitted by `jolt
;; build --opt` for a struct-typed receiver, where i is the field's declared slot.
;; When r is the expected record it reads the value vector directly: no field-key
;; hashtable lookup, no jolt-get dispatch. Falls back to jolt-get otherwise (a map
;; downgraded by dissoc, or a value the inference mistyped), so it stays correct
;; even if the static type is wrong.
(define (jrec-field-at r i k)
  (if (and (jrec? r) (fx< i (vector-length (jrec-vals r))))
      (vector-ref (jrec-vals r) i)
      (jolt-get r k)))

;; mutate a deftype's mutable field in place: the value vector is mutable, so
;; vector-set! updates the field. (set! field v) inside a method lowers to this;
;; returns v, as set! does.
(define (jolt-set-field! inst k v)
  (if (jrec? inst)
      (let ((i (jrec-field-index inst k)))
        (if i (begin (vector-set! (jrec-vals inst) i v) v)
            (error #f "set! of an unknown field" k)))
      (error #f "set! of a field on a non-record" inst)))
(define (jrec-ext=? ea eb)
  (cond ((and (jolt-nil? ea) (jolt-nil? eb)) #t)
        ((or (jolt-nil? ea) (jolt-nil? eb)) #f)
        (else (jolt=2 ea eb))))
(define (jrec=? a b)
  (and (string=? (jrec-tag a) (jrec-tag b))
       (= (vector-length (jrec-vals a)) (vector-length (jrec-vals b)))
       (let ((va (jrec-vals a)) (vb (jrec-vals b)) (n (vector-length (jrec-vals a))))
         (let loop ((i 0))
           (or (= i n)
               (and (jolt=2 (vector-ref va i) (vector-ref vb i)) (loop (+ i 1))))))
       (jrec-ext=? (jrec-ext a) (jrec-ext b))))
(define (jrec-hash r)
  (let* ((fkeys (jrdesc-fkeys (jrec-desc r))) (vals (jrec-vals r)) (n (vector-length vals))
         (base (let loop ((i 0) (acc (string-hash (jrec-tag r))))
                 (if (= i n) acc
                     (loop (+ i 1) (+ acc (jolt-hash (vector-ref fkeys i))
                                       (jolt-hash (vector-ref vals i))))))))
    (let ((ext (jrec-ext r)))
      (if (jolt-nil? ext) base (+ base (jolt-hash ext))))))
(define (jrec-pr r)                      ; #ns.Name{:k v, :k v}
  (let ((fkeys (jrdesc-fkeys (jrec-desc r))) (vals (jrec-vals r)))
    (string-append "#" (jrec-tag r) "{"
      (let ((n (vector-length vals)))
        (let loop ((i 0) (first #t) (acc ""))
          (if (= i n)
              (let ((ext (jrec-ext r)))
                (if (jolt-nil? ext) acc
                    (let eloop ((es (jrec-ext-pairs ext)) (first first) (acc acc))
                      (if (null? es) acc
                          (eloop (cdr es) #f
                                 (string-append acc (if first "" ", ")
                                   (jolt-pr-readable (caar es)) " " (jolt-pr-readable (cdar es))))))))
              (loop (+ i 1) #f
                    (string-append acc (if first "" ", ")
                      (jolt-pr-readable (vector-ref fkeys i)) " " (jolt-pr-readable (vector-ref vals i)))))))
      "}")))

;; ---- extend the collection dispatchers with a jrec arm ----------------------
;; equality for a jrec: a deftype implementing IPersistentCollection/equiv (e.g.
;; core.cache's caches, which equiv to their backing map) compares through that
;; method, so (= cache {…}) works; a plain record has no equiv and falls back to
;; field-wise jrec=? (and a record is never = a plain map).
(register-eq-arm! (lambda (a b) (or (jrec? a) (jrec? b)))
                  (lambda (a b)
                    (cond ((and (jrec? a) (jrec-cl a "equiv")) => (lambda (m) (if (jolt-truthy? (jolt-invoke m a b)) #t #f)))
                          ((and (jrec? b) (jrec-cl b "equiv")) => (lambda (m) (if (jolt-truthy? (jolt-invoke m b a)) #t #f)))
                          ((and (jrec? a) (jrec? b)) (jrec=? a b))
                          (else #f))))
(register-hash-arm! jrec? jrec-hash)
;; get on a jrec: a real field reads raw (so a deftype method's own field bindings,
;; compiled to (get inst :field), never recurse); a NON-field key on a deftype that
;; implements clojure.lang.ILookup routes to its valAt (core.match's pattern types
;; compute ::tag in valAt), else the default.
;; jrec is the hottest get target (every record field read); jolt-get-dispatch
;; (collections.ss) checks jrec? directly and calls jrec-ref, skipping the get-arm
;; walk. This registration is the equivalent fallback for any other caller.
(register-get-arm! jrec? jrec-ref)
;; A jrec is a defrecord (map of fields) by default, BUT a deftype that
;; implements a clojure.lang collection interface carries the op as an inline
;; method — prefer that method, else fall back to the field/map behavior. (jrec-cl
;; finds the method; find-method-any-protocol / jolt-invoke resolve at call time.)
(define (jrec-cl coll name) (and (jrec? coll) (find-method-any-protocol (jrec-tag coll) name)))
(define %r-jolt-count jolt-count)
(set! jolt-count (lambda (coll)
  (cond ((jrec-cl coll "count") => (lambda (m) (jolt-invoke m coll)))
        ((jrec? coll) (+ (vector-length (jrec-vals coll))
                         (let ((ext (jrec-ext coll))) (if (jolt-nil? ext) 0 (%r-jolt-count ext)))))
        (else (%r-jolt-count coll)))))
;; contains?: a deftype implementing Associative/containsKey (e.g. core.cache's
;; caches) answers through that; a plain defrecord checks its fields.
(define %r-jolt-contains? jolt-contains?)
(set! jolt-contains? (lambda (coll k)
  (cond ((jrec-cl coll "containsKey") => (lambda (m) (if (jolt-truthy? (jolt-invoke m coll k)) #t #f)))
        ((jrec? coll) (jrec-has? coll k))
        (else (%r-jolt-contains? coll k)))))
;; assoc: replacing a declared field copies the value vector; any other key grows
;; the extension map (the value vector is shared — fields are immutable).
(define %r-jolt-assoc1 jolt-assoc1)
(set! jolt-assoc1 (lambda (coll k v)
  (cond ((jrec-cl coll "assoc") => (lambda (m) (jolt-invoke m coll k v)))
        ((jrec? coll)
         (let ((i (and (keyword? k) (jrec-field-index coll k))))
           (if i
               (let ((nv (jrec-vec-copy (jrec-vals coll))))
                 (vector-set! nv i v)
                 (make-jrec (jrec-desc coll) nv (jrec-ext coll)))
               (let ((ext (jrec-ext coll)))
                 (make-jrec (jrec-desc coll) (jrec-vals coll)
                            (%r-jolt-assoc1 (if (jolt-nil? ext) empty-pmap ext) k v))))))
        (else (%r-jolt-assoc1 coll k v)))))
;; dissoc: a deftype implementing IPersistentMap/without answers through it.
;; Removing a declared field downgrades a plain record to a map (JVM parity); an
;; extension key drops from the ext map (normalized back to jolt-nil when empty).
(define (jrec->map-without r drop-k)
  (let* ((fkeys (jrdesc-fkeys (jrec-desc r))) (vals (jrec-vals r)) (n (vector-length vals)))
    (let loop ((i 0) (m empty-pmap))
      (if (= i n)
          (let ((ext (jrec-ext r)))
            (if (jolt-nil? ext) m
                (fold-left (lambda (mm p) (%r-jolt-assoc1 mm (car p) (cdr p))) m (jrec-ext-pairs ext))))
          (let ((fk (vector-ref fkeys i)))
            (loop (+ i 1) (if (eq? fk drop-k) m (%r-jolt-assoc1 m fk (vector-ref vals i)))))))))
(define %r-jolt-dissoc jolt-dissoc)
(define (jrec-dissoc1 coll k)
  (if (not (jrec? coll))
      (%r-jolt-dissoc coll k)            ; an earlier declared-field dissoc downgraded it
      (let ((i (and (keyword? k) (jrec-field-index coll k))))
        (if i (jrec->map-without coll k)
            (let ((ext (jrec-ext coll)))
              (if (jolt-nil? ext) coll
                  (let ((ne (%r-jolt-dissoc ext k)))
                    (make-jrec (jrec-desc coll) (jrec-vals coll)
                               (if (= 0 (%r-jolt-count ne)) jolt-nil ne)))))))))
(set! jolt-dissoc (lambda (coll . ks)
  (cond ((jrec-cl coll "without")
         => (lambda (m) (fold-left (lambda (c k) (jolt-invoke m c k)) coll ks)))
        ((jrec? coll) (fold-left jrec-dissoc1 coll ks))
        (else (apply %r-jolt-dissoc coll ks)))))
;; keys/vals over a jrec read its entry seq (jolt-seq is method-first, so a
;; map-like deftype delegates to its Seqable; a defrecord's seq is its fields, so
;; the result is unchanged for records).
(define (jrec-seq-col m which)
  (let loop ((s (jolt-seq m)) (acc '()))
    (if (jolt-nil? s) (list->cseq (reverse acc))
        (loop (jolt-seq (seq-more s)) (cons (jolt-nth (seq-first s) which) acc)))))
(define %r-jolt-keys jolt-keys)
(set! jolt-keys (lambda (m) (if (jrec? m) (jrec-seq-col m 0) (%r-jolt-keys m))))
(define %r-jolt-vals jolt-vals)
(set! jolt-vals (lambda (m) (if (jrec? m) (jrec-seq-col m 1) (%r-jolt-vals m))))
;; a record's seq is its field map-entries in declared order, then any extensions.
(define (jrec-entry-list r)
  (let* ((fkeys (jrdesc-fkeys (jrec-desc r))) (vals (jrec-vals r)) (n (vector-length vals)))
    (let loop ((i 0) (acc '()))
      (if (= i n)
          (let ((ext (jrec-ext r)))
            (append (reverse acc)
                    (if (jolt-nil? ext) '()
                        (map (lambda (p) (make-map-entry (car p) (cdr p))) (jrec-ext-pairs ext)))))
          (loop (+ i 1) (cons (make-map-entry (vector-ref fkeys i) (vector-ref vals i)) acc))))))
(define %r-jolt-seq jolt-seq)
(set! jolt-seq (lambda (x)
  (cond ((jrec-cl x "seq") => (lambda (m) (jolt-seq (jolt-invoke m x))))
        ((jrec? x) (list->cseq (jrec-entry-list x)))
        (else (%r-jolt-seq x)))))
(define %r-jolt-conj1 jolt-conj1)
(set! jolt-conj1 (lambda (coll x)
  (cond ((jrec-cl coll "cons") => (lambda (m) (jolt-invoke m coll x)))
        ((jrec? coll) (jolt-assoc1 coll (jolt-nth x 0) (jolt-nth x 1)))
        (else (%r-jolt-conj1 coll x)))))
;; peek/pop on a deftype implementing IPersistentStack (data.priority-map, which
;; core.cache's LRU/LU caches lean on) dispatch to its methods.
;; empty? over a jrec: a map-like deftype is empty iff its entry seq is (data
;; .priority-map's peek calls (.isEmpty this) -> empty?). jolt-seq is method-first.
(define %r-jolt-empty? jolt-empty?)
(set! jolt-empty? (lambda (coll) (if (jrec? coll) (jolt-nil? (jolt-seq coll)) (%r-jolt-empty? coll))))
(define %r-jolt-peek jolt-peek)
(set! jolt-peek (lambda (coll)
  (cond ((jrec-cl coll "peek") => (lambda (m) (jolt-invoke m coll)))
        (else (%r-jolt-peek coll)))))
(define %r-jolt-pop jolt-pop)
(set! jolt-pop (lambda (coll)
  (cond ((jrec-cl coll "pop") => (lambda (m) (jolt-invoke m coll)))
        (else (%r-jolt-pop coll)))))
(register-pr-arm! jrec? jrec-pr)

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
  ;; numbers dispatch by actual type (a Double is NOT a Long): flonum -> Double,
  ;; exact ratio -> Ratio, exact integer -> Long.
  (cond ((flonum? obj) '("Double" "Float" "Number" "Object"))
        ((and (number? obj) (exact? obj) (not (integer? obj))) '("Ratio" "Number" "Object"))
        ((number? obj) '("Long" "Integer" "BigInteger" "BigInt" "Number" "Object"))
        ((string? obj) '("String" "CharSequence" "Object"))
        ((boolean? obj) '("Boolean" "Object"))
        ((keyword? obj) '("Keyword" "Named" "Object"))
        ((jolt-symbol? obj) '("Symbol" "Named" "Object"))
        ((pvec? obj) '("PersistentVector" "APersistentVector" "IPersistentVector" "IPersistentCollection"
                       "List" "java.util.List" "Sequential" "Collection" "Iterable" "java.lang.Iterable" "Object"))
        ((pmap? obj) '("PersistentArrayMap" "APersistentMap" "IPersistentMap" "Associative"
                       "Map" "java.util.Map" "Iterable" "java.lang.Iterable" "Object"))
        ((pset? obj) '("PersistentHashSet" "APersistentSet" "IPersistentSet" "Set" "java.util.Set" "Collection" "Iterable" "java.lang.Iterable" "Object"))
        ((or (cseq? obj) (empty-list-t? obj)) '("ASeq" "ISeq" "IPersistentCollection" "Sequential" "Collection" "Iterable" "java.lang.Iterable" "Object"))
        ;; java.net.URI jhost — extend-protocol java.net.URI (hiccup ToURI/ToStr).
        ((and (jhost? obj) (string=? (jhost-tag obj) "uri")) '("URI" "java.net.URI" "Object"))
        ;; a ByteBuffer — extend-protocol java.nio.ByteBuffer (aws-api util).
        ((and (jhost? obj) (string=? (jhost-tag obj) "byte-buffer")) '("ByteBuffer" "java.nio.ByteBuffer" "Object"))
        ;; arrays dispatch by their JVM array-class name — extend-protocol to
        ;; (Class/forName "[B") for byte[] (data.json, aws-api), "[C" for char[].
        ((and (jolt-array? obj) (eq? (jolt-array-kind obj) 'byte)) '("[B" "Object"))
        ((and (jolt-array? obj) (eq? (jolt-array-kind obj) 'char)) '("[C" "Object"))
        ((jolt-array? obj) '("[Ljava.lang.Object;" "Object"))
        ;; a regex VALUE — extend-protocol java.util.regex.Pattern (core.match.regex).
        ((regex-t? obj) '("Pattern" "java.util.regex.Pattern" "Object"))
        ;; host value types a library may extend a protocol to by class (data.json
        ;; extends JSONWriter to java.util.UUID / java.util.Date / java.math.BigDecimal).
        ((juuid? obj) '("UUID" "java.util.UUID" "Object"))
        ((jinst? obj) '("Date" "java.util.Date" "Timestamp" "java.sql.Timestamp" "Object"))
        ((jbigdec? obj) '("BigDecimal" "java.math.BigDecimal" "Number" "Object"))
        ((and (jhost? obj) (string=? (jhost-tag obj) "instant")) '("Instant" "java.time.Instant" "Object"))
        ((and (jhost? obj) (string=? (jhost-tag obj) "local-date")) '("LocalDate" "java.time.LocalDate" "Object"))
        ((and (jhost? obj) (string=? (jhost-tag obj) "local-time")) '("LocalTime" "java.time.LocalTime" "Object"))
        ((and (jhost? obj) (string=? (jhost-tag obj) "local-date-time")) '("LocalDateTime" "java.time.LocalDateTime" "Object"))
        ((and (jhost? obj) (string=? (jhost-tag obj) "duration")) '("Duration" "java.time.Duration" "TemporalAmount" "java.time.temporal.TemporalAmount" "Object"))
        ((and (jhost? obj) (string=? (jhost-tag obj) "period")) '("Period" "java.time.Period" "TemporalAmount" "java.time.temporal.TemporalAmount" "Object"))
        ((and (jhost? obj) (string=? (jhost-tag obj) "month-enum")) '("Month" "java.time.Month" "Object"))
        ((and (jhost? obj) (string=? (jhost-tag obj) "dow-enum")) '("DayOfWeek" "java.time.DayOfWeek" "Object"))
        ((and (jhost? obj) (string=? (jhost-tag obj) "year")) '("Year" "java.time.Year" "Object"))
        ((and (jhost? obj) (string=? (jhost-tag obj) "year-month")) '("YearMonth" "java.time.YearMonth" "Object"))
        ((and (jhost? obj) (string=? (jhost-tag obj) "chrono-unit")) '("ChronoUnit" "java.time.temporal.ChronoUnit" "TemporalUnit" "java.time.temporal.TemporalUnit" "Object"))
        ((and (jhost? obj) (string=? (jhost-tag obj) "chrono-field")) '("ChronoField" "java.time.temporal.ChronoField" "TemporalField" "java.time.temporal.TemporalField" "Object"))
        ((and (jhost? obj) (string=? (jhost-tag obj) "zone-offset")) '("ZoneOffset" "java.time.ZoneOffset" "ZoneId" "java.time.ZoneId" "Object"))
        ((and (jhost? obj) (string=? (jhost-tag obj) "zone-id")) '("ZoneId" "java.time.ZoneId" "Object"))
        ((and (jhost? obj) (string=? (jhost-tag obj) "zoned-date-time")) '("ZonedDateTime" "java.time.ZonedDateTime" "Object"))
        ((and (jhost? obj) (string=? (jhost-tag obj) "offset-date-time")) '("OffsetDateTime" "java.time.OffsetDateTime" "Object"))
        ((and (jhost? obj) (string=? (jhost-tag obj) "offset-time")) '("OffsetTime" "java.time.OffsetTime" "Object"))
        ((and (jhost? obj) (string=? (jhost-tag obj) "clock")) '("Clock" "java.time.Clock" "Object"))
        ;; java.sql.Date — a distinct class from java.util.Date so a protocol
        ;; extended to both (data.json's JSONWriter) routes a sql.Date to its impl.
        ((and (jhost? obj) (string=? (jhost-tag obj) "sql-date")) '("java.sql.Date" "Date" "java.util.Date" "Object"))
        ;; a bare procedure (fn) — extend-protocol to clojure.lang.{Fn,IFn,AFn}.
        ((procedure? obj) '("Fn" "IFn" "AFn" "Object"))
        ((jolt-nil? obj) '("nil"))
        (else '("Object"))))

(define (record-tag obj) (and (jrec? obj) (jrec-tag obj)))

;; ---- the native that handles the analyzer/overlay call ----------------------
;; make-deftype-ctor: (name-sym field-kws field-tags field-muts) -> ctor closure.
;; The tag is baked at definition time in the type's ns (chez-current-ns).
(define (make-deftype-ctor name-sym field-kws . rest-args)
  (let* ((tag (string-append (chez-current-ns) "." (symbol-t-name name-sym)))
         (kws (seq->list field-kws))
         (field-tags (if (pair? rest-args) (seq->list (car rest-args)) '()))
         (desc (make-jrdesc tag kws))
         (nf (length kws))
         (ctor (lambda args
                 ;; fill the value vector from the positional args, padding missing
                 ;; trailing fields with nil and ignoring any extras.
                 (let ((v (make-vector nf jolt-nil)))
                   (let loop ((as args) (i 0))
                     (if (or (null? as) (= i nf)) (make-jrec desc v jolt-nil)
                         (begin (vector-set! v i (car as)) (loop (cdr as) (+ i 1)))))))))
    ;; Register the ctor globally by simple class name (like StringBuilder) so
    ;; (Name. …) interop resolves ns-agnostically: a deftype used across files works
    ;; even when the runtime current ns is the caller's, not the defining ns
    ;; (host-new checks class-ctors-tbl before the current-ns var fallback).
    (register-class-ctor! (symbol-t-name name-sym) ctor)
    ;; record the shape for whole-program inference, keyed by the positional
    ;; ctor var "ns/->Name" the analyzer resolves a (->Name …) call to.
    (register-record-shape! (string-append (chez-current-ns) "/->" (symbol-t-name name-sym))
                            kws field-tags tag)
    ctor))

;; make-protocol: a protocol value the overlay reads via (get p :name)/(get p :methods).
(define (make-protocol name-str methods)
  (jolt-hash-map (keyword #f "jolt/type") (keyword #f "jolt/protocol")
                 (keyword #f "name") (jolt-symbol jolt-nil name-str)
                 (keyword #f "methods") methods))

;; register-protocol-methods!: record each method's var-key -> [proto method] for
;; the inference driver (devirtualization). Dispatch itself is by the receiver's
;; type tag at call time, so this table is read only by `jolt build` inference.
;; Called by defprotocol-emitted code in the protocol's ns.
(define (register-protocol-methods! proto-name method-names)
  (let ((ns (chez-current-ns)))
    (for-each (lambda (mn)
                (let ((m (if (symbol-t? mn) (symbol-t-name mn) mn)))
                  (hashtable-set! chez-protocol-methods-tbl
                                  (string-append ns "/" m) (cons proto-name m))))
              (seq->list method-names)))
  jolt-nil)

;; register-method: extend-type/extend register an impl. Host type names keep a
;; bare canonical tag; record names qualify to the current ns.
(define host-type-set
  (let ((h (make-hashtable string-hash string=?)))
    (for-each (lambda (n) (hashtable-set! h n #t))
              '("Long" "Integer" "Number" "Double" "Ratio" "BigInt" "BigInteger"
                "String" "CharSequence" "Boolean" "Character"
                "Keyword" "Symbol" "Named" "Object" "nil"
                "Fn" "IFn" "AFn" "URI"
                "PersistentVector" "APersistentVector" "IPersistentVector"
                "PersistentArrayMap" "APersistentMap" "IPersistentMap"
                "PersistentHashSet" "APersistentSet" "IPersistentSet"
                "ASeq" "ISeq" "IPersistentCollection" "Associative" "Sequential"
                "Map" "java.util.Map" "List" "java.util.List" "Set" "java.util.Set"
                "Collection" "java.util.Collection" "Iterable" "java.lang.Iterable"
                "UUID" "BigDecimal" "Date" "Timestamp" "Instant" "java.sql.Date"
                "Pattern" "java.util.regex.Pattern"
                ;; java.time value types (extend-protocol Duration / ZonedDateTime / …)
                "Duration" "Period" "LocalDate" "LocalTime" "LocalDateTime"
                "ZonedDateTime" "OffsetDateTime" "OffsetTime" "ZoneId" "ZoneOffset"
                "Clock" "Year" "YearMonth" "Month" "DayOfWeek"
                "ChronoUnit" "ChronoField" "TemporalAmount" "TemporalUnit" "TemporalField"
                ;; ByteBuffer + JVM array classes (extend-protocol to (Class/forName "[B"))
                "ByteBuffer" "java.nio.ByteBuffer"
                "[B" "[C" "[I" "[J" "[D" "[Ljava.lang.Object;"))
    h))
(define (strip-prefix s p)
  (let ((pl (string-length p)))
    (and (> (string-length s) pl) (string=? (substring s 0 pl) p) (substring s pl (string-length s)))))
(define (canonical-host-tag type-name)
  (let ((base (or (strip-prefix type-name "java.lang.")
                  (strip-prefix type-name "java.util.regex.")
                  (strip-prefix type-name "java.util.")
                  (strip-prefix type-name "java.net.")
                  (strip-prefix type-name "java.math.")
                  (strip-prefix type-name "java.time.")
                  (strip-prefix type-name "clojure.lang.")
                  type-name)))
    (and (hashtable-ref host-type-set base #f) base)))
;; An extend/extend-type/extend-protocol registration marks the tag as an
;; extender of the protocol (recorded inside type-registry so the per-case prune
;; restores it). deftype/defrecord inline impls go through register-inline-method
;; and skip the mark: the JVM compiles inline protocol methods into the class, so
;; extenders excludes them.
(define extend-mark "__jolt_extend__")
(define (mark-extend! tag proto-name)
  (let ((ti (hashtable-ref type-registry tag #f)))
    (when ti (let ((pi (hashtable-ref ti proto-name #f)))
               (when pi (hashtable-set! pi extend-mark #t))))))
(define (register-method type-name proto-name method-name fn)
  (let* ((host (canonical-host-tag type-name))
         (tag (or host (string-append (chez-current-ns) "." type-name))))
    (register-protocol-method tag proto-name method-name fn)
    (mark-extend! tag proto-name)
    jolt-nil))

;; register-inline-method: a deftype/defrecord inline impl. Registers for dispatch
;; under the ns-qualified record tag but does NOT mark it as an extender.
(define (register-inline-method type-name proto-name method-name fn)
  (register-protocol-method (string-append (chez-current-ns) "." type-name) proto-name method-name fn)
  jolt-nil)
;; record that a deftype/defrecord implements a protocol even when it adds no
;; methods (a MARKER protocol, e.g. core.match's IPseudoPattern) — so
;; instance?/satisfies? on the protocol hold.
(define (register-inline-protocol! type-name proto-name)
  (let* ((tag (string-append (chez-current-ns) "." type-name))
         (ti (or (hashtable-ref type-registry tag #f)
                 (let ((h (make-hashtable string-hash string=?))) (hashtable-set! type-registry tag h) h))))
    (unless (hashtable-ref ti proto-name #f)
      (hashtable-set! ti proto-name (make-hashtable string-hash string=?))))
  jolt-nil)

;; protocol-resolve: the impl procedure for obj — by record type tag, a reify's
;; instance-local method, or the protocol's extended impls over obj's host tags.
;; Raises if none implements the method. The dispatchN entry points apply it
;; directly so a protocol call doesn't cons a rest-list (the impl fn is always a
;; procedure, registered by register-(inline-)method/extend).
(define (protocol-resolve proto-name method-name obj)
  (cond
    ((and (jrec? obj) (find-protocol-method (jrec-tag obj) proto-name method-name)))
    ((reified-methods obj)
     => (lambda (rm)
          (or (hashtable-ref rm method-name #f)
              ;; not implemented on the reify — fall back to the protocol's
              ;; extended impls over the reify's host tags (e.g. an Object/default
              ;; extension). malli reifies some protocols and leans on the default.
              (let loop ((tags (value-host-tags obj)))
                (cond ((null? tags) (error #f (string-append "No reified method " method-name)))
                      ((find-protocol-method (car tags) proto-name method-name))
                      (else (loop (cdr tags))))))))
    (else
     (let loop ((tags (value-host-tags obj)))
       (cond ((null? tags) (error #f (string-append "No method " method-name " in " proto-name)))
             ((find-protocol-method (car tags) proto-name method-name))
             (else (loop (cdr tags))))))))
;; Fixed-arity entry points the protocol-method shims call: no rest-list, no seq
;; round-trip — apply the resolved impl directly. defprotocol emits one clause per
;; declared arity that calls the matching dispatchN.
(define (protocol-dispatch1 proto-name method-name obj)
  ((protocol-resolve proto-name method-name obj) obj))
(define (protocol-dispatch2 proto-name method-name obj a)
  ((protocol-resolve proto-name method-name obj) obj a))
(define (protocol-dispatch3 proto-name method-name obj a b)
  ((protocol-resolve proto-name method-name obj) obj a b))
;; the variadic fallback (a declared arity of 4+ args) takes a seqable rest.
(define (protocol-dispatch proto-name method-name obj rest-args)
  (let ((rest (if (jolt-nil? rest-args) '() (seq->list rest-args))))
    (apply (protocol-resolve proto-name method-name obj) obj rest)))

;; dot-dispatch fallback used by emit for (.method record args): find the method
;; in ANY protocol the record's type implements.
;; java.util.Iterator over a jolt seqable: (.iterator coll) returns a jiterator
;; holding a mutable cursor over (seq coll); (.hasNext it)/(.next it) walk it.
;; hiccup/compiler's run! loop iterates collections this way.
(define-record-type jiterator (fields (mutable cur)) (nongenerative jolt-iterator-v1))
;; (seq an-iterator) / (iterator-seq it): a jiterator wraps the remaining seq in
;; cur, so seq just yields it — clojure.test's (iterator-seq (.iterator coll)).
(let ((prev-seq jolt-seq))
  (set! jolt-seq (lambda (x) (if (jiterator? x) (jiterator-cur x) (prev-seq x)))))
;; A Chez condition's message string (for Throwable .getMessage/.toString): the
;; &message text plus any &irritants, or display-condition output as a fallback.
(define (condition->message-string c)
  (if (message-condition? c)
      (let* ((m (condition-message c))
             (irr (if (irritants-condition? c) (condition-irritants c) '()))
             (append-irr (lambda ()
                           (let loop ((xs irr) (acc m))
                             (if (null? xs) acc
                                 (loop (cdr xs) (string-append acc " " (jolt-pr-str (car xs)))))))))
        ;; some Chez conditions (open-input-file etc.) carry a format-template
        ;; message ("failed for ~a: ~(~a~)") whose irritants fill the directives;
        ;; format it in. Fall back to appending the irritants if that fails.
        (if (and (string? m) (let scan ((i 0))
                               (cond ((>= i (string-length m)) #f)
                                     ((char=? (string-ref m i) #\~) #t)
                                     (else (scan (+ i 1))))))
            (guard (e (#t (append-irr))) (apply format m irr))
            (append-irr)))
      (with-output-to-string (lambda () (display-condition c)))))
;; expose a Chez condition's message to Clojure (ex-message returns nil for raw
;; host conditions): the nREPL eval handler surfaces it instead of an opaque
;; "#<compound condition>".
(def-var! "jolt.host" "condition-message"
  (lambda (c) (if (condition? c) (condition->message-string c) jolt-nil)))
(define (record-method-dispatch obj method-name rest-args)
  (let ((rest (if (jolt-nil? rest-args) '() (seq->list rest-args))))
    (cond
      ;; (.getClass x): universal Object method — the class token for any value
      ;; (jolt has no Class objects; the token is the canonical name string, on
      ;; which .getName/.getSimpleName work via the String method shim).
      ((and (string=? method-name "getClass") (not (jrec? obj)) (not (jreify? obj)))
       (jolt-class obj))
      ((and (jrec? obj) (find-method-any-protocol (jrec-tag obj) method-name))
       => (lambda (f) (apply jolt-invoke f obj rest)))
      ;; (.field inst): a deftype/record field read with no matching method.
      ;; Clojure reads the field for (.q x) just like (.-q x); a declared method
      ;; (above) wins, this is the field-accessor fallback.
      ((and (jrec? obj) (null? rest) (jrec-has? obj (keyword #f method-name)))
       (jrec-lookup obj (keyword #f method-name) jolt-nil))
      ((reified-methods obj)
       => (lambda (rm) (let ((f (hashtable-ref rm method-name #f)))
                         (if f (apply jolt-invoke f obj rest) (error #f (string-append "No method " method-name))))))
      ;; java.lang.String interop: defined in natives-str.ss, loaded
      ;; after this file (free reference, resolved at call time).
      ((string? obj) (jolt-string-method method-name obj rest))
      ((jiterator? obj)
       (cond ((string=? method-name "hasNext") (not (jolt-nil? (jolt-seq (jiterator-cur obj)))))
             ((string=? method-name "next")
              (let ((s (jolt-seq (jiterator-cur obj))))
                (if (jolt-nil? s) (error #f "iterator exhausted")
                    (let ((v (jolt-first s))) (jiterator-cur-set! obj (jolt-rest s)) v))))
             (else (error #f (string-append "No method " method-name " on Iterator")))))
      ((string=? method-name "iterator") (make-jiterator (jolt-seq obj)))
      ;; clojure.lang.Keyword interop: a Keyword carries an interned `sym` field
      ;; (the symbol form, ns + name) plus the Named methods. honeysql/reitit read
      ;; (.sym k) on their :clj branch to recover the symbol without the colon.
      ((keyword-t? obj)
       (cond ((string=? method-name "sym")
              (jolt-symbol (keyword-t-ns obj) (keyword-t-name obj)))
             ((string=? method-name "getName") (keyword-t-name obj))
             ((string=? method-name "getNamespace") (or (keyword-t-ns obj) jolt-nil))
             ((string=? method-name "toString")
              (string-append ":" (if (keyword-t-ns obj) (string-append (keyword-t-ns obj) "/") "")
                             (keyword-t-name obj)))
             ((string=? method-name "hashCode") (keyword-t-khash obj))
             ((string=? method-name "equals") (and (pair? rest) (eq? obj (car rest))))
             (else (error #f (string-append "No method " method-name " on Keyword")))))
      ;; clojure.lang.Symbol interop: the Named methods + getName/getNamespace.
      ((symbol-t? obj)
       (cond ((string=? method-name "getName") (symbol-t-name obj))
             ((string=? method-name "getNamespace") (or (symbol-t-ns obj) jolt-nil))
             ((string=? method-name "toString")
              (string-append (if (symbol-t-ns obj) (string-append (symbol-t-ns obj) "/") "")
                             (symbol-t-name obj)))
             ((string=? method-name "equals") (and (pair? rest) (jolt=2 obj (car rest))))
             (else (error #f (string-append "No method " method-name " on Symbol")))))
      ;; clojure.lang.Namespace: name/getName yield the ns name as a Symbol (JVM:
      ;; Namespace.name is a Symbol). clojure.spec.alpha reads (.name *ns*).
      ((jns? obj)
       (cond ((or (string=? method-name "name") (string=? method-name "getName"))
              (jolt-symbol #f (jns-name obj)))
             ((string=? method-name "toString") (jns-name obj))
             (else (error #f (string-append "No method " method-name " on Namespace")))))
      ;; clojure.lang.Var: ns -> its Namespace, sym -> the simple-name Symbol.
      ;; clojure.spec.alpha's ->sym reads (.name (.ns v)) and (.sym v).
      ((var-cell? obj)
       (cond ((string=? method-name "ns") (intern-ns! (var-cell-ns obj)))
             ((or (string=? method-name "sym") (string=? method-name "name"))
              (jolt-symbol #f (var-cell-name obj)))
             ((string=? method-name "getName")
              (jolt-symbol (var-cell-ns obj) (var-cell-name obj)))
             ((string=? method-name "toString") (string-append "#'" (var-cell-ns obj) "/" (var-cell-name obj)))
             (else (error #f (string-append "No method " method-name " on Var")))))
      ;; java.lang.Throwable interop over a Chez condition. A jolt host error
      ;; (`error`/`assertion-violationf`) raises a Chez condition; Clojure code
      ;; that catches it as a Throwable reads (.getMessage e) / (.toString e).
      ((condition? obj)
       (cond ((or (string=? method-name "getMessage") (string=? method-name "getLocalizedMessage"))
              (condition->message-string obj))
             ((string=? method-name "toString") (condition->message-string obj))
             ((string=? method-name "getCause") jolt-nil)
             ;; java.sql.SQLException chaining — jolt errors don't chain (nil).
             ((string=? method-name "getNextException") jolt-nil)
             ((string=? method-name "getStackTrace") (jolt-vector))
             ((string=? method-name "printStackTrace") jolt-nil)
             (else (error #f (string-append "No method " method-name " on Throwable")))))
      ;; java.lang.Character interop: (.toString \+) -> "+", etc.
      ((char? obj)
       (cond ((string=? method-name "toString") (string obj))
             ((string=? method-name "charValue") obj)
             ((string=? method-name "hashCode") (char->integer obj))
             ((string=? method-name "equals") (and (pair? rest) (char? (car rest)) (char=? obj (car rest))))
             ((string=? method-name "compareTo")
              (let ((o (car rest))) (cond ((char<? obj o) -1) ((char>? obj o) 1) (else 0))))
             (else (error #f (string-append "No method " method-name " on char")))))
      ;; java.util.List .indexOf / .lastIndexOf over any seqable (vector / list /
      ;; seq) — -1 when absent, like the JVM (medley/index-of reads this).
      ((or (string=? method-name "indexOf") (string=? method-name "lastIndexOf"))
       (let ((target (car rest)) (last? (string=? method-name "lastIndexOf")))
         (let loop ((s (jolt-seq obj)) (i 0) (found -1))
           (cond ((jolt-nil? s) found)
                 ((jolt=2 (seq-first s) target)
                  (if last? (loop (jolt-seq (seq-more s)) (fx+ i 1) i) i))
                 (else (loop (jolt-seq (seq-more s)) (fx+ i 1) found))))))
      ;; universal Object methods on any remaining value (boolean, etc.).
      ((string=? method-name "toString") (jolt-str-render-one obj))
      ((string=? method-name "hashCode") (jolt-hash obj))
      ((string=? method-name "equals") (and (pair? rest) (if (jolt= obj (car rest)) #t #f)))
      (else (error #f (string-append "No method " method-name " for value: "
                                     (jolt-pr-str obj)))))))

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

;; extenders: type-tags that extend a protocol via extend/extend-type/extend-
;; protocol, as symbols (extends? reads this). Inline deftype/defrecord impls are
;; excluded — only tags carrying the extend mark count, matching the JVM.
(define (extenders proto)
  (let* ((pn (jolt-get proto (keyword #f "name") jolt-nil))
         (pn-str (if (symbol-t? pn) (symbol-t-name pn) pn))
         (out '()))
    (vector-for-each
      (lambda (tag)
        (let ((ti (hashtable-ref type-registry tag #f)))
          (when ti (let ((pi (hashtable-ref ti pn-str #f)))
                     (when (and pi (hashtable-ref pi extend-mark #f))
                       (set! out (cons (jolt-symbol jolt-nil tag) out)))))))
      (hashtable-keys type-registry))
    (if (null? out) jolt-nil (list->cseq out))))

;; jolt exception values (ex-info + host-constructed throwables) are ex-info-shaped
;; maps tagged :jolt/type :jolt/ex-info; (class …)/instance? read the JVM class off
;; the optional :jolt/class key, defaulting to clojure.lang.ExceptionInfo.
(register-str-render! jrec?
  (lambda (v)
    (let ((f (find-protocol-method (jrec-tag v) "Object" "toString")))
      (if f (jolt-invoke f v)
          (let ((s (jrec-pr v))) (substring s 1 (string-length s)))))))

;; `type` lives in natives-meta.ss: it needs jolt-meta for the :type
;; override and a total value->taxonomy mapping, so it sits with meta — a record
;; yields (jolt-symbol #f (jrec-tag x)), the ns.Name class-name symbol.

(def-var! "clojure.core" "make-deftype-ctor" make-deftype-ctor)
(def-var! "clojure.core" "make-protocol" make-protocol)
(def-var! "clojure.core" "register-protocol-methods!" register-protocol-methods!)
(def-var! "clojure.core" "register-method" register-method)
(def-var! "clojure.core" "register-inline-method" register-inline-method)
(def-var! "clojure.core" "register-inline-protocol!" register-inline-protocol!)
(def-var! "jolt.host" "set-field!" jolt-set-field!)
(def-var! "clojure.core" "protocol-dispatch" (lambda (pn mn obj rest) (protocol-dispatch pn mn obj rest)))
(def-var! "clojure.core" "protocol-dispatch1" (lambda (pn mn obj) (protocol-dispatch1 pn mn obj)))
(def-var! "clojure.core" "protocol-dispatch2" (lambda (pn mn obj a) (protocol-dispatch2 pn mn obj a)))
(def-var! "clojure.core" "protocol-dispatch3" (lambda (pn mn obj a b) (protocol-dispatch3 pn mn obj a b)))
(def-var! "clojure.core" "satisfies?" jolt-satisfies?)
(def-var! "clojure.core" "extenders" extenders)
(def-var! "clojure.core" "make-reified" (lambda (mm . rest) (apply make-reified mm rest)))
(def-var! "clojure.core" "record-method-dispatch" (lambda (obj m rest) (record-method-dispatch obj m rest)))
