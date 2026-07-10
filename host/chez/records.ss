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
;; by every instance. Holds the tag, the field keywords in declared order, an
;; eq?-keyed keyword->index table (field keys are interned, so identity lookup),
;; and a per-type protocol-method cache: an eq?-hashtable keyed by an INTERNED
;; (proto . method) identity -> impl fn (populated lazily/mirrored from the
;; string-keyed type-registry at register time). protocol-resolve's record branch
;; reads it with one field read + one eq?-ref instead of recomputing the tag and
;; protocol-resolution reads it with one field read + one eq?-ref instead of
;; recomputing the tag and walking three nested string tables. ptable is #f
;; (uninitialized) until the first register-protocol-method for this desc; a
;; stale (pre-redef) descriptor has its ptable set to #f explicitly so lookups
;; fall back to the string registry. See register-protocol-method / protocol-resolve.
(define-record-type (jrdesc make-jrdesc-rec jrdesc?)
  (fields tag fkeys index (mutable ptable))
  (nongenerative chez-jrdesc-v3))
(define (make-jrdesc tag fkey-list)
  (let ((index (make-eq-hashtable)))
    (let loop ((ks fkey-list) (i 0))
      (unless (null? ks) (hashtable-set! index (car ks) i) (loop (cdr ks) (+ i 1))))
    (make-jrdesc-rec tag (list->vector fkey-list) index #f)))
;; An instance: the shared descriptor + an extension map (jolt-nil unless
;; non-field keys have been assoc'd on) + the field values INLINE in the record.
;; Fields are flattened into per-field-count native record types jrec1..jrec8,
;; each a child of jrec carrying desc+ext as inherited fields; a record with more
;; than 8 fields spills into jrec* (desc+ext+ a vals vector). One heap object per
;; record (two for spill), and a field read is a direct slot, not a vector
;; indirection. jrec-desc/jrec-ext are inherited accessors, so every child (and
;; spill) answers them — desc-keyed dispatch/PIC stays uniform.
(define-syntax define-jrec-family
  (lambda (x)
    (syntax-case x ()
      ((_ max-n)
       (let ((tid (car (syntax->list x))) (mn (syntax->datum #'max-n)))
         (define (range lo hi)
           (let loop ((i hi) (acc '())) (if (< i lo) acc (loop (- i 1) (cons i acc)))))
         (define (sym fmt . args) (string->symbol (apply format #f fmt args)))
         (define (fsym i) (sym "f~a" i))
         (define (pred k) (sym "jrec~a?" k))
         (define (rtname k) (sym "jrec~a" k))
         (define (mkname k) (sym "make-jrec~a" k))
         (define (ngname k) (sym "chez-jrec~av1" k))
         (define (acc k i) (sym "jrec~a-f~a" k i))
         (define (mut k i) (sym "jrec~a-f~a-set!" k i))
         (define (fsyms k) (map fsym (range 0 (- k 1))))
         (define base-def
           '(define-record-type (jrec make-jrec0 jrec?)
             (fields (immutable desc) (immutable ext)) (nongenerative chez-jrec-v4)))
         (define (child-def k)
           `(define-record-type (,(rtname k) ,(mkname k) ,(pred k))
              (parent jrec)
              (fields ,@(map (lambda (f) `(mutable ,f)) (fsyms k)))
              (nongenerative ,(ngname k))))
         (define spill-def
           '(define-record-type (jrec* make-jrec* jrec*?)
             (parent jrec) (fields (mutable vals)) (nongenerative chez-jrecsp-v1)))
         (define nfields-def
           `(define (jrec-nfields r)
              (cond ,@(map (lambda (k) `((,(pred k) r) ,k)) (range 1 mn))
                    ((jrec*? r) (vector-length (jrec*-vals r)))
                    (else 0))))
         (define (ref-branch k)
           `((,(pred k) r)
             (case i
               ,@(map (lambda (i) `((,i) (,(acc k i) r))) (range 0 (- k 1)))
               (else (error 'jrec-field-ref "index out of range" i)))))
         (define fieldref-def
           `(define (jrec-field-ref r i)
              (cond ,@(map ref-branch (range 1 mn))
                    ((jrec*? r) (vector-ref (jrec*-vals r) i))
                    (else (error 'jrec-field-ref "not a fielded record" r)))))
         (define (set-branch k)
           `((,(pred k) r)
             (case i
               ,@(map (lambda (i) `((,i) (,(mut k i) r v))) (range 0 (- k 1)))
               (else (error 'jrec-field-set! "index out of range" i)))))
         (define fieldset-def
           `(define (jrec-field-set! r i v)
              (cond ,@(map set-branch (range 1 mn))
                    ((jrec*? r) (vector-set! (jrec*-vals r) i v))
                    (else (error 'jrec-field-set! "not a fielded record" r)))))
         (define (fieldat-branch k)
           `((,(pred k) r)
             (case i
               ,@(map (lambda (i) `((,i) (,(acc k i) r))) (range 0 (- k 1)))
               (else (jolt-get r k)))))
         (define fieldat-def
           `(define (jrec-field-at r i k)
              (cond ,@(map fieldat-branch (range 1 mn))
                    ((jrec*? r) (if (fx< i (vector-length (jrec*-vals r)))
                                    (vector-ref (jrec*-vals r) i) (jolt-get r k)))
                    (else (jolt-get r k)))))
         (define ctor-vec-def
           `(define jrec-ctor-vec (vector ,@(map mkname (range 0 mn)))))
         (datum->syntax tid
           `(begin ,base-def
                   ,@(map child-def (range 1 mn))
                   ,spill-def
                   ,nfields-def ,fieldref-def ,fieldset-def ,fieldat-def ,ctor-vec-def)))))))
(define-jrec-family 8)
;; compatibility ctor (desc vals-vector ext): dispatches to the native per-arity
;; ctor. The hot ctor paths (make-deftype-ctor / backend inline emission) build
;; the native ctor directly; this remains for meta-copy and callers still holding
;; a field vector.
(define (make-jrec desc vals ext)
  (let ((n (vector-length vals)))
    (if (fx<= n 8)
        (apply (vector-ref jrec-ctor-vec n) desc ext (vector->list vals))
        (make-jrec* desc ext vals))))
;; compatibility accessor: rebuilds a fresh field vector from the inline slots.
;; Read-only — never set! through this, it returns a copy.
(define (jrec-vals r)
  (let* ((n (jrec-nfields r)) (v (make-vector n)))
    (do ((i 0 (fx+ i 1))) ((fx= i n) v)
      (vector-set! v i (jrec-field-ref r i)))))
(define (jrec-tag r) (jrdesc-tag (jrec-desc r)))
;; descriptor or #f — the dispatch key the inline cache eq?-scans. #f for a
;; non-record so a PIC site (and protocol-resolve's record branch) cheaply falls
;; through to the value-host-tags path without a separate type test.
(define (jrec-pic-desc x) (and (jrec? x) (jrec-desc x)))

;; defrecord vs deftype: a defrecord IS a map (map?/seq/keys/assoc over its
;; fields); a bare deftype is an opaque object with only its declared interfaces,
;; never a map (Clojure semantics). defrecord registers its type tag here; the
;; default jrec-as-map behaviour (map?/record?/field-seq) is gated on it, while
;; method dispatch (a deftype implementing ISeq/Counted/…) stays open to any jrec.
(define chez-record-type-tbl (make-hashtable string-hash string=?))
(define (jrec-record? x) (and (jrec? x) (hashtable-ref chez-record-type-tbl (jrec-tag x) #f) #t))
;; every deftype/defrecord tag, and a simple-name -> tag index. An extend-protocol
;; in a DIFFERENT ns names the type bare (it is :import-ed), so register-method
;; resolves "Raw" to its real tag "a.util.Raw" here instead of prepending the
;; calling ns. The local ns is preferred, so a same-named local type still wins.
(define chez-deftype-tag-set (make-hashtable string-hash string=?))
;; ctor procedure -> its class tag: the type NAME var holds the ctor (a jolt-ism;
;; the JVM resolves it to the class), so class-key maps the ctor back to the
;; class for (ancestors TypeName) / (isa? x TypeName) / derive on the type.
(define chez-deftype-ctor-tag (make-weak-eq-hashtable))
(define chez-simple-name-tag (make-hashtable string-hash string=?))
;; type-tag STRING -> the shared jrdesc for that type (set at deftype/defrecord
;; ctor construction). Strong: a desc lives as long as any instance does (every
;; instance references it), so this never outlives the type. string=? because the
;; tag strings registered by extend-type/inline-method are fresh allocations, not
;; the ctor's exact object — equal tags must match. register-protocol-method
;; mirrors an impl into desc's ptable through this index so protocol-resolve's
;; record branch can resolve by descriptor identity.
(define chez-tag-desc (make-hashtable string-hash string=?))
;; type-tag STRING -> fixed-arity ctor procedure: (lambda (f0 .. fn) (make-jrec ...))
;; with one arg per field, no rest-list walk. Used by the backend's emit-invoke
;; a jrec that is coll? — a record, or a deftype implementing a collection
;; interface (its seq/count/nth/valAt/cons method is registered). find-method-any-
;; protocol is defined later; resolved at call time. An opaque deftype is not coll?.
(define (jrec-collection? x)
  (and (jrec? x)
       (or (jrec-record? x)
           (let ((tag (jrec-tag x)))
             ;; coll? is instance? IPersistentCollection — its marker is `cons`
             ;; (and ISeq's `first`). ILookup(valAt) / Indexed(nth) / Counted(count)
             ;; / Seqable(seq) alone do NOT make a value coll?, matching the JVM
             ;; (e.g. core.logic's LVar implements only valAt and is not coll?).
             (or (find-method-any-protocol tag "cons")
                 (find-method-any-protocol tag "first"))))
       #t))
;; a jrec that is map? — a record, or a deftype implementing clojure.lang
;; .IPersistentMap (clojure.core.cache's caches do). `without` (dissoc) is the
;; map-distinctive method: vectors/sets implement Associative/ILookup but not it.
(define (jrec-maplike? x)
  (and (jrec? x)
       (or (jrec-record? x)
           (find-method-any-protocol (jrec-tag x) "without"))
       #t))
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
;; type-tag "ns.Name" -> #(bool ...) marking which fields are ^double, so the ctor
;; and set! coerce them to flonums (JVM primitive-field semantics, and what makes
;; reading the field back as :double sound for fl-ops).
(define chez-record-dbl-tbl (make-hashtable string-hash string=?))
(define (chez-double-tag? t) (and (string? t) (string=? t "double")))

(define (register-record-shape! ctor-key field-kws field-tags type-tag)
  (hashtable-set! chez-record-shapes-tbl ctor-key
                  (vector field-kws field-tags type-tag))
  (hashtable-set! chez-record-dbl-tbl type-tag
                  (list->vector (map chez-double-tag? field-tags))))

;; Coerce ^double fields to flonums in-place on a freshly-built field vector.
;; The dbl-flags vector (from chez-record-dbl-tbl or the ctor's closure) has a
;; true at each index that must be a flonum. No-op when the type has no ^double
;; fields (most records). Used by the fixed-arity ctors.
(define (coerce-double-fields! v dbl-flags)
  (let ((ndbl (vector-length dbl-flags)))
    (when (fx> ndbl 0)
      (let ((n (min ndbl (vector-length v))))
        (do ((i 0 (fx+ i 1))) ((fx= i n))
          (let ((a (vector-ref v i)))
            (when (and (vector-ref dbl-flags i)
                       (number? a) (not (flonum? a)))
              (vector-set! v i (exact->inexact a)))))))));; simple name of a dotted/slashed string: the segment after the last . or /.
(define (chez-shape-simple-name s)
  (let loop ((i (- (string-length s) 1)))
    (cond ((< i 0) s)
          ((or (char=? (string-ref s i) #\.) (char=? (string-ref s i) #\/))
           (substring s (+ i 1) (string-length s)))
          (else (loop (- i 1))))))

;; resolve a field's declared type tag to what jolt.passes.types wants: "num"
;; passes through; a record name (simple "Vec3" or qualified "ns.Vec3") resolves
;; to its ctor-key (so the field reads back as that record); anything else -> nil.
;; Resolution order: the tag as written (a qualified "ns.Vec3" hits its exact
;; entry), then the owner namespace's record of that simple name (two namespaces
;; can each define a Node — a simple ^Node means the local one), then any record
;; with that simple name (imported/cross-ns).
(define (chez-resolve-field-tag tag by-name owner-ns)
  (cond ((or (not tag) (jolt-nil-t? tag)) jolt-nil)
        ((string=? tag "num") "num")
        ((string=? tag "double") "double")   ; a ^double field reads back as a flonum
        (else (let* ((simple (chez-shape-simple-name tag))
                     (qualified? (not (string=? simple tag)))
                     (ck (or (and qualified? (hashtable-ref by-name tag #f))
                             (hashtable-ref by-name (string-append owner-ns "." simple) #f)
                             (hashtable-ref by-name simple #f))))
                (if ck ck jolt-nil)))))

;; namespace part of a ctor-key "ns/->Name" (up to the /).
(define (chez-ctor-key-ns k)
  (let loop ((i 0))
    (cond ((>= i (string-length k)) k)
          ((char=? (string-ref k i) #\/) (substring k 0 i))
          (else (loop (+ i 1))))))

;; materialize chez-record-shapes-tbl into "ns/->Name" -> {:fields :tags :type},
;; the shape record-type-from-entry consumes.
(define (chez-record-shapes-map)
  (let ((by-name (make-hashtable string-hash string=?))
        (kw-fields (keyword #f "fields")) (kw-tags (keyword #f "tags")) (kw-type (keyword #f "type"))
        (out (jolt-hash-map)))
    ;; index the full type tag "ns.Name" AND the simple record name -> ctor-key
    ;; for nested-field-tag resolution (qualified entries are unambiguous; the
    ;; simple entry is the cross-ns fallback and may be overwritten on collision).
    (let-values (((ks vs) (hashtable-entries chez-record-shapes-tbl)))
      (vector-for-each
        (lambda (k v)
          (let ((type-tag (vector-ref v 2)))
            (hashtable-set! by-name type-tag k)
            (hashtable-set! by-name (chez-shape-simple-name type-tag) k)))
        ks vs)
      (vector-for-each
        (lambda (k v)
          (let* ((fields (vector-ref v 0)) (tags (vector-ref v 1)) (type-tag (vector-ref v 2))
                 (owner-ns (chez-ctor-key-ns k))
                 (rtags (map (lambda (t) (chez-resolve-field-tag t by-name owner-ns)) tags)))
            (set! out (jolt-assoc out k
                                  (jolt-hash-map kw-fields (apply jolt-vector fields)
                                                 kw-tags   (apply jolt-vector rtags)
                                                 kw-type   type-tag)))))
        ks vs))
    out))

;; resolve a record TYPE name (a ^Type param hint's tag) to the ctor-key
;; "ns/->Name" the inference seeds with. Prefer the ctor in `ns` (the compile ns);
;; else any registered record with that simple name (cross-ns / imported). #f if
;; the name isn't a record type (so a ^double/^String hint resolves to nil).
(define (chez-find-ctor-key name ns)
  (let* ((simple (chez-shape-simple-name name))
         (target (string-append "->" simple))
         (preferred (string-append ns "/->" simple)))
    (if (hashtable-ref chez-record-shapes-tbl preferred #f)
        preferred
        (let loop ((ks (vector->list (hashtable-keys chez-record-shapes-tbl))))
          (cond ((null? ks) #f)
                ((string=? (chez-shape-simple-name (car ks)) target) (car ks))
                (else (loop (cdr ks))))))))

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
        (if i (jrec-field-ref r i)
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
        (if i (jrec-field-ref coll i)
            (let* ((ext (jrec-ext coll))
                   (v (if (jolt-nil? ext) jrec-absent (jolt-get ext k jrec-absent))))
              (if (eq? v jrec-absent)
                  (cond ((find-method-any-protocol (jrec-tag coll) "valAt")
                          => (lambda (m) (jolt-invoke m coll k d)))
                         (else d))
                   v))))))

;; mutate a deftype's mutable field in place: fields are mutable slots, so
;; jrec-field-set! updates the field. (set! field v) inside a method lowers to
;; this; returns v, as set! does.
(define (jolt-set-field! inst k v)
  (if (jrec? inst)
      (let ((i (jrec-field-index inst k)))
        (if i (let* ((flags (hashtable-ref chez-record-dbl-tbl (jrec-tag inst) #f))
                     ;; a ^double field stays a flonum across set!, like the ctor —
                     ;; keeps a later field read sound to unbox.
                     (v2 (if (and flags (fx< i (vector-length flags)) (vector-ref flags i)
                                  (number? v) (not (flonum? v)))
                             (exact->inexact v) v)))
                (jrec-field-set! inst i v2) v2)
            (error #f "set! of an unknown field" k)))
      (error #f "set! of a field on a non-record" inst)))
(define (jrec-ext=? ea eb)
  (cond ((and (jolt-nil? ea) (jolt-nil? eb)) #t)
        ((or (jolt-nil? ea) (jolt-nil? eb)) #f)
        (else (jolt=2 ea eb))))
(define (jrec=? a b)
  (and (string=? (jrec-tag a) (jrec-tag b))
       (let ((n (jrec-nfields a)))
         (and (= n (jrec-nfields b))
              (let loop ((i 0))
                (or (= i n)
                    (and (jolt=2 (jrec-field-ref a i) (jrec-field-ref b i)) (loop (+ i 1)))))))
       (jrec-ext=? (jrec-ext a) (jrec-ext b))))
(define (jrec-hash r)
  (let* ((fkeys (jrdesc-fkeys (jrec-desc r))) (n (jrec-nfields r))
         (base (let loop ((i 0) (acc (string-hash (jrec-tag r))))
                 (if (= i n) acc
                     (loop (+ i 1) (+ acc (jolt-hash (vector-ref fkeys i))
                                       (jolt-hash (jrec-field-ref r i))))))))
    (let ((ext (jrec-ext r)))
      (if (jolt-nil? ext) base (+ base (jolt-hash ext))))))
(define (jrec-pr r)                      ; #ns.Name{:k v, :k v}
  (let ((fkeys (jrdesc-fkeys (jrec-desc r))))
    (string-append "#" (jrec-tag r) "{"
      (let ((n (vector-length fkeys)))
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
                      (jolt-pr-readable (vector-ref fkeys i)) " " (jolt-pr-readable (jrec-field-ref r i)))))))
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
                          ;; a deftype with a custom Object.equals (but no equiv) governs
                          ;; its own value equality and map-key identity — core.logic's
                          ;; LVar/LCons key substitutions on id, ignoring metadata, so
                          ;; structural jrec=? (which sees the meta field) is wrong here.
                          ((and (jrec? a) (jrec-cl a "equals")) => (lambda (m) (if (jolt-truthy? (jolt-invoke m a b)) #t #f)))
                          ((and (jrec? b) (jrec-cl b "equals")) => (lambda (m) (if (jolt-truthy? (jolt-invoke m b a)) #t #f)))
                          ((and (jrec? a) (jrec? b)) (jrec=? a b))
                          (else #f))))
;; a deftype's declared hashCode governs its map/set hashing (paired with the
;; equals/equiv above so the hash/eq contract holds); a plain record hashes its
;; fields structurally via jrec-hash.
(register-hash-arm! jrec?
  (lambda (x) (let ((m (jrec-cl x "hashCode")))
                (if m (jolt-invoke m x) (jrec-hash x)))))
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
;; Same lookup as collections.ss rec-coll-method — one definition, aliased here.
(define jrec-cl rec-coll-method)

;; iface-method: the single deftype/reify interface-method lookup. Returns the
;; impl fn for METHOD declared by V (a deftype/record OR a reify), or #f. NARGS
;; (including `this`) selects the matching arity for a deftype; #f means any
;; arity. Core fns route interface dispatch through this instead of each
;; re-deriving jrec-vs-reify lookup and arity handling.
(define (iface-method v method nargs)
  (cond ((jrec? v)
         (if nargs (find-method-any-protocol-arity (jrec-tag v) method nargs)
             (find-method-any-protocol (jrec-tag v) method)))
        ((jreify? v) (let ((rm (reified-methods v))) (and rm (hashtable-ref rm method #f))))
        (else #f)))
(register-count-arm! (lambda (coll) (or (jrec? coll) (jolt-transient? coll)))
  (lambda (coll)
    (cond ((jrec-cl coll "count") => (lambda (m) (jolt-invoke m coll)))
          ((jrec? coll) (+ (jrec-nfields coll)
                           (let ((ext (jrec-ext coll))) (if (jolt-nil? ext) 0 (jolt-count ext)))))
          ((jolt-transient? coll) (t-count coll))
          (else (error 'count "uncountable record"))))
  ;; note: the else arm is unreachable since the predicate matches both
  ;; jrec? and jolt-transient? — kept as a safety net
)
;; contains?: a deftype implementing Associative/containsKey (e.g. core.cache's
;; caches) answers through that; a plain defrecord checks its fields.
(define %r-jolt-contains? jolt-contains?)
(set! jolt-contains? (lambda (coll k)
  (cond ((jrec-cl coll "containsKey") => (lambda (m) (if (jolt-truthy? (jolt-invoke m coll k)) #t #f)))
        ((jrec? coll) (jrec-has? coll k))
        ((jolt-transient? coll) (t-contains? coll k))
        (else (%r-jolt-contains? coll k)))))
;; nth: transient unwrapping (vec→direct buf access, other→fallback), then original
(define %r-jolt-nth jolt-nth)
(set! jolt-nth
  (case-lambda
    ((coll i)
     (if (jolt-transient? coll)
         (if (eq? (jolt-transient-kind coll) 'vec)
             (let ((idx (->idx i)))
               (if (tvec-in-bounds? coll idx) (vector-ref (jolt-transient-buf coll) idx) (error 'nth "index out of bounds")))
             (%r-jolt-nth (jolt-transient-buf coll) i))
         (%r-jolt-nth coll i)))
    ((coll i d)
     (if (jolt-transient? coll)
         (if (eq? (jolt-transient-kind coll) 'vec)
             (let ((idx (->idx i))) (if (tvec-in-bounds? coll idx) (vector-ref (jolt-transient-buf coll) idx) d))
             (%r-jolt-nth (jolt-transient-buf coll) i d))
         (%r-jolt-nth coll i d)))))
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
  (let* ((fkeys (jrdesc-fkeys (jrec-desc r))) (n (vector-length fkeys)))
    (let loop ((i 0) (m empty-pmap))
      (if (= i n)
          (let ((ext (jrec-ext r)))
            (if (jolt-nil? ext) m
                (fold-left (lambda (mm p) (%r-jolt-assoc1 mm (car p) (cdr p))) m (jrec-ext-pairs ext))))
          (let ((fk (vector-ref fkeys i)))
            (loop (+ i 1) (if (eq? fk drop-k) m (%r-jolt-assoc1 m fk (jrec-field-ref r i)))))))))
(define %r-jolt-dissoc jolt-dissoc)
(define %r-jolt-dissoc2 jolt-dissoc2)
(define (jrec-dissoc1 coll k)
  (if (not (jrec? coll))
      (%r-jolt-dissoc coll k)            ; an earlier declared-field dissoc downgraded it
      (let ((i (and (keyword? k) (jrec-field-index coll k))))
        (if i (jrec->map-without coll k)
            (let ((ext (jrec-ext coll)))
              (if (jolt-nil? ext) coll
                  (let ((ne (%r-jolt-dissoc ext k)))
                    (make-jrec (jrec-desc coll) (jrec-vals coll)
                                (if (= 0 (jolt-count ne)) jolt-nil ne)))))))))
(set! jolt-dissoc (lambda (coll . ks)
  (cond ((jrec-cl coll "without")
         => (lambda (m) (fold-left (lambda (c k) (jolt-invoke m c k)) coll ks)))
        ((jrec? coll) (fold-left jrec-dissoc1 coll ks))
        (else (apply %r-jolt-dissoc coll ks)))))
(set! jolt-dissoc2
  (lambda (coll k)
    (cond ((jrec-cl coll "without") => (lambda (m) (jolt-invoke m coll k)))
          ((jrec? coll) (jrec-dissoc1 coll k))
          (else (%r-jolt-dissoc2 coll k)))))
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
  (let* ((fkeys (jrdesc-fkeys (jrec-desc r))) (n (vector-length fkeys)))
    (let loop ((i 0) (acc '()))
      (if (= i n)
          (let ((ext (jrec-ext r)))
            (append (reverse acc)
                    (if (jolt-nil? ext) '()
                        (map (lambda (p) (make-map-entry (car p) (cdr p))) (jrec-ext-pairs ext)))))
          (loop (+ i 1) (cons (make-map-entry (vector-ref fkeys i) (jrec-field-ref r i)) acc))))))
(define %r-jolt-seq jolt-seq)
(set! jolt-seq (lambda (x)
  (cond ((jrec-cl x "seq") => (lambda (m) (jolt-seq (jolt-invoke m x))))
        ;; a record seqs its fields; a bare deftype is not seqable (falls through
        ;; to %r-jolt-seq, which errors like the JVM).
        ((jrec-record? x) (list->cseq (jrec-entry-list x)))
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
(set! jolt-empty? (lambda (coll)
  (if (jrec-collection? coll) (jolt-nil? (jolt-seq coll)) (%r-jolt-empty? coll))))
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
;; only a defrecord is a map (Clojure: a record IS an associative map); a bare
;; deftype is not. coll? additionally covers a deftype implementing a collection
;; interface. predicates.ss vars hold a snapshot, so re-def-var! after extending.
(define %r-jolt-map? jolt-map?)
(set! jolt-map? (lambda (x) (or (jrec-maplike? x) (%r-jolt-map? x))))
(def-var! "clojure.core" "map?" jolt-map?)
(def-var! "clojure.core" "coll?" (lambda (x) (or (jrec-collection? x) (jolt-coll-pred? x))))

;; ---- protocol registry ------------------------------------------------------
;; type-tag -> (proto-name -> (method-name -> fn)) — the source of truth for the
;; open world (extend-type/extend-protocol at any time, incl. mode A REPL).
;; register-protocol-method also mirrors the impl into the type's jrdesc ptable
;; (keyed by an interned proto-method identity) so protocol-resolve's record
;; branch resolves by descriptor identity instead of re-walking this string tree.
(define type-registry (make-hashtable string-hash string=?))
;; Global protocol epoch: bumped on EVERY register-protocol-method. A per-site
;; inline cache (the PIC the back end emits) tags itself with the epoch at
;; populate time; a later extension (a new bump) invalidates it so a cached
;; site re-resolves instead of serving a stale impl.
(define jolt-proto-epoch 0)
;; interned (proto . method) -> eq?-comparable key, keyed by "proto<nul>method"
;; so two pairs that print alike but split differently never collide. Minted once
;; per pair; reused by the descriptor ptable (eq?-ref).
(define proto-method-keys (make-hashtable string-hash string=?))
(define (intern-pm-key proto method)
  (let* ((s (string-append proto (string (integer->char 0)) method))
         (k (hashtable-ref proto-method-keys s #f)))
    (or k (let ((nk (gensym (string-append proto "." method))))
            (hashtable-set! proto-method-keys s nk) nk))))
;; descriptor ptable lookup (the record fast path): one eq?-ref keyed by the
;; interned identity, valid while this desc is current (not invalidated by a
;; type re-def). #f on any miss so protocol-resolve drops to the string registry.
(define (find-protocol-method-desc desc proto method)
  (let ((pt (jrdesc-ptable desc)))
    (and pt (hashtable-ref pt (intern-pm-key proto method) #f))))
(define (register-protocol-method type-tag proto method fn)
  (set! jolt-proto-epoch (fx+ jolt-proto-epoch 1))
  (let* ((ti (or (hashtable-ref type-registry type-tag #f)
                 (let ((h (make-hashtable string-hash string=?))) (hashtable-set! type-registry type-tag h) h)))
         (pi (or (hashtable-ref ti proto #f)
                 (let ((h (make-hashtable string-hash string=?))) (hashtable-set! ti proto h) h))))
    (hashtable-set! pi method fn))
  ;; mirror onto the type's descriptor ptable (record types only — a host tag
  ;; like "String"/"Object" has no desc). A re-def invalidated the old desc's
  ;; ptable (set it to #f), and this call populates the new desc's ptable.
  (let ((desc (hashtable-ref chez-tag-desc type-tag #f)))
    (when desc
      (let ((pt (or (jrdesc-ptable desc)
                    (let ((h (make-eq-hashtable))) (jrdesc-ptable-set! desc h) h))))
        (hashtable-set! pt (intern-pm-key proto method) fn))))
  (if #f #f))
(define (find-protocol-method type-tag proto method)
  (let ((ti (hashtable-ref type-registry type-tag #f)))
    (and ti (let ((pi (hashtable-ref ti proto #f))) (and pi (hashtable-ref pi method #f))))))
(define (find-method-any-protocol type-tag method)
  (let ((ti (hashtable-ref type-registry type-tag #f)))
    (and ti
         (let* ((ks (hashtable-keys ti)) (n (vector-length ks)))
           (let loop ((i 0))
             (and (fx< i n)
                  (let ((f (hashtable-ref (hashtable-ref ti (vector-ref ks i) #f) method #f)))
                    (or f (loop (fx+ i 1))))))))))
;; A deftype can implement a method NAME at two arities from two interfaces (e.g.
;; data.priority-map's seq: Seqable.seq[this] and Sorted.seq[this ascending]),
;; registered under different protocols. Pick the impl whose procedure accepts
;; the call's arg count (this + args); fall back to any same-named impl.
(define (proc-accepts? f n)
  (and (procedure? f) (bitwise-bit-set? (procedure-arity-mask f) n)))
(define (find-method-any-protocol-arity type-tag method nargs)
  (let ((ti (hashtable-ref type-registry type-tag #f)))
    (and ti
         (let* ((ks (hashtable-keys ti)) (n (vector-length ks)))
           (let loop ((i 0) (fallback #f))
             (if (fx>= i n)
                 fallback
                 (let ((f (hashtable-ref (hashtable-ref ti (vector-ref ks i) #f) method #f)))
                   (cond ((and f (proc-accepts? f nargs)) f)
                         (else (loop (fx+ i 1) (or fallback f)))))))))))
(define (type-satisfies? type-tag proto)
  (let ((ti (hashtable-ref type-registry type-tag #f)))
    (and ti (hashtable-ref ti proto #f) #t)))
;; True when a deftype/record instance DECLARES a method by this name (an inline
;; protocol impl), so clojure.core can prefer it over generic collection behavior
;; — e.g. (empty priority-map) must use the type's own empty, not return {}.
(def-var! "jolt.host" "jrec-method?"
  (lambda (v name) (if (and (jrec? v) (find-method-any-protocol (jrec-tag v) name)) #t #f)))

;; host type-tag candidates for a non-record value (extend-protocol on builtins).
(define (value-host-tags obj)
  ;; numbers dispatch by actual type (a Double is NOT a Long): flonum -> Double,
  ;; exact ratio -> Ratio, exact integer -> Long.
  (cond ((flonum? obj) '("Double" "Float" "Number" "Object"))
        ((and (number? obj) (exact? obj) (not (integer? obj))) '("Ratio" "Number" "Object"))
        ((number? obj) '("Long" "Integer" "BigInteger" "BigInt" "Number" "Object"))
        ((string? obj) '("String" "CharSequence" "Object"))
        ((boolean? obj) '("Boolean" "Object"))
        ((keyword? obj) (jch-tags "clojure.lang.Keyword"))
        ((jolt-symbol? obj) (jch-tags "clojure.lang.Symbol"))
        ((pvec? obj) (jch-tags "clojure.lang.PersistentVector"))
        ((pmap? obj) (jch-tags "clojure.lang.PersistentArrayMap"))
        ((pset? obj) (jch-tags "clojure.lang.PersistentHashSet"))
        ;; jolt models every seq as a list (no distinct LazySeq), so a seq also
        ;; reports PersistentList / IPersistentList / IPersistentStack — extend-protocol
        ;; clojure.lang.IPersistentList (algo.monads' writer monad) dispatches on one.
        ((or (cseq? obj) (empty-list-t? obj)) (jch-tags "clojure.lang.PersistentList"))
        ;; a lazy seq (map/filter/… result) is clojure.lang.LazySeq: a Sequential
        ;; ISeq, but not a PersistentList — matching the JVM so extend-protocol /
        ;; instance? on a deferred seq dispatch like an eager one where they should.
        ((jolt-lazyseq? obj) (jch-tags "clojure.lang.LazySeq"))
        ;; a var is clojure.lang.Var (also IDeref / IFn) — reitit's Expand protocol
        ;; extends to Var so a #'handler route dispatches.
        ((var-cell? obj) (jch-tags "clojure.lang.Var"))
        ;; java.net.URI jhost — extend-protocol java.net.URI (hiccup ToURI/ToStr).
        ((and (jhost? obj) (string=? (jhost-tag obj) "uri")) '("URI" "java.net.URI" "Object"))
        ;; a ByteBuffer — extend-protocol java.nio.ByteBuffer (aws-api util).
        ((and (jhost? obj) (string=? (jhost-tag obj) "byte-buffer")) '("ByteBuffer" "java.nio.ByteBuffer" "Object"))
        ;; java.io readers/writers — so (extend-protocol java.io.Reader …) (data.csv)
        ;; and the like dispatch on one. A PushbackReader is also a Reader.
        ((and (jhost? obj) (string=? (jhost-tag obj) "string-reader"))
         '("StringReader" "java.io.StringReader" "Reader" "java.io.Reader" "Object"))
        ((and (jhost? obj) (string=? (jhost-tag obj) "pushback-reader"))
         '("PushbackReader" "java.io.PushbackReader" "FilterReader" "java.io.FilterReader" "Reader" "java.io.Reader" "Object"))
        ((and (jhost? obj) (string=? (jhost-tag obj) "char-reader"))
         '("Reader" "java.io.Reader" "Object"))
        ((and (jhost? obj) (string=? (jhost-tag obj) "char-writer"))
         '("Writer" "java.io.Writer" "Object"))
        ((and (jhost? obj) (string=? (jhost-tag obj) "writer"))
         '("Writer" "java.io.Writer" "Object"))
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
        ((procedure? obj) (jch-tags "clojure.lang.AFunction"))
        ((jolt-nil? obj) '("nil"))
        ;; a defrecord IS the clojure.lang map/record interfaces, so a protocol
        ;; extended to IRecord / IPersistentMap / Associative / Seqable / … (and not
        ;; to the record's own type) dispatches to it — e.g. core.logic extends
        ;; IWalkTerm to clojure.lang.IRecord, and walking a record value must hit
        ;; that, not the Object default (which would recur forever). The record's
        ;; own type is tried first (dispatch checks jrec-tag before these tags).
        ((jrec-record? obj)
         (cons (jrec-tag obj)
               '("IRecord" "clojure.lang.IRecord" "IPersistentMap" "clojure.lang.IPersistentMap"
                 "APersistentMap" "Associative" "ILookup" "Seqable" "Counted"
                 "IPersistentCollection" "IObj" "IMeta" "Map" "java.util.Map"
                 "Iterable" "java.lang.Iterable" "Object")))
        ;; a bare deftype is opaque — its declared interfaces dispatch via the
        ;; inline methods registered under its own tag (tried before these tags).
        ((jrec? obj) (list (jrec-tag obj) "Object"))
        (else '("Object"))))


;; ---- the native that handles the analyzer/overlay call ----------------------
;; make-deftype-ctor: (name-sym field-kws field-tags field-muts) -> ctor closure.
;; The tag is baked at definition time in the type's ns (chez-current-ns).
(define (make-deftype-ctor name-sym field-kws . rest-args)
  (let* ((tag (string-append (chez-current-ns) "." (symbol-t-name name-sym)))
         (kws (seq->list field-kws))
         (field-tags (if (pair? rest-args) (seq->list (car rest-args)) '()))
         ;; which fields are ^double — coerced to a flonum on construction (JVM
         ;; primitive-field parity), so reading them back is a genuine flonum.
         (dbl-flags (list->vector (map chez-double-tag? field-tags)))
         (ndbl (vector-length dbl-flags))
          (desc (make-jrdesc tag kws))
          ;; index the descriptor so register-protocol-method can mirror impls
          ;; onto its ptable (protocol-resolve's record fast path). A re-def of
          ;; the same type tag installs a fresh desc here; first invalidate the
          ;; old desc's ptable (set to #f) so pre-redef instances fall back to
          ;; the string registry on the next protocol-resolve.
          (old-desc (hashtable-ref chez-tag-desc tag #f))
          (_ (when old-desc (jrdesc-ptable-set! old-desc #f)))
          (_ (hashtable-set! chez-tag-desc tag desc))
         (nf (length kws))
          (ctor (lambda args
                  ;; fill the value vector from the positional args, padding missing
                  ;; trailing fields with nil and ignoring any extras.
                  (let ((v (make-vector nf jolt-nil)))
                    (let loop ((as args) (i 0))
                      (if (or (null? as) (= i nf)) (make-jrec desc v jolt-nil)
                          (let ((a (car as)))
                            (vector-set! v i
                                         (if (and (fx< i ndbl) (vector-ref dbl-flags i)
                                                  (number? a) (not (flonum? a)))
                                             (exact->inexact a) a))
                            (loop (cdr as) (+ i 1)))))))))
    ;; Register the ctor under its fully-qualified tag ("ns.Name") — a bare
    ;; (Name. …) in the DEFINING ns is qualified to this by the analyzer, so a
    ;; deftype whose simple name collides with a built-in host class (tools.reader's
    ;; PushbackReader vs java.io.PushbackReader) still resolves correctly there.
    (register-class-ctor! tag ctor)
    ;; Also register the simple name so (Name. …) resolves ns-agnostically across
    ;; files — BUT never clobber a built-in host class of the same simple name (an
    ;; unrelated ns's bare (Name. …) must still reach the built-in). A prior deftype
    ;; (tracked in chez-simple-name-tag) is fine to overwrite (last def wins / redef).
    (when (or (not (hashtable-ref class-ctors-tbl (symbol-t-name name-sym) #f))
              (hashtable-ref chez-simple-name-tag (symbol-t-name name-sym) #f))
      (register-class-ctor! (symbol-t-name name-sym) ctor))
    ;; index the tag so a cross-ns extend-protocol resolves the bare type name.
    (hashtable-set! chez-deftype-tag-set tag #t)
    (hashtable-set! chez-simple-name-tag (symbol-t-name name-sym) tag)
    ;; graft the type onto the class graph so isa?/supers/ancestors see it. A
    ;; bare deftype is an IType; defrecord (which runs register-record-type!
    ;; right after) replaces the row with the record interface set.
    (jch-set-supers! tag '("clojure.lang.IType"))
    (hashtable-set! chez-deftype-ctor-tag ctor tag)
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
                "Fn" "IFn" "AFn" "URI" "Var" "IDeref"
                "PersistentVector" "APersistentVector" "IPersistentVector"
                "PersistentArrayMap" "APersistentMap" "IPersistentMap"
                "PersistentHashSet" "APersistentSet" "IPersistentSet"
                "ASeq" "ISeq" "IPersistentCollection" "Associative" "Sequential"
                "PersistentList" "IPersistentList" "IPersistentStack"
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
                "[B" "[C" "[I" "[J" "[D" "[Ljava.lang.Object;"
                ;; java.io readers/writers — extend-protocol java.io.Reader (data.csv)
                "Reader" "java.io.Reader" "Writer" "java.io.Writer"
                "StringReader" "java.io.StringReader" "PushbackReader" "java.io.PushbackReader"
                "BufferedReader" "java.io.BufferedReader" "FilterReader" "java.io.FilterReader"
                "InputStream" "java.io.InputStream" "OutputStream" "java.io.OutputStream"))
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
    ;; a host class if the literal set lists it OR the class graph models it — both
    ;; feed value-host-tags (which emits the same bare segment), so a protocol
    ;; extended to any modeled class keys under a tag the value reports. A
    ;; deftype/defrecord is in the graph too (its ancestry), but its VALUES report
    ;; the ns-qualified tag, not the bare segment — so a name that resolves to a
    ;; deftype never canonicalizes through the graph arm.
    (and (or (hashtable-ref host-type-set base #f)
             (and (not (hashtable-ref chez-simple-name-tag type-name #f))
                  (not (hashtable-ref chez-deftype-tag-set type-name #f))
                  (or (jch-known? base) (jch-known? type-name))))
         base)))
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
         (local (string-append (chez-current-ns) "." type-name))
         ;; a host class -> its canonical tag; a deftype defined in THIS ns -> the
         ;; local tag; an :import-ed deftype from another ns -> its real tag via the
         ;; simple-name index; otherwise the local tag (a forward extend).
         (tag (cond (host host)
                    ((hashtable-ref chez-deftype-tag-set local #f) local)
                    ((hashtable-ref chez-simple-name-tag type-name #f))
                    (else local))))
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
  ;; the protocol's interface joins the type's class ancestry, spelled like the
  ;; JVM interface (munged ns; the defining ns is assumed to be the current one —
  ;; the macro passes only the simple protocol name).
  (let ((iface (string-append (jch-munge-segments (chez-current-ns)) "." proto-name)))
    (jch-mark-interface! iface)
    (jch-register-supers! (string-append (chez-current-ns) "." type-name) (list iface)))
  jolt-nil)

;; protocol-resolve: the impl procedure for obj — by record type tag, a reify's
;; instance-local method, or the protocol's extended impls over obj's host tags.
;; Raises if none implements the method. The dispatchN entry points apply it
;; directly so a protocol call doesn't cons a rest-list (the impl fn is always a
;; procedure, registered by register-(inline-)method/extend). The record branch
;; reads the per-type descriptor once and tries its eq?-keyed ptable (the interned
;; proto-method identity, current-epoch) before walking the nested string tables —
;; one field read + one eq?-ref instead of two field reads + three string hashes.
(define (protocol-resolve proto-name method-name obj)
  (cond
    ((and (jrec? obj)
          (let* ((desc (jrec-desc obj))
                 (f (find-protocol-method-desc desc proto-name method-name)))
            (or f (find-protocol-method (jrdesc-tag desc) proto-name method-name)))))
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

;; ---- per-site polymorphic inline cache (PIC) --------------------------------
;; The back end emits, at each protocol call site it recognizes under --opt, a
;; cache keyed on the receiver's descriptor identity: a small mutable vector cell
;; holding N (desc . impl) pairs + a round-robin write cursor + the epoch at which
;; the cache was populated. The emitted call inlines the eq? scan over the cached
;; descs (no string hashing, no table walk, no helper call after warmup); a miss
;; (uncached desc) or a stale epoch resolves via these helpers and (re)fills the
;; cache. The epoch guard invalidates the whole cache when ANY register-protocol-
;; method runs after it was populated, so an extend-type at runtime can't leave a
;; cached site serving a pre-extension impl. jolt-pic-n is the cache width — 4
;; covers the megamorphic bench; a monomorphic site stays on the devirt path above.
(define jolt-pic-n 4)
(define (jolt-pic-make)
  ;; #(d0 i0 d1 i1 d2 i2 d3 i3 cursor epoch): 2N entries + cursor + epoch.
  ;; epoch starts at -1 (jolt-proto-epoch is always >= 0) so the very first call
  ;; misses the epoch guard and rebuilds, seeding slot 0 + stamping the real epoch.
  (let ((v (make-vector (+ (* jolt-pic-n 2) 2) #f)))
    (vector-set! v (* jolt-pic-n 2) 0)
    (vector-set! v (+ (* jolt-pic-n 2) 1) -1)
    v))
;; cache current, desc not found: resolve + round-robin install into the cursor slot.
(define (jolt-pic-install v d proto method obj)
  (let ((f (protocol-resolve proto method obj)))
    (when d
      (let ((slot (* (vector-ref v (* jolt-pic-n 2)) 2)))
        (vector-set! v slot d)
        (vector-set! v (fx+ slot 1) f)
        (vector-set! v (* jolt-pic-n 2)
                     (if (fx= (vector-ref v (* jolt-pic-n 2)) (fx- jolt-pic-n 1))
                         0 (fx+ (vector-ref v (* jolt-pic-n 2)) 1)))))
    f))
;; epoch stale (an extension ran) or first population: clear, resolve, seed slot 0.
(define (jolt-pic-rebuild v d proto method obj)
  (let ((f (protocol-resolve proto method obj)))
    (when d
      (let loop ((i 0))
        (when (fx< i (* jolt-pic-n 2)) (vector-set! v i #f) (loop (fx+ i 1))))
      (vector-set! v 0 d)
      (vector-set! v 1 f)
      (vector-set! v (* jolt-pic-n 2) 1)
      (vector-set! v (+ (* jolt-pic-n 2) 1) jolt-proto-epoch))
    f))

;; devirt-resolve: the impl for a call the inference proved monomorphic. Try the
;; static type tag directly (the fast path that skips receiver-type computation),
;; and fall back to ordinary dispatch when it misses — a record can satisfy a
;; protocol via an Object/host-tag default rather than a direct impl, which
;; find-protocol-method on its own tag wouldn't see. Mirrors jrec-field-at falling
;; back to jolt-get: correct regardless of how precise the inference was.
(define (devirt-resolve type-tag proto-name method-name obj)
  (or (find-protocol-method type-tag proto-name method-name)
      (protocol-resolve proto-name method-name obj)))

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
(define (record-method-dispatch-base obj method-name rest-args)
  (let ((rest (if (jolt-nil? rest-args) '() (seq->list rest-args))))
    (cond
      ((and (jrec? obj) (find-method-any-protocol-arity (jrec-tag obj) method-name (+ 1 (length rest))))
       => (lambda (f) (apply jolt-invoke f obj rest)))
      ;; (.field inst): a deftype/record field read with no matching method.
      ;; Clojure reads the field for (.q x) just like (.-q x); a declared method
      ;; (above) wins, this is the field-accessor fallback.
      ((and (jrec? obj) (null? rest) (jrec-has? obj (keyword #f method-name)))
       (jrec-lookup obj (keyword #f method-name) jolt-nil))
      ;; a defrecord is Associative / ILookup / IPersistentMap / Seqable / Counted,
      ;; so its clojure.lang interface methods delegate to the map fns when not
      ;; overridden by a declared method — reitit's impl calls (.assoc match k v),
      ;; (.valAt …), (.without …) directly. A bare deftype implements these via its
      ;; own declared methods (handled above), so this is record-only.
      ((and (jrec-record? obj)
            (member method-name '("valAt" "assoc" "without" "containsKey" "cons"
                                  "count" "seq" "equiv" "entryAt" "empty")))
       (cond
         ((string=? method-name "valAt")
          (if (null? (cdr rest)) (jolt-get obj (car rest) jolt-nil) (jolt-get obj (car rest) (cadr rest))))
         ((string=? method-name "assoc") (jolt-assoc1 obj (car rest) (cadr rest)))
         ((string=? method-name "without") (jolt-dissoc obj (car rest)))
         ((string=? method-name "containsKey") (if (jolt-truthy? (jolt-contains? obj (car rest))) #t #f))
         ((string=? method-name "cons") (jolt-conj1 obj (car rest)))
         ((string=? method-name "count") (jolt-count obj))
         ((string=? method-name "seq") (jolt-seq obj))
         ((string=? method-name "equiv") (if (jolt= obj (car rest)) #t #f))
         ((string=? method-name "entryAt")
          (if (jolt-truthy? (jolt-contains? obj (car rest)))
              (make-map-entry (car rest) (jolt-get obj (car rest) jolt-nil)) jolt-nil))
         (else jolt-nil)))   ; .empty of a record is nil on the JVM
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
             ((string=? method-name "hashCode")
              (java-symbol-hash (symbol-t-name obj) (symbol-t-ns obj)))
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
      ;; java.util.Collection.contains over a list/seq (vectors/sets handle it in
      ;; dot-coll-method): value membership, like the JVM.
      ((string=? method-name "contains")
       (let ((target (car rest)))
         (let loop ((s (jolt-seq obj)))
           (cond ((jolt-nil? s) #f)
                 ((jolt=2 (seq-first s) target) #t)
                 (else (loop (jolt-seq (seq-more s))))))))
      ;; universal Object methods on any remaining value (boolean, etc.).
      ((string=? method-name "toString") (jolt-str-render-one obj))
      ((string=? method-name "hashCode") (jolt-hash obj))
      ((string=? method-name "equals") (and (pair? rest) (if (jolt= obj (car rest)) #t #f)))
      (else (error #f (string-append "No method " method-name " for value: "
                                     (jolt-pr-str obj)))))))

;; ---- method-dispatch arm registry ------------------------------------------
;; A .method call (record-method-dispatch) is resolved by an ordered list of arms
;; (ascending priority), each (obj method-name rest-args) -> result | 'pass.
;; This replaces a stack of (set! record-method-dispatch ...) rebindings across
;; six files whose precedence was implicit in load order — priority is now
;; explicit data. record-method-dispatch-base is the final fallback (the
;; string/keyword/symbol/Object-method surface). A host shim / library registers
;; an arm with register-method-arm! instead of set!-wrapping the dispatcher.
(define method-dispatch-arms '())   ; list of (priority . arm), ascending priority
(define (register-method-arm! priority arm)
  (set! method-dispatch-arms
    (let ins ((as method-dispatch-arms))
      (cond ((null? as) (list (cons priority arm)))
            ((< priority (caar as)) (cons (cons priority arm) as))
            (else (cons (car as) (ins (cdr as))))))))
(define (record-method-dispatch obj method-name rest-args)
  (let loop ((as method-dispatch-arms))
    (if (null? as)
        (record-method-dispatch-base obj method-name rest-args)
        (let ((r ((cdar as) obj method-name rest-args)))
          (if (eq? r 'pass) (loop (cdr as)) r)))))

;; (.getClass x): a universal Object method reached by EVERY value before any
;; per-type arm — the class token for the value (jolt has no Class objects; the
;; token is the canonical name string, on which .getName/.getSimpleName work).
;; One arm, so a type arm that only whitelists its own methods can't steal it.
(register-method-arm! 5
  (lambda (obj method-name rest-args)
    (if (string=? method-name "getClass") (jolt-class obj) 'pass)))

;; reify: instance-local method table. obj is a jreify carrying a method ht +
;; the protocol short-names it implements (for satisfies?/instance?).
(define-record-type jreify (fields methods protos) (nongenerative chez-jreify-v1))
(define (reified-methods obj) (and (jreify? obj) (jreify-methods obj)))
;; (get reify k) / (:k reify) routes to a reify's ILookup valAt — clojure.spec.alpha
;; reifies fspec/regex specs as clojure.lang.ILookup and reads (:args spec) off them.
(register-get-arm! jreify?
  (lambda (coll k d)
    (let ((m (and (reified-methods coll) (hashtable-ref (reified-methods coll) "valAt" #f))))
      (if m (jolt-invoke m coll k d) d))))
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

;; a reify with a toString method renders through it, like the JVM.
(register-str-render! (lambda (v) (and (jreify? v) (reified-methods v)
                                       (hashtable-ref (reified-methods v) "toString" #f) #t))
  (lambda (v) (jolt-invoke (hashtable-ref (reified-methods v) "toString" #f) v)))

;; `type` lives in natives-meta.ss: it needs jolt-meta for the :type
;; override and a total value->taxonomy mapping, so it sits with meta — a record
;; yields (jolt-symbol #f (jrec-tag x)), the ns.Name class-name symbol.

(def-var! "clojure.core" "make-deftype-ctor" make-deftype-ctor)

;; defrecord marks its type a record (deftype does not), keyed by the same
;; "ns.Name" tag make-deftype-ctor bakes — so jrec-record? distinguishes the two.
(define (register-record-type! name-sym)
  (let ((tag (string-append (chez-current-ns) "." (symbol-t-name name-sym))))
    (hashtable-set! chez-record-type-tbl tag #t)
    ;; a defrecord's class ancestry: replace the deftype IType row with the
    ;; record interfaces (their closure supplies Associative/Seqable/ILookup/…),
    ;; keeping any protocol interfaces already grafted by the inline
    ;; registrations that ran between the deftype ctor and this call.
    (let ((protos (filter (lambda (s) (not (string=? s "clojure.lang.IType")))
                          (jch-direct-supers tag))))
      (jch-set-supers! tag (append protos
                                   '("clojure.lang.IRecord" "clojure.lang.IObj"
                                     "clojure.lang.IPersistentMap" "java.util.Map"
                                     "clojure.lang.IHashEq" "java.io.Serializable")))))
  jolt-nil)
(def-var! "clojure.core" "register-record-type!" register-record-type!)
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
