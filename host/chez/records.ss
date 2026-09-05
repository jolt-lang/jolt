;; records.ss — the jrec layout: the value representation shared by
;; deftype/defrecord, its type/shape registries, equality/hash, and printing.
;;
;; A record is a `jrec`: a shared per-type descriptor + field values inline in
;; declared order, plus an extension map for any non-field keys assoc'd on
;; (jolt-nil when there are none — the common case). This lays fields out like a
;; native struct: construction allocates one heap object, and a field read is a
;; direct slot, not a list scan. It is map?/coll?, equal to another jrec of the
;; same type with equal fields (never equal to a plain map), and prints as
;; #ns.Name{...}. (get r :jolt/deftype) returns the tag, so the overlay record?
;; predicate works unchanged.
;;
;; The deftype/defrecord + defprotocol/extend-type subsystem spans four files,
;; loaded in this order (rt.ss) — form order across them is load-bearing:
;;   records.ss          — this file: layout, registries, equality/hash, print
;;   records-coll.ss     — jrec arms on the collection dispatchers
;;   protocols.ss        — protocol registry, resolution, dispatch caches
;;   records-dispatch.ss — .method interop dispatch, reify, def-var! surface
;;
;; Loaded after collections/seq/values/converters/printing/transients/multimethods
;; (the dispatchers the arms wrap + chez-current-ns).

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
;; The FIELD LIST IS FROZEN: a jrec rides raw in a jolt.image dump and carries its
;; descriptor with it, so a released image holding any record stops restoring the
;; moment this layout changes (the same trap jolt-ref-v1 sprang in v0.6.5).
;; test/chez/fixtures/image-v0.6.8-record.image is the tripwire. Per-type derived
;; state goes in jrdesc-ifc-tbl instead — see jrdesc-ifc-of.
(define-record-type (jrdesc make-jrdesc-rec jrdesc?)
  (fields tag fkeys index (mutable ptable))
  (nongenerative chez-jrdesc-v3))
(define (make-jrdesc tag fkey-list)
  (let ((index (make-eq-hashtable)))
    (let loop ((ks fkey-list) (i 0))
      (unless (null? ks) (hashtable-set! index (car ks) i) (loop (cdr ks) (+ i 1))))
    (make-jrdesc-rec tag (list->vector fkey-list) index #f)))
;; declared field count of a descriptor (== the record's arity: a desc is built from
;; the same field list as its ctor). jrec-field-ref dispatches on this fixnum so Chez
;; compiles the case to a jump/binary search instead of an arity-predicate chain.
(define (jrdesc-nfields d) (vector-length (jrdesc-fkeys d)))
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
         (define (ngname k) (sym "chez-jrec~av2" k))
         (define (acc k i) (sym "jrec~a-f~a" k i))
         (define (mut k i) (sym "jrec~a-f~a-set!" k i))
         (define (fsyms k) (map fsym (range 0 (- k 1))))
         ;; hasheq is the defrecord __hasheq slot generalized to the family:
         ;; 0 = unset; a defrecord caches its structural hash here, a plain
         ;; deftype its identity hash, and a type with a declared
         ;; hasheq/hashCode never fills it (records-coll.ss jrec-hasheq-slow).
         ;; make-jrecN is the raw ctor, so every call passes the slot's 0
         ;; explicitly — a ctor protocol would keep the old arity but costs two
         ;; closure hops per construction (measured 40 -> 64 ns). The slot makes
         ;; the family's layout new image surface: every nongenerative tag
         ;; bumps, and state-image.ss reads older images through a legacy arm.
         (define base-def
           '(define-record-type (jrec make-jrec0 jrec?)
             (fields (immutable desc) (immutable ext) (mutable hasheq))
             (nongenerative chez-jrec-v5)))
         (define (child-def k)
           `(define-record-type (,(rtname k) ,(mkname k) ,(pred k))
              (parent jrec)
              (fields ,@(map (lambda (f) `(mutable ,f)) (fsyms k)))
              (nongenerative ,(ngname k))))
         (define spill-def
           '(define-record-type (jrec* make-jrec* jrec*?)
             (parent jrec) (fields (mutable vals)) (nongenerative chez-jrecsp-v2)))
         (define nfields-def
           `(define (jrec-nfields r)
              (cond ,@(map (lambda (k) `((,(pred k) r) ,k)) (range 1 mn))
                    ((jrec*? r) (vector-length (jrec*-vals r)))
                    (else 0))))
          (define (ref-case k)
            `((,k)
              (case i
                ,@(map (lambda (i) `((,i) (,(acc k i) r))) (range 0 (- k 1)))
                (else (error 'jrec-field-ref "index out of range" i)))))
          (define fieldref-def
            `(define (jrec-field-ref r i)
               (let ((n (jrdesc-nfields (jrec-desc r))))
                 (case n
                   ,@(map ref-case (range 1 mn))
                   (else (if (jrec*? r) (vector-ref (jrec*-vals r) i)
                             (error 'jrec-field-ref "not a fielded record" r)))))))
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
          ;; make-jrec-from-existing: build a fresh jrec of SRC's type sharing its
          ;; descriptor, with EXT and field values copied straight from SRC's inline
          ;; slots (direct reads — no intermediate field vector, no vector->list, no
          ;; rest-list allocation). When OV is a fixnum field index that one slot takes
          ;; OV-VAL instead (the assoc path's changed field); OV #f copies every field
          ;; verbatim (dissoc / meta-copy). Spill (>8 fields) copies the backing vals
          ;; vector. Replaces the old jrec-vals + jrec-vec-copy + make-jrec triple-copy.
          (define (fromexisting-clause k)
            `((,k) (,(mkname k) desc ext 0
                    ,@(map (lambda (j) `(if (eq? ov ,j) ov-val (,(acc k j) src)))
                           (range 0 (- k 1))))))
          (define fromexisting-def
            `(define (make-jrec-from-existing src ov ov-val ext)
               (let ((desc (jrec-desc src)))
                 (case (jrec-nfields src)
                   ((0) (make-jrec0 desc ext 0))
                   ,@(map fromexisting-clause (range 1 mn))
                   (else (let ((nv (jrec-vec-copy (jrec*-vals src))))
                           (when ov (vector-set! nv ov ov-val))
                           (make-jrec* desc ext 0 nv)))))))
          (datum->syntax tid
            `(begin ,base-def
                    ,@(map child-def (range 1 mn))
                    ,spill-def
                    ,nfields-def ,fieldref-def ,fieldset-def ,fieldat-def
                    ,ctor-vec-def ,fromexisting-def)))))))
(define-jrec-family 8)
;; compatibility ctor (desc vals-vector ext): dispatches to the native per-arity
;; ctor. The hot ctor paths (make-deftype-ctor / backend inline emission) build
;; the native ctor directly; this remains for meta-copy and callers still holding
;; a field vector.
(define (make-jrec desc vals ext)
  (let ((n (vector-length vals)))
    (if (fx<= n 8)
        (apply (vector-ref jrec-ctor-vec n) desc ext 0 (vector->list vals))
        (make-jrec* desc ext 0 vals))))
;; One throwaway record, built here at load and reused, for the arm registries to
;; probe a candidate predicate against (reject-fast-type-claim!, values.ss).
;; Built ONCE rather than per registration: registrations also happen at runtime,
;; as user code defines records, and calling make-jrec from that context resolves
;; differently. collections.ss reaches it through probe-if-available, since it
;; loads before this file and the earliest arms register before it too.
(define jrec-fast-type-probe (make-jrec 'fast-type-probe (vector) jolt-nil))

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
;; rec-tbl-mu serializes every MUTATION of the type/protocol/shape/clone tables
;; below, every WHOLE-TABLE scan of them, and the jolt-proto-epoch bump. Single-key
;; reads stay unlocked, on the split rt.ss spells out for var-table: these are
;; strong general hashtables, so an unlocked reader walks consistent structure and
;; the worst it sees is a stale miss. That is the point — find-protocol-method is
;; three probes on the protocol-DISPATCH path, and a mutex there would sit on
;; every protocol call in the program.
;;
;; The writers are deftype / defrecord / defprotocol / extend-type, which look
;; like load-time work but are not: extend-type inside a fn, an nREPL session, and
;; SCI all run them at any time from any thread. Three things went wrong without
;; this. The nested registries are built check-then-create, so two threads
;; extending different protocols on one type each made their own inner table and
;; one impl vanished with it. jolt-proto-epoch is a read-modify-write, so a lost
;; bump leaves a per-site inline cache tagged current while serving a superseded
;; impl. And find-method-any-protocol walks (hashtable-keys ti), which under a
;; concurrent registration returns trailing FILL slots — a hashtable-ref on 0
;; against a #f inner table, i.e. a crash on the dispatch path.
(define rec-tbl-mu (make-mutex))

(define chez-record-type-tbl (make-hashtable string-hash string=?))
;; The string-keyed lookup. jrec-record? answers from the per-type cache instead
;; (jrdesc-ifc-of), which is what derives its value; this is the one that reads the
;; table, so nothing recurses.
(define (jrec-record?-uncached x)
  (and (jrec? x) (hashtable-ref chez-record-type-tbl (jrec-tag x) #f) #t))
(define (jrec-record? x) (and (jrec? x) (vector-ref (jrdesc-ifc-of x) 3)))
;; every deftype/defrecord tag, and a simple-name -> tag index. An extend-protocol
;; in a DIFFERENT ns names the type bare (it is :import-ed), so register-method
;; resolves "Raw" to its real tag "a.util.Raw" here instead of prepending the
;; calling ns. The local ns is preferred, so a same-named local type still wins.
(define chez-deftype-tag-set (make-hashtable string-hash string=?))
;; ctor procedure -> its class tag: the type NAME var holds the ctor (a jolt-ism;
;; the JVM resolves it to the class), so class-key maps the ctor back to the
;; class for (ancestors TypeName) / (isa? x TypeName) / derive on the type.
;; COPY-ON-WRITE, because this one is read-mostly and write-almost-never: it is
;; written once per deftype/defrecord DEFINITION and read on multimethod dispatch
;; (mm-dispatch-val-canon), the class-tag chain and the interop path. A weak-eq
;; table cannot be read unlocked while another thread writes it — Chez's eq
;; adjust! relinks live cells in place with $set-tlc-next! (s/library.ss) and the
;; reader is unsafe primitive code, so a resize concurrent with a lookup hangs or
;; faults. But a mutex on THIS read path would sit on dispatch.
;;
;; So the writer copies under the lock and swaps the box; a reader unboxes once
;; and walks a table nobody will ever mutate again. The copy is O(number of
;; deftypes) and happens only at definition time. The GC's own rehash of a weak
;; table is not a factor: it stops the world.
(define chez-deftype-ctor-tag-box (box (make-weak-eq-hashtable)))
(define chez-deftype-ctor-tag-mu (make-mutex))
(define (deftype-ctor-tag p) (hashtable-ref (unbox chez-deftype-ctor-tag-box) p #f))
(define (deftype-ctor-tag-set! ctor tag)
  ;; The token is = to its Class (host-static-classes.ss), so pin its identity
  ;; hash to the class NAME's hash before anything can ask — =/hash have to agree
  ;; or a map keyed by one spelling answers nil for the other.
  (jolt-identity-hasheq-seed! ctor (jolt-hash tag))
  (jolt-with-mutex chez-deftype-ctor-tag-mu
    (let ((t (hashtable-copy (unbox chez-deftype-ctor-tag-box) #t)))  ; copy is weak too
      (hashtable-set! t ctor tag)
      (set-box! chez-deftype-ctor-tag-box t))))
;; A deftype/defrecord name used as a multimethod DISPATCH VALUE evaluates to its
;; ctor token (a procedure). Normalize it to the "ns.Name" class-name STRING that
;; __type-tag / class / type yield for an INSTANCE (jolt models a class as its name
;; string), so a JVM-style (defmethod print-method SomeType …) matches a (class x)
;; / __type-tag dispatch. Any non-deftype dispatch value passes through unchanged.
;; Called from multimethods.ss (forward-referenced; records.ss loads after it but
;; before any user defmethod runs).
(define (mm-dispatch-val-canon dval)
  (or (and (procedure? dval) (deftype-ctor-tag dval))
      dval))
(define chez-simple-name-tag (make-hashtable string-hash string=?))
;; simple deftype/defrecord name -> its "ns.Name" tag, or #f. Used by the analyzer
;; (host-contract.ss) to resolve a bare class name to its class-name string.
(define (chez-deftype-simple->tag nm) (hashtable-ref chez-simple-name-tag nm #f))
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
;; The ^double field coercion, as one procedure. A NUMBER is widened to a flonum
;; (JVM primitive-field parity, and what makes reading the field back as :double
;; sound for the fl-ops); anything else passes through unchanged, which is the
;; hint-as-contract behaviour the field read already has.
;;
;; It is a named procedure because there are two callers: make-deftype-ctor's
;; `build` (protocols.ss) on the dispatched path, and the back end's inlined
;; make-jrecN emission, which splices it by name. Two transcriptions of the rule
;; would let the fast ctor and the slow one disagree about a field's value.
;; The "jolt-" prefix keeps munge-name from letting a user local shadow it.
(define (jolt-rec-dbl a)
  (if (and (number? a) (not (flonum? a))) (exact->inexact a) a))

;; type-tag "ns.Name" -> the declared field keywords, in order. The shapes table
;; above is keyed by ctor-key, which the reflection surface does not have: it is
;; handed a class and has to answer what that class declares.
(define chez-record-fields-tbl (make-hashtable string-hash string=?))
(define (chez-record-field-kws type-tag)
  (or (hashtable-ref chez-record-fields-tbl type-tag #f) '()))

(define (register-record-shape! ctor-key field-kws field-tags type-tag)
  (jolt-with-mutex rec-tbl-mu
    (hashtable-set! chez-record-shapes-tbl ctor-key
                    (vector field-kws field-tags type-tag))
    (hashtable-set! chez-record-fields-tbl type-tag field-kws)
    (hashtable-set! chez-record-dbl-tbl type-tag
                    (list->vector (map chez-double-tag? field-tags)))))

;; Coerce ^double fields to flonums in-place on a freshly-built field vector.
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
;; Resolution order: the tag as written (a qualified "ns.Vec3" hits its exact
;; entry), then the owner namespace's record of that simple name (two namespaces
;; can each define a Node — a simple ^Node means the local one), then any record
;; with that simple name (imported/cross-ns).
(define (chez-resolve-field-tag tag by-name owner-ns)
  (cond ((or (not tag) (jolt-nil-t? tag)) jolt-nil)
        ((string=? tag "num") "num")
        ((string=? tag "double") "double")   ; a ^double field reads back as a flonum
        ;; The scalar tags a declared field can carry, normalized to the short
        ;; names jolt.passes.types/field-type-from-tag keys on. They resolve HERE,
        ;; ahead of the record-name lookup below, so a record that happens to be
        ;; named String cannot shadow the scalar reading.
        ;;
        ;; ^long reads back "num" rather than a long of its own: the lattice has no
        ;; :long scalar, and :num is the honest answer — the field holds a number,
        ;; which is what the checker and the flonum contagion need. Giving ^long the
        ;; fx arithmetic path needs a new lattice scalar; see the follow-up bead.
        ((string=? tag "long") "num")
        ((or (string=? tag "String") (string=? tag "java.lang.String")) "str")
        ((or (string=? tag "Keyword") (string=? tag "clojure.lang.Keyword")) "kw")
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
    (let-values (((ks vs) (jolt-with-mutex rec-tbl-mu
                            (let-values (((a b) (hashtable-entries chez-record-shapes-tbl)))
                              (values a b)))))
      (vector-for-each
        (lambda (k v)
          (let ((type-tag (vector-ref v 2)))
            (hashtable-set! by-name type-tag k)
            (hashtable-set! by-name (chez-shape-simple-name type-tag) k)))
        ks vs)
      ;; A type that owns its lookup is left OUT: the passes read this map to
      ;; decide that (:field x) is a slot read — the (:k (->T …)) fold and the
      ;; proven-struct guard drop both do — and for such a type it is not one.
      ;; Omitting the entry, rather than flagging it, is what keeps a consumer
      ;; from forgetting to ask: an absent ctor-key already means "not a shape I
      ;; know", which every reader handles (a nested field tag reads :any).
      (vector-for-each
        (lambda (k v)
          (let* ((fields (vector-ref v 0)) (tags (vector-ref v 1)) (type-tag (vector-ref v 2))
                 (owner-ns (chez-ctor-key-ns k))
                 (rtags (map (lambda (t) (chez-resolve-field-tag t by-name owner-ns)) tags)))
            (unless (chez-type-owns-lookup? type-tag)
              (set! out (jolt-assoc out k
                                    (jolt-hash-map kw-fields (apply jolt-vector fields)
                                                   kw-tags   (apply jolt-vector rtags)
                                                   kw-type   type-tag))))))
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
        (let loop ((ks (vector->list (jolt-with-mutex rec-tbl-mu (hashtable-keys chez-record-shapes-tbl)))))
          (cond ((null? ks) #f)
                ((string=? (chez-shape-simple-name (car ks)) target) (car ks))
                (else (loop (cdr ks))))))))

;; materialize chez-protocol-methods-tbl into "ns/method" -> [proto method].
(define (chez-protocol-methods-map)
  (let ((out (jolt-hash-map)))
    (let-values (((ks vs) (jolt-with-mutex rec-tbl-mu
                            (let-values (((a b) (hashtable-entries chez-protocol-methods-tbl)))
                              (values a b)))))
      (vector-for-each
        (lambda (k v) (set! out (jolt-assoc out k (jolt-vector (car v) (cdr v)))))
        ks vs))
    out))

;; A type that declares its own clojure.lang.ILookup has its fields MASKED from
;; the get path: on the JVM a bare deftype has no key lookup but the one it
;; declares, so its valAt answers for a field-named key too, and reading the slot
;; first handed back whatever the slot held where valAt was there to transform it.
;; The slot stays in the index, biased negative, because every OTHER read still
;; wants it — .-field, the deftype macro's own field bindings, (set! (.-f x) v).
;; Encoding the mask in the value the get path already reads is what keeps
;; (:field r) on a defrecord at exactly the cost it had: a per-type flag
;; consulted ahead of the index measured 1.27x on that path.
;;
;; index of a declared field key, or #f (only an interned keyword can be one).
(define (jrec-field-index r k)
  (let ((i (hashtable-ref (jrdesc-index (jrec-desc r)) k #f)))
    (and i (if (fx<? i 0) (fx- -1 i) i))))
;; the slot the GET path may read — #f for a masked type, so get falls through to
;; the valAt that type declares.
(define (jrec-get-index r k)
  (let ((i (hashtable-ref (jrdesc-index (jrec-desc r)) k #f)))
    (and i (fx>=? i 0) i)))
;; #t when TAG answers every key through a valAt IT declares — a bare deftype
;; implementing clojure.lang.ILookup. Such a type has no field-first lookup at
;; all, so three things have to agree about it: the get path (which reads the
;; masked index below), the mask itself, and the record-shapes registry the
;; optimizing passes read. One predicate is what makes them agree — the passes
;; used to see such a type as an ordinary record shape and fold (:field (->T …))
;; straight to the ctor argument in a built binary, past the valAt (jolt-fpp3.1).
;; A defrecord is never one: its generated field-first lookup stands, and the JVM
;; will not compile one declaring another valAt.
(define (chez-type-owns-lookup? tag)
  (and (find-method-any-protocol tag "valAt")
       (not (hashtable-ref chez-record-type-tbl tag #f))
       #t))
;; Bias every field of DESC negative. Driven by register-protocol-method the
;; moment a non-record type registers a valAt; fkeys is walked (not the table's
;; keys) so nothing scans a hashtable another thread may be writing.
(define (jrdesc-mask-fields! desc)
  (let ((idx (jrdesc-index desc)))
    (vector-for-each
      (lambda (k)
        (let ((i (hashtable-ref idx k #f)))
          (when (and i (fx>=? i 0)) (hashtable-set! idx k (fx- -1 i)))))
      (jrdesc-fkeys desc))))
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
;; The get path. A bare deftype that DECLARES clojure.lang.ILookup answers every
;; key through its own valAt, field-named keys included: the JVM gives such a
;; type no key lookup at all, so a declared valAt is the only lookup there is and
;; cannot lose to a slot. Reading the slot first handed back whatever the slot
;; held where valAt was there to transform it — typed.clojure keeps a lazy thunk
;; in one and forces it in valAt, so (:variances tfn) came back as the raw thunk.
;; A defrecord is unaffected (its generated field-first lookup stands; the JVM
;; will not compile one declaring another valAt), and so is a deftype with no
;; valAt — for both, slot 6 of the per-type cache is #f and this costs the
;; vector-ref that cache already answers the other flags with.
;;
;; Which arity: the JVM's RT.get calls valAt(k) for (get x k) and valAt(k, nf)
;; for (get x k nf), but jolt's get seam hands 2-arg get a nil default and cannot
;; tell the two apart. A nil default picks the 2-arity when the type declares
;; one, which is right for every (get x k) and for (:k x); only an explicit
;; (get x k nil) reaches the 3-arity where the JVM would have called the 2-arity,
;; and a valAt pair that answers those two differently for a nil not-found is
;; answering the same question twice.
;;
;; jrec-lookup, NOT this, is the declared-slot read: it is what .-field and the
;; deftype macro's own field bindings go through, so a valAt body reading its
;; fields does not re-enter itself.
(define (jrec-declared-valat r) (vector-ref (jrdesc-ifc-of r) 6))
;; Which arity: the JVM's RT.get calls valAt(k) for (get x k) and valAt(k, nf)
;; for (get x k nf), but jolt's get seam hands 2-arg get a nil default and cannot
;; tell the two apart. A nil default picks the 2-arity when the type declares one,
;; which is right for every (get x k) and for (:k x); only an explicit
;; (get x k nil) reaches the 3-arity where the JVM would have called the 2-arity,
;; and a valAt pair answering those two differently for a nil not-found is
;; answering the same question twice.
(define (jrec-call-valat va coll k d)
  (let ((m3 (car va)) (m2 (cdr va)))
    (if (and m2 (jolt-nil? d))
        (jolt-invoke m2 coll k)
        (if m3 (jolt-invoke m3 coll k d) (jolt-invoke m2 coll k)))))
(define (jrec-ref coll k d)
  (if (eq? k jolt-deftype-kw)
      (jrec-tag coll)
      (let ((i (jrec-get-index coll k)))
        (if i (jrec-field-ref coll i) (jrec-ref-slow coll k d)))))
;; Everything the field slots did not answer: the extension map, then the type's
;; own lookup. A masked type arrives here for its OWN field keys too, which is
;; the point.
(define (jrec-ref-slow coll k d)
  (let* ((ext (jrec-ext coll))
         (v (if (jolt-nil? ext) jrec-absent (jolt-get ext k jrec-absent))))
    (if (eq? v jrec-absent)
        (cond ((jrec-declared-valat coll)
                => (lambda (va) (jrec-call-valat va coll k d)))
              ;; a defrecord that declares a valAt anyway (the JVM refuses to
              ;; compile one, but jolt has always allowed it): its fields are not
              ;; masked, so this only answers keys they miss.
              ((find-method-any-protocol (jrec-tag coll) "valAt")
                => (lambda (m) (jolt-invoke m coll k d)))
              ;; a deftype implementing clojure.lang.IPersistentSet.get
              ;; (get returns the element when present, else nil) — a
              ;; membership lookup, so (get an-ordered-set k) works.
              ((find-method-any-protocol (jrec-tag coll) "get")
                => (lambda (m) (let ((r (jolt-invoke m coll k))) (if (jolt-nil? r) d r))))
              (else d))
        v)))

;; The declared-slot read the deftype macro binds each immutable field with, and
;; the clojure.core/__deftype-field op the backend lowers that to. It is
;; jrec-lookup, i.e. deliberately NOT the get path: a method body reading its own
;; fields must not go through a valAt the same type declares, or every method
;; entry re-enters that valAt and allocates without bound. Reaching the slot
;; directly is also cheaper than the (get inst :field) it replaces, which walked
;; jolt-get's type cascade to arrive at the same place.
(define (jrec-field r k) (jrec-lookup r k jolt-nil))
(def-var! "clojure.core" "__deftype-field" jrec-field)

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
            (throw-jvm (quote IllegalArgumentException) (string-append "set! of an unknown field: " (jolt-final-str k)))))
      ;; (set! (.__methodImplCache f) …) on a plain fn: jolt has no protocol-method
      ;; cache, so this is a harmless no-op (the paired read is nil). See the
      ;; __methodImplCache method arm — schema's fn instrumentation drives both.
      (if (let ((kn (cond ((string? k) k) ((keyword-t? k) (keyword-t-name k)) (else #f))))
            (and kn (string=? kn "__methodImplCache")))
          v
          (throw-jvm (quote IllegalArgumentException) "set! of a field on a non-record"))))
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
;; The class-hash half of a record's hash is a murmur over the type tag, so it
;; is a per-TYPE constant — but it was recomputed on every hash of every
;; instance, walking the tag's characters each time. A symbol's hasheq is cached
;; by identity for exactly this reason (symbol-hasheq, hasheq.ss); this is the
;; same cache keyed by descriptor. Weak, so a redefined type's descriptor stays
;; collectable; no epoch guard because a descriptor's tag never changes.
(define jrdesc-class-hash-tbl (make-weak-eq-hashtable))
(define jrdesc-class-hash-mu (make-mutex))
(define (jrdesc-class-hash d)
  (or (hashtable-ref jrdesc-class-hash-tbl d #f)
      (let ((h (compute-symbol-hasheq #f (jrdesc-tag d))))
        (jolt-with-mutex jrdesc-class-hash-mu (hashtable-set! jrdesc-class-hash-tbl d h))
        h)))
(define (jrec-hash r)
  ;; JVM defrecord hasheq: (bit-xor class-hash map-hasheq)
  ;; class-hash = (hash classname-symbol).
  ;; Jolt symbols store qualified names as a single flat string (e.g. "user.Point")
  ;; with ns=#f, so class-hash = compute-symbol-hasheq(#f, tag).
  ;; map-hasheq = Murmur3.hashUnordered over fields+ext entries
  (let* ((class-hash (jrdesc-class-hash (jrec-desc r)))
         (fkeys (jrdesc-fkeys (jrec-desc r)))
         (n (jrec-nfields r))
         (result (let loop ((i 0) (acc 0) (cnt 0))
                   (if (= i n)
                       (cons acc cnt)
                       (loop (+ i 1)
                             (add32 acc (entry-hasheq (vector-ref fkeys i)
                                                      (jrec-field-ref r i)))
                             (+ cnt 1)))))
         (ext (jrec-ext r))
         (total (if (jolt-nil? ext)
                    result
                    (pmap-fold ext
                               (lambda (k v a)
                                 (cons (add32 (car a) (entry-hasheq k v))
                                       (+ (cdr a) 1)))
                               result)))
         (map-hash (mix-coll-hash (car total) (cdr total))))
    (i32 (bitwise-xor class-hash map-hash))))
;; Per-INSTANCE hasheq cache for defrecords — the JVM's __hasheq field
;; (core_deftype.clj emits it; computed once, then a field read). jolt's jrec
;; layout is image-format surface (a field means bumping every family tag), so
;; the cache is the hasheq slot on the instance — the JVM defrecord's __hasheq
;; field. DEFRECORDS ONLY (jrec-record?) fill it with the STRUCTURAL hash here; a
;; plain deftype fills it with its identity hash instead, and a type with a
;; declared hasheq/hashCode never fills it — both in records-coll.ss
;; jrec-hasheq-slow, the one dispatch site. The slot write is a plain fixnum
;; store with no lock: racing writers compute the same value (a structural hash
;; is deterministic, and the identity table answers one id per object), so a
;; double store is benign. The slot never travels: the image dump zeroes it
;; (state-image.ss), as the JVM marks __hasheq transient.
(define (jrec-hash-cached r)
  (if (jrec-record? r)
      (let ((h (jrec-hasheq r)))
        (if (eqv? h 0)
            (let ((h2 (jrec-hash r))) (jrec-hasheq-set! r h2) h2)
            h))
      (jrec-hash r)))
;; A non-record deftype that declares a collection interface prints in THAT
;; collection's shape, which is how print-method dispatches on the JVM: ISeq as
;; (…), IPersistentVector as […], IPersistentSet as #{…}, IPersistentMap as
;; {k v, …}. The shape is derived per type by jrdesc-derive-ifc, which is where the
;; precedence and the choice of these four are explained; a defrecord keeps its
;; #ns.Name{…} form.
;;
;; Elements render through jolt-pr-readable, so print / println reach this too
;; (with *print-readably* off) rather than a type's toString, again as on the JVM.
(define (jrec-coll-print-shape r) (vector-ref (jrdesc-ifc-of r) 1))
(define (jrec-coll-pr r shape)
  (if (jolt-print-hash?)
      "#"
      (with-deeper-print
        (if (eq? shape 'map)
            ;; a map's seq yields entries; render each as "k v" and comma-join, in
            ;; the order the type's own seq produced them (the JVM prints an
            ;; insertion-ordered map-like deftype in that order too).
            (string-append "{"
              (jolt-str-join-comma
                (jolt-limited-list-strs
                  (map (lambda (e) (string-append (jolt-pr-readable (jolt-nth e 0)) " "
                                                  (jolt-pr-readable (jolt-nth e 1))))
                       (seq->list r))))
              "}")
            (let ((body (jolt-str-join (jolt-limited-seq-strs (jolt-seq r) jolt-pr-readable))))
              (case shape
                ((seq) (string-append "(" body ")"))
                ((vec) (string-append "[" body "]"))
                (else (string-append "#{" body "}"))))))))
(define (jrec-pr r)                      ; #ns.Name{:k v, :k v}
  (cond
    ((jrec-coll-print-shape r) => (lambda (shape) (jrec-coll-pr r shape)))
    (else (jrec-field-pr r))))
(define (jrec-field-pr r)
  ;; one "k v" string per entry, joined once: the extension map is unbounded
  ;; (any non-field key assoc'd on lands there), and appending each entry to a
  ;; growing accumulator is quadratic in the entry count.
  (let* ((fkeys (jrdesc-fkeys (jrec-desc r)))
         (n (vector-length fkeys))
         (entry-strs
          (let loop ((i 0) (acc '()))
            (if (= i n)
                (let ((ext (jrec-ext r)))
                  (reverse
                   (if (jolt-nil? ext) acc
                       (fold-left (lambda (a p)
                                    (cons (string-append (jolt-pr-readable (car p)) " "
                                                         (jolt-pr-readable (cdr p)))
                                          a))
                                  acc (jrec-ext-pairs ext)))))
                (loop (+ i 1)
                      (cons (string-append (jolt-pr-readable (vector-ref fkeys i)) " "
                                           (jolt-pr-readable (jrec-field-ref r i)))
                            acc))))))
    ;; the JVM spelling of the tag (my_app.core.Foo for a type in my-app.core):
    ;; what the JVM's reader resolves a record literal by, and what its own
    ;; printer writes. jolt's reader takes either spelling (reader.ss).
    (string-append "#" (jch-munge-segments (jrec-tag r)) "{" (jolt-str-join-comma entry-strs) "}")))
