;; state-image.ss — dump a running program's state to a file and read it back.
;;
;; This is a STATE image, not a process image. Chez removed save-heap, and
;; fasl-write rejects every procedure, continuation, port and thread, so nothing
;; here can capture in-flight execution. What travels is the value graph.
;;
;; The body is pure data by construction: a procedure is written as the NAME of
;; the var it is bound to, never as code. Because the stream then holds no code
;; objects, Chez stamps it machine-type 0 (machine-independent), which is what
;; makes restoring on another architecture work.
;;
;; File layout — three fasl objects written back-to-back on one port:
;;   1. header    : vector, version + compat fields
;;   2. externals : list of descriptors, one per object fasl-write refused
;;   3. body      : the graph, with each refused object replaced by a placeholder
;; Reading resolves the descriptors first and hands the resulting vector to
;; fasl-read, which fails loudly if the count disagrees with the body.
;;
;; Loaded LAST from rt.ss: needs the collections, the var table, the printers,
;; and proc-name-tbl (rt.ss) for the procedure -> "ns/name" direction.

;; 2: closures travel as source records (image-fnsrc), sorted colls as
;; image-sorted, unhandled resources as image-stub. A version-1 reader
;; refuses these images by the header check instead of misreading records.
;; 3: refs travel as image-ref descriptors instead of raw jolt-ref records,
;; so the ref record layout is no longer image-format surface. A version-2
;; reader refuses these images by the header check — on a runtime without
;; the descriptor a ref would restore as an inert record, silently wrong.
;; 4: hash containers whose key subtree reaches a procedure travel as
;; image-rekey entry records and REBUILD on restore — their trie placement is
;; derived from per-process procedure identity hasheqs, so a raw copy answered
;; nil to every lookup in the restoring process. A version-3 reader refuses
;; these images by the header check instead of restoring the record inert.
;;
;; Version 7: a map's record is chez-pmap-v5 — an array-mode map is a flat k/v
;; slot vector, where formats 6 and older carried a trie root plus an order
;; list (chez-pmap-v4). Those restore through image-legacy-pmap? below.
;;
;; This build still READS versions 2 to 6: everything they can contain
;; (including raw jolt-ref-v1 records) restores here via the legacy arms.
(define jolt-image-format-version 7)
(define jolt-image-read-versions '(2 3 4 5 6 7))

;; --- classification -----------------------------------------------------------
;; An eq hashtable is the ONE hashtable kind Chez can fasl; eqv/equal/string-hash
;; tables carry their hash and equivalence procedures, so they need descriptors.
(define (image-eq-hashtable? x)
  (and (hashtable? x) (eq? (hashtable-equivalence-function x) eq?)))

;; Objects that must not travel as raw fasl. Two distinct reasons:
;;
;;  - Chez REFUSES them (procedures, non-eq hashtables, ports, threads).
;;  - Chez would happily write them, but the copy that comes back is WRONG.
;;    Keywords are interned (values.ss) and jolt map lookup compares them by
;;    identity, so a fasl-copied keyword is a key nothing can ever find: the map
;;    prints and counts correctly but every (:k m) returns nil and = is false.
;;    They are re-interned through the externals path instead. Their cached
;;    khash is content-derived, so re-interning yields an equal hash and the
;;    restored trie stays valid.
;;
;; Symbols are deliberately NOT here: they are not interned and compare by
;; ns/name, so a copy behaves correctly as a map key.
(define (image-external? x)
  (or (procedure? x)
      (keyword? x)
      (and (hashtable? x) (not (image-eq-hashtable? x)))
      (port? x)
      (thread? x)
      ;; A var-rooted multimethod or reify: code, like a named fn, and written the
      ;; same way -- as the var's name through the fn-ref descriptor. code-value?
      ;; is a two-predicate check and runs before the table lookup, so this costs
      ;; a walked value nothing it did not already pay (jolt-2cny).
      (jns? x)
      (and (code-value? x) (proc-name-of x) #t)))

;; A record's fields for image purposes: its own AND every parent's, root first —
;; the order record-constructor takes them in. record-type-field-names reports only
;; a type's own fields, so walking by it alone misses the inherited ones and
;; rebuilding by it alone misapplies the constructor. That is not a corner case:
;; every defrecord/deftype instance is a chez-jrec subtype whose descriptor and
;; extension map live on the PARENT, so a record holding an atom used to fail the
;; dump outright ("incorrect number of arguments to #<procedure constructor>").
;; Memoized per rtd — the walk asks once per object.
(define image-record-fields-tbl (make-eq-hashtable))
(define image-record-fields-mutex (make-mutex))
(define (image-record-fields rtd)      ; -> vector of (name . accessor)
  (or (hashtable-ref image-record-fields-tbl rtd #f)
      (let* ((chain (let up ((r rtd) (acc '()))
                      (if r (up (record-type-parent r) (cons r acc)) acc)))
             (fields (apply append
                            (map (lambda (r)
                                   (let ((ns (record-type-field-names r)))
                                     (let loop ((i 0) (acc '()))
                                       (if (fx=? i (vector-length ns))
                                           (reverse acc)
                                           (loop (fx+ i 1)
                                                 (cons (cons (vector-ref ns i) (record-accessor r i))
                                                       acc))))))
                                 chain)))
             (v (list->vector fields)))
        (jolt-with-mutex image-record-fields-mutex
          (hashtable-set! image-record-fields-tbl rtd v))
        v)))
;; A field read that answers #f rather than escaping: an opaque or uninitialized
;; field must not abort a dump that is only trying to find what is in it.
(define (image-record-field-ref f x)
  (call/cc (lambda (k)
             (with-exception-handler (lambda (e) (k #f))
               (lambda () ((cdr f) x))))))
;; A jrec's descriptor is its TYPE, not its value: every instance of the type
;; shares one, and it rides raw in the fasl (which is why its layout is frozen —
;; see make-jrdesc). Walking it would copy it per instance, giving each restored
;; record its own descriptor, and would drag the descriptor's internal eq
;; hashtable through the transform. The record's own fields and its extension map
;; are the value, and those are walked.
(define (image-opaque-field? v) (jrdesc? v))

;; --- path-tracking walker ------------------------------------------------------
;; fasl-write's externals-pred sees objects but not where they live, and an
;; "unserializable object" error with no path is close to useless on a real
;; application state graph. So the walk is ours: it classifies every reachable
;; object and, for anything it cannot encode, records the route to it.

(define (image-path->string path)
  ;; path is accumulated innermost-first
  (let loop ((p (reverse path)) (acc ""))
    (if (null? p)
        (if (string=? acc "") "<root>" acc)
        (loop (cdr p)
              (if (string=? acc "") (car p) (string-append acc " -> " (car p)))))))

;; Why an unregistered PROCEDURE cannot be written, and what to do instead. A fn
;; is writable only through its recorded source (:src-form + :free-names, emitted
;; by the back end's fnsrc registration); an unregistered one has nothing to
;; rebuild from.
;;
;; A fn literal the compiler SPLICED is registered like any other (the copy
;; carries its own capture list — see image-recover-free-values), so inlining is
;; no longer a way to land here. What is: a fn the language itself built rather
;; than analyzed from a literal, one whose namespace is not registered at all
;; (the core tier), and one whose source form holds a value the back end could
;; not render, which drops the registration at emit.
;; Empty for a non-procedure, whose refusal has nothing to do with any of this.
(define (image-unregistered-fn-hint x)
  (if (procedure? x)
      (string-append
        ": this fn has no recorded source, so there is nothing to rebuild it from."
        " Anonymous fns written in your own namespaces travel; ones the runtime"
        " built for you do not. Store a top-level fn, or the data to rebuild one.")
      ""))

(define (image-describe-obj x)
  (cond
    ((procedure? x) "#<procedure>")
    ;; Never PRINT a seq to describe it. Printing forces it, and an unrealized
    ;; one can be infinite -- (image-scan (repeat :z)) would hang instead of
    ;; reporting. A description exists to identify the object, and this does.
    ;; Reachable since an unwritable lazy cell is refused as ITSELF rather than
    ;; as the anonymous procedure inside it (jolt-zr91).
    ((jolt-lazyseq? x) "#<lazy-seq>")
    ((cseq? x) "#<seq>")
    ((port? x) "#<port>")
    ((thread? x) "#<thread>")
    ((hashtable? x) "#<hashtable>")
    (else (call/cc (lambda (k)
            (with-exception-handler (lambda (e) (k "#<object>"))
              (lambda () (jolt-pr-readable x))))))))

;; Walk the graph. Calls (visit obj path) on every object; collects nothing
;; itself. Cycle-safe via an eq table of in-progress/seen nodes.
(define (image-walk root visit)
  (let ((seen (make-eq-hashtable)))
    (let walk ((x root) (path '()))
      (unless (or (null? x) (boolean? x) (number? x) (char? x)
                  (symbol? x) (string? x) (bytevector? x))
        (if (hashtable-ref seen x #f)
            #t
            (begin
              (hashtable-set! seen x #t)
              (visit x path)
              (cond
                ;; jolt collections first — their internal trie shape would make
                ;; useless paths, so walk them as maps/vectors/sets instead.
                ((pmap? x)
                 (pmap-fold-fwd x (lambda (k v acc)
                                    (walk k (cons "<key>" path))
                                    (walk v (cons (image-describe-obj k) path))
                                    acc)
                                #f))
                ;; the lookup value is a distinct object when a transient conj!
                ;; split it from its key, and it carries its own metadata
                ((pset? x)
                 (pset-fold-pairs x (lambda (e v acc)
                                      (walk e (cons (image-describe-obj e) path))
                                      (unless (eq? v e) (walk v (cons (image-describe-obj v) path)))
                                      acc)
                                  #f))
                ((pvec? x)
                 (let ((n (pvec-count x)))
                   (let loop ((i 0))
                     (when (fx<? i n)
                       (walk (pvec-nth-d x i jolt-nil) (cons (number->string i) path))
                       (loop (fx+ i 1))))))
                ;; root AND meta: meta is a FIELD of the cell (rt.ss), so fasl-write
                ;; sees it, so the walk has to. Skipping it let (def ^{:test (fn …)} v)
                ;; scan clean and then fail the dump on the very same graph, with no
                ;; path to name — and ^{:test fn} is where deftest puts a test body.
                ((var-cell? x)
                 (let ((vp (string-append "#'" (var-cell-ns x) "/" (var-cell-name x))))
                   (walk (var-cell-root x) (cons vp path))
                   (let ((m (var-cell-meta x)))
                     (when m (walk m (cons (string-append vp " meta") path))))))
                ;; cover val + watches + validator — everything fasl-write sees
                ((jolt-atom? x)
                 (walk (jolt-atom-val x) (cons "@" path))
                 (for-each (lambda (w) (walk w (cons "@watch" path)))
                           (jolt-atom-watches x))
                 (walk (jolt-atom-validator x) (cons "@validator" path)))
                ((pair? x) (walk (car x) (cons "car" path)) (walk (cdr x) (cons "cdr" path)))
                ((vector? x)
                 (let ((n (vector-length x)))
                   (let loop ((i 0))
                     (when (fx<? i n)
                       (walk (vector-ref x i) (cons (number->string i) path))
                       (loop (fx+ i 1))))))
                ((and (hashtable? x) (hashtable-mutable? x))
                 (let-values (((ks vs) (hashtable-entries x)))
                   (let loop ((i 0))
                     (when (fx<? i (vector-length ks))
                       (walk (vector-ref vs i) (cons (image-describe-obj (vector-ref ks i)) path))
                       (loop (fx+ i 1))))))
                ;; generic record: walk declared fields by name, inherited ones
                ;; included. Covers user defrecords, lazy seqs, refs, everything
                ;; not special-cased.
                ((and (record? x) (record-rtd x))
                 (let* ((fs (image-record-fields (record-rtd x)))
                        (n (vector-length fs)))
                   (let loop ((i 0))
                     (when (fx<? i n)
                       (let* ((f (vector-ref fs i))
                              (v (image-record-field-ref f x)))
                         (unless (image-opaque-field? v)
                           (walk v (cons (symbol->string (car f)) path))))
                       (loop (fx+ i 1))))))
                (else #t)))))))
  jolt-nil)

;; --- externals: encode one refused object as data ------------------------------
;; Returns a descriptor (pure data) or #f when the object cannot be encoded.
;; Handlers registered from jolt get first refusal, so an application can teach
;; the encoder about its own resources.
(define image-handlers '())   ; list of (pred dump restore)

(define (jolt-image-register-handler! pred dump restore)
  (set! image-handlers (cons (list pred dump restore) image-handlers))
  jolt-nil)

(define (image-handler-for x)
  (let loop ((hs image-handlers))
    (cond ((null? hs) #f)
          ((jolt-truthy? (jolt-invoke (caar hs) x)) (car hs))
          (else (loop (cdr hs))))))

(define (image-encode-external x)
  (let ((h (image-handler-for x)))
    (cond
      (h (list 'handler (jolt-invoke (cadr h) x)))
      ((keyword? x) (list 'kw (keyword-t-ns x) (keyword-t-name x)))
      ;; a namespace is interned by name, exactly like a keyword: a fasl copy
      ;; would be a SECOND `user` that merely = the live one, where find-ns is
      ;; identity-stable and a var round-trips to the identical var (jolt-ji1h)
      ((jns? x) (list 'ns (jns-name x)))
      ;; A named fn travels as its var's name. So does any other CODE value some
      ;; var roots -- a multimethod, a reify -- which is not a procedure but is
      ;; equally something the restoring build already has (rt.ss code-value?).
      ;; A bare closure has no stable identity to write, so it is refused here
      ;; and reported with its path.
      ((or (procedure? x) (proc-name-of x))
       (let ((p (proc-name-of x)))
         (and p (list 'fn-ref (car p) (cdr p)))))
      ;; A non-eq hashtable is refused rather than described. Its contents would
      ;; have to ride in the descriptor stream, which is written WITHOUT an
      ;; externals-pred — so a table holding procedures would blow up there,
      ;; outside the mechanism that is supposed to catch it. Refusing keeps the
      ;; failure inside the path-reporting path. (A sorted coll never reaches
      ;; this arm: the transformer intercepts the wrapper upstream.)
      (else #f))))

(define (image-decode-external d)
  (case (car d)
    ;; back through the intern table, so the restored keyword IS the live one
    ((kw) (keyword (cadr d) (caddr d)))
    ((ns) (intern-ns! (cadr d)))
    ((fn-ref)
     (let ((c (var-cell-lookup (cadr d) (caddr d))))
       (if (and c (not (jolt-var-unbound? (var-cell-root c))))
           (var-cell-root c)
           (jolt-throw (jolt-ex-info
                         (string-append "image: no var " (cadr d) "/" (caddr d)
                                        " in this build to restore a function reference")
                         empty-pmap)))))
    ((handler)
     (let loop ((hs image-handlers))
       (if (null? hs)
           (jolt-throw (jolt-ex-info "image: no handler registered to restore a resource" empty-pmap))
           ;; restore fns are tried in registration order; the first that accepts wins
           (let ((r (call/cc (lambda (k)
                      (with-exception-handler (lambda (e) (k 'image-no))
                        (lambda () (jolt-invoke (caddr (car hs)) (cadr d))))))))
             (if (eq? r 'image-no) (loop (cdr hs)) r)))))
    (else (jolt-throw (jolt-ex-info "image: unknown external descriptor" empty-pmap)))))

;; --- R2: substitution pre-pass --------------------------------------------------
;; A var root a handler claimed, replaced by the handler's plain-data payload.
;; Substituting through the transformer (not only at world var roots) is what
;; lets a handler claim a resource at ANY depth: the payload rides in the body,
;; so a function inside it becomes a fn-ref or an image-fnsrc and a keyword
;; inside it gets re-interned, exactly as if the application had stored that
;; data directly. Routing it through a descriptor instead would put it in the
;; one part of the file that cannot carry either.
(define-record-type image-handled (fields payload) (nongenerative image-handled-v1))

;; An anonymous closure a state image can rebuild from source (write side here;
;; R3 reconstructs). name is the unique jfn$<ns>$<def>$<n> the backend bound
;; the literal under — what Chez's inspector reports for the live closure;
;; form/ns/free-names come from the load-time registration (fn-form-registry.ss);
;; free-values are the LIVE captured values recovered by name through the
;; inspector and pushed back through this same pass, so a captured mutable cell
;; cycles into the written graph instead of pointing into the live one. Rides in
;; the BODY: its fields are walked by fasl, so a keyword inside re-interns, a
;; named fn goes fn-ref, and a nested anon closure was already substituted.
(define-record-type image-fnsrc
  (fields name form ns free-names (mutable free-values))
  (nongenerative image-fnsrc-v1))

;; A sorted map/set a state image rebuilds from the public constructors: kind
;; ('map | 'set), the ORIGINAL user comparator (jolt-nil for natural order), and
;; the entries in seq order — (key . val) pairs for a map, elements for a set.
;; The write-side arm on htable-sorted? intercepts the wrapper before the
;; externals path ever sees its internal comparator hashtable; cmp-fn composes
;; with the proc verdict (named fn -> fn-ref, registered literal -> image-fnsrc,
;; else refuse-with-path) and entries walk as plain data. Restore re-mints
;; through sorted-map(-by)/sorted-set(-by) and folds the entries in ordered.
(define-record-type image-sorted
  (fields kind cmp-fn entries)
  (nongenerative image-sorted-v1))

;; A hash container whose KEY subtree reaches a procedure. Its trie placement
;; is computed from procedure identity hasheqs, which are per-process
;; (hasheq.ss proc-hasheq-tbl) — travel it raw and every lookup in the
;; restoring process silently answers nil, because keys hash with ids the
;; stored placement was never built from. Same cure as sorted containers
;; (whose comparator-derived placement can't travel either): substitute an
;; entries record on the write side and REBUILD in the restoring process, so
;; placement is computed from the ids the lookups will actually use. kind is
;; 'map ((k . v) pairs) or 'set (elements — the stored lookup VALUE, which is
;; what the collection holds).
(define-record-type image-rekey
  (fields kind entries)
  (nongenerative image-rekey-v1))

;; A ref travels by VALUE: the write side substitutes this descriptor for the
;; live jolt-ref, restore re-mints a fresh ref through make-jolt-ref and copies
;; meta — so the ref record's layout is not image-format surface (refs.ss can
;; change it freely; the v1 lock-field freeze is lifted). val is mutable so a
;; self-referencing ref memoizes its descriptor before its val walks, exactly
;; like walk-atom. Watches and validators live in the weak iref side tables and
;; do not travel — the same as when refs rode raw. Images older than format 3
;; carry refs as raw jolt-ref-v1 records instead; see image-legacy-ref? below.
(define-record-type image-ref
  (fields (mutable val))
  (nongenerative image-ref-v1))

;; A mutex or condition variable, standing in for one in the written graph. Chez
;; fasls a mutex without complaint and the copy is NOT a live primitive -- but an
;; uncontended acquire on the copy SUCCEEDS, so nothing goes wrong until
;; something actually waits, and then it is "mutex-acquire: failed: Invalid
;; argument" from whichever thread reached it first, with no path and no name.
;;
;; kind is 'mutex or 'condition; restore mints a fresh live one. Substituting at
;; the WALK rather than through a walker arm per bearing type makes it total:
;; jolt-promise, jolt-future, jolt-agent, the per-node lock on a lazy cell and on
;; a seq, async channels, the tap queue and the fibers queues all carry one, and
;; a record added later carries it correctly without anyone reading this file.
;; jolt-atom and jolt-ref rebuild through their own constructors and so were
;; always right; they are the shape this generalizes (jolt-ojoh).
(define-record-type image-sync
  (fields kind)
  (nongenerative image-sync-v1))

;; A raw jolt-ref record in a format-2 image (v0.6.5/v0.6.6). The live runtime
;; type is jolt-ref-v2, so the fasl's nongenerative jolt-ref-v1 rtd (fields:
;; val lock) materializes from the image without conflict and its instances
;; arrive as inert records of that type — NOT jolt-ref? — which the restore
;; walk re-mints into live refs, reading val through the materialized rtd's
;; own accessor. The jolt-ref-v1 uid is RETIRED: it must never be reused for
;; a record with a different layout, or these images stop reading.
(define (image-legacy-ref? x)
  (and (record? x)
       (record-rtd x)
       (eq? (record-type-uid (record-rtd x)) 'jolt-ref-v1)))
;; A jrec from an image written before the hasheq slot (format <= 4): the
;; family's nongenerative tags bumped, so the fasl materializes the OLD rtds and
;; the instances answer #f to the current jrec?. Detected by uid prefix — every
;; family generation is chez-jrec… (chez-jrdesc… differs at the 8th char, and
;; the descriptor type is unchanged anyway) — and rebuilt into the current
;; family from its own rtd's accessors, hasheq starting unset.
(define (image-legacy-jrec? x)
  (and (record? x) (not (jrec? x))
       (let ((uid (record-type-uid (record-rtd x))))
         (and (symbol? uid)
              (let ((n (symbol->string uid)))
                (and (fx>=? (string-length n) 9)
                     (string=? (substring n 0 9) "chez-jrec")))))))
(define (legacy-ref-val x)
  ((record-accessor (record-rtd x) 0) x))
;; A map from an image written before the flat array-mode representation
;; (format <= 6): chez-pmap-v4's layout is (root cnt order hasheq all-kw), where
;; root is a trie in BOTH modes and `order`, for an array-mode map, the
;; (key . value) pairs in reverse insertion order (#f in hash mode). The tag
;; bumped, so the fasl materializes the old rtd and its instances answer #f to
;; pmap?. Detected by uid prefix like a legacy jrec and rebuilt through the
;; old rtd's accessors: an array-mode map's pairs become its slot vector, a
;; hash-mode root is kept as is — hnode/hcoll are unchanged, so it IS a
;; current trie.
(define (image-legacy-pmap? x)
  (and (record? x) (not (pmap? x))
       (let ((uid (record-type-uid (record-rtd x))))
         (and (symbol? uid)
              (let ((n (symbol->string uid)))
                (and (fx>=? (string-length n) 9)
                     (string=? (substring n 0 9) "chez-pmap")))))))
(define (legacy-pmap->pmap x)
  (let* ((rtd (record-rtd x))
         (root ((record-accessor rtd 0) x))
         (cnt ((record-accessor rtd 1) x))
         (ord ((record-accessor rtd 2) x)))
    (if (or (pair? ord) (null? ord))
        (let* ((ps (reverse ord)) (n (length ps)) (arr (make-vector (fx* 2 n))))
          (let loop ((ps ps) (i 0))
            (unless (null? ps)
              (vector-set! arr i (caar ps))
              (vector-set! arr (fx+ i 1) (cdar ps))
              (loop (cdr ps) (fx+ i 2))))
          (make-pmap arr n))
        (make-pmap root cnt))))

;; A resource the dump could not write (port, thread, non-eq hashtable,
;; unregistered closure) that stub mode substitutes in place of a refusal. id
;; is a per-dump ordinal; kind the object's class string; description its
;; human rendering; path the graph route to it; extra a jolt map of per-kind
;; detail from a registered describer (or jolt-nil). Rides in the BODY like
;; image-fnsrc. Restore keeps the record as an inert value unless a resolver
;; claims it; jolt.image/stubs lists them, resolve-stub! replaces them.
(define-record-type image-stub
  (fields id kind description path extra)
  (nongenerative image-stub-v1))

;; --- R6: stub describers and resolvers -----------------------------------------
;; A describer supplies per-kind detail for a stub. GUARDED: a describer that
;; throws contributes jolt-nil extra — it must never fail the dump.
(define image-stub-describers '())   ; list of (pred . f)
(define (jolt-image-register-stub-describer! pred f)
  (set! image-stub-describers (cons (cons pred f) image-stub-describers))
  jolt-nil)
(define (image-stub-describer-for x)
  (let loop ((ds image-stub-describers))
    (cond ((null? ds) #f)
          ((jolt-truthy? (jolt-invoke (caar ds) x)) (cdar ds))
          (else (loop (cdr ds))))))

;; A resolver turns a stub back into a live value during restore. pred-or-kind
;; is a jolt fn (gets the stub info map) or a kind string; f gets the info map
;; and returns the live value. Matching is registration-order first-accepting.
(define image-stub-resolvers '())   ; list of (pred-or-kind . f)
(define (jolt-image-register-stub-resolver! pred-or-kind f)
  (set! image-stub-resolvers (cons (cons pred-or-kind f) image-stub-resolvers))
  jolt-nil)

(define (image-stub-info s)
  ;; the info map resolver preds and jolt.image/stubs see
  (jolt-hash-map (jolt-keyword "id") (image-stub-id s)
                 (jolt-keyword "kind") (image-stub-kind s)
                 (jolt-keyword "description") (image-stub-description s)
                 (jolt-keyword "path") (image-stub-path s)
                 (jolt-keyword "extra") (image-stub-extra s)))

(define (image-stub-resolver-match spec info)
  (cond
    ((string? spec) (string=? spec (jolt-str-one (jolt-get info (jolt-keyword "kind")))))
    (else (guard (e (#t #f)) (jolt-truthy? (jolt-invoke spec info))))))

(define (image-stub-resolver-for s)
  (let ((info (image-stub-info s)))
    (let loop ((rs image-stub-resolvers))
      (cond ((null? rs) #f)
            ((image-stub-resolver-match (caar rs) info) (cdar rs))
            (else (loop (cdr rs)))))))

(define (image-stub-detail-of x)
  ;; the describer's detail, guarded: a throwing describer is jolt-nil extra
  (let ((d (image-stub-describer-for x)))
    (if d
        (call/cc (lambda (k)
          (with-exception-handler (lambda (e) (k jolt-nil))
            (lambda () (jolt-invoke d x)))))
        jolt-nil)))

;; kind is the class string via the class arms, with image-describe-obj as the
;; fallback so a stub always names itself even when the class chain cannot.
(define (image-stub-kind-of x)
  (guard (e (#t (image-describe-obj x)))
    (let ((n (jolt-class-name x)))
      (if (and (string? n) (not (string=? n "")))
          n
          (image-describe-obj x)))))

;; Built-ins: a file port stubs with direction, a recoverable name, and
;; open/closed state; a thread stubs with the id Chez exposes cheaply.
(jolt-image-register-stub-describer!
  (lambda (x) (port? x))
  (lambda (x)
    (jolt-hash-map
      (jolt-keyword "direction")
      (if (input-port? x) (jolt-keyword "input") (jolt-keyword "output"))
      (jolt-keyword "name")
      (guard (e (#t jolt-nil))
        (let ((n (port-name x))) (if (string? n) n jolt-nil)))
      (jolt-keyword "open")
      (not (port-closed? x)))))
(jolt-image-register-stub-describer!
  (lambda (x) (thread? x))
  ;; a Chez thread object exposes no identity of its own (get-thread-id names
  ;; the CALLING thread) — the kind string is the honest detail
  (lambda (x) (jolt-hash-map)))

;; backend_scheme.clj's munge-name, exposed so the write side munges registered
;; free names with EXACTLY the mapping the emitter used. Deref'd at dump time,
;; when the compiler core is loaded; the stateimage gate pins agreement between
;; this seam and the backend.
(def-var! "jolt.host" "munge-name"
  (lambda (s) (jolt-invoke1 (var-deref "jolt.backend-scheme" "munge-name") s)))

(define (image-munge s) (jolt-invoke1 (var-deref "jolt.host" "munge-name") s))


;; Carry the meta side-table entry from a rebuilt object's original (the weak
;; table in natives-meta.ss), so image-collect-meta keys the SUBSTITUTED
;; objects — the ones fasl-write actually sees.
(define (image-meta-copy! orig new)
  (when (not (eq? orig new))
    (let ((m (call/cc (lambda (k)
              (with-exception-handler (lambda (e) (k jolt-nil))
                (lambda () (jolt-meta orig)))))))
      (unless (jolt-nil? m)
        (meta-table-set! new m)))))

;; A procedure's substitution decision, shared by both modes so scan and dump
;; cannot disagree. Returns the registration (name . (form ns free-names)) for
;; a registered jfn$ literal, or #f when the closure must refuse: no inspector
;; name, not jfn$-prefixed, or no registration (core-tier closures, bare
;; unregistered lambdas, no-inspector builds).
(define (image-fnsrc-probe x)
  (guard (e (#t #f))
    (let* ((info (sa-procedure-info x))
           (nm (and info (car info))))
      ;; No prefix test: an anonymous literal is bound under jfn$..., a NAMED one
      ;; under <name>$jf<n>, and the registry lookup is the real question either
      ;; way -- a name nothing registered simply misses.
      (and (string? nm)
           (let ((reg (image-fn-form-lookup nm)))
             (and reg (cons nm reg)))))))

;; The one procedure decision, consumed by BOTH modes so scan and dump cannot
;; disagree. Precedence: a handler is claimed in the walk's outer cond before
;; this arm ever runs; then a named var-root fn (fn-ref — restore as the live
;; fn); then a registered jfn$ literal (returns (name . registration)); else
;; 'refuse. scan and dump branch on the same verdict, never on their own copies
;; of the rules.
(define (image-proc-verdict x)
  (cond
    ((proc-name-of x) 'fn-ref)
    (else (or (image-fnsrc-probe x) 'refuse))))

;; --- the EMIT-side consumer of the verdict above -----------------------------
;; A macro may put a live VALUE in the form it returns. Clojure's compiler falls
;; through to ConstantExpr for anything it does not recognize, so this works
;; there — verified on the 1.12.5 oracle, AOT included — and sci's copy-var does
;; exactly it with a macro var's root, which is what jolt-l7tq is.
;;
;; jolt compiles to Scheme TEXT, so the value cannot simply be spelled: it has to
;; be rendered as an expression that rebuilds it. That is the same problem the
;; image writer solves, and this reads the same verdict rather than a second copy
;; of the rules — the image scan side, the image dump side and the back end now
;; agree by construction about which fns can be rebuilt from source.
;;
;;   {:kind :var, :ns, :name}                  a named var-root fn or code value
;;   {:kind :fnsrc, :form, :ns, :frees, :vals} a registered anon literal
;;   nil                                       nothing to rebuild it from
;;
;; The free values come back LIVE (the identity walk), because the back end
;; renders them itself — each one re-enters the analyzer and lands back here if
;; it is opaque too. A capture the compiler const-folded away is unrecoverable
;; and reported as nil, exactly as it is refused at dump.
(define (image-embed-plan x)
  ;; The cheap test FIRST. The analyzer asks this about every quoted form, and
  ;; image-proc-verdict's second arm probes the inspector inside a guard —
  ;; sa-procedure-info raises on anything that is not a procedure, so letting an
  ;; ordinary quoted list reach it costs an exception per form. Measured: five
  ;; quoted forms per (is …) took compiling 500 of them from 0.6s to 10.4s.
  ;; This is exactly the test the image walk applies before its own procedure
  ;; arm (a code value some var roots is not a `procedure?` but is nameable).
  (if (not (or (procedure? x) (proc-name-of x)))
      jolt-nil
      (image-embed-plan* x)))
(define (image-embed-plan* x)
  (let ((v (image-proc-verdict x)))
    (cond
      ((eq? v 'refuse) jolt-nil)
      ((eq? v 'fn-ref)
       (let ((p (proc-name-of x)))
         (if p
             (jolt-hash-map (keyword #f "kind") (keyword #f "var")
                            (keyword #f "ns") (car p)
                            (keyword #f "name") (cdr p))
             jolt-nil)))
      ((pair? v)
       (let* ((reg (cdr v))
              (frees (vector-ref reg 2))
              (fvs (image-recover-free-values x reg frees (vector-ref reg 3)
                                              (lambda (val path) val) '())))
         (cond
           ((vector? fvs)
            (jolt-hash-map (keyword #f "kind") (keyword #f "fnsrc")
                           (keyword #f "form") (vector-ref reg 0)
                           (keyword #f "ns") (vector-ref reg 1)
                           (keyword #f "frees") frees
                           (keyword #f "vals") (apply jolt-vector (vector->list fvs))))
           ;; The source form still names a capture the compiled closure does not
           ;; carry, because cp0 folded its value into the code. Say WHICH — the
           ;; caller's message is the only place a reader learns that the fix is
           ;; to stop closing over a constant, and "no recorded source" would be
           ;; a plain lie about a fn whose source is recorded.
           ((and (pair? fvs) (eq? (car fvs) 'image-folded))
            (jolt-hash-map (keyword #f "kind") (keyword #f "folded")
                           (keyword #f "name") (cdr fvs)))
           (else jolt-nil))))
      (else jolt-nil))))
(def-var! "jolt.host" "embed-plan" image-embed-plan)

;; Recover the LIVE captured values, in REGISTERED free-name order, by munging
;; each original name and matching it against the inspector's munged names. A
;; registered name the inspector does not report (cp0 dropped a dead capture)
;; binds jolt-nil. Every recovered value runs back through the transformer so
;; the written graph never points into the live one. Any inspector/munge
;; failure returns 'image-no (the caller refuses) — never a crash, never a
;; silent partial; the free-value walk itself is unguarded so a refusal on a
;; nested free value keeps its own, more specific path.
;; Which closure SLOT holds each of a site's free names, as a list index-aligned
;; with free-names. Derived once per site and cached on the registration.
;;
;; Chez hands a closure's captures back by position; the names that say which is
;; which are inspector information, which a release build does not generate (it
;; costs +117% on the compiled prelude to name a few hundred captures out of
;; every procedure in core). So `jolt run` could not write any closure
;; clojure.core makes — cycle, repeat, partial, comp — while a default app build,
;; which does generate it, wrote them fine.
;;
;; The maker settles it. Every instance of a site comes from one code object, so
;; the capture layout is a property of the CODE and identical across instances:
;; call the maker once with distinct sentinels, see which slot each landed in,
;; and read every later instance through that permutation. 'none when there is no
;; maker, when the probe cannot be read, or when a sentinel does not appear at
;; all — Chez dropped a capture the body never uses, and guessing which of the
;; remaining slots is which is exactly the wrong answer to give a restore.
(define (image-fnsrc-layout reg)
  (let ((cached (image-fn-form-layout reg)))
    (if cached
        cached
        (let ((mk (image-fn-form-maker reg)))
          (let ((v (if (not mk)
                       'none
                       (guard (e (#t 'none))
                         (let* ((frees (vector-ref reg 2))
                                (n (let loop ((f (jolt-seq frees)) (k 0))
                                     (if (jolt-nil? f) k (loop (jolt-next f) (fx+ k 1)))))
                                (sent (let loop ((i 0) (acc '()))
                                        (if (fx=? i n)
                                            (reverse acc)
                                            (loop (fx+ i 1) (cons (list 'jolt-fnsrc-probe i) acc)))))
                                (slots (sa-procedure-free-values (apply mk sent))))
                           (if (not slots)
                               'none
                               (let ((perm (map (lambda (s)
                                                  (let loop ((l slots) (i 0))
                                                    (cond ((null? l) #f)
                                                          ((eq? (car l) s) i)
                                                          (else (loop (cdr l) (fx+ i 1))))))
                                                sent)))
                                 (if (memq #f perm) 'none perm))))))))
            (image-fn-form-layout-set! reg v)
            v)))))

(define (image-recover-free-values x reg frees lives walk path)
  (call/cc
    (lambda (refuse)
      (define (refuse-on-fail thunk)
        (guard (e (#t (refuse 'image-no))) (thunk)))
      (let* ((layout (and reg (let ((l (image-fnsrc-layout reg))) (and (pair? l) l))))
             (slots  (and layout (sa-procedure-free-values x)))
             (info (refuse-on-fail (lambda () (sa-procedure-info x))))
             ;; No inspector information at all is fatal only when there is no
             ;; layout to read positions through.
             (tbl (begin (unless (or info layout) (refuse 'image-no))
                         (let ((h (make-hashtable string-hash string=?)))
                           (when info
                             (for-each (lambda (p) (hashtable-set! h (car p) (cdr p)))
                                       (cdr info)))
                           h))))
        ;; frees and lives run in lockstep: frees names the wrapper parameter (and
        ;; the error message), lives says where this one's value comes from — a
        ;; variable in the live closure, or, for a spliced copy whose constant
        ;; argument left no capture, a one-element vector holding the value.
        (let loop ((fs (jolt-seq frees)) (ls (jolt-seq lives)) (k 0) (acc '()))
          (if (jolt-nil? fs)
              (list->vector (reverse acc))
              (let* ((orig (jolt-first fs))
                     (live (if (jolt-nil? ls) orig (jolt-first ls)))
                     (val (if (string? live)
                              (let ((byname (hashtable-ref
                                              tbl
                                              (refuse-on-fail (lambda () (image-munge live)))
                                              'image-missing)))
                                ;; the name table first (it is exact when present),
                                ;; the site's learned layout when it has nothing
                                (if (and (eq? byname 'image-missing) layout slots)
                                    (let ((idx (list-ref layout k)))
                                      (if (and idx (fx<? idx (length slots)))
                                          (list-ref slots idx)
                                          'image-missing))
                                    byname))
                              (refuse-on-fail (lambda () (jolt-nth live 0))))))
                ;; A name the source references but the compiled closure does not
                ;; carry: const-folding baked its value into the code (a let-bound
                ;; constant, a provably-dead branch), so the value is UNRECOVERABLE
                ;; here while the stored source still needs it. Binding nil instead
                ;; would restore a closure that silently computes with nil — refuse,
                ;; naming the capture, so the failure is at dump time and actionable.
                (if (eq? val 'image-missing)
                    (refuse (cons 'image-folded orig))
                    (loop (jolt-next fs) (if (jolt-nil? ls) ls (jolt-next ls)) (fx+ k 1)
                          (cons (walk val (cons (string-append "free:" orig) path))
                                acc))))))))))

;; The image-fnsrc record is memoized BEFORE its free values are recovered, so
;; a capture cycle (closure -> atom -> closure) finds this record in the memo
;; instead of recursing forever; the free-values field is mutable for that fill.
(define (image-fnsrc-build x reg walk memo path make-stub)
  (let ((r (make-image-fnsrc (car reg) (vector-ref (cdr reg) 0)
                             (vector-ref (cdr reg) 1) (vector-ref (cdr reg) 2)
                             (vector))))
    (hashtable-set! memo x r)
    (let ((fvs (image-recover-free-values x (cdr reg) (vector-ref (cdr reg) 2)
                                          (vector-ref (cdr reg) 3) walk path)))
      (cond
        ((vector? fvs)
         (image-fnsrc-free-values-set! r fvs)
         (image-meta-copy! x r)
         r)
        ((and (pair? fvs) (eq? (car fvs) 'image-folded))
         ;; stub mode: the closure itself is the unwritable object — stub it,
         ;; naming the folded capture in the description
         (if make-stub
             (let ((s (make-stub x path
                                 (string-append "folded capture " (cdr fvs)))))
               (hashtable-set! memo x s)
               (image-meta-copy! x s)
               s)
             (jolt-throw (jolt-ex-info
                           (string-append "image: cannot write " (image-describe-obj x)
                                          " at " (image-path->string path)
                                          ": captured local '" (cdr fvs)
                                          "' was optimized into the compiled code, so its value"
                                          " cannot be recovered from the live closure —"
                                          " store a named fn, or the data to rebuild one")
                           empty-pmap))))
        (else
         (jolt-throw (jolt-ex-info
                       (string-append "image: cannot write " (image-describe-obj x)
                                      " at " (image-path->string path))
                       empty-pmap)))))))

;; A condition's human text, best effort — jolt ex-info and raw Chez
;; conditions both pass through here on the restore failure path.
(define (image-condition-text e)
  (cond
    ((and (jolt-ex-info-record? e) (string? (jolt-ex-info-record-message e)))
     (jolt-ex-info-record-message e))
    ((condition? e) (condition->message-string e))
    (else "error")))

;; The compile spine, reached through the top level at CALL time —
;; compile-eval.ss loads after this file, and a tree-shaken build that
;; dropped the compiler has no binding at all: refuse by name instead of
;; surfacing an unbound-variable error mid-restore.
(define (image-compile-eval-seam)
  (let ((ce (sa-baked-global 'jolt-compile-eval-form)))
    (and (procedure? ce) ce)))

;; Rebuild one fn source record into a live closure: compile
;; (fn* [free-names…] form) in the record's defining ns, then apply it to
;; the restored free values. The wrapper params SHADOW the outer-scope
;; names the body references — that is what reconstructs the lexical
;; environment the closure was compiled in.
(define (image-eval-fnsrc x tfvs)
  (let ((ce (image-compile-eval-seam)))
    (unless ce
      (jolt-throw (jolt-ex-info
                    (string-append "image: this build has no compiler; cannot rebuild fn "
                                   (image-fnsrc-name x) " from source"
                                   " (a tree-shaken build that dropped the compiler"
                                   " cannot restore images holding anonymous fns)")
                    empty-pmap)))
    (let* ((frees (image-fnsrc-free-names x))
           (params (let loop ((s (jolt-seq frees)) (acc '()))
                     (if (jolt-nil? s)
                         (reverse acc)
                         (loop (seq-more s) (cons (jolt-symbol #f (seq-first s)) acc)))))
           (wrapper (list->cseq (list (jolt-symbol #f "fn*")
                                      (apply jolt-vector params)
                                      (image-fnsrc-form x))))
           (wfn (guard (e (#t (jolt-throw (jolt-ex-info
                                            (string-append "image: cannot compile fn "
                                                           (image-fnsrc-name x) " in ns "
                                                           (image-fnsrc-ns x) ": "
                                                           (image-condition-text e))
                                            empty-pmap))))
                  (ce wrapper (image-fnsrc-ns x)))))
      (apply jolt-invoke wfn tfvs))))

;; Restore fns are tried in registration order; the first that accepts wins —
;; the same contract the externals handler path has always had.
(define (image-restore-handler payload)
  (let loop ((hs image-handlers))
    (if (null? hs)
        (jolt-throw (jolt-ex-info "image: no handler registered to restore a resource" empty-pmap))
        (let ((r (call/cc (lambda (k)
                   (with-exception-handler (lambda (e) (k 'image-no))
                     (lambda () (jolt-invoke (caddr (car hs)) payload)))))))
          (if (eq? r 'image-no) (loop (cdr hs)) r)))))

;; The one traversal skeleton in two modes, so scan and dump share the arms and
;; cannot disagree about what is writable. 'rebuild returns a substituted copy
;; of the graph — identity for any subtree that holds nothing to substitute,
;; dirtiness tracked bottom-up — and throws jolt-ex-info on the first object
;; the write path cannot encode, with the route to it; 'report performs the
;; same descent and decisions but records a finding via (report! obj path)
;; instead of building or throwing. The memo doubles as the cycle guard:
;; mutable cells (atoms, var cells, mutable hashtables, fnsrc records) are
;; memoized before their children fill in, so a cycle through them resolves.
;; The read side is 'rebuild plus fnsrc/handled reconstruction, so the two
;; modes share every container arm; only report diverges.
(define (image-rebuild-mode? mode)
  (or (eq? mode 'rebuild) (eq? mode 'rebuild-stub) (eq? mode 'restore)))
(define (image-report-disposition mode)
  (if (eq? mode 'report-stub)
      (jolt-keyword "would-stub")
      (jolt-keyword "unwritable")))

;; Stub mode: a refusal builds an image-stub instead of throwing. 'rebuild-stub
;; substitutes stubs in; 'report-stub reports them with a :would-stub finding
;; instead of :unwritable.
(define (image-graph-process root mode report!)
  ;; MODE decided once, not re-asked per node. The walk is ONE traversal for all
  ;; four modes — what differs is whether a container MATERIALIZES a transformed
  ;; copy or just answers #t, plus a handful of leaf cases. Splitting it per mode
  ;; would copy all 30-odd container walkers four times, which is the opposite of
  ;; what this file needs; naming the policy is the part that was missing.
  ;;
  ;;   rebuild?   build a substituted/restored copy ('rebuild 'rebuild-stub 'restore)
  ;;   restore?   the read side, which has its own image-* leaf cases
  ;;   stubbing?  a refusal builds an image-stub instead of throwing
  ;;   reporting? walk for findings only, materializing nothing
  (let* ((rebuild?   (image-rebuild-mode? mode))
         (restore?   (eq? mode 'restore))
         (stubbing?  (eq? mode 'rebuild-stub))
         (reporting? (or (eq? mode 'report) (eq? mode 'report-stub)))
         (memo (make-eq-hashtable))
         (stub-acc '()))
    (letrec ((stub-id-box (list 0))
             (make-stub
               (lambda (x path desc)
                 (let ((id (car stub-id-box)))
                   (set-car! stub-id-box (+ id 1))
                   (let ((s (make-image-stub id (image-stub-kind-of x)
                                             (or desc (image-describe-obj x))
                                             (image-path->string path)
                                             (image-stub-detail-of x))))
                     (set! stub-acc (cons s stub-acc))
                     s))))
             ;; Did the walk just pass through a procedure? Set at walk ENTRY,
             ;; before the memo check, so a fn seen earlier (memoized) still
             ;; flips it. walk-key gives each hash-container key its own
             ;; save/or window: the container learns whether anything under
             ;; THIS key reaches a procedure (its hash then depends on a
             ;; per-process id -> the container must rekey, image-rekey above),
             ;; and a nested container's window ORs back into its parent's, so
             ;; an fn-keyed map used as a key marks the outer map too.
             (proc-touched #f)
             (walk-key
              (lambda (k path)
                (let ((saved proc-touched))
                  (set! proc-touched #f)
                  (let* ((r (walk k path))
                         (touched proc-touched))
                    (set! proc-touched (or saved touched))
                    (values r touched)))))
             (walk
              (lambda (x path)
                ;; procedures AND plain deftypes hash by per-process identity
                ;; (hasheq.ss jolt-identity-hasheq), so either under a key means
                ;; the container's placement cannot travel. A deftype with a
                ;; declared hashCode rekeys too — over-approximate, harmless.
                (when (or (procedure? x)
                          (and (jrec? x) (not (jrec-record? x))))
                  (set! proc-touched #t))
                (cond
                  ;; scalar leaves can never hold a procedure
                  ((or (null? x) (boolean? x) (number? x) (char? x)
                       (symbol? x) (string? x) (bytevector? x))
                   (if rebuild? x #t))
                  ((hashtable-ref memo x #f) =>
                   (lambda (m) (if rebuild? m #t)))
                  (else
                   (cond
                     ;; R3 read side: image-owned records rebuild first, before
                     ;; user handlers could claim them. A stored fn source record
                     ;; becomes a live closure; a stored handler payload is handed
                     ;; to the registered restore fn.
                     ((and restore? (image-fnsrc? x))
                      (walk-fnsrc-restore x path))
                     ((and restore? (image-handled? x))
                      (walk-handled-restore x path))
                     ((and restore? (image-sorted? x))
                      (walk-sorted-restore x path))
                     ((and restore? (image-rekey? x))
                      (walk-rekey-restore x path))
                     ;; a ref descriptor re-mints a live ref; a raw jolt-ref-v1
                     ;; record from a format-2 image re-mints through the
                     ;; legacy arm (same construction, val read via its own rtd)
                     ((and restore? (image-sync? x))
                      (if (eq? (image-sync-kind x) 'condition) (make-condition) (make-mutex)))
                     ((and restore? (image-ref? x))
                      (walk-ref-restore (image-ref-val x) x path))
                     ((and restore? (image-legacy-ref? x))
                      (walk-ref-restore (legacy-ref-val x) x path))
                     ((and restore? (image-legacy-jrec? x))
                      (walk-legacy-jrec x path))
                     ;; a pre-format-7 map, or a set over one, re-minted into
                     ;; the current representation and then walked like any map
                     ((and restore? (image-legacy-pmap? x))
                      (walk-legacy-pmap x path))
                     ((and restore? (pset? x) (image-legacy-pmap? (pset-m x)))
                      (walk-legacy-pmap x path))
                     ;; a stub with a matching resolver becomes the live value it
                     ;; stands for; without one it stays the inert record — the
                     ;; per-restore table (populated by restore-world!) lists it
                     ((and restore? (image-stub? x))
                      (let ((r (image-stub-resolver-for x)))
                        (if r
                            (let ((v (guard (e (#t (jolt-throw (jolt-ex-info
                                        (string-append "image: stub resolver failed for #"
                                                       (number->string (image-stub-id x))
                                                       " (" (image-stub-kind x) "): "
                                                       (image-condition-text e))
                                        empty-pmap))))
                                       (jolt-invoke r (image-stub-info x)))))
                              (hashtable-set! memo x v)
                              v)
                            (begin (hashtable-set! memo x x) x))))
                     ;; handlers claim at any depth, before anything else
                     ((and (pair? image-handlers) (image-handler-for x)) =>
                      (lambda (h)
                        (if rebuild?
                            (let ((r (make-image-handled (jolt-invoke (cadr h) x))))
                              (hashtable-set! memo x r)
                              r)
                            (begin (hashtable-set! memo x #t) #t))))
                     ;; a named fn stays in place: the fn-ref external restores
                     ;; it as the live fn (cheaper than source, no form needed).
                     ;; On the READ side a procedure IS an already-restored
                     ;; fn-ref external — force the fn-ref verdict (identity).
                     ((procedure? x)
                      (let ((v (if restore? 'fn-ref (image-proc-verdict x))))
                        (cond
                          ((eq? v 'fn-ref)
                           (if rebuild?
                               (begin (hashtable-set! memo x x) x)
                               #t))
                          ((eq? v 'refuse)
                           (cond
                             (stubbing?
                              (let ((s (make-stub x path #f)))
                                (hashtable-set! memo x s)
                                s))
                             (rebuild?
                              (jolt-throw
                                (jolt-ex-info
                                  (string-append "image: cannot write " (image-describe-obj x)
                                                 " at " (image-path->string path)
                                                 (image-unregistered-fn-hint x))
                                  empty-pmap)))
                             (else
                              (hashtable-set! memo x #t)
                              (report! x path (image-report-disposition mode)))))
                          (else
                           ;; v is a (name . registration) pair. Report mode must
                           ;; agree with what the build would do, so it prechecks
                           ;; recoverability (a const-folded capture refuses).
                           (if rebuild?
                               (image-fnsrc-build x v walk memo path
                                                  (if stubbing? make-stub #f))
                               ;; the REAL walk, not a stub: a captured value can
                               ;; itself be unwritable (a letfn fn a lazy-seq
                               ;; thunk closes over), and passing (lambda (fv p) #t)
                               ;; meant scan never looked -- it reported a value
                               ;; clean that dump then refused, which is exactly
                               ;; the scan/dump disagreement the shared verdict
                               ;; above exists to prevent.
                               (let ((probe (image-recover-free-values
                                              x (cdr v) (vector-ref (cdr v) 2)
                                              (vector-ref (cdr v) 3) walk path)))
                                 (hashtable-set! memo x #t)
                                 (if (vector? probe)
                                     #t
                                     (report! x path (image-report-disposition mode)))))))))
                     (else
                       ;; non-procedure externals the descriptor machinery
                       ;; cannot encode (non-eq hashtables, ports, threads):
                       ;; report them in scan; rebuild leaves them for the
                       ;; externals stage, which refuses with the path. A
                       ;; keyword and a named fn's fn-ref encode fine.
                       (cond
                         ;; stub-mode REBUILD substitutes; stub-mode REPORT must
                         ;; still report (as :would-stub), never swallow
                         ((and stubbing? (image-external? x)
                               (not (image-encode-external x)))
                          (let ((s (make-stub x path #f)))
                            (hashtable-set! memo x s)
                            s))
                         (else
                           (when (and reporting?
                                      (image-external? x)
                                      (not (image-encode-external x)))
                             (hashtable-set! memo x #t)
                             (report! x path (image-report-disposition mode)))
                           (cond
                             ((pmap? x) (walk-pmap x path))
                             ((pset? x) (walk-pset x path))
                             ((pvec? x) (walk-pvec x path))
                             ((htable-sorted? x) (walk-sorted x path))
                             ((var-cell? x) (walk-var-cell x path))
                             ((jolt-atom? x) (walk-atom x path))
                             ((jolt-ref? x) (walk-ref x path))
                             ((pair? x) (walk-pair x path))
                             ((vector? x) (walk-vector x path))
                             ((and (hashtable? x) (hashtable-mutable? x))
                              (walk-hashtable x path))
                             ;; A mutex or condition variable is per-process
                             ;; kernel state. fasl copies one happily and the
                             ;; copy is NOT a live primitive -- but an
                             ;; uncontended acquire on it succeeds, so nothing
                             ;; goes wrong until something actually waits, and
                             ;; then it is "mutex-acquire: failed: Invalid
                             ;; argument" from whichever thread got there first.
                             ;;
                             ;; Handled here rather than by a walker arm per
                             ;; bearing type, so it is TOTAL: jolt-promise,
                             ;; jolt-future, jolt-agent, the per-node locks on a
                             ;; lazy cell and a seq, async channels, the tap
                             ;; queue and the fibers queues all carry one, and a
                             ;; record added later carries it correctly without
                             ;; anyone remembering this file. jolt-atom and
                             ;; jolt-ref predate it and rebuild through their own
                             ;; constructors, which is why they were already
                             ;; right. A FRESH primitive, not #f: a lock field
                             ;; that is created on demand tolerates one either
                             ;; way, and a promise's mu is dereferenced
                             ;; unconditionally (jolt-ojoh).
                             ;; Execution does not travel, and these two are the
                             ;; cases where a record's own state says so.
                             ;;
                             ;; A future that has not completed is waiting on a
                             ;; thread the image cannot carry: restored, nothing
                             ;; will ever finish it, so `deref` hangs forever.
                             ;; Refuse it, the way any other unwritable object is
                             ;; refused -- naming it, and stubbing under stub
                             ;; mode. A completed one is just its value and
                             ;; travels.
                             ((and (jolt-future? x) (not (jolt-future-done? x)))
                              (cond
                                (stubbing?
                                 (let ((st (make-stub x path "a future that has not completed")))
                                   (hashtable-set! memo x st) st))
                                (rebuild?
                                 (jolt-throw
                                   (jolt-ex-info
                                     (string-append
                                       "image: cannot write a running future at "
                                       (image-path->string path)
                                       ": it is waiting on a thread, and a state image"
                                       " carries state, not execution. Deref it first,"
                                       " or store what it computes.")
                                     empty-pmap)))
                                (else (hashtable-set! memo x #t)
                                      (report! x path (image-report-disposition mode)))))
                             ;; An agent's QUEUE is pending execution too. Its
                             ;; state travels; the actions behind it do not, and
                             ;; carrying `running?` across would leave the
                             ;; restored agent believing a worker it does not
                             ;; have is mid-action, so every later send would
                             ;; queue behind nothing and never run -- silently
                             ;; wedged, which is worse than dropping them.
                             ((jolt-agent? x)
                              (if rebuild?
                                  ;; mu/cv go through the walk like any other
                                  ;; field, so the marker/mint rule below covers
                                  ;; this arm in both directions rather than
                                  ;; being restated here (they are immutable
                                  ;; fields, hence walked before construction).
                                  (let ((nx (make-jolt-agent jolt-nil jolt-nil jolt-nil
                                                             (vector '() '()) #f
                                                             (walk (jolt-agent-mu x) (cons "@mu" path))
                                                             (walk (jolt-agent-cv x) (cons "@cv" path))
                                                             (jolt-agent-err-mode x) jolt-nil)))
                                    (hashtable-set! memo x nx)
                                    (image-meta-copy! x nx)
                                    (jolt-agent-state-set! nx (walk (jolt-agent-state x) (cons "@" path)))
                                    (jolt-agent-err-set! nx (walk (jolt-agent-err x) (cons "@err" path)))
                                    (jolt-agent-validator-set! nx
                                      (walk (jolt-agent-validator x) (cons "@validator" path)))
                                    (jolt-agent-err-handler-set! nx
                                      (walk (jolt-agent-err-handler x) (cons "@error-handler" path)))
                                    nx)
                                  (begin
                                    (hashtable-set! memo x #t)
                                    (walk (jolt-agent-state x) (cons "@" path))
                                    (walk (jolt-agent-err x) (cons "@err" path))
                                    (walk (jolt-agent-validator x) (cons "@validator" path))
                                    (walk (jolt-agent-err-handler x) (cons "@error-handler" path))
                                    #t)))
;; An unrealized lazy cell whose thunk is a closure the image cannot record.
                             ;; clojure.core's NATIVE producers carry a descriptor
                             ;; and travel (below); its overlay ones -- cycle,
                             ;; repeatedly, map-indexed and the rest -- are fn
                             ;; literals in clojure.core, and the language's own
                             ;; namespaces are not registered, which is the same
                             ;; limit that stops a partial/comp closure travelling.
                             ;; Refuse by NAME rather than let the generic
                             ;; procedure refusal report an anonymous #<procedure>
                             ;; at a path ending in "thunk" (jolt-zr91).
                             ((and (jolt-lazyseq? x)
                                   (not (jolt-lazyseq-realized? x))
                                   (procedure? (jolt-lazyseq-thunk x))
                                   (not (image-fnsrc-probe (jolt-lazyseq-thunk x))))
                              (cond
                                (stubbing?
                                 (let ((st (make-stub x path "an unrealized lazy sequence")))
                                   (hashtable-set! memo x st) st))
                                (rebuild?
                                 (jolt-throw
                                   (jolt-ex-info
                                     (string-append
                                       "image: cannot write an unrealized lazy sequence at "
                                       (image-path->string path)
                                       ": it was produced by a clojure.core fn whose body the"
                                       " image cannot record, the same reason a partial or comp"
                                       " closure cannot travel. Realize it first (doall), or"
                                       " store the data it produces.")
                                     empty-pmap)))
                                (else (hashtable-set! memo x #t)
                                      (report! x path (image-report-disposition mode)))))
                             ;; A clojure.core lazy producer, recorded as its
                             ;; arguments plus a forcer (seq.ss lazy-src). The
                             ;; forcer is a procedure and cannot travel, so the
                             ;; image carries the producer's NAME in its place
                             ;; and the restore puts the live one back. The
                             ;; arguments are ordinary values and walk as data,
                             ;; so a producer over another lazy seq nests and a
                             ;; self-referential one closes on the memo.
                             ;;
                             ;; Restoring a seq this way, rather than forcing it
                             ;; at dump, is the whole point: an infinite seq
                             ;; keeps generating and a side effect still has not
                             ;; run (jolt-a6k2).
                             ((and (lazy-src? x)
                                   (if restore?
                                       (lazy-src-proc-of (lazy-src-fn x))
                                       (lazy-src-name-of (lazy-src-fn x))))
                              => (lambda (swapped)
                                   (if rebuild?
                                       (let ((nx (make-lazy-src swapped #f #f)))
                                         (hashtable-set! memo x nx)
                                         (image-meta-copy! x nx)
                                         (lazy-src-a-set! nx (walk (lazy-src-a x) (cons "lazy-arg" path)))
                                         (lazy-src-b-set! nx (walk (lazy-src-b x) (cons "lazy-arg" path)))
                                         nx)
                                       (begin
                                         (hashtable-set! memo x #t)
                                         (walk (lazy-src-a x) (cons "lazy-arg" path))
                                         (walk (lazy-src-b x) (cons "lazy-arg" path))
                                         #t))))
                             ;; a var-rooted multimethod or reify: code the
                             ;; restoring build already has, so it travels as the
                             ;; var's NAME through the same fn-ref external a
                             ;; named fn uses. Without this the walk descended
                             ;; into a multifn's dispatch tables and refused at a
                             ;; raw hashtable the user could do nothing about
                             ;; (jolt-2cny).
                             ;; A transient is thread-owned mutable state whose
                             ;; owning thread is gone by definition after a
                             ;; restore, and half of them could not travel anyway
                             ;; -- a transient vector wrote silently while a
                             ;; transient map refused on its backing hashtable.
                             ;; Refuse both, saying what to do (jolt-ji1h).
                             ((jolt-transient? x)
                              (cond
                                (stubbing?
                                 (let ((st (make-stub x path "a transient")))
                                   (hashtable-set! memo x st) st))
                                (rebuild?
                                 (jolt-throw
                                   (jolt-ex-info
                                     (string-append
                                       "image: cannot write a transient at "
                                       (image-path->string path)
                                       ": it belongs to the thread that made it, which"
                                       " a restore does not have. Call persistent! on it"
                                       " first.")
                                     empty-pmap)))
                                (else (hashtable-set! memo x #t)
                                      (report! x path (image-report-disposition mode)))))
                             ((and (not (procedure? x)) (proc-name-of x))
                              (hashtable-set! memo x x)
                              x)
                             ((mutex? x)
                              (cond (restore? (make-mutex))
                                    (rebuild? (make-image-sync 'mutex))
                                    (else #t)))
                             ((thread-condition? x)
                              (cond (restore? (make-condition))
                                    (rebuild? (make-image-sync 'condition))
                                    (else #t)))
                             ((and (record? x) (record-rtd x))
                              (walk-record x path))
                             (else (if rebuild? x #t)))))))))))
             (walk-pmap
              (lambda (x path)
                (if rebuild?
                    (let ((entries '()) (dirty #f) (rekey #f))
                      (pmap-fold-fwd x
                        (lambda (k v acc)
                          (let*-values (((wk ktouched) (walk-key k (cons "<key>" path))))
                            (let ((wv (walk v (cons (image-describe-obj k) path))))
                              (when ktouched (set! rekey #t))
                              (set! entries (cons (cons wk wv) entries))
                              (set! dirty (or dirty (not (eq? wk k)) (not (eq? wv v))))
                              acc)))
                        #f)
                      (if (hashtable-ref memo x #f)
                          (hashtable-ref memo x #f)
                          (if rekey
                              ;; a key's hash depends on a per-process fn id —
                              ;; substitute the entries record; restore rebuilds
                              (let ((r (make-image-rekey 'map
                                         (list->vector (reverse entries)))))
                                (hashtable-set! memo x r)
                                (image-meta-copy! x r)
                                r)
                          (if dirty
                              (let ((nx (apply jolt-hash-map
                                               (apply append
                                                      (map (lambda (e) (list (car e) (cdr e)))
                                                           (reverse entries))))))
                                (hashtable-set! memo x nx)
                                (image-meta-copy! x nx)
                                nx)
                              (begin (hashtable-set! memo x x) x)))))
                    (begin
                      (hashtable-set! memo x #t)
                      (pmap-fold-fwd x
                        (lambda (k v acc)
                          (walk k (cons "<key>" path))
                          (walk v (cons (image-describe-obj k) path))
                          acc)
                        #f)
                      #t))))
             (walk-pset
              (lambda (x path)
                (if rebuild?
                    ;; pair-wise, like the sub path: the lookup value can be an
                    ;; element merely jolt= to the key it is filed under
                    (let ((pairs '()) (dirty #f) (rekey #f))
                      (pset-fold-pairs x
                        (lambda (e v acc)
                          ;; a set's element IS its key — its whole subtree
                          ;; decides placement, so track both halves of the pair
                          (let*-values (((w etouched) (walk-key e (cons (image-describe-obj e) path))))
                            (let*-values (((wv vtouched)
                                           (if (eq? v e)
                                               (values w etouched)
                                               (walk-key v (cons (image-describe-obj v) path)))))
                              (when (or etouched vtouched) (set! rekey #t))
                              (set! pairs (cons (cons w wv) pairs))
                              (set! dirty (or dirty (not (eq? w e)) (not (eq? wv v))))
                              acc)))
                        #f)
                      (if (hashtable-ref memo x #f)
                          (hashtable-ref memo x #f)
                          (if rekey
                              (let ((r (make-image-rekey 'set
                                         (list->vector (reverse pairs)))))
                                (hashtable-set! memo x r)
                                (image-meta-copy! x r)
                                r)
                          (if dirty
                              (let ((nx (pset-from-pairs (reverse pairs))))
                                (hashtable-set! memo x nx)
                                (image-meta-copy! x nx)
                                nx)
                              (begin (hashtable-set! memo x x) x)))))
                    (begin
                      (hashtable-set! memo x #t)
                      ;; the split lookup value is its own object, with its own
                      ;; metadata for image-collect-meta to pick up
                      (pset-fold-pairs x (lambda (e v acc)
                                           (walk e (cons (image-describe-obj e) path))
                                           (unless (eq? v e) (walk v (cons (image-describe-obj v) path)))
                                           acc)
                                       #f)
                      #t))))
             (walk-sorted
              (lambda (x path)
                (if rebuild?
                    ;; write side: substitute to an image-sorted record. The
                    ;; wrapper is immutable data, so there are no cycles to
                    ;; pre-memoize; cmp-fn routes through the shared proc
                    ;; verdict, entries walk as plain data.
                    (let ((map? (htable-sorted-map? x)))
                      (if map?
                          (let ((pairs '()))
                            (let loop ((s (jolt-seq x)))
                              (unless (jolt-nil? s)
                                (let* ((e (jolt-first s))
                                       (k (jolt-nth e 0))
                                       (v (jolt-nth e 1))
                                       (wk (walk k (cons "<key>" path)))
                                       (wv (walk v (cons (image-describe-obj k) path))))
                                  (set! pairs (cons (cons wk wv) pairs)))
                                (loop (jolt-next s))))
                            (let ((r (make-image-sorted 'map
                                       (walk (jolt-ref-get x kw-cmp-fn) (cons "cmp-fn" path))
                                       (list->vector (reverse pairs)))))
                              (hashtable-set! memo x r)
                              (image-meta-copy! x r)
                              r))
                          (let ((items '()))
                            (let loop ((s (jolt-seq x)))
                              (unless (jolt-nil? s)
                                (set! items (cons (walk (jolt-first s)
                                                        (cons (image-describe-obj (jolt-first s)) path))
                                                  items))
                                (loop (jolt-next s))))
                            (let ((r (make-image-sorted 'set
                                       (walk (jolt-ref-get x kw-cmp-fn) (cons "cmp-fn" path))
                                       (list->vector (reverse items)))))
                              (hashtable-set! memo x r)
                              (image-meta-copy! x r)
                              r))))
                    (begin
                      (hashtable-set! memo x #t)
                      (walk (jolt-ref-get x kw-cmp-fn) (cons "cmp-fn" path))
                      (let loop ((s (jolt-seq x)))
                        (unless (jolt-nil? s)
                          (let ((e (jolt-first s)))
                            (if (htable-sorted-map? x)
                                (begin
                                  (walk (jolt-nth e 0) (cons "<key>" path))
                                  (walk (jolt-nth e 1)
                                        (cons (image-describe-obj (jolt-nth e 0)) path)))
                                (walk e (cons (image-describe-obj e) path))))
                          (loop (jolt-next s))))
                      #t))))
             (walk-pvec
              (lambda (x path)
                (let ((n (pvec-count x)))
                  (if rebuild?
                      (let ((items '()) (dirty #f))
                        (let loop ((i 0))
                          (if (fx<? i n)
                              (let* ((v (pvec-nth-d x i jolt-nil))
                                     (w (walk v (cons (number->string i) path))))
                                (set! items (cons w items))
                                (set! dirty (or dirty (not (eq? v w))))
                                (loop (fx+ i 1)))
                              (or (hashtable-ref memo x #f)
                                  (if dirty
                                      (let ((nx (apply jolt-vector (reverse items))))
                                        (hashtable-set! memo x nx)
                                        (image-meta-copy! x nx)
                                        nx)
                                      (begin (hashtable-set! memo x x) x))))))
                      (begin
                        (hashtable-set! memo x #t)
                        (let loop ((i 0))
                          (when (fx<? i n)
                            (walk (pvec-nth-d x i jolt-nil) (cons (number->string i) path))
                            (loop (fx+ i 1))))
                        #t)))))
             ;; root AND meta, on both paths. meta is a FIELD of the cell (rt.ss), so
             ;; fasl-write sees it and the walk has to reach it — same parity rule as
             ;; the atom below. The rebuilt cell takes the WALKED meta, so a stub or a
             ;; handled payload inside it is rebuilt like any other reachable value;
             ;; installing the original would have carried the source graph's objects
             ;; into the rebuilt one.
             (walk-var-cell
              (lambda (x path)
                (let* ((vp (string-append "#'" (var-cell-ns x) "/" (var-cell-name x)))
                       (mp (cons (string-append vp " meta") path))
                       (m (var-cell-meta x)))
                  (if rebuild?
                      ;; dyn-bound? is NOT carried over: it is a per-process
                      ;; observation ("someone bound this var here"), not part of
                      ;; the var's value, and a rebuilt cell has had no bindings.
                      ;; Copying it in would only cost the rebuilt var its fast
                      ;; read path, never break it.
                      (let ((nx (make-var-cell (var-cell-ns x) (var-cell-name x)
                                               jolt-nil (var-cell-defined? x)
                                               #f (var-cell-macro? x) #f
                                               (var-cell-dynamic? x))))
                        (hashtable-set! memo x nx)
                        (var-cell-root-set! nx (walk (var-cell-root x) (cons vp path)))
                        (var-cell-meta-set! nx (and m (walk m mp)))
                        nx)
                      (begin
                        (hashtable-set! memo x #t)
                        (walk (var-cell-root x) (cons vp path))
                        (when m (walk m mp))
                        #t)))))
             ;; cover val + watches + validator — everything fasl-write sees
             ;; (the scan/dump parity fix)
             (walk-atom
              (lambda (x path)
                (if rebuild?
                    (let ((nx (make-jolt-atom jolt-nil '() jolt-nil (make-mutex))))
                      (hashtable-set! memo x nx)
                      (image-meta-copy! x nx)
                      (jolt-atom-val-set! nx (walk (jolt-atom-val x) (cons "@" path)))
                      (jolt-atom-watches-set! nx
                        (map (lambda (w) (walk w (cons "@watch" path)))
                             (jolt-atom-watches x)))
                      (jolt-atom-validator-set! nx
                        (walk (jolt-atom-validator x) (cons "@validator" path)))
                      nx)
                    (begin
                      (hashtable-set! memo x #t)
                      (walk (jolt-atom-val x) (cons "@" path))
                      (for-each (lambda (w) (walk w (cons "@watch" path)))
                                (jolt-atom-watches x))
                      (walk (jolt-atom-validator x) (cons "@validator" path))
                      #t))))
             ;; cover val — the ref's only traveling state. Watches/validators
             ;; live in the weak iref side tables and do not travel (identical
             ;; to the raw-record days); the STM lock is global, nothing else
             ;; to carry. Write side substitutes the descriptor; memoize BEFORE
             ;; walking val so a self-referencing ref closes its cycle on the
             ;; descriptor.
             (walk-ref
              (lambda (x path)
                (if rebuild?
                    (let ((nx (make-image-ref jolt-nil)))
                      (hashtable-set! memo x nx)
                      (image-meta-copy! x nx)
                      (image-ref-val-set! nx (walk (jolt-ref-val x) (cons "ref" path)))
                      nx)
                    (begin
                      (hashtable-set! memo x #t)
                      (walk (jolt-ref-val x) (cons "ref" path))
                      #t))))
             ;; read side: re-mint a live ref from a descriptor's (or a legacy
             ;; raw record's) val — the caller passes the val read the right
             ;; way for x's representation. Memoize before walking val, same
             ;; cycle discipline as walk-atom.
             (walk-ref-restore
              (lambda (v x path)
                (let ((nx (make-jolt-ref jolt-nil)))
                  (hashtable-set! memo x nx)
                  (image-meta-copy! x nx)
                  (jolt-ref-val-set! nx (walk v (cons "ref" path)))
                  nx)))
             (walk-pair
              (lambda (x path)
                (if rebuild?
                    (let* ((a (walk (car x) (cons "car" path)))
                           (d (walk (cdr x) (cons "cdr" path))))
                      (or (hashtable-ref memo x #f)
                          (if (and (eq? a (car x)) (eq? d (cdr x)))
                              (begin (hashtable-set! memo x x) x)
                              (let ((nx (cons a d)))
                                (hashtable-set! memo x nx)
                                nx))))
                    (begin
                      (hashtable-set! memo x #t)
                      (walk (car x) (cons "car" path))
                      (walk (cdr x) (cons "cdr" path))
                      #t))))
             (walk-vector
              (lambda (x path)
                (let ((n (vector-length x)))
                  (if rebuild?
                      (let ((out (make-vector n)) (dirty #f))
                        (let loop ((i 0))
                          (if (fx<? i n)
                              (let* ((v (vector-ref x i))
                                     (w (walk v (cons (number->string i) path))))
                                (vector-set! out i w)
                                (set! dirty (or dirty (not (eq? v w))))
                                (loop (fx+ i 1)))
                              (or (hashtable-ref memo x #f)
                                  (if dirty
                                      (begin
                                        (hashtable-set! memo x out)
                                        (image-meta-copy! x out)
                                        out)
                                      (begin (hashtable-set! memo x x) x))))))
                      (begin
                        (hashtable-set! memo x #t)
                        (let loop ((i 0))
                          (when (fx<? i n)
                            (walk (vector-ref x i) (cons (number->string i) path))
                            (loop (fx+ i 1))))
                        #t)))))
             (walk-hashtable
              (lambda (x path)
                (let-values (((ks vs) (hashtable-entries x)))
                  (if rebuild?
                      ;; an eq/eqv hashtable has NO hash function to read back
                      ;; (hashtable-hash-function answers #f), so it has to be
                      ;; re-made through its own constructor
                      (let ((nx (let ((eqv (hashtable-equivalence-function x)))
                                  (cond ((eq? eqv eq?) (make-eq-hashtable))
                                        ((eq? eqv eqv?) (make-eqv-hashtable))
                                        (else (make-hashtable (hashtable-hash-function x) eqv))))))
                        (hashtable-set! memo x nx)
                        (image-meta-copy! x nx)
                        (let loop ((i 0))
                          (when (fx<? i (vector-length ks))
                            (hashtable-set! nx
                              (walk (vector-ref ks i) (cons "<key>" path))
                              (walk (vector-ref vs i)
                                    (cons (image-describe-obj (vector-ref ks i)) path)))
                            (loop (fx+ i 1))))
                        nx)
                      (begin
                        (hashtable-set! memo x #t)
                        (let loop ((i 0))
                          (when (fx<? i (vector-length ks))
                            (walk (vector-ref ks i) (cons "<key>" path))
                            (walk (vector-ref vs i)
                                  (cons (image-describe-obj (vector-ref ks i)) path))
                            (loop (fx+ i 1))))
                        #t)))))
             ;; generic record: walk declared fields by name, inherited ones
             ;; included (image-record-fields). Covers user defrecords, lazy seqs,
             ;; refs, image records, everything not special-cased above. The
             ;; rebuild applies record-constructor to the SAME list, which is why
             ;; the parent's fields have to be in it and in front.
             ;; a pre-slot jrec (image-legacy-jrec?, format <= 4) rebuilds into
             ;; the CURRENT family: fields read via its own rtd's accessors come
             ;; back (desc ext fields...) root-first, the descriptor type is
             ;; unchanged, and the ctor protocols start hasheq unset. The spill
             ;; type is told apart by its rtd name — field counts collide with a
             ;; one-field child.
             (walk-legacy-jrec
              (lambda (x path)
                (or (hashtable-ref memo x #f)
                    (let* ((fs (image-record-fields (record-rtd x)))
                           (n (vector-length fs))
                           (vals (let loop ((i 0) (acc '()))
                                   (if (fx=? i n) (reverse acc)
                                       (let ((f (vector-ref fs i)))
                                         (loop (fx+ i 1)
                                               (cons (walk (image-record-field-ref f x)
                                                           (cons (symbol->string (car f)) path))
                                                     acc)))))))
                      (let* ((desc (car vals)) (ext (cadr vals)) (fvals (cddr vals))
                             (nx (if (eq? (record-type-name (record-rtd x)) 'jrec*)
                                     (make-jrec desc (car fvals) ext)
                                     (apply (vector-ref jrec-ctor-vec (length fvals))
                                            desc ext 0 fvals))))
                        (hashtable-set! memo x nx)
                        (image-meta-copy! x nx)
                        nx)))))
             (walk-legacy-pmap
              (lambda (x path)
                (or (hashtable-ref memo x #f)
                    (let ((nx (if (pset? x)
                                  (walk-pset (make-pset (legacy-pmap->pmap (pset-m x))) path)
                                  (walk-pmap (legacy-pmap->pmap x) path))))
                      (hashtable-set! memo x nx)
                      (image-meta-copy! x nx)
                      nx))))
             (walk-record
              (lambda (x path)
                (let* ((rtd (record-rtd x))
                       (fs (image-record-fields rtd))
                       (n (vector-length fs)))
                  (if rebuild?
                      (let ((vals (make-vector n)) (dirty #f))
                        (let loop ((i 0))
                          (if (fx<? i n)
                              (let* ((f (vector-ref fs i))
                                     (v (image-record-field-ref f x))
                                     (w (if (image-opaque-field? v) v
                                            (walk v (cons (symbol->string (car f)) path)))))
                                (vector-set! vals i w)
                                (set! dirty (or dirty (not (eq? v w))))
                                (loop (fx+ i 1)))
                              (or (hashtable-ref memo x #f)
                                  (if dirty
                                      (let ((nx (apply (record-constructor rtd)
                                                       (vector->list vals))))
                                        ;; rtd's raw ctor copies every slot, the
                                        ;; hasheq cache included — start the copy
                                        ;; unset whatever the pass order was
                                        (when (jrec? nx) (jrec-hasheq-set! nx 0))
                                        (hashtable-set! memo x nx)
                                        (image-meta-copy! x nx)
                                        nx)
                                      (begin (hashtable-set! memo x x) x))))))
                      (begin
                        (hashtable-set! memo x #t)
                        (let loop ((i 0))
                          (when (fx<? i n)
                            (let* ((f (vector-ref fs i))
                                   (v (image-record-field-ref f x)))
                              (unless (image-opaque-field? v)
                                (walk v (cons (symbol->string (car f)) path))))
                            (loop (fx+ i 1))))
                        #t)))))
             ;; R3 read side: a stored fn source record rebuilds into a live
             ;; closure. Bottom-up: the record's free values transform first (a
             ;; captured value may itself be or contain a fnsrc record), then the
             ;; wrapper (fn* [free-names…] form) is compiled+eval'd in the
             ;; record's ns and APPLIED to the transformed values — the wrapper
             ;; params SHADOW the outer-scope names the body references. The
             ;; result is memoized keyed by the RECORD, so a record shared by two
             ;; slots evals once and both slots get the SAME closure; a cycle
             ;; through a mutable cell works because the cell is memoized before
             ;; its contents walk (the R2 order), so the closure's free value IS
             ;; the restored cell.
             (walk-fnsrc-restore
              (lambda (x path)
                (let* ((frees (image-fnsrc-free-names x))
                       (fvs (image-fnsrc-free-values x))
                       (n (vector-length fvs)))
                  (unless (fx=? n (pvec-count frees))
                    (jolt-throw (jolt-ex-info
                                  (string-append "image: malformed fn source record "
                                                 (image-fnsrc-name x) ": " (number->string n)
                                                 " free values for " (number->string (pvec-count frees))
                                                 " free names")
                                  empty-pmap)))
                  (let ((tfvs (map (lambda (v)
                                     (walk v (cons (string-append "free:" (image-fnsrc-name x)) path)))
                                   (vector->list fvs))))
                    (let ((cl (image-eval-fnsrc x tfvs)))
                      (hashtable-set! memo x cl)
                      (image-meta-copy! x cl)
                      cl)))))
             ;; R4 read side: a stored sorted coll rebuilds through the public
             ;; constructors. cmp-fn is walked first (jolt-nil -> natural ctor,
             ;; live fn -> fn-ref identity, stored fnsrc -> compiled closure);
             ;; the entries were written in sorted order, so folding them in via
             ;; jolt-assoc / jolt-conj (which dispatch sorted) is ordered input.
             (walk-sorted-restore
              (lambda (x path)
                (let* ((map? (eq? (image-sorted-kind x) 'map))
                       (cmp-fn (walk (image-sorted-cmp-fn x) (cons "cmp-fn" path)))
                       (entries (image-sorted-entries x))
                       (n (vector-length entries)))
                  (let ((coll (if (jolt-nil? cmp-fn)
                                  (jolt-invoke (var-deref "clojure.core"
                                                           (if map? "sorted-map" "sorted-set")))
                                  (jolt-invoke (var-deref "clojure.core"
                                                           (if map? "sorted-map-by" "sorted-set-by"))
                                               cmp-fn))))
                    (let loop ((i 0))
                      (when (fx<? i n)
                        (let ((e (walk (vector-ref entries i) (cons "entry" path))))
                          (if map?
                              (set! coll (jolt-assoc coll (car e) (cdr e)))
                              (set! coll (jolt-conj coll e)))
                          (loop (fx+ i 1)))))
                    (hashtable-set! memo x coll)
                    (image-meta-copy! x coll)
                    coll))))
             ;; read side of image-rekey: rebuild the container in THIS process
             ;; so placement is computed from the ids its lookups will use.
             ;; Entries walk first (an fnsrc key becomes the live closure before
             ;; it is hashed); the map rebuild mirrors walk-pmap's dirty rebuild
             ;; (jolt-hash-map), the set one walk-pset's (pset-from-pairs).
             (walk-rekey-restore
              (lambda (x path)
                (let* ((entries (image-rekey-entries x))
                       (n (vector-length entries))
                       (pairs (let loop ((i 0) (acc '()))
                                (if (fx>=? i n)
                                    (reverse acc)
                                    (let* ((e (vector-ref entries i))
                                           (wk (walk (car e) (cons "<key>" path)))
                                           (wv (if (eq? (cdr e) (car e))
                                                   wk
                                                   (walk (cdr e) (cons "<val>" path)))))
                                      (loop (fx+ i 1) (cons (cons wk wv) acc)))))))
                  (let ((coll (if (eq? (image-rekey-kind x) 'map)
                                  (apply jolt-hash-map
                                         (apply append
                                                (map (lambda (e) (list (car e) (cdr e))) pairs)))
                                  (pset-from-pairs pairs))))
                    (hashtable-set! memo x coll)
                    (image-meta-copy! x coll)
                    coll))))
             (walk-handled-restore
              (lambda (x path)
                (let ((tp (walk (image-handled-payload x) (cons "payload" path))))
                  (let ((r (image-restore-handler tp)))
                    (hashtable-set! memo x r)
                    r)))))
      ;; the walk must finish before the accumulator is read — argument
      ;; evaluation order is unspecified, so sequence explicitly
      (let ((g (walk root '())))
        (values g (reverse stub-acc))))))


;; --- scan ----------------------------------------------------------------------
;; Dry run: every object that cannot be encoded, with the route to it. Returns a
;; jolt vector of maps so callers can render or assert on it.
(define (jolt-image-scan v)
  ;; stub-aware by default: an object a stub-mode dump would stub reports
  ;; :would-stub, so dumpable? (which counts only :unwritable) stays true for
  ;; graphs a stub-mode dump can write.
  (jolt-image-scan-mode v 'report-stub))
(define (jolt-image-scan-mode v mode)
  (let ((bad '()))
    (image-graph-process v mode
      (lambda (x path disp)
        (set! bad (cons (list (image-path->string path)
                              (image-describe-obj x)
                              disp)
                        bad))))
    (apply jolt-vector
           (map (lambda (p)
                  (jolt-hash-map (jolt-keyword "path") (car p)
                                 (jolt-keyword "object") (cadr p)
                                 (jolt-keyword "disposition") (caddr p)))
                (reverse bad)))))

;; --- header --------------------------------------------------------------------
(define (image-header)
  (vector 'jolt-image
          jolt-image-format-version
          (jolt-image-runtime-version)
          (sa-host-tag)))

(define (image-check-header! h path)
  (unless (and (vector? h) (fx=? (vector-length h) 4) (eq? (vector-ref h 0) 'jolt-image))
    (jolt-throw (jolt-ex-info (string-append "image: " path " is not a jolt image") empty-pmap)))
  (unless (member (vector-ref h 1) jolt-image-read-versions)
    (jolt-throw (jolt-ex-info
                  (string-append "image: " path " has format version "
                                 (jolt-str-one (vector-ref h 1)) ", this build reads versions 2 to 7")
                  empty-pmap)))
  ;; The fasl version moves with Chez, and a mismatch otherwise surfaces as an
  ;; opaque fasl-read error, so name it here instead.
  (unless (equal? (vector-ref h 2) (jolt-image-runtime-version))
    (jolt-throw (jolt-ex-info
                  (string-append "image: " path " was written by runtime "
                                 (jolt-str-one (vector-ref h 2)) ", this is "
                                 (jolt-image-runtime-version))
                  empty-pmap)))
  #t)

;; Runtime identity an image is pinned to. The fasl format moves with the Chez
;; release, so the Chez version is the honest key; the architecture deliberately
;; is NOT part of it.
(define (jolt-image-runtime-version)
  (string-append "chez-" (number->string (scheme-version-number*))))

(define (scheme-version-number*)
  ;; (scheme-version) is like "Chez Scheme Version 10.4.1"; reduce to an integer
  ;; so the check is a cheap equal? and prints readably.
  (let* ((s (scheme-version))
         (n (string-length s)))
    (let loop ((i 0) (acc 0) (seen #f))
      (if (fx>=? i n)
          acc
          (let ((c (string-ref s i)))
            (cond ((char-numeric? c) (loop (fx+ i 1) (+ (* acc 10) (- (char->integer c) 48)) #t))
                  ((and seen (char=? c #\.)) (loop (fx+ i 1) acc #t))
                  (seen acc)
                  (else (loop (fx+ i 1) acc seen))))))))

;; --- write / read --------------------------------------------------------------
;; Metadata lives in a weak side table keyed by object identity (natives-meta.ss),
;; so it cannot ride on the object itself. It rides in the SAME fasl stream
;; instead: fasl preserves sharing within one stream, so the objects in this
;; alist come back eq? to the ones in the graph and the meta can be re-attached.
(define (image-collect-meta v)
  (let ((acc '()))
    (image-walk v (lambda (x path)
                    ;; Same write-side walk, second job: drop cached hasheqs so
                    ;; no cache lands in the fasl. A content-derived hash would
                    ;; be stable across processes, but a collection holding a
                    ;; VAR-REFERENCED fn travels raw (only anon closures are
                    ;; rebuilt into fnsrc records), and procedure hasheq is
                    ;; per-process identity (hasheq.ss) — a restored coll would
                    ;; carry a hash its own contents no longer produce,
                    ;; corrupting the = fast-reject and any post-restore keying.
                    ;; Zeroing the LIVE object (v* shares untouched subgraphs)
                    ;; is harmless: it is a cache, and the next hash refills it.
                    ;; The JVM marks these fields transient for serialization.
                    (cond ((pvec? x) (pvec-hasheq-set! x 0))
                          ((pmap? x) (pmap-hasheq-set! x 0))
                          ((pset? x) (pset-hasheq-set! x 0))
                          ;; a jrec's slot may hold an identity hash (plain
                          ;; deftype) — per-process by construction — and even a
                          ;; record's structural hash can embed one through a fn
                          ;; or deftype field. Same rule as the collections.
                          ((jrec? x) (jrec-hasheq-set! x 0)))
                    (unless (var-cell? x)
                      (let ((m (call/cc (lambda (k)
                                 (with-exception-handler (lambda (e) (k jolt-nil))
                                   (lambda () (jolt-meta x)))))))
                        (unless (jolt-nil? m) (set! acc (cons (cons x m) acc)))))))
    acc))

(define (image-reattach-meta! pairs)
  (for-each (lambda (p) (meta-table-set! (car p) (cdr p))) pairs))

(define jolt-image-write!
  (case-lambda
    ((path v) (jolt-image-write!* path v jolt-nil))
    ((path v opts) (jolt-image-write!* path v opts))))
(define (jolt-image-write!* path v opts)
  ;; R2: substitute first — anon closures become image-fnsrc records and handler
  ;; resources become image-handled payloads, so what fasl sees is exactly what
  ;; the transformer approved. OPTS is jolt-nil or a jolt map; {:unwritable :stub}
  ;; substitutes image-stub records for otherwise-refused objects (ports,
  ;; threads, non-eq hashtables, unregistered closures) instead of failing the
  ;; dump, and the return value reports them as {:stubbed [info-maps]}.
  (let* ((stub-mode? (and (not (jolt-nil? opts))
                          (eq? (jolt-get opts (jolt-keyword "unwritable") jolt-nil)
                               (jolt-keyword "stub"))))
         (stubs #f)
         (v* (let-values (((g s)
                           (image-graph-process v
                                                (if stub-mode? 'rebuild-stub 'rebuild)
                                                #f)))
               (set! stubs s)
               g))
         ;; externals are collected in encounter order; the eq table is only for
         ;; membership, since a keyword-dense graph makes a list scan quadratic.
         (externals '()) (ext-seen (make-eq-hashtable)) (ext-tail #f))
    ;; Body first: the externals list is discovered during fasl-write, so it
    ;; cannot be written ahead of the body.
    (let ((body (call-with-bytevector-output-port
                  (lambda (p)
                    (sa-fasl-write (vector v* (image-collect-meta v*)) p
                      (lambda (x)
                        (and (image-external? x)
                             (begin
                               (unless (hashtable-ref ext-seen x #f)
                                 (hashtable-set! ext-seen x #t)
                                 (let ((cell (list x)))
                                   (if ext-tail
                                       (begin (set-cdr! ext-tail cell) (set! ext-tail cell))
                                       (begin (set! externals cell) (set! ext-tail cell)))))
                               #t))))))))
      (let ((descs (map (lambda (x)
                          (or (image-encode-external x)
                              ;; Re-walk for the path only on the failure branch,
                              ;; so the happy path pays nothing for it.
                              (let ((where "<unknown>"))
                                (image-walk v* (lambda (o p)
                                                 (when (eq? o x) (set! where (image-path->string p)))))
                                (jolt-throw (jolt-ex-info
                                              (string-append "image: cannot write "
                                                             (image-describe-obj x)
                                                             " at " where)
                                              empty-pmap)))))
                        externals)))
        ;; Descriptors are written WITHOUT an externals-pred, so a handler that
        ;; returns something non-data would fail here with a raw Chez error.
        ;; Check before opening the file, so a rejected dump never leaves a
        ;; half-written image behind.
        (let ((desc-bytes
                (call/cc (lambda (k)
                  (with-exception-handler
                    (lambda (e)
                      (k (jolt-throw (jolt-ex-info
                                       "image: a resource handler returned a value that is not plain data"
                                       empty-pmap))))
                    (lambda () (call-with-bytevector-output-port
                                 (lambda (p) (sa-fasl-write descs p)))))))))
          (let ((port (open-file-output-port path (file-options no-fail))))
            (sa-fasl-write (image-header) port)
            (put-bytevector port desc-bytes)
            (put-bytevector port body)
            (close-port port)))))
    (if stub-mode?
        (jolt-hash-map (jolt-keyword "stubbed")
                       (apply jolt-vector (map image-stub-info stubs)))
        jolt-nil)))

(define (jolt-image-read path)
  (unless (file-exists? path)
    (jolt-throw (jolt-ex-info (string-append "image: no such file: " path) empty-pmap)))
  (let ((port (open-file-input-port path)))
    (let* ((h (sa-fasl-read port))
           (_ (image-check-header! h path))
           (descs (sa-fasl-read port))
           (exts (list->vector (map image-decode-external descs)))
           (b (sa-fasl-read port 'load exts)))
      (close-port port)
      (unless (and (vector? b) (fx=? (vector-length b) 2))
        (jolt-throw (jolt-ex-info (string-append "image: malformed body in " path) empty-pmap)))
      (image-reattach-meta! (vector-ref b 1))
      ;; R3: rebuild what the write side substituted — fn source records become
      ;; live closures, handler payloads go back through their restore fns.
      ;; Runs after meta re-attachment so container rebuilds carry meta forward.
      (let-values (((g stubs) (image-graph-process (vector-ref b 0) 'restore #f)))
        g))))

;; --- whole-world image ----------------------------------------------------------
;; The Smalltalk/Common Lisp shape: don't ask which variable to save, save the
;; world. Walk the var table and write every var's root, so restoring brings the
;; program's whole state back rather than one value the caller remembered to name.
;;
;; What makes this affordable on Chez is that CODE does not have to travel. A var
;; whose root is a procedure is skipped outright: the restoring process is the
;; same build, so it already has that function: `defn` bodies, protocol impls and
;; multimethod tables are all present before the image is read. Only DATA moves.
;; That is also why an image is pinned to its build — see the header check.
;;
;; Namespaces owned by the language are skipped by default. clojure.core holds
;; mutable vars (*ns*, *warn-on-reflection*, printer state) that belong to the
;; process being restored INTO, not to the image; carrying them over would make a
;; restore quietly reconfigure the reader and printer.
;; `user` is deliberately NOT skipped: at a REPL it is where the work lives, and
;; an image that quietly dropped it would lose exactly what the user typed.
(define image-system-ns-prefixes '("clojure." "jolt."))

(define (image-system-ns? ns)
  (or (string=? ns "clojure.core")
      (let loop ((ps image-system-ns-prefixes))
        (and (pair? ps)
             (or (and (>= (string-length ns) (string-length (car ps)))
                      (string=? (substring ns 0 (string-length (car ps))) (car ps)))
                 (loop (cdr ps)))))))

;; Hooks, the *save-hooks* / *init-hooks* pair. An application quiesces in
;; before-dump (stop pools, park threads) and rebuilds whatever it could not
;; carry in after-restore (reopen resources, re-derive computed cells).
(define image-before-dump-hooks '())
(define image-after-restore-hooks '())
(define (jolt-image-add-before-dump-hook! f)
  (set! image-before-dump-hooks (append image-before-dump-hooks (list f))) jolt-nil)
(define (jolt-image-add-after-restore-hook! f)
  (set! image-after-restore-hooks (append image-after-restore-hooks (list f))) jolt-nil)
(define (image-run-hooks! hs) (for-each (lambda (f) (jolt-invoke f)) hs) jolt-nil)

;; ns-list is a jolt seq of namespace-name strings, or nil for "every namespace
;; that isn't the language's own".
(define (image-world-vars ns-list)
  (let ((want (if (jolt-nil? ns-list)
                  #f
                  (let loop ((s (jolt-seq ns-list)) (acc '()))
                    (if (jolt-nil? s) acc
                        (loop (jolt-next s) (cons (jolt-first s) acc))))))
        (out '()))
    (let* ((kv (var-table-entries))          ; snapshot under var-table-mu (rt.ss)
           (ks (car kv))
           (vs (cdr kv)))
      (let loop ((i 0))
        (when (fx<? i (vector-length ks))
          (let* ((cell (vector-ref vs i))
                 (ns (var-cell-ns cell))
                 (nm (var-cell-name cell))
                 (root (var-cell-root cell)))
            (when (and (if want (member ns want) (not (image-system-ns? ns)))
                       ;; code is already in the restoring build; only data moves
                       (not (procedure? root))
                       (not (jolt-var-unbound? root)))
              (let ((h (image-handler-for root)))
                (set! out (cons (cons (string-append ns "/" nm)
                                      (if h
                                          (make-image-handled (jolt-invoke (cadr h) root))
                                          root))
                                out)))))
          (loop (fx+ i 1)))))
    out))

;; The world capture is best-effort BY DEFAULT: an unhandled resource stubs
;; instead of failing the dump, and the return value reports what was stubbed.
;; {:unwritable :fail} opts back into the strict contract dump! has.
(define jolt-image-dump-world!
  (case-lambda
    ((path ns-list) (jolt-image-dump-world! path ns-list jolt-nil))
    ((path ns-list opts)
     (image-run-hooks! image-before-dump-hooks)
     (let ((strict? (and (not (jolt-nil? opts))
                         (eq? (jolt-get opts (jolt-keyword "unwritable") jolt-nil)
                              (jolt-keyword "fail")))))
       (jolt-image-write! path (vector 'jolt-world (image-world-vars ns-list))
                          (if strict?
                              jolt-nil
                              (jolt-hash-map (jolt-keyword "unwritable")
                                             (jolt-keyword "stub"))))))))

(define (jolt-image-scan-world ns-list)
  (jolt-image-scan (vector 'jolt-world (image-world-vars ns-list))))

(define (jolt-image-restore-world! path)
  (hashtable-clear! image-restore-stub-tbl)
  (let ((w (jolt-image-read path)))
    (unless (and (vector? w) (fx=? (vector-length w) 2) (eq? (vector-ref w 0) 'jolt-world))
      (jolt-throw (jolt-ex-info
                    (string-append "image: " path
                                   " is a value image, not a world image — read it with read-image")
                    empty-pmap)))
    (let ((n 0))
      (for-each
        (lambda (p)
          (let* ((k (car p))
                 (slash (let scan ((i 0))
                          (cond ((fx>=? i (string-length k)) #f)
                                ((char=? (string-ref k i) #\/) i)
                                (else (scan (fx+ i 1))))))
                 (ns (substring k 0 slash))
                 (nm (substring k (fx+ slash 1) (string-length k))))
            (let ((cell (jolt-var ns nm))
                  (v (cdr p)))
              ;; handled payloads and fn source records were already rebuilt by
              ;; the read transform; the root binds as-is. Unresolved stubs left
              ;; in the root are recorded with their owning var so
              ;; jolt.image/stubs can list them and resolve-stub! can reach them.
              (image-walk v (lambda (o pth)
                              (when (image-stub? o)
                                (hashtable-set! image-restore-stub-tbl
                                                (image-stub-id o) (cons o k)))))
              (var-cell-root-set! cell v)
              (var-cell-defined?-set! cell #t)
              (set! n (fx+ n 1)))))
        (vector-ref w 1))
      (image-run-hooks! image-after-restore-hooks)
      n)))

;; --- unresolved stubs after a world restore -------------------------------------
;; id -> (stub . "ns/name"), reset per restore-world! call. Value images cannot
;; be reached after the fact (the caller holds the graph), so resolvers must be
;; registered before read-image for those; this table is the WORLD workflow.
(define image-restore-stub-tbl (make-eqv-hashtable))

(define (jolt-image-stubs)
  (let-values (((ks vs) (hashtable-entries image-restore-stub-tbl)))
    (let loop ((i 0) (acc '()))
      (if (fx>=? i (vector-length ks))
          (apply jolt-vector
                 (map cdr (sort (lambda (a b) (< (car a) (car b))) acc)))
          (let* ((e (vector-ref vs i))
                 (info (jolt-assoc (image-stub-info (car e))
                                   (jolt-keyword "var") (cdr e))))
            (loop (fx+ i 1) (cons (cons (vector-ref ks i) info) acc)))))))

;; Replace STUB (by identity) with VALUE everywhere under ROOT. Mutable cells
;; patch in place; immutable containers rebuild bottom-up; memo preserves
;; sharing. Returns (values new-root replaced-count).
(define (image-replace-stub root stub value)
  (let ((memo (make-eq-hashtable)) (count 0))
    (define (sub x)
      (cond
        ((eq? x stub) (set! count (fx+ count 1)) value)
        ((or (null? x) (boolean? x) (number? x) (char? x)
             (symbol? x) (string? x) (bytevector? x))
         x)
        ((hashtable-ref memo x #f) => (lambda (m) m))
        ((pmap? x)
         (let ((entries '()) (dirty #f))
           (pmap-fold-fwd x (lambda (k v acc)
                              (let ((sk (sub k)) (sv (sub v)))
                                (set! entries (cons (cons sk sv) entries))
                                (set! dirty (or dirty (not (eq? sk k)) (not (eq? sv v))))
                                acc))
                          #f)
           (let ((nx (if dirty
                         (apply jolt-hash-map
                                (apply append (map (lambda (e) (list (car e) (cdr e)))
                                                   (reverse entries))))
                         x)))
             (hashtable-set! memo x nx)
             (when dirty (image-meta-copy! x nx))
             nx)))
        ((pvec? x)
         (let* ((cnt (pvec-count x)) (dirty #f)
                (items (let loop ((i 0) (acc '()))
                         (if (fx>=? i cnt)
                             (reverse acc)
                             (let* ((v (pvec-nth-d x i jolt-nil)) (sv (sub v)))
                               (unless (eq? sv v) (set! dirty #t))
                               (loop (fx+ i 1) (cons sv acc)))))))
           (let ((nx (if dirty (apply jolt-vector items) x)))
             (hashtable-set! memo x nx)
             (when dirty (image-meta-copy! x nx))
             nx)))
        ;; pair-wise: a set's lookup value can be an element merely jolt= to its key
        ;; (pset-fold-pairs), and rebuilding element-by-element would drop it
        ((pset? x)
         (let ((dirty #f)
               (pairs (reverse (pset-fold-pairs x (lambda (e v acc)
                                                    (let ((se (sub e)) (sv (sub v)))
                                                      (unless (and (eq? se e) (eq? sv v)) (set! dirty #t))
                                                      (cons (cons se sv) acc)))
                                                '()))))
           (let ((nx (if dirty (pset-from-pairs pairs) x)))
             (hashtable-set! memo x nx)
             (when dirty (image-meta-copy! x nx))
             nx)))
        ((jolt-atom? x)
         (hashtable-set! memo x x)
         (jolt-atom-val-set! x (sub (jolt-atom-val x)))
         x)
        ((jolt-ref? x)
         (hashtable-set! memo x x)
         (jolt-ref-val-set! x (sub (jolt-ref-val x)))
         x)
        ((pair? x)
         (hashtable-set! memo x x)
         (let ((a (sub (car x))) (d (sub (cdr x))))
           (if (and (eq? a (car x)) (eq? d (cdr x)))
               x
               (let ((nx (cons a d))) (hashtable-set! memo x nx) nx))))
        ((vector? x)
         (hashtable-set! memo x x)
         (let loop ((i 0))
           (when (fx<? i (vector-length x))
             (vector-set! x i (sub (vector-ref x i)))
             (loop (fx+ i 1))))
         x)
        ((and (hashtable? x) (hashtable-mutable? x))
         (hashtable-set! memo x x)
         (let-values (((ks vs) (hashtable-entries x)))
           (let loop ((i 0))
             (when (fx<? i (vector-length ks))
               (hashtable-set! x (vector-ref ks i) (sub (vector-ref vs i)))
               (loop (fx+ i 1)))))
         x)
        (else (hashtable-set! memo x x) x)))
    (let ((r (sub root)))
      (values r count))))

(define (jolt-image-resolve-stub! id value)
  (let ((e (hashtable-ref image-restore-stub-tbl id #f)))
    (unless e
      (jolt-throw (jolt-ex-info
                    (string-append "image: no unresolved stub #"
                                   (jolt-str-one id) " from the last world restore")
                    empty-pmap)))
    (let* ((k (cdr e))
           (slash (let scan ((i 0))
                    (cond ((fx>=? i (string-length k)) #f)
                          ((char=? (string-ref k i) #\/) i)
                          (else (scan (fx+ i 1))))))
           (cell (jolt-var (substring k 0 slash)
                           (substring k (fx+ slash 1) (string-length k)))))
      (let-values (((nr cnt) (image-replace-stub (var-cell-root cell) (car e) value)))
        (var-cell-root-set! cell nr)
        (hashtable-delete! image-restore-stub-tbl id)
        cnt))))

(def-var! "jolt.host" "image-dump-world!" jolt-image-dump-world!)
(def-var! "jolt.host" "image-restore-world!" jolt-image-restore-world!)
(def-var! "jolt.host" "image-scan-world" jolt-image-scan-world)
(def-var! "jolt.host" "image-add-before-dump-hook!" jolt-image-add-before-dump-hook!)
(def-var! "jolt.host" "image-add-after-restore-hook!" jolt-image-add-after-restore-hook!)
(def-var! "jolt.host" "image-write!" jolt-image-write!)
(def-var! "jolt.host" "image-read" jolt-image-read)
(def-var! "jolt.host" "image-scan" jolt-image-scan)
(def-var! "jolt.host" "image-register-stub-describer!" jolt-image-register-stub-describer!)
(def-var! "jolt.host" "image-register-stub-resolver!" jolt-image-register-stub-resolver!)
(def-var! "jolt.host" "image-stubs" jolt-image-stubs)
(def-var! "jolt.host" "image-resolve-stub!" jolt-image-resolve-stub!)
(def-var! "jolt.host" "image-register-handler!" jolt-image-register-handler!)
(def-var! "jolt.host" "image-runtime-version" jolt-image-runtime-version)

;; --- stub presentation -----------------------------------------------------------
;; An unresolved stub prints as #image/stub{...} and reports the class
;; jolt.image.Stub, so an error from using one names what it is and where to
;; look (jolt.image/stubs lists it; resolve-stub! attaches a live value).
(define (image-stub-render s)
  (string-append "#image/stub{:id " (number->string (image-stub-id s))
                 " :kind " (jolt-pr-readable (image-stub-kind s))
                 " :path " (jolt-pr-readable (image-stub-path s)) "}"))
(register-pr-arm! image-stub? image-stub-render)
(register-class-arm! image-stub? (lambda (s) "jolt.image.Stub"))
