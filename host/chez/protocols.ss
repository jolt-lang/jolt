;; protocols.ss — protocol identity, registry, and resolution: the "<ns>/<Name>"
;; dispatch key, type-registry + jolt-proto-epoch, method lookup
;; (find-protocol-method / find-method-any-protocol), value-host-tags (the host
;; type-tag candidates a non-record value dispatches through), make-deftype-ctor
;; / make-protocol, register-method + the extend plumbing, protocol-resolve with
;; the dispatchN entry points, the per-site PIC, devirt-resolve, and the
;; contagion clone registry.
;;
;; Loaded after records-coll.ss; uses the jrec layout + rec-tbl-mu from
;; records.ss. reified-methods / record-method-dispatch are free references into
;; records-dispatch.ss, resolved at call time.

;; ---- protocol identity ------------------------------------------------------
;; A protocol's dispatch key is "<defining-ns>/<Name>". Two protocols named alike
;; in different namespaces are distinct interfaces on the JVM, so they must not
;; share a dispatch table — keying by the bare name let the later extend silently
;; replace the earlier one. defprotocol bakes the key into the protocol value and
;; into its method shims; the type definers read it back off the resolved var, so
;; every side of a protocol's identity agrees on one string.
;;
;; A key naming a HOST interface (Object, java.util.Map, an :import-ed
;; clojure.lang.ILookup) has no defining namespace and stays bare: that is the
;; spelling value-host-tags reports.
(define proto-kw-jtype (keyword #f "jolt/type"))
(define proto-kw-protocol (keyword #f "jolt/protocol"))
(define proto-kw-name (keyword #f "name"))
(define (jolt-protocol-value? v)
  (and (pmap? v) (eq? (jolt-get v proto-kw-jtype jolt-nil) proto-kw-protocol)))
(define (protocol-value-key v)
  (and (jolt-protocol-value? v)
       (let ((n (jolt-get v proto-kw-name jolt-nil)))
         (cond ((symbol-t? n) (symbol-t-name n))
               ((string? n) n)
               (else #f)))))
(define (proto-str-index s ch)
  (let ((n (string-length s)))
    (let loop ((i 0)) (cond ((fx=? i n) #f) ((char=? (string-ref s i) ch) i) (else (loop (fx+ i 1)))))))
;; The JVM interface a protocol key names: "ns/Name" -> "ns.Name" (dashes munged,
;; as the JVM spells a namespace). A host interface name is already canonical.
(define (proto-iface-name key)
  (let ((i (proto-str-index key #\/)))
    (jch-munge-segments
     (if i
         (string-append (substring key 0 i) "." (substring key (fx+ i 1) (string-length key)))
         key))))
(define (proto-key-qualified? key) (and (proto-str-index key #\/) #t))
(define (dotted-name? s) (and (proto-str-index s #\.) #t))
;; Does a protocol key answer an instance?/satisfies? query for CLASS-NAME? A
;; qualified key must match in full — that is what keeps qb.core.Shape from
;; matching a reify of qa.core/Shape. A name with no namespace on either side is
;; matched by last segment: jolt has no import table to resolve a bare interface
;; name against, so ILookup and clojure.lang.ILookup are the same question.
(define (proto-class-match? key qname)
  (let ((ki (proto-iface-name key))
        (qi (proto-iface-name qname)))
    (or (string=? ki qi)
        (and (or (not (dotted-name? ki)) (not (dotted-name? qi)))
             (string=? (jch-last-segment ki) (jch-last-segment qi))))))

;; ---- protocol registry ------------------------------------------------------
;; type-tag -> (proto-key -> (method-name -> fn)) — the source of truth for the
;; open world (extend-type/extend-protocol at any time, incl. mode A REPL).
;; register-protocol-method also mirrors the impl into the type's jrdesc ptable
;; (keyed by an interned proto-method identity) so protocol-resolve's record
;; branch resolves by descriptor identity instead of re-walking this string tree.
(define type-registry (make-hashtable string-hash string=?))
;; type-tag -> (method-name -> ((proto . fn) ...)), the SAME impls the tree above
;; holds, indexed by method name instead of by protocol.
;;
;; "Which protocol declares this method?" is the question every collection op on
;; a record asks (count/contains?/conj/assoc/seq each look for a declared impl
;; before falling back to the record behaviour), and answering it from the tree
;; means snapshotting the type's protocol keys under the registry mutex and
;; probing each one — a lock, a vector allocation, and a string hash per
;; protocol, per operation. For a plain defrecord the answer is always "none",
;; so that whole walk is spent to conclude nothing. Measured on a record
;; implementing three protocols, 300k ops: count 90ms vs 32ms for the same
;; record with no protocols, contains? 161 vs 31, seq 415 vs 105 — i.e. the walk,
;; not the operation, was the cost, and it grew with the type's protocol count.
;;
;; Maintained HERE, at the one place impls are written, under the same mutex —
;; so it cannot drift from the tree — and read with a single unlocked
;; hashtable-ref on the same terms as the rest of this registry (strong tables,
;; consistent structure, a stale miss the worst outcome).
;;
;; The alist per method preserves registration order, which makes the "any
;; protocol" answer deterministic rather than dependent on hashtable iteration
;; order; a re-registration of the same (type, proto, method) replaces its entry
;; in place instead of shadowing it.
(define type-method-index (make-hashtable string-hash string=?))
(define (tmi-add! type-tag proto method fn)
  (let* ((mi (or (hashtable-ref type-method-index type-tag #f)
                 (let ((h (make-hashtable string-hash string=?)))
                   (hashtable-set! type-method-index type-tag h) h)))
         (entries (hashtable-ref mi method '())))
    (hashtable-set! mi method
      (if (assoc proto entries)
          (map (lambda (p) (if (string=? (car p) proto) (cons proto fn) p)) entries)
          (append entries (list (cons proto fn)))))))
(define (tmi-entries type-tag method)
  (let ((mi (hashtable-ref type-method-index type-tag #f)))
    (if mi (hashtable-ref mi method '()) '())))
;; Prune both tables together. The per-case harnesses (run-corpus.ss/run-unit.ss)
;; drop every type a case defined; pruning the tree alone would leave this index
;; answering for types that no longer exist, so the two are pruned through one
;; entry point rather than by remembering to do both.
(define (prune-type-registry! keep?)
  (vector-for-each
    (lambda (k)
      (unless (keep? k)
        (hashtable-delete! type-registry k)
        (hashtable-delete! type-method-index k)))
    (hashtable-keys type-registry))
  ;; a tag can reach a derived table without reaching the tree only if they ever
  ;; diverge; sweep both on the same rule so a leak cannot outlive the prune.
  (vector-for-each
    (lambda (k) (unless (keep? k) (hashtable-delete! type-method-index k)))
    (hashtable-keys type-method-index))
  (vector-for-each
    (lambda (k) (unless (keep? k) (hashtable-delete! type-class-memo k)))
    (hashtable-keys type-class-memo)))
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
    ;; double-checked: the whole point of this table is that one (proto . method)
    ;; has ONE eq?-comparable identity. Two threads racing a fresh pair used to
    ;; mint two gensyms, and the descriptor ptable is keyed by that identity — so
    ;; a store under one key and a lookup under the other miss each other for the
    ;; life of the process, silently demoting the record fast path.
    (or k (jolt-with-mutex rec-tbl-mu
            (or (hashtable-ref proto-method-keys s #f)
                (let ((nk (gensym (string-append proto "." method))))
                  (hashtable-set! proto-method-keys s nk) nk))))))
;; descriptor ptable lookup (the record fast path): one eq?-ref keyed by the
;; interned identity, valid while this desc is current (not invalidated by a
;; type re-def). #f on any miss so protocol-resolve drops to the string registry.
(define (find-protocol-method-desc desc proto method)
  (let ((pt (jrdesc-ptable desc)))
    (and pt (hashtable-ref pt (intern-pm-key proto method) #f))))
(define (register-protocol-method type-tag proto method fn)
  ;; the epoch bump, the two check-then-creates and the impl write are ONE step:
  ;; split, two threads extending the same type each build their own inner table
  ;; and the second overwrite drops the first's impl entirely.
  (jolt-with-mutex rec-tbl-mu
    (set! jolt-proto-epoch (fx+ jolt-proto-epoch 1))
    (let* ((ti (or (hashtable-ref type-registry type-tag #f)
                   (let ((h (make-hashtable string-hash string=?))) (hashtable-set! type-registry type-tag h) h)))
           (pi (or (hashtable-ref ti proto #f)
                   (let ((h (make-hashtable string-hash string=?))) (hashtable-set! ti proto h) h))))
      (hashtable-set! pi method fn)
      ;; the by-method index of the same impl, inside the same critical section
      (tmi-add! type-tag proto method fn)))
  ;; mirror onto the type's descriptor ptable (record types only — a host tag
  ;; like "String"/"Object" has no desc). A re-def invalidated the old desc's
  ;; ptable (set it to #f), and this call populates the new desc's ptable.
  (let ((desc (hashtable-ref chez-tag-desc type-tag #f)))
    (when desc
      ;; intern-pm-key first, OUTSIDE the lock it takes itself, then the ptable
      ;; create-and-write as one step for the same reason as above
      (let ((k (intern-pm-key proto method)))
        (jolt-with-mutex rec-tbl-mu
          (let ((pt (or (jrdesc-ptable desc)
                        (let ((h (make-eq-hashtable))) (jrdesc-ptable-set! desc h) h))))
            (hashtable-set! pt k fn))))))
  ;; a (re)registration of this impl invalidates any contagion clone built for it —
  ;; the clone captured the prior body. Keyed exactly (type/proto/method) so a
  ;; sibling type's clone survives; devirt-resolve-fl then falls back to devirt-resolve.
  (remove-clone! type-tag proto method)
  (if #f #f))
(define (find-protocol-method type-tag proto method)
  (let ((ti (hashtable-ref type-registry type-tag #f)))
    (and ti (let ((pi (hashtable-ref ti proto #f))) (and pi (hashtable-ref pi method #f))))))
;; The impl for METHOD under any protocol this type implements — one ref into
;; the by-method index, first registration wins. This is the hot one (every
;; record collection op asks it), so it neither locks nor allocates.
(define (find-method-any-protocol type-tag method)
  (let ((entries (tmi-entries type-tag method)))
    (and (pair? entries) (cdar entries))))
;; A deftype can implement a method NAME at two arities from two interfaces (e.g.
;; data.priority-map's seq: Seqable.seq[this] and Sorted.seq[this ascending]),
;; registered under different protocols. Pick the impl whose procedure accepts
;; the call's arg count (this + args); fall back to any same-named impl.
(define (proc-accepts? f n)
  (and (procedure? f) (bitwise-bit-set? (procedure-arity-mask f) n)))
(define (find-method-any-protocol-arity type-tag method nargs)
  (let ((entries (tmi-entries type-tag method)))
    (and (pair? entries)
         (let loop ((es entries))
           (cond ((null? es) (cdar entries))          ; no arity match — any impl
                 ((proc-accepts? (cdar es) nargs) (cdar es))
                 (else (loop (cdr es))))))))
(define (type-satisfies? type-tag proto)
  (let ((ti (hashtable-ref type-registry type-tag #f)))
    (and ti (hashtable-ref ti proto #f) #t)))
;; instance?'s question, which arrives as a CLASS name rather than a protocol key:
;; (instance? clojure.lang.ILookup x), (instance? some.ns.SomeProtocol x). Exact
;; first (a host-interface key is spelled the same), then match each of the type's
;; protocol keys as an interface name.
(define (type-implements-class?-uncached type-tag qname)
  (let ((ti (hashtable-ref type-registry type-tag #f)))
    (and ti
         (or (and (hashtable-ref ti qname #f) #t)
             (let* ((ks (jolt-with-mutex rec-tbl-mu (hashtable-keys ti))) (n (vector-length ks)))
               (let loop ((i 0))
                 (and (fx< i n)
                      (or (proto-class-match? (vector-ref ks i) qname)
                          (loop (fx+ i 1))))))))))
;; …and memoized per (type-tag, class-name), because that walk is not cheap and
;; instance? asks it repeatedly with the same pair. Every candidate key is run
;; through proto-class-match?, which re-derives BOTH sides' interface names —
;; string munging and allocation per protocol, per call — on top of the usual
;; snapshot-under-the-mutex. Measured at 1.8us per (instance? SomeProtocol x)
;; against 140ns for satisfies? answering the same question.
;;
;; Guarded by the same epoch pair as the descriptor ifc cache: an extend-type can
;; add a protocol and a class registration can change what an interface name
;; resolves to, and both only ever increase. Nested tag -> qname -> (epoch . bool)
;; so a hit is two refs and no allocation.
(define type-class-memo (make-hashtable string-hash string=?))
(define type-class-memo-mu (make-mutex))
(define (type-implements-class? type-tag qname)
  (let* ((epoch (jrdesc-ifc-epoch))
         (inner (hashtable-ref type-class-memo type-tag #f))
         (hit (and inner (hashtable-ref inner qname #f))))
    (if (and hit (fx=? (car hit) epoch))
        (cdr hit)
        (let ((v (type-implements-class?-uncached type-tag qname)))
          (jolt-with-mutex type-class-memo-mu
            (let ((i2 (or (hashtable-ref type-class-memo type-tag #f)
                          (let ((h (make-hashtable string-hash string=?)))
                            (hashtable-set! type-class-memo type-tag h) h))))
              (hashtable-set! i2 qname (cons epoch v))))
          v))))
;; True when a deftype/record/reify instance DECLARES a method by this name (an
;; inline protocol impl), so clojure.core can prefer it over generic collection
;; behavior — e.g. (empty priority-map) must use the type's own empty, not return
;; {}, and (ifn? (reify IFn (invoke [_] …))) is true as on the JVM.
(def-var! "jolt.host" "jrec-method?"
  (lambda (v name)
    (cond ((jrec? v) (if (find-method-any-protocol (jrec-tag v) name) #t #f))
          ((reified-methods v) => (lambda (m) (if (hashtable-ref m name #f) #t #f)))
          (else #f))))

;; (str x) is x.toString() on the JVM, so a deftype/record that DECLARES toString
;; renders through it. A defrecord's automatic map rendering is not a declared
;; method, so only an explicit impl takes over; pr-str is untouched, which is also
;; the JVM split (pr of a record shows its map whatever toString says).
;; instaparse's Segment -- a CharSequence view over a string -- is one such type.
;; Hooked here rather than through register-str-render!: a record is matched by an
;; earlier collection arm and never reaches that registry.
(set-str-tostring-hook!
  (lambda (v)
    (and (jrec? v)
         (find-method-any-protocol (jrec-tag v) "toString")
         (record-method-dispatch v "toString" jolt-nil))))

;; the interfaces every defrecord carries, spelled both ways (the record arm of
;; value-host-tags below).
(define jrec-record-iface-tags
  '("IRecord" "clojure.lang.IRecord" "IPersistentMap" "clojure.lang.IPersistentMap"
    "APersistentMap" "Associative" "ILookup" "Seqable" "Counted"
    "IPersistentCollection" "IObj" "IMeta" "Map" "java.util.Map"
    "Iterable" "java.lang.Iterable" "Object"))

;; a jch-tags list always ends with "Object"; the tail without it, for splicing
;; ahead of another tag list.
(define (jch-tags-sans-object ts)
  (if (null? (cdr ts)) '() (cons (car ts) (jch-tags-sans-object (cdr ts)))))

;; A reify's dispatch tags: each declared protocol/interface as its JVM interface
;; name plus that interface's modeled ancestry, deduped, Object last — so an
;; extend-protocol filed under an interface name (clojure.lang.IReduceInit,
;; java.lang.Iterable, …) reaches a reify declaring it, exactly as instanceof
;; answers on the JVM. The reify's own method table is consulted before these
;; (protocol-resolve), so an inline impl still wins.
(define (jreify-host-tags obj)
  (let loop ((ps (jreify-protos obj)) (acc '()))
    (if (null? ps)
        (reverse (cons "Object" acc))
        (let inner ((ts (jch-tags (proto-iface-name (car ps)))) (acc acc))
          (if (null? ts)
              (loop (cdr ps) acc)
              (inner (cdr ts)
                     (let ((t (car ts)))
                       (if (or (string=? t "Object") (member t acc)) acc (cons t acc)))))))))

;; host type-tag candidates for a non-record value (extend-protocol on builtins).
(define (value-host-tags obj)
  ;; numbers dispatch by actual type (a Double is NOT a Long): flonum -> Double,
  ;; exact ratio -> Ratio, exact integer -> Long.
  (cond ((flonum? obj) '("Double" "Float" "Number" "Object"))
        ((and (number? obj) (exact? obj) (not (integer? obj))) '("Ratio" "Number" "Object"))
        ;; exact integers split at the LONG RANGE (issue #627), the same
        ;; boundary the printer's N suffix uses — NOT the fixnum range: Chez
        ;; fixnums are 61-bit, so Long/MAX_VALUE is a Chez bignum that must
        ;; still be a Long (tools.reader asserts it). In range: Long, plus the
        ;; JVM-int breadth ported code checks for (Integer). Beyond: what the
        ;; JVM boxes as clojure.lang.BigInt, with the BigInteger tag kept —
        ;; jolt's one big representation serves both classes, a documented
        ;; superset. (instance? BigInt 21) is false on the JVM and now here.
        ((and (number? obj) (exact? obj) (integer? obj))
         (if (jolt-bigint-print? obj)
             '("BigInt" "BigInteger" "Number" "Object")
             '("Long" "Integer" "Number" "Object")))
        ((number? obj) '("Number" "Object"))
        ((string? obj) '("String" "CharSequence" "Object"))
        ((boolean? obj) '("Boolean" "Object"))
        ((char? obj) (jch-tags "java.lang.Character"))
        ((keyword? obj) (jch-tags "clojure.lang.Keyword"))
        ((jolt-symbol? obj) (jch-tags "clojure.lang.Symbol"))
        ;; a map entry is a flagged pvec — check before plain vectors so its
        ;; class tags are clojure.lang.MapEntry's (which include APersistentVector,
        ;; so vector checks still hold) plus java.util.Map$Entry.
        ((jolt-map-entry? obj) (jch-tags "clojure.lang.MapEntry"))
        ;; a subvec view dispatches as the JVM's SubVector: through
        ;; APersistentVector every vector check holds, but an extend-protocol
        ;; on the concrete PersistentVector does NOT catch it (issue #629)
        ((jolt-subvec-view? obj) (jch-tags "clojure.lang.APersistentVector$SubVector"))
        ((pvec? obj) (jch-tags "clojure.lang.PersistentVector"))
        ((pmap? obj) (if (pmap-array? obj)
                        (jch-tags "clojure.lang.PersistentArrayMap")
                        (jch-tags "clojure.lang.PersistentHashMap")))
        ((pset? obj) (jch-tags "clojure.lang.PersistentHashSet"))
        ;; A seq dispatches as the concrete class its flavor says it is — the same
        ;; answer (class …) gives, read from the same table (host-class.ss
        ;; cseq-class-name, a forward reference resolved at dispatch time). So
        ;; extend-protocol clojure.lang.IPersistentList (algo.monads' writer monad)
        ;; catches a real list, and an extend-protocol on the concrete
        ;; PersistentList no longer also swallows every vector seq and map seq.
        ((cseq? obj) (jch-tags (cseq-class-name obj)))
        ((empty-list-t? obj) (jch-tags "clojure.lang.PersistentList$EmptyList"))
        ;; a lazy seq (map/filter/… result) is clojure.lang.LazySeq: a Sequential
        ;; ISeq, but not a PersistentList — matching the JVM so extend-protocol /
        ;; instance? on a deferred seq dispatch like an eager one where they should.
        ((jolt-lazyseq? obj) (jch-tags "clojure.lang.LazySeq"))
        ;; a var is clojure.lang.Var (also IDeref / IFn) — reitit's Expand protocol
        ;; extends to Var so a #'handler route dispatches.
        ((var-cell? obj) (jch-tags "clojure.lang.Var"))
        ;; a Class VALUE — a modeled host Class (jhost "class") or a deftype/record
        ;; type token (its make-deftype-ctor closure). Both are java.lang.Class on the
        ;; JVM, so a protocol extended to Class dispatches on them (schema extends its
        ;; Schema protocol to Class, then calls (spec SomeClass)).
        ((jclass? obj) '("Class" "java.lang.Class" "Object"))
        ((and (procedure? obj) (deftype-ctor-tag obj))
         '("Class" "java.lang.Class" "Object"))
        ;; a named fn reports its own JVM-style class "ns$munged-name" (the same
        ;; (class the-fn) yields) ahead of AFunction's ancestry from the class
        ;; graph, so a protocol extended to a SPECIFIC fn's class dispatches on
        ;; it — schema keys its primitive schemas by (class @(resolve 'double))
        ;; and friends. The ancestry is the same list an anonymous fn reports
        ;; below; a hand-copied list here had no Comparator, Runnable or
        ;; Callable, so (instance? java.util.Comparator inc) was false while
        ;; (instance? java.util.Comparator (fn [a b] 0)) was true.
        ((and (procedure? obj) (hashtable-ref proc-name-tbl obj #f))
         => (lambda (p)
              (cons (string-append (class-munge-name (car p)) "$" (class-munge-name (cdr p)))
                    (jch-tags "clojure.lang.AFunction"))))
        ;; a value-layer shim value (java.time.*, URI, ByteBuffer, java.io reader/
        ;; writer, ArrayList/HashMap, …) reports its class's whole ancestry from the
        ;; single jhost-tag->fqn registry (class-hierarchy.ss). So (extend-protocol
        ;; java.time.temporal.Temporal …) fires on an Instant, java.io.Reader on a
        ;; PushbackReader, java.util.List on an ArrayList — each inheriting the
        ;; modeled supers instead of a hand-copied literal that drifts. A tag naming
        ;; no modeled class (in-stream, jolt-comparator) returns #f and falls through.
        ((and (jhost? obj) (jhost-value-tags (jhost-tag obj))) => (lambda (tags) tags))
        ;; arrays dispatch by their JVM array-class name — extend-protocol to
        ;; (Class/forName "[B") for byte[] (data.json, aws-api), "[C" for char[].
        ((and (jolt-array? obj) (eq? (jolt-array-kind obj) 'byte)) '("[B" "Object"))
        ((and (jolt-array? obj) (eq? (jolt-array-kind obj) 'char)) '("[C" "Object"))
        ((and (jolt-array? obj) (eq? (jolt-array-kind obj) 'int)) '("[I" "Object"))
        ((and (jolt-array? obj) (eq? (jolt-array-kind obj) 'long)) '("[J" "Object"))
        ((and (jolt-array? obj) (eq? (jolt-array-kind obj) 'double)) '("[D" "Object"))
        ((jolt-array? obj) '("[Ljava.lang.Object;" "Object"))
        ;; host value types with a distinct record repr (not jhost-backed): a regex,
        ;; a #uuid, a #inst Date, a BigDecimal — each derives its ancestry from the
        ;; class graph. A #inst is a java.util.Date (NOT a java.sql.Timestamp — the
        ;; instance? arm in inst-time.ss agrees).
        ((regex-t? obj) (jch-tags "java.util.regex.Pattern"))
        ((juuid? obj) (jch-tags "java.util.UUID"))
        ((jinst? obj) (jch-tags "java.util.Date"))
        ((jbigdec? obj) (jch-tags "java.math.BigDecimal"))
        ;; a bare procedure (fn) — extend-protocol to clojure.lang.{Fn,IFn,AFn}.
        ((procedure? obj) (jch-tags "clojure.lang.AFunction"))
        ((jolt-nil? obj) '("nil"))
        ;; a defrecord IS the clojure.lang map/record interfaces, so a protocol
        ;; extended to IRecord / IPersistentMap / Associative / Seqable / … (and not
        ;; to the record's own type) dispatches to it — e.g. core.logic extends
        ;; IWalkTerm to clojure.lang.IRecord, and walking a record value must hit
        ;; that, not the Object default (which would recur forever). The record's
        ;; own type is tried first (dispatch checks jrec-tag before these tags).
        ;; a reify reports its declared interfaces and their ancestry — see
        ;; jreify-host-tags above.
        ((jreify? obj) (jreify-host-tags obj))
        ;; A record that declares FURTHER interfaces beyond the automatic map set
        ;; reports those too (register-inline-protocol! files them as the tag's
        ;; supers in the class graph, so cddr of the cached jch-tags list — own
        ;; tag and simple segment dropped — is exactly the declared ancestry).
        ;; The common record declares nothing extra and keeps the shared list,
        ;; costing this hot fallback path one cons, as before.
        ((jrec-record? obj)
         (let* ((tag (jrec-tag obj))
                (extra (cddr (jch-tags tag))))
           (cons tag (if (null? (cdr extra))
                         jrec-record-iface-tags
                         (append (jch-tags-sans-object extra) jrec-record-iface-tags)))))
        ;; a bare deftype dispatches through its own tag first, then its declared
        ;; interfaces and their modeled ancestry — an extend-protocol filed under
        ;; clojure.lang.IReduceInit reaches a deftype declaring it, as instanceof
        ;; does. The tag's own SIMPLE segment is dropped (cddr): extensions on a
        ;; deftype name file under its ns-qualified tag, and a bare segment could
        ;; collide with a host class of the same name. A type declaring no
        ;; interfaces keeps (tag "Object").
        ((jrec? obj) (let ((tag (jrec-tag obj))) (cons tag (cddr (jch-tags tag)))))
        ;; a throwable reports its OWN class and that class's ancestry, so
        ;; (extend-protocol P Throwable …) reaches an ex-info or a host-constructed
        ;; RuntimeException. clojure.datafy extends Datafiable to Throwable exactly
        ;; this way; without it the Object default won and datafy never reached
        ;; Throwable->map.
        ((jolt-ex-info-record? obj) (jch-tags (jolt-ex-info-record-class-name obj)))
        ;; an atom is clojure.lang.Atom — an IRef, so a protocol extended to IRef
        ;; or IDeref dispatches on one (clojure.datafy again).
        ((jolt-atom? obj) (jch-tags "clojure.lang.Atom"))
        ;; a namespace value is clojure.lang.Namespace — (class *ns*) already says
        ;; so, and clojure.datafy extends Datafiable to it.
        ((jns? obj) (jch-tags "clojure.lang.Namespace"))
        ;; anything else that reports a modeled class — an agent, a volatile, a
        ;; delay, a future, a promise, a reduced box, a chunk buffer, a ref —
        ;; dispatches as that class and its ancestry: the class arms name it
        ;; (host-class.ss) and the graph carries its supers, so an extension on
        ;; clojure.lang.Volatile, or on IDeref for any of them, reaches the
        ;; value. The same class instance? reads. A value naming no modeled
        ;; class stays a plain Object; this is the last arm, so only values
        ;; every arm above declined pay for the lookup. jolt-class-name is the
        ;; java host layer's (host-class.ss, loaded after this file); the Gambit
        ;; runtime shares this file and shims it to answer no class (rt-core.ss).
        ((let ((n (jolt-class-name obj))) (and (string? n) (jch-known-exact? n) n))
         => jch-tags)
        (else '("Object"))))


;; assoc every entry of a map onto a record — the __extmap of the record
;; class's full constructor, carried as extension fields.
(define (jrec-assoc-entries r ext)
  (let loop ((s (jolt-seq ext)) (r r))
    (if (jolt-nil? s) r
        (let ((e (seq-first s)))
          (loop (jolt-seq (seq-more s)) (jolt-assoc r (jolt-nth e 0) (jolt-nth e 1)))))))

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
          ;; read-invalidate-install is one step: split, a concurrent re-def of
          ;; the same tag can install its desc between this read and this write
          ;; and have its ptable invalidated by us right after, leaving the live
          ;; desc permanently on the slow path.
          (_ (jolt-with-mutex rec-tbl-mu
               (let ((old-desc (hashtable-ref chez-tag-desc tag #f)))
                 (when old-desc (jrdesc-ptable-set! old-desc #f)))
               (hashtable-set! chez-tag-desc tag desc)))
         (nf (length kws))
         ;; the ctor var's name, baked at definition (the JVM ArityException
         ;; names the positional ctor: "… passed to: ns/->Name").
         (ctor-name (string-append (chez-current-ns) "/->" (symbol-t-name name-sym)))
           (build (lambda (args)
                    (let ((v (make-vector nf jolt-nil)))
                      (let loop ((as args) (i 0))
                        (if (or (null? as) (fx=? i nf)) (make-jrec desc v jolt-nil)
                            (let ((a (car as)))
                             (vector-set! v i
                                          (if (and (fx< i ndbl) (vector-ref dbl-flags i)
                                                   (number? a) (not (flonum? a)))
                                              (exact->inexact a) a))
                             (loop (cdr as) (+ i 1))))))))
           (ctor (lambda args
                   (let ((n (length args)))
                     (cond
                       ((= n nf) (build args))
                       ;; A record class has a second constructor on the JVM:
                       ;; the fields, then __meta and __extmap. (R. f1 .. fn m
                       ;; ext) is what a macro building records without the
                       ;; positional factory emits (typed.clojure's create-expr
                       ;; expands to `new` with all eight).
                       ((and (= n (+ nf 2)) (hashtable-ref chez-record-type-tbl tag #f))
                        (let* ((r (build args))
                               (m (list-ref args nf))
                               (ext (list-ref args (+ nf 1)))
                               (r (if (jolt-nil? ext) r (jrec-assoc-entries r ext))))
                          (if (jolt-nil? m) r (jolt-with-meta r m))))
                       (else
                        (throw-jvm (quote ArityException)
                          (string-append "Wrong number of args (" (number->string n)
                                         ") passed to: " ctor-name))))))))
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
    (jolt-with-mutex rec-tbl-mu
      (hashtable-set! chez-deftype-tag-set tag #t)
      (hashtable-set! chez-simple-name-tag (symbol-t-name name-sym) tag))
    ;; graft the type onto the class graph so isa?/supers/ancestors see it. A
    ;; bare deftype is an IType; defrecord (which runs register-record-type!
    ;; right after) replaces the row with the record interface set.
    (jch-set-supers! tag '("clojure.lang.IType"))
    (deftype-ctor-tag-set! ctor tag)
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
                  (jolt-with-mutex rec-tbl-mu
                    (hashtable-set! chez-protocol-methods-tbl
                                    (string-append ns "/" m) (cons proto-name m)))))
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
    (cond
      ;; a "ns$name" fn-class / inner-class name (from a Class value, e.g.
      ;; (class some-fn) -> "clojure.core$double") is always a host tag — the
      ;; value reports the same string in value-host-tags — so use it verbatim
      ;; rather than localizing it to the current ns (schema extends its Schema
      ;; protocol to (class @(resolve 'double)) and friends).
      ((let loop ((i 0)) (cond ((fx>=? i (string-length type-name)) #f)
                               ((char=? (string-ref type-name i) #\$) #t)
                               (else (loop (fx+ i 1)))))
       type-name)
      ;; a literal host-type-set member canonicalizes to its stripped base (which
      ;; is already the simple segment or an FQN the value reports verbatim, e.g.
      ;; "[Ljava.lang.Object;").
      ((hashtable-ref host-type-set base #f) base)
      ;; a graph-modeled class canonicalizes to its simple last segment — the tag
      ;; value-host-tags emits via jch-tags. Using base here would leave a partial
      ;; segment for nested packages (java.time.temporal.Temporal -> "temporal.Temporal"),
      ;; which no dispatch tag matches, so a class-keyed protocol never fires.
      ((and (not (hashtable-ref chez-simple-name-tag type-name #f))
            (not (hashtable-ref chez-deftype-tag-set type-name #f))
            (or (jch-known? base) (jch-known? type-name)))
       (jch-last-segment type-name))
      ;; A dotted name the graph does not model and that names no deftype is still
      ;; a CLASS name: one a library declared for its own values through
      ;; __register-class! (whose tags-fn reports this exact string), or one jolt
      ;; does not model. File it verbatim. Falling through localized it to the
      ;; EXTENDING ns instead — filing the impl under "jdbc.impl.java.sql.Connection"
      ;; for (extend-protocol IConnection java.sql.Connection …) — a tag no value
      ;; can carry, so the extension silently never fired. That defeated the whole
      ;; point of __register-class!, which exists to make such an extension
      ;; dispatch. A deftype tag is dotted too, and verbatim is right for it as
      ;; well: it is already the tag format, so a forward extend by fully-qualified
      ;; name lands correctly instead of being prefixed twice.
      ((dotted-name? type-name) type-name)
      (else #f))))
;; An extend/extend-type/extend-protocol registration marks the tag as an
;; extender of the protocol (recorded inside type-registry so the per-case prune
;; restores it). deftype/defrecord inline impls go through register-inline-method
;; and skip the mark: the JVM compiles inline protocol methods into the class, so
;; extenders excludes them.
(define extend-mark "__jolt_extend__")
(define (mark-extend! tag proto-name)
  (jolt-with-mutex rec-tbl-mu
    (let ((ti (hashtable-ref type-registry tag #f)))
      (when ti (let ((pi (hashtable-ref ti proto-name #f)))
                 (when pi (hashtable-set! pi extend-mark #t)))))))
(define (register-method type-name proto-name method-name fn)
  (let* ((host (canonical-host-tag type-name))
         (local (string-append (chez-current-ns) "." type-name))
         ;; a host class -> its canonical tag; a deftype defined in THIS ns -> the
         ;; local tag; an :import-ed deftype from another ns -> its real tag via the
         ;; simple-name index; otherwise the local tag (a forward extend).
         (tag (cond (host host)
                    ((hashtable-ref chez-deftype-tag-set local #f) local)
                    ;; a deftype named by its FULLY-QUALIFIED name — the tag
                    ;; format itself, so it needs no ns prefix. schema does this:
                    ;; (extend-protocol Completer schema.spec.variant.VariantSpec
                    ;; …) from a third namespace. Without this the name is
                    ;; prefixed with the EXTENDING ns and the impl is filed under
                    ;; a tag no value carries.
                    ((hashtable-ref chez-deftype-tag-set type-name #f) type-name)
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
  (let ((tag (string-append (chez-current-ns) "." type-name)))
    (jolt-with-mutex rec-tbl-mu
      (let ((ti (or (hashtable-ref type-registry tag #f)
                    (let ((h (make-hashtable string-hash string=?))) (hashtable-set! type-registry tag h) h))))
        (unless (hashtable-ref ti proto-name #f)
          (hashtable-set! ti proto-name (make-hashtable string-hash string=?))))))
  ;; the protocol's interface joins the type's class ancestry, spelled like the
  ;; JVM interface. A protocol key carries its defining ns, so "a.b/P" is the
  ;; interface a.b.P wherever the implementing type lives. A dotted host name
  ;; (clojure.lang.IPersistentMap, a java interface) is already canonical.
  (let ((iface (cond
                 ((proto-key-qualified? proto-name) (proto-iface-name proto-name))
                 ((dotted-name? proto-name) proto-name)
                 ;; a SIMPLE name: an imported JVM interface (IPersistentMap, from
                 ;; (:import (clojure.lang IPersistentMap))) resolves to its
                 ;; canonical FQN so the type inherits that interface's own
                 ;; ancestry (IPersistentMap → Associative → IPersistentCollection);
                 ;; anything else is qualified against the defining ns.
                 (else
                  (let ((fqn (jch-fqn-of-simple proto-name)))
                    (if (string=? fqn proto-name)
                        (string-append (jch-munge-segments (chez-current-ns)) "." proto-name)
                        fqn))))))
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
                (cond ((null? tags) (throw-jvm (quote IllegalArgumentException) (string-append "No reified method " method-name)))
                      ((find-protocol-method (car tags) proto-name method-name))
                      (else (loop (cdr tags))))))))
    (else
     (let loop ((tags (value-host-tags obj)))
       (cond ((null? tags) (throw-jvm (quote IllegalArgumentException) (string-append "No method " method-name " in " proto-name)))
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

;; ---- contagion clone registry -----------------------------------
;; A devirtualized call site over an impl whose body has a :num field beside a
;; proven :double operand resolves a contagion-specialized clone (fl* + the :num
;; operand coerced via exact->inexact) instead of the shared impl body. The clone is
;; emitted once per (impl, record-type) in a whole-program build and registered here,
;; keyed exactly like devirt-resolve's lookup. PIC and the generic protocol registry
;; never see it — a megamorphic site resolves the shared impl through type-registry,
;; so the contagion is gated to monomorphic call sites by construction.
;; register-protocol-method invalidates a key's clone on (re)registration, so a
;; re-extend that replaces the impl makes devirt-resolve-fl fall back to the fresh
;; devirt-resolve. Startup order is sound: the impl registers first (bumping the
;; epoch and removing a not-yet-present clone), then the clone's sibling def registers.
(define clone-registry (make-hashtable string-hash string=?))
(define (register-clone type-tag proto method fn)
  (jolt-with-mutex rec-tbl-mu
    (let* ((ti (or (hashtable-ref clone-registry type-tag #f)
                   (let ((h (make-hashtable string-hash string=?))) (hashtable-set! clone-registry type-tag h) h)))
           (pi (or (hashtable-ref ti proto #f)
                   (let ((h (make-hashtable string-hash string=?))) (hashtable-set! ti proto h) h))))
      (hashtable-set! pi method fn)
      jolt-nil)))
;; the back end emits this alongside a register-inline-method call, passing the bare
;; type-name (the call's first arg); the runtime tags it via the current ns exactly
;; as register-inline-method does, so the clone and the impl land under the same tag
;; the devirt site's full :devirt-type names. No ns bookkeeping in the back end.
(define (register-clone* type-name proto method fn)
  (register-clone (string-append (chez-current-ns) "." type-name) proto method fn))
(define (find-clone type-tag proto method)
  (let ((ti (hashtable-ref clone-registry type-tag #f)))
    (and ti (let ((pi (hashtable-ref ti proto #f))) (and pi (hashtable-ref pi method #f))))))
(define (remove-clone! type-tag proto method)
  (jolt-with-mutex rec-tbl-mu
    (let ((ti (hashtable-ref clone-registry type-tag #f)))
      (when ti
        (let ((pi (hashtable-ref ti proto #f)))
          (when pi (hashtable-delete! pi method)))))))
;; a devirt site whose (type/proto/method) has a clone resolves it; otherwise the
;; ordinary devirt-resolve. The non-specialized path is byte-identical to before —
;; the back end emits devirt-resolve-fl only at a site it knows has a clone.
(define (devirt-resolve-fl type-tag proto-name method-name obj)
  (or (find-clone type-tag proto-name method-name)
      (devirt-resolve type-tag proto-name method-name obj)))


;; ---- compare over a declared Comparable ------------------------------------
;; clojure.core/compare calls compareTo on anything that implements Comparable
;; (Util.compare's ((Comparable) o1).compareTo(o2)), and a deftype/defrecord that
;; declares the interface is exactly that. Without this arm the type's own
;; compareTo was reachable as (.compareTo a b) but invisible to compare — so
;; sort, sorted-set and sorted-map-by all raised "cannot be compared to" on
;; values that carry an ordering, and (into (sorted-set) types) — how
;; typedclojure builds every union — could not be evaluated at all.
;;
;; Registered here rather than in converters.ss because it needs the protocol
;; registry, and it goes through find-method-any-protocol so a compareTo declared
;; under any interface the type implements answers, the same lookup
;; record-method-dispatch performs for the direct call.
(define (jrec-comparable-method v)
  (and (jrec? v) (find-method-any-protocol (jrec-tag v) "compareTo")))
(register-compare-arm!
  (lambda (a b) (and (jrec-comparable-method a) #t))
  (lambda (a b) (jnum->exact (jolt-invoke (jrec-comparable-method a) a b))))
