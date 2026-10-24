;; protocols.ss — protocol identity, registry, and resolution: the "<ns>/<Name>"
;; dispatch key, type-registry + jolt-proto-epoch, method lookup
;; (find-protocol-method / find-method-any-protocol), value-host-tags (the host
;; type-tag candidates a non-record value dispatches through), make-deftype-ctor
;; / make-protocol, register-method + the extend plumbing, protocol-resolve with
;; the dispatchN entry points, the per-site PIC, devirt-resolve, and the
;; contagion clone registry.
;;
;; Loaded after records-coll.ss; uses the jrec layout + rec-tbl-mu from
;; records.ss. reify-method-ref / record-method-dispatch are free references into
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
;;
;; The discriminator is the SAME keyword every other :jolt/type in the host is —
;; (keyword "jolt" "type"), namespace "jolt", name "type", which is what the
;; reader produces for :jolt/type in Clojure source and what host-contract.ss
;; (hc-kw-jolt-type), host-table.ss (kw-jtype) and natives-meta.ss (ty-kw-jtype)
;; all spell. It was once (keyword #f "jolt/type") — one keyword whose NAME was
;; the whole string — which printed identically, compared equal to none of them,
;; and so left the tag INERT: (:jolt/type P) was nil from source, and hc-map?
;; ("a map form is a pmap with no :jolt/type") read a protocol value as a plain
;; map form, re-analyzing a spliced one as a map literal. jolt-dkz.
(define proto-kw-jtype (keyword "jolt" "type"))
(define proto-kw-protocol (keyword "jolt" "protocol"))
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
;; Forget everything registered against ONE type tag: its protocols, its methods,
;; the by-method index, the class-answer memo and any devirt clone. A deftype
;; redefinition is a new class on the JVM — neither the previous definition's
;; own methods nor an extend-type made against it carries over — and jolt kept
;; them, so a method the new definition does not declare still answered
;; (jolt-lnwq). make-deftype-ctor calls this when a fresh descriptor replaces an
;; existing one, before the new definition's registrations run.
;; The epoch bump is what retires the per-site inline caches that resolved an
;; impl this drops.
(define (forget-type-methods! type-tag)
  (jolt-with-mutex rec-tbl-mu
    (set! jolt-proto-epoch (fx+ jolt-proto-epoch 1))
    (hashtable-delete! type-registry type-tag)
    (hashtable-delete! type-method-index type-tag)
    (hashtable-delete! type-class-memo type-tag)
    (hashtable-delete! clone-registry type-tag))
  ;; find-method-any-protocol reads type-method-index, and
  ;; chez-type-owns-lookup? (records.ss) consults it while the inference
  ;; shapes map is built: dropping a type's valAt changes that map.
  (chez-infer-registry-bump!))

(define (prune-type-registry! keep?)
  ;; a registry change like any other to the caches keyed on the epoch (the
  ;; PICs, satisfies?'s memo): a tag a later definition reuses must not find a
  ;; pruned type's answer
  (set! jolt-proto-epoch (fx+ jolt-proto-epoch 1))
  ;; ...and to the inference shapes map, through the same
  ;; find-method-any-protocol / chez-type-owns-lookup? path as
  ;; forget-type-methods!.
  (chez-infer-registry-bump!)
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
;; ...and in front of it, the same answer keyed eq? on the two name OBJECTS. A
;; protocol call on a record resolves through here every time (the per-site PIC
;; exists only under --opt), and the names it passes are the literals the
;; defprotocol shim was compiled with, so a repeat is two eq?-refs where the
;; string table cost a string-append and a hash of the joined name per call.
;; A name spelled by another string object fills its own entry; the key it gets
;; is still the one identity the string table hands out.
(define pm-key-eq-cache (make-weak-eq-hashtable))
(define pm-key-eq-mu (make-mutex))
(define (intern-pm-key proto method)
  (let* ((inner (hashtable-ref pm-key-eq-cache proto #f))
         (k (and inner (hashtable-ref inner method #f))))
    (or k
        (let ((k (intern-pm-key-by-name proto method)))
          (jolt-with-mutex pm-key-eq-mu
            (let ((t (or (hashtable-ref pm-key-eq-cache proto #f)
                         (let ((t (make-weak-eq-hashtable)))
                           (hashtable-set! pm-key-eq-cache proto t)
                           t))))
              (hashtable-set! t method k)))
          k))))
(define (intern-pm-key-by-name proto method)
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
            (hashtable-set! pt k fn))))
      ;; A type declaring its own clojure.lang.ILookup masks its fields from the
      ;; get path (records.ss jrdesc-mask-fields!) — that valAt is the only key
      ;; lookup the JVM gives a bare deftype, so it answers for a field-named key
      ;; too. A defrecord keeps its generated field-first lookup, and its
      ;; register-record-type! has already run by the time its methods register.
      (when (and (string=? method "valAt") (chez-type-owns-lookup? type-tag))
        (jrdesc-mask-fields! desc))))
  ;; a (re)registration of this impl invalidates any contagion clone built for it —
  ;; the clone captured the prior body. Keyed exactly (type/proto/method) so a
  ;; sibling type's clone survives; devirt-resolve-fl then falls back to devirt-resolve.
  (remove-clone! type-tag proto method)
  ;; an impl can flip a type's owns-lookup reading (records.ss
  ;; chez-type-owns-lookup?), which decides whether the shapes map carries the
  ;; type at all — so the inference-registry cache has to rebuild.
  (chez-infer-registry-bump!)
  (if #f #f))
(define (find-protocol-method type-tag proto method)
  (let ((ti (hashtable-ref type-registry type-tag #f)))
    (and ti (let ((pi (hashtable-ref ti proto #f))) (and pi (hashtable-ref pi method #f))))))

;; The key mark-extend! (below) writes into a protocol's impl table to record
;; that the type EXTENDED the protocol rather than declaring it inline. It lives
;; here rather than beside mark-extend! because the two questions this file
;; answers about a type — what it implements, and what its class declares — both
;; have to be able to tell the two apart.
(define extend-mark "__jolt_extend__")
;; …and its counterpart, written by register-inline-protocol! for a protocol a
;; deftype/defrecord DECLARES in its own body. Both marks can sit in one table:
;; jolt lets a type that already declares a protocol inline be extended with it
;; afterwards, which the JVM refuses outright ("class T already directly
;; implements interface P for protocol"). The class question still has the JVM's
;; answer there — the class declares the interface, so instance? is true and a
;; later extend cannot take that away — so the inline mark WINS.
(define inline-mark "__jolt_inline__")
;; Was this impl table filled by extend/extend-type/extend-protocol ALONE?
(define (extend-impl-table? pi)
  (and pi
       (hashtable-ref pi extend-mark #f)
       (not (hashtable-ref pi inline-mark #f))
       #t))
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
;;
;; An EXTENDED protocol does not count. On the JVM, extend/extend-type/
;; extend-protocol only file the implementation in the protocol's own method
;; table — the target class is never touched, so it gains neither the protocol's
;; interface nor its methods, and (instance? <the protocol's interface> x) stays
;; false while satisfies? becomes true. jolt files both kinds of implementation
;; in one registry, so the extend mark is what keeps the two questions apart.
;; Code leans on the distinction: sci.impl.types/eval-node? is
;; (instance? sci.impl.types.Eval x), meant to be true only for the AST nodes
;; that DECLARE Eval — SCI extends the same protocol onto sci.lang.Var and
;; friends for the eval function's sake. Answering true there made SCI classify
;; every resolved var as an AST node, so `resolve` returned nil inside a jolt SCI
;; context and no defrecord could find its protocol ("Protocol not found: P").
(define (type-implements-class?-uncached type-tag qname)
  (let ((ti (hashtable-ref type-registry type-tag #f)))
    (define (declared? k)
      (let ((pi (hashtable-ref ti k #f)))
        (and pi (not (extend-impl-table? pi)))))
    (and ti
         (or (declared? qname)
             (let* ((ks (jolt-with-mutex rec-tbl-mu (hashtable-keys ti))) (n (vector-length ks)))
               (let loop ((i 0))
                 (and (fx< i n)
                      (or (let ((k (vector-ref ks i)))
                            (and (declared? k) (proto-class-match? k qname)))
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
          ((jreify? v) (if (reify-method-ref v name) #t #f))
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
;;
;; A function of the protocol list and the class graph only, so it is memoized
;; per list (interned by make-reified-delegating, so one per reify site) and
;; stamped with jch-graph-epoch, which is what changes an interface's ancestry.
(define jreify-tags-memo (make-weak-eq-hashtable))
(define (jreify-host-tags obj)
  (let* ((ps (jreify-protos obj))
         (e (hashtable-ref jreify-tags-memo ps #f)))
    (if (and e (fx= (car e) jch-graph-epoch))
        (cdr e)
        (let* ((epoch jch-graph-epoch)
               (tags (jreify-protos-tags ps)))
          (jolt-with-mutex jch-cache-mutex
            (when (fx= epoch jch-graph-epoch)
              (hashtable-set! jreify-tags-memo ps (cons epoch tags))))
          tags))))
(define (jreify-protos-tags protos)
  (let loop ((ps protos) (acc '()))
    (if (null? ps)
        (reverse (cons "Object" acc))
        (let inner ((ts (jch-tags (proto-iface-name (car ps)))) (acc acc))
          (if (null? ts)
              (loop (cdr ps) acc)
              (inner (cdr ts)
                     (let ((t (car ts)))
                       (if (or (string=? t "Object") (member t acc)) acc (cons t acc)))))))))

;; A class's tags with jolt's EXTRA spellings for the same representation spliced
;; in behind the class's own two: one Chez flonum answers for Double and Float,
;; one exact integer for Long and Integer. jch-tags is (fqn simple ancestors…
;; "Object"), so keeping the first two ahead of the extras leaves the concrete
;; class outranking both the supersets and the ancestry in dispatch order.
;;
;; Cached per name the way jch-tags is (same epoch stamp, same mutex, same
;; unlocked read), so every Long reports the ONE list rather than a fresh
;; splice per dispatch — which is also what lets the satisfies? memo
;; (records-dispatch.ss) keep an answer for a number: it stores only a list the
;; graph owns, and graph-owned-tags? below is how it asks.
(define jch-tags-plus-cache (make-hashtable string-hash string=?))
(define (jch-tags-plus name extra)
  (let ((e (hashtable-ref jch-tags-plus-cache name #f)))
    (if (and e (fx= (car e) jch-graph-epoch))
        (cdr e)
        (let* ((epoch jch-graph-epoch)
               (ts (jch-tags name))
               (result (if (or (null? ts) (null? (cdr ts)))
                           (append ts extra)
                           (cons (car ts) (cons (cadr ts) (append extra (cddr ts)))))))
          (jolt-with-mutex jch-cache-mutex
            (when (fx= epoch jch-graph-epoch)
              (hashtable-set! jch-tags-plus-cache name (cons epoch result))))
          result))))
;; Is TAGS a list the class graph hands out and keeps — jch-tags' or
;; jch-tags-plus' cached list for its own head — as opposed to one built for a
;; single call (a record's cons onto its ancestry, a reify's, a named fn's)?
;; A per-call list is never the same object twice, so a cache keyed on the
;; object must not store it.
(define (graph-owned-tags? tags)
  (and (pair? tags)
       (let ((head (car tags)))
         (or (eq? tags (jch-tags head))
             (let ((e (hashtable-ref jch-tags-plus-cache head #f)))
               (and e (eq? tags (cdr e))))))))

;; host type-tag candidates for a non-record value (extend-protocol on builtins).
(define (value-host-tags obj)
  ;; numbers dispatch by actual type (a Double is NOT a Long): flonum -> Double,
  ;; exact ratio -> Ratio, exact integer -> Long.
  ;;
  ;; Every arm reads the class graph, so a scalar carries the SAME ancestry
  ;; isa?/supers report for its class and instance? (which answers from this
  ;; list) cannot disagree with them. These arms used to carry hand-written tag
  ;; lists that stopped at Number / CharSequence, so (instance? Comparable 1) was
  ;; false while (isa? Long Comparable) was true — and a protocol extended to an
  ;; interface a number implements never reached one.
  (cond ((flonum? obj) (jch-tags "java.lang.Double"))
        ((jfloat? obj) (jch-tags "java.lang.Float"))
        ((and (number? obj) (exact? obj) (not (integer? obj))) (jch-tags "clojure.lang.Ratio"))
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
             (jch-tags-plus "clojure.lang.BigInt" '("java.math.BigInteger" "BigInteger"))
             (jch-tags-plus "java.lang.Long" '("java.lang.Integer" "Integer"))))
        ((number? obj) (jch-tags "java.lang.Number"))
        ((string? obj) (jch-tags "java.lang.String"))
        ((boolean? obj) (jch-tags "java.lang.Boolean"))
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
        ;; no modeled class (in-stream) returns #f and falls through.
        ((and (jhost? obj) (jhost-value-tags (jhost-tag obj))) => (lambda (tags) tags))
        ;; arrays dispatch by their JVM array-class name — extend-protocol to
        ;; (Class/forName "[B") for byte[] (data.json, aws-api), "[C" for char[].
        ((and (jolt-array? obj) (eq? (jolt-array-kind obj) 'byte)) '("[B" "Object"))
        ((and (jolt-array? obj) (eq? (jolt-array-kind obj) 'char)) '("[C" "Object"))
        ((and (jolt-array? obj) (eq? (jolt-array-kind obj) 'int)) '("[I" "Object"))
        ((and (jolt-array? obj) (eq? (jolt-array-kind obj) 'long)) '("[J" "Object"))
        ((and (jolt-array? obj) (eq? (jolt-array-kind obj) 'double)) '("[D" "Object"))
        ;; a typed reference array (String[], int[][]) is also an Object[]
        ((jolt-array? obj)
         (let ((k (jolt-array-kind obj)))
           (if (string? k)
               (list (na-kind-class-name k) "[Ljava.lang.Object;" "Object")
               '("[Ljava.lang.Object;" "Object"))))
        ;; host value types with a distinct record repr (not jhost-backed): a regex,
        ;; a #uuid, a #inst Date, a BigDecimal — each derives its ancestry from the
        ;; class graph. A #inst is a java.util.Date (NOT a java.sql.Timestamp — the
        ;; instance? arm in inst-time.ss agrees).
        ((regex-t? obj) (jch-tags "java.util.regex.Pattern"))
        ((matcher-t? obj) (jch-tags "java.util.regex.Matcher"))
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
          ;; a redefinition drops what the previous definition registered, so the
          ;; new one starts from nothing — see forget-type-methods!
          (_ (when (hashtable-ref chez-tag-desc tag #f) (forget-type-methods! tag)))
          (_ (jolt-with-mutex rec-tbl-mu
               (let ((old-desc (hashtable-ref chez-tag-desc tag #f)))
                 (when old-desc (jrdesc-ptable-set! old-desc #f)))
               (hashtable-set! chez-tag-desc tag desc)))
          ;; A redefinition, or an extend-type that named this tag before the
          ;; type existed, may already have registered a valAt for it: the fresh
          ;; descriptor has to arrive masked, since register-protocol-method's
          ;; mask ran against the descriptor that is now gone.
          (_ (when (chez-type-owns-lookup? tag) (jrdesc-mask-fields! desc)))
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
                                          (if (and (fx< i ndbl) (vector-ref dbl-flags i))
                                              (jolt-rec-dbl a) a))
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
                          (string-append "Wrong number of args (" (jvm-arity-count n)
                                         ") passed to: " ctor-name))))))))
    ;; Register the ctor under its fully-qualified tag ("ns.Name") — a bare
    ;; (Name. …) in the DEFINING ns is qualified to this by the analyzer, so a
    ;; deftype whose simple name collides with a built-in host class (tools.reader's
    ;; PushbackReader vs java.io.PushbackReader) still resolves correctly there.
    (class-ctor-set! tag ctor)
    ;; Also register the simple name so (Name. …) resolves ns-agnostically across
    ;; files — BUT never clobber a built-in host class of the same simple name (an
    ;; unrelated ns's bare (Name. …) must still reach the built-in). A prior deftype
    ;; (tracked in chez-simple-name-tag) is fine to overwrite (last def wins / redef).
    (when (or (not (hashtable-ref class-ctors-tbl (symbol-t-name name-sym) #f))
              (hashtable-ref chez-simple-name-tag (symbol-t-name name-sym) #f))
      (class-ctor-set! (symbol-t-name name-sym) ctor))
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

;; make-protocol: a protocol value the overlay reads via (get p :name)/(get p :methods),
;; carrying the keys Clojure's protocol map carries for everyone else — :sigs,
;; :doc and :method-map here, plus :var, which defprotocol assoc's after the def
;; (the var does not exist yet at this call).
;;
;; :doc is PRESENT ONLY WHEN THE PROTOCOL HAS A DOCSTRING, which is what the
;; reference does: its opts map starts with :on/:on-interface and picks up :doc
;; only if it reads a leading string, so (contains? P :doc) is false for an
;; undocumented protocol rather than true-with-nil. jolt-nil is the macro's "no
;; docstring" signal, so an assoc, not a fixed key.
;;
;; The keys the reference has that jolt does NOT set — :on, :on-interface,
;; :method-builders, :extend-via-metadata — are a deliberate omission, not an
;; oversight: the first three describe the interface defprotocol generates on
;; the JVM, and jolt generates none (dispatch is by the receiver's type tag,
;; protocol-resolve), while :extend-via-metadata would advertise a dispatch rule
;; jolt does not yet honor. Pinned jolt-side in unit.edn (defprotocol-value) and
;; written up in the divergence legend; jolt-2j3 tracks metadata extension.
;;
;; Each trailing argument is optional only so a seed minted before it existed
;; still loads while it mints the next one: the arity the OLD prelude spells is
;; the one that has to keep working.
(define (make-protocol name-str methods . rest)
  (let* ((sigs (if (null? rest) (jolt-hash-map) (car rest)))
         (rest2 (if (null? rest) '() (cdr rest)))
         (doc (if (null? rest2) jolt-nil (car rest2)))
         (rest3 (if (null? rest2) '() (cdr rest2)))
         (method-map (if (null? rest3) (jolt-hash-map) (car rest3)))
         (base (jolt-hash-map proto-kw-jtype proto-kw-protocol
                              proto-kw-name (jolt-symbol jolt-nil name-str)
                              (keyword #f "methods") methods
                              (keyword #f "sigs") sigs
                              (keyword #f "method-map") method-map)))
    (if (jolt-nil? doc) base (jolt-assoc base (keyword #f "doc") doc))))

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
  (chez-infer-registry-bump!)
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
                "Matcher" "java.util.regex.Matcher" "MatchResult" "java.util.regex.MatchResult"
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
;; JVM array class name for an array of KIND ((class (int-array 3)) -> "[I", like
;; the JVM's Class.getName for arrays): a String[] is "[Ljava.lang.String;", and
;; an array whose component is itself an array prefixes one more "[". A reference
;; array's kind is its component class name (java/natives-array.ss).
(define (na-kind-class-name k)
  (case k
    ((int) "[I") ((long) "[J") ((short) "[S") ((double) "[D")
    ((float) "[F") ((boolean) "[Z") ((byte) "[B") ((char) "[C")
    (else (cond ((not (string? k)) "[Ljava.lang.Object;")
                ((char=? (string-ref k 0) #\[) (string-append "[" k))
                (else (string-append "[L" k ";"))))))
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
      ;; A dotted name must be modeled under that exact spelling (or be a known
      ;; class once its java.*/clojure.lang prefix is stripped): jch-known? falls
      ;; back to the last segment, so my.Shape read as modeled the moment ANY
      ;; class or protocol interface named Shape existed, and the extension was
      ;; filed under "Shape" where a __register-class! value reporting
      ;; "my.Shape" never looks.
      ((and (not (hashtable-ref chez-simple-name-tag type-name #f))
            (not (hashtable-ref chez-deftype-tag-set type-name #f))
            (if (dotted-name? type-name)
                (or (jch-known-exact? type-name)
                    (and (not (string=? base type-name)) (jch-known? base)))
                (jch-known? type-name)))
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
      ;; an array class name ("[[I", "[Ljava.lang.String;") is the tag an array
      ;; of that class reports (value-host-tags), whether or not it has a dot
      ((and (fx>? (string-length type-name) 1) (char=? (string-ref type-name 0) #\[)) type-name)
      (else #f))))
;; An extend/extend-type/extend-protocol registration marks the tag as an
;; extender of the protocol (recorded inside type-registry so the per-case prune
;; restores it). deftype/defrecord inline impls go through register-inline-method
;; and skip the mark: the JVM compiles inline protocol methods into the class, so
;; extenders excludes them.
(define (mark-extend! tag proto-name)
  (jolt-with-mutex rec-tbl-mu
    (let ((ti (hashtable-ref type-registry tag #f)))
      (when ti (let ((pi (hashtable-ref ti proto-name #f)))
                 (when pi (hashtable-set! pi extend-mark #t)))))))
;; A deftype named by its JVM spelling — the namespace munged, rf.def_two.R3 for
;; the tag rf.def-two.R3 — is what a library computes from (namespace-munge *ns*)
;; and how typedclojure's subtype table spells every type-rep class. The tag.
(define (deftype-tag-for-jvm-name type-name)
  (and (not (hashtable-ref chez-deftype-tag-set type-name #f))
       (let ((c (jch-registered-name type-name)))
         (and c (not (string=? c type-name))
              (hashtable-ref chez-deftype-tag-set c #f)
              c))))
;; The class the extending namespace MAPS type-name to: an :import binds the short
;; name to the class token (natives-str.ss chez-runtime-import), and its registered
;; name is the tag. This is the JVM's first question for a bare name
;; (Compiler.maybeClass reads the ns mapping before trying Class.forName). A var
;; holding a class answers too — a superset: the JVM reads only a class mapping
;; here and rejects the var.
(define (ns-mapped-class-tag type-name)
  (let ((cell (var-cell-lookup (chez-current-ns) type-name)))
    (and cell
         (let ((v (var-cell-root cell)))
           (and (jclass? v) (jclass-name v))))))
(define (register-method type-name proto-name method-name fn)
  (let* ((type-name (or (deftype-tag-for-jvm-name type-name) type-name))
         (host (canonical-host-tag type-name))
         (local (string-append (chez-current-ns) "." type-name))
         ;; a host class -> its canonical tag; a deftype defined in THIS ns -> the
         ;; local tag; a deftype the ns imported -> the class it maps; a deftype
         ;; from another ns by simple name -> its tag via the simple-name index.
         ;; Anything else is "Unable to resolve classname", as the JVM raises at
         ;; load time — never a registration under a tag no value carries, which
         ;; surfaced as a dispatch miss at the first call instead.
         (tag (cond (host host)
                    ((hashtable-ref chez-deftype-tag-set local #f) local)
                    ;; a deftype named by its FULLY-QUALIFIED name — the tag
                    ;; format itself, so it needs no ns prefix. schema does this:
                    ;; (extend-protocol Completer schema.spec.variant.VariantSpec
                    ;; …) from a third namespace. Without this the name is
                    ;; prefixed with the EXTENDING ns and the impl is filed under
                    ;; a tag no value carries.
                    ((hashtable-ref chez-deftype-tag-set type-name #f) type-name)
                    ((ns-mapped-class-tag type-name))
                    ((hashtable-ref chez-simple-name-tag type-name #f))
                    (else (throw-jvm 'IllegalArgumentException
                            (string-append "Unable to resolve classname: " type-name))))))
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
      ;; the type gains a protocol: an epoch bump like a method registration,
      ;; so a memoized satisfies? (records-dispatch.ss) re-asks
      (set! jolt-proto-epoch (fx+ jolt-proto-epoch 1))
      (let ((ti (or (hashtable-ref type-registry tag #f)
                    (let ((h (make-hashtable string-hash string=?))) (hashtable-set! type-registry tag h) h))))
        (unless (hashtable-ref ti proto-name #f)
          (hashtable-set! ti proto-name (make-hashtable string-hash string=?)))
        ;; the type DECLARES this protocol, whatever else marks the same table
        ;; later (see inline-mark): the class carries the interface from here on.
        (hashtable-set! (hashtable-ref ti proto-name #f) inline-mark #t))))
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

;; A dispatch miss is worded as the reference's emit-method-builder words it —
;; the method as a keyword, the protocol as its var, the receiver's class by
;; name, "nil" for nil — so a caller matching on the reference's message (a
;; library's own miss handling, seq.ss's hand-written IKVReduce miss) reads the
;; same string here. proto-name is defprotocol's "<ns>/<Name>" key, which is the
;; var's print form. jolt-class-name is the java host layer's (host-class.ss,
;; loaded after this file); a receiver it cannot name reports "?".
(define (protocol-miss-throw proto-name method-name obj)
  (throw-jvm (quote IllegalArgumentException)
             (string-append "No implementation of method: :" method-name
                            " of protocol: #'" proto-name
                            " found for class: "
                            (if (jolt-nil? obj)
                                "nil"
                                (let ((n (guard (e (#t #f)) (jolt-class-name obj))))
                                  (if (string? n) n "?"))))))

;; protocol-resolve's walk over a plain value's host tags, memoized per
;; (protocol method, tag list) the way satisfies? memoizes its own walk
;; (records-dispatch.ss satisfies-memo), and stamped with the same two epochs:
;; the registry's (jolt-proto-epoch, bumped by every registration, prune and
;; inline marker) and the class graph's (jch-graph-epoch, which changes what a
;; tag list holds). The method is its interned key (intern-pm-key: two eq?-refs
;; on the name objects the call site passes) and the list is keyed eq?, so only
;; a list the class graph hands out and keeps is ever stored (graph-owned-tags?)
;; -- one built per call could never hit again. Without it every call walked
;; the value's tags through the string-keyed registry, three string hashes a
;; tag, until one had the method: a protocol extended to Object cost ~5 us a
;; call on a vector and ~3.5 us on a map, which is what core.logic's unifier
;; pays on every term it walks. Read unlocked, written under jch-cache-mutex
;; with both epochs re-checked, as satisfies-memo is.
(define resolve-memo (make-eq-hashtable))
(define (resolve-by-host-tags proto-name method-name obj tags)
  (let* ((k (intern-pm-key proto-name method-name))
         (inner (hashtable-ref resolve-memo k #f))
         (e (and inner (hashtable-ref inner tags #f))))
    (if (and e
             (fx= (vector-ref e 0) jolt-proto-epoch)
             (fx= (vector-ref e 1) jch-graph-epoch))
        (vector-ref e 2)
        (let* ((pe jolt-proto-epoch)
               (ge jch-graph-epoch)
               (f (let loop ((ts tags))
                    (cond ((null? ts) #f)
                          ((find-protocol-method (car ts) proto-name method-name))
                          (else (loop (cdr ts)))))))
          (unless f (protocol-miss-throw proto-name method-name obj))
          (when (graph-owned-tags? tags)
            (jolt-with-mutex jch-cache-mutex
              (when (and (fx= pe jolt-proto-epoch) (fx= ge jch-graph-epoch))
                (let ((t (or (hashtable-ref resolve-memo k #f)
                             (let ((t (make-weak-eq-hashtable)))
                               (hashtable-set! resolve-memo k t)
                               t))))
                  (hashtable-set! t tags (vector pe ge f))))))
          f))))

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
    ((jreify? obj)
     => (lambda (_)
          (or (reify-method-ref obj method-name)
              ;; not implemented on the reify — fall back to the protocol's
              ;; extended impls over the reify's host tags (e.g. an Object/default
              ;; extension). malli reifies some protocols and leans on the default.
              (let loop ((tags (value-host-tags obj)))
                (cond ((null? tags) (protocol-miss-throw proto-name method-name obj))
                      ((find-protocol-method (car tags) proto-name method-name))
                      (else (loop (cdr tags))))))))
    (else (resolve-by-host-tags proto-name method-name obj (value-host-tags obj)))))
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
;; cache keyed on the receiver's descriptor identity: a vector of N (desc . impl)
;; pairs, most recent first, and the epoch at which they were resolved. The
;; emitted call inlines the eq? scan over the cached descs (no string hashing, no
;; table walk, no helper call after warmup); a miss (uncached desc) or a stale
;; epoch resolves through jolt-pic-add, which returns the site's next cache. The
;; epoch guard invalidates the whole cache when ANY register-protocol-method runs
;; after it was resolved, so an extend-type at runtime can't leave a cached site
;; serving a pre-extension impl. jolt-pic-n is the cache width — 4 covers the
;; megamorphic bench; a monomorphic site stays on the devirt path below.
;;
;; Every thread calling the function shares the site's cache, so a cache vector
;; is never written once published: a miss builds a new one and the site cell is
;; set! to it, a single pointer store. A reader holds whichever vector it loaded,
;; whose pairs and epoch always agree. Two threads missing at once each publish
;; their own and one entry is lost, which costs a later miss and nothing else.
;;
;; A site that has filled its cache in the current epoch is megamorphic: a miss
;; there resolves without caching, so a site seeing more types than it holds
;; keeps the types it has rather than allocating a cache per call.
(define jolt-pic-n 4)
;; #(d0 i0 d1 i1 d2 i2 d3 i3 epoch). The empty cache every site starts from: its
;; epoch, -1, never matches jolt-proto-epoch, so the first call misses.
(define jolt-pic-empty
  (let ((v (make-vector (+ (* jolt-pic-n 2) 1) #f)))
    (vector-set! v (* jolt-pic-n 2) -1)
    v))
;; A miss on desc d in cache v: the impl, when v is full and current, else the
;; site's next cache, with d and its impl in slot 0 followed by v's entries when
;; they are of the current epoch. The epoch is read before resolving, so an
;; extension that lands in between leaves the new cache stale rather than holding
;; a pre-extension impl under the new epoch.
(define (jolt-pic-add v d proto method obj)
  (let* ((e jolt-proto-epoch)
         (top (* jolt-pic-n 2))
         (current? (eqv? (vector-ref v top) e)))
    (if (and current? (vector-ref v (fx- top 2)))
        (protocol-resolve proto method obj)
        (let ((f (protocol-resolve proto method obj))
              (nv (make-vector (fx+ top 1) #f)))
          (vector-set! nv 0 d)
          (vector-set! nv 1 f)
          (when current?
            (let loop ((i 2))
              (when (fx< i top)
                (vector-set! nv i (vector-ref v (fx- i 2)))
                (loop (fx+ i 1)))))
          (vector-set! nv top e)
          ;; the vector's contents must be visible before the pointer to it is
          (when jolt-mt? (memory-order-release))
          nv))))

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
