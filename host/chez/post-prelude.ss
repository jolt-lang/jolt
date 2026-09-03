;; post-prelude overrides — loaded AFTER the assembled clojure.core
;; prelude, so these win over the overlay's own def-var!.
;;
;; A few clojure.core predicates are implemented in the overlay by inspecting a
;; tagged value's :jolt/type key (e.g. (get x :jolt/type)). That key doesn't
;; exist for native representations: a jolt char is a Scheme char, an atom is a
;; Chez record. The overlay's def-var! loads after rt.ss, so it clobbers the
;; correct native shims (predicates.ss / atoms.ss) with versions that return
;; false on every Chez value. Re-assert the native versions here.
(def-var! "clojure.core" "char?" jolt-char-pred?)
(def-var! "clojure.core" "atom?" jolt-atom?)
;; atom watches/validators: the overlay drives these via jolt.host/ref-put! on a
;; tagged table (get a :watches), which a Chez atom record is not — re-assert the
;; native versions (defined in atoms.ss), and swap!/reset! notify+validate there.
(def-var! "clojure.core" "add-watch" jolt-add-watch)
(def-var! "clojure.core" "remove-watch" jolt-remove-watch)
(def-var! "clojure.core" "set-validator!" jolt-set-validator!)
(def-var! "clojure.core" "get-validator" jolt-get-validator)
;; volatiles: a Chez volatile is a jvol record, but the overlay vreset!/vswap!/
;; volatile? drive it via jolt.host/ref-put!+get / :jolt/type (tagged-table only).
;; Override with the native versions (defined in natives-transduce.ss).
(def-var! "clojure.core" "vreset!" jolt-vreset!)
(def-var! "clojure.core" "vswap!" jolt-vswap!)
(def-var! "clojure.core" "volatile?" jolt-volatile-pred?)
;; bound?: the overlay reads (get v :root) — nil on a Chez var-cell record, so it
;; would wrongly report every var unbound. Native version (defined in vars.ss).
(def-var! "clojure.core" "bound?" jolt-bound?)
;; uuid?/random-uuid/parse-uuid/tagged-literal? are overlay (read :jolt/type or
;; build tagged tables) — re-assert the native versions (natives-misc.ss).
(def-var! "clojure.core" "uuid?" jolt-uuid-pred?)
(def-var! "clojure.core" "random-uuid" jolt-random-uuid)
(def-var! "clojure.core" "parse-uuid" jolt-parse-uuid)
(def-var! "clojure.core" "tagged-literal?" jolt-tagged-literal-pred?)
;; ns-name: the overlay reads (get ns :name) — nil on a jns namespace record.
;; Native version (defined in ns.ss) returns the namespace's name symbol.
(def-var! "clojure.core" "ns-name" jolt-ns-name)
;; concurrency: the overlay's future-done?/future-cancelled?/realized? read a
;; future-map's :cached/:cancelled keys, and promise/deliver are a non-blocking
;; atom shim. A Chez future/promise is a record, and we want JVM (blocking,
;; shared-heap) semantics — re-assert the native versions. realized?
;; wraps the overlay (which still handles delay/lazy-seq/atom) for non-futures.
(def-var! "clojure.core" "future-done?" jolt-native-future-done?)
(def-var! "clojure.core" "future-cancelled?" jolt-native-future-cancelled?)
(def-var! "clojure.core" "future?" jolt-future?)
(def-var! "clojure.core" "promise" jolt-promise-new)
(def-var! "clojure.core" "deliver" jolt-deliver)
;; agents: the overlay (50-io) is a synchronous shim (agent = atom, send applies
;; immediately). Re-assert the native async agents (per-agent serialized worker),
;; matching the JVM. await/restart-agent are new (the overlay has neither).
(def-var! "clojure.core" "agent" jolt-agent-new)
(def-var! "clojure.core" "agent?" jolt-agent?)
(def-var! "clojure.core" "send" jolt-agent-send)
(def-var! "clojure.core" "send-off" jolt-agent-send)
(def-var! "clojure.core" "await" jolt-agent-await)
(def-var! "clojure.core" "agent-error" jolt-agent-error)
(def-var! "clojure.core" "restart-agent" jolt-agent-restart)
(def-var! "clojure.core" "deref" jolt-deref)
(let ((overlay-realized? (var-deref "clojure.core" "realized?")))
  (def-var! "clojure.core" "realized?"
    (lambda (x)
      (cond
        ((or (jolt-future? x) (jolt-promise? x) (jolt-delay? x)) (jolt-conc-realized? x))
        ;; a lazy-seq carries its own realized? flag (lazy-bridge.ss). The overlay
        ;; realized? reads :jolt/type and throws on a jolt-lazyseq record.
        ((jolt-lazyseq? x) (jolt-lazyseq-realized? x))
        ;; a record/reify implementing clojure.lang.IPending answers via its
        ;; isRealized method (the JVM casts to IPending and calls it); the
        ;; result is boolean-cast like any interface-boolean return.
        ((and (jrec? x) (find-method-any-protocol (jrec-tag x) "isRealized"))
         => (lambda (m) (if (jolt-truthy? (jolt-invoke m x)) #t #f)))
        ((and (reified-methods x) (hashtable-ref (reified-methods x) "isRealized" #f))
         => (lambda (m) (if (jolt-truthy? (jolt-invoke m x)) #t #f)))
        ;; a seq cell answers by its forced flag: the rest of a realized lazy
        ;; chain is a cseq under jolt's seq model, and (realized? (rest s)) after
        ;; a next must be true like the JVM's realized LazySeq — never a throw
        ;; whose message renders the (possibly infinite) seq.
        ;; a PLAIN seq (list/cons/range — not a lazy-seq wrapper) is not an
        ;; IPending on the JVM: realized? throws.
        ((or (cseq? x) (empty-list-t? x))
         (jolt-throw (jolt-host-throwable
                      "java.lang.ClassCastException"
                      (string-append "class " (guard (e (#t "?")) (jolt-class-name x))
                                     " cannot be cast to class clojure.lang.IPending"))))
        (else (jolt-invoke overlay-realized? x))))))
;; clojure.edn/read over a reader: drain the jhost reader, then read through the
;; overlay read-string so the opts map (:readers/:default/:eof) is honored.
(def-var! "clojure.edn" "read"
  (case-lambda
    ((reader) (chez-edn-read reader))
    ((opts reader)
     (jolt-invoke (var-deref "clojure.edn" "read-string") opts
                  (if (reader-jhost? reader) (drain-reader reader) (jolt-str-render-one reader))))))
;; line-seq: a jhost reader (io/reader result) -> drain+split; a map-reader (the
;; overlay's :read-line-fn model, e.g. with-in-str) -> the overlay version.
(let ((overlay-line-seq (var-deref "clojure.core" "line-seq")))
  (def-var! "clojure.core" "line-seq"
    (lambda (rdr)
      (if (reader-jhost? rdr) (chez-line-seq rdr) (jolt-invoke overlay-line-seq rdr)))))
;; JVM-parity numeric tower. integer?/float? are on the compiler emit/inference
;; path (so they stay native) but the overlay (20-coll.clj) still carries an
;; all-flonum int?/double? (int? -> integer?, double? -> not-integer) that
;; misclassifies exact rationals (e.g. (double? 1/2) -> true). Re-assert the
;; native tower-correct versions so they win over those overlay defs. int?/double?
;; alias integer?/float?. == is value-equality. (ratio?/rational? are now correct
;; in the overlay, built on jolt.host tower tests, so they need no re-assertion.)
(def-var! "clojure.core" "integer?" jolt-integer?)
(def-var! "clojure.core" "int?" jolt-integer?)
(def-var! "clojure.core" "float?" jolt-float?)
(def-var! "clojure.core" "double?" jolt-float?)
;; ratio?/rational? now live (correctly) in the overlay, so they no longer need a
;; native re-assertion here. decimal? stays (bigdec re-binds it).
(def-var! "clojure.core" "decimal?" jolt-decimal?)
(def-var! "clojure.core" "==" jolt-num-equiv)
;; chunked-seq? is true for a vector's seq (a real chunked-seq); the overlay's
;; always-false stub loaded over the host fn, so re-assert it.
(def-var! "clojure.core" "chunked-seq?" na-chunked-seq?)
;; refs: native record (jolt-ref) not a :jolt/type-tagged map. The overlay has
;; no Clojure-level ref?/ref-set/alter/commute/ensure/loaded-libs, but establish
;; the priority so a future overlay tier can't clobber the host fns. sync/io!
;; are overlay MACROS (30-macros.clj) over the __sync-call/__txn-running? seams.
(def-var! "clojure.core" "ref" jolt-ref-new)
(def-var! "clojure.core" "ref?" jolt-ref?)
(def-var! "clojure.core" "ref-set" jolt-ref-set)
(def-var! "clojure.core" "alter" jolt-alter)
(def-var! "clojure.core" "commute" jolt-commute)
(def-var! "clojure.core" "ensure" jolt-ensure)
(def-var! "clojure.core" "__sync-call" jolt-sync)
(def-var! "clojure.core" "__txn-running?" jolt-txn-running?)
(def-var! "clojure.core" "loaded-libs" (lambda () (jolt-deref (var-deref "clojure.core" "*loaded-libs*"))))
;; re-assert refs instance? arms after records-interop.ss registers instance-check.
(register-instance-check-arm!
  (lambda (type-sym val)
    (if (and (symbol-t? type-sym) (jolt-ref? val))
        (let* ((tname (symbol-t-name type-sym))
               (tl (string-length tname))
               (dot (let loop ((i (- tl 1)))
                      (if (< i 0) -1
                          (if (char=? (string-ref tname i) #\.) i
                              (loop (- i 1))))))
               (short (if (< dot 0) tname (substring tname (+ dot 1) tl))))
          (or (string=? short "Ref")
              (string=? short "IRef")
              (string=? short "IDeref")
              ;; clojure.lang.Ref implements IFn (invoke derefs)
              (string=? short "IFn")))
        'pass)))
;; record? is a host type check — true only for a defrecord, not a bare deftype
;; (jrec-record?), matching the JVM (instance? IRecord). The overlay's
;; (some? (get x :jolt/deftype)) get-trick would invoke a sorted-map comparator.
(def-var! "clojure.core" "record?" (lambda (x) (jrec-record? x)))

;; read / read+string over a HOST reader jhost (java.io StringReader/PushbackReader):
;; the overlay's IReader protocol only covers the reify map-reader, so a (read
;; pushback-reader) — cuerdas' string interpolation — would miss. Intercept a host
;; reader; everything else (the *in* reify) delegates to the overlay.
;; The 2-arity is Clojure's (read opts stream): :eof in opts is the end-of-input
;; value, and its ABSENCE is what makes EOF throw — {:eof nil} reads nil at EOF.
(let ((ov-read (var-deref "clojure.core" "read"))
      (kw-eof (keyword #f "eof")))
  (def-var! "clojure.core" "read"
    (case-lambda
      (() (jolt-invoke ov-read))
      ((stream)
       (if (reader-jhost? stream)
           (let-values (((form found?) (host-reader-read-form stream)))
             (if found? form (jolt-throw (jolt-ex-info "EOF while reading" empty-pmap))))
           (jolt-invoke ov-read stream)))
      ((opts stream)
       (if (reader-jhost? stream)
           (let-values (((form found?) (host-reader-read-form stream)))
             (cond (found? form)
                   ((and (pmap? opts) (jolt-contains? opts kw-eof)) (jolt-get opts kw-eof))
                   (else (jolt-throw (jolt-ex-info "EOF while reading" empty-pmap)))))
           (jolt-invoke ov-read opts stream)))
      ((stream e? ev)
       (if (reader-jhost? stream)
           (let-values (((form found?) (host-reader-read-form stream)))
             (cond (found? form)
                   ((jolt-truthy? e?) (jolt-throw (jolt-ex-info "EOF while reading" empty-pmap)))
                   (else ev)))
           (jolt-invoke ov-read stream e? ev)))
      ;; the 4th argument is the JVM reader's recursive? bookkeeping, not ours
      ((stream e? ev recursive?)
       (jolt-invoke (var-deref "clojure.core" "read") stream e? ev)))))

;; read-line reads the line off whatever *in* holds, and *in* may hold a HOST
;; reader rather than the overlay's reify — tools.reader's own read-line does
;; (binding [*in* rdr] (clojure.core/read-line)) for a LineNumberingPushbackReader.
;; On the JVM that call is (.readLine *in*), so route a host reader to its own
;; readLine method; a reader that has none raises there, as it does on the JVM.
(let ((ov-read-line (var-deref "clojure.core" "read-line")))
  (def-var! "clojure.core" "read-line"
    (lambda ()
      (let ((in (var-deref "clojure.core" "*in*")))
        (if (reader-jhost? in)
            (record-method-dispatch in "readLine" jolt-nil)
            (jolt-invoke ov-read-line))))))
(let ((ov-rps (var-deref "clojure.core" "read+string"))
      (kw-eof (keyword #f "eof")))
  (def-var! "clojure.core" "read+string"
    (case-lambda
      (() (jolt-invoke ov-rps))
      ((stream) (jolt-invoke (var-deref "clojure.core" "read+string") stream #t jolt-nil))
      ((opts stream)
       (if (and (pmap? opts) (jolt-contains? opts kw-eof))
           (jolt-invoke (var-deref "clojure.core" "read+string") stream #f (jolt-get opts kw-eof))
           (jolt-invoke (var-deref "clojure.core" "read+string") stream #t jolt-nil)))
      ((stream e? ev recursive?)
       (jolt-invoke (var-deref "clojure.core" "read+string") stream e? ev))
      ((stream e? ev)
       (if (reader-jhost? stream)
           (let* ((s (drain-reader stream)) (pr (jolt-parse-next s)))
             (if (jolt-nil? pr)
                 (begin (reader-refill! stream "")
                        (if (jolt-truthy? e?) (jolt-throw (jolt-ex-info "EOF while reading" empty-pmap))
                            (jolt-vector ev "")))
                 (let ((rest (jolt-nth pr 1)))
                   (reader-refill! stream rest)
                   (jolt-vector (jolt-nth pr 0) (substring s 0 (- (string-length s) (string-length rest)))))))
           (jolt-invoke ov-rps stream e? ev))))))

;; inst? / inst-ms are host instance checks (the JVM's Inst protocol covers
;; java.util.Date, its java.sql subclasses, and java.time.Instant). The overlay's
;; tagged-:jolt/type read crashes on a sorted collection (get dispatches into the
;; comparator with an incomparable key) and misses the Instant shim.
;; A java.time.Instant is the base library's opaque value — a host tagged-table
;; carrying :jolt.time/type = :jolt.time/instant and its epoch-nanos under :nanos
;; (stdlib/jolt/time/instant.clj). Recognize it the same way host-table.ss spots a
;; sorted map, so inst?/inst-ms cover an Instant without loading anything.
(let ((kw-jt-type    (keyword "jolt.time" "type"))
      (kw-jt-instant (keyword "jolt.time" "instant"))
      (kw-nanos      (keyword #f "nanos")))
  (let ((instant? (lambda (x) (and (htable? x) (jolt=2 (jolt-ref-get x kw-jt-type) kw-jt-instant)))))
    (def-var! "clojure.core" "inst?"
      (lambda (x) (if (or (jinst? x) (instant? x)) #t #f)))
    (let ((ov-inst-ms (var-deref "clojure.core" "inst-ms")))
      (def-var! "clojure.core" "inst-ms"
        (lambda (x)
          (cond ((jinst? x) (jinst-ms x))
                ((instant? x) (quotient (exact (truncate (jolt-ref-get x kw-nanos))) 1000000))
                (else (jolt-invoke ov-inst-ms x))))))))

;; A throwable is not a collection, function, or meta carrier on the JVM. The
;; ex-info record type is NOT a pmap, so pmap?/coll?/seqable?/ifn?/associative?
;; /counted? are naturally false — no exclusion arms needed.
;;
;; Override the seed prelude's ex-info accessors (which read :jolt/type + jolt-get
;; on the old pmap backing) with record-field-based native implementations.
;; matched at call time, so remint is not required.
(def-var! "clojure.core" "ex-info-val?"
  (lambda (x) (jolt-ex-info-record? x)))
(def-var! "clojure.core" "ex-unwrap"
  (lambda (e) e))
(def-var! "clojure.core" "ex-data"
  (lambda (e) (if (jolt-ex-info-record? e) (jolt-ex-info-record-data e) jolt-nil)))
(def-var! "clojure.core" "ex-message"
  (lambda (e) (if (jolt-ex-info-record? e) (jolt-ex-info-record-message e) jolt-nil)))
(def-var! "clojure.core" "ex-cause"
  (lambda (e) (if (jolt-ex-info-record? e) (jolt-ex-info-record-cause e) jolt-nil)))
;; Throwable->map: the seed prelude version reads ex-data/ex-message/ex-cause
;; through the old var-deref chain; re-assert with the native versions.
;; seqable? additionally covers the iterable java.util shims (Iterable on the JVM).
;; The shim set lives in java/host-static-classes.ss (jhost-seqable-shim?) — do
;; not duplicate the tag list here.
(let ((prev (var-deref "clojure.core" "seqable?")))
  (def-var! "clojure.core" "seqable?"
    (lambda (x)
      (if (jhost-seqable-shim? x)
          #t
          (jolt-invoke1 prev x)))))
;; transients are IFn on the JVM (invoke = lookup); the queue is a full
;; sequential persistent collection; a reader-conditional is a record, not a
;; map — pmap?/coll?/seqable?/ifn?/associative? are naturally false.
(let ((prev (var-deref "clojure.core" "ifn?")))
  (def-var! "clojure.core" "ifn?"
    (lambda (x) (if (or (jolt-transient? x) (jolt-ref? x)) #t (jolt-invoke1 prev x)))))
(for-each
  (lambda (nm)
    (let ((prev (var-deref "clojure.core" nm)))
      (def-var! "clojure.core" nm
        (lambda (x) (if (jolt-queue? x) #t (jolt-invoke1 prev x))))))
  '("coll?" "sequential?" "seqable?"))
;; reader-conditional? override: the seed prelude checks (get x :jolt/type);
;; a record has no :jolt/type key — override with the native predicate.
(def-var! "clojure.core" "reader-conditional?"
  (lambda (x) (jolt-reader-conditional-record? x)))

;; a deftype implementing a persistent-collection interface answers the
;; corresponding predicate, like the JVM's instance?-backed map?/vector?/set?
;; (a bare deftype stays out of all of them; records already answer via
;; jrec-record?).
(define (jrec-iface-pred? x iface)
  (and (jrec? x) (not (jrec-record? x))
       (eq? #t (instance-check (jolt-symbol #f iface) x))))
(for-each
  (lambda (p)
    (let ((nm (car p)) (iface (cdr p)))
      (let ((prev (var-deref "clojure.core" nm)))
        (def-var! "clojure.core" nm
          (lambda (x) (if (jrec-iface-pred? x iface) #t (jolt-invoke1 prev x)))))))
  '(("coll?" . "clojure.lang.IPersistentCollection")
    ("map?" . "clojure.lang.IPersistentMap")
    ("vector?" . "clojure.lang.IPersistentVector")
    ("set?" . "clojure.lang.IPersistentSet")
    ("associative?" . "clojure.lang.Associative")
    ("sequential?" . "clojure.lang.Sequential")))

;; counted?: Clojure's is (instance? clojure.lang.Counted x), and jolt now models
;; that interface, so the ONE statement of which things count in O(1) is the class
;; graph (java/class-hierarchy.ss) — read here through cseq-counted?, which derives
;; from it per seq flavor. The overlay's hand-rolled (or (vector? x) (map? x) (set?
;; x) (list? x)) had drifted from the graph in both directions: it answered false
;; for a vector's own seq, which jolt-count answers without walking (pvec-count
;; less the cell's index), and false for a queue, which the JVM counts too.
;;
;; A native closure for the same reason sequential? below is one: the overlay
;; spelling routes every call through four more overlay fn invocations, and
;; counted? sits on core's own count/into fast paths.
(let ((prev (var-deref "clojure.core" "counted?")))
  (def-var! "clojure.core" "counted?"
    (lambda (x)
      (cond ((pvec? x) #t)              ; vectors, subvec views, map entries
            ((pmap? x) #t)
            ((pset? x) #t)
            ((cseq? x) (cseq-counted? x))
            ((empty-list-t? x) #t)
            ((jolt-queue? x) #t)
            ;; a String is NOT Counted on the JVM (count goes through
            ;; CharSequence.length, not an O(1) collection count), and a lazy seq
            ;; cannot be — its length is unknown until it is realized.
            ((or (string? x) (jolt-lazyseq? x) (jolt-nil? x) (number? x)
                 (keyword-t? x) (symbol-t? x) (boolean? x) (char? x) (procedure? x))
             #f)
            ;; sorted colls, records and deftypes declaring Counted stay with the
            ;; overlay, which answers them through map?/set?/instance?.
            (else (jolt-invoke1 prev x))))))

;; ident?: the overlay's (or (keyword? x) (symbol? x)) pays two overlay var
;; calls (~130ns) for two native predicates; dispatch-heavy code (honeysql's
;; format-selectable-dsl) calls it per branch. One native closure.
(def-var! "clojure.core" "ident?" (lambda (x) (or (keyword-t? x) (symbol-t? x))))
;; sequential?: the overlay's (or (vector? x) (seq? x)) routes EVERY call through
;; two overlay fn invocations (var deref + jolt-invoke each, ~250ns on a pmap —
;; and map destructuring expands to a sequential? test per binding form, so
;; honeysql's format path paid it per clause). One native closure: the fast
;; native types answer directly; concrete non-sequential natives answer #f
;; without touching the overlay; only exotic values fall through to it.
(let ((prev (var-deref "clojure.core" "sequential?")))
  (def-var! "clojure.core" "sequential?"
    (lambda (x)
      (cond ((pvec? x) #t)
            ((cseq? x) #t)
            ((jolt-lazyseq? x) #t)
            ((empty-list-t? x) #t)
            ((jolt-queue? x) #t)
            ((jrec-iface-pred? x "clojure.lang.Sequential") #t)
            ((or (pmap? x) (pset? x) (string? x) (symbol-t? x) (keyword-t? x)
                 (number? x) (char? x) (boolean? x) (jolt-nil? x) (procedure? x))
             #f)
            (else (jolt-invoke1 prev x))))))

;; --- value-position natives are nameable ---------------------------------------
;; A core fn used as a VALUE compiles to the runtime's Scheme procedure, not to
;; the var's root, and the image writes a procedure as its var NAME. def-var!
;; records that name -- but only for the procedure it was handed, and seq/get/nth
;; are set!-EXTENDED afterwards (lazy-bridge for a lazy seq, records-coll for a
;; deftype, natives-array for an array, nio-file for a Path). The extension is a
;; new procedure nothing named, so (tree-seq vector? seq coll) refused to travel
;; while 37 of 40 core fns sampled were fine.
;;
;; Registered HERE rather than beside each set!, because "the last extension" is
;; not a place any one file can know it is: three files extend jolt-nth. This
;; runs after every one of them. Same fix rt.ss already applies to the comparison
;; chain singletons (jolt-lt/gt/le/ge), for the same reason (jolt-6cwk).
;;
;; `make coreproc` fails if any core fn goes unnameable, so a new extension that
;; forgets is caught rather than discovered by an image that will not write.
(for-each (lambda (p) (register-proc-name! (cdr p) "clojure.core" (car p)))
          (list (cons "seq" jolt-seq)
                  ;; the op registry's alength (natives-array.ss): value-position
                  ;; alength compiles to it, so an image needs its name
                  (cons "alength" jolt-alength)
                  (cons "get" jolt-get)
                  (cons "nth" jolt-nth)
                  (cons "sequential?" jolt-sequential?)
                  (cons "seq?" jolt-seq?)
                  (cons "peek" jolt-peek)
                  (cons "pop" jolt-pop)
                  ;; value position compiles to the checked numeric layer's own
                  ;; procedures, while the var roots went elsewhere -- the same
                  ;; split as the comparison chain registered in rt.ss
                  (cons "min" jolt-min)
                  (cons "max" jolt-max)
                  (cons "mod" jolt-mod)
                  (cons "rem" jolt-rem)
                  (cons "quot" jolt-quot)
                  (cons "bit-and" jolt-bit-and*)
                  (cons "bit-or" jolt-bit-or*)
                  (cons "bit-xor" jolt-bit-xor*)
                  (cons "some?" jolt-some?-fn)
                  ;; protocol dispatch entry points: internal, but they are var
                  ;; roots and a value built from one is as unwritable as any
                  (cons "protocol-dispatch1" protocol-dispatch1)
                  (cons "protocol-dispatch2" protocol-dispatch2)
                (cons "protocol-dispatch3" protocol-dispatch3)))
