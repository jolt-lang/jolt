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
              (string=? short "IDeref")))
        'pass)))
;; record? is a host type check — true only for a defrecord, not a bare deftype
;; (jrec-record?), matching the JVM (instance? IRecord). The overlay's
;; (some? (get x :jolt/deftype)) get-trick would invoke a sorted-map comparator.
(def-var! "clojure.core" "record?" (lambda (x) (jrec-record? x)))

;; read / read+string over a HOST reader jhost (java.io StringReader/PushbackReader):
;; the overlay's IReader protocol only covers the reify map-reader, so a (read
;; pushback-reader) — cuerdas' string interpolation — would miss. Intercept a host
;; reader; everything else (the *in* reify) delegates to the overlay.
(let ((ov-read (var-deref "clojure.core" "read")))
  (def-var! "clojure.core" "read"
    (case-lambda
      (() (jolt-invoke ov-read))
      ((stream)
       (if (reader-jhost? stream)
           (let-values (((form found?) (host-reader-read-form stream)))
             (if found? form (jolt-throw (jolt-ex-info "EOF while reading" empty-pmap))))
           (jolt-invoke ov-read stream)))
      ((stream e? ev)
       (if (reader-jhost? stream)
           (let-values (((form found?) (host-reader-read-form stream)))
             (cond (found? form)
                   ((jolt-truthy? e?) (jolt-throw (jolt-ex-info "EOF while reading" empty-pmap)))
                   (else ev)))
           (jolt-invoke ov-read stream e? ev))))))
(let ((ov-rps (var-deref "clojure.core" "read+string")))
  (def-var! "clojure.core" "read+string"
    (case-lambda
      (() (jolt-invoke ov-rps))
      ((stream) (jolt-invoke (var-deref "clojure.core" "read+string") stream #t jolt-nil))
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
