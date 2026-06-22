;; post-prelude overrides (jolt-9ziu) — loaded AFTER the assembled clojure.core
;; prelude, so these win over the overlay's own def-var!.
;;
;; A few clojure.core predicates are implemented in the overlay by inspecting a
;; Janet-host tagged value's :jolt/type key (e.g. (get x :jolt/type)). That key
;; doesn't exist for Chez-native representations: a jolt char is a Scheme char,
;; an atom is a Chez record. The overlay's def-var! loads after rt.ss, so it
;; clobbers the correct native shims (predicates.ss / atoms.ss) with versions
;; that return false on every Chez value. Re-assert the native versions here.
;;
;; (Long-term these predicates want a host-neutral implementation that calls a
;; host primitive instead of reading :jolt/type; until then this is the Chez-host
;; override.)
(def-var! "clojure.core" "char?" jolt-char-pred?)
(def-var! "clojure.core" "atom?" jolt-atom?)
;; atom watches/validators: the overlay drives these via jolt.host/ref-put! on a
;; Janet table (get a :watches), which a Chez atom record is not — re-assert the
;; native versions (defined in atoms.ss), and swap!/reset! notify+validate there.
(def-var! "clojure.core" "add-watch" jolt-add-watch)
(def-var! "clojure.core" "remove-watch" jolt-remove-watch)
(def-var! "clojure.core" "set-validator!" jolt-set-validator!)
(def-var! "clojure.core" "get-validator" jolt-get-validator)
;; volatiles: a Chez volatile is a jvol record, but the overlay vreset!/vswap!/
;; volatile? drive it via jolt.host/ref-put!+get / :jolt/type (tagged-table only).
;; Override with the native versions (defined in natives-xform.ss).
(def-var! "clojure.core" "vreset!" jolt-vreset!)
(def-var! "clojure.core" "vswap!" jolt-vswap!)
(def-var! "clojure.core" "volatile?" jolt-volatile-pred?)
;; bound?: the overlay reads (get v :root) — nil on a Chez var-cell record, so it
;; would wrongly report every var unbound. Native version (defined in vars.ss).
(def-var! "clojure.core" "bound?" jolt-bound?)
;; uuid?/random-uuid/parse-uuid/tagged-literal? are overlay (read :jolt/type or
;; build tagged tables) — re-assert the native versions (defined in natives-misc.ss).
(def-var! "clojure.core" "uuid?" jolt-uuid-pred?)
(def-var! "clojure.core" "random-uuid" jolt-random-uuid)
(def-var! "clojure.core" "parse-uuid" jolt-parse-uuid)
(def-var! "clojure.core" "tagged-literal?" jolt-tagged-literal-pred?)
;; ns-name: the overlay reads (get ns :name) — nil on a jns namespace record.
;; Native version (defined in ns.ss) returns the namespace's name symbol.
(def-var! "clojure.core" "ns-name" jolt-ns-name)
;; concurrency (jolt-byjr): the overlay's future-done?/future-cancelled?/realized?
;; read a Janet future-map's :cached/:cancelled keys, and promise/deliver are a
;; non-blocking atom shim. A Chez future/promise is a record, and we want JVM
;; (blocking, shared-heap) semantics — re-assert the native versions. realized?
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
        (else (jolt-invoke overlay-realized? x))))))
;; clojure.edn/read over a reader: the overlay edn.clj drain-reader uses janet/type;
;; the native Chez version (io.ss) drains the jhost reader instead (jolt-uicd/jolt-7t3l).
(def-var! "clojure.edn" "read"
  (case-lambda
    ((reader) (chez-edn-read reader))
    ((opts reader) (chez-edn-read reader))))
;; line-seq: a jhost reader (io/reader result) -> drain+split; a map-reader (the
;; overlay's :read-line-fn model, e.g. with-in-str) -> the overlay version.
(let ((overlay-line-seq (var-deref "clojure.core" "line-seq")))
  (def-var! "clojure.core" "line-seq"
    (lambda (rdr)
      (if (reader-jhost? rdr) (chez-line-seq rdr) (jolt-invoke overlay-line-seq rdr)))))
;; JVM-parity numeric tower (jolt-n6al): the overlay (20-coll.clj) carries an
;; all-flonum number-predicate web with no Ratio concept (ratio? -> false,
;; double? -> not-integer, float? -> double?, rational? -> int?), which
;; misclassifies exact rationals on the Chez tower (e.g. (double? 1/2) -> true).
;; Re-assert the native tower-correct versions (predicates.ss) so they win over
;; the overlay defs. int?/double? alias integer?/float?. == is value-equality.
(def-var! "clojure.core" "integer?" jolt-integer?)
(def-var! "clojure.core" "int?" jolt-integer?)
(def-var! "clojure.core" "float?" jolt-float?)
(def-var! "clojure.core" "double?" jolt-float?)
(def-var! "clojure.core" "ratio?" jolt-ratio?)
(def-var! "clojure.core" "rational?" jolt-rational?)
(def-var! "clojure.core" "decimal?" jolt-decimal?)
(def-var! "clojure.core" "==" jolt-num-equiv)
;; chunked-seq? is true for a vector's seq (a real chunked-seq); the overlay's
;; always-false stub loaded over the host fn, so re-assert it (jolt-hs5q).
(def-var! "clojure.core" "chunked-seq?" na-chunked-seq?)
