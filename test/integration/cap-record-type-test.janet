# A record type keeps its :type tag through depth-capping (jolt-3ko follow-up).
# cap (jolt.passes.types) truncates a deep type's field VALUES to :any so the
# inter-procedural fixpoint stays finite, but the record :type tag is identity,
# independent of field depth — so it must survive. Before this fix cap rebuilt
# the struct via mk-struct and dropped :type, degrading a record stored in a
# deep container to a plain struct: devirtualization (jolt-41m) and record?
# folding silently stopped firing on it. This drives whole-program inference
# over a vector-of-records and asserts (a) the element keeps REC identity and
# (b) a protocol call on an element devirtualizes (the inference annotates the
# call node with :devirt-type, which it can't do without the receiver's :type).
(import ../../src/jolt/api :as api)
(import ../../src/jolt/backend :as backend)
(import ../../src/jolt/types :as types)

(print "Record :type survives capping (jolt-3ko)...")

(def dir (string (or (os/getenv "TMPDIR") "/tmp") "/jolt-cap-rec"))
(os/mkdir dir)
(spit (string dir "/cr.clj")
      (string
       "(ns cr)\n"
       "(defrecord V3 [r g b])\n"                                    # nested record -> forces a deep type
       "(defprotocol Shape (area [s]))\n"
       "(defrecord Box [^V3 lo ^V3 hi]\n"
       "  Shape\n"
       "  (area [s] (+ (:r (:hi s)) (:r (:lo s)))))\n"
       "(defn total [coll] (reduce (fn [acc x] (+ acc (area x))) 0.0 coll))\n"
       "(defn drive [n]\n"
       "  (let [world [(->Box (->V3 1.0 2.0 3.0) (->V3 4.0 5.0 6.0))\n"
       "               (->Box (->V3 0.0 0.0 0.0) (->V3 2.0 2.0 2.0))]]\n"
       "    (loop [i 0 acc 0.0] (if (< i n) (recur (inc i) (+ acc (total world))) acc))))\n"))

(os/setenv "JOLT_DIRECT_LINK" "1")
(os/setenv "JOLT_WHOLE_PROGRAM" "1")
(os/setenv "JOLT_PATH" dir)
(def ctx (api/init {:compile? true}))
(api/eval-string ctx "(require '[cr])")
(def report (backend/infer-program! ctx))

# (a) total's collection param keeps its record element identity through cap
(def coll-t (get (get report "cr/total") 0))
(def elem-t (and coll-t (get coll-t :vec)))
(assert (and elem-t (= "cr.Box" (get elem-t :type)))
        (string "vec element keeps record :type through cap (got " (string/format "%p" elem-t) ")"))

# (b) the protocol call on an element devirtualizes — the inference can only
# annotate :devirt-type when it knows the receiver's record :type.
(def pns (types/ctx-find-ns ctx "jolt.passes"))
(def reinfer (types/var-get (types/ns-find pns "reinfer-def")))
(def cell (get ((types/ctx-find-ns ctx "cr") :mappings) "total"))
(def reinferred (string/format "%p" (reinfer (get cell :infer-ir) @{"coll" coll-t})))
(assert (not (nil? (string/find ":devirt-type" reinferred)))
        "protocol call on a collection-read record devirtualizes (:devirt-type present)")

# correctness: the program still computes the right answer
(assert (= 5.0
           (api/eval-string ctx "(cr/total [(cr/->Box (cr/->V3 1.0 0 0) (cr/->V3 4.0 0 0)) (cr/->Box (cr/->V3 0 0 0) (cr/->V3 0 0 0))])"))
        "devirtualized record-in-collection computes correctly")

(print "Record :type survives capping (jolt-3ko) passed!")
