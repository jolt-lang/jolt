;;   Portions of this file are from ClojureScript's src/main/cljs/cljs/core.cljs,
;;   Copyright (c) Rich Hickey. All rights reserved.
;;   The use and distribution terms for that software are covered by the
;;   Eclipse Public License 1.0 (licenses/EPL-1.0.txt).

;; clojure.core — sorted collections tier.
;;
;; A sorted-map / sorted-set is a tagged host table
;;   {:jolt/type :jolt/sorted-map|:jolt/sorted-set
;;    :tree      NODE | nil      ; a persistent B+tree, comparator-ordered
;;    :cnt       N               ; element count (O(1))
;;    :cmp       FN-or-nil       ; 3-way comparator; nil = natural order (compare)
;;    :ops       {op-kw fn}}     ; this tier's implementations, attached to the value
;;
;; The tree is a persistent B+tree kept by the host (host-table.ss, "the
;; B+tree"), where a node is a Scheme vector of up to 16 entries (a leaf) or 32
;; children. assoc / get / dissoc / contains are O(log n).
;;
;; A sorted-SET stores its elements as keys with a nil value; its ops project the
;; key. ALL the semantics live here in Clojure; the host keeps only its
;; dispatch branches (conj/assoc/seq/count/…), each a one-line call through
;; the value's own :ops table, so the ops travel WITH the value (correct across
;; contexts, forks, and AOT images). The two hot lookups, get and contains?,
;; read the tree directly (host-table.ss), answering what :get / :contains do. The wrapper is the host's sorted record
;; (host-table.ss), minted by jolt.host/make-sorted and read field by field
;; through jolt.host/sorted-tree, -cnt, -cmp, -cmp-fn and -ops. Plain `get` on a
;; sorted coll IS the comparator lookup — it dispatches back into these ops, so
;; reading a field with it would recurse forever.

;; Clojure's fn->comparator: a comparator fn may return a number (3-way) or a
;; boolean less-than predicate, and may be a reify/deftype Comparator, which is
;; not IFn. jolt.host/comparator->cmp is that reading as a host procedure, the
;; same one a java.util.TreeMap holds, so the tree calls it directly.
(defn- fn->cmp [f] (jolt.host/comparator->cmp f))

(defn- the-cmp [sc] (or (jolt.host/sorted-cmp sc) compare))

;; --- the tree ----------------------------------------------------------------
;; The tree is the host's (host-table.ss, "the B+tree"): its nodes and the
;; insert/replace/delete walks over them. What stays here is what each op does
;; with the result. The
;; comparator handed to them is the collection's own :cmp, nil for natural
;; order (Util.compare, as PersistentTreeMap's default comparator is).
(defn- tree-cmp [sc] (jolt.host/sorted-cmp sc))

(defn- make-sorted [tag tree cnt cmp cmp-fn ops]
  (jolt.host/make-sorted tag tree cnt cmp cmp-fn ops))

;; --- sorted-map ops ---------------------------------------------------------
;; The tree answers with values (jolt.host/rbt-get, -entry, -assoc, ...), never
;; with its nodes, so its layout is the host's alone.

(defn- contains-key? [sc k] (jolt.host/rbt-contains? (jolt.host/sorted-tree sc) (tree-cmp sc) k))

(defn- sm-get [sm k not-found]
  (jolt.host/rbt-get (jolt.host/sorted-tree sm) (tree-cmp sm) k not-found))

;; Associative.entryAt — the entry carries the stored key, which is what
;; clojure.core/find hands back (its metadata is read).
(defn- sm-entry-at [sm k]
  (jolt.host/rbt-entry (jolt.host/sorted-tree sm) (tree-cmp sm) k))

;; A present key keeps its first-inserted key and takes the new value, unless the
;; value is the IDENTICAL object, which leaves the map as it was (found.val() ==
;; val): `=` kept [1] where the JVM stores the '(1) it was handed.
(defn- sm-assoc-1 [sm k v]
  (let [tree (jolt.host/sorted-tree sm)
        r (jolt.host/rbt-assoc tree (tree-cmp sm) k v)]
    (if (jolt.host/rbt-present? r)
      (let [t (jolt.host/rbt-present-tree r)]
        (if (identical? t tree) sm (jolt.host/sorted-with sm t (jolt.host/sorted-cnt sm))))
      (jolt.host/sorted-with sm r (inc (jolt.host/sorted-cnt sm))))))

;; Many entries at once (a constructor's arguments, a multi-arg assoc/conj, an
;; into): KVS is k0 v0 k1 v1 ... (or, KEYS-ONLY, a set's elements), handed to the
;; tree in one call, which sorts and bulk-loads them when the coll is small
;; against the batch. Same result as one assoc after another, the SAME coll when
;; nothing changes.
(defn- assoc-all [sc kvs keys-only]
  (let [tree (jolt.host/sorted-tree sc)
        r (jolt.host/rbt-assoc-all tree (tree-cmp sc) (jolt.host/sorted-cnt sc) kvs keys-only)
        t (key r)]
    (if (identical? t tree) sc (jolt.host/sorted-with sc t (val r)))))

;; under this many entries a batch costs more to gather than it saves
(def ^:private batch-min 8)

(defn- small? [xs n] (and (counted? xs) (< (count xs) n)))

(defn- sm-assoc-many [sm kvs]
  (let [n (count kvs)]
    (when (odd? n)
      (throw (IllegalArgumentException. (str "No value supplied for key: " (nth kvs (dec n))))))
    (if (< n (* 2 batch-min))
      (loop [m sm i 0]
        (if (< i n) (recur (sm-assoc-1 m (nth kvs i) (nth kvs (inc i))) (+ i 2)) m))
      (assoc-all sm kvs false))))

(defn- sm-dissoc-1 [sm k]
  (let [tree (jolt.host/sorted-tree sm)
        t (jolt.host/rbt-without tree (tree-cmp sm) k)]
    (if (identical? t tree) sm (jolt.host/sorted-with sm t (dec (jolt.host/sorted-cnt sm))))))

(defn- sm-dissoc-many [sm ks] (reduce sm-dissoc-1 sm ks))

;; conj on a map, as APersistentMap.cons: a [k v] pair (2-vector / map-entry),
;; then anything that seqs into map entries (a map, a seq of entries; nil seqs to
;; nothing). A vector of any other length is the JVM's named refusal, a value
;; that does not seq is seq's own IllegalArgumentException, and a seq element
;; that is not an entry fails the Map$Entry cast.
(defn- conj-entries [acc x add]
  (if (vector? x)
    (throw (IllegalArgumentException. "Vector arg to map conj must be a pair"))
    (reduce (fn [acc e]
              ;; a map entry (MapEntry or a TreeMap's), never a plain pair:
              ;; the JVM casts each element to java.util.Map$Entry
              (cond (map-entry? e) (add acc (key e) (val e))
                    :else (entry-cast-throw e)))
            acc (seq x))))

(defn- sm-conj-1 [sm x]
  (cond
    (and (vector? x) (= 2 (count x))) (sm-assoc-1 sm (nth x 0) (nth x 1))
    (map-entry? x) (sm-assoc-1 sm (key x) (val x))
    :else (conj-entries sm x sm-assoc-1)))

(defn- kvs-add [kvs k v] (conj! (conj! kvs k) v))

;; sm-conj-1's cases, onto a flat k v buffer
(defn- conj-kvs [kvs x]
  (cond
    (and (vector? x) (= 2 (count x))) (kvs-add kvs (nth x 0) (nth x 1))
    (map-entry? x) (kvs-add kvs (key x) (val x))
    :else (conj-entries kvs x kvs-add)))

(defn- sm-conj-many [sm xs]
  (if (small? xs batch-min)
    (reduce sm-conj-1 sm xs)
    (assoc-all sm (persistent! (reduce conj-kvs (transient []) xs)) false)))

;; --- sorted-set ops (elements stored as keys, nil value) --------------------
(defn- ss-get [ss x not-found]
  (jolt.host/rbt-get-key (jolt.host/sorted-tree ss) (tree-cmp ss) x not-found))

;; One walk. PersistentTreeSet.cons walks twice (contains, then add), so a
;; counting comparator sees fewer calls here than on the JVM. A present element
;; has the identical nil value, so rbt-assoc leaves the tree as it was.
(defn- ss-conj-1 [ss x]
  (let [r (jolt.host/rbt-assoc (jolt.host/sorted-tree ss) (tree-cmp ss) x nil)]
    (if (jolt.host/rbt-present? r)
      ss
      (jolt.host/sorted-with ss r (inc (jolt.host/sorted-cnt ss))))))

(defn- ss-conj-many [ss xs]
  (if (small? xs batch-min) (reduce ss-conj-1 ss xs) (assoc-all ss xs true)))

(defn- ss-disj-1 [ss x]
  (let [tree (jolt.host/sorted-tree ss)
        t (jolt.host/rbt-without tree (tree-cmp ss) x)]
    (if (identical? t tree) ss (jolt.host/sorted-with ss t (dec (jolt.host/sorted-cnt ss))))))

(defn- ss-disj-many [ss xs] (reduce ss-disj-1 ss xs))

;; --- the ops tables the host dispatches through ------------------------

(def ^:private sm-ops
  {:count    (fn [sm] (jolt.host/sorted-cnt sm))
   :seq      (fn [sm] (jolt.host/sorted-seq (jolt.host/sorted-tree sm) true 0))
   :rseq     (fn [sm] (jolt.host/sorted-seq (jolt.host/sorted-tree sm) false 0))
   :first    (fn [sm] (when-let [t (jolt.host/sorted-tree sm)] (jolt.host/rbt-min-entry t)))
   :get      sm-get
   :entry-at sm-entry-at
   :contains contains-key?
   :assoc    sm-assoc-many
   :assoc1   sm-assoc-1
   :dissoc   sm-dissoc-many
   :dissoc1  sm-dissoc-1
   :conj     sm-conj-many
   :conj1    sm-conj-1
   ;; into's batch, built by the host one element at a time: :conj-kvs folds an
   ;; element onto a transient k v buffer, :assoc-all takes the finished buffer
   :conj-kvs  conj-kvs
   :assoc-all (fn [sm kvs] (assoc-all sm kvs false))
   :empty    (fn [sm] (jolt.host/sorted-with sm nil 0))})

(def ^:private ss-ops
  {:count    (fn [ss] (jolt.host/sorted-cnt ss))
   :seq      (fn [ss] (jolt.host/sorted-seq (jolt.host/sorted-tree ss) true 1))
   :rseq     (fn [ss] (jolt.host/sorted-seq (jolt.host/sorted-tree ss) false 1))
   :first    (fn [ss] (when-let [t (jolt.host/sorted-tree ss)] (jolt.host/rbt-min-key t)))
   :get      ss-get
   :contains contains-key?
   :conj     ss-conj-many
   :conj1    ss-conj-1
   :conj-kvs  (fn [acc x] (conj! acc x))
   :assoc-all (fn [ss xs] (assoc-all ss xs true))
   :disj     ss-disj-many
   :disj1    ss-disj-1
   :empty    (fn [ss] (jolt.host/sorted-with ss nil 0))})

;; --- constructors + predicates -----------------------------------------------

(defn sorted-map [& kvs]
  (sm-assoc-many (make-sorted :jolt/sorted-map nil 0 nil nil sm-ops) (vec kvs)))

(defn sorted-map-by [comparator & kvs]
  (sm-assoc-many (make-sorted :jolt/sorted-map nil 0 (fn->cmp comparator) comparator sm-ops) (vec kvs)))

(defn sorted-set [& xs]
  (ss-conj-many (make-sorted :jolt/sorted-set nil 0 nil nil ss-ops) (vec xs)))

(defn sorted-set-by [comparator & xs]
  (ss-conj-many (make-sorted :jolt/sorted-set nil 0 (fn->cmp comparator) comparator ss-ops) (vec xs)))

(defn sorted-map? [x] (= :map (jolt.host/sorted-kind x)))
(defn sorted-set? [x] (= :set (jolt.host/sorted-kind x)))
(defn sorted? [x] (or (sorted-map? x) (sorted-set? x)))

;; --- subseq / rsubseq ---------------------------------------------------------
;; test is one of < <= > >= applied Clojure-style to the comparator result:
;; keep entries whose (cmp entry-key k) satisfies (test _ 0). Returns a seq or
;; nil, like Clojure.
;;
;; clojure.core's own definitions, over Sorted.seqFrom: jolt.host/sorted-seq-from
;; seeks the bound in O(log n) and hands back the same lazy stack walk seq uses,
;; so walking on costs amortized O(1) per entry in either direction.

(defn- sc-mode [sc] (if (sorted-map? sc) 0 1))

;; clojure.core/mk-bound-fn
(defn- mk-bound-fn [sc test k]
  (let [cmp (the-cmp sc) map? (sorted-map? sc)]
    (fn [e] (test (cmp (if map? (key e) e) k) 0))))

(defn- seq-from [sc k asc?]
  (jolt.host/sorted-seq-from (jolt.host/sorted-tree sc) asc? (sc-mode sc) (the-cmp sc) k))

(defn- sc-seq [sc asc?]
  (jolt.host/sorted-seq (jolt.host/sorted-tree sc) asc? (sc-mode sc)))

(defn- assert-sorted [sc]
  (when-not (sorted? sc)
    (throw (ClassCastException. (str "subseq/rsubseq require a sorted collection: " (class sc))))))

(defn subseq
  ([sc test k]
   (assert-sorted sc)
   (let [include (mk-bound-fn sc test k)]
     (if (or (identical? test >) (identical? test >=))
       (when-let [s (seq-from sc k true)]
         (if (include (first s)) s (next s)))
       (take-while include (sc-seq sc true)))))
  ([sc start-test start-k end-test end-k]
   (assert-sorted sc)
   (when-let [s (seq-from sc start-k true)]
     (take-while (mk-bound-fn sc end-test end-k)
                 (if ((mk-bound-fn sc start-test start-k) (first s)) s (next s))))))

(defn rsubseq
  ([sc test k]
   (assert-sorted sc)
   (let [include (mk-bound-fn sc test k)]
     (if (or (identical? test <) (identical? test <=))
       (when-let [s (seq-from sc k false)]
         (if (include (first s)) s (next s)))
       (take-while include (sc-seq sc false)))))
  ([sc start-test start-k end-test end-k]
   (assert-sorted sc)
   (when-let [s (seq-from sc end-k false)]
     (take-while (mk-bound-fn sc start-test start-k)
                 (if ((mk-bound-fn sc end-test end-k) (first s)) s (next s))))))
