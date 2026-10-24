;;   Portions of this file are from ClojureScript's src/main/cljs/cljs/core.cljs,
;;   Copyright (c) Rich Hickey. All rights reserved.
;;   The use and distribution terms for that software are covered by the
;;   Eclipse Public License 1.0 (licenses/EPL-1.0.txt).

;; clojure.core — sorted collections tier.
;;
;; A sorted-map / sorted-set is a tagged host table
;;   {:jolt/type :jolt/sorted-map|:jolt/sorted-set
;;    :tree      RB-NODE | nil   ; a red-black tree, comparator-ordered
;;    :cnt       N               ; element count (O(1))
;;    :cmp       FN-or-nil       ; 3-way comparator; nil = natural order (compare)
;;    :ops       {op-kw fn}}     ; this tier's implementations, attached to the value
;;
;; The tree is a red-black tree — Rich Hickey's algorithm, ported from the
;; ClojureScript PersistentTreeMap — kept by the host (host-table.ss), where a
;; node is a Scheme vector. assoc / get / dissoc / contains are O(log n).
;;
;; A sorted-SET stores its elements as keys with a nil value; its ops project the
;; key. ALL the semantics live here in Clojure; the host keeps only its
;; dispatch branches (conj/assoc/get/seq/count/…), each a one-line call through
;; the value's own :ops table, so the ops travel WITH the value (correct across
;; contexts, forks, and AOT images). The wrapper is the host's sorted record
;; (host-table.ss), minted by jolt.host/make-sorted and read field by field
;; through jolt.host/sorted-tree, -cnt, -cmp, -cmp-fn and -ops. Plain `get` on a
;; sorted coll IS the comparator lookup — it dispatches back into these ops, so
;; reading a field with it would recurse forever.

;; Clojure's fn->comparator: a comparator fn may return a number (3-way) or a
;; boolean less-than predicate. The value may also be a reify/deftype Comparator,
;; which is not IFn — __comparator-fn is the shared seam that turns either into a
;; 2-arg compare fn.
(defn- fn->cmp [f]
  (let [f (__comparator-fn f)]
    (fn [a b]
      (let [r (f a b)]
        (if (number? r)
          r
          (if r -1 (if (f b a) 1 0)))))))

(defn- the-cmp [sc] (or (jolt.host/sorted-cmp sc) compare))

;; --- the red-black tree -----------------------------------------------------
;; The tree is the host's (host-table.ss, "the red-black tree"): Scheme-vector
;; nodes, whose fields read without a dispatch, and the insert/replace/delete
;; walks over them. What stays here is what each op does with the result. The
;; comparator handed to them is the collection's own :cmp, nil for natural
;; order (Util.compare, as PersistentTreeMap's default comparator is).
(defn- tree-cmp [sc] (jolt.host/sorted-cmp sc))

(defn- make-sorted [tag tree cnt cmp cmp-fn ops]
  (jolt.host/make-sorted tag tree cnt cmp cmp-fn ops))

;; --- sorted-map ops ---------------------------------------------------------
;; a real map-entry (map-entry? true), so key/val/seq destructuring work like a
;; regular map's entries.
(defn- map-entry [t] (jolt.host/map-entry (jolt.host/rbt-key t) (jolt.host/rbt-val t)))

(defn- lookup [sc k] (jolt.host/rbt-lookup (jolt.host/sorted-tree sc) (tree-cmp sc) k))

(defn- sm-get [sm k not-found]
  (let [n (lookup sm k)]
    (if (nil? n) not-found (jolt.host/rbt-val n))))

;; Associative.entryAt — the entry carries the node's own key, which is what
;; clojure.core/find hands back (its metadata is read).
(defn- sm-entry-at [sm k]
  (let [n (lookup sm k)]
    (when-not (nil? n) (map-entry n))))

;; One walk for a fresh key (rbt-insert reports a present one instead of
;; inserting), two for a replace, as PersistentTreeMap.assoc walks. The value is
;; kept only when it is the IDENTICAL object, as there (found.val() == val):
;; `=` kept [1] where the JVM stores the '(1) it was handed.
(defn- sm-assoc-1 [sm k v]
  (let [cmp (tree-cmp sm) tree (jolt.host/sorted-tree sm)
        t (jolt.host/rbt-insert tree cmp k v)
        found (jolt.host/rbt-found t)]
    (if (nil? found)
      (jolt.host/sorted-with sm t (inc (jolt.host/sorted-cnt sm)))
      ;; the key is present: keep its first-inserted key, replace the value
      (if (identical? v (jolt.host/rbt-val found))
        sm
        (jolt.host/sorted-with sm (jolt.host/rbt-replace tree cmp k v) (jolt.host/sorted-cnt sm))))))

(defn- sm-assoc-many [sm kvs]
  (let [n (count kvs)]
    (when (odd? n)
      (throw (IllegalArgumentException. (str "No value supplied for key: " (nth kvs (dec n))))))
    (loop [m sm i 0]
      (if (< i n)
        (recur (sm-assoc-1 m (nth kvs i) (nth kvs (inc i))) (+ i 2))
        m))))

(defn- sm-dissoc-1 [sm k]
  (if (nil? (lookup sm k))
    sm
    (jolt.host/sorted-with sm (jolt.host/rbt-delete (jolt.host/sorted-tree sm) (tree-cmp sm) k)
                           (dec (jolt.host/sorted-cnt sm)))))

(defn- sm-dissoc-many [sm ks] (reduce sm-dissoc-1 sm ks))

;; conj on a map: a [k v] pair (2-vector / map-entry) or a map to merge;
;; nil is a no-op, as in Clojure.
(defn- sm-conj-1 [sm x]
  (cond
    (nil? x) sm
    (map? x) (reduce (fn [m e] (sm-assoc-1 m (first e) (second e))) sm (seq x))
    (and (vector? x) (= 2 (count x))) (sm-assoc-1 sm (nth x 0) (nth x 1))
    ;; any other java.util.Map$Entry (a TreeMap's), as APersistentMap.cons takes
    (map-entry? x) (sm-assoc-1 sm (key x) (val x))
    :else (throw (ex-info "conj on a sorted-map requires a [key value] pair or a map" {}))))

(defn- sm-conj-many [sm xs] (reduce sm-conj-1 sm xs))

;; --- sorted-set ops (elements stored as keys, nil value) --------------------
(defn- ss-get [ss x not-found]
  (let [n (lookup ss x)]
    (if (nil? n) not-found (jolt.host/rbt-key n))))

;; One walk. PersistentTreeSet.cons walks twice (contains, then add), so a
;; counting comparator sees fewer calls here than on the JVM; the set is not
;; pinned in the corpus for that reason, the map is.
(defn- ss-conj-1 [ss x]
  (let [t (jolt.host/rbt-insert (jolt.host/sorted-tree ss) (tree-cmp ss) x nil)]
    (if (nil? (jolt.host/rbt-found t))
      (jolt.host/sorted-with ss t (inc (jolt.host/sorted-cnt ss)))
      ss)))

(defn- ss-conj-many [ss xs] (reduce ss-conj-1 ss xs))

(defn- ss-disj-1 [ss x]
  (if (nil? (lookup ss x))
    ss
    (jolt.host/sorted-with ss (jolt.host/rbt-delete (jolt.host/sorted-tree ss) (tree-cmp ss) x)
                           (dec (jolt.host/sorted-cnt ss)))))

(defn- ss-disj-many [ss xs] (reduce ss-disj-1 ss xs))

;; --- the ops tables the host dispatches through ------------------------

(def ^:private sm-ops
  {:count    (fn [sm] (jolt.host/sorted-cnt sm))
   :seq      (fn [sm] (jolt.host/sorted-seq (jolt.host/sorted-tree sm) true 0))
   :rseq     (fn [sm] (jolt.host/sorted-seq (jolt.host/sorted-tree sm) false 0))
   :first    (fn [sm] (when-let [t (jolt.host/sorted-tree sm)] (map-entry (jolt.host/rbt-leftmost t))))
   :get      sm-get
   :entry-at sm-entry-at
   :contains (fn [sm k] (not (nil? (lookup sm k))))
   :assoc    sm-assoc-many
   :assoc1   sm-assoc-1
   :dissoc   sm-dissoc-many
   :dissoc1  sm-dissoc-1
   :conj     sm-conj-many
   :conj1    sm-conj-1
   :empty    (fn [sm] (jolt.host/sorted-with sm nil 0))})

(def ^:private ss-ops
  {:count    (fn [ss] (jolt.host/sorted-cnt ss))
   :seq      (fn [ss] (jolt.host/sorted-seq (jolt.host/sorted-tree ss) true 1))
   :rseq     (fn [ss] (jolt.host/sorted-seq (jolt.host/sorted-tree ss) false 1))
   :first    (fn [ss] (when-let [t (jolt.host/sorted-tree ss)] (jolt.host/rbt-key (jolt.host/rbt-leftmost t))))
   :get      ss-get
   :contains (fn [ss x] (not (nil? (lookup ss x))))
   :conj     ss-conj-many
   :conj1    ss-conj-1
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
