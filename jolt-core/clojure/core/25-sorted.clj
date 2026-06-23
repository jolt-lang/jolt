;; clojure.core — sorted collections tier.
;;
;; A sorted-map / sorted-set is a tagged host table
;;   {:jolt/type :jolt/sorted-map|:jolt/sorted-set
;;    :tree      RB-NODE | nil   ; a red-black tree, comparator-ordered
;;    :cnt       N               ; element count (O(1))
;;    :cmp       FN-or-nil       ; 3-way comparator; nil = natural order (compare)
;;    :ops       {op-kw fn}}     ; this tier's implementations, attached to the value
;;
;; The tree is a left-leaning-free red-black tree — Rich Hickey's algorithm,
;; ported from the ClojureScript PersistentTreeMap (cljs.core: tree-map-add /
;; balance-left / balance-right / tree-map-append / balance-*-del). assoc / get /
;; dissoc / contains are O(log n). cljs uses BlackNode/RedNode
;; deftypes, but this tier loads before 30-macros (no deftype), so a node is a
;; plain vector [color k v left right] (color :red/:black; left/right node|nil)
;; and the methods become functions — the algorithm is identical.
;;
;; A sorted-SET stores its elements as keys with a nil value; its ops project the
;; key. ALL the semantics live here in Clojure; the host keeps only its
;; dispatch branches (conj/assoc/get/seq/count/…), each a one-line call through
;; the value's own :ops table, so the ops travel WITH the value (correct across
;; contexts, forks, and AOT images). The wrapper is minted/read through the host
;; value primitives jolt.host/tagged-table + ref-put! + ref-get.

;; Raw field read on the wrapper (host primitive). Plain `get` on a sorted coll
;; IS the comparator lookup — it dispatches back into these ops, so reading
;; :tree/:cmp/:ops with it would recurse forever.
(defn- sfield [sc k] (jolt.host/ref-get sc k))

;; Clojure's fn->comparator: a comparator fn may return a number (3-way) or a
;; boolean less-than predicate.
(defn- fn->cmp [f]
  (fn [a b]
    (let [r (f a b)]
      (if (number? r)
        r
        (if r -1 (if (f b a) 1 0))))))

(defn- the-cmp [sc] (or (sfield sc :cmp) compare))

;; --- red-black tree nodes: [color key val left right] -----------------------
(defn- nd-key [n] (nth n 1))
(defn- nd-val [n] (nth n 2))
(defn- nd-left [n] (nth n 3))
(defn- nd-right [n] (nth n 4))
(defn- red? [n] (and n (identical? :red (nth n 0))))
(defn- black? [n] (and n (identical? :black (nth n 0))))
(defn- mk-red [k v l r] [:red k v l r])
(defn- mk-black [k v l r] [:black k v l r])
;; BlackNode.blacken = self; RedNode.blacken = a black copy.
(defn- blacken [n] (if (red? n) [:black (nd-key n) (nd-val n) (nd-left n) (nd-right n)] n))
;; BlackNode.redden = a red copy; RedNode.redden = invariant violation (never hit
;; on the paths that call it: redden is only applied to a known-black node).
(defn- redden [n] [:red (nd-key n) (nd-val n) (nd-left n) (nd-right n)])
;; replace a node's key/val/children KEEPING its color.
(defn- replace-node [n k v l r] (if (red? n) (mk-red k v l r) (mk-black k v l r)))

;; --- insert balancing (the RedNode/BlackNode .balance-left/.balance-right) ---
(defn- ins-balance-left [ins parent]
  (if (red? ins)
    (let [l (nd-left ins) r (nd-right ins)]
      (cond
        (red? l) (mk-red (nd-key ins) (nd-val ins)
                         (blacken l)
                         (mk-black (nd-key parent) (nd-val parent) r (nd-right parent)))
        (red? r) (mk-red (nd-key r) (nd-val r)
                         (mk-black (nd-key ins) (nd-val ins) l (nd-left r))
                         (mk-black (nd-key parent) (nd-val parent) (nd-right r) (nd-right parent)))
        :else (mk-black (nd-key parent) (nd-val parent) ins (nd-right parent))))
    (mk-black (nd-key parent) (nd-val parent) ins (nd-right parent))))

(defn- ins-balance-right [ins parent]
  (if (red? ins)
    (let [l (nd-left ins) r (nd-right ins)]
      (cond
        (red? r) (mk-red (nd-key ins) (nd-val ins)
                         (mk-black (nd-key parent) (nd-val parent) (nd-left parent) l)
                         (blacken r))
        (red? l) (mk-red (nd-key l) (nd-val l)
                         (mk-black (nd-key parent) (nd-val parent) (nd-left parent) (nd-left l))
                         (mk-black (nd-key ins) (nd-val ins) (nd-right l) r))
        :else (mk-black (nd-key parent) (nd-val parent) (nd-left parent) ins)))
    (mk-black (nd-key parent) (nd-val parent) (nd-left parent) ins)))

;; node .add-left / .add-right (parent gains a new left/right subtree `ins`)
(defn- add-left [parent ins]
  (if (red? parent)
    (mk-red (nd-key parent) (nd-val parent) ins (nd-right parent))
    (ins-balance-left ins parent)))
(defn- add-right [parent ins]
  (if (red? parent)
    (mk-red (nd-key parent) (nd-val parent) (nd-left parent) ins)
    (ins-balance-right ins parent)))

;; insert k/v into tree, assuming k is NOT already present (the caller checks).
(defn- tree-ins [cmp tree k v]
  (if (nil? tree)
    (mk-red k v nil nil)
    (if (neg? (cmp k (nd-key tree)))
      (add-left tree (tree-ins cmp (nd-left tree) k v))
      (add-right tree (tree-ins cmp (nd-right tree) k v)))))

;; replace the value at an existing key, keeping the tree structure (and the
;; first-inserted key, like Clojure's PersistentTreeMap).
(defn- tree-replace [cmp tree k v]
  (let [c (cmp k (nd-key tree))]
    (cond
      (zero? c) (replace-node tree (nd-key tree) v (nd-left tree) (nd-right tree))
      (neg? c)  (replace-node tree (nd-key tree) (nd-val tree) (tree-replace cmp (nd-left tree) k v) (nd-right tree))
      :else     (replace-node tree (nd-key tree) (nd-val tree) (nd-left tree) (tree-replace cmp (nd-right tree) k v)))))

(defn- tree-lookup [tree cmp k]
  (loop [t tree]
    (if (nil? t)
      nil
      (let [c (cmp k (nd-key t))]
        (cond (zero? c) t
              (neg? c) (recur (nd-left t))
              :else (recur (nd-right t)))))))

;; --- delete balancing (cljs standalone balance-left / balance-right / *-del) -
(defn- balance-left [k v ins right]
  (if (red? ins)
    (let [il (nd-left ins) ir (nd-right ins)]
      (cond
        (red? il) (mk-red (nd-key ins) (nd-val ins) (blacken il) (mk-black k v ir right))
        (red? ir) (mk-red (nd-key ir) (nd-val ir)
                          (mk-black (nd-key ins) (nd-val ins) il (nd-left ir))
                          (mk-black k v (nd-right ir) right))
        :else (mk-black k v ins right)))
    (mk-black k v ins right)))

(defn- balance-right [k v left ins]
  (if (red? ins)
    (let [il (nd-left ins) ir (nd-right ins)]
      (cond
        (red? ir) (mk-red (nd-key ins) (nd-val ins) (mk-black k v left il) (blacken ir))
        (red? il) (mk-red (nd-key il) (nd-val il)
                          (mk-black k v left (nd-left il))
                          (mk-black (nd-key ins) (nd-val ins) (nd-right il) ir))
        :else (mk-black k v left ins)))
    (mk-black k v left ins)))

(defn- balance-left-del [k v del right]
  (cond
    (red? del) (mk-red k v (blacken del) right)
    (black? right) (balance-right k v del (redden right))
    (and (red? right) (black? (nd-left right)))
      (mk-red (nd-key (nd-left right)) (nd-val (nd-left right))
              (mk-black k v del (nd-left (nd-left right)))
              (balance-right (nd-key right) (nd-val right) (nd-right (nd-left right)) (redden (nd-right right))))
    :else (throw (ex-info "red-black tree invariant violation" {}))))

(defn- balance-right-del [k v left del]
  (cond
    (red? del) (mk-red k v left (blacken del))
    (black? left) (balance-left k v (redden left) del)
    (and (red? left) (black? (nd-right left)))
      (mk-red (nd-key (nd-right left)) (nd-val (nd-right left))
              (balance-left (nd-key left) (nd-val left) (redden (nd-left left)) (nd-left (nd-right left)))
              (mk-black k v (nd-right (nd-right left)) del))
    :else (throw (ex-info "red-black tree invariant violation" {}))))

;; merge two subtrees (the children of a removed node)
(defn- tree-append [left right]
  (cond
    (nil? left) right
    (nil? right) left
    (red? left)
      (if (red? right)
        (let [app (tree-append (nd-right left) (nd-left right))]
          (if (red? app)
            (mk-red (nd-key app) (nd-val app)
                    (mk-red (nd-key left) (nd-val left) (nd-left left) (nd-left app))
                    (mk-red (nd-key right) (nd-val right) (nd-right app) (nd-right right)))
            (mk-red (nd-key left) (nd-val left) (nd-left left)
                    (mk-red (nd-key right) (nd-val right) app (nd-right right)))))
        (mk-red (nd-key left) (nd-val left) (nd-left left) (tree-append (nd-right left) right)))
    (red? right)
      (mk-red (nd-key right) (nd-val right) (tree-append left (nd-left right)) (nd-right right))
    :else
      (let [app (tree-append (nd-right left) (nd-left right))]
        (if (red? app)
          (mk-red (nd-key app) (nd-val app)
                  (mk-black (nd-key left) (nd-val left) (nd-left left) (nd-left app))
                  (mk-black (nd-key right) (nd-val right) (nd-right app) (nd-right right)))
          (balance-left-del (nd-key left) (nd-val left) (nd-left left)
                            (mk-black (nd-key right) (nd-val right) app (nd-right right)))))))

;; remove k from tree, assuming k IS present (the caller checks).
(defn- tree-del [cmp tree k]
  (let [c (cmp k (nd-key tree))]
    (cond
      (zero? c) (tree-append (nd-left tree) (nd-right tree))
      (neg? c) (let [del (tree-del cmp (nd-left tree) k)]
                 (if (black? (nd-left tree))
                   (balance-left-del (nd-key tree) (nd-val tree) del (nd-right tree))
                   (mk-red (nd-key tree) (nd-val tree) del (nd-right tree))))
      :else (let [del (tree-del cmp (nd-right tree) k)]
              (if (black? (nd-right tree))
                (balance-right-del (nd-key tree) (nd-val tree) (nd-left tree) del)
                (mk-red (nd-key tree) (nd-val tree) (nd-left tree) del))))))

;; in-order walk: conj (proj node) for each node, ascending.
(defn- tree-collect [t proj acc]
  (if (nil? t)
    acc
    (tree-collect (nd-right t) proj
                  (conj (tree-collect (nd-left t) proj acc) (proj t)))))

(defn- make-sorted [tag tree cnt cmp ops]
  (-> (jolt.host/tagged-table tag)
      (jolt.host/ref-put! :tree tree)
      (jolt.host/ref-put! :cnt cnt)
      (jolt.host/ref-put! :cmp cmp)
      (jolt.host/ref-put! :ops ops)))

;; entries as a vector (ascending), the materialized form seq/rseq/subseq use.
(defn- sc-entries [sc proj]
  (tree-collect (sfield sc :tree) proj []))

;; --- sorted-map ops ---------------------------------------------------------
;; a real map-entry (map-entry? true), so key/val/seq destructuring work like a
;; regular map's entries.
(defn- map-entry [t] (jolt.host/map-entry (nd-key t) (nd-val t)))

(defn- sm-get [sm k not-found]
  (let [n (tree-lookup (sfield sm :tree) (the-cmp sm) k)]
    (if (nil? n) not-found (nd-val n))))

(defn- sm-assoc-1 [sm k v]
  (let [cmp (the-cmp sm) tree (sfield sm :tree)
        node (tree-lookup tree cmp k)]
    (cond
      (and node (= v (nd-val node))) sm
      node (make-sorted :jolt/sorted-map (tree-replace cmp tree k v) (sfield sm :cnt) (sfield sm :cmp) (sfield sm :ops))
      :else (make-sorted :jolt/sorted-map (blacken (tree-ins cmp tree k v)) (inc (sfield sm :cnt)) (sfield sm :cmp) (sfield sm :ops)))))

(defn- sm-assoc-many [sm kvs]
  (let [n (count kvs)]
    (when (odd? n)
      (throw (ex-info "sorted-map assoc expects an even number of key/values" {:count n})))
    (loop [m sm i 0]
      (if (< i n)
        (recur (sm-assoc-1 m (nth kvs i) (nth kvs (inc i))) (+ i 2))
        m))))

(defn- sm-dissoc-1 [sm k]
  (let [cmp (the-cmp sm) tree (sfield sm :tree)]
    (if (nil? (tree-lookup tree cmp k))
      sm
      (let [t (tree-del cmp tree k)]
        (make-sorted :jolt/sorted-map (when t (blacken t)) (dec (sfield sm :cnt)) (sfield sm :cmp) (sfield sm :ops))))))

(defn- sm-dissoc-many [sm ks] (reduce sm-dissoc-1 sm ks))

;; conj on a map: a [k v] pair (2-vector / map-entry) or a map to merge;
;; nil is a no-op, as in Clojure.
(defn- sm-conj-1 [sm x]
  (cond
    (nil? x) sm
    (map? x) (reduce (fn [m e] (sm-assoc-1 m (first e) (second e))) sm (seq x))
    (and (vector? x) (= 2 (count x))) (sm-assoc-1 sm (nth x 0) (nth x 1))
    :else (throw (ex-info "conj on a sorted-map requires a [key value] pair or a map" {}))))

(defn- sm-conj-many [sm xs] (reduce sm-conj-1 sm xs))

;; --- sorted-set ops (elements stored as keys, nil value) --------------------
(defn- ss-get [ss x not-found]
  (let [n (tree-lookup (sfield ss :tree) (the-cmp ss) x)]
    (if (nil? n) not-found (nd-key n))))

(defn- ss-conj-1 [ss x]
  (let [cmp (the-cmp ss) tree (sfield ss :tree)]
    (if (tree-lookup tree cmp x)
      ss
      (make-sorted :jolt/sorted-set (blacken (tree-ins cmp tree x nil)) (inc (sfield ss :cnt)) (sfield ss :cmp) (sfield ss :ops)))))

(defn- ss-conj-many [ss xs] (reduce ss-conj-1 ss xs))

(defn- ss-disj-1 [ss x]
  (let [cmp (the-cmp ss) tree (sfield ss :tree)]
    (if (nil? (tree-lookup tree cmp x))
      ss
      (let [t (tree-del cmp tree x)]
        (make-sorted :jolt/sorted-set (when t (blacken t)) (dec (sfield ss :cnt)) (sfield ss :cmp) (sfield ss :ops))))))

(defn- ss-disj-many [ss xs] (reduce ss-disj-1 ss xs))

;; --- the ops tables the host dispatches through ------------------------

(def ^:private sm-ops
  {:count    (fn [sm] (sfield sm :cnt))
   :entries  (fn [sm] (sc-entries sm map-entry))
   :seq      (fn [sm] (seq (sc-entries sm map-entry)))
   :rseq     (fn [sm] (seq (vec (reverse (sc-entries sm map-entry)))))
   :first    (fn [sm] (first (sc-entries sm map-entry)))
   :get      sm-get
   :contains (fn [sm k] (not (nil? (tree-lookup (sfield sm :tree) (the-cmp sm) k))))
   :assoc    sm-assoc-many
   :dissoc   sm-dissoc-many
   :conj     sm-conj-many
   :empty    (fn [sm] (make-sorted :jolt/sorted-map nil 0 (sfield sm :cmp) (sfield sm :ops)))})

(def ^:private ss-ops
  {:count    (fn [ss] (sfield ss :cnt))
   :entries  (fn [ss] (sc-entries ss nd-key))
   :seq      (fn [ss] (seq (sc-entries ss nd-key)))
   :rseq     (fn [ss] (seq (vec (reverse (sc-entries ss nd-key)))))
   :first    (fn [ss] (first (sc-entries ss nd-key)))
   :get      ss-get
   :contains (fn [ss x] (not (nil? (tree-lookup (sfield ss :tree) (the-cmp ss) x))))
   :conj     ss-conj-many
   :disj     ss-disj-many
   :empty    (fn [ss] (make-sorted :jolt/sorted-set nil 0 (sfield ss :cmp) (sfield ss :ops)))})

;; --- constructors + predicates -----------------------------------------------

(defn sorted-map [& kvs]
  (sm-assoc-many (make-sorted :jolt/sorted-map nil 0 nil sm-ops) (vec kvs)))

(defn sorted-map-by [comparator & kvs]
  (sm-assoc-many (make-sorted :jolt/sorted-map nil 0 (fn->cmp comparator) sm-ops) (vec kvs)))

(defn sorted-set [& xs]
  (ss-conj-many (make-sorted :jolt/sorted-set nil 0 nil ss-ops) (vec xs)))

(defn sorted-set-by [comparator & xs]
  (ss-conj-many (make-sorted :jolt/sorted-set nil 0 (fn->cmp comparator) ss-ops) (vec xs)))

(defn sorted-map? [x] (= :jolt/sorted-map (sfield x :jolt/type)))
(defn sorted-set? [x] (= :jolt/sorted-set (sfield x :jolt/type)))
(defn sorted? [x] (or (sorted-map? x) (sorted-set? x)))

;; --- subseq / rsubseq ---------------------------------------------------------
;; test is one of < <= > >= applied Clojure-style to the comparator result:
;; keep entries whose (cmp entry-key k) satisfies (test _ 0). Returns a seq or
;; nil, like Clojure.

(defn- sc-keyf [sc] (if (sorted-map? sc) first identity))
(defn- sc-proj [sc] (if (sorted-map? sc) map-entry nd-key))

(defn- sub-filter [sc tests]
  (let [cmp (the-cmp sc)
        keyf (sc-keyf sc)]
    (filterv (fn [e]
               (every? (fn [[test k]] (test (cmp (keyf e) k) 0)) tests))
             (sc-entries sc (sc-proj sc)))))

(defn subseq
  ([sc test k] (seq (sub-filter sc [[test k]])))
  ([sc start-test start-k end-test end-k]
   (seq (sub-filter sc [[start-test start-k] [end-test end-k]]))))

(defn rsubseq
  ([sc test k] (seq (vec (reverse (sub-filter sc [[test k]])))))
  ([sc start-test start-k end-test end-k]
   (seq (vec (reverse (sub-filter sc [[start-test start-k] [end-test end-k]]))))))
