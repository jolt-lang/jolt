;; clojure.core — sorted collections tier (stage 3, jolt-0lj).
;;
;; A sorted-map / sorted-set is a tagged host table
;;   {:jolt/type :jolt/sorted-map|:jolt/sorted-set
;;    :entries   VECTOR        ; comparator-ordered: [k v] pairs / elements
;;    :cmp       FN-or-nil     ; 3-way comparator; nil = natural order (compare)
;;    :ops       {op-kw fn}}   ; this tier's implementations, attached to the value
;;
;; ALL the semantics live here in Clojure. The Janet seed keeps only its
;; dispatch branches (conj/assoc/get/seq/count/…), each a one-line call through
;; the value's own :ops table — so the ops travel WITH the value (correct
;; across contexts, forks, and AOT images; no module-level hooks to re-wire).
;; The wrapper table itself is minted and read through the minimal host value
;; primitives: jolt.host/tagged-table + jolt.host/ref-put! + jolt.host/ref-get.
;;
;; Clojure semantics this port fixes vs the old Janet kernel: lookup and
;; membership go through the COMPARATOR ((contains? (sorted-set 1) 1.0) was a
;; deep= scan; assoc/conj of a comparator-equal key replaces/no-ops), equality
;; is representation-agnostic ((= (sorted-map :a 1) {:a 1})), empty?/empty see
;; the collection rather than the wrapper, (empty sc) keeps the comparator,
;; iteration (map/reduce/filter) works, and sorted colls canonicalize as map
;; keys. Entries keep the FIRST-inserted key on replace, as Clojure's
;; PersistentTreeMap does.

;; Raw field read on the wrapper (host primitive). Plain `get` on a sorted coll
;; IS the comparator lookup — it dispatches back into these ops, so reading
;; :entries/:cmp/:ops with it would recurse forever.
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

;; Lowest index in [0, n) whose key is >= k under cmp (n when none).
(defn- lower-bound [es keyf cmp k]
  (loop [lo 0 hi (count es)]
    (if (< lo hi)
      (let [mid (quot (+ lo hi) 2)]
        (if (neg? (cmp (keyf (nth es mid)) k))
          (recur (inc mid) hi)
          (recur lo mid)))
      lo)))

;; Index of the comparator-equal entry, or -1.
(defn- find-idx [sc keyf k]
  (let [es (sfield sc :entries)
        cmp (the-cmp sc)
        i (lower-bound es keyf cmp k)]
    (if (and (< i (count es)) (zero? (cmp (keyf (nth es i)) k))) i -1)))

(defn- make-sorted [tag es cmp ops]
  (-> (jolt.host/tagged-table tag)
      (jolt.host/ref-put! :entries es)
      (jolt.host/ref-put! :cmp cmp)
      (jolt.host/ref-put! :ops ops)))

(defn- insert-at [es i x] (into (conj (subvec es 0 i) x) (subvec es i)))
(defn- remove-at [es i] (into (subvec es 0 i) (subvec es (inc i))))

;; --- sorted-map ops ---------------------------------------------------------

(defn- sm-get [sm k not-found]
  (let [i (find-idx sm first k)]
    (if (neg? i) not-found (second (nth (sfield sm :entries) i)))))

(defn- sm-assoc-1 [sm k v]
  (let [es (sfield sm :entries)
        cmp (the-cmp sm)
        i (lower-bound es first cmp k)
        found (and (< i (count es)) (zero? (cmp (first (nth es i)) k)))]
    (make-sorted :jolt/sorted-map
                 (if found
                   (assoc es i [(first (nth es i)) v])
                   (insert-at es i [k v]))
                 (sfield sm :cmp) (sfield sm :ops))))

(defn- sm-assoc-many [sm kvs]
  (let [n (count kvs)]
    (when (odd? n)
      (throw (ex-info "sorted-map assoc expects an even number of key/values" {:count n})))
    (loop [m sm i 0]
      (if (< i n)
        (recur (sm-assoc-1 m (nth kvs i) (nth kvs (inc i))) (+ i 2))
        m))))

(defn- sm-dissoc-many [sm ks]
  (reduce (fn [m k]
            (let [i (find-idx m first k)]
              (if (neg? i)
                m
                (make-sorted :jolt/sorted-map (remove-at (sfield m :entries) i)
                             (sfield m :cmp) (sfield m :ops)))))
          sm ks))

;; conj on a map: a [k v] pair (2-vector / map-entry) or a map to merge;
;; nil is a no-op, as in Clojure.
(defn- sm-conj-1 [sm x]
  (cond
    (nil? x) sm
    (map? x) (reduce (fn [m e] (sm-assoc-1 m (first e) (second e))) sm (seq x))
    (and (vector? x) (= 2 (count x))) (sm-assoc-1 sm (nth x 0) (nth x 1))
    :else (throw (ex-info "conj on a sorted-map requires a [key value] pair or a map" {}))))

(defn- sm-conj-many [sm xs] (reduce sm-conj-1 sm xs))

;; --- sorted-set ops ---------------------------------------------------------

(defn- ss-get [ss x not-found]
  (let [i (find-idx ss identity x)]
    (if (neg? i) not-found (nth (sfield ss :entries) i))))

(defn- ss-conj-1 [ss x]
  (let [es (sfield ss :entries)
        cmp (the-cmp ss)
        i (lower-bound es identity cmp x)]
    (if (and (< i (count es)) (zero? (cmp (nth es i) x)))
      ss
      (make-sorted :jolt/sorted-set (insert-at es i x) (sfield ss :cmp) (sfield ss :ops)))))

(defn- ss-conj-many [ss xs] (reduce ss-conj-1 ss xs))

(defn- ss-disj-many [ss xs]
  (reduce (fn [s x]
            (let [i (find-idx s identity x)]
              (if (neg? i)
                s
                (make-sorted :jolt/sorted-set (remove-at (sfield s :entries) i)
                             (sfield s :cmp) (sfield s :ops)))))
          ss xs))

;; --- the ops tables the Janet seed dispatches through ------------------------

(def ^:private sm-ops
  {:count    (fn [sm] (count (sfield sm :entries)))
   :seq      (fn [sm] (seq (sfield sm :entries)))
   :rseq     (fn [sm] (seq (vec (reverse (sfield sm :entries)))))
   :first    (fn [sm] (first (sfield sm :entries)))
   :keys     (fn [sm] (seq (mapv first (sfield sm :entries))))
   :vals     (fn [sm] (seq (mapv second (sfield sm :entries))))
   :get      sm-get
   :contains (fn [sm k] (not (neg? (find-idx sm first k))))
   :assoc    sm-assoc-many
   :dissoc   sm-dissoc-many
   :conj     sm-conj-many
   :empty    (fn [sm] (make-sorted :jolt/sorted-map [] (sfield sm :cmp) (sfield sm :ops)))})

(def ^:private ss-ops
  {:count    (fn [ss] (count (sfield ss :entries)))
   :seq      (fn [ss] (seq (sfield ss :entries)))
   :rseq     (fn [ss] (seq (vec (reverse (sfield ss :entries)))))
   :first    (fn [ss] (first (sfield ss :entries)))
   :get      ss-get
   :contains (fn [ss x] (not (neg? (find-idx ss identity x))))
   :conj     ss-conj-many
   :disj     ss-disj-many
   :empty    (fn [ss] (make-sorted :jolt/sorted-set [] (sfield ss :cmp) (sfield ss :ops)))})

;; --- constructors + predicates -----------------------------------------------

(defn sorted-map [& kvs]
  (sm-assoc-many (make-sorted :jolt/sorted-map [] nil sm-ops) (vec kvs)))

(defn sorted-map-by [comparator & kvs]
  (sm-assoc-many (make-sorted :jolt/sorted-map [] (fn->cmp comparator) sm-ops) (vec kvs)))

(defn sorted-set [& xs]
  (ss-conj-many (make-sorted :jolt/sorted-set [] nil ss-ops) (vec xs)))

(defn sorted-set-by [comparator & xs]
  (ss-conj-many (make-sorted :jolt/sorted-set [] (fn->cmp comparator) ss-ops) (vec xs)))

(defn sorted-map? [x] (= :jolt/sorted-map (sfield x :jolt/type)))
(defn sorted-set? [x] (= :jolt/sorted-set (sfield x :jolt/type)))
(defn sorted? [x] (or (sorted-map? x) (sorted-set? x)))

;; --- subseq / rsubseq ---------------------------------------------------------
;; test is one of < <= > >= applied Clojure-style to the comparator result:
;; keep entries whose (cmp entry-key k) satisfies (test _ 0). Returns a seq or
;; nil, like Clojure.

(defn- sc-keyf [sc] (if (sorted-map? sc) first identity))

(defn- sub-filter [sc tests]
  (let [cmp (the-cmp sc)
        keyf (sc-keyf sc)]
    (filterv (fn [e]
               (every? (fn [[test k]] (test (cmp (keyf e) k) 0)) tests))
             (sfield sc :entries))))

(defn subseq
  ([sc test k] (seq (sub-filter sc [[test k]])))
  ([sc start-test start-k end-test end-k]
   (seq (sub-filter sc [[start-test start-k] [end-test end-k]]))))

(defn rsubseq
  ([sc test k] (seq (vec (reverse (sub-filter sc [[test k]])))))
  ([sc start-test start-k end-test end-k]
   (seq (vec (reverse (sub-filter sc [[start-test start-k] [end-test end-k]]))))))
