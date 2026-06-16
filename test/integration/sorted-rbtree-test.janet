# sorted-map / sorted-set are a red-black tree (jolt-0hbr), ported from the
# ClojureScript PersistentTreeMap. assoc/dissoc are O(log n); the old sorted-
# vector rep was O(n) per op (O(n^2) to build — 55s for 2000 entries). This
# drives big shuffled insert/delete sequences (which stress rebalancing) through
# the built binary and checks ordering + a sub-quadratic build time.
(import ../../src/jolt/api :as api)

(print "sorted-map/set red-black tree (jolt-0hbr)...")

(os/setenv "JOLT_DIRECT_LINK" "1")
(def ctx (api/init {:compile? true}))
(defn ev [s] (api/eval-string ctx s))

(var fails 0)
(defn check [label got expected]
  (if (= got expected) (print "  ok   " label)
    (do (++ fails) (printf "  FAIL %s: want %p got %p" label expected got))))

# --- ordering is maintained through heavy rebalancing -----------------------
(check "shuffled 500 keys come back sorted"
  (ev "(= (vec (keys (apply sorted-map (interleave (shuffle (vec (range 500))) (range 500))))) (vec (range 500)))")
  true)
(check "sorted-set of shuffled 500 is sorted"
  (ev "(= (vec (apply sorted-set (shuffle (vec (range 500))))) (vec (range 500)))")
  true)

# --- delete maintains order + correctness (stresses delete rebalancing) ------
(check "dissoc every even key, odds remain in order"
  (ev "(let [m (apply sorted-map (interleave (shuffle (vec (range 200))) (range 200)))
             m2 (reduce dissoc m (range 0 200 2))]
         (= (vec (keys m2)) (vec (range 1 200 2))))")
  true)
(check "disj down to empty then rebuild"
  (ev "(let [s (apply sorted-set (range 100))
             s2 (reduce disj s (shuffle (vec (range 100))))]
         (and (= 0 (count s2)) (= [1 2 3] (vec (conj s2 3 1 2 1)))))")
  true)

# --- comparator + lookup correctness ----------------------------------------
(check "custom comparator (descending)"
  (ev "(= (vec (keys (sorted-map-by > 1 :a 3 :c 2 :b))) [3 2 1])") true)
(check "get/contains go through comparator (1 vs 1.0)"
  (ev "(and (contains? (sorted-set 1 2 3) 1.0) (= :a (get (sorted-map 1 :a) 1.0)))") true)
(check "first-inserted key kept on value replace"
  (ev "(first (first (assoc (sorted-map 1 :a) 1.0 :b)))") 1)

# --- count + subseq ----------------------------------------------------------
(check "count after mixed ops"
  (ev "(count (-> (apply sorted-map (interleave (range 50) (range 50))) (dissoc 10 20 30) (assoc 100 1 101 2)))") 49)
(check "subseq range"
  (ev "(= (vec (map first (subseq (apply sorted-map (interleave (range 20) (range 20))) >= 5 < 9))) [5 6 7 8])") true)

# --- complexity: building a big map must be sub-quadratic --------------------
(def t0 (os/clock))
(ev "(count (loop [i 0 m (sorted-map)] (if (< i 5000) (recur (inc i) (assoc m (mod (* i 7919) 10007) i)) m)))")
(def elapsed (- (os/clock) t0))
(printf "  5000 assocs: %.2fs" elapsed)
(if (< elapsed 8.0)
  (print "  ok    sorted assoc is sub-quadratic (< 8s)")
  (do (++ fails) (printf "  FAIL  sorted build too slow (%.1fs) — O(n^2)?" elapsed)))

(if (> fails 0) (do (printf "sorted-rbtree: %d FAILED" fails) (os/exit 1))
  (print "sorted-rbtree (jolt-0hbr) passed!"))
