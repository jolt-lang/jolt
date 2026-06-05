# Phase 2: PersistentHashMap Tests
# Uses Clojure = (core-=) for PHM-aware comparison

(use ../../src/jolt/api)

(defn ct-eval [ctx s] (normalize-pvecs (eval-string ctx s)))

# Helper: compare via Clojure = which handles PHM
(defn clj= [ctx a b]
  (eval-string ctx (string "(= " a " " b ")")))

# ============================================================
# 1. Basic hash-map construction and access
# ============================================================
(print "1: hash-map construction...")
(let [ctx (init)]
  (def m1 (ct-eval ctx "(hash-map :a 1)"))
  (assert (not (nil? m1)) "hash-map returns non-nil")
  (assert (= true (ct-eval ctx "(map? (hash-map :a 1))")) "map? returns true for PHM")
  (assert (= true (ct-eval ctx "(= (hash-map :a 1) {:a 1})")) "PHM = struct via Clojure =")

  (assert (= 0 (ct-eval ctx "(count (hash-map))")) "count empty")
  (assert (= 2 (ct-eval ctx "(count (hash-map :a 1 :b 2))")) "count two")
  (assert (= 1 (ct-eval ctx "(get (hash-map :a 1 :b 2) :a)")) "get present")
  (assert (= nil (ct-eval ctx "(get (hash-map :a 1) :z)")) "get missing"))
(print "  passed")

# ============================================================
# 2. assoc and dissoc
# ============================================================
(print "2: assoc/dissoc...")
(let [ctx (init)]
  (assert (= true (ct-eval ctx "(= (assoc (hash-map :a 1) :b 2) (hash-map :a 1 :b 2))")) "assoc add")
  (assert (= true (ct-eval ctx "(= (assoc (hash-map :a 1) :a 99) (hash-map :a 99))")) "assoc replace")
  (assert (= true (ct-eval ctx "(= (dissoc (hash-map :a 1 :b 2) :a) (hash-map :b 2))")) "dissoc")
  (assert (= true (ct-eval ctx "(contains? (hash-map :a 1) :a)")) "contains? true")
  (assert (= false (ct-eval ctx "(contains? (hash-map :a 1) :z)")) "contains? false"))
(print "  passed")

# ============================================================
# 3. keys, vals, merge
# ============================================================
(print "3: keys/vals/merge...")
(let [ctx (init)]
  (assert (= 2 (ct-eval ctx "(count (keys (hash-map :a 1 :b 2)))")) "keys count")
  (assert (= 2 (ct-eval ctx "(count (vals (hash-map :a 1 :b 2)))")) "vals count")
  (assert (= true (ct-eval ctx "(= (merge (hash-map :a 1) (hash-map :b 2)) (hash-map :a 1 :b 2))")) "merge"))

(print "  passed")

# ============================================================
# 4. Empty and seq
# ============================================================
(print "4: empty? and seq...")
(let [ctx (init)]
  (assert (= true (ct-eval ctx "(empty? (hash-map))")) "empty? true")
  (assert (= false (ct-eval ctx "(empty? (hash-map :a 1))")) "empty? false")
  (assert (= 1 (ct-eval ctx "(count (seq (hash-map :a 1)))")) "seq count"))
(print "  passed")

# ============================================================
# 5. Larger maps
# ============================================================
(print "5: larger maps...")
(let [ctx (init)]
  (eval-string ctx "
    (def big-map
      (reduce (fn [m i] (assoc m (keyword (str \"k\" i)) i))
              (hash-map)
              (range 100)))")
  (assert (= 100 (ct-eval ctx "(count big-map)")) "count 100")
  (assert (= 42 (ct-eval ctx "(get big-map :k42)")) "get k42"))
(print "  passed")

(print "\nAll PersistentHashMap tests passed!")
