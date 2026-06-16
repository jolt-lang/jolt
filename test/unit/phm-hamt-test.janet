# Persistent hash map correctness + complexity (jolt-684u). Exercises the phm-*
# primitives directly (scalar keys — the perf-critical path) against a plain
# Janet table oracle, then asserts assoc is sub-linear per op (a HAMT, not the
# old O(n) copy-on-write bucket array). Collection-key value-equality is covered
# by the full clojure suite (needs core's canonicalize-key); here we test the
# representation in isolation.
(import ../../src/jolt/phm :as phm)

(var fails 0)
(defn check [label got expected]
  (if (deep= got expected) (print "  ok   " label)
    (do (++ fails) (printf "  FAIL %s: want %p got %p" label expected got))))

# --- basic assoc / get / overwrite / dissoc / count -------------------------
(var m (phm/make-phm))
(check "empty count" (phm/phm-count m) 0)
(check "get missing -> default" (phm/phm-get m :x :none) :none)
(set m (phm/phm-assoc m :a 1))
(set m (phm/phm-assoc m :b 2))
(set m (phm/phm-assoc m :c 3))
(check "count after 3" (phm/phm-count m) 3)
(check "get a" (phm/phm-get m :a) 1)
(check "get c" (phm/phm-get m :c) 3)
(check "contains b" (phm/phm-contains? m :b) true)
(check "contains missing" (phm/phm-contains? m :z) false)
(set m (phm/phm-assoc m :b 22))           # overwrite
(check "overwrite value" (phm/phm-get m :b) 22)
(check "overwrite keeps count" (phm/phm-count m) 3)
(set m (phm/phm-dissoc m :a))
(check "dissoc removes" (phm/phm-get m :a :gone) :gone)
(check "dissoc decrements" (phm/phm-count m) 2)
(check "dissoc missing is noop" (phm/phm-count (phm/phm-dissoc m :zzz)) 2)

# --- nil key (Clojure maps allow nil keys; struct drops them) ---------------
(var n (phm/phm-assoc (phm/make-phm) nil :nilval))
(check "nil key get" (phm/phm-get n nil) :nilval)
(check "nil key count" (phm/phm-count n) 1)
(check "nil key contains" (phm/phm-contains? n nil) true)
(set n (phm/phm-assoc n :k :v))
(check "nil + other count" (phm/phm-count n) 2)
(check "nil after add other" (phm/phm-get n nil) :nilval)
(set n (phm/phm-dissoc n nil))
(check "dissoc nil key" (phm/phm-get n nil :gone) :gone)
(check "dissoc nil count" (phm/phm-count n) 1)

# --- many keys vs a Janet-table oracle (string + int + keyword keys) --------
(def oracle @{})
(var big (phm/make-phm))
(def N 5000)
(loop [i :range [0 N]]
  (def k (cond (= 0 (% i 3)) (string "s" i)
               (= 1 (% i 3)) i
               (keyword "k" i)))
  (put oracle k i)
  (set big (phm/phm-assoc big k i)))
(check "big count == oracle" (phm/phm-count big) (length oracle))
(var mism 0)
(eachp [k v] oracle (unless (= (phm/phm-get big k :MISS) v) (++ mism)))
(check "all keys read back correctly" mism 0)
# entries round-trip
(check "entries count" (length (phm/phm-entries big)) (length oracle))
(def back @{})
(each e (phm/phm-entries big) (put back (in e 0) (in e 1)))
(check "entries round-trip == oracle" back oracle)
# dissoc half, recheck
(var half big)
(loop [i :range [0 N 2]]
  (def k (cond (= 0 (% i 3)) (string "s" i) (= 1 (% i 3)) i (keyword "k" i)))
  (set half (phm/phm-dissoc half k)))
(var hmism 0)
(loop [i :range [0 N]]
  (def k (cond (= 0 (% i 3)) (string "s" i) (= 1 (% i 3)) i (keyword "k" i)))
  (def want (if (even? i) :gone i))
  (unless (= (phm/phm-get half k :gone) want) (++ hmism)))
(check "dissoc half reads correctly" hmism 0)

# --- hash collisions (HashCollisionNode path) -------------------------------
# "k6595" and "k144747" both hash to 690120568 — distinct keys, same 32-bit hash.
(when (= (hash "k6595") (hash "k144747"))   # guard: only if Janet's hash still collides them
  (var c (phm/make-phm))
  (set c (phm/phm-assoc c "k6595" :A))
  (set c (phm/phm-assoc c "k144747" :B))
  (set c (phm/phm-assoc c :other 99))
  (check "collision: both keys present" (phm/phm-count c) 3)
  (check "collision: get first" (phm/phm-get c "k6595") :A)
  (check "collision: get second" (phm/phm-get c "k144747") :B)
  (check "collision: overwrite one" (phm/phm-get (phm/phm-assoc c "k6595" :A2) "k6595") :A2)
  (set c (phm/phm-dissoc c "k6595"))
  (check "collision: dissoc one keeps other" (phm/phm-get c "k144747") :B)
  (check "collision: dissoc removes" (phm/phm-get c "k6595" :gone) :gone)
  (check "collision: count after dissoc" (phm/phm-count c) 2))

# --- complexity: assoc must be sub-linear per op (HAMT, not O(n) copy) -------
# Build 20000 entries; on the old O(n)-copy map this is O(n^2) (~minutes). A HAMT
# does it in well under a second. Guard generously (5s) to avoid flakiness.
(def t0 (os/clock))
(var perf (phm/make-phm))
(loop [i :range [0 20000]] (set perf (phm/phm-assoc perf i i)))
(def elapsed (- (os/clock) t0))
(printf "  20000 assocs: %.3fs" elapsed)
(check "20000 assocs complete" (phm/phm-count perf) 20000)
(if (< elapsed 5.0)
  (print "  ok    assoc is sub-linear (< 5s)")
  (do (++ fails) (printf "  FAIL  assoc too slow (%.2fs) — O(n) per op?" elapsed)))

(if (> fails 0) (do (printf "phm-hamt: %d FAILED" fails) (os/exit 1))
  (print "phm-hamt: all passed"))
