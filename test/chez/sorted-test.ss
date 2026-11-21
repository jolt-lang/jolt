;; Sorted collections: the tree walk behind seq/rseq/keys/vals/subseq, and the
;; hash and = of a sorted map/set. Run:
;;   chez --script test/chez/sorted-test.ss
;; Values are pinned to JVM Clojure (each was checked against it). Scaling is
;; pinned as ALLOCATION, which is deterministic where wall time is not: reaching
;; the head of a seq over a sorted coll used to run the whole tree into a vector
;; (O(n) bytes), a subseq walk re-descended from the root for every node without a
;; right child, and hash / = rebuilt a full hash map on every call.

(import (chezscheme))
(load "host/chez/gate-boot.ss")

(define total 0) (define fails 0)
(define (ok name pred) (set! total (+ total 1)) (unless pred (set! fails (+ fails 1)) (printf "FAIL: ~a\n" name)))
(define (ev s) (jolt-final-str (jolt-compile-eval (string-append "(do " s ")") "user")))
(define (is s expect)
  (let ((got (guard (e (#t "<threw>")) (ev s))))
    (ok (string-append s " => " expect " (got " got ")") (string=? got expect))))

(ev "(def sm (into (sorted-map) (map (fn [i] [i (* 10 i)]) (range 0 100 3))))")
(ev "(def ss (into (sorted-set) (range 0 100 3)))")
(ev "(def rs (sorted-set-by > 1 5 3 9 7))")

;; --- the walk: order, both directions, every view ---------------------------
(is "(take 3 sm)" "([0 0] [3 30] [6 60])")
(is "(take 3 (rseq sm))" "([99 990] [96 960] [93 930])")
(is "(= (reverse (seq ss)) (rseq ss))" "true")
(is "(= (range 0 100 3) (seq ss) (keys sm))" "true")
(is "(= (map #(* 10 %) (range 0 100 3)) (vals sm))" "true")
(is "(seq rs)" "(9 7 5 3 1)")
(is "(rseq rs)" "(1 3 5 7 9)")
(is "(nth (seq ss) 33)" "99")
(is "(count (seq ss))" "34")
(is "(reduce + (rseq ss))" "1683")
(is "(into [] (map inc) (take 5 (drop 30 ss)))" "[91 94 97 100]")
(is "[(seq (sorted-map)) (rseq (sorted-set)) (keys (sorted-map)) (vals (sorted-map))]" "[nil nil nil nil]")
;; the walk crosses chunk boundaries (32 nodes) at every size around them
(is "(every? (fn [n] (let [s (into (sorted-set) (range n))] (and (= (range n) (seq s)) (= (reverse (range n)) (rseq s))))) [31 32 33 63 64 65 1000])" "true")
;; the JVM's classes: a map's seq is PersistentTreeMap$Seq both ways, keys/vals
;; and a set's seq are KeySeq/ValSeq, and none of them is chunked
(is "(map class [(seq sm) (rseq sm) (next (seq sm))])"
    "(clojure.lang.PersistentTreeMap$Seq clojure.lang.PersistentTreeMap$Seq clojure.lang.PersistentTreeMap$Seq)")
(is "(map class [(seq ss) (rseq ss) (keys sm) (vals sm)])"
    "(clojure.lang.APersistentMap$KeySeq clojure.lang.APersistentMap$KeySeq clojure.lang.APersistentMap$KeySeq clojure.lang.APersistentMap$ValSeq)")
(is "(chunked-seq? (seq sm))" "false")

;; --- subseq / rsubseq: clojure.core's definitions over seqFrom --------------
(is "(subseq ss > 10)" "(12 15 18 21 24 27 30 33 36 39 42 45 48 51 54 57 60 63 66 69 72 75 78 81 84 87 90 93 96 99)")
(is "(take 2 (subseq ss >= 9))" "(9 12)")
(is "(subseq ss < 10)" "(0 3 6 9)")
(is "[(subseq ss > 1000) (subseq ss < -1) (subseq (sorted-set) > 1)]" "[nil () nil]")
(is "(subseq ss > 10 < 30)" "(12 15 18 21 24 27)")
(is "(subseq ss >= 9 <= 30)" "(9 12 15 18 21 24 27 30)")
(is "(subseq ss > 50 < 30)" "()")
(is "(subseq ss < 50 > 90)" "()")
(is "(rsubseq ss < 10)" "(9 6 3 0)")
(is "(rsubseq ss <= 9)" "(9 6 3 0)")
(is "(take 2 (rsubseq ss > 10))" "(99 96)")
(is "(rsubseq ss >= 9 <= 30)" "(30 27 24 21 18 15 12 9)")
(is "(subseq sm > 10 <= 30)" "([12 120] [15 150] [18 180] [21 210] [24 240] [27 270] [30 300])")
(is "(rsubseq sm >= 10 < 30)" "([27 270] [24 240] [21 210] [18 180] [15 150] [12 120])")
(is "[(subseq rs > 5) (subseq rs < 5) (rsubseq rs > 5) (rsubseq rs <= 5)]" "[(3 1) (9 7) (1 3) (5 7 9)]")
(is "[(subseq rs >= 7 <= 3) (rsubseq rs >= 7 <= 3)]" "[(7 5 3) (3 5 7)]")
(is "(map class [(subseq ss > 10) (subseq ss < 10)])" "(clojure.lang.APersistentMap$KeySeq clojure.lang.LazySeq)")

;; clojure.lang.Sorted's own methods: seqFrom is the same seeked walk (it used
;; to filter the whole seq, and read a set's elements as entries)
(is "[(take 3 (.seqFrom ss 10 true)) (take 3 (.seqFrom sm 10 false)) (.seqFrom ss 100 true) (take 2 (.seq ss false))]"
    "[(12 15 18) ([9 90] [6 60] [3 30]) nil (99 96)]")

;; --- hash: the hash of the equivalent hash map/set, cached ------------------
(is "[(= (hash sm) (hash (into {} sm))) (= (hash ss) (hash (into #{} ss)))]" "[true true]")
(is "[(= (hash (sorted-map)) (hash {})) (= (hash (sorted-set)) (hash #{}))]" "[true true]")
(is "(hash (sorted-set-by compare [1 2] [3]))" "-949793658")
(is "(let [m (with-meta sm {:x 1})] [(= m sm) (= (hash m) (hash sm))])" "[true true]")
(is "[(get {sm 1} (into {} sm)) (get {(into {} sm) 1} sm) (contains? #{ss} (into #{} ss))]" "[1 1 true]")

;; --- = across the map/set types; the comparator is not part of equality ----
(is "[(= sm (into {} sm)) (= (into {} sm) sm) (= ss (into #{} ss)) (= (into #{} ss) ss)]" "[true true true true]")
(is "[(= sm (dissoc (into {} sm) 0)) (= sm (assoc (into {} sm) 0 1)) (= ss (conj (into #{} ss) 1000))]" "[false false false]")
(is "[(= sm (into (sorted-map) sm)) (= ss (into (sorted-set-by >) ss)) (= rs #{1 5 3 9 7})]" "[true true true]")
(is "[(= (sorted-map 1 2) (sorted-map 1 3)) (= (sorted-map 1 2) (sorted-set 1)) (= (sorted-set 1) [1])]" "[false false false]")
(is "[(= (sorted-map :a 1) (sorted-map :b 1)) (= (sorted-set :a) (sorted-set :b)) (= (sorted-set) #{}) (= (sorted-map) {})]" "[false false true true]")
(is "[(= (sorted-map 1 nil) {2 nil}) (= (sorted-map 1 nil) {1 nil}) (= (sorted-map nil 1) {nil 1})]" "[false true true]")

;; --- the tree under churn: a model check across splits and merges -----------
;; Random assoc/dissoc (and conj/disj) runs against a plain hash map as the
;; model, at sizes that cross many node splits and merges in both directions.
;; After each phase the whole observable surface is compared with the model, and
;; the tree's own shape invariant (bt-check: order, occupancy, depth, routing
;; keys) is asserted on the host value.
(ev "(defn lcg [s] (mod (+ (* s 1103515245) 12345) 2147483648))")
(ev "(defn churn [empty-coll n-ops key-space set? seed]
       (loop [i 0 s seed c empty-coll m {} checks []]
         (if (= i n-ops)
           [c m]
           (let [s1 (lcg s) s2 (lcg s1)
                 k (mod (quot s1 7) key-space)
                 add? (< (mod (quot s2 11) 100) (if (< i (quot n-ops 2)) 70 30))]
             (recur (inc i) s2
                    (if set? (if add? (conj c k) (disj c k)) (if add? (assoc c k i) (dissoc c k)))
                    (if add? (assoc m k i) (dissoc m k))
                    checks)))))")
(ev "(defn agrees? [c m set? cmp]
       (let [ks (sort cmp (keys m))
             probe (range -3 (+ 3 (if (seq ks) (reduce max ks) 0)))]
         (and (= (count c) (count m))
              (= (seq (if set? c (keys c))) (seq ks))
              (= (seq (if set? (rseq c) (map key (rseq c)))) (seq (reverse ks)))
              (or set? (= (seq (vals c)) (seq (map m ks))))
              (every? (fn [k] (if set? (= (contains? c k) (contains? m k))
                                       (= (get c k :none) (get m k :none)))) probe)
              (every? (fn [k] (= (seq (if set? (subseq c >= k) (map key (subseq c >= k))))
                                 (seq (filter #(>= (cmp % k) 0) ks)))) (take-nth 7 probe))
              (every? (fn [k] (= (seq (if set? (rsubseq c < k) (map key (rsubseq c < k))))
                                 (seq (reverse (filter #(neg? (cmp % k)) ks))))) (take-nth 7 probe))
              (= (reduce + 0 (if set? c (vals c))) (reduce + 0 (if set? ks (vals m))))
              (= c (if set? (set ks) m))
              (= (hash c) (hash (if set? (set ks) m))))))")
(define (tree-ok? src)
  (let ((c (jolt-compile-eval src "user")))
    (bt-check (scoll-tree c) (scoll-cmp c))))
(for-each
  (lambda (spec)
    (let ((n (car spec)) (space (cadr spec)) (seed (caddr spec)))
      (for-each
        (lambda (kind)
          (let* ((set? (memq kind '(set set-by)))
                 (empty (case kind ((map) "(sorted-map)") ((map-by) "(sorted-map-by >)")
                              ((set) "(sorted-set)") ((set-by) "(sorted-set-by >)")))
                 (cmp (if (memq kind '(map-by set-by)) "(fn [a b] (compare b a))" "compare"))
                 (run (format "(churn ~a ~a ~a ~a ~a)" empty n space (if set? "true" "false") seed)))
            (ev (format "(def churned ~a)" run))
            (ok (format "~a churn n=~a agrees" kind n)
                (string=? "true" (ev (format "(agrees? (first churned) (second churned) ~a ~a)"
                                             (if set? "true" "false") cmp))))
            (ok (format "~a churn n=~a keeps the tree shape" kind n)
                (tree-ok? "(first churned)"))))
        '(map map-by set set-by))))
  '((40 20 1) (400 150 7) (3000 1200 42) (6000 600 99)))
;; build up then drain to empty, in order and in reverse, through every size
(is "(let [s (into (sorted-set) (range 5000))] [(count (reduce disj s (range 5000))) (count (reduce disj s (reverse (range 5000))))])" "[0 0]")
(is "(let [s (into (sorted-set) (range 5000)) d (reduce disj s (range 0 5000 2))] [(count d) (take 3 d) (take 3 (rseq d))])" "[2500 (1 3 5) (4999 4997 4995)]")
(ok "draining every other key keeps the tree shape"
    (tree-ok? "(reduce disj (into (sorted-set) (range 5000)) (range 0 5000 2))"))
(ok "draining from the front keeps the tree shape"
    (tree-ok? "(reduce disj (into (sorted-set) (range 5000)) (range 4000))"))
(ok "draining from the back keeps the tree shape"
    (tree-ok? "(reduce disj (into (sorted-set) (range 5000)) (range 4999 999 -1))"))
;; the priority-queue loop: first + disj until empty, both ends
(is "(loop [s (into (sorted-set) (range 3000)) n 0] (if (seq s) (recur (disj s (first s)) (inc n)) n))" "3000")
(is "(loop [s (into (sorted-set) (range 3000)) n 0] (if (seq s) (recur (disj s (first (rseq s))) (inc n)) n))" "3000")
;; a missing key leaves the SAME coll; an identical value too (PersistentTreeMap
;; returns this); comparator-equal keys keep the first-inserted key
(is "(let [m (into (sorted-map) (map (fn [i] [i i]) (range 500)))] [(identical? m (dissoc m 1000)) (identical? m (dissoc m -1)) (identical? m (assoc m 7 7))])" "[true true true]")
(is "(let [s (into (sorted-set) (range 500))] [(identical? s (disj s 1000)) (identical? s (conj s 7))])" "[true true]")
(is "(let [m (assoc (into (sorted-map) (map (fn [i] [i i]) (range 500))) 7.0 :x)] [(get m 7) (some #(when (= 7 %) (class %)) (keys m))])" "[:x java.lang.Long]")
(is "(let [s (conj (into (sorted-set) (range 500)) 7.0)] [(count s) (some #(when (= 7 %) (class %)) s)])" "[500 java.lang.Long]")
(is "(let [m (into (sorted-map) (map (fn [i] [i i]) (range 500)))] [(find m 250) (find m 1000) (key (find m 250.0))])" "[[250 250] nil 250]")
;; nil is a key like any other under compare
(is "(let [m (into (sorted-map nil :n) (map (fn [i] [i i]) (range 100)))] [(first m) (get m nil) (count (dissoc m nil)) (first (dissoc m nil))])" "[[nil :n] :n 100 [0 0]]")
;; the leftmost/rightmost read across a deep tree
(is "(let [s (into (sorted-set) (shuffle (range 20000)))] [(first s) (first (rseq s)) (first (subseq s > 19998)) (first (rsubseq s < 1))])" "[0 19999 19999 0]")
;; reduce and reduce-kv stop at a reduced value mid-leaf and mid-tree
(is "(let [s (into (sorted-set) (range 1000))] [(reduce (fn [a x] (if (= x 77) (reduced a) (+ a x))) 0 s) (reduce-kv (fn [a k v] (if (= k 500) (reduced k) a)) 0 (into (sorted-map) (map (fn [i] [i i])) (range 1000)))])" "[2926 500]")

;; --- many entries at once: constructors, multi-arg assoc/conj, into ----------
;; A batch is sorted and bulk-loaded; its result must be conj after conj's.
(is "(let [m (into (sorted-map) [[1 :a] [1.0 :b] [2 :c]])] [m (class (key (first m)))])" "[{1 :b, 2 :c} java.lang.Long]")
(is "(let [m (sorted-map 3 :c 1 :a 1.0 :b)] [m (class (key (first m)))])" "[{1 :b, 3 :c} java.lang.Long]")
(is "(let [s (sorted-set 3 1 2 1.0 1)] [s (class (first s))])" "[#{1 2 3} java.lang.Long]")
(is "(conj (sorted-set 3) 1 2 1)" "#{1 2 3}")
(is "(assoc (sorted-map 1 :a) 0 :z 1 :b 2 :c)" "{0 :z, 1 :b, 2 :c}")
(is "(into (sorted-set-by >) (shuffle (range 50)))" (format "#{~{~a~^ ~}}" (reverse (iota 50))))
;; the existing key stays and the batch's last value wins, both ways round
(is "(let [m (into (sorted-map 1.0 :old 5 :five) [[1 :x] [2 :y] [1 :z] [3 :w]])] [m (class (key (first m)))])" "[{1.0 :z, 2 :y, 3 :w, 5 :five} java.lang.Double]")
(is "(let [big (into (sorted-map) (map (fn [i] [i i])) (range 100)) m (into big [[5 :x] [200 :y]])] [(count m) (get m 5) (get m 200) (get m 6)])" "[101 :x :y 6]")
(is "(let [a (into (sorted-set) (range 0 100 2)) b (into a (range 0 300 3))] [(count b) (= (seq b) (sort (distinct (concat (range 0 100 2) (range 0 300 3)))))])" "[133 true]")
;; nothing changed: the SAME coll, as conj after conj returns it
(is "(let [m (sorted-map 1 :a 2 :b)] [(identical? m (into m [[1 :a]])) (identical? m (into m [])) (identical? m (into m [[2 :b] [1 :a]])) (identical? m (into m [[1 :x]]))])" "[true true true false]")
(is "(let [s (sorted-set 1 2 3)] [(identical? s (into s [1 2 3])) (identical? s (conj s 3 2)) (identical? s (into s [4]))])" "[true true false]")
;; past the first few elements an into buffers; equal keys either side of the
;; switch keep the first key and the last value
(is "(into (sorted-map) (map (fn [i] [(mod i 5) i])) (range 20))" "{0 15, 1 16, 2 17, 3 18, 4 19}")
(is "(let [m (into (sorted-map) (map (fn [i] [(if (even? i) (mod i 5) (double (mod i 5))) i])) (range 20))] [(vals m) (map class (keys m))])"
    "[(15 16 17 18 19) (java.lang.Long java.lang.Double java.lang.Long java.lang.Double java.lang.Long)]")
(is "(let [m (into (sorted-map 3.0 :x) (map (fn [i] [(mod i 12) i])) (range 30))] [(count m) (get m 3) (class (key (nth (seq m) 3)))])" "[12 27 java.lang.Double]")
(is "(into (sorted-set-by >) (comp (filter odd?) (map #(* % %))) (range 30))" "#{841 729 625 529 441 361 289 225 169 121 81 49 25 9 1}")
(is "(let [s (sorted-set 1 2 3)] [(identical? s (into s (map identity) [1 2 3])) (identical? s (into s (map identity) (range 1 4))) (identical? s (into s (map inc) (range 20)))])" "[true true false]")
(is "(let [m (into (sorted-map) (map (fn [i] [i i])) (range 100))] [(identical? m (into m (map (fn [i] [i i])) (range 100))) (count (into m (map (fn [i] [i (- i)])) (range 100)))])" "[true 100]")
;; a transducer's completion step still lands in the coll
(is "(into (sorted-set) (comp (map inc) (partition-all 3) (map first)) (range 10))" "#{1 4 7 10}")
(is "(into (sorted-map) (map (fn [i] [(- i) i])) (range 5))" "{-4 4, -3 3, -2 2, -1 1, 0 0}")
;; a bad entry or an incomparable key still throws
(is "(try (into (sorted-map) [[1 2] [3]]) (catch Exception e :threw))" ":threw")
(is "(try (into (sorted-set) [1 :a]) (catch ClassCastException e :cce))" ":cce")
(is "(try (sorted-map 1 2 3) (catch IllegalArgumentException e :iae))" ":iae")
;; the receiver's metadata carries
(is "(meta (into (with-meta (sorted-set) {:a 1}) [3 1 2]))" "{:a 1}")
(is "(meta (into (with-meta (sorted-map) {:a 1}) (map (fn [i] [i i])) (range 3)))" "{:a 1}")
(ok "a bulk-loaded tree keeps the tree shape"
    (tree-ok? "(into (sorted-set) (shuffle (range 5000)))"))
(ok "a bulk-loaded ascending tree keeps the tree shape"
    (tree-ok? "(into (sorted-map) (map (fn [i] [i i])) (range 4097))"))
(ok "a merged batch keeps the tree shape"
    (tree-ok? "(into (into (sorted-set) (range 0 3000 2)) (shuffle (range 0 6000 3)))"))
(ok "a bulk-loaded tree takes single ops after it"
    (tree-ok? "(reduce disj (reduce conj (into (sorted-set) (range 1000)) (range 1000 1500)) (range 0 1500 3))"))
(is "(let [s (into (sorted-set) (shuffle (range 3000)))] [(= (range 3000) (seq s)) (= (reverse (range 3000)) (rseq s)) (count s)])" "[true true 3000]")

;; --- java.util.TreeMap / TreeSet against a model ---------------------------
;; navigation and bounded view sizes read the tree directly; each is checked
;; against a filter of the sorted keys, at bounds that are and are not keys
(ev "(def tm-keys (vec (distinct (map (fn [i] (* 3 (mod (* i 7919) 2000))) (range 3000)))))")
(ev "(def tmm (let [m (java.util.TreeMap.)] (doseq [k tm-keys] (.put m k (- k))) m))")
(ev "(def tm-sorted (sort tm-keys))")
(ev "(def tm-probes (range -5 6010 7))")
(is "(every? (fn [k] (and (= (.floorKey tmm k) (last (filter #(<= % k) tm-sorted)))
                          (= (.lowerKey tmm k) (last (filter #(< % k) tm-sorted)))
                          (= (.ceilingKey tmm k) (first (filter #(>= % k) tm-sorted)))
                          (= (.higherKey tmm k) (first (filter #(> % k) tm-sorted)))))
            tm-probes)" "true")
(is "(every? (fn [[lo hi]] (let [lo (min lo hi) hi (max lo hi)]
                              (and (= (.size (.subMap tmm lo true hi true)) (count (filter #(<= lo % hi) tm-sorted)))
                                   (= (.size (.subMap tmm lo false hi false)) (count (filter #(< lo % hi) tm-sorted)))
                                   (= (.size (.headMap tmm hi)) (count (filter #(< % hi) tm-sorted)))
                                   (= (.size (.headMap tmm hi true)) (count (filter #(<= % hi) tm-sorted)))
                                   (= (.size (.tailMap tmm lo)) (count (filter #(>= % lo) tm-sorted)))
                                   (= (.size (.tailMap tmm lo false)) (count (filter #(> % lo) tm-sorted)))
                                   (= (.size (.descendingMap (.subMap tmm lo true hi false))) (count (filter #(and (<= lo %) (< % hi)) tm-sorted))))))
            (map vector tm-probes (reverse tm-probes)))" "true")
(is "[(.size (.subMap tmm 7 true 7 true)) (.size (.subMap tmm 6 true 6 true)) (.size (.subMap tmm 6 false 6 true)) (.size (.headMap tmm -100)) (.size (.tailMap tmm 100000))]" "[0 1 0 0 0]")
(is "(= (keys (.headMap tmm 300 true)) (filter #(<= % 300) tm-sorted))" "true")
(is "(= (map key (.entrySet (.descendingMap (.tailMap tmm 5800)))) (reverse (filter #(>= % 5800) tm-sorted)))" "true")
(is "(let [m (java.util.TreeMap.)] [(.put m 1 :a) (.put m 1 :b) (.get m 1) (.remove m 1) (.remove m 1) (.size m)])" "[nil :a :b :b nil 0]")
(is "(let [s (java.util.TreeSet.)] [(.add s 3) (.add s 3) (.add s 1) (vec s) (.remove s 3) (.remove s 3) (vec s)])" "[true false true [1 3] true false [1]]")
(is "(let [s (java.util.TreeSet. [5 1 3])] [(.add s 3) (.add s 2) (vec s) (.add (.headSet s 3) 1) (try (.add (.headSet s 3) 4) (catch IllegalArgumentException _ :iae))])" "[false true [1 2 3 5] false :iae]")
(is "(let [m (java.util.TreeMap. (comparator >))] (doseq [k (shuffle (range 100))] (.put m k k)) [(.firstKey m) (.lastKey m) (.floorKey m 50) (.higherKey m 50) (.size (.headMap m 90))])" "[99 0 50 49 9]")
(is "(let [m (java.util.TreeMap.)] (.put m \"b\" 1) (.put m \"a\" 2) [(.firstKey m) (try (.put m 1 1) (catch ClassCastException _ :cce)) (try (.get m nil) (catch NullPointerException _ :npe))])" "[a :cce :npe]")
(is "(let [m (java.util.TreeMap.)] (.put m 1 :x) [(try (.put m 1.5 :y) (catch ClassCastException _ :cce)) (try (.floorKey m 2.0) (catch ClassCastException _ :cce))])" "[:cce :cce]")

;; --- scaling, as bytes allocated --------------------------------------------
(define (bytes-of thunk reps)
  (thunk)
  (let ((b0 (sstats-bytes (statistics))))
    (do ((i 0 (fx+ i 1))) ((fx= i reps)) (thunk))
    (quotient (- (sstats-bytes (statistics)) b0) reps)))
;; the same op over n and 4n entries; f is compiled once per size
(define (at-sizes src)
  (map (lambda (n)
         (let ((f (jolt-compile-eval
                    (format "(let [sm (into (sorted-map) (map (fn [i] [i i]) (range ~a))) ss (into (sorted-set) (range ~a)) ss2 (into (sorted-set) (range (inc ~a)))] (fn [] ~a))" n n n src)
                    "user")))
           (bytes-of (lambda () (jolt-invoke0 f)) 10)))
       '(10000 40000)))
(define (flat name src limit)
  (let* ((bs (at-sizes src)) (a (car bs)) (b (cadr bs)))
    (printf "  ~a: ~a bytes at 10k, ~a at 40k\n" name a b)
    (ok (format "~a does not grow with the coll (~a -> ~a bytes, limit ~a)" name a b limit)
        (and (<= a limit) (<= b limit)))))
;; each was O(n): the whole tree into a vector, then a cell per element
(flat "(first (seq sm))" "(first (seq sm))" 8000)
(flat "(doall (take 3 sm))" "(doall (take 3 sm))" 8000)
(flat "(first (rseq sm))" "(first (rseq sm))" 8000)
(flat "(first (keys sm))" "(first (keys sm))" 8000)
(flat "(second (vals sm))" "(second (vals sm))" 8000)
(flat "(first (subseq ss >= 5000))" "(first (subseq ss >= 5000))" 8000)
(flat "(first (rsubseq ss < 5000))" "(first (rsubseq ss < 5000))" 8000)
;; the hash is computed once per value; = of different counts is a count compare
(flat "a repeated (hash sm)" "(hash sm)" 256)
(flat "(= ss ss2) with different counts" "(= ss ss2)" 1024)
;; and = of equal colls walks the trees without building a hash coll, where the
;; conversion allocated a HAMT per side
(let* ((n 40000)
       (f (jolt-compile-eval (format "(let [a (into (sorted-set) (range ~a)) b (into (sorted-set) (range ~a))] (fn [] (= a b)))" n n) "user"))
       (per (quotient (bytes-of (lambda () (jolt-invoke0 f)) 10) n)))
  (printf "  = of two equal 40k sorted sets: ~a bytes/entry\n" per)
  (ok "= of two equal sorted sets walks both trees (<= 48 bytes per entry, was ~2600)" (<= per 48)))
;; a full walk stays linear and allocates no more than the eager vector did
(let* ((n 40000)
       (f (jolt-compile-eval (format "(let [sm (into (sorted-map) (map (fn [i] [i i]) (range ~a)))] (fn [] (loop [s (seq sm) a 0] (if s (recur (next s) (+ a (val (first s)))) a))))" n) "user"))
       (per (quotient (bytes-of (lambda () (jolt-invoke0 f)) 5) n)))
  (printf "  full next-walk of a 40k sorted map: ~a bytes/entry\n" per)
  (ok "a full walk allocates no more than the eager vector walk did (<= 200 bytes per entry, was ~376)" (<= per 200)))
;; a bounded subseq walks on from the node it found: one comparator call per
;; entry for the bound, plus the seek. Re-descending from the root for every node
;; without a right child made it O(n log n) calls (~24k here).
(is "(let [c (atom 0) s (into (sorted-set-by (fn [a b] (swap! c inc) (compare a b))) (range 4096))] (reset! c 0) (dorun (subseq s > -1 < 5000)) (<= @c 4200))" "true")
(is "(let [c (atom 0) s (into (sorted-set-by (fn [a b] (swap! c inc) (compare a b))) (range 4096))] (reset! c 0) (dorun (rsubseq s > -1 < 5000)) (<= @c 4200))" "true")

(printf "~a/~a passed~n" (- total fails) total)
(exit (if (zero? fails) 0 1))
