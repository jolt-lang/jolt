;; Lazy seq realization must not keep what it has walked past.
;;
;; Every case walks past a run of N elements to find the one it answers with --
;; a filter skip, a run of duplicates, a chain of lazy seqs that each answer
;; another -- and must do it in constant memory, as the JVM does: `make
;; lazyretain` runs each in its own process under JOLT_MAX_HEAP=256m, and the
;; JVM runs every one of them in 96MB. Two things made jolt keep the run alive:
;;
;;   - a lazy seq whose body answers another lazy seq was forced INSIDE the body
;;     (coll->cells), so a run of skips became a recursion as deep as the run,
;;     each frame pinning its place in the source. The reference returns the
;;     inner seq unforced and walks the chain in a loop (LazySeq.realize/unwrap),
;;     as lazy-bridge.ss now does. writ's prover held 31M cells (2.4GB) this way
;;     through clojure.core/keep, where the JVM held 256MB.
;;   - the thunk kept what it closed over until it returned. The reference's
;;     lazy-seq thunk is ^:once, and its captures are cleared as it runs; a thunk
;;     looping over a run (distinct, for's :when) otherwise pins the head it
;;     started from (backend emit-fn, :once).
;;
;; Each case prints "<name> <answer>" and the gate compares the answers.
(ns lazy-retention-test)

(def N 3000000)

(def cases
  {"filter"          [N #(first (filter (fn [x] (= x N)) (range (inc N))))]
   "remove"          [N #(first (remove (fn [x] (< x N)) (range (inc N))))]
   "keep"            [N #(first (keep (fn [x] (when (= x N) x)) (range (inc N))))]
   "keep-indexed"    [N #(first (keep-indexed (fn [i x] (when (= i N) x)) (range (inc N))))]
   "drop-while"      [N #(first (drop-while (fn [x] (< x N)) (range (inc N))))]
   "distinct"        [2 #(second (distinct (concat (repeat N 1) [2])))]
   "dedupe"          [2 #(second (dedupe (concat (repeat N 1) [2])))]
   "mapcat-empties"  [N #(first (mapcat (fn [x] (if (= x N) [x] [])) (range (inc N))))]
   "for-when"        [N #(first (for [x (range (inc N)) :when (= x N)] x))]
   "remove-nils"     [1 #(first (remove nil? (concat (repeat N nil) [1])))]
   "flatten-empties" [1 #(first (flatten (concat (repeat N []) [[1]])))]
   "interleave-drop" [(quot N 2) #(first (drop N (interleave (range (inc N)) (range (inc N)))))]
   "lazy-returns-lazy" [:done #(let [f (fn f [n] (lazy-seq (if (pos? n) (f (dec n)) (list :done))))] (first (f N)))]
   "cons-after-skip" [N #(let [f (fn f [xs] (lazy-seq (when-let [s (seq xs)] (if (= (first s) N) (cons (first s) nil) (f (rest s))))))] (first (f (range (inc N)))))]})

(defn -main [& [only]]
  (if only
    (let [[want f] (get cases only)]
      (println only (if (= want (f)) "ok" "WRONG")))
    (doseq [k (sort (keys cases))] (println k))))

(apply -main *command-line-args*)
