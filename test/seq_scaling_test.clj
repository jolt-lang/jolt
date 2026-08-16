;; Seq-tier shape gates.
;;
;; jolt's map/filter/remove are chunk-preserving: a chunked source has the whole
;; 32-element chunk realized at once and the result is itself chunked. The point
;; of doing that is that walking a CHUNKABLE source (vector, range) should cost
;; less per element than walking an unchunkable one (a list) — the chunk is
;; resolved once and its elements read straight out of it.
;;
;; That was not true. cseq-chunked re-derived the element's leaf through
;; pvec-nth-d on every element, and vec->seq re-descended the trie per element.
;; Over a 100k vector that cost 107.4 ns/elem against 79.7 for the same map over
;; a LIST — so chunking was worse than not chunking. Resolving the leaf once per
;; 32-block took the vector row to 61.9, i.e. below the list row where it
;; belongs. (The JVM's equivalents are 16.7 and 49.2 over 32 elements.)
;;
;; So the gate is a SHAPE, judged in one process: per-element cost over a vector
;; must come in under the same over a list. It walks with COUNT rather than
;; doall — doall retains n cells per repetition, so the two arms end up compared
;; under different GC pressure and the ratio wanders across runs. An implementation that stops
;; consuming chunks, or that goes back to re-deriving the leaf per element, makes
;; the two equal again and fails here. Absolute ns ceilings are deliberately NOT
;; asserted — they flake across machines and CI (see the readscaling family).

(ns seq-scaling-test)

;; LARGE, and that matters: a vector of 32 keeps every element in its tail array,
;; so pvec-nth-d never descends the trie and the defect is invisible — the same
;; probe at n=32 showed 93.8 ns/elem both before and after the fix. The descent
;; only bites once the vector outgrows its tail, where the fix measured
;; 107.4 -> 61.9 ns/elem. Before it, map over a 100k VECTOR (107.4) was slower
;; than over a 100k LIST (79.7): chunking was not merely failing to pay, it was
;; costing more than walking cons cells.
(def ^:private n 100000)
(def ^:private reps 5)

(defn- timed [f]
  (let [t (System/nanoTime)]
    (f)
    (- (System/nanoTime) t)))

(defn- best-of [k f]
  (f)                                    ; warm before the first sample
  (reduce min (map (fn [_] (timed f)) (range k))))

(def ^:private failures (atom 0))

(defn- judge [label t-chunkable t-plain ceiling detail]
  (let [ratio (double (/ t-chunkable (max 1 t-plain)))]
    (println (format "seq %-22s chunkable %5dms vs plain %5dms, ratio %5.2f (ceiling %.2f)"
                     label (quot t-chunkable 1000000) (quot t-plain 1000000) ratio ceiling))
    (when (> ratio ceiling)
      (println (str "FAIL seq " label ": " detail))
      (swap! failures inc))))

(defn -main [& _]
  (let [v (vec (range n))
        l (apply list (range n))]
    ;; the two sources must agree on values, or the ratio compares nothing
    (when-not (and (= (seq v) (seq l))
                   (= (map inc v) (map inc l))
                   (= (filter odd? v) (filter odd? l))
                   (= (count (chunk-first (seq v))) 32)
                   (chunked-seq? (seq v))
                   (not (chunked-seq? (seq l))))
      (println "FAIL seq-scaling: vector and list sources disagree, or the vector source is not chunked")
      (System/exit 1))

    ;; NOT gated: (seq v) against (seq l). A list IS already a seq, so that side
    ;; allocates nothing while the vector side must build cells — the comparison
    ;; measures construction against no construction, not chunking. map/filter
    ;; below are the fair test: both sides build a fresh cell per element, so the
    ;; only difference left is whether the source's chunk is exploited.

    ;; the chunk-preserving transforms
    (judge "map" (best-of 5 #(dotimes [_ reps] (count (map inc v))))
                 (best-of 5 #(dotimes [_ reps] (count (map inc l))))
           1.0
           "chunked map costs as much per element as unchunked map — either map stopped taking the na-chunked-seq? branch, or cseq-chunked is re-deriving the element's leaf per element (host/chez/seq.ss)")

    ;; Coarser than the map section: filter reads 0.84 with the defect present
    ;; and 0.68 without it, so it passes either way and is a "does this still
    ;; consume chunks at all" check. MAP is the section that red-proofs the leaf
    ;; resolution — it goes above 1.00 the moment the per-element descent returns.
    (judge "filter" (best-of 5 #(dotimes [_ reps] (count (filter odd? v))))
                    (best-of 5 #(dotimes [_ reps] (count (filter odd? l))))
           1.0
           "chunked filter costs as much per element as unchunked filter (host/chez/seq.ss)")

    (if (pos? @failures)
      (do (println (str "seq-scaling: " @failures " section(s) failed"))
          (System/exit 1))
      (println "seq-scaling: passed"))))

(-main)
