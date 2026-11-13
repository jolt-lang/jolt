;; Run from the generated collection project. See README.md for the commands.
(require '[workload.collections :as workload]
         '[jolt.profile :as profile]
         '[jolt.scheme :as scheme])

(def size 32768)
(def items (mapv inc (range size)))

(defn profile-builds []
  (let [probes (long-array [123 456])
        keys (vec (range size))
        pairs (mapv (fn [k] [k (inc k)]) keys)]
    (doseq [[kind kernel source expected batches]
            [[:vector workload/collections-vector-build-transient items 581 1200]
             [:map workload/collections-map-build-transient pairs 581 150]
             [:set workload/collections-set-build-transient keys 2 190]]]
      (let [run #(assert (= expected (kernel nil source size probes 2)))]
        (dotimes [_ 10] (run))
        (println kind)
        (profile/sample! {:interval-ms 1})
        (try (dotimes [_ batches] (run))
             (finally (profile/stop-sampling!)))
        (profile/sample-report {:limit 14})))))

;; All controls start with a fresh empty vector transient. Reserving includes
;; the cost of growing its buffer once. Each control uses the same persistent!.
(def reserve! (scheme/proc "tvec-ensure!"))
(def append-one!
  (scheme/eval-string "(lambda (t x) (tvec-conj1! t x) t)"))

;; This control assumes a fresh, unbased destination and a vector source.
;; Copy one contiguous leaf span at a time into the destination's own buffer.
(def copy-leaves!
  (scheme/eval-string
   "(lambda (t src)
      (let ((n (pvec-count src)))
        (tvec-ensure! t n)
        (let ((out (jolt-transient-buf t)))
          (let loop ((i 0))
            (if (fx=? i n)
                (begin (jolt-transient-n-set! t n) t)
                (let-values (((leaf off) (pv-leaf-for src i)))
                  (let ((len (fxmin (fx- n i) (fx- (vector-length leaf) off))))
                    (sa-vector-copy-range! out i leaf off (fx+ off len))
                    (loop (fx+ i len)))))))))"))

(defn reduce-build [step reserve?]
  (let [t (transient [])]
    (when reserve? (reserve! t size))
    (persistent! (reduce step t items))))

(def variants
  [[:conj #(reduce-build conj! false)]
   [:conj-reserved #(reduce-build conj! true)]
   [:append #(reduce-build append-one! false)]
   [:append-reserved #(reduce-build append-one! true)]
   [:copy-leaves #(persistent! (copy-leaves! (transient []) items))]])

(defn sample [build]
  (let [builds 16
        start (System/nanoTime)
        checksum (loop [i 0 total 0]
                   (if (< i builds)
                     (let [v (build) k (mod (* 7919 i) size)]
                       (recur (inc i) (+ total (nth v k))))
                     total))
        elapsed (- (System/nanoTime) start)
        expected (reduce + (map #(inc (mod (* 7919 %) size)) (range builds)))]
    (assert (= expected checksum))
    (/ (double elapsed) (* size builds))))

(defn vector-controls []
  (doseq [[_ build] variants]
    (assert (= items (build)))
    (dotimes [_ 10] (sample build)))
  (println "round,variant,ns_per_element")
  (dotimes [round 7]
    (dotimes [j (count variants)]
      (let [[label build] (nth variants (mod (+ round j) (count variants)))]
        (println (str (inc round) "," (name label) "," (sample build)))))))

(case (first *command-line-args*)
  "profile" (profile-builds)
  "vector" (vector-controls))
