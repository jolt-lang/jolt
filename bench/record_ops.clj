;; record-ops — a defrecord used AS A MAP: the clojure.core collection functions
;; applied to records whose type the compiler cannot see.
;;
;; A record carries its layout — its fields, in order, at fixed slots — so on the
;; JVM every one of these is a direct method its class generates: count is a
;; field count, containsKey and valAt a `case` on the keyword, assoc a
;; constructor call, kvreduce a walk of the fields. None of that needs the
;; caller to know the type. `typed-records` and `mono-dispatch` measure the paths
;; where the compiler DOES know it (field reads, protocol calls on `this`); this
;; row is the other half, and the one #1470 found 40-60% slower than the same
;; work on a plain map, with count 10x, reduce-kv 18x and into 6x:
;;
;;   - every op walked the host's whole extension-arm list before reaching the
;;     record arm, then looked the type's declared methods up by name;
;;   - (R. …) went through the runtime class lookup and a variadic constructor;
;;   - map destructuring lost the record's type through its (if (seq? m) …) and
;;     read every key generically.
;;
;; The records arrive from a top-level vector through `reduce`, so nothing infers
;; their type: each op takes the generic path a library function would.
;;
;; Portable Clojure (jolt + JVM Clojure).
;;   bench/run.sh record-ops 20000
(ns record-ops)

(defrecord Order [id qty price status])

(def orders
  (mapv (fn [i] (if (even? i)
                  (->Order i (inc (mod i 7)) (* 1.25 i) :open)
                  (map->Order {:id i :qty 1 :price 2.5 :status :held :note "x"})))
        (range 64)))

(defn- order-work ^long [o]
  (let [{:keys [id qty status]} o                      ; map destructuring
        o2 (-> o (assoc :qty (inc qty)) (update :id inc)) ; assoc/update a field
        o3 (assoc o2 :seen true)                          ; ...and a non-field key
        o4 (Order. id qty 1.0 status)]                    ; the class constructor
    (+ (count o3)
       (if (contains? o3 :note) 1 0)
       (reduce-kv (fn [^long a k v] (if (number? v) (inc a) a)) 0 o2)
       (count (into {} o3))
       (count (keys o))
       (if (= o4 (Order. id qty 1.0 status)) 1 0)
       (if (= :open (:status (merge o4 {:status :open}))) 1 0)
       (bit-and (hash o4) 1))))

(defn run ^long [^long n]
  (loop [p 0 acc 0]
    (if (< p n)
      (recur (inc p) (+ acc (reduce (fn [^long a o] (+ a (order-work o))) 0 orders)))
      acc)))

(defn -main [& args]
  (let [n (if (seq args) (Integer/parseInt (first args)) 2000)]
    (dotimes [_ 2] (run (quot n 4)))                     ; warmup
    (let [runs 3
          times (mapv (fn [_]
                        (let [t0 (System/nanoTime)
                              r (run n)
                              ms (/ (- (System/nanoTime) t0) 1000000.0)]
                          [ms r]))
                      (range runs))
          mss (mapv first times)
          mean (/ (reduce + mss) runs)]
      (println "record-ops n" n "result" (second (first times)))
      (println "runs:" (mapv (fn [t] (/ (Math/round (* t 10.0)) 10.0)) mss))
      (println "mean:" (/ (Math/round (* mean 10.0)) 10.0) "ms"))))
