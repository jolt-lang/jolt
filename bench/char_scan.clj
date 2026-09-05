;; char-scan — the CHARACTER-LOOP workload: walking a string one code point at a
;; time through `.charAt`, with the numeric casts that hinted Clojure puts around
;; it. Per-iteration work is one host method call and two or three coercions, so
;; the cast primitives and interop dispatch dominate rather than any algorithm.
;;
;; This is the regime `mathfns` and `arrays` miss. They exercise arithmetic on
;; values that are already the right type; here every character crosses the
;; char->int->long boundary, which is a different set of primitives. Two of them
;; used to be startlingly expensive on jolt:
;;
;;   - `unchecked-int` and `unchecked-long` fell through a generic cond to a
;;     `truncate` call plus generic bitwise masking, 44.7 ns for what the JVM does
;;     in 2.7. `long` was worse in a way that is easy to miss: Chez fixnums are
;;     61-bit, so the +-2^63 bounds `long` range-checks against are BIGNUMS, and
;;     every (long x) on an ordinary integer paid two fixnum-vs-bignum compares.
;;   - a `case` over small integer states, which is how a hand-rolled scanner
;;     dispatches, and which pays a cast per branch.
;;
;; The shape is honeysql's. honey.sql/alphanumeric? is a regex rewritten as a
;; character state machine, called once per entity part inside format-entity, and
;; it ran at 30x the JVM — almost entirely in the casts, not the loop.
;;
;; `count-digits-hinted` adds the fourth shape: the SAME walk with the index
;; declared `^int` on the parameter rather than cast at each use, which is how
;; ported JVM code is actually written.
;;
;; Portable Clojure (jolt + JVM Clojure).
;;   bench/run.sh char-scan 40000
(ns char-scan)

(def entities ["table" "some_column" "a" "x1" "SELECT" "order_by_2" "_leading" "42"])
(def sentence "the quick brown fox jumps over the lazy dog 0123456789")

;; --- honeysql's alphanumeric?, verbatim in shape ------------------------------
;; `^(?:[0-9_]+|[A-Za-z_][A-Za-z0-9_]*)$` as a state machine. The three casts per
;; character — the index, the char, and the widening — are the point.
(defn alphanumeric? [^String s]
  (let [leading-underscore 1
        numeric 2
        identifier 3
        dead 4
        n (long (.length s))]
    (loop [i (unchecked-long 0)
           state (unchecked-long 0)]
      (if (or (= state dead) (>= i n))
        (or (= state leading-underscore) (= state numeric) (= state identifier))
        (let [c  (.charAt s (unchecked-int i))
              c  (unchecked-long (unchecked-int c))
              ni (unchecked-inc i)]
          (case state
            0 (cond (or (and (>= c 65) (<= c 90)) (and (>= c 97) (<= c 122)))
                    (recur ni identifier)
                    (and (>= c 48) (<= c 57)) (recur ni numeric)
                    (= c 95) (recur ni leading-underscore)
                    :else (recur ni dead))
            1 (cond (or (and (>= c 65) (<= c 90)) (and (>= c 97) (<= c 122)))
                    (recur ni identifier)
                    (and (>= c 48) (<= c 57)) (recur ni identifier)
                    (= c 95) (recur ni leading-underscore)
                    :else (recur ni dead))
            2 (if (or (and (>= c 48) (<= c 57)) (= c 95))
                (recur ni numeric)
                (recur ni dead))
            3 (if (or (and (>= c 65) (<= c 90)) (and (>= c 97) (<= c 122))
                      (and (>= c 48) (<= c 57)) (= c 95))
                (recur ni identifier)
                (recur ni dead))
            (recur ni dead)))))))

;; --- the same walk with no state machine: casts and .charAt only --------------
(defn sum-code-points ^long [^String s]
  (let [n (long (.length s))]
    (loop [i 0 acc 0]
      (if (>= i n)
        acc
        (recur (unchecked-inc i)
               (unchecked-add acc (unchecked-long (unchecked-int (.charAt s (unchecked-int i))))))))))

;; --- the checked casts, which are the ones ordinary code writes ---------------
(defn count-digits ^long [^String s]
  (let [n (int (.length s))]
    (loop [i 0 acc 0]
      (if (>= i n)
        acc
        (let [c (long (int (.charAt s (int i))))]
          (recur (inc i) (if (and (>= c 48) (<= c 57)) (inc acc) acc)))))))

;; --- the same walk with the index HINTED ^int, not cast per use ---------------
;; ^int is the tag ported JVM code writes on an index parameter — clojure.core's
;; own gvec is written in them. Reference Clojure supports long and double
;; primitive parameters only, so ^int there is inert documentation; jolt has no
;; 32-bit integer, so it can read one as the fixnum promise ^long is. Without that
;; the whole body is generic arithmetic over a parameter the source has already
;; declared. Same work as `count-digits`, with the casts moved to the signature.
(defn count-digits-hinted ^long [^String s ^int from]
  (let [n (.length s)]
    (loop [i from acc 0]
      (if (>= i n)
        acc
        (let [c (long (int (.charAt s i)))]
          (recur (inc i) (if (and (>= c 48) (<= c 57)) (inc acc) acc)))))))

(defn run [iters]
  (loop [i 0 acc 0]
    (if (< i iters)
      (recur (inc i)
             (unchecked-add
              acc
              (unchecked-add
               (unchecked-add (reduce (fn [a e] (if (alphanumeric? e) (inc a) a)) 0 entities)
                              (sum-code-points sentence))
               (unchecked-add (count-digits sentence)
                              (count-digits-hinted sentence 0)))))
      acc)))

(defn -main [& args]
  (let [iters (if (seq args) (Integer/parseInt (first args)) 40000)]
    (dotimes [_ 2] (run (quot iters 4)))                 ; warmup
    (let [runs 3
          ts (mapv (fn [_]
                     (let [t0 (System/currentTimeMillis)
                           r (run iters)
                           el (- (System/currentTimeMillis) t0)]
                       (when (zero? r) (println "unexpected zero"))
                       el))
                   (range runs))]
      (println "runs:" ts)
      (println "mean:" (quot (reduce + ts) runs) "ms"))))
