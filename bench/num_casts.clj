;; num-casts: the DOUBLE-TO-INTEGER cast primitives, `long`, `int`, `unchecked-long`
;; and `unchecked-int` applied to a double, against `long` of a value that is
;; already a fixnum as the floor.
;;
;; `char-scan` watches the casts on fixnum input. This file watches the other
;; branch: a double going through the checked casts' range check. That check
;; used to compare the flonum against the type's bounds held as EXACT integers,
;; and for `long` those (+-2^63) are bignums on every Chez (61-bit fixnums on
;; 64-bit, 30-bit on 32-bit), so each cast paid two flonum-vs-bignum generic
;; compares and then a second exact-vs-bignum check after truncation. On the Wii
;; (32-bit, portable bytecode) that was 135 us per (long x) against 26 us for
;; (unchecked-long x), and swapping the casts in one program's drawing code took
;; its draw from 252 ms to 60 ms. `unchecked-long` saturated through the same
;; bignum compares.
;;
;; The doubles come from an array built at run time from the size, spread across
;; the int range with fractional parts, so nothing folds at compile time, and
;; each phase sums its results so the cast is not dead code.
;;
;; Portable Clojure (jolt + JVM Clojure).
;;   bench/run.sh num-casts 2000000
(ns num-casts)

(def ^:private width 1024)

(defn- make-doubles [n]
  ;; values in (-2^31, 2^31) with a fractional part; n only seeds the spread so
  ;; the array content depends on the run, not on the source
  (let [a (double-array width)
        step (/ 4294967290.0 width)]
    (dotimes [i width]
      (aset a i (+ -2147483645.0 (* i step) (/ (mod (+ i n) 7) 8.0))))
    a))

(defn- make-longs []
  (let [a (long-array width)]
    (dotimes [i width] (aset a i (- (* i 4194303) 2147483645)))
    a))

(def ^:private dbl-arr (atom nil))
(def ^:private lng-arr (atom nil))

(defn- long-of-double [n]
  (let [^doubles a @dbl-arr]
    (loop [i 0 acc 0]
      (if (< i n)
        (recur (inc i) (+ acc (long (aget a (rem i width)))))
        acc))))

(defn- int-of-double [n]
  (let [^doubles a @dbl-arr]
    (loop [i 0 acc 0]
      (if (< i n)
        (recur (inc i) (+ acc (int (aget a (rem i width)))))
        acc))))

(defn- unchecked-long-of-double [n]
  (let [^doubles a @dbl-arr]
    (loop [i 0 acc 0]
      (if (< i n)
        (recur (inc i) (+ acc (unchecked-long (aget a (rem i width)))))
        acc))))

(defn- unchecked-int-of-double [n]
  (let [^doubles a @dbl-arr]
    (loop [i 0 acc 0]
      (if (< i n)
        (recur (inc i) (+ acc (unchecked-int (aget a (rem i width)))))
        acc))))

(defn- long-of-fixnum [n]
  (let [^longs a @lng-arr]
    (loop [i 0 acc 0]
      (if (< i n)
        (recur (inc i) (+ acc (long (aget a (rem i width)))))
        acc))))

(def ^:private phases
  [["long-of-double" long-of-double]
   ["int-of-double" int-of-double]
   ["unchecked-long-of-double" unchecked-long-of-double]
   ["unchecked-int-of-double" unchecked-int-of-double]
   ["long-of-fixnum" long-of-fixnum]])

(defn run [n]
  (reduce (fn [acc [_ f]] (+ acc (f n))) 0 phases))

(defn- ms-since [t0] (/ (- (System/nanoTime) t0) 1000000.0))
(defn- r1 [x] (/ (Math/round (* x 10.0)) 10.0))

(defn -main [& args]
  (let [n (if (seq args) (Integer/parseInt (first args)) 2000000)]
    (reset! dbl-arr (make-doubles n))
    (reset! lng-arr (make-longs))
    (dotimes [_ 2] (run (quot n 4)))                     ; warmup
    (let [runs 5
          ;; the phase split, for reading by hand; `mean:` below is the row
          split (mapv (fn [[nm f]] (let [t0 (System/nanoTime)] (f n) [nm (ms-since t0)])) phases)
          times (mapv (fn [_]
                        (let [t0 (System/nanoTime)
                              r (run n)]
                          [(ms-since t0) r]))
                      (range runs))
          mss (mapv first times)
          mean (/ (reduce + mss) runs)]
      (println "num-casts n" n "result" (second (first times)))
      (println "phases:" (apply str (interpose "  " (map (fn [[nm ms]] (str nm " " (r1 ms) " ms")) split))))
      (println "runs:" (mapv r1 mss))
      (println "mean:" (r1 mean) "ms"))))
