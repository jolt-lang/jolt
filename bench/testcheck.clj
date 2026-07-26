;; testcheck — 64-BIT INTEGER ARITHMETIC and generator machinery, the axis the
;; AOT suite doesn't cover. A Chez fixnum is 61-bit, so a genuine 64-bit value is
;; a heap bignum; the SplitMix PRNG behind clojure.test.check is the worst case,
;; since every rand-long is a handful of 64-bit ops.
;;
;; Measured in RUN mode (`jolt run`), not as an AOT binary: this is library code
;; loaded through the normal require path, which is how a test suite hits it.
;;
;; The first two rows isolate one cost each — 64-bit arithmetic, and open-world
;; deftype allocation + protocol dispatch. The rest are real test.check entry
;; points, so they carry both plus the rose-tree machinery.
;;
;; Portable Clojure (jolt + JVM Clojure). Same warmup/mean convention as the rest
;; of the suite: 2 warmup runs at quarter size, then the mean of 3 timed runs.
;;   bench/testcheck.sh
(ns testcheck
  (:require [clojure.test.check.random :as random]
            [clojure.test.check.generators :as gen]))

;; --- 1. 64-bit arithmetic, no allocation, no dispatch ------------------------
;; SplitMix64's finalizing mix: three xor-shifts and two 64-bit multiplies. Every
;; intermediate leaves fixnum range, so this is the bignum path end to end.
(defn mix-64 [n]
  (let [n (unchecked-multiply (bit-xor n (unsigned-bit-shift-right n 33))
                              -49064778989728563)
        n (unchecked-multiply (bit-xor n (unsigned-bit-shift-right n 33))
                              -4265267296055464877)]
    (bit-xor n (unsigned-bit-shift-right n 33))))

(defn run-mix [n]
  (loop [i 0 acc 0]
    (if (< i n) (recur (inc i) (bit-xor acc (mix-64 (unchecked-add i 1)))) acc)))

;; --- 2. deftype allocation + protocol dispatch -------------------------------
;; What a generator IS: a record wrapping a function, called through a protocol.
;; Open-world, so the call can't devirtualize.
(defprotocol IBoxed (unboxed [this]))
(deftype Boxed [v] IBoxed (unboxed [this] v))

(defn run-boxed [n]
  (loop [i 0 acc 0]
    (if (< i n) (recur (inc i) (unchecked-add acc (unboxed (Boxed. i)))) acc)))

;; --- 3. the PRNG itself ------------------------------------------------------
(defn run-random [n]
  (loop [i 0 r (random/make-random 42) acc 0]
    (if (< i n)
      (let [pair (random/split r)]
        (recur (inc i) (nth pair 1)
               (bit-xor acc (random/rand-long (nth pair 0)))))
      acc)))

;; --- 4/5. real generators ----------------------------------------------------
(defn run-large-int [n] (count (gen/sample gen/large-integer n)))
(defn run-gen-vector [n] (count (gen/sample (gen/vector gen/large-integer) n)))

(def WORKLOADS
  [["mix-64" run-mix 100000]
   ["deftype+protocol" run-boxed 100000]
   ["split + rand-long" run-random 20000]
   ["gen/large-integer" run-large-int 2000]
   ["(gen/vector gen/large-integer)" run-gen-vector 500]])

(defn timed [f n]
  (let [t0 (System/nanoTime)]
    (f n)
    (/ (- (System/nanoTime) t0) 1000000.0)))

(defn -main [& _]
  (doseq [w WORKLOADS]
    (let [label (nth w 0) f (nth w 1) n (nth w 2)]
      (dotimes [_ 2] (f (max 1 (quot n 4))))               ; warmup
      (let [mss (mapv (fn [_] (timed f n)) (range 3))
            mean (/ (reduce + mss) 3)]
        (println (str label "|" n "|" (/ (Math/round (* mean 10.0)) 10.0)))))))
