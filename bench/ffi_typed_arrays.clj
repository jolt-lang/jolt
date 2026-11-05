;; Typed jolt.ffi array moves: read-array, write-array and read-into! over 6000
;; elements, plus System/arraycopy between two double-arrays. Jolt-only: there
;; is no JVM FFI reference for this API. Each case reports the min and median of
;; its samples, in microseconds per call, so an A/B/A run can show both columns.
;; A case the API under test does not have yet prints n/a.
(ns ffi-typed-arrays
  (:require [jolt.ffi :as ffi]))

(defn- sample-us [f k]
  (let [t0 (System/nanoTime)]
    (dotimes [_ k] (f))
    (/ (- (System/nanoTime) t0) 1000.0 k)))

(defn- run-case [label f k samples]
  (if (try (f) true (catch Throwable _ false))
    (do (dotimes [_ 20] (f))
        (let [xs (sort (repeatedly samples #(sample-us f k)))]
          (println (format "%-34s min %9.2f us  median %9.2f us"
                           label (first xs) (nth xs (quot (count xs) 2))))))
    (println (format "%-34s n/a" label))))

(defn -main [& args]
  (let [n (if (seq args) (Integer/parseInt (first args)) 6000)
        k 200
        samples 15
        p (ffi/alloc (* 8 n))
        small (ffi/alloc 4096)
        da (double-array (map #(+ 0.5 %) (range n)))
        db (double-array n)
        fa (float-array (map #(+ 0.5 %) (range n)))
        la (long-array (range n))
        ia (int-array (range n))
        sa (short-array (map #(rem % 30000) (range n)))
        ba (byte-array n)
        f48 (float-array 48)]
    (ffi/write-array p :double da)
    (doseq [[label f]
            [["read-array :double" #(ffi/read-array p :double n)]
             ["read-array :float" #(ffi/read-array p :float n)]
             ["read-array :int64" #(ffi/read-array p :int64 n)]
             ["read-array :int32" #(ffi/read-array p :int32 n)]
             ["read-array :int16" #(ffi/read-array p :int16 n)]
             ["read-array bytes" #(ffi/read-array p n)]
             ["write-array :double" #(ffi/write-array p :double da)]
             ["write-array :float <- float-array" #(ffi/write-array p :float fa)]
             ["write-array :float <- double-arr" #(ffi/write-array p :float da)]
             ["write-array :int64" #(ffi/write-array p :int64 la)]
             ["write-array :int32" #(ffi/write-array p :int32 ia)]
             ["write-array :int16" #(ffi/write-array p :int16 sa)]
             ["write-array bytes" #(ffi/write-array p ba)]
             ["write-array :float 48" #(ffi/write-array small :float f48)]
             ["read-into! bytes" #(ffi/read-into! p :byte ba 0 0 n)]
             ["read-into! 16 bytes" #(ffi/read-into! p :byte ba 0 0 16)]
             ["write-array 16 bytes" #(ffi/write-array p :byte ba 0 0 16)]
             ["read-into! :double" #(ffi/read-into! p :double db)]
             ["read-into! :float -> float-array" #(ffi/read-into! p :float fa)]
             ["read-into! :int32" #(ffi/read-into! p :int32 ia)]
             ["write-array :double first half" #(ffi/write-array p :double da 0 0 (quot n 2))]
             ["System/arraycopy double" #(System/arraycopy da 0 db 0 n)]
             ["System/arraycopy long" #(System/arraycopy la 0 la 1 (dec n))]]]
      (run-case label f k samples))))
