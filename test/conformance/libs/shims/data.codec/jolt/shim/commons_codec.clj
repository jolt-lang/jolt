;; Stand-in for org.apache.commons.codec.binary.Base64, the Apache Commons
;; Codec JVM jar that clojure.data.codec's suite uses as its ORACLE: every
;; encode/decode property check compares jolt's result against this class's
;; statics, and the generators build inputs from its encodeBase64 output. The
;; jar does not exist on jolt, so the class is registered here with the two
;; statics the suite calls, implemented to RFC 4648 independently of the
;; library's own base64.
;;
;; A shim rather than an edit: changing the suite to drop its oracle would
;; silently turn the recorded tally into a measure of our own source. The
;; library under test is unmodified.
(ns jolt.shim.commons-codec)

(def ^:private alphabet
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

;; char code -> 6-bit value, for the decode direction.
(def ^:private dec-table
  (let [t (byte-array 128)]
    (doseq [i (range 64)]
      (aset t (int (nth alphabet i)) (unchecked-byte i)))
    t))

(defn- valid-char? [c]
  (or (= 61 c) (<= 65 c 90) (<= 97 c 122) (<= 48 c 57) (= 43 c) (= 47 c)))

;; RFC 4648 §4. A stored byte is signed, so each read is masked to 0-255
;; before the bit juggling; output chars are all < 128.
(defn- encode* [input]
  (let [n (alength input)]
    (loop [i 0 acc []]
      (if (>= i n)
        (byte-array (map byte acc))
        (let [left (- n i)
              b0 (bit-and (aget input i) 255)
              b1 (if (< 1 left) (bit-and (aget input (inc i)) 255) 0)
              b2 (if (< 2 left) (bit-and (aget input (+ i 2)) 255) 0)
              c0 (int (nth alphabet (bit-shift-right b0 2)))
              c1 (int (nth alphabet (bit-or (bit-shift-left (bit-and b0 3) 4)
                                            (bit-shift-right b1 4))))
              c2 (int (nth alphabet (bit-or (bit-shift-left (bit-and b1 15) 2)
                                            (bit-shift-right b2 6))))
              c3 (int (nth alphabet (bit-and b2 63)))]
          (recur (+ i 3)
                 (case (min left 3)
                   1 (conj acc c0 c1 61 61)
                   2 (conj acc c0 c1 c2 61)
                   3 (conj acc c0 c1 c2 c3))))))))

;; RFC 4648 §4. Total length is a multiple of 4; the trailing '=' bytes mark
;; how many of the last group's bytes are padding, so the loop drops that many.
;; Decoded bytes may exceed the checked byte range, hence unchecked-byte.
;; Invalid characters and lengths throw, as the JVM does.
(defn- decode* [input]
  (let [n (alength input)
        pad (loop [i (dec n) p 0]
              (if (and (>= i 0) (= 61 (aget input i)))
                (recur (dec i) (inc p))
                p))
        body (- n pad)]
    (when (or (> pad 2) (not= 0 (mod n 4)))
      (throw (IllegalArgumentException. "Invalid Base64 length")))
    (loop [i 0 acc []]
      (if (>= i body)
        (byte-array (map unchecked-byte (drop-last pad acc)))
        ;; Mask each byte to 0-255 (a stored byte is signed) and validate BEFORE
        ;; the table lookup — dec-table is 128 wide, so an out-of-alphabet
        ;; character would otherwise throw an index error instead of the
        ;; IllegalArgumentException the JVM raises.
        (let [c0 (bit-and (aget input i) 255)
              c1 (bit-and (aget input (inc i)) 255)
              c2 (bit-and (aget input (+ i 2)) 255)
              c3 (bit-and (aget input (+ i 3)) 255)
              _ (when-not (and (valid-char? c0) (valid-char? c1)
                               (valid-char? c2) (valid-char? c3))
                  (throw (IllegalArgumentException. "Invalid Base64 character")))
              v0 (aget dec-table c0)
              v1 (aget dec-table c1)
              v2 (aget dec-table c2)
              v3 (aget dec-table c3)]
          (recur (+ i 4)
                 (conj acc
                       (bit-or (bit-shift-left v0 2) (bit-shift-right v1 4))
                       (bit-or (bit-shift-left (bit-and v1 15) 4)
                               (bit-shift-right v2 2))
                       (bit-or (bit-shift-left (bit-and v2 3) 6) v3))))))))

;; Registering the statics is also what makes the class name resolve, so the
;; suite's (:import org.apache.commons.codec.binary.Base64 …) succeeds.
(__register-class-statics! "org.apache.commons.codec.binary.Base64"
  {"encodeBase64" (fn [input] (encode* input))
   "decodeBase64" (fn [input] (decode* input))})
