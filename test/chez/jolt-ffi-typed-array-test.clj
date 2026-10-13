;; jolt.ffi typed array moves — read-array / write-array / read-into! with a
;; type wider than a byte, including either float type with either float array
;; and a write of an array's first n elements. The host block-copies the memory and decodes it into
;; (or encodes it out of) the array's flvector or fxvector backing; the property
;; under test is that this answers exactly what the element loop it replaced
;; answers — the same values, the same array class, a backing aset and aget
;; still work on — for every width, at an offset, at 0, 1 and past the 8192
;; elements where a large backing is pinned.
;; Run: bin/jolt run test/chez/jolt-ffi-typed-array-test.clj, or `make ffi`;
;; exits nonzero on a failure, and smoke.sh greps for
;; "JOLT-FFI-TYPED-ARRAY-TEST OK".
(ns jolt-ffi-typed-array-test)

(require '[jolt.ffi :as ffi])

(def failures (atom []))

(defmacro check-eq [label got want]
  `(let [g# ~got w# ~want]
     (when-not (= g# w#)
       (swap! failures conj (str ~label ": want " (pr-str w#) " got " (pr-str g#))))))

;; Two NaNs are not =, so compare elements with NaN spelled as a keyword
(defn- elems [arr] (mapv #(if (and (float? %) (Double/isNaN (double %))) :nan %) arr))

;; The element loop read-array ran before the block decode, kept here as the
;; reference the native path is held to.
(def carriers
  {:double [double-array :double 8]
   :float  [float-array :float 4]
   :int64  [long-array :int64 8]
   :int32  [int-array :int32 4]
   :uint32 [int-array :int32 4]
   :int16  [short-array :int16 2]
   :uint16 [short-array :int16 2]})

(defn- loop-read [p t n off]
  (let [[ctor element width] (carriers t)
        arr (ctor n)]
    (dotimes [i n] (aset arr i (ffi/read p element (+ off (* i width)))))
    arr))

(def samples
  {:double [0.0 -0.0 1.5 -2.5 1e300 -1e300 Double/MAX_VALUE Double/MIN_VALUE
            Double/POSITIVE_INFINITY Double/NEGATIVE_INFINITY Double/NaN]
   :float  [0.0 -0.0 1.5 -2.5 0.1 3.4e38 -3.4e38 1.4e-45
            Double/POSITIVE_INFINITY Double/NEGATIVE_INFINITY Double/NaN]
   :int64  [0 1 -1 Long/MAX_VALUE Long/MIN_VALUE 1152921504606846975 -1152921504606846976]
   :int32  [0 1 -1 2147483647 -2147483648 123456]
   :uint32 [0 1 4294967295 2147483648]
   :int16  [0 1 -1 32767 -32768 1234]
   :uint16 [0 1 65535 32768]})

(def buf (ffi/alloc (* 8 10000)))

(defn- fill! [t vs off]
  (let [[_ element width] (carriers t)
        wtype ({:uint32 :uint32 :uint16 :uint16} t element)]
    (doseq [[i v] (map-indexed vector vs)]
      (ffi/write buf wtype v (+ off (* i width))))))

;; --- read-array: the native decode answers what the loop answers ----------
(doseq [t (keys samples)
        off [0 8 12]]
  (let [vs (samples t) n (count vs)]
    (fill! t vs off)
    (let [got (ffi/read-array buf t n off)
          want (loop-read buf t n off)]
      (check-eq (str "read-array " t " at offset " off " matches the element loop")
                (elems got) (elems want))
      (check-eq (str "read-array " t " answers the loop's array class")
                (class got) (class want))
      (check-eq (str "read-array " t " has n elements") (alength got) n))))

(check-eq "read-array :double reads the values written"
          (elems (do (fill! :double [1.5 -2.5 Double/NaN] 0)
                     (ffi/read-array buf :double 3)))
          [1.5 -2.5 :nan])
(check-eq "read-array :float reads single precision"
          (do (fill! :float [0.1] 0) (aget (ffi/read-array buf :float 1) 0))
          (float 0.1))
(check-eq "read-array :float elements are Floats"
          (class (aget (ffi/read-array buf :float 1) 0)) Float)
(check-eq "read-array :uint32 above Integer/MAX_VALUE reads back negative"
          (do (fill! :uint32 [4294967295] 0) (vec (ffi/read-array buf :uint32 1)))
          [-1])
(check-eq "read-array :int64 keeps a value past the fixnum range"
          (do (fill! :int64 [5 Long/MAX_VALUE Long/MIN_VALUE 7] 0)
              (vec (ffi/read-array buf :int64 4)))
          [5 Long/MAX_VALUE Long/MIN_VALUE 7])

;; --- sizes: 0, 1, and past the pinning threshold --------------------------
(doseq [t [:double :float :int64 :int32 :int16]
        n [0 1 9000]]
  (let [[_ element width] (carriers t)]
    (dotimes [i n] (ffi/write buf element (if (#{:double :float} t) (* 0.5 i) (- i 4500)) (* i width)))
    (let [got (ffi/read-array buf t n)]
      (check-eq (str "read-array " t " of " n " matches the element loop")
                (vec got) (vec (loop-read buf t n 0)))
      (check-eq (str "read-array " t " of " n " has alength " n) (alength got) n))))

;; --- a negative count -----------------------------------------------------
;; The JVM's NegativeArraySizeException, as (double-array -1) raises, and not a
;; Scheme constructor's message.
(defn- thrown [f]
  (try (f) :no-throw
       (catch Throwable e [(.getName (class e)) (ex-message e)])))
(doseq [t [:double :float :int64 :int32 :int16 :uint8]]
  (check-eq (str "read-array " t " of -1 throws NegativeArraySizeException")
            (thrown #(ffi/read-array buf t -1))
            ["java.lang.NegativeArraySizeException" "-1"]))
(check-eq "read-array of -3 raw octets throws NegativeArraySizeException"
          (thrown #(ffi/read-array buf -3))
          ["java.lang.NegativeArraySizeException" "-3"])

;; --- the answer is an ordinary array afterwards ---------------------------
(doseq [t [:double :float :int64 :int32 :int16]
        n [4 9000]]
  (let [arr (ffi/read-array buf t n)
        v (if (#{:double :float} t) 2.5 -7)]
    (aset arr (dec n) v)
    (check-eq (str "aset / aget work on a read-array " t " of " n)
              (aget arr (dec n)) (aget (doto ((first (carriers t)) 1) (aset 0 v)) 0))))
(let [arr (do (fill! :int64 [1 2 3] 0) (ffi/read-array buf :int64 3))]
  (aset arr 1 Long/MAX_VALUE)
  (check-eq "a read-array :int64 widens on a later aset like any long-array"
            (vec arr) [1 Long/MAX_VALUE 3]))

;; --- write-array: the block encode lands the bits the loop landed ---------
(doseq [t (keys samples)
        off [0 4 8]]
  (let [vs (samples t)
        [ctor element width] (carriers t)
        arr (ctor (count vs))]
    (doseq [[i v] (map-indexed vector vs)]
      (aset arr i (if (#{:uint32 :uint16} t) (ffi/read (doto buf (ffi/write t v)) element) v)))
    (check-eq (str "write-array " t " answers the element count")
              (ffi/write-array buf t arr off) (count vs))
    (check-eq (str "write-array " t " at offset " off " round-trips through read")
              (elems (map #(ffi/read buf element (+ off (* % width))) (range (count vs))))
              (elems arr))))

(check-eq "write-array :int16 of an int-array keeps the low 16 bits"
          (do (ffi/write-array buf :int16 (int-array [65537 -65537 98304]))
              (vec (ffi/read-array buf :int16 3)))
          [1 -1 -32768])
(check-eq "write-array :int32 of a long-array keeps the low 32 bits"
          (do (ffi/write-array buf :int32 (long-array [4294967297 -4294967297]))
              (vec (ffi/read-array buf :int32 2)))
          [1 -1])
(check-eq "write-array :int64 of fixnums round-trips"
          (do (ffi/write-array buf :int64 (long-array [1152921504606846975 -1152921504606846976 -1]))
              (vec (ffi/read-array buf :int64 3)))
          [1152921504606846975 -1152921504606846976 -1])
(check-eq "write-array :float of a double-array rounds to single"
          (do (ffi/write-array buf :float (double-array [0.1]))
              (aget (ffi/read-array buf :float 1) 0))
          (float 0.1))
(check-eq "write-array :double of a float-array widens"
          (do (ffi/write-array buf :double (float-array [0.5]))
              (vec (ffi/read-array buf :double 1)))
          [0.5])
(check-eq "write-array of an empty typed array writes nothing"
          (ffi/write-array buf :double (double-array 0)) 0)
(check-eq "write-array of 9000 doubles round-trips"
          (let [arr (double-array (map #(* 0.25 %) (range 9000)))]
            (ffi/write-array buf :double arr)
            (= (vec arr) (vec (ffi/read-array buf :double 9000))))
          true)
;; what the block encode does not take still goes through the element loop
(check-eq "write-array of a vector still writes"
          (do (ffi/write-array buf :int32 [7 8 9]) (vec (ffi/read-array buf :int32 3)))
          [7 8 9])
(check-eq "write-array of a long-array holding a bignum still writes"
          (let [arr (long-array [1 2])]
            (aset arr 0 Long/MAX_VALUE)
            (ffi/write-array buf :int64 arr)
            (vec (ffi/read-array buf :int64 2)))
          [Long/MAX_VALUE 2])

;; --- read-into!: the typed form fills an existing array -------------------
(fill! :double [1.5 2.5 3.5] 0)
(let [dst (double-array [9.0 9.0 9.0 9.0 9.0])]
  (check-eq "read-into! :double answers n" (ffi/read-into! buf :double dst 0 1 3) 3)
  (check-eq "read-into! :double fills from the index, nothing else"
            (vec dst) [9.0 1.5 2.5 3.5 9.0]))
(fill! :double [4.5 5.5] 8)
(let [dst (double-array 2)]
  (ffi/read-into! (+ buf 8) :double dst 0 0 2)
  (check-eq "read-into! reads from the pointer it is given" (vec dst) [4.5 5.5]))
(let [dst (double-array 3)]
  (dotimes [_ 3] (ffi/read-into! buf :double dst 0 0 3))
  (check-eq "read-into! refills the same array on every call" (vec dst) [1.5 4.5 5.5]))
(fill! :float [0.1 -0.5] 0)
(let [dst (float-array 2)]
  (ffi/read-into! buf :float dst 0 0 2)
  (check-eq "read-into! :float stores what read-array reads"
            (vec dst) (vec (ffi/read-array buf :float 2)))
  (check-eq "read-into! :float elements are Floats" (class (aget dst 0)) Float))
(fill! :int64 [3 Long/MIN_VALUE] 0)
(let [dst (long-array 3)]
  (ffi/read-into! buf :int64 dst 0 1 2)
  (check-eq "read-into! :int64 keeps a value past the fixnum range"
            (vec dst) [0 3 Long/MIN_VALUE]))
(fill! :int16 [-32768 32767] 0)
(let [dst (short-array 2)]
  (ffi/read-into! buf :int16 dst 0 0 2)
  (check-eq "read-into! :int16 fills a short-array" (vec dst) [-32768 32767]))
(fill! :uint32 [4294967295 7] 0)
(let [dst (int-array 2)]
  (ffi/read-into! buf :uint32 dst 0 0 2)
  (check-eq "read-into! :uint32 lands the bits, as read-array does" (vec dst) [-1 7]))
(let [dst (int-array 9000)]
  (dotimes [i 9000] (ffi/write buf :int32 (- i) (* 4 i)))
  (ffi/read-into! buf :int32 dst 0 0 9000)
  (check-eq "read-into! :int32 fills a pinned-size int-array"
            (vec dst) (vec (ffi/read-array buf :int32 9000))))
(check-eq "read-into! of 0 elements answers 0"
          (ffi/read-into! buf :double (double-array 2) 0 2 0) 0)
(defn- thrown-data [f]
  (try (f) :no-throw
       (catch clojure.lang.ExceptionInfo e (ex-data e))
       (catch Throwable e [(.getName (class e)) (ex-message e)])))
(check-eq "read-into! past the end throws, naming the range"
          (thrown-data #(ffi/read-into! buf :double (double-array 2) 0 1 2))
          {:index 1 :length 2 :capacity 2})
(check-eq "read-into! at a negative index throws"
          (thrown-data #(ffi/read-into! buf :double (double-array 2) 0 -1 1))
          {:index -1 :length 1 :capacity 2})
(check-eq "read-into! of a negative count throws"
          (thrown-data #(ffi/read-into! buf :double (double-array 2) 0 0 -1))
          {:index 0 :length -1 :capacity 2})
(check-eq "read-into! :int32 into a short-array throws, naming the type"
          (thrown-data #(ffi/read-into! buf :int32 (short-array 2) 0 0 2))
          {:type :int32})
(check-eq "read-into! :int32 into a long-array throws: integers fill their own width"
          (thrown-data #(ffi/read-into! buf :int32 (long-array 2) 0 0 2))
          {:type :int32})
(check-eq "read-into! :double into an int-array throws"
          (thrown-data #(ffi/read-into! buf :double (int-array 2) 0 0 2))
          {:type :double})
(check-eq "read-into! into a vector throws"
          (thrown-data #(ffi/read-into! buf :int32 [0 0] 0 0 2))
          {:type :int32})
(check-eq "read-into! of a :string throws, as read-array does"
          (try (ffi/read-into! buf :string (long-array 2) 0 0 2) :no-throw
               (catch Exception _ :threw))
          :threw)
(let [dst (double-array [9.0 9.0])]
  (try (ffi/read-into! buf :double dst 0 1 2) (catch Throwable _ nil))
  (check-eq "a refused read-into! writes nothing" (vec dst) [9.0 9.0]))

;; a byte-array fills through a one-byte type, a slice through index and count
(ffi/write-array buf (byte-array [1 -1 3]))
(let [dst (byte-array 4)]
  (check-eq "read-into! :byte of a slice answers n" (ffi/read-into! buf :byte dst 0 1 3) 3)
  (check-eq "read-into! :byte fills the slice, signed" (vec dst) [0 1 -1 3]))
(let [dst (byte-array 3)]
  (ffi/read-into! buf :uint8 dst 0 0 3)
  (check-eq "read-into! :uint8 is the byte form" (vec dst) [1 -1 3]))

;; --- read-into!: either float type into either float array -----------------
;; As write-array writes :float or :double from either: a :float widens exactly
;; into a double-array, and a :double rounds to single into a float-array, the
;; values (float x) and aset give.
(def float-samples [0.0 -0.0 1.5 -2.5 0.1 1e300 -1e300 1e-300 3.4e38 1.4e-45
                    Double/POSITIVE_INFINITY Double/NEGATIVE_INFINITY Double/NaN])
(doseq [[t ctor] [[:float float-array] [:float double-array]
                  [:double double-array] [:double float-array]]
        off [0 2]]
  (let [n (count float-samples)
        _ (fill! t float-samples 0)
        want (ctor (vec (ffi/read-array buf t n)))   ; what aset stores
        dst (ctor (+ n off))]
    (ffi/read-into! buf t dst 0 off n)
    (check-eq (str "read-into! " t " into a " (if (= ctor float-array) "float" "double")
                   "-array from index " off " stores what aset stores")
              (elems (drop off dst)) (elems want))))
(check-eq "read-into! :double rounds into a float-array"
          (do (fill! :double [0.1 1e300] 0)
              (let [d (float-array 2)] (ffi/read-into! buf :double d 0 0 2) (vec d)))
          [(float 0.1) Float/POSITIVE_INFINITY])
(check-eq "read-into! :double into a float-array stores Floats"
          (let [d (float-array 1)] (ffi/read-into! buf :double d 0 0 1) (class (aget d 0)))
          Float)
(check-eq "read-into! :float widens exactly into a double-array"
          (do (fill! :float [0.1] 0)
              (let [d (double-array 1)] (ffi/read-into! buf :float d 0 0 1) (vec d)))
          [(double (float 0.1))])
(check-eq "read-into! :float into a double-array stores Doubles"
          (let [d (double-array 1)] (ffi/read-into! buf :float d 0 0 1) (class (aget d 0)))
          Double)

;; --- write-array: the first n elements ------------------------------------
;; A staging array sized for the largest batch, written part full. The bytes on
;; either side of what was written are left as they were.
(def sentinel (byte-array (repeat 96 -86)))   ; 0xaa in every octet
(defn- octets [p k] (vec (ffi/read-array p k)))
(defn- staged [t arr off n]
  (ffi/write-array buf sentinel)
  [(ffi/write-array buf t arr off 0 n) (octets buf 96)])
(defn- staged-range [t arr off index n]
  (ffi/write-array buf sentinel)
  [(ffi/write-array buf t arr off index n) (octets buf 96)])
;; --- write-array: either float type from either float array ----------------
;; The arm is picked by the backing, not the array's class: each pairing writes
;; the octets an element-by-element ffi/write of the same values writes.
(doseq [[t ctor width] [[:float float-array 4] [:float double-array 4]
                        [:double double-array 8] [:double float-array 8]]]
  (let [arr (ctor (remove #(and (= ctor float-array) (> (Math/abs (double %)) 3.4e38)) float-samples))
        k (count arr)]
    (dotimes [i k] (ffi/write buf t (aget arr i) (* i width)))
    (let [want (octets buf (* k width))]
      (ffi/write-array buf sentinel)
      (ffi/write-array buf t arr)
      (check-eq (str "write-array " t " from a " (if (= ctor float-array) "float" "double")
                     "-array lands what ffi/write of each element lands")
                (octets buf (* k width)) want))))

;; --- the host primitives check their own range ----------------------------
;; The copy loops use unchecked element access, so the range is proved in the
;; host as well as in the stdlib that normally calls it.
(check-eq "__read-into-typed! refuses a range past the array"
          (thrown-data #(jolt.ffi/__read-into-typed! buf :double (double-array 2) 1 2))
          {:index 1 :length 2 :capacity 2})
(check-eq "__write-array-typed refuses a range past the array"
          (thrown-data #(jolt.ffi/__write-array-typed buf :float (float-array 2) 0 3))
          {:index 0 :length 3 :capacity 2})
(check-eq "__read-into-typed! refuses a negative start"
          (thrown-data #(jolt.ffi/__read-into-typed! buf :int32 (int-array 2) -1 1))
          {:index -1 :length 1 :capacity 2})

(let [[got bytes] (staged :double (double-array [1.5 2.5 3.5 4.5 5.5]) 8 3)]
  (check-eq "write-array of the first 3 doubles answers 3" got 3)
  (check-eq "write-array of the first 3 doubles leaves the bytes before the offset"
            (subvec bytes 0 8) (vec (repeat 8 -86)))
  (check-eq "write-array of the first 3 doubles writes exactly them"
            (vec (ffi/read-array buf :double 3 8)) [1.5 2.5 3.5])
  (check-eq "write-array of the first 3 doubles leaves the bytes after them"
            (subvec bytes 32 96) (vec (repeat 64 -86))))
(doseq [[t ctor width vs] [[:float double-array 4 [0.1 2.5 -3.5]]
                           [:float float-array 4 [0.1 2.5 -3.5]]
                           [:double float-array 8 [0.5 2.5 -3.5]]
                           [:int32 int-array 4 [7 -8 9]]
                           [:int16 short-array 2 [7 -8 9]]
                           [:int64 long-array 8 [7 -8 9]]]]
  (let [arr (ctor vs)
        [_ bytes] (staged t arr 0 2)
        whole (do (ffi/write-array buf sentinel) (ffi/write-array buf t arr) (octets buf 96))]
    (check-eq (str "write-array " t " of the first 2 writes what the whole write writes there")
              (subvec bytes 0 (* 2 width)) (subvec whole 0 (* 2 width)))
    (check-eq (str "write-array " t " of the first 2 leaves the third element's bytes")
              (subvec bytes (* 2 width) 96) (vec (repeat (- 96 (* 2 width)) -86)))))
(let [arr (double-array [1.0 2.0 3.0])]
  (check-eq "write-array of 0 elements answers 0 and writes nothing"
            (staged :double arr 0 0) [0 (vec (repeat 96 -86))])
  (check-eq "write-array of every element is the whole write"
            (second (staged :double arr 0 3))
            (do (ffi/write-array buf sentinel) (ffi/write-array buf :double arr) (octets buf 96)))
  (check-eq "write-array of one past the end throws, naming the count"
            (thrown-data #(ffi/write-array buf :double arr 0 0 4)) {:index 0 :length 4 :capacity 3})
  (check-eq "write-array of a negative count throws"
            (thrown-data #(ffi/write-array buf :double arr 0 0 -1)) {:index 0 :length -1 :capacity 3})
  (check-eq "a refused write-array writes nothing"
            (do (ffi/write-array buf sentinel)
                (try (ffi/write-array buf :double arr 0 0 4) (catch Throwable _ nil))
                (octets buf 96))
            (vec (repeat 96 -86)))
  (check-eq "write-array of a range starting past the start throws, naming it"
            (thrown-data #(ffi/write-array buf :double arr 0 2 2)) {:index 2 :length 2 :capacity 3})
  (check-eq "write-array at a negative index throws"
            (thrown-data #(ffi/write-array buf :double arr 0 -1 1)) {:index -1 :length 1 :capacity 3})
  (check-eq "write-array with an index and count needs a type"
            (thrown-data #(ffi/write-array buf arr 0 0 2 9)) {:second arr}))
(check-eq "write-array of the first 2 of a vector takes the element loop"
          (do (ffi/write-array buf sentinel) (ffi/write-array buf :int32 [7 8 9] 0 0 2)
              [(vec (ffi/read-array buf :int32 2)) (subvec (octets buf 96) 8 12)])
          [[7 8] [-86 -86 -86 -86]])
(check-eq "write-array of the first 2 of a byte-array with a one-byte type"
          (do (ffi/write-array buf sentinel) (ffi/write-array buf :uint8 (byte-array [1 2 3]) 4 0 2)
              (subvec (octets buf 96) 3 7))
          [-86 1 2 -86])

;; --- babashka.ffi's argument order ----------------------------------------
;; In every typed form position four is a BYTE offset into the pointer, as in
;; babashka.ffi (its ADR 0004): read-into! is write-array in the other
;; direction, and a partial copy is two trailing arguments, an index into the
;; array and a count, that leave the meaning of the first four alone.
(fill! :double [1.5 2.5 3.5 4.5 5.5 6.5] 0)
(let [dst (double-array 3)]
  (check-eq "(read-into! p t arr) fills all of arr from byte 0, answering its length"
            [(ffi/read-into! buf :double dst) (vec dst)] [3 [1.5 2.5 3.5]]))
(let [dst (double-array 3)]
  (check-eq "(read-into! p t arr offset) reads from offset BYTES into p"
            [(ffi/read-into! buf :double dst 16) (vec dst)]
            [3 (vec (ffi/read-array buf :double 3 16))]))
(let [dst (double-array [9.0 9.0 9.0 9.0 9.0])]
  (check-eq "(read-into! p t arr offset index count) fills count elements from index"
            [(ffi/read-into! buf :double dst 8 1 3) (vec dst)]
            [3 [9.0 2.5 3.5 4.5 9.0]])
  (check-eq "and is the pointer-arithmetic form with the offset as an argument"
            (vec dst)
            (vec (doto (double-array [9.0 9.0 9.0 9.0 9.0])
                   (as-> d (ffi/read-into! (+ buf 8) :double d 0 1 3))))))
(check-eq "read-into! of the whole of an empty array answers 0"
          (ffi/read-into! buf :double (double-array 0) 8) 0)
(check-eq "read-into! with five arguments, the old (p t arr index count), throws"
          (try (ffi/read-into! buf :double (double-array 3) 1 2) :no-throw
               (catch Throwable _ :threw))
          :threw)
(let [arr (double-array 3)]
  (check-eq "read-into! with three arguments needs a type"
            (thrown-data #(ffi/read-into! buf arr 0)) {:second arr}))
(ffi/write-array buf (byte-array [10 11 12 13 14 15 16 17]))
(let [dst (byte-array 3)]
  (check-eq "(read-into! p :uint8 arr offset) fills a byte-array from offset bytes in"
            [(ffi/read-into! buf :uint8 dst 4) (vec dst)] [3 [14 15 16]]))
(let [dst (byte-array 4)]
  (check-eq "(read-into! p :uint8 arr offset index count) is the byte form's slice"
            [(ffi/read-into! buf :uint8 dst 2 1 2) (vec dst)] [2 [0 12 13 0]]))
;; jolt's (read-into! p arr off n) and (write-array p arr off n), with off an
;; index into the byte-array, are removed: in babashka.ffi's order position four
;; is a byte offset into p. The old call raises, naming its replacement, and
;; reads or writes nothing.
(let [dst (byte-array 4)]
  (check-eq "the removed (read-into! p arr off n) raises, naming its replacement"
            [(try (ffi/read-into! buf dst 1 2) :no-throw (catch Exception e (ex-message e))) (vec dst)]
            [(str "jolt.ffi/read-into! takes a type before the array: "
                  "(read-into! p :byte arr 0 index count) reads a slice into a byte-array")
             [0 0 0 0]])
  (check-eq "and its replacement reads the same slice"
            (do (ffi/read-into! buf :byte dst 0 1 2) (vec dst)) [0 10 11 0]))
(check-eq "read-into! of a range past the end throws before reading"
          (let [dst (double-array [9.0 9.0])]
            [(thrown-data #(ffi/read-into! buf :double dst 0 1 2)) (vec dst)])
          [{:index 1 :length 2 :capacity 2} [9.0 9.0]])
;; write-array's partial form, the mirror image
(let [[got bytes] (staged-range :double (double-array [1.5 2.5 3.5 4.5 5.5]) 8 1 3)]
  (check-eq "(write-array p t arr offset index count) writes arr[index, index+count)"
            [got (vec (ffi/read-array buf :double 3 8))] [3 [2.5 3.5 4.5]])
  (check-eq "and nothing before offset or after the count"
            [(subvec bytes 0 8) (subvec bytes 32 96)] [(vec (repeat 8 -86)) (vec (repeat 64 -86))]))
(check-eq "write-array's partial form from a vector takes the element loop"
          (do (ffi/write-array buf sentinel) (ffi/write-array buf :int32 [7 8 9] 4 1 2)
              [(vec (ffi/read-array buf :int32 2 4)) (subvec (octets buf 96) 0 4) (subvec (octets buf 96) 12 16)])
          [[8 9] [-86 -86 -86 -86] [-86 -86 -86 -86]])
(check-eq "write-array's partial form with a one-byte type"
          (do (ffi/write-array buf sentinel) (ffi/write-array buf :uint8 (byte-array [1 2 3 4]) 2 1 2)
              (subvec (octets buf 96) 1 5))
          [-86 2 3 -86])
(check-eq "write-array with five arguments throws"
          (try (ffi/write-array buf :double (double-array 3) 0 2) :no-throw
               (catch Throwable _ :threw))
          :threw)
(check-eq "the removed (write-array p arr off n) raises, naming its replacement"
          (do (ffi/write-array buf sentinel)
              [(try (ffi/write-array buf (byte-array [1 2 3 4]) 1 2) :no-throw
                    (catch Exception e (ex-message e)))
               (subvec (octets buf 96) 0 3)])
          [(str "jolt.ffi/write-array takes a type before the array: "
                "(write-array p :byte arr 0 index count) writes a slice of a byte-array")
           [-86 -86 -86]])
(check-eq "and its replacement writes the same slice"
          (do (ffi/write-array buf sentinel) (ffi/write-array buf :byte (byte-array [1 2 3 4]) 0 1 2)
              (subvec (octets buf 96) 0 3))
          [2 3 -86])

;; --- arrays widened before the copy ---------------------------------------
;; A long-array that has held a bignum, or an int-array that has held a
;; non-number, has a boxed backing; the decode stores through ja-set! into it.
;; read-into! is the only way to hand the decode such an array.
(ffi/write-array buf :int64 (long-array [11 -12 13]))
(let [dst (long-array 3)]
  (aset dst 0 Long/MAX_VALUE)
  (ffi/read-into! buf :int64 dst 0 0 3)
  (check-eq "read-into! :int64 fills a widened long-array" (vec dst) [11 -12 13]))
(ffi/write-array buf :int32 (int-array [21 -22 23]))
(let [dst (int-array 4)]
  (aset dst 0 "not a number")
  (ffi/read-into! buf :int32 dst 0 1 3)
  (check-eq "read-into! :int32 fills an int-array widened by a non-number"
            (vec (rest dst)) [21 -22 23]))

(ffi/free buf)

;; --- chunk boundaries ------------------------------------------------------
;; The copy moves 4KB at a time — 512 eight-byte, 1024 four-byte or 2048
;; two-byte elements — so these sizes end mid-chunk after crossing many, and
;; cross the 8192-element pinning threshold too.
(def big (ffi/alloc (* 8 50000)))
(doseq [[t n] [[:double 20001] [:int64 20001] [:float 40001] [:int32 40001] [:int16 70001]]
        off [0 6]]
  (let [[ctor element width] (carriers t)]
    (dotimes [i n]
      (ffi/write big element (if (#{:double :float} t) (- (* 0.5 i) 100.0) (- (rem (* 7 i) 30000) 15000))
                 (+ off (* i width))))
    (let [want (loop-read big t n off)]
      (check-eq (str "read-array " t " of " n " at offset " off " crosses chunks intact")
                (= (vec (ffi/read-array big t n off)) (vec want)) true)
      (let [dst (ctor (+ n 3))]
        (ffi/read-into! (+ big off) t dst 0 3 n)
        (check-eq (str "read-into! " t " of " n " into index 3 crosses chunks intact")
                  (= (vec (drop 3 dst)) (vec want)) true))
      (let [src (ctor (map #(if (#{:double :float} t) (* -0.25 %) (- (rem % 20000) 10000)) (range n)))]
        (ffi/write-array big t src off)
        (check-eq (str "write-array " t " of " n " at offset " off " crosses chunks intact")
                  (= (vec (loop-read big t n off)) (vec src)) true)
        ;; the first n-1001: ends mid-chunk, and the last 1001 keep their old values
        (let [k (- n 1001)
              before (vec (loop-read big t n off))
              src2 (ctor (map #(if (#{:double :float} t) (* 0.5 %) (- (rem % 9000) 4500)) (range n)))]
          (ffi/write-array big t src2 off 0 k)
          (check-eq (str "write-array " t " of the first " k " of " n " crosses chunks and stops there")
                    (= (vec (loop-read big t n off)) (into (vec (take k src2)) (drop k before))) true))))))

;; A value past the fixnum range in a LATER chunk widens the array there, and
;; every element after it, in that chunk and the next, still lands.
(dotimes [i 20001] (ffi/write big :int64 i (* 8 i)))
(ffi/write big :int64 Long/MIN_VALUE (* 8 10000))
(let [a (ffi/read-array big :int64 20001)]
  (check-eq "read-array :int64 widens on a bignum in the second chunk"
            [(aget a 9999) (aget a 10000) (aget a 10001) (aget a 16384) (aget a 20000) (alength a)]
            [9999 Long/MIN_VALUE 10001 16384 20000 20001])
  (check-eq "and every other element is the one written"
            (= (vec (assoc (vec a) 10000 10000)) (vec (range 20001))) true))
(ffi/write big :int64 Long/MAX_VALUE (* 8 9000))
(let [a (ffi/read-array big :int64 9001)]
  (check-eq "read-array :int64 widens a pinned-size array on its last element"
            [(aget a 8999) (aget a 9000) (alength a)] [8999 Long/MAX_VALUE 9001]))
(ffi/free big)

;; --- memory: a large copy costs its array, not a second copy of it --------
;; Chez's own allocation counters, reached through jolt.scheme only to measure.
;; bytes-allocated alone is the heap's CURRENT size and drops at a collection,
;; so a collection inside the measured call would hide what it allocated; with
;; bytes-deallocated added it is the running total, which only grows. The
;; counter is compiled once: an eval-string per reading would itself allocate
;; ~16KB and swamp the limits below.
(require '[jolt.scheme :as scm])
(def ^:private total-allocated
  (scm/eval-string "(lambda () (+ (bytes-allocated) (bytes-deallocated)))"))
(defn- allocated [f]
  (let [b0 (total-allocated)]
    (f)
    (- (total-allocated) b0)))
(def huge-n 1000000)
(def huge (ffi/alloc (* 8 huge-n)))
(ffi/read-array huge :double 16)   ; warm, so the measure sees only the copy
(check-eq "read-array of 8MB of doubles allocates its array and under 64KB besides"
          (< (allocated #(ffi/read-array huge :double huge-n)) (+ (* 8 huge-n) (* 64 1024))) true)
(let [src (double-array huge-n) dst (double-array huge-n)]
  (check-eq "write-array of 8MB of doubles allocates under 16KB"
            (< (allocated #(ffi/write-array huge :double src)) (* 16 1024)) true)
  ;; the point of read-into!: a refill allocates the chunk, not an array
  (check-eq "read-into! of 8MB of doubles allocates under 16KB"
            (< (allocated #(ffi/read-into! huge :double dst 0 0 huge-n)) (* 16 1024)) true))
(let [dst (double-array 6000)]
  (ffi/read-into! huge :double dst 0 0 6000)
  (check-eq "a frame-sized read-into! of 6000 doubles allocates under 16KB, not 48KB"
            (< (allocated #(ffi/read-into! huge :double dst 0 0 6000)) (* 16 1024)) true))
;; A :float element must not box a flonum on its way between the bytevector
;; and the array's flvector (16 bytes an element, 96KB for these 6000, through
;; the checked single-float accessors).
(doseq [[label f] [["read-into! :float into a float-array" #(ffi/read-into! huge :float (float-array 6000))]
                   ["read-into! :float into a double-array" #(ffi/read-into! huge :float (double-array 6000))]]]
  (f)
  (check-eq (str "a " label " of 6000 allocates its array and under 16KB besides")
            (< (allocated f) (+ (* 8 6000) (* 16 1024))) true))
(let [fa (float-array 6000) da (double-array 6000)]
  (doseq [[label f] [["read-into! :float into an existing float-array" #(ffi/read-into! huge :float fa)]
                     ["write-array :float from a float-array" #(ffi/write-array huge :float fa)]
                     ["write-array :float from a double-array" #(ffi/write-array huge :float da)]]]
    (f)
    (check-eq (str label " of 6000 allocates under 16KB") (< (allocated f) (* 16 1024)) true)))
(ffi/free huge)

(if (empty? @failures)
  (do (println "JOLT-FFI-TYPED-ARRAY-TEST OK") (flush) (System/exit 0))
  (do (doseq [f @failures] (println "FAIL:" f))
      (println "JOLT-FFI-TYPED-ARRAY-TEST FAILED:" (count @failures))
      (flush)
      (System/exit 1)))
