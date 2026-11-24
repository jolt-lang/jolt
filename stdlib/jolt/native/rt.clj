(ns jolt.native.rt
  "Runtime support shared by both halves of a native region: the exceptions a
  failed region raises (C reports a code, these raise what the JVM raises), and
  the checked long arithmetic the Jolt fallback runs, so a region overflows the
  same way in the REPL as in a built binary. Jolt's own long arithmetic
  promotes to a bignum (test/conformance/known-divergences.edn); a region
  follows the JVM's primitive longs instead, because C has no bignums.")

(defn raise!
  "Raise the exception C's out code stands for."
  [code a b d]
  (case (long code)
    1 (throw (ArrayIndexOutOfBoundsException.
              (str "Index " a " out of bounds for length " b)))
    2 (throw (ArithmeticException. "integer overflow"))
    3 (throw (ArithmeticException. "/ by zero"))
    4 (throw (NegativeArraySizeException. (str a)))
    5 (throw (IllegalArgumentException. (str "Value out of range for long: " d)))
    6 (throw (OutOfMemoryError. "native region: allocation failed"))
    7 (throw (IllegalArgumentException.
              (str "native region: " a " does not fit a borrowed long-array, which"
                   " holds fixnums (-2^60 to 2^60-1); widen the array outside the region")))
    ;; a recursive helper ran out of the thread's C stack
    8 (throw (StackOverflowError.))
    (throw (IllegalStateException. (str "native region: unknown failure code " code)))))

(defn- in-long [r]
  (if (and (<= Long/MIN_VALUE r) (<= r Long/MAX_VALUE))
    r
    (raise! 2 0 0 0.0)))

(defn add [a b] (in-long (+ a b)))
(defn sub [a b] (in-long (- a b)))
(defn mul [a b] (in-long (* a b)))
(defn neg [a] (in-long (- a)))
;; Long/MIN_VALUE / -1 wraps to Long/MIN_VALUE on the JVM (Numbers.quotient is
;; Java's long /), with no exception
(defn quot* [a b]
  (cond (zero? b) (raise! 3 0 0 0.0)
        (and (= b -1) (= a Long/MIN_VALUE)) a
        :else (quot a b)))
(defn rem* [a b] (if (zero? b) (raise! 3 0 0 0.0) (rem a b)))
(defn mod* [a b] (if (zero? b) (raise! 3 0 0 0.0) (mod a b)))

(defn index
  "RT.intCast of an array index: outside the int range is an overflow."
  [i]
  (if (and (<= Integer/MIN_VALUE i) (<= i Integer/MAX_VALUE)) i (raise! 2 0 0 0.0)))

(defn fixnum-store
  "A value stored into a borrowed long-array: it must stay a fixnum, as the C
  half (which writes the fxvector in place) requires."
  [v]
  (if (and (<= -1152921504606846976 v) (<= v 1152921504606846975)) v (raise! 7 v 0 0.0)))

(defn fixnum-aset
  "aset into a borrowed long-array: the index is checked before the value, as
  the C half checks them."
  [^longs arr i v]
  (aget arr i)
  (aset arr i (fixnum-store v)))
