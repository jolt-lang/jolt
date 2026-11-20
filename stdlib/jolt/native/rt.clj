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
    3 (throw (ArithmeticException. "Divide by zero"))
    4 (throw (NegativeArraySizeException. (str a)))
    5 (throw (IllegalArgumentException. (str "Value out of range for long: " d)))
    6 (throw (OutOfMemoryError. "native region: allocation failed"))
    (throw (IllegalStateException. (str "native region: unknown failure code " code)))))

(defn- in-long [r]
  (if (and (<= Long/MIN_VALUE r) (<= r Long/MAX_VALUE))
    r
    (raise! 2 0 0 0.0)))

(defn add [a b] (in-long (+ a b)))
(defn sub [a b] (in-long (- a b)))
(defn mul [a b] (in-long (* a b)))
(defn neg [a] (in-long (- a)))
(defn quot* [a b] (if (zero? b) (raise! 3 0 0 0.0) (in-long (quot a b))))
(defn rem* [a b] (if (zero? b) (raise! 3 0 0 0.0) (rem a b)))
(defn mod* [a b] (if (zero? b) (raise! 3 0 0 0.0) (mod a b)))

(defn index
  "RT.intCast of an array index: outside the int range is an overflow."
  [i]
  (if (and (<= Integer/MIN_VALUE i) (<= i Integer/MAX_VALUE)) i (raise! 2 0 0 0.0)))

(defn size [n] (if (neg? n) (raise! 4 n 0 0.0) n))
