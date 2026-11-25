(ns jolt.native.rt
  "Runtime support for a native region call: the exceptions a failed region
  raises. C reports a code in its status; raise! throws what the JVM throws.")

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
    ;; mod by zero: clojure.core/mod takes Objects, so Numbers' message
    9 (throw (ArithmeticException. "Divide by zero"))
    ;; JN_VERIFY_FACTS (the native gate's sanitizer lane): a check a range fact
    ;; dropped would have fired, so the fact is wrong
    10 (throw (IllegalStateException. "native region: a range fact did not hold"))
    (throw (IllegalStateException. (str "native region: unknown failure code " code)))))
