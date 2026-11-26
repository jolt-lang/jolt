;; Negative integer constants past the fixnum range are Chez bignums, and a built
;; binary's units are vfasl images. Stock Chez's vfasl writer stored a negative
;; bignum's two's-complement bits as its magnitude, so -1152921504606846977 printed
;; as -17293822569102704639N (chez-patches/0012). Each value here is a literal or
;; a constant the compiler folds; the last two are a negative big ratio and a
;; wrapped long product.
(ns nbapp.main)

(defn -main [& _]
  (prn -1152921504606846977
       -9223372036854775808
       -9223372036854775807
       -7046029254386353131
       -18446744073709551621N
       -18446744073709551621/2
       (unchecked-multiply 12345 -7046029254386353131)))
