;; The same expression without the set!. Overflow must still throw here, which is
;; what proves the sibling file's set! did not escape into the root binding.
(ns umapp.checked)

(defn add-max [] (+ 9223372036854775807 1))
