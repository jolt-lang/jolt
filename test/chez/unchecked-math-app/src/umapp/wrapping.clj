;; A file that opts into wrapping arithmetic the way a real library does: a
;; top-level (set! *unchecked-math* …). RT.load binds the var around a file load,
;; so the set! is legal and its effect ends with this file.
(ns umapp.wrapping)

(set! *unchecked-math* true)

(defn add-max [] (+ 9223372036854775807 1))
