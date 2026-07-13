;; Regression fixture: a cljs-only reader conditional between two clj defns.
;; The emission reader must skip #?(:cljs …) (it reads as "no form") and still
;; emit `after`; a truncating reader drops `after`, so a built binary calling it
;; crashes with an unbound var. See host/chez/emit-image.ss ei-read-all.
(ns cljccond.lib)

(defn before [] :before)

#?(:cljs (defn only-cljs [] :cljs))

(defn after [] :after)
