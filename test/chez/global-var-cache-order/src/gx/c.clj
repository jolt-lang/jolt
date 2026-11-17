;; Calls and reads gx.a/g by its full name without requiring gx.a: it compiles
;; when something else has loaded gx.a first (gx.main1), and a cached copy can
;; then be loaded before gx.a exists (gx.main2).
(ns gx.c)
(defn call-g [x] (gx.a/g x))
(defn ref-g [] gx.a/g)
