;; clojure.datafy cannot be a corpus row: a row is one form, and a qualified
;; reference in the same form as its (require ...) compiles before the require
;; runs. Every expectation below was captured from reference Clojure 1.12.5.
(ns datafy-test
  (:require [clojure.datafy :as d]
            [clojure.core.protocols :as p]))

(def failures (atom 0))

(defn check [label got want]
  (if (= got want)
    true
    (do (swap! failures inc)
        (println "datafy FAIL:" label "\n  want" (pr-str want) "\n  got " (pr-str got))
        false)))

;; --- the protocol defaults ---------------------------------------------------
;; "default identity" in Datafiable/Navigable's docstrings is these two arms.
(check "datafy of a plain value is itself" (d/datafy 42) 42)
(check "datafy of nil is nil" (d/datafy nil) nil)
(check "nav defaults to returning v" (d/nav [1 2 3] 0 :v) :v)
(check "protocols/datafy default" (p/datafy :kw) :kw)

;; --- the arms clojure.datafy itself extends ----------------------------------
;; Each needs the value to report a class MORE specific than Object, or the
;; Object default wins and the arm never runs.
(check "datafy of an atom is its value in a vector" (d/datafy (atom 7)) [7])
;; datafy carries the ref's own metadata through, then adds its ::obj/::class
;; provenance on top — so check the ref's key rather than the whole map.
(check "datafy of a ref carries its metadata"
       (:m (meta (d/datafy (atom 1 :meta {:m :yes})))) :yes)
(check "datafy of a throwable is Throwable->map"
       (vec (sort (keys (d/datafy (ex-info "boom" {:a 1}))))) [:cause :data :trace :via])
(check "datafy of a throwable keeps ex-data"
       (:data (d/datafy (ex-info "boom" {:a 1}))) {:a 1})
(check "datafy of a namespace reports its name"
       (:name (d/datafy (find-ns 'clojure.string))) 'clojure.string)
(check "datafy of a namespace lists publics"
       (contains? (:publics (d/datafy (find-ns 'clojure.string))) 'join) true)

;; --- the metadata datafy attaches when it transforms -------------------------
(check "a transformed value records the original under ::d/obj"
       (let [a (atom 7)] (identical? a (::d/obj (meta (d/datafy a))))) true)
(check "an untransformed value gets no ::d/obj"
       (::d/obj (meta (d/datafy [1 2]))) nil)

(if (zero? @failures)
  (println "DATAFY OK")
  (println "DATAFY FAIL:" @failures "failure(s)"))
