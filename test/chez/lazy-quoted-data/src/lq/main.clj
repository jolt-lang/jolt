(ns lq.main
  (:require [lq.a :as a] [lq.b :as b] [clojure.string :as str] [jolt.image]))

(def vars [#'a/pick #'a/multi #'a/hinted #'a/private-fn #'a/plain #'a/*dyn* #'a/hidden
           #'a/tagged #'a/user-meta #'a/evaluated-meta #'a/regex-meta #'a/twice #'a/area
           #'a/Shape #'a/perimeter #'a/scale #'a/->Rect #'a/map->Rect #'a/->Pt #'a/forward
           #'a/redefinable #'a/with-arglists #'a/konst #'a/once-only #'a/boom #'a/keyword-set
           #'b/bvar])

(defn show-meta [v]
  (let [m (meta v)]
    ;; key order and every value, without the absolute :file a source run has
    (binding [*print-meta* true]
      (pr-str (mapv (fn [[k x]] [k (cond (= k :file) (str/replace (str x) #".*/test/chez/lazy-quoted-data/" "")
                                         (fn? x) :a-fn
                                         :else x)])
                    (dissoc m :ns))))))

;; Many threads reading a var's metadata for the first time while one alters it:
;; every reader sees the def's map, and the alteration is never lost.
(defn race-check [n]
  (let [rs (vec (for [i (range n)]
                  (let [v (eval (list 'def (with-meta (symbol (str "race-" i)) {:doc "d" :i i}) i))
                        lazy0 (jolt.host/var-meta-lazy? v)
                        start (promise)
                        readers (doall (for [_ (range 4)] (future @start (meta v))))
                        writer (future @start (alter-meta! v assoc :z true))]
                    (deliver start true)
                    (let [ms (mapv deref readers)]
                      @writer
                      [lazy0 (every? #(and (= "d" (:doc %)) (= i (:i %))) ms) (:z (meta v))]))))]
    [(count (filter first rs)) (frequencies (map rest rs))]))

(defn -main [& _]
  ;; metadata no form has read yet, written first
  (println "lazy:" (jolt.host/var-meta-lazy? #'a/am1) (jolt.host/var-meta-lazy? #'a/rm1))
  (alter-meta! #'a/am1 assoc :k 1)
  (reset-meta! #'a/rm1 {:only true})
  (println "alter-meta! first" (dissoc (meta #'a/am1) :ns :file))
  (println "reset-meta! first" (dissoc (meta #'a/rm1) :ns))
  (println "vary-meta on a var" (try (vary-meta #'a/vm1 assoc :a 1) (catch Throwable t (.getName (class t)))))
  (alter-meta! #'a/am1 assoc :k 2)
  (println "alter-meta! again" (:k (meta #'a/am1)) (:doc (meta #'a/am1)))
  (let [[lazy0 fr] (race-check 300)]
    (println "lazy: race lazy before first read" lazy0)
    (println "race" fr))
  (doseq [v vars] (println (str v) (show-meta v)))
  ;; values read off evaluated metadata
  (println "computed" (:computed (meta #'a/evaluated-meta)) ((:f (meta #'a/evaluated-meta))))
  (println "regex" (re-find (:re (meta #'a/regex-meta)) "xaab"))
  (println "dynamic" (.isDynamic #'a/*dyn*) (binding [a/*dyn* :bound] a/*dyn*) (.isDynamic #'a/plain))
  (println "publics" (contains? (ns-publics 'lq.a) 'hidden) (contains? (ns-publics 'lq.a) 'pick)
           (contains? (ns-interns 'lq.a) 'hidden))
  (println "macro" (:macro (meta #'a/twice)) (macroexpand-1 '(lq.a/twice 1)))
  (println "protocol" (= #'a/Shape (:protocol (meta #'a/perimeter))) (a/perimeter (a/->Rect 1 2)))
  ;; literals: equality, identity and metadata, across namespaces and loads
  (let [la (a/literals) lb (b/literals)]
    (println "literals =" (= la lb) (identical? (nth la 0) (nth lb 0)) (identical? :a (nth la 0))
             (identical? (nth la 1) (nth lb 1)) (meta (nth la 2)) (meta (nth lb 2))
             (identical? (nth la 3) (nth lb 3))))
  (println "same site" (identical? (a/same-site) (a/same-site)) (= (a/same-site) [1 2 {:a 1}]))
  (println "sym-list" (map meta a/sym-list) (identical? (first a/sym-list) 'a))
  (println "keyword-set" (a/keyword-set :b) (identical? :b (a/keyword-set :b)))
  ;; the source registry's rows for lq/a.clj, read through jolt.profile's
  ;; consumer of it (clojure.repl/source-fn keeps no source in jolt)
  (println "source rows" (sort (map str (jolt.host/profile-defs ["lq/a.clj"]))))
  (println "trace" (try (a/calls-boom 1) (catch Exception e
                                            (mapv str (take 2 (.getStackTrace e))))))
  (println "closure" (a/closure 1) (re-find #"jfn\$lq\.a/fn-maker\$0" (pr-str (class a/closure))))
  ;; closures from a cached namespace through a state image: the image writer
  ;; looks each one's registered source up (fn-form-registry.ss)
  (let [f (java.io.File/createTempFile "lqimg" ".img")
        p (.getPath f)
        v {:closure a/closure :adder a/adder :nested ((a/nested-maker 1) 2)}]
    (println "image scan" (jolt.image/scan v))
    (jolt.image/dump! p v)
    (let [r (jolt.image/read-image p)]
      (println "image" ((:closure r) 1) ((:adder r) 1) ((:nested r) 3)))
    (.delete f))
  (println "done")
  (shutdown-agents))
