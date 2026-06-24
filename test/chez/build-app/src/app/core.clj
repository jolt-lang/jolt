(ns app.core
  (:require [app.util :as util :refer [greet]]
            [clojure.java.io :as io]))

;; An aliased cross-ns defmethod: 'util/greet is passed quoted to defmethod-setup,
;; so the AOT build must register the `util` alias for app.core or it resolves to
;; ns "util" and never reaches app.util/greet (the dispatch falls to :default).
(defmethod util/greet :loud [_] "greet:loud")

;; A defmethod on a REFERRED multifn (bare `greet`): the AOT build must register
;; the :refer so the bare name resolves to app.util/greet, not a shadow.
(defmethod greet :soft [_] "greet:soft")

(defn -main [& args]
  ;; the resource is baked into the binary (deps.edn :jolt/build :embed), so this
  ;; resolves with no resources/ dir on disk, run from any cwd.
  (println (slurp (io/resource "greeting.txt")))
  (util/twice (println (util/shout "hello from a built binary")))
  (println "args:" (vec args))
  (println "sum:" (reduce + (map count args)))
  (println "greet-default:" (util/greet :unknown))
  (println "greet-loud:" (util/greet :loud))
  (println "greet-soft:" (util/greet :soft)))
