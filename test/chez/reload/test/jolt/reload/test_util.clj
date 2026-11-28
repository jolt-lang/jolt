;; From clj-reload 1.0.0 (https://github.com/tonsky/clj-reload), Copyright (c)
;; 2024 Nikita Prokopov, MIT License: licenses/MIT-clj-reload.txt. Ported to
;; jolt.reload; changes are marked "jolt:".
(ns jolt.reload.test-util
  (:require
    [jolt.reload :as reload]
    [jolt.reload.util :as util]
    [clojure.java.io :as io]
    [clojure.string :as str]))

(def *trace
  (atom []))

(defn trace []
  (let [[trace _] (reset-vals! *trace [])]
    trace))

(def *time
  (atom (util/now)))

(def ^:dynamic *dir*)

(defn reset [nses]
  (reset! *trace [])
  (reset! @#'reload/*state {})
  (alter-var-root #'reload/*config* (constantly {}))
  (let [now (util/now)]
    (reset! *time now)
    (doseq [file (next (file-seq (io/file *dir*)))
            :when (> (util/last-modified file) now)]
      (util/set-last-modified file now)))
  (doseq [ns nses]
    (when (@@#'clojure.core/*loaded-libs* ns)
      (remove-ns ns)
      (dosync
        (alter @#'clojure.core/*loaded-libs* disj ns)))))

(defn sym->file [sym]
  (-> sym
    name
    (str/replace "-" "_")
    (str/replace "." "/")
    (str ".clj")
    (->>
      (str *dir* "/")
      (io/file))))

;; jolt: upstream stamps test-start + n seconds, which assumes a test finishes
;; within a second of its start; a slower run stamps a file behind the last
;; scan and the change goes unseen. Never stamp earlier than a second from now.
(defn touch [sym]
  (let [now  (swap! *time #(max (+ % 1000) (+ (util/now) 1000)))
        file (sym->file sym)]
    (util/set-last-modified file now)))

(defmacro with-changed [sym content' & body]
  `(let [sym#     ~sym
         file#    (sym->file sym#)
         content# (slurp file#)]
     (try
       (spit file# ~content')
       (touch sym#)
       ~@body
       (finally
         (spit file# content#)
         (touch sym#)))))

(defmacro with-deleted [sym & body]
  `(let [sym#     ~sym
         file#    (sym->file sym#)
         content# (slurp file#)]
     (try
       (util/file-delete file#)
       ~@body
       (finally
         (spit file# content#)
         (touch sym#)))))

(defn log-fn [& args]
  (swap! *trace
    (fn [track]
      (let [[type arg & _] args
            last-type (->> track (filter string?) last)]
        (cond
          (nil? arg)                                 track
          (= "fixtures/core_test/err_parse.clj" arg) track
          (= "  exception during unload hook" type)  (conj track type (.getName (class arg)))
          (not= type last-type)                      (conj track type arg)
          :else                                      (conj track arg))))))

(defn init [& args]
  (let [[opts nses] (if (map? (first args))
                      [(first args) (next args)]
                      [nil args])]
    (reload/init
      (merge
        {:dirs [*dir*]
         :output :verbose}
        opts))
    (when-not (empty? nses)
      (apply require nses))))

(defn unload
  ([]
   (unload nil))
  ([opts]
   (reload/unload
     (merge {:log-fn log-fn} opts))))

(defn reload
  ([]
   (reload nil))
  ([opts]
   (reload/reload
     (merge {:log-fn log-fn} opts))))

(def bb? (System/getProperty "babashka.version"))
