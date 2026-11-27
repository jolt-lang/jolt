;; Ported from clj-reload 1.0.0, https://github.com/tonsky/clj-reload
;;
;; Copyright (c) 2024 Nikita Prokopov
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(ns jolt.reload.util
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str])
  (:import
    [clojure.lang LineNumberingPushbackReader]
    [java.io File StringReader]))

(def ^:dynamic *log-fn*
  println)

(defn log [& args]
  (when *log-fn*
    (apply *log-fn* args)))

(def reader-opts
  {:read-cond :allow
   :features  #{:clj}
   :eof       ::eof})

(def dummy-resolver
  (reify clojure.lang.LispReader$Resolver
    (currentNS [_]
      'user)
    (resolveClass [_ sym]
      sym)
    (resolveAlias [_ sym]
      (symbol (str ":" sym)))
    (resolveVar [_ sym]
      sym)))

(defn read-form [reader]
  (binding [*read-eval*       false
            *suppress-read*   true
            *reader-resolver* dummy-resolver]
    (read reader-opts reader)))

(defn throwable? [o]
  (instance? Throwable o))

(defn update! [m k f & args]
  (assoc! m k (apply f (m k) args)))

(def conjs
  (fnil conj #{}))

(def intos
  (fnil into #{}))

(defn assoc-some [m & kvs]
  (reduce
    (fn [m [k v]]
      (cond-> m
        (some? v) (assoc k v)))
    m
    (partition 2 kvs)))

(defn some-map [& kvs]
  (apply assoc-some nil kvs))

(defn some-set [& vals]
  (not-empty
    (set (filter some? vals))))

(defn map-vals [m f]
  (when (some? m)
    (persistent!
      (reduce-kv
        #(assoc! %1 %2 (f %3))
        (transient (empty m))
        m))))

(defn deep-merge [& ms]
  (apply merge-with merge ms))

(defmacro for-map [& body]
  `(into {}
     (for ~@body)))

(defmacro for-set [& body]
  `(into #{}
     (for ~@body)))

(defn doeach [f xs]
  (doseq [x xs]
    (f x)))

(defn now []
  (System/currentTimeMillis))

(defn last-modified [^File f]
  (some-> f .lastModified))

(defn set-last-modified [^File f t]
  (some-> f (.setLastModified t)))

(defn file? [^File f]
  (some-> f .isFile))

(defn directory? [^File f]
  (some-> f .isDirectory))

(defn file-name [^File f]
  (some-> f .getName))

(defn file-path [^File f]
  (some-> f .getPath))

(defn file-delete [^File f]
  (some-> f .delete))

(defn file-reader ^LineNumberingPushbackReader [f]
  (LineNumberingPushbackReader.
    (io/reader (io/file f))))

(defn string-reader ^LineNumberingPushbackReader [^String s]
  (LineNumberingPushbackReader.
    (StringReader. s)))

;; jolt: Compiler/load is jolt.host/load-string, which binds *ns* for the load
;; the same way and takes the source path the load reports as *file*.
(defn ns-load-file [content ns ^File file]
  (let [[_ ext] (re-matches #".*\.([^.]+)" (.getName file))
        path    (-> ns str (str/replace #"\-" "_") (str/replace #"\." "/") (str "." ext))]
    (jolt.host/load-string content path)
    ;; jolt: mark it loaded the way a require would (see jolt.host/mark-loaded!)
    (when (find-ns ns)
      (jolt.host/mark-loaded! ns))))

;; jolt: there is no class loader chain to walk; the loader's source roots are
;; the class path (deps.edn :paths, then each dependency's). jolt's own install
;; roots are on it too, as relative paths that only exist inside a jolt checkout.
(defn classpath-dirs []
  (->> (jolt.host/source-roots)
    (map io/as-file)
    (filter directory?)
    (distinct)
    (mapv file-path)))

(comment
  (classpath-dirs))