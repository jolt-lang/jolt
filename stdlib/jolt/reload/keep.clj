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

(ns jolt.reload.keep
  (:require
    [jolt.reload.util :as util]
    [clojure.string :as str])
  (:import
    [java.io File]))

(defn maybe-quote [form]
  (cond
    (symbol? form)
    (list 'quote form)
   
    (and (sequential? form) (not (vector? form)))
    (list 'quote form)
   
    :else
    form))

(defn meta-str [var]
  (let [meta (dissoc (meta var) :ns :file :line :column :name)]
    (when-not (empty? meta)
      (str "^" (pr-str (util/map-vals meta maybe-quote)) " "))))

(defn classname [ns sym]
  (-> (name ns)
    (str/replace "-" "_")
    (str "." sym)))

(defn stash-ns []
  (or
    (find-ns 'jolt.reload.stash)
    (binding [*ns* *ns*]
      (in-ns 'jolt.reload.stash)
      *ns*)))

(def keep-methods-defs
  {:resolve 
   (fn [ns sym keep]
     (when-some [var (resolve (symbol (name ns) (name sym)))]
       {:var var}))
   
   :patch
   (fn [ns sym keep]
     (when-some [var (:var keep)]
       (intern (stash-ns) sym @var)
       (str
         "(def " (meta-str var) sym " @#'jolt.reload.stash/" sym ")")))})

(def keep-methods-deftype
  {:resolve 
   (fn [ns sym keep]
     (when-some [ctor (resolve (symbol (name ns) (str "->" sym)))]
       {:ctor ctor}))
   
   :patch
   (fn [ns sym keep]
     (when-some [ctor (:ctor keep)]
       (intern (stash-ns) (symbol (str "->" sym)) @ctor)
       (str
         "(clojure.core/import " (classname ns sym) ") "
         "(def " (meta-str ctor) "->" sym " jolt.reload.stash/->" sym ")")))})

(def keep-methods-defrecord
  {:resolve 
   (fn [ns sym keep]
     (when-some [ctor (resolve (symbol (name ns) (str "->" sym)))]
       (when-some [map-ctor (resolve (symbol (name ns) (str "map->" sym)))]
         {:ctor ctor
          :map-ctor map-ctor})))
   
   :patch
   (fn [ns sym keep]
     (when-some [ctor (:ctor keep)]
       (when-some [map-ctor (:map-ctor keep)]
         (intern (stash-ns) (symbol (str "->" sym)) @ctor)
         (intern (stash-ns) (symbol (str "map->" sym)) @map-ctor)
         (str
           "(clojure.core/import " (classname ns sym) ") "
           "(def " (meta-str ctor) "->" sym " jolt.reload.stash/->" sym ") "
           "(def " (meta-str map-ctor) "map->" sym " jolt.reload.stash/map->" sym ")"))))})

(def keep-methods-defprotocol
  {:resolve 
   (fn [ns sym keep]
     (when-some [proto (resolve (symbol (name ns) (name sym)))]
       ;; jolt: a protocol map has no :method-builders (the JVM's var -> builder
       ;; fn, repointed at the new method vars after a reload); its :sigs name
       ;; the methods, whose vars live in the protocol's namespace. A method
       ;; dispatches on the protocol's name, so the kept fns need no rewiring.
       {:proto   proto
        :methods (util/for-map [{method-sym :name} (vals (:sigs @proto))
                                :let [method-var (resolve (symbol (name ns) (name method-sym)))]
                                :when method-var]
                   [method-sym method-var])}))
   
   :patch
   (fn [ns sym keep]
     (when-some [proto (:proto keep)]
       (when-some [methods (:methods keep)]
         (intern (stash-ns) sym @proto)
         (doseq [[method-sym method] methods]
           (intern (find-ns 'jolt.reload.stash) method-sym @method))
         (str
           "(def " (meta-str proto) sym " jolt.reload.stash/" sym ") "
           "(clojure.core/alter-var-root #'" sym " assoc :var #'" sym ") "
           (str/join " "
             (for [[method-sym method] methods]
               (str "(def " (meta-str method) "^{:protocol #'" sym "} " method-sym " jolt.reload.stash/" method-sym ")")))))))})

(def keep-methods
  (delay
    @(resolve 'jolt.reload/keep-methods)))

(defn keep-resolve [ns sym keep]
  ((:resolve (@keep-methods (:tag keep))) ns sym keep))

(defn keep-patch [ns sym keep]
  ((:patch (@keep-methods (:tag keep))) ns sym keep))

(defn resolve-keeps [ns syms]
  (util/for-map [[sym keep] syms
                 :let [resolved (keep-resolve ns sym keep)]
                 :when resolved]
    [sym resolved]))

;; jolt: LineNumberingPushbackReader has no captureString/getString. Its line
;; and column after a read mark where that form's text ends, so a form's text —
;; with the whitespace and comments before it, as captureString has it — is the
;; slice of the source from the previous mark to this one.
;; The reader counts a line break the way LineNumberingPushbackReader does:
;; \n, \r\n, and a lone \r each end a line.
(defn- line-starts [^String s]
  (let [n (count s)]
    (loop [i 0 acc (transient [0])]
      (if (< i n)
        (let [c (.charAt s i)]
          (cond
            (= c \newline) (recur (inc i) (conj! acc (inc i)))
            (= c \return)  (let [j (if (and (< (inc i) n) (= \newline (.charAt s (inc i))))
                                     (+ i 2)
                                     (inc i))]
                             (recur j (conj! acc j)))
            :else          (recur (inc i) acc)))
        (persistent! acc)))))

(defn patch-file [^String content patch-fn]
  (let [rdr     (util/string-reader content)
        starts  (line-starts content)
        mark    (fn []
                  (min (count content)
                    (+ (nth starts (dec (.getLineNumber rdr)))
                      (dec (.getColumnNumber rdr)))))
        patched (StringBuilder.)
        *from   (volatile! 0)]
    (loop []
      (let [form (util/read-form rdr)
            ;; at end of input the reader stands past a final newline it
            ;; never saw; the text left is everything after the last form
            to   (if (= :jolt.reload.util/eof form) (count content) (mark))
            text (subs content @*from to)
            _    (vreset! *from to)]
        (cond
          (= :jolt.reload.util/eof form)
          (str patched)
          
          (and (list? form) (>= (count form) 2))
          (if-some [text' (patch-fn (take 2 form))]
            (let [[_ ws] (re-matches #"(?s)(\s*).*" text)
                  _      (.append patched ws)
                  text   (subs text (count ws))
                  lines  (str/split text #"\r\n|\r|\n")
                  text'' (if (= 1 (count lines))
                           (if (<= (count text) (count text'))
                             text'
                             (str text' (str/join (repeat (- (count text) (count text')) \space))))
                           (str text'
                             (str/join (repeat (dec (count lines)) \newline))
                             (str/join (repeat (count (last lines)) \space))))]
              (.append patched text'')
              (recur))
            (do
              (.append patched text)
              (recur)))
          
          :else
          (do
            (.append patched text)
            (recur)))))))

(defn patch-fn [ns keeps]
  (fn [[tag sym]]
    (when-some [keep (get keeps sym)]
      (when (= tag (:tag keep))
        (keep-patch ns sym keep)))))

(defn ns-load-patched [ns ^File file keeps]
  (try    
    (let [content (patch-file (slurp file) (patch-fn ns keeps))]
      (util/ns-load-file content ns file))
    
    ;; check 
    (when (not (find-ns ns))
      (throw (Exception. (format "namespace '%s' not found after loading '%s'" ns (.getPath file)))))
      
    (finally
      ;; drop everything in stash
      (remove-ns 'jolt.reload.stash)
      (dosync
        (alter @#'clojure.core/*loaded-libs* disj 'jolt.reload.stash)))))
