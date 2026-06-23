;; clojure.edn — reading EDN data. Delegates to the Jolt reader via
;; clojure.core/read-string (which parses, never evaluates — safe for EDN), and
;; adds the opts-map arity with :eof plus nil/blank-input handling.
(ns clojure.edn
  "Reading EDN data."
  (:require [clojure.string :as cstr]))

;; The reader yields set literals as a FORM ({:jolt/type :jolt/set :value [...]})
;; rather than a constructed set, so build the actual values, recursing into
;; maps/vectors/lists. (Lists stay lists — EDN never evaluates them as code.)
(defn- edn->value [opts x]
  (cond
    ;; Reader FORMS are detected by :jolt/type tag, never by map? — strict map?
    ;; (correctly) excludes tagged structs, so the old (and (map? x) ...) guard
    ;; would skip them.
    (= :jolt/set (get x :jolt/type)) (set (map (fn [v] (edn->value opts v)) (get x :value)))
    ;; Tagged elements: a reader from the :readers opt wins, then the built-in
    ;; data readers (#uuid/#inst + registered); an unknown tag falls to the
    ;; :default opt fn (called with tag and value, as in Clojure) or throws.
    (= :jolt/tagged (get x :jolt/type))
      (let [tag (get x :tag)
            v (edn->value opts (get x :form))
            ;; the reader stores the tag as a :#name keyword; :readers maps are
            ;; keyed by the SYMBOL (Clojure's shape) — normalize for lookup
            tag-sym (let [n (name tag)]
                      (symbol (if (= "#" (subs n 0 1)) (subs n 1) n)))
            custom (get (get opts :readers) tag-sym)]
        (cond
          custom (custom v)
          (get opts :default) ((get opts :default) tag v)
          :else (__read-tagged tag v)))
    (map? x)
      (into {} (map (fn [e] [(edn->value opts (key e)) (edn->value opts (val e))]) x))
    (vector? x) (mapv (fn [v] (edn->value opts v)) x)
    (seq? x) (map (fn [v] (edn->value opts v)) x)
    :else x))

;; Private helper, NOT named read-string: an unqualified (read-string …) call
;; dispatches the core read-string SPECIAL FORM (by name, regardless of ns), so
;; the 1-arity can't delegate to the 2-arity through that name.
(defn- read-edn [opts s]
  (if (or (nil? s) (cstr/blank? s))
    (get opts :eof nil)
    (edn->value opts (clojure.core/read-string s))))

(defn read-string
  "Reads one object from the string s. Returns the :eof option value (default
  nil) for nil or blank input. opts is an options map; :eof sets the value
  returned at end of input."
  ([s] (read-edn {} s))
  ([opts s] (read-edn opts s)))

(defn- drain-reader
  "All remaining content of a reader as a string. Shim readers (StringReader,
  PushbackReader, io/reader results) expose char-wise .read; a raw file
  handle is read whole."
  [reader]
  (if (= :core/file (janet/type reader))
    (janet.file/read reader :all)
    (loop [acc (transient []) c (.read reader)]
      (if (== -1 c)
        (apply str (map char (persistent! acc)))
        (recur (conj! acc c) (.read reader))))))

(defn read
  "Reads one EDN object from reader (a PushbackReader or any jolt reader).
  Returns the :eof option value (default nil) at end of input."
  ([reader] (read {} reader))
  ([opts reader] (read-edn opts (drain-reader reader))))
