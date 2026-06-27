;; clojure.edn — reading EDN data. Delegates to the Jolt reader via
;; clojure.core/read-string (which parses, never evaluates — safe for EDN), and
;; adds the opts-map arity with :eof plus nil/blank-input handling.
(ns clojure.edn
  "Reading EDN data."
  (:require [clojure.string :as cstr]))

;; The reader yields set literals as a FORM ({:jolt/type :jolt/set :value [...]})
;; rather than a constructed set, so build the actual values, recursing into
;; maps/vectors/lists. (Lists stay lists — EDN never evaluates them as code.)
;; Re-attach the source value's metadata to each rebuilt collection — read-string
;; preserves reader metadata (^:ref […]) but this rebuild would otherwise drop it,
;; which a metadata-driven config lib (aero/integrant) relies on.
(defn- edn->value [opts x]
  (cond
    ;; Reader FORMS are detected by :jolt/type tag, never by map? — strict map?
    ;; (correctly) excludes tagged structs, so the old (and (map? x) ...) guard
    ;; would skip them.
    (= :jolt/set (get x :jolt/type)) (with-meta (set (map (fn [v] (edn->value opts v)) (get x :value))) (edn->value opts (meta x)))
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
          ;; Clojure calls :default with the tag as a SYMBOL and the value.
          (get opts :default) ((get opts :default) tag-sym v)
          :else (__read-tagged tag v)))
    (map? x)
      (with-meta (into {} (map (fn [e] [(edn->value opts (key e)) (edn->value opts (val e))]) x)) (edn->value opts (meta x)))
    (vector? x) (with-meta (mapv (fn [v] (edn->value opts v)) x) (edn->value opts (meta x)))
    ;; a constructed set: recurse into its elements too, so a tagged literal
    ;; inside #{…} gets the :readers/:default treatment (aero's #ref in a set).
    (set? x) (with-meta (set (map (fn [v] (edn->value opts v)) x)) (edn->value opts (meta x)))
    (seq? x) (with-meta (map (fn [v] (edn->value opts v)) x) (edn->value opts (meta x)))
    :else x))

;; Private helper, NOT named read-string: an unqualified (read-string …) call
;; dispatches the core read-string SPECIAL FORM (by name, regardless of ns), so
;; the 1-arity can't delegate to the 2-arity through that name.
(defn- read-edn [opts s]
  (if (or (nil? s) (cstr/blank? s))
    (get opts :eof nil)
    ;; read the RAW form (tagged/set literals stay forms) so edn->value applies
    ;; every #tag through :readers/:default — read-string would build the built-in
    ;; #inst/#uuid eagerly, ignoring an override and failing on a non-string form.
    (edn->value opts (__read-form-raw s))))

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
  (loop [acc (transient []) c (.read reader)]
    (if (== -1 c)
      (apply str (map char (persistent! acc)))
      (recur (conj! acc c) (.read reader)))))

(defn read
  "Reads one EDN object from reader (a PushbackReader or any jolt reader).
  Returns the :eof option value (default nil) at end of input."
  ([reader] (read {} reader))
  ([opts reader] (read-edn opts (drain-reader reader))))
