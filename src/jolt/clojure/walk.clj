; Jolt Standard Library: clojure.walk
; This file generalizes tree walking for Clojure data structures.

(defn walk
  "Traverses form, an arbitrary data structure. inner and outer are
  functions. Applies inner to each element of form, building up a
  data structure of the same type, then applies outer to the result.
  Recognizes all Clojure data structures. Consumes seqs."
  [inner outer form]
  (cond
    (list? form) (outer (apply list (map inner form)))
    (seq? form) (outer (doall (map inner form)))
    (vector? form) (outer (vec (map inner form)))
    (map? form) (outer (into (empty form) (map inner form)))
    (set? form) (outer (into (empty form) (map inner form)))
    :else (outer form)))

(defn postwalk
  "Performs a depth-first, post-order traversal of form. Calls f on
  each sub-form, uses f's return value in place of the original.
  Recognizes all Clojure data structures. Consumes seqs."
  [f form]
  (walk (partial postwalk f) f form))

(defn prewalk
  "Like postwalk, but does pre-order traversal."
  [f form]
  (walk (partial prewalk f) identity (f form)))

(defn postwalk-demo
  "Demonstrates the behavior of postwalk by returning a lazy seq of
  forms passed to the postwalk outer function during the traversal
  of form."
  [form]
  (let [acc (atom [])]
    (postwalk (fn [x] (swap! acc conj x) x) form)
    @acc))

(defn prewalk-demo
  "Demonstrates the behavior of prewalk by returning a lazy seq of
  forms passed to the prewalk outer function during the traversal
  of form."
  [form]
  (let [acc (atom [])]
    (prewalk (fn [x] (swap! acc conj x) x) form)
    @acc))

(defn postwalk-replace
  "Recursively transforms form by replacing keys in smap with their
  values. Like clojure.string/replace but works with any data
  structure. Does replacement at the leaves of the tree first."
  [smap form]
  (postwalk (fn [x] (if (contains? smap x) (get smap x) x)) form))

(defn prewalk-replace
  "Recursively transforms form by replacing keys in smap with their
  values. Like postwalk-replace but does replacement at the root of
  the tree first."
  [smap form]
  (prewalk (fn [x] (if (contains? smap x) (get smap x) x)) form))

(defn keywordize-keys
  "Recursively transforms all map keys from strings to keywords."
  [m]
  (let [f (fn [[k v]] (if (string? k) [(keyword k) v] [k v]))]
    (postwalk (fn [x] (if (map? x) (into {} (map f x)) x)) m)))

(defn stringify-keys
  "Recursively transforms all map keys from keywords to strings."
  [m]
  (let [f (fn [[k v]] (if (keyword? k) [(name k) v] [k v]))]
    (postwalk (fn [x] (if (map? x) (into {} (map f x)) x)) m)))

(defn macroexpand-all
  "Recursively performs all possible macroexpansions in form."
  [form]
  (prewalk (fn [x] (if (seq? x) (macroexpand x) x)) form))
