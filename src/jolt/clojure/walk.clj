; Jolt Standard Library: clojure.walk
; Tree walking for Clojure data structures.

(defn walk
  [inner outer form]
  (cond
    ; vectors/maps first so seq? can't swallow them (a vector is not seq? on
    ; jolt, but keep the concrete branches authoritative)
    (vector? form) (outer (vec (map inner form)))
    (map? form) (outer (into (empty form) (map inner form)))
    ; lists rebuild as lists, other seqs (incl. macro/template output: cons/
    ; concat/lazy-seq) walk too — without this, postwalk-replace silently no-op'd
    ; a quoted list, breaking clojure.template/apply-template (jolt-khk)
    (list? form) (outer (apply list (map inner form)))
    (seq? form) (outer (map inner form))
    :else (outer form)))

(defn postwalk
  [f form]
  (walk (partial postwalk f) f form))

(defn prewalk
  [f form]
  (walk (partial prewalk f) identity (f form)))

(defn postwalk-demo
  "Demonstrates the behavior of postwalk by printing each form as it is walked."
  [form]
  (postwalk (fn [x] (print "Walked: ") (prn x) x) form))

(defn prewalk-demo
  "Demonstrates the behavior of prewalk by printing each form as it is walked."
  [form]
  (prewalk (fn [x] (print "Walked: ") (prn x) x) form))

(defn postwalk-replace
  [smap form]
  (postwalk (fn [x] (if (contains? smap x) (get smap x) x)) form))

(defn prewalk-replace
  [smap form]
  (prewalk (fn [x] (if (contains? smap x) (get smap x) x)) form))

(defn keywordize-keys
  [m]
  (let [f (fn [[k v]] (if (string? k) [(keyword k) v] [k v]))]
    (postwalk (fn [x] (if (map? x) (into {} (map f x)) x)) m)))

(defn stringify-keys
  [m]
  (let [f (fn [[k v]] (if (keyword? k) [(name k) v] [k v]))]
    (postwalk (fn [x] (if (map? x) (into {} (map f x)) x)) m)))
