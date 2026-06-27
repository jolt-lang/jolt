; Jolt Standard Library: clojure.walk
; Tree walking for Clojure data structures.

(defn walk
  [inner outer form]
  (cond
    ; vectors/maps first so seq? can't swallow them (a vector is not seq? on
    ; jolt, but keep the concrete branches authoritative). Re-attach the form's
    ; metadata to the rebuilt collection, as Clojure does — a metadata-driven walk
    ; (aero/spec) needs ^:ref and friends to survive the rebuild.
    (vector? form) (outer (with-meta (vec (map inner form)) (meta form)))
    ; a record is also map?, but (empty record) yields a plain map — rebuild by
    ; conj-ing the walked entries back onto the original so the record TYPE
    ; survives. Type-dispatched walks depend on it (e.g. integrant resolves
    ; #ig/ref by detecting its Ref record while postwalking the config).
    (record? form) (outer (reduce (fn [r x] (conj r (inner x))) form form))
    (map? form) (outer (with-meta (into (empty form) (map inner form)) (meta form)))
    ; lists rebuild as lists, other seqs (incl. macro/template output: cons/
    ; concat/lazy-seq) walk too — without this, postwalk-replace silently no-op'd
    ; a quoted list, breaking clojure.template/apply-template
    (list? form) (outer (with-meta (apply list (map inner form)) (meta form)))
    (seq? form) (outer (with-meta (map inner form) (meta form)))
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

(defn macroexpand-all
  "Recursively performs all possible macroexpansions in form."
  [form]
  (prewalk (fn [x] (if (seq? x) (macroexpand x) x)) form))

(defn keywordize-keys
  [m]
  (let [f (fn [[k v]] (if (string? k) [(keyword k) v] [k v]))]
    (postwalk (fn [x] (if (map? x) (into {} (map f x)) x)) m)))

(defn stringify-keys
  [m]
  (let [f (fn [[k v]] (if (keyword? k) [(name k) v] [k v]))]
    (postwalk (fn [x] (if (map? x) (into {} (map f x)) x)) m)))
