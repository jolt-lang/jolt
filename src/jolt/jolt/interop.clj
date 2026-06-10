; Jolt Standard Library: jolt.interop
; Janet interop helpers for Jolt.

(defn janet-eval
  [s]
  (eval (parse s)))

(defn janet-type
  [x]
  (janet/type x))

(defn janet-describe
  [x]
  (describe x))

(defn janet-table-keys
  [t]
  (keys t))

(defn janet-table-vals
  [t]
  (vals t))

(defn janet-table->map
  [t]
  (into {} (map (fn [k] [k (get t k)]) (keys t))))
