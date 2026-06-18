;; dynamic vars (jolt-9ls5) — the handful of clojure.core dynamic vars the seed
;; binds natively (src/jolt/core.janet) that aren't emitted into the prelude, so
;; they var-deref to nil on Chez. These two are plain constants; *ns* (a namespace
;; object) needs a value type with get-see-through and map?=false and is tracked
;; separately. Loaded from rt.ss after the value model + def-var!.

;; *clojure-version* — a jolt map {:major 1 :minor 11 :incremental 0 :qualifier nil}
;; (jolt is all-flonum, so the numbers are flonums).
(def-var! "clojure.core" "*clojure-version*"
  (jolt-hash-map (keyword #f "major") 1.0
                 (keyword #f "minor") 11.0
                 (keyword #f "incremental") 0.0
                 (keyword #f "qualifier") jolt-nil))

;; *unchecked-math* — jolt does no unchecked-math elision; the var reads false.
(def-var! "clojure.core" "*unchecked-math*" #f)
