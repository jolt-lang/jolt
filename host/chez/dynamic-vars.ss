;; dynamic vars — the handful of clojure.core dynamic vars that aren't emitted into
;; the prelude. These two are plain constants; *ns* (a namespace object) needs a
;; value type with get-see-through and map?=false and is tracked separately. Loaded
;; from rt.ss after the value model + def-var!.

;; *clojure-version* — a map {:major 1 :minor 11 :incremental 0 :qualifier nil}.
(def-var! "clojure.core" "*clojure-version*"
  (jolt-hash-map (keyword #f "major") 1
                 (keyword #f "minor") 11
                 (keyword #f "incremental") 0
                 (keyword #f "qualifier") jolt-nil))

;; *unchecked-math* — jolt does no unchecked-math elision; the var reads false.
(def-var! "clojure.core" "*unchecked-math*" #f)

;; *warn-on-reflection* — jolt has no reflection, so the var reads false; (set!
;; *warn-on-reflection* …) resolves and updates it (a no-op effect).
(def-var! "clojure.core" "*warn-on-reflection*" #f)

;; *assert* — gates `assert`; settable/bindable (malli.assert toggles it). Default
;; true, like the JVM.
(def-var! "clojure.core" "*assert*" #t)

;; *print-readably* — bound by pr-family / with-out-str-style code; default true.
(def-var! "clojure.core" "*print-readably*" #t)
