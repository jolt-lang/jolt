(ns allowlib.core)

;; spec.gen.alpha/dynaload's shape: a `resolve` reached only through a delay
;; that nothing in this program forces. It is reachable code — `describe`
;; references the delay — so without the library's :allow-dynamic it bails
;; the shake of every app that uses the library.
;;
;; ^:redef keeps the inline pass from splicing this body into gen-delay: the
;; bail scan names the def a `resolve` ref ends up IN, and the allow entry in
;; lib/deps.edn names dynaload, so dynaload must still own its call.
(defn ^:redef dynaload [s]
  (or (resolve s)
      (throw (ex-info (str "Var " s " is not on the classpath") {}))))

(def gen-delay (delay (dynaload 'clojure.test.check.generators/int)))

(defn describe [x]
  (if (= x :gen) @gen-delay (str "allowlib:" x)))
