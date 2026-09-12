(ns allowlib.core)

;; spec.gen.alpha/dynaload's shape, whole: a `require` of a COMPUTED name, then
;; a `resolve`, reached only through a delay that nothing in this program
;; forces. It is reachable code — `describe` references the delay — so without
;; the library's :allow-dynamic it bails the shake of every app that uses the
;; library. The computed require matters: it is a load from source at run time
;; (jolt.host/load-namespace), and a vouch that covered the resolve but not the
;; load left every spec app unshakeable (jolt-lang/jolt#890 follow-up).
;;
;; ^:redef keeps the inline pass from splicing this body into gen-delay: the
;; bail scan names the def a ref ends up IN, and the allow entry in
;; lib/deps.edn names dynaload, so dynaload must still own its calls.
(defn ^:redef dynaload [s]
  (let [ns (namespace s)]
    (require (symbol ns))
    (or (resolve s)
        (throw (ex-info (str "Var " s " is not on the classpath") {})))))

(def gen-delay (delay (dynaload 'clojure.test.check.generators/int)))

(defn describe [x]
  (if (= x :gen) @gen-delay (str "allowlib:" x)))
