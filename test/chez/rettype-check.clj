;; The return-type claims the compiler trusts, checked against the runtime.
;; Loaded form by form by test/chez/rettype-test.ss.
;;
;; The types pass and the checker read hand-written tables: an op-registry
;; :num-result? op types its call :num, a :bool? op answers a boolean, a
;; :num-args? op rejects a non-number, a :dbl-contagion? op answers a flonum
;; over a flonum operand, and lattice's vector-/str-/string-ns-ret-fns type
;; their calls :vec/:str. A wrong entry is a silent miscompile: (nil? (+ x))
;; folded to false, though (+ nil) is nil. So, after Baker's Nimble inferencer
;; (§6.4), each claim is tested the way it was meant: run the actual function on
;; representative values of every lattice class, at every arity the registry
;; admits, and require each call that RETURNS to answer inside its claim. A call
;; that throws satisfies every claim (the inference only types values that exist).

(ns rettype-check
  (:require [jolt.op-registry :as reg]
            [jolt.passes.types.lattice :as lat]))

(def num-reps
  [0 1 -1 2 7 -7 1.5 -0.5 0.0 -0.0 ##NaN ##Inf ##-Inf 1/2 -3/4 (bigint 5)
   9223372036854775807 -9223372036854775808 1152921504606846975 1152921504606846976
   100000000000000000000N 1.5M (float 2.5)])
(def other-reps [nil true false "s" "" :k 'sym \a [1 2] '(1) {:a 1} #{1} (range 3) (Object.)])
(def reps (into num-reps other-reps))

(defn tuples [vals n]
  (if (zero? n) [[]] (for [t (tuples vals (dec n)) v vals] (conj t v))))

;; arity 3 runs over the numbers only: every claim on a 3-arg call is numeric,
;; and the full product is 37^3 calls an op
(defn arg-tuples [n] (tuples (if (< n 3) reps num-reps) n))

(defn call [f args]
  (try {:ok (apply f args)} (catch Throwable _ {:threw true})))

;; The one known exception to a :num-args? claim, and the JVM's own: min/max
;; answer a NaN operand without comparing the other, so (min ##NaN "s") and
;; (min nil ##NaN) are ##NaN there too. The checker only rejects a PROVEN
;; non-number, and a NaN beside one is no program anybody writes, so the flag stays.
(defn nan-minmax? [op args]
  (and (contains? #{"min" "max"} op)
       (some (fn [a] (and (double? a) (not (== a a)))) args)))

(defn spec-problems
  "[claim op args result] for every call of `f` that returns outside a claim
  `spec` makes. Arity 1 of an arg1-identity op answers its argument, so the
  number claims start at arity 2 there."
  [op f spec]
  (for [n (filter (or (:arity spec) (constantly true)) [0 1 2 3])
        args (arg-tuples n)
        :let [r (call f args) v (:ok r)
              id1? (and (= n 1) (:arg1-identity? spec))]
        :when (contains? r :ok)
        claim [:num-result :bool :num-args :dbl-contagion :arg1-identity]
        :when (case claim
                :num-result (and (:num-result? spec) (not id1?) (not (number? v)))
                :bool (and (:bool? spec) (not (boolean? v)))
                :num-args (and (:num-args? spec) (not id1?) (some (complement number?) args)
                               (not (nan-minmax? op args)))
                :dbl-contagion (and (:dbl-contagion? spec) (seq args) (every? number? args)
                                    (some double? args) (not (some decimal? args))
                                    (not (double? v)))
                :arg1-identity (and id1? (not (identical? v (nth args 0)))
                                    ;; a number is answered as itself, but a
                                    ;; boxed copy is no identity failure
                                    (not (and (number? v) (= v (nth args 0))))))]
    [claim op args v]))

(defn core-fn [ns-name nm]
  (let [v (resolve (symbol ns-name nm))] (when v (let [f @v] (when (fn? f) f)))))

(defn registry-problems [registry]
  (mapcat (fn [[op spec]] (when-let [f (core-fn "clojure.core" op)] (spec-problems op f spec)))
          registry))

(defn set-problems [claim pred ns-name names]
  (for [nm names
        :let [f (core-fn ns-name nm)] :when f
        n [0 1 2 3]
        args (tuples (if (< n 3) reps [1 "s" :k [1]]) n)
        :let [r (call f args)]
        :when (and (contains? r :ok) (not (pred (:ok r))))]
    [claim (str ns-name "/" nm) args (:ok r)]))

(defn all-problems []
  (concat (registry-problems reg/op-registry)
          (set-problems :vec-ret vector? "clojure.core" lat/vector-ret-fns)
          (set-problems :str-ret string? "clojure.core" lat/str-ret-fns)
          (set-problems :str-ret string? "clojure.string" lat/string-ns-ret-fns)))

(defn summary
  "One line per [claim op]: the count and the first offending call."
  [problems]
  (map (fn [[k g]] (str k " x" (count g) ", e.g. " (pr-str (nth (first g) 2))
                        " => " (pr-str (nth (first g) 3))))
       (group-by (fn [p] [(nth p 0) (nth p 1)]) problems)))
