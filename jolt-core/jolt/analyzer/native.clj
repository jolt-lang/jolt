(ns jolt.analyzer.native
  "The jolt.native/__region special form: a native region call site, validated
  into the :native-region node the back end lowers to a call to the region's C
  entry with a Jolt fallback."
  (:require [jolt.host :refer [form-sym? form-sym-name form-sym-ns form-vec?
                               form-vec-items form-keyword?]]
            [jolt.analyzer.diagnostics :refer [analysis-error]]))

;; jolt.native/__region — a native region call site, what the jolt.native/native
;; macro expands to:
;;   (jolt.native/__region "jn_<16 hex>" "<C source>" [:doubles :long ...] :result
;;                         [arg ...] fallback-fn raise-fn :pinned|:hold-gc)
;; The kinds and result are literals read here; the args, the fallback fn and the
;; raise fn are child expressions. The back end derives the Chez foreign types
;; from the kinds rather than taking them from the form, so a ^doubles slot is
;; always scheme-object fed by the arg check that guarantees an flvector.
(defn native-form? [head]
  (and (form-sym? head) (= "jolt.native" (form-sym-ns head))
       (= "__region" (form-sym-name head))))

(def ^:private native-param-kinds #{"doubles" "longs" "long" "double" "bool"})
(def ^:private native-result-kinds #{"doubles" "longs" "long" "double" "bool" "unit"})

(defn- native-region-sym? [s]
  (and (string? s) (= 19 (count s)) (= "jn_" (subs s 0 3))
       (every? (fn [c] (or (and (>= (int c) 48) (<= (int c) 57))
                           (and (>= (int c) 97) (<= (int c) 102))))
               (subs s 3))))

(defn- native-kind [k allowed what]
  (when-not (and (form-keyword? k) (nil? (namespace k)) (contains? allowed (name k)))
    (analysis-error :analyze/invalid-native-region
                    (str "jolt.native/__region: bad " what " kind " (pr-str k))))
  (name k))

(defn analyze-native-region
  "ANALYZE is the analyzer's own entry, passed in: this namespace loads before it."
  [analyze ctx items env]
  (when-not (= 9 (count items))
    (analysis-error :analyze/invalid-native-region
                    (str "jolt.native/__region expects (__region sym src [kinds] result [args] "
                         "fallback raise mode); use the jolt.native/native macro")))
  (let [sym (nth items 1) src (nth items 2) kinds-form (nth items 3)
        args-form (nth items 5)]
    (when-not (native-region-sym? sym)
      (analysis-error :analyze/invalid-native-region
                      (str "jolt.native/__region: the symbol must be jn_ and 16 hex digits, got "
                           (pr-str sym))))
    (when-not (string? src)
      (analysis-error :analyze/invalid-native-region "jolt.native/__region: the C source must be a string"))
    (when-not (and (form-vec? kinds-form) (form-vec? args-form))
      (analysis-error :analyze/invalid-native-region "jolt.native/__region: kinds and args must be vectors"))
    (let [kinds (mapv #(native-kind % native-param-kinds "parameter") (form-vec-items kinds-form))
          args (form-vec-items args-form)]
      (when-not (= (count kinds) (count args))
        (analysis-error :analyze/invalid-native-region
                        "jolt.native/__region: one kind per argument"))
      {:op :native-region
       :sym sym
       :src src
       :kinds kinds
       :result (native-kind (nth items 4) native-result-kinds "result")
       :args (mapv #(analyze ctx % env) args)
       :fallback (analyze ctx (nth items 6) env)
       :raise (analyze ctx (nth items 7) env)
       ;; :pinned (lock + collect-safe call) or :hold-gc (plain call, holds the GC)
       :mode (native-kind (nth items 8) #{"pinned" "hold-gc"} "mode")})))
