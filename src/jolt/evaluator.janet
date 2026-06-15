# Jolt Evaluator
# Direct interpreter for Clojure forms on Janet.
#
# This file is the AGGREGATOR (jolt-oudv, phase 2a): the interpreter is now split
# into cluster modules, loaded here in dependency order and re-exported
# (:export true) so every consumer keeps a single `(use ./evaluator)`. The
# eval-form entry (set at the bottom) ties resolution, special forms and the
# collection/map literal evaluation together.

(use ./types)
(use ./phm)
(use ./phs)
(use ./lazyseq)
(use ./pv)
(use ./plist)
(use ./config)
(use ./reader)
(use ./regex)

(import ./eval_base :prefix "" :export true)
(import ./eval_resolve :prefix "" :export true)
(import ./eval_runtime :prefix "" :export true)
(import ./eval_special :prefix "" :export true)

(defn- map-needs-phm? [kvs]
  (var need false) (var i 0)
  (while (< i (length kvs))
    (let [k (in kvs i) v (in kvs (+ i 1))]
      (when (or (table? k) (array? k) (nil? k) (nil? v)) (set need true) (break)))
    (+= i 2))
  need)

(defn- build-eval-map [kvs]
  (if (map-needs-phm? kvs)
    (do (var m (make-phm)) (var j 0)
        (while (< j (length kvs)) (set m (phm-assoc m (in kvs j) (in kvs (+ j 1)))) (+= j 2)) m)
    (struct ;kvs)))

(set eval-form (fn [ctx bindings form]
  (cond
    (nil? form) nil
    (number? form) form
    (string? form) form
    (keyword? form) form
    (bytes? form) form
    (buffer? form) form
    (tuple? form)
      (let [els (map |(eval-form ctx bindings $) form)]
        (if mutable? (array ;els) (pv-from-indexed els)))
    (struct? form)
    (if (= :symbol (form :jolt/type))
      (resolve-sym ctx bindings form)
      (if (= :jolt/char (form :jolt/type))
        form
      # a UUID/inst value flowing back through eval (macro expansion, eval of a
      # read form) is a self-evaluating literal, like chars. A namespace object
      # does too: `~*ns*` in a syntax-quote (clojure.tools.logging) splices the
      # live ns into the expansion.
      (if (or (= :jolt/uuid (form :jolt/type)) (= :jolt/inst (form :jolt/type))
              (= :jolt/namespace (form :jolt/type)))
        form
      (if (= :jolt/set (form :jolt/type))
        # evaluate each element (set literals like #{(inc 1)} must compute)
        (apply make-phs (map |(eval-form ctx bindings $) (form :value)))
      (if (= :jolt/tagged (form :jolt/type))
        (let [tag (form :tag)
              data-readers (get (ctx :env) :data-readers)
              reader-fn (if data-readers (get data-readers tag))]
          (cond
            # #"..." regex literal -> a regex value (Janet PEG-backed)
            (= tag :regex) (compile-regex (form :form))
            reader-fn (reader-fn (form :form))
            (error (string "No reader function for tag " tag))))
      (if (get form :jolt/type)
        (error (string "Unexpected tagged form: " (form :jolt/type)))
        # plain map literal: evaluate keys and values in SOURCE order when
        # the reader order rides along (jolt-p3c), hash order otherwise
        (let [kvs @[]
              order (form-kv-order form)]
          (if order
            (each x order (array/push kvs (eval-form ctx bindings x)))
            (each k (keys form)
              (array/push kvs (eval-form ctx bindings k))
              (array/push kvs (eval-form ctx bindings (get form k)))))
          (build-eval-map kvs))))))))
    # A phm map-literal FORM (reader emits one for {:a nil} etc., which a struct
    # would have dropped): evaluate its key/value forms and rebuild, preserving nil.
    (phm? form)
    (let [kvs @[]
          order (form-kv-order form)]
      (if order
        (each x order (array/push kvs (eval-form ctx bindings x)))
        (each e (phm-entries form)
          (array/push kvs (eval-form ctx bindings (in e 0)))
          (array/push kvs (eval-form ctx bindings (in e 1)))))
      (build-eval-map kvs))
    (array? form)
    (if (= 0 (length form))
      @[]
      (eval-list ctx bindings form))
    form)))
