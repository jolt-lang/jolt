;; The MIT License (MIT)
;;
;; Copyright (c) 2016 Richard Hull
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.


(ns jolt.infix
  (:require
   [jolt.parser :refer [parse-all]]
   [jolt.infix.core :refer [rewrite resolve-alias base-env]]
   [jolt.infix.grammar :refer [expression]]))

(defn- costly-op?
  "A division or a Math call: ops whose generic path dispatches on the operand
   types, so a double branch pays for its guard. `+`, `-` and `*` alone already
   take an inline fixnum/flonum path, and a guard there costs a long input more
   than the double branch saves (17 vs 21 ms per 3M calls of `x * 3 + y - x`)."
  [op]
  (and (symbol? op)
       (or (= '/ op)
           (= "Math" (namespace op)))))

(defn- costly-arithmetic? [form]
  (and (seq? form)
       (or (costly-op? (first form))
           (boolean (some costly-arithmetic? (rest form))))))

(defn- operand-locals
  "The locals (keys of the macro's &env) the prefix form reads as operands,
   in order of first use. A call's head is not an operand."
  [env form]
  (cond
    (symbol? form) (when (contains? env form) [form])
    (seq? form)    (mapcat #(operand-locals env %) (rest form))
    :else          nil))

(defn- specialize
  "Guards the prefix form with a branch for the case where every local it reads
   is a double. Both branches are the same form, so no answer changes: in the
   first, each local is rebound through `double`, which lets the compiler prove
   it a flonum and lower the arithmetic and Math calls to native flonum ops
   instead of generic dispatch. Longs, ratios and anything else take the
   second branch, the expansion as it always was. Only a form with a division
   or a Math call gets the guard (see costly-op?)."
  [env form]
  (let [locals (distinct (operand-locals env form))]
    (if (and (seq locals) (costly-arithmetic? form))
      `(if (and ~@(map (fn [l] `(double? ~l)) locals))
         (let [~@(mapcat (fn [l] [l `(double ~l)]) locals)] ~form)
         ~form)
      form)))

(defmacro infix
  "Takes an infix expression, resolves any aliases before rewriting the
   infix expressions into standard LISP prefix expressions. When the
   expression does arithmetic on locals, it also gets a fast path taken when
   all of those locals are doubles at run time."
  [& expr]
  (->>
   expr
   (map resolve-alias)
   rewrite
   (specialize &env)))

;; Short alias for infix
(defmacro $=
  "Takes an infix expression, resolves any aliases before rewriting the
   infix expressions into standard LISP prefix expressions."
  [& expr]
  `(infix ~@expr))

(defn- binding-vars [bindings]
  (->>
   bindings
   (map #(vector (keyword %) (gensym "arg_")))
   (into {})))

(defmacro from-string
  ([expr]
   `(from-string [] ~expr))

  ([bindings expr]
   ;; Reference the base-env var at runtime rather than splicing its value:
   ;; the map holds live fn objects, which jolt can't embed as a compile-time
   ;; constant (JVM Clojure tolerates it, but referring the var is portable).
   `(from-string ~bindings base-env ~expr))

  ([bindings env expr]
   (cond
     (not (vector? bindings))
     (throw (IllegalArgumentException. "Binding variables is not a vector"))

     :else
     (let [b# (binding-vars bindings)
           p# (mapv keyword bindings)]
       `(if-let [f# (parse-all expression ~expr)]
          (with-meta
            (fn [~@(vals b#)]
              (f# (merge ~env ~b#)))
            {:doc ~expr :params ~p#}))))))
