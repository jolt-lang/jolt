;; Verbatim from clojure.template (Stuart Sierra) — pure Clojure over
;; clojure.walk, which jolt ships. Added so honeysql's :clj branch (which
;; requires clojure.template) loads.
(ns clojure.template
  "Macros that expand to repeated copies of a template expression."
  (:require [clojure.walk :as walk]))

(defn apply-template
  "For use in macros.  argv is an argument list, as in defn.  expr is
  a quoted expression using the symbols in argv.  values is a sequence
  of values to be used for the arguments.

  apply-template will recursively replace argument symbols in expr
  with their corresponding values, returning a modified expr.

  Example: (apply-template '[x] '(+ x x) '[2])
           ;=> (+ 2 2)"
  [argv expr values]
  (assert (vector? argv))
  (assert (every? symbol? argv))
  (walk/postwalk-replace (zipmap argv values) expr))

(defmacro do-template
  "Repeatedly copies expr (in a do block) for each group of arguments
  in values.  values are automatically partitioned by the number of
  arguments in argv, an argument vector as in defn.

  Example: (macroexpand '(do-template [x y] (+ y x) 2 4 3 5))
           ;=> (do (+ 4 2) (+ 5 3))"
  [argv expr & values]
  (let [c (count argv)]
    `(do ~@(map (fn [a] (apply-template argv expr a))
                (partition c values)))))
