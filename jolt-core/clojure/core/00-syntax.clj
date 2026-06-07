;; clojure.core — syntax tier. The control macros the compiler and every later
;; tier depend on (when/cond/and/or/...), expressed as defmacro. Loaded FIRST
;; (before 00-kernel), interpreted, so the macros exist before any code that uses
;; them is compiled — including the kernel tier, the self-hosted analyzer, and the
;; seq/coll tiers.
;;
;; CONSTRAINT: a macro here may use ONLY special forms (if/do/let*/fn*/not) and
;; core-renames SEED primitives (first/next/rest/nth/count/empty?/...). It must
;; NOT use kernel-tier fns (second/peek/subvec/...) or anything defined later —
;; those don't exist yet when this tier loads.

(defmacro when [test & body]
  `(if ~test (do ~@body)))

(defmacro when-not [test & body]
  `(if (not ~test) (do ~@body)))

(defmacro and [& exprs]
  (if (empty? exprs)
    true
    (if (empty? (rest exprs))
      (first exprs)
      `(let* [and# ~(first exprs)] (if and# (and ~@(rest exprs)) and#)))))

(defmacro or [& exprs]
  (if (empty? exprs)
    nil
    (if (empty? (rest exprs))
      (first exprs)
      `(let* [or# ~(first exprs)] (if or# or# (or ~@(rest exprs)))))))

;; :else (any truthy value) is just a test, so no special case — (if :else e ...)
;; takes e.
(defmacro cond [& clauses]
  (if (empty? clauses)
    nil
    `(if ~(first clauses) ~(nth clauses 1) (cond ~@(drop 2 clauses)))))
