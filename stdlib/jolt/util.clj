(ns jolt.util
  "General-purpose helpers for Jolt code and libraries.")

(defmacro import-vars
  "Re-export the public vars of `from-ns` into the current namespace as thin
  delegating definitions. A function becomes a fn that applies the source var,
  and a macro becomes a macro that expands to a call of the source macro.

  Because the definitions are static and resolve the source at call time, they
  bake into an AOT-built binary and do not depend on load order — unlike a
  runtime `intern` over `ns-publics`, which an AOT build cannot reproduce. This
  is the tool for putting a public face on a vendored library (see how jolt.fs
  wraps babashka.fs).

  `from-ns` must already be required. Options:

    :exclude  a set of symbols not to re-export

  Example:

    (ns my.fs
      (:require [babashka.fs]
                [jolt.util :refer [import-vars]]))
    (import-vars babashka.fs :exclude #{zip unzip gzip gunzip})"
  [from-ns & {:keys [exclude]}]
  (let [excl (set exclude)]
    (cons `do
          (for [[sym v] (ns-publics from-ns)
                :when (not (contains? excl sym))
                :let [target (symbol (name from-ns) (name sym))]]
            (if (:macro (meta v))
              (list 'defmacro sym ['& 'args] (list 'clojure.core/cons (list 'quote target) 'args))
              (list 'defn sym ['& 'args] (list 'clojure.core/apply target 'args)))))))
