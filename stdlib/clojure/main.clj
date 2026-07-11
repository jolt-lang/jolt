(ns clojure.main)

;; The subset of clojure.main libraries reach for at runtime. The REPL entry
;; points live in the jolt CLI, not here.

(defn demunge
  "Given a string representation of a fn class, as in a stack trace element,
  returns a readable version."
  [fn-name]
  (clojure.lang.Compiler/demunge fn-name))

(defn root-cause
  "Returns the initial cause of an exception chain."
  [t]
  (loop [cause t]
    (if-let [c (ex-cause cause)]
      (recur c)
      cause)))
