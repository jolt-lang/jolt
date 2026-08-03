;;   Copyright (c) Rich Hickey. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.stacktrace
  "Print stack traces oriented towards Clojure, not Java.

  Ported from Clojure's own clojure/stacktrace.clj. jolt compiles tail calls, so
  a Throwable's stack trace is empty here where the JVM has frames — every fn in
  this namespace still works, and print-stack-trace reports \"[empty stack
  trace]\" rather than a frame list. That is the :trace divergence recorded in
  test/conformance/known-divergences.edn, not a gap in this namespace. The cause
  chain, the throwable line and ex-data all print exactly as on the JVM, which is
  what a test runner (kaocha reaches for print-cause-trace) actually reports."
  {:author "Stuart Sierra"})

(defn root-cause
  "Returns the last 'cause' Throwable in a chain of Throwables."
  {:added "1.1"}
  [tr]
  (if-let [cause (.getCause tr)]
    (recur cause)
    tr))

(defn print-trace-element
  "Prints a Clojure-oriented view of one element in a stack trace."
  {:added "1.1"}
  [e]
  (let [class (.getClassName e)
        method (.getMethodName e)]
    (let [match (re-matches #"^([A-Za-z0-9_.-]+)\$(\w+)__\d+$" (str class))]
      (if (and match (= "invoke" method))
        (apply printf "%s/%s" (rest match))
        (printf "%s.%s" class method))))
  (printf " (%s:%d)" (or (.getFileName e) "") (.getLineNumber e)))

(defn print-throwable
  "Prints the class and message of a Throwable."
  {:added "1.1"}
  [tr]
  (printf "%s: %s" (.getName (class tr)) (.getMessage tr))
  (when-let [info (ex-data tr)]
    (newline)
    (pr info)))

(defn print-stack-trace
  "Prints a Clojure-oriented stack trace of tr, a Throwable.
  Prints a maximum of n stack frames (default: unlimited).
  Does not print chained exceptions (causes)."
  {:added "1.1"}
  ([tr] (print-stack-trace tr nil))
  ([tr n]
   (let [st (.getStackTrace tr)]
     (print-throwable tr)
     (newline)
     (print " at ")
     (if-let [e (first st)]
       (print-trace-element e)
       (print "[empty stack trace]"))
     (newline)
     (doseq [e (if (nil? n) (rest st) (take (dec n) (rest st)))]
       (print " ")
       (print-trace-element e)
       (newline)))))

(defn print-cause-trace
  "Like print-stack-trace but prints chained exceptions (causes)."
  {:added "1.1"}
  ([tr] (print-cause-trace tr nil))
  ([tr n]
   (print-stack-trace tr n)
   (when-let [cause (.getCause tr)]
     (print "Caused by: ")
     (recur cause n))))

(defn e
  "REPL utility.  Prints a brief stack trace for the root cause of the
  most recent exception."
  {:added "1.1"}
  []
  (print-stack-trace (root-cause *e) 8))
