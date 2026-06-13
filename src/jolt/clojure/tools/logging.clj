;; clojure.tools.logging — jolt shim of the public API.
;;
;; Faithful to the upstream macro surface (logp/logf, the level macros and their
;; f-variants, log, enabled?, spy), dispatching through the Logger/LoggerFactory
;; protocols in clojure.tools.logging.impl. Two deliberate departures from
;; upstream, both forced by jolt not being a JVM:
;;
;;   - log* writes directly. The real log* may hand off to an agent when inside a
;;     running clojure.lang.LockingTransaction; jolt has neither agents nor STM.
;;   - The throwable-first-argument branch is dropped. Upstream splits args on
;;     (instance? Throwable x); on jolt that is always false for exception values,
;;     so the would-be throwable simply becomes part of the printed message. This
;;     also avoids the double-evaluation the branch would otherwise cause here.
(ns clojure.tools.logging
  (:require [clojure.tools.logging.impl :as impl]))

;; The LoggerFactory used to obtain loggers. Defaults to the jolt stderr
;; factory; rebind to plug in a different backend.
(def ^:dynamic *logger-factory* (impl/find-factory))

(defn log*
  "Write a log entry through the logger. Direct, unlike upstream's agent path."
  [logger level throwable message]
  (impl/write! logger level throwable message)
  nil)

(defmacro logp
  "Log a message built by print-str of args, at the given level. Args are
  evaluated only when the level is enabled."
  [level & args]
  `(let [logger# (clojure.tools.logging.impl/get-logger *logger-factory* ~(str *ns*))]
     (when (clojure.tools.logging.impl/enabled? logger# ~level)
       (log* logger# ~level nil (print-str ~@args)))))

(defmacro logf
  "Log a clojure.core/format message at the given level. Args are evaluated
  only when the level is enabled."
  [level & args]
  `(let [logger# (clojure.tools.logging.impl/get-logger *logger-factory* ~(str *ns*))]
     (when (clojure.tools.logging.impl/enabled? logger# ~level)
       (log* logger# ~level nil (format ~@args)))))

(defmacro log
  ([level message] `(logp ~level ~message))
  ([level throwable message] `(logp ~level ~throwable ~message)))

(defmacro trace [& args] `(logp :trace ~@args))
(defmacro debug [& args] `(logp :debug ~@args))
(defmacro info  [& args] `(logp :info ~@args))
(defmacro warn  [& args] `(logp :warn ~@args))
(defmacro error [& args] `(logp :error ~@args))
(defmacro fatal [& args] `(logp :fatal ~@args))

(defmacro tracef [& args] `(logf :trace ~@args))
(defmacro debugf [& args] `(logf :debug ~@args))
(defmacro infof  [& args] `(logf :info ~@args))
(defmacro warnf  [& args] `(logf :warn ~@args))
(defmacro errorf [& args] `(logf :error ~@args))
(defmacro fatalf [& args] `(logf :fatal ~@args))

(defmacro enabled?
  "True if the given level is enabled for the (optionally given) logger ns."
  ([level] `(clojure.tools.logging.impl/enabled? (clojure.tools.logging.impl/get-logger *logger-factory* ~(str *ns*)) ~level))
  ([level log-ns] `(clojure.tools.logging.impl/enabled? (clojure.tools.logging.impl/get-logger *logger-factory* ~log-ns) ~level)))

(defmacro spy
  "Evaluate expr, log it at the given level (default :debug) as expr => value,
  and return the value."
  ([expr] `(spy :debug ~expr))
  ([level expr]
   `(let [a# ~expr]
      (logp ~level (print-str '~expr "=>" (pr-str a#)))
      a#)))
