;; clojure.tools.logging.impl — jolt shim.
;;
;; The real impl probes slf4j / log4j / log4j2 / commons-logging / java.util.logging
;; via Class/forName and reflection. jolt has no JVM, so find-factory returns a
;; native stderr-backed factory instead. The Logger and LoggerFactory protocols
;; match the real signatures, so consumers (and any custom factory bound through
;; clojure.tools.logging/*logger-factory*) work unchanged.
(ns clojure.tools.logging.impl)

(defprotocol Logger
  (enabled? [logger level])
  (write! [logger level throwable message]))

(defprotocol LoggerFactory
  (name [factory])
  (get-logger [factory logger-ns]))

;; Minimum level the default jolt stderr logger emits. trace and debug are
;; suppressed by default; bind or alter-var-root this to change the threshold.
(def ^:dynamic *level* :info)

(def ^:private level-order
  {:trace 0 :debug 1 :info 2 :warn 3 :error 4 :fatal 5})

;; The protocol method `name` shadows clojure.core/name in this namespace, so a
;; keyword->label uses an explicit table rather than (name level).
(defn- level-str [level]
  (case level
    :trace "TRACE" :debug "DEBUG" :info "INFO"
    :warn "WARN" :error "ERROR" :fatal "FATAL"
    (str level)))

(defn- stderr-logger [logger-ns]
  (reify Logger
    (enabled? [_ level]
      (>= (get level-order level 2) (get level-order *level* 2)))
    (write! [_ level throwable message]
      (__eprint
        (str (level-str level) " " logger-ns " - " message
             (when throwable (str " " throwable)))))))

(def ^:private jolt-stderr-factory
  (reify LoggerFactory
    (name [_] "jolt/stderr")
    (get-logger [_ logger-ns] (stderr-logger (str logger-ns)))))

(defn find-factory
  "jolt has no JVM logging backends; return the native stderr factory."
  []
  jolt-stderr-factory)
