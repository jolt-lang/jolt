;; Forward-declaration stubs for SCI's host-layer modules.
;;
;; sci.impl.{parser,read,load,macroexpand,resolve,proxy,reify} implement SCI's
;; reader, loader, macroexpander and JVM proxy/reify support. On the JVM those
;; lean on tools.reader, java.io and reflection; Jolt has its own native
;; reader/loader, so rather than load SCI's host implementation we provide just
;; the var names that sci.impl.namespaces references when it builds the
;; clojure.core binding map. The map is built at load time but these are only
;; *called* through SCI's own runtime, which Jolt replaces — so no-op
;; placeholders are sufficient. (They must be non-nil so `:refer` re-interns
;; them: interning a nil-valued var drops the binding.)

(ns sci.impl.parser)
(defn parse-next [& _] nil)
(def data-readers {})
(defn default-data-reader-fn [& _] nil)
(defn read-eval [& _] nil)
(defn reader-resolver [& _] nil)
(defn suppress-read [& _] nil)

(ns sci.impl.read)
(defn read [& _] nil)
(defn read-string [& _] nil)
(defn source-logging-reader [& _] nil)

(ns sci.impl.load)
(defn load-string [& _] nil)
(defn load-reader [& _] nil)
(defn add-loaded-lib [& _] nil)
(defn eval-refer [& _] nil)
(defn eval-refer-global [& _] nil)
(defn eval-require [& _] nil)
(defn eval-require-global [& _] nil)
(defn eval-use [& _] nil)

(ns sci.impl.macroexpand)
(defn macroexpand [& _] nil)
(defn macroexpand-1 [& _] nil)

(ns sci.impl.resolve)
(defn resolve-symbol [& _] nil)

(ns sci.impl.proxy)
(defn proxy [& _] nil)
(defn proxy* [& _] nil)

(ns sci.impl.reify)
(defn reify [& _] nil)
(defn reify* [& _] nil)
