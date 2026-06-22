;; clojure.pprint — minimal jolt shim.
;;
;; The real clojure.pprint is a full pretty-printer (thousands of lines of column
;; tracking / dispatch tables). This shim provides just the surface that portable
;; libraries reach for when they "pretty print" a value — notably
;; clojure.tools.logging/spy (pprint + with-pprint-dispatch + code-dispatch).
;; pprint writes the value readably with a trailing newline (no pretty layout);
;; the dispatch vars are recognized but not used for layout.
(ns clojure.pprint)

(def code-dispatch
  "Recognized but not used for layout on jolt." :code-dispatch)
(def simple-dispatch
  "Recognized but not used for layout on jolt." :simple-dispatch)

(def ^:dynamic *print-pprint-dispatch* simple-dispatch)

(defn pprint
  "Print object readably followed by a newline. Not a pretty-printer on jolt —
  no column-aware layout. jolt routes all printing through the host output seam
  (with-out-str captures it), and *out* is not a bindable var, so an explicit
  writer arg is accepted for API compatibility but not honored — output goes to
  the current output. (The old `(binding [*out* writer] ...)` never redirected
  anything either, and made the defn fall back to the interpreter; dropping it
  lets pprint compile cleanly, which the no-fallback Chez back end requires.)"
  ([object] (prn object))
  ([object _writer] (prn object)))

(defmacro with-pprint-dispatch
  "Evaluate body with the given pprint dispatch selected. On jolt the dispatch is
  recognized but does not affect layout, so this just evaluates body."
  [_dispatch & body]
  `(do ~@body))
