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
  "Print object readably followed by a newline. Writes to *out* (or the given
  writer). Not a pretty-printer on jolt — no column-aware layout."
  ([object] (prn object))
  ([object writer] (binding [*out* writer] (prn object))))

(defmacro with-pprint-dispatch
  "Evaluate body with the given pprint dispatch selected. On jolt the dispatch is
  recognized but does not affect layout, so this just evaluates body."
  [_dispatch & body]
  `(do ~@body))
