# Specification: metadata.
(use ../support/harness)

(defspec "metadata / with-meta & meta"
  ["meta of bare value"  "nil"        "(meta [1 2 3])"]
  ["with-meta then meta"  "{:a 1}"    "(meta (with-meta [1 2 3] {:a 1}))"]
  ["with-meta preserves value" "true" "(= [1 2 3] (with-meta [1 2 3] {:a 1}))"]
  ["with-meta on map"     "{:doc \"x\"}" "(meta (with-meta {:k 1} {:doc \"x\"}))"]
  ["vary-meta"            "{:a 2}"     "(meta (vary-meta (with-meta [1] {:a 1}) update :a inc))"]
  ["vary-meta extra args" "{:a 1 :b 2}" "(meta (vary-meta (with-meta [1] {:a 1}) assoc :b 2))"]
  ["meta reader ^"        "{:tag :int}" "(meta ^{:tag :int} [1 2])"]
  ["with-meta on fn ok"   "true"       "(fn? (with-meta inc {:a 1}))"]
  ["with-meta nil clears" "nil"        "(meta (with-meta [1 2 3] nil))"])

(defspec "metadata / type hints"
  # ^Type / ^:kw / ^"str" on a symbol attach as metadata and are otherwise inert:
  # the symbol stays a symbol so hints are transparent in every position.
  ["type hint on param"      "\"hi\""  "(do (defn f [^String s] s) (f \"hi\"))"]
  ["type hint, extra params" "[1 2]"   "(do (defn g [^String x y] [x y]) (g 1 2))"]
  ["type hint in let"        "6"       "(let [^long x 5] (inc x))"]
  ["type hint in body"       "2"       "(let [s \"ab\"] (count ^String s))"]
  ["type hint in destructure" "3"      "(let [{:keys [^long a]} {:a 3}] a)"]
  ["symbol hint -> :tag"     "\"String\"" "(:tag (meta (read-string \"^String x\")))"]
  ["keyword hint -> true"    "true"     "(:foo (meta (read-string \"^:foo x\")))"])

(defspec "metadata / def metadata"
  ["^:dynamic var binds"     "9"       "(do (def ^:dynamic *d* 1) (binding [*d* 9] *d*))"]
  ["^:private on var"        "true"    "(do (def ^:private pv 1) (:private (meta (var pv))))"]
  ["^Type tag on var"        "\"String\"" "(do (def ^String tv \"a\") (:tag (meta (var tv))))"]
  ["^{:doc} on var"          "\"hi\""  "(do (def ^{:doc \"hi\"} dv 1) (:doc (meta (var dv))))"]
  ["(def name doc val) doc"  "\"d\""   "(do (def dd \"d\" 5) (:doc (meta (var dd))))"])
