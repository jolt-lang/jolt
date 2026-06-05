# Specification: metadata.
(use ../support/harness)

(defspec "metadata / with-meta & meta"
  ["meta of bare value"  "nil"        "(meta [1 2 3])"]
  ["with-meta then meta"  "{:a 1}"    "(meta (with-meta [1 2 3] {:a 1}))"]
  ["with-meta preserves value" "true" "(= [1 2 3] (with-meta [1 2 3] {:a 1}))"]
  ["with-meta on map"     "{:doc \"x\"}" "(meta (with-meta {:k 1} {:doc \"x\"}))"]
  ["vary-meta"            "{:a 2}"     "(meta (vary-meta (with-meta [1] {:a 1}) update :a inc))"]
  ["meta reader ^"        "{:tag :int}" "(meta ^{:tag :int} [1 2])"]
  ["with-meta on fn ok"   "true"       "(fn? (with-meta inc {:a 1}))"])
