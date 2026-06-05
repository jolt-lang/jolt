# Specification: transducers.
(use ../support/harness)

(defspec "transducers / into"
  ["map xform"          "[2 3 4]"   "(into [] (map inc) [1 2 3])"]
  ["filter xform"       "[2 4]"     "(into [] (filter even?) [1 2 3 4])"]
  ["remove xform"       "[1 3]"     "(into [] (remove even?) [1 2 3 4])"]
  ["take xform"         "[1 2]"     "(into [] (take 2) [1 2 3 4])"]
  ["drop xform"         "[3 4]"     "(into [] (drop 2) [1 2 3 4])"]
  ["take-while xform"   "[1 2]"     "(into [] (take-while (fn [x] (< x 3))) [1 2 3 1])"]
  ["keep xform"         "[1 3]"     "(into [] (keep (fn [x] (if (odd? x) x nil))) [1 2 3 4])"]
  ["map-indexed xform"  "[[0 :a] [1 :b]]" "(into [] (map-indexed vector) [:a :b])"]
  ["mapcat xform"       "[1 1 2 2]" "(into [] (mapcat (fn [x] [x x])) [1 2])"]
  ["cat xform"          "[1 2 3 4]" "(into [] cat [[1 2] [3 4]])"]
  ["into a set"         "#{2 3 4}"  "(into #{} (map inc) [1 2 3])"])

# transducer comp applies left-to-right: (comp (map a) (filter b)) maps then filters
(defspec "transducers / compose"
  ["comp map+filter"    "[2 4 6 8]" "(into [] (comp (map (fn [x] (* x 2))) (filter even?)) [1 2 3 4])"]
  ["comp filter+map"    "[2 4]"     "(into [] (comp (filter odd?) (map inc)) [1 2 3 4])"]
  ["comp three"         "[2]"       "(into [] (comp (map inc) (filter even?) (take 1)) [1 2 3 4])"])

(defspec "transducers / transduce & sequence"
  ["transduce sum"      "9"        "(transduce (map inc) + [1 2 3])"]
  ["transduce init"     "19"        "(transduce (map inc) + 10 [1 2 3])"]
  ["transduce filter"   "6"         "(transduce (filter even?) + [1 2 3 4])"]
  ["sequence xform"     "[2 3 4]"   "(sequence (map inc) [1 2 3])"]
  ["eduction"           "[2 3 4]"   "(into [] (eduction (map inc) [1 2 3]))"]
  ["completing"         "9"        "(transduce (map inc) (completing +) 0 [1 2 3])"])

# A `take`/`take-while` transducer returns `reduced`, which must short-circuit
# the reduction so transducing over an INFINITE seq terminates rather than
# realizing it eagerly.
(defspec "transducers / short-circuit over infinite seqs"
  ["into take (range)"        "[0 1 2 3 4]" "(into [] (take 5) (range))"]
  ["transduce take (range)"   "3"           "(transduce (take 3) + 0 (range))"]
  ["sequence take (range)"    "[0 1 2 3 4]" "(sequence (take 5) (range))"]
  ["take-while over (range)"  "[0 1 2]"     "(into [] (take-while (fn [x] (< x 3))) (range))"]
  ["comp take over (range)"   "[1 3 5]"     "(into [] (comp (filter odd?) (take 3)) (range))"]
  ["into take iterate"        "[0 1 2 3 4]" "(into [] (take 5) (iterate inc 0))"])

# `reduce` itself honors `reduced`, so a reducing fn that returns `reduced`
# terminates even over an infinite seq.
(defspec "reduce / honors reduced"
  ["reduced short-circuits inf" "105"
   "(reduce (fn [a x] (if (> a 100) (reduced a) (+ a x))) 0 (range))"]
  ["reduce take inf"            "10"  "(reduce + (take 5 (range)))"]
  ["reduce no-init first elem"  "6"   "(reduce + [1 2 3])"]
  ["reduce no-init single"      "42"  "(reduce + [42])"]
  ["reduce empty calls f"       "0"   "(reduce + [])"]
  ["reduce with-init"           "16"  "(reduce + 10 [1 2 3])"]
  ["reduce reduced immediate"   ":x"  "(reduce (fn [a x] (reduced :x)) :init [1 2 3])"])
