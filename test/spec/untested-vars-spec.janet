# Specification: spec rows for every previously-untested implemented var
# (jolt-brh follow-up — promotes the dashboard's implemented-untested category).
# Rows assert jolt's ACTUAL documented behavior, including the stub families:
# arrays are vectors/host buffers, proxies/JVM reflection are resolve-only,
# unchecked-* are plain double ops, chunks are eager equivalents.
(use ../support/harness)

(defspec "untested / primed + division + bit ops"
  ["+'"        "3"  "(+' 1 2)"]
  ["-'"        "3"  "(-' 5 2)"]
  ["*'"        "12" "(*' 3 4)"]
  ["inc'"      "2.5" "(inc' 1.5)"]
  ["dec'"      "1.5" "(dec' 2.5)"]
  ["/"         "2"  "(/ 6 3)"]
  ["/ ratio-as-double" "0.5" "(/ 1 2)"]
  ["bit-not"   "-6" "(bit-not 5)"]
  ["bit-and-not" "4" "(bit-and-not 12 10)"]
  ["bit-flip"  "3"  "(bit-flip 2 0)"]
  ["unsigned-bit-shift-right" "2" "(unsigned-bit-shift-right 8 2)"])

(defspec "untested / hash family"
  ["hash stable"       "true" "(= (hash :a) (hash :a))"]
  ["hash int"          "true" "(int? (hash [1 2]))"]
  ["hash-combine"      "true" "(int? (hash-combine 1 2))"]
  ["hash-ordered-coll" "true" "(int? (hash-ordered-coll [1 2]))"]
  ["hash-unordered-coll" "true" "(int? (hash-unordered-coll #{1}))"])

(defspec "untested / array stubs (vectors + host buffers)"
  ["make-array"   "(quote (nil nil nil))" "(make-array 3)"]
  ["into-array"   "(quote (1 2))" "(into-array [1 2])"]
  ["to-array"     "(quote (1 2))" "(to-array [1 2])"]
  ["aclone vec"   "(quote (1 2))" "(aclone [1 2])"]
  ["aclone independent" "(quote (9 2))" "(let [a (aclone (to-array [1 2]))] (aset a 0 9) (seq a))"]
  ["aset/aget"    "9"    "(let [a (to-array [1 2 3])] (aset a 0 9) (aget a 0))"]
  ["aset-int"     "7"    "(let [a (to-array [1 2])] (aset-int a 0 7) (aget a 0))"]
  ["aset-boolean" "true" "(let [a (to-array [1])] (aset-boolean a 0 true) (aget a 0))"]
  ["aset-byte"    "9"    "(let [a (to-array [0])] (aset-byte a 0 9) (aget a 0))"]
  ["aset-char"    "\\a"  "(let [a (to-array [0])] (aset-char a 0 \\a) (aget a 0))"]
  ["aset-double"  "1.5"  "(let [a (to-array [0])] (aset-double a 0 1.5) (aget a 0))"]
  ["aset-float"   "2.5"  "(let [a (to-array [0])] (aset-float a 0 2.5) (aget a 0))"]
  ["aset-long"    "3"    "(let [a (to-array [0])] (aset-long a 0 3) (aget a 0))"]
  ["aset-short"   "4"    "(let [a (to-array [0])] (aset-short a 0 4) (aget a 0))"]
  ["boolean-array" "(quote (false false))" "(boolean-array 2)"]
  ["int-array"    "(quote (1 2))" "(int-array [1 2])"]
  ["long-array"   "(quote (0 0))" "(long-array 2)"]
  ["double-array" "(quote (0 0))" "(double-array 2)"]
  ["float-array"  "(quote (0 0))" "(float-array 2)"]
  ["short-array"  "(quote (0 0))" "(short-array 2)"]
  ["char-array count" "2" "(count (char-array 2))"]
  ["byte-array bytes?" "true" "(bytes? (byte-array 2))"]
  ["bytes? not vec" "false" "(bytes? [1])"])

(defspec "untested / typed coercion views"
  ["booleans" "(quote (true))" "(booleans [true])"]
  ["doubles"  "(quote (1))"    "(doubles [1.0])"]
  ["floats"   "(quote (1))"    "(floats [1.0])"]
  ["ints"     "(quote (1))"    "(ints [1])"]
  ["longs"    "(quote (1))"    "(longs [1])"]
  ["shorts"   "(quote (1))"    "(shorts [1])"]
  ["chars first" "\\a"         "(first (chars [\\a]))"]
  ["bytes view"  "true"        "(bytes? (bytes [65]))"]
  ["byte"     "65" "(byte 65)"]
  ["short"    "1"  "(short 1)"]
  ["long truncates" "1" "(long 1.7)"]
  ["double"   "3"  "(double 3)"]
  ["float"    "3"  "(float 3)"])

(defspec "untested / unchecked-* are plain ops"
  ["unchecked-add"      "3" "(unchecked-add 1 2)"]
  ["unchecked-add-int"  "3" "(unchecked-add-int 1 2)"]
  ["unchecked-subtract" "3" "(unchecked-subtract 5 2)"]
  ["unchecked-subtract-int" "3" "(unchecked-subtract-int 5 2)"]
  ["unchecked-multiply" "6" "(unchecked-multiply 2 3)"]
  ["unchecked-multiply-int" "6" "(unchecked-multiply-int 2 3)"]
  ["unchecked-inc"      "2" "(unchecked-inc 1)"]
  ["unchecked-inc-int"  "2" "(unchecked-inc-int 1)"]
  ["unchecked-dec"      "2" "(unchecked-dec 3)"]
  ["unchecked-dec-int"  "2" "(unchecked-dec-int 3)"]
  ["unchecked-negate"   "-4" "(unchecked-negate 4)"]
  ["unchecked-negate-int" "-4" "(unchecked-negate-int 4)"]
  ["unchecked-divide-int" "3" "(unchecked-divide-int 7 2)"]
  ["unchecked-remainder-int" "1" "(unchecked-remainder-int 7 2)"]
  ["unchecked-int"      "3" "(unchecked-int 3.7)"]
  ["unchecked-long"     "3" "(unchecked-long 3.7)"]
  ["unchecked-double"   "3" "(unchecked-double 3)"]
  ["unchecked-float"    "3" "(unchecked-float 3)"]
  ["unchecked-byte"     "65" "(unchecked-byte 65)"]
  ["unchecked-char"     "97" "(unchecked-char 97)"]
  ["unchecked-short"    "5" "(unchecked-short 5)"])

(defspec "untested / chunk family (eager equivalents) + cat"
  ["chunk round-trip" "1"
   "(let [cb (chunk-buffer 4)] (chunk-append cb 1) (chunk-first (chunk-cons (chunk cb) nil)))"]
  ["cat transducer" "[1 2 3]" "(into [] cat [[1] [2 3]])"]
  ["ensure-reduced wraps" "true" "(reduced? (ensure-reduced 5))"]
  ["ensure-reduced keeps reduced" "true" "(reduced? (ensure-reduced (reduced 5)))"]
  ["halt-when" "4" "(transduce (halt-when even?) conj [] [1 3 4 5])"]
  ["chunk-next exhausted" "nil"
   "(let [cb (chunk-buffer 2)] (chunk-append cb 1) (chunk-next (chunk-cons (chunk cb) nil)))"]
  ["chunk-rest seqable" "()"
   "(let [cb (chunk-buffer 2)] (chunk-append cb 1) (vec (chunk-rest (chunk-cons (chunk cb) nil))))"])

(defspec "untested / JVM-shape stubs (documented jolt behavior)"
  ["class number"  "\"java.lang.Number\"" "(class 1)"]
  ["class string"  "\"java.lang.String\"" "(class \"s\")"]
  ["class keyword" "\"clojure.lang.Keyword\"" "(class :k)"]
  ["class nil"     "nil" "(class nil)"]
  ["bean is the map" "{:a 1}" "(bean {:a 1})"]
  ["biginteger"    "\"5\"" "(str (biginteger 5))"]
  ["proxy resolves nil" "nil" "(proxy [Object] [] (toString [] \"x\"))"]
  ["construct-proxy throws" :throws "(construct-proxy nil)"]
  ["get-proxy-class throws" :throws "(get-proxy-class)"]
  ["init-proxy"    "nil" "(init-proxy nil {})"]
  ["update-proxy"  "nil" "(update-proxy nil {})"]
  ["proxy-mappings" "{}" "(proxy-mappings nil)"]
  ["proxy-call-with-super calls" "1" "(proxy-call-with-super (fn [] 1) nil \"m\")"]
  ["memfn upper"   "\"ABC\"" "((memfn toUpperCase) \"abc\")"]
  ["memfn with args" "2"   "((memfn indexOf needle) \"hello\" \"l\")"]
  ["memfn length"  "3"     "((memfn length) \"abc\")"]
  ["array-seq"     "(quote (1 2 3))" "(array-seq (to-array [1 2 3]))"]
  ["array-seq empty" "nil" "(array-seq (to-array []))"]
  ["proxy-super throws" :throws "(proxy-super count [1])"]
  ["re-groups throws" :throws "(re-groups (re-matcher #\"a\" \"b\"))"]
  ["re-matcher builds" "false" "(nil? (re-matcher #\"a\" \"abc\"))"]
  ["print-dup nil writer throws" :throws "(print-dup 1 nil)"]
  ["print-method nil writer throws" :throws "(print-method 1 nil)"]
  ["uri? string"   "false" "(uri? \"http://x\")"]
  ["uri? nil"      "false" "(uri? nil)"]
  ["definterface defines" "true" "(var? (definterface IFoo (foo [x])))"]
  ["enumeration-seq" "(quote (1 2))" "(enumeration-seq [1 2])"]
  ["iterator-seq"  "(quote (1 2))" "(iterator-seq [1 2])"]
  ["seque passthrough" "(quote (1 2))" "(seque [1 2])"]
  ["delay? true"   "true" "(delay? (delay 1))"]
  ["delay? false"  "false" "(delay? 1)"]
  ["future-call"   "42" "(deref (future-call (fn [] 42)))"]
  [". calls String surface" "3" "(. \"abc\" length)"]
  [".. threads members" "\"ABC\"" "(.. \"abc\" toUpperCase)"]
  ["unknown String member throws" :throws "(. \"abc\" frobnicate)"])

(defspec "untested / protocols: extend + extends?"
  ["extend registers" ":str"
   "(do (defprotocol Pe (pe [x])) (extend (quote String) Pe {:pe (fn [x] :str)}) (pe \"s\"))"]
  ["extend two methods" "[1 2]"
   "(do (defprotocol P3 (pa [x]) (pb [x])) (extend (quote Long) P3 {:pa (fn [x] 1) :pb (fn [x] 2)}) [(pa 0) (pb 0)])"]
  ["extends? after extend" "true"
   "(do (defprotocol P4 (pc [x])) (extend (quote Long) P4 {:pc (fn [x] 1)}) (extends? P4 (quote Long)))"]
  ["extends? without" "false" "(do (defprotocol P5 (pd [x])) (extends? P5 (quote Long)))"])

(defspec "untested / ns + REPL machinery"
  ["all-ns non-empty" "true" "(pos? (count (all-ns)))"]
  ["ns-interns sees def" "true" "(do (def zz 1) (pos? (count (ns-interns (quote user)))))"]
  ["ns-interns countable" "true" "(map? (ns-interns (quote user)))"]
  ["ns-imports empty user" "0" "(count (ns-imports (quote user)))"]
  ["reset-meta!" "{:doc \"d\"}" "(do (def vv 1) (reset-meta! (var vv) {:doc \"d\"}))"]
  ["prefers empty" "{}" "(do (defmulti mm identity) (prefers mm))"]
  ["refer-clojure" "nil" "(refer-clojure)"]
  ["special-symbol? if" "true" "(special-symbol? (quote if))"]
  ["special-symbol? fn name" "false" "(special-symbol? (quote foo))"]
  ["destructure expands" "true" "(pos? (count (destructure (quote [[a b] x]))))"]
  ["seq-to-map-for-destructuring" "{:a 1}" "(seq-to-map-for-destructuring (quote (:a 1)))"]
  ["s2m trailing map passes through" "{:b 2}" "(seq-to-map-for-destructuring (list {:b 2}))"]
  ["s2m unpaired key throws" :throws "(seq-to-map-for-destructuring (quote (:a 1 :b)))"]
  ["s2m kwargs trailing map call" "2" "((fn [& {:keys [b]}] b) {:b 2})"]
  ["*clojure-version* major" "1" "(:major *clojure-version*)"]
  ["*ns* user" "\"user\"" "(str *ns*)"]
  ["*1 nil outside repl" "nil" "*1"]
  ["*2 nil" "nil" "*2"]
  ["*3 nil" "nil" "*3"]
  ["*e nil" "nil" "*e"]
  ["*unchecked-math*" "false" "*unchecked-math*"]
  ["*in* bound" "true" "(map? *in*)"])

(defspec "untested / misc seqs + binding machinery"
  ["nfirst" "(quote (2))" "(nfirst [[1 2] [3]])"]
  ["xml-seq root" "1" "(count (xml-seq {:tag :a :content []}))"]
  ["xml-seq walks" "2" "(count (xml-seq {:tag :a :content [{:tag :b :content []}]}))"]
  # regression: comp with a keyword stage must use jolt IFn dispatch
  ["comp keyword stage" "(quote (1 2))" "((comp seq :content) {:content [1 2]})"]
  ["comp three stages"  "4" "((comp inc inc :n) {:n 2})"]
  ["random-sample all" "(quote (1 2))" "(random-sample 1.0 [1 2])"]
  ["random-sample none" "()" "(random-sample 0.0 [1 2])"]
  ["reader-conditional builds" "true" "(reader-conditional? (reader-conditional (quote (:clj 1)) false))"]
  ["->Eduction" "[2 3]" "(vec (->Eduction (map inc) [1 2]))"]
  ["bound-fn calls" "42" "((bound-fn [] 42))"]
  ["push/pop-thread-bindings" ":ok" "(do (push-thread-bindings {}) (pop-thread-bindings) :ok)"])
