# Specification: ad-hoc hierarchies (make-hierarchy/derive/underive/isa?/
# parents/ancestors/descendants) — ported to pure Clojure (stage 3). The
# 3-arity forms are PURE (derive returns a new hierarchy); the 1/2-arity forms
# use the global hierarchy. Multi-parent derive, transitive ancestors AND
# descendants, and vector-pair isa? match Clojure (the old Janet kernel had
# single-parent :parents and direct-only descendants).
(use ../support/harness)

(defspec "hierarchy / pure 3-arity"
  ["derive returns new h" "true"
   "(let [h (derive (make-hierarchy) :rect :shape)] (and (map? h) (isa? h :rect :shape)))"]
  ["original unchanged"   "false"
   "(let [h0 (make-hierarchy) h1 (derive h0 :rect :shape)] (isa? h0 :rect :shape))"]
  ["isa? self"            "true"  "(isa? (make-hierarchy) :a :a)"]
  ["isa? transitive"      "true"
   "(let [h (-> (make-hierarchy) (derive :square :rect) (derive :rect :shape))] (isa? h :square :shape))"]
  ["multi-parent"         "[true true]"
   "(let [h (-> (make-hierarchy) (derive :sq :rect) (derive :sq :rhombus))] [(isa? h :sq :rect) (isa? h :sq :rhombus)])"]
  ["parents set"          "true"
   "(let [h (-> (make-hierarchy) (derive :sq :rect) (derive :sq :rhombus))] (= #{:rect :rhombus} (parents h :sq)))"]
  ["ancestors transitive" "true"
   "(let [h (-> (make-hierarchy) (derive :square :rect) (derive :rect :shape))] (= #{:rect :shape} (ancestors h :square)))"]
  ["descendants transitive" "true"
   "(let [h (-> (make-hierarchy) (derive :square :rect) (derive :rect :shape))] (= #{:rect :square} (descendants h :shape)))"]
  ["underive removes"     "false"
   "(let [h (-> (make-hierarchy) (derive :a :b) (underive :a :b))] (isa? h :a :b))"]
  ["vector isa?"          "true"
   "(let [h (-> (make-hierarchy) (derive :rect :shape))] (isa? h [:rect :rect] [:shape :shape]))"]
  ["vector isa? length"   "false"
   "(isa? (make-hierarchy) [:a] [:a :a])"]
  ["cyclic derive throws" :throws
   "(-> (make-hierarchy) (derive :a :b) (derive :b :a))"]
  ["duplicate derive ok"  "true"
   "(let [h (-> (make-hierarchy) (derive :a :b) (derive :a :b))] (isa? h :a :b))"]
  ["parents nil when none" "nil" "(parents (make-hierarchy) :x)"])

(defspec "hierarchy / global + multimethod dispatch"
  ["global derive + isa?" "true" "(do (derive :gsq :grect) (isa? :gsq :grect))"]
  ["global ancestors"     "true"
   "(do (derive :ga :gb) (derive :gb :gc) (contains? (ancestors :ga) :gc))"]
  ["global underive"      "false"
   "(do (derive :gu :gv) (underive :gu :gv) (isa? :gu :gv))"]
  ["dispatch via hierarchy" ":is-shape"
   "(do (derive :hsq :hshape) (defmulti hmm identity) (defmethod hmm :hshape [_] :is-shape) (hmm :hsq))"]
  ["dispatch custom hierarchy" ":parent"
   "(do (def hh (atom (derive (make-hierarchy) :c :p))) (defmulti cmm identity :hierarchy hh) (defmethod cmm :p [_] :parent) (cmm :c))"]
  ["dispatch exact beats isa" ":exact"
   "(do (derive :de1 :de2) (defmulti emm identity) (defmethod emm :de2 [_] :parent) (defmethod emm :de1 [_] :exact) (emm :de1))"])
