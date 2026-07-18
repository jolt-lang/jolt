(ns jolt.op-registry
  "The single source of truth for per-op facts about the clojure.core operations
  the compiler special-cases. One entry per op name; every fact table the back
  end and the passes need is DERIVED from it here, so a fact is edited in one
  place and the mirrors can't drift.

  A leaf namespace: it depends only on clojure.core, and both jolt.backend-scheme
  and the jolt.passes.* namespaces require it. It loads before them.

  Entry fields:
    :call    Scheme proc emitted for a CALL-position use (bare head). Its presence
             is what makes an op a `native-op` — the emitter lowers it inline, so
             the analyzer can resolve the name to an unbound-rooted clojure.core
             var (host-contract.ss declares them; manifest-check.sh pins that list
             to native-ops).
    :value   Scheme proc for a VALUE-position use ((map + xs)); defaults to :call.
    :dbl     flonum fast-path proc (^double contagion) — emit-numeric splices it.
    :lng     fixnum fast-path proc (^long). `/` has none (long/long is a Ratio).
    :bd      BigDecimal fast-path proc.
    :bool?   the emitted proc(s) return a genuine Scheme boolean, so an :if test
             built from one needs no jolt-truthy? wrapper.
    :arity   predicate on the call's arg count — a use of another arity is not
             lowered (falls back to a var call).
    :fixed   {arg-count -> specialized proc} for a hot fixed arity.

  Classifier facts (booleans) used by the type/inference/fold passes. Each was
  formerly a hand-maintained set in a separate namespace; the value in each
  comment names the mirror it replaces:
    :dbl-contagion?  arithmetic op that yields a flonum under double contagion
                     across fn boundaries (types/dbl-arith-ops). min/max are OUT:
                     they return an operand unchanged, so contagion would change
                     its type ((min 2.5 1) must stay 1). Comparisons are OUT
                     (bool result).
    :num-result?     result is provably a number (lattice/num-ret-fns).
    :num-args?       every argument must be a number (check/num-ops).
    :pure?           pure AND total — never throws on a legal input, so a fold may
                     duplicate or discard the call (inline/pure-fns). `/`/quot/rem/
                     mod throw on a zero divisor and are OUT; even?/odd? throw on a
                     non-integer and are OUT.
    :foldable?       a seed-tier pure numeric fn that constant-folds at compile
                     time (fold/foldable's key set). min/max/abs are OUT: they live
                     in a later core tier that isn't loaded when fold loads.")

(def op-registry
  ;; arithmetic
  {"+"   {:call "jolt-n+"    :value "jolt-add"   :dbl "fl+"  :lng "fx+"        :bd "jbd-add"
          :dbl-contagion? true :num-result? true :num-args? true :pure? true :foldable? true}
   "-"   {:call "jolt-n-"    :value "jolt-sub"   :dbl "fl-"  :lng "fx-"        :bd "jbd-sub"
          :dbl-contagion? true :num-result? true :num-args? true :pure? true :foldable? true}
   "*"   {:call "jolt-n*"    :value "jolt-mul"   :dbl "fl*"  :lng "fx*"        :bd "jbd-mul"
          :dbl-contagion? true :num-result? true :num-args? true :pure? true :foldable? true}
   "/"   {:call "jolt-n-div" :value "jolt-div"   :dbl "fl/"                   :bd "jbd-div"
          :dbl-contagion? true :num-result? true :num-args? true :foldable? true}
   ;; comparisons: vacuously true at arity 1 and don't inspect the arg, but
   ;; Scheme's < demands a number even there — cmp1-ops special-cases that.
   "<"   {:call "jolt-n<"   :value "jolt-lt" :bool? true
          :dbl "fl<?"  :lng "jolt-l<"  :bd "jbd-lt?" :pure? true :foldable? true}
   ">"   {:call "jolt-n>"   :value "jolt-gt" :bool? true
          :dbl "fl>?"  :lng "jolt-l>"  :bd "jbd-gt?" :pure? true :foldable? true}
   "<="  {:call "jolt-n<="  :value "jolt-le" :bool? true
          :dbl "fl<=?" :lng "jolt-l<=" :bd "jbd-le?" :pure? true :foldable? true}
   ">="  {:call "jolt-n>="  :value "jolt-ge" :bool? true
          :dbl "fl>=?" :lng "jolt-l>=" :bd "jbd-ge?" :pure? true :foldable? true}
   "="   {:call "jolt=" :bool? true :arity #(>= % 2)
          :fixed {2 "jolt=2"} :dbl "fl=?" :lng "jolt-l=" :pure? true :foldable? true}
   "=="  {:dbl "fl=?" :lng "jolt-l="}  ; numeric-only, not a native op
   "not=" {:pure? true}                ; not a native op; pure classifier only
   "inc" {:call "jolt-inc" :arity #(= % 1)
          :dbl-contagion? true :num-result? true :num-args? true :pure? true :foldable? true}
   "dec" {:call "jolt-dec" :arity #(= % 1)
          :dbl-contagion? true :num-result? true :num-args? true :pure? true :foldable? true}
   "not" {:call "jolt-not" :arity #(= % 1) :bool? true :pure? true}
   "min" {:call "jolt-n-min" :value "jolt-min"
          :dbl "flmin" :lng "jolt-l-min" :bd "jbd-min"
          :num-result? true :num-args? true :pure? true}
   "max" {:call "jolt-n-max" :value "jolt-max"
          :dbl "flmax" :lng "jolt-l-max" :bd "jbd-max"
          :num-result? true :num-args? true :pure? true}
   "abs" {:num-result? true :num-args? true :pure? true}  ; overlay fn, not a native op
   "mod"   {:call "jolt-mod"  :arity #(= % 2)
            :lng "jolt-l-mod"  :bd "jbd-mod"
            :num-result? true :num-args? true :foldable? true}
   "rem"   {:call "jolt-rem"  :arity #(= % 2)
            :lng "jolt-l-rem"  :bd "jbd-rem"
            :num-result? true :num-args? true :foldable? true}
   "quot"  {:call "jolt-quot" :arity #(= % 2)
            :lng "jolt-l-quot" :bd "jbd-quot"
            :num-result? true :num-args? true :foldable? true}
   "unchecked-add"      {:lng "jolt-uncadd2"}
   "unchecked-subtract" {:lng "jolt-uncsub2"}
   "unchecked-multiply" {:lng "jolt-uncmul2"}
   ;; collections
   "vector"      {:call "jolt-vector"}
   "hash-map"    {:call "jolt-hash-map-fn"}
   "hash-set"    {:call "jolt-hash-set"}
   "conj"        {:call "jolt-conj"    :arity #(>= % 1) :fixed {2 "jolt-conj2"}}
   "get"         {:call "jolt-get"     :arity #(or (= % 2) (= % 3)) :pure? true}
   "nth"         {:call "jolt-nth"     :arity #(or (= % 2) (= % 3))}
   "count"       {:call "jolt-count"   :arity #(= % 1) :num-result? true}
   "assoc"       {:call "jolt-assoc"   :arity #(and (>= % 3) (odd? %))
                  :fixed {3 "jolt-assoc3"}}
   "dissoc"      {:call "jolt-dissoc"  :arity #(>= % 1) :fixed {2 "jolt-dissoc2"}}
   "contains?"   {:call "jolt-contains?" :arity #(= % 2) :bool? true}
   "empty?"      {:call "jolt-empty?"   :arity #(= % 1) :bool? true}
   "peek"        {:call "jolt-peek"    :arity #(= % 1)}
   "pop"         {:call "jolt-pop"     :arity #(= % 1)}
   ;; seq
   "first"   {:call "jolt-first"   :arity #(= % 1)}
   "rest"    {:call "jolt-rest"    :arity #(= % 1)}
   "next"    {:call "jolt-next"    :arity #(= % 1)}
   "seq"     {:call "jolt-seq"     :arity #(= % 1)}
   "cons"    {:call "jolt-cons"    :arity #(= % 2)}
   "list"    {:call "jolt-list"}
   "reverse" {:call "jolt-reverse" :arity #(= % 1)}
   "last"    {:call "jolt-last"    :arity #(= % 1)}
   "map"     {:call "jolt-map"     :arity #(>= % 2)}
   "filter"  {:call "jolt-filter"  :arity #(= % 2)}
   "remove"  {:call "jolt-remove"  :arity #(= % 2)}
   "reduce"  {:call "jolt-reduce"  :arity #(or (= % 2) (= % 3))}
   "into"    {:call "jolt-into"    :arity #(= % 2)}
   "concat"  {:call "jolt-concat"}
   "apply"   {:call "jolt-apply"   :arity #(>= % 2)}
   "range"   {:call "jolt-range"   :arity #(and (>= % 0) (<= % 3))}
   "take"    {:call "jolt-take"    :arity #(= % 2)}
   "drop"    {:call "jolt-drop"    :arity #(= % 2)}
   "keys"    {:call "jolt-keys"    :arity #(= % 1)}
   "vals"    {:call "jolt-vals"    :arity #(= % 1)}
   ;; predicates
   "even?"    {:call "jolt-even?"  :arity #(= % 1) :bool? true}
   "odd?"     {:call "jolt-odd?"   :arity #(= % 1) :bool? true}
   "pos?"     {:call "jolt-pos?"   :arity #(= % 1) :bool? true :bd "jbd-pos?" :pure? true}
   "neg?"     {:call "jolt-neg?"   :arity #(= % 1) :bool? true :bd "jbd-neg?" :pure? true}
   "zero?"    {:call "jolt-zero?"  :arity #(= % 1) :bool? true :bd "jbd-zero?" :pure? true}
   "identity" {:call "jolt-identity" :arity #(= % 1)}
   "nil?"     {:call "jolt-nil?"   :arity #(= % 1) :bool? true :pure? true}
   "some?"    {:call "jolt-some?"  :arity #(= % 1) :bool? true :pure? true}
   "ex-info"  {:call "jolt-ex-info" :arity #(or (= % 2) (= % 3))}
   ;; bit ops: and/or/xor/not are Chez bitwise primitives (inlined to native
   ;; code, no helper call); operands must be integers (a non-integer errors,
   ;; like the JVM). The shifts keep their helpers (Java >>> masking /
   ;; arithmetic shift) but emit a direct call instead of var-deref + the
   ;; variadic overlay. and/or/xor get strict min-2 twins as their VALUE (the raw
   ;; Chez prims accept arity 0/1, diverging from the JVM); bit-and-not is left to its overlay: its only Scheme impl is
   ;; 2-arg, so a value-position arity-3 use (via the variadic overlay) would
   ;; mis-emit.
   "bit-and"                 {:call "bitwise-and"  :value "jolt-bit-and*" :arity #(= % 2)
                              :num-result? true :num-args? true :pure? true :foldable? true}
   "bit-or"                  {:call "bitwise-ior"  :value "jolt-bit-or*"  :arity #(= % 2)
                              :num-result? true :num-args? true :pure? true :foldable? true}
   "bit-xor"                 {:call "bitwise-xor"  :value "jolt-bit-xor*" :arity #(= % 2)
                              :num-result? true :num-args? true :pure? true :foldable? true}
   "bit-not"                 {:call "bitwise-not"  :arity #(= % 1) :num-args? true}
   "bit-shift-left"          {:call "jolt-bit-shift-left"          :arity #(= % 2) :num-args? true}
   "bit-shift-right"         {:call "jolt-bit-shift-right"         :arity #(= % 2) :num-args? true}
   "unsigned-bit-shift-right" {:call "jolt-unsigned-bit-shift-right" :arity #(= % 2)}
   ;; positional protocol-method dispatch (defprotocol-emitted shims) — bind
   ;; directly to the records.ss entry points so a protocol call doesn't
   ;; var-deref. Emitted bare (call name == op name).
   "protocol-dispatch1" {:call "protocol-dispatch1" :arity #(= % 3)}
   "protocol-dispatch2" {:call "protocol-dispatch2" :arity #(= % 4)}
   "protocol-dispatch3" {:call "protocol-dispatch3" :arity #(= % 5)}})

;; --- derived accessor tables (edit per-op facts above, not here) -------------
(def native-ops
  (into {} (keep (fn [[op spec]] (when (:call spec) [op (:call spec)]))) op-registry))
(def core-value-procs
  (into {} (keep (fn [[op spec]]
                   (when (:call spec) [op (or (:value spec) (:call spec))]))
                 op-registry)))
;; Native-op Scheme procedures that return a genuine Scheme boolean, so an :if
;; test built from them needs no jolt-truthy? wrapper. Covers every proc a :bool?
;; op can emit — its :call name plus each :fixed helper — so jolt=2 is covered.
(def bool-returning-ops
  (into #{}
        (for [[_ spec] op-registry
              :when (:bool? spec)
              proc (cons (:call spec) (vals (:fixed spec)))
              :when proc]
          proc)))
(def op-arity
  (into {} (keep (fn [[op spec]] (when (:arity spec) [op (:arity spec)]))) op-registry))
(def fixed-arity-ops
  (into {} (keep (fn [[op spec]] (when (:fixed spec) [op (:fixed spec)]))) op-registry))
(def dbl-ops
  (into {} (keep (fn [[op spec]] (when (:dbl spec) [op (:dbl spec)]))) op-registry))
(def lng-ops
  (into {} (keep (fn [[op spec]] (when (:lng spec) [op (:lng spec)]))) op-registry))
(def bd-ops
  (into {} (keep (fn [[op spec]] (when (:bd spec) [op (:bd spec)]))) op-registry))

;; jolt's comparison ops are vacuously true at arity 1 and DON'T inspect the arg
;; (< > <= >=: a :bool? :dbl op with no fixed :arity — that excludes =, and the
;; predicates, which lack :dbl); Scheme's < demands a number even at arity 1.
(def cmp1-ops
  (into #{} (keep (fn [[_ spec]]
                    (when (and (:bool? spec) (:dbl spec) (not (:arity spec)))
                      (:call spec)))
                  op-registry)))

;; Every bare Scheme identifier the registry drives the back end to emit — each
;; :call/:dbl/:lng/:value/:bd name plus each :fixed helper. The back end unions
;; this with its own closed set of runtime helpers for the local-shadow guard.
(def registry-emitted-names
  (into #{}
        (comp (mapcat (fn [[_ spec]]
                        (keep identity
                              (concat [(:call spec) (:dbl spec) (:lng spec)
                                       (:value spec) (:bd spec)]
                                      (vals (:fixed spec))))))
              (remove nil?))
        op-registry))

;; --- classifier op-name sets (each replaces a hand-list in a pass) -----------
(defn- keys-with [flag] (into #{} (keep (fn [[op spec]] (when (flag spec) op))) op-registry))

;; types/dbl-arith-ops: arithmetic ops that yield a flonum under double contagion.
(def dbl-arith-ops (keys-with :dbl-contagion?))
;; lattice/num-ret-fns: ops whose result is provably a number.
(def num-result-ops (keys-with :num-result?))
;; check/num-ops: ops that require every argument to be a number.
(def num-arg-ops (keys-with :num-args?))
;; inline/pure-fns: pure AND total ops (a fold may duplicate or discard them).
(def pure-ops (keys-with :pure?))
;; fold/foldable's key set: seed-tier pure numeric fns that constant-fold.
(def foldable-ops (keys-with :foldable?))
