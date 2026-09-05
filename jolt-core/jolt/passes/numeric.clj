(ns jolt.passes.numeric
  "Hint-directed numeric specialization. A local forward type-flow that seeds
  local kinds from `^double`/`^long` fn-param hints and float literals, propagates
  them through let inits, arithmetic results, and if/do, and tags an arithmetic
  `:invoke` node with `:num-kind :double` or `:long` when every operand is that
  kind (an integer literal is a wildcard, valid in either). The back end then emits
  Chez `fl*`/`fx*` ops instead of generic arithmetic.

  Soundness: `:long` is seeded from an explicit `^long` hint, OR — matching JVM loop
  semantics — from an integer-literal loop init that fits Chez's fixnum range, so a
  literal-init loop var is a primitive long whose fx/jolt-l ops raise on overflow
  rather than promoting to bignum (jolt previously promoted here, a divergence; this
  closes it modulo the documented 61-bit vs 64-bit width). Integer code outside a
  typed loop keeps jolt's arbitrary-precision numbers. `:double` is seeded from
  `^double` hints and float literals; flonum arithmetic is always flonum, so this
  matches the generic result. A `^long`/`fixnum-literal` seed is a promise the value
  is a fixnum: `fx+` raises on overflow rather than promoting, exactly as a JVM
  primitive long is fixed-width. A `:long` operand at a `:double`-specialized site is
  widened via `fixnum->flonum` (== JVM long->double widening), so mixed long×double
  contagion is value-neutral.

  Runs in every build and at `-e`/repl, but not the seed mint (which compiles with
  the passes off), so it stays out of the self-host fixpoint and benefits open and
  closed builds alike."
  (:require [jolt.ir :refer [map-ir-children]]))

;; java.lang.Math members that ALWAYS return a double on the JVM and have a native
;; Chez flonum op. When every operand is a proven flonum, the Math call lowers to
;; the flonum op (the back end reads :fl-op) instead of the generic string-keyed
;; host-static dispatch — and, crucially, the result types :double so it doesn't
;; break flonum contagion in the surrounding arithmetic. Chez flatan takes 1 or 2
;; args (2-arg = atan2); fllog takes 1 or 2. abs/floor/ceil are double-in→double-out
;; here because the branch only fires on a proven-double operand.
(def ^:private math-fl-ops
  {"sqrt" "flsqrt" "sin" "flsin" "cos" "flcos" "tan" "fltan"
   "asin" "flasin" "acos" "flacos" "atan" "flatan" "atan2" "flatan"
   "exp" "flexp" "log" "fllog" "floor" "flfloor" "ceil" "flceiling"
   "pow" "flexpt" "abs" "flabs"})

;; java.lang.Math members that are integer-in/integer-out on the JVM, as
;; [op arity] over the jolt-l-* fixnum macros (host/chez/seq.ss). Without this a
;; Math call on a proven :long took host-static-call — a string-keyed method
;; lookup per invocation — where the flonum side had been lowering to a native op
;; all along.
;;
;; The jolt-l-* helpers, not bare fx ops: a :long is promised to be within the
;; 64-bit range, which is WIDER than Chez's 61-bit fixnum, so a bare fxmin would
;; crash on a legitimate :long. Each one tests its operands and falls back to the
;; generic op, exactly as the arithmetic path does. floorMod reuses jolt-l-mod —
;; Math.floorMod is Scheme's modulo — and floorDiv gets jolt-floor-quotient, since
;; the quot above truncates toward zero where floorDiv floors.
;;
;; Only members the Math shim (host/chez/java/host-static-methods.ss) actually
;; carries. A member absent there must stay absent here too, or a type hint would
;; decide whether the call RESOLVES at all: Math/addExact is not shimmed, so
;; (Math/addExact 2 3) raises IllegalArgumentException — and lowering a hinted
;; (Math/addExact ^long a ^long b) to jolt-l+ would make the same call succeed
;; because a parameter carried a tag. Adding the Exact family is a shim change, not
;; an optimization; see the follow-up bead.
(def ^:private math-lng-ops
  {"abs" ["jolt-l-abs" 1]
   "min" ["jolt-l-min" 2] "max" ["jolt-l-max" 2]
   "floorDiv" ["jolt-floor-quotient" 2] "floorMod" ["jolt-l-mod" 2]})

;; Interop methods that answer a Chez FIXNUM when the receiver is proven (the
;; :target-type stamp jolt.passes.types/the analyzer puts on a :host-call). Both
;; the direct emit (string-direct-emit) and the generic jolt-string-method arm
;; return a fixnum for these — string-length, str-index-of-any, java-string-hash
;; (which is jolt-s32-narrowed), jolt-str-compare's -1/0/1, char->integer — so a
;; :long kind lets the surrounding arithmetic take the fx path.
;;
;; A method whose answer is NOT a fixnum is absent, and so is every method on an
;; unproven receiver: a `.length` on a record is a protocol method that can answer
;; anything. A LYING hint (a non-string at runtime) reaches record-method-dispatch
;; and its result meets an fx op, which is the same contract a wrong ^String hint
;; already has at the accessor.
(def ^:private str-fx-methods
  #{"length" "indexOf" "lastIndexOf" "hashCode" "compareTo" "compareToIgnoreCase"
    "codePointAt"})
(defn- interop-fx-kind [tt m]
  (when (or (and (= :str tt) (contains? str-fx-methods m))
            (and (= :kw tt) (= "hashCode" m)))
    :long))

;; Array kinds with a BOXED Chez vector backing — everything but double/float,
;; whose flvector the :fl-aget/:fl-aset path unboxes. :bytes reads but does not
;; write here (see the aset clause in an-invoke).
(def ^:private boxed-akinds #{:longs :ints :bytes :objects})
(def ^:private boxed-aset-kinds #{:longs :ints :objects})

;; --- operand classification -------------------------------------------------
(defn- int-lit? [n]
  (and (= :const (get n :op))
       (let [v (get n :val)] (and (number? v) (integer? v)))))
(defn- float-lit? [n]
  (and (= :const (get n :op))
       (let [v (get n :val)] (and (number? v) (float? v)))))

;; Chez's signed 61-bit fixnum range on a 64-bit host is asymmetric: [-2^60, 2^60-1]
;; (most-positive-fixnum = 2^60-1, most-negative-fixnum = -2^60). An integer literal
;; within it is a real fixnum, so a literal-init loop var seeded :long needs no entry
;; coercion (the literal IS a fixnum by this check). jolt targets Chez exclusively and
;; the width is fixed (see host/chez/hasheq.ss), so the bound is a compile-time const.
(def ^:private fixnum-max (dec (bit-shift-left 1 60)))
;; -(inc fixnum-max) = -2^60 — the true minimum. (NB: (- fixnum-max (inc fixnum-max))
;; is -1, which silently keeps every negative literal below -1 generic.)
(def ^:private fixnum-min (- (inc fixnum-max)))
(defn- fixnum-lit? [v] (and (integer? v) (<= fixnum-min v fixnum-max)))

;; A bigdec (1.5M) LITERAL operand — its node op, not a let-bound copy of one.
(defn- bigdec-lit? [n] (= :bigdec (get n :op)))
;; A bigdec literal promoted to a flonum const for double contagion: (+ 1.5M 2.0)
;; => 3.5 Double, the bigdec contributing its double value (BigDecimal.doubleValue).
;; The source is the bare numeric text (M stripped at read); double-rounding the
;; parsed value yields the nearest double — identical to the JVM conversion.
(defn- bigdec-lit->flonum [n] {:op :const :val (double (read-string (get n :source)))})

;; result kind of a double-specialized op at this name/arity, or nil if N/A.
;; arithmetic -> :double; comparison -> :bool (operands specialized, result not numeric).
;; Every op name dbl-spec / lng-spec returns non-nil for must have a Chez op in
;; jolt.backend-scheme/dbl-ops resp. lng-ops, or emit-numeric splices a nil op.
(defn- dbl-spec [nm n]
  (cond
    (and (>= n 1) (contains? #{"+" "-" "*" "/" "min" "max"
                               "unchecked-add" "unchecked-subtract" "unchecked-multiply"} nm)) :double
    (and (= n 1) (contains? #{"inc" "dec" "unchecked-inc" "unchecked-dec" "unchecked-negate"} nm)) :double
    (and (>= n 2) (contains? #{"<" ">" "<=" ">=" "=" "=="} nm)) :bool
    :else nil))

;; result kind of a long-specialized op, or nil. `/` is absent on purpose:
;; (/ long long) is a Ratio in Clojure, not a long. unchecked-* join the fast path
;; (they aren't native ops otherwise).
(defn- lng-spec [nm n]
  (cond
    (and (>= n 1) (contains? #{"+" "-" "*" "min" "max"
                               "unchecked-add" "unchecked-subtract" "unchecked-multiply"} nm)) :long
    (and (= n 1) (contains? #{"inc" "dec" "unchecked-inc" "unchecked-dec" "unchecked-negate"} nm)) :long
    (and (= n 2) (contains? #{"quot" "rem" "mod"} nm)) :long
    (and (>= n 2) (contains? #{"<" ">" "<=" ">=" "=" "=="} nm)) :bool
    :else nil))

;; result kind of a bigdec-specialized op, or nil. Arithmetic / quot / rem yield a
;; bigdec; the comparisons and zero?/pos?/neg? yield a bool. `=` is left to the
;; generic jolt= (already bigdec-aware), and `/` can throw (non-terminating) but is
;; still a bigdec op. Each non-nil name must have an entry in backend bd-ops.
(defn- bd-spec [nm n]
  (cond
    (and (>= n 1) (contains? #{"+" "-" "*" "/" "min" "max"} nm)) :bigdec
    (and (= n 2) (contains? #{"quot" "rem"} nm)) :bigdec
    (and (= n 1) (contains? #{"zero?" "pos?" "neg?"} nm)) :bool
    (and (>= n 2) (contains? #{"<" ">" "<=" ">="} nm)) :bool
    :else nil))

;; A non-numeric result (a comparison) doesn't propagate a numeric kind.
(defn- propagate [spec] (if (= spec :bool) nil spec))

(declare an)

;; The recur-arg kinds for the recurs targeting THIS loop level. recur only appears
;; in tail position (an if branch, a do's ret, a let body), so descend only those;
;; a nested loop/fn (and any non-tail child) owns its own recur and is skipped.
(defn- recur-kinds [node tenv]
  (let [op (get node :op)]
    (cond
      (= op :recur) [(mapv (fn [a] (nth (an a tenv) 0)) (get node :args))]
      (= op :let) (recur-kinds (get node :body)
                               (reduce (fn [te b] (assoc te (nth b 0) (nth (an (nth b 1) te) 0)))
                                       tenv (get node :bindings)))
      (= op :if) (concat (recur-kinds (get node :then) tenv) (recur-kinds (get node :else) tenv))
      (= op :do) (recur-kinds (get node :ret) tenv)
      :else [])))

;; Loop-var kinds by bounded fixpoint. A var keeps its seed kind (:double or :long)
;; only if every recur arg in that slot is the same kind (under the current
;; assumption) — a monotone demotion that stops at a fixpoint, bounded by the var
;; count. A :long seed now flows from an explicit ^long hint OR an integer-literal
;; init that fits the fixnum range (JVM loop semantics: a literal-init loop var is a
;; primitive long, so the fx/jolt-l ops raise on overflow rather than promoting — see
;; the ns docstring). A literal past the fixnum range, or a recur arg that doesn't
;; agree, demotes the slot back to generic (arbitrary precision). A typed loop var's
;; init and recur args are all flonums/fixnums, so no entry coercion is needed here,
;; unlike a fn param.
(defn- loop-kinds [names seed body tenv]
  (loop [cur seed iter 0]
    (if (> iter (count names))
      cur
      (let [te (reduce (fn [t i] (assoc t (nth names i) (nth cur i))) tenv (range (count names)))
            rks (recur-kinds body te)
            nxt (mapv (fn [j]
                        (let [k (nth cur j)]
                          (if (and k (every? (fn [rk] (= k (nth rk j))) rks)) k nil)))
                      (range (count names)))]
        (if (= nxt cur) cur (recur nxt (inc iter)))))))

;; Seed a fn arity's local env from its numeric param hints; an unhinted param
;; shadows any same-named outer local to nil.
(defn- arity-env [tenv a]
  (let [nh (into {} (get a :nhints))
        ah (into {} (get a :ahints))
        pe (reduce (fn [e p] (assoc e p (or (get nh p) (get ah p)))) tenv (get a :params))]
    (if (get a :rest) (assoc pe (get a :rest) nil) pe)))

(defn- an-invoke
  "Annotate an :invoke with its numeric kind. An arithmetic core op specializes to
  the Chez fl*/fx* op only when every operand is the same kind (:double or :long),
  except an integer literal is :wild — valid in either — so (+ ^double x 2) stays
  double. A call to a ^double/^long-returning var yields that kind without lowering
  the call (its body already coerces the return)."
  [node tenv]
  (let [fnode (get node :fn)
        nm (when (and (= :var (get fnode :op)) (= "clojure.core" (get fnode :ns)))
             (get fnode :name))
        math-static? (and (= :host-static (get fnode :op)) (= "Math" (get fnode :class)))
        math-op (when math-static? (get math-fl-ops (get fnode :member)))
        math-lng (when math-static? (get math-lng-ops (get fnode :member)))
        fnode' (nth (an fnode tenv) 1)
        ars (mapv (fn [a] (an a tenv)) (get node :args))
        argnodes (mapv (fn [r] (nth r 1)) ars)
        node1 (assoc node :fn fnode' :args argnodes)
        n (count ars)]
    (cond
      ;; a field read the structural inference proved is a flonum (a ^double record
      ;; field) is a :double operand — so (* (:x v) (:x v)) unboxes. The read itself
      ;; isn't lowered here; it keeps its keyword/jrec-field-at emit.
      (= :double (get node :num-read)) [:double node1]
      ;; a call to a var with a declared numeric return (^double/^long) yields that
      ;; kind, so an accumulator over the result types. The call itself isn't an
      ;; arithmetic op to lower — its body already coerces the return.
      (get fnode :num-ret) [(get fnode :num-ret) node1]
      ;; java.lang.Math over proven flonum operands -> native Chez flonum op, result
      ;; typed :double (so it doesn't de-opt the surrounding arithmetic). Requires at
      ;; least one genuine :double operand (so (Math/abs 5) keeps its int result) and
      ;; every other operand a :double, a :long (widened to flonum — JVM widens a long
      ;; arg to these double-returning overloads too), or an integer literal.
      (and math-op (pos? n)
           (some (fn [r] (= :double (nth r 0))) ars)
           (every? (fn [r] (let [k (nth r 0)]
                             (or (= k :double) (= k :long) (int-lit? (nth r 1))))) ars))
      (let [args' (mapv (fn [r] (let [k (nth r 0) nd (nth r 1)]
                                 (cond (int-lit? nd) (assoc nd :val (double (get nd :val)))
                                       (= k :long) (assoc nd :fl-coerce true)
                                       :else nd)))
                        ars)]
        [:double (assoc node1 :args args' :fl-op math-op)])
      ;; java.lang.Math over proven fixnum operands -> the jolt-l-* fixnum macro,
      ;; result typed :long so it doesn't de-opt the surrounding integer arithmetic.
      ;; Mirrors the flonum clause above: at least one genuine :long operand (so
      ;; (Math/abs 5) keeps its generic result) and every other operand a :long or a
      ;; fixnum-range integer literal. A bignum literal is excluded for the reason
      ;; the :long arithmetic path excludes it — the fx arm takes fixnums only.
      (and math-lng (= (nth math-lng 1) n)
           (some (fn [r] (= :long (nth r 0))) ars)
           (every? (fn [r] (let [k (nth r 0) nd (nth r 1)]
                             (or (= k :long)
                                 (and (int-lit? nd) (fixnum-lit? (get nd :val))))))
                   ars))
      [:long (assoc node1 :lng-op (nth math-lng 0))]
      ;; (aget ^doubles-array i) -> unboxed flvector read, result proven :double, so
      ;; the surrounding arithmetic unboxes to fl*/fl+. A PROVEN-:long (or fixnum-
      ;; literal) index is tagged :fl-idx-long so the back end emits (flvector-ref
      ;; (jolt-array-vec A) I) INLINE — keeping the value unboxed across the procedure
      ;; boundary instead of boxing at the jolt-flaget call. An unproven index keeps
      ;; (jolt-flaget A I), which owns the fixnum?/na-idx coercion.
      (and (= nm "aget") (= n 2) (= :doubles (nth (nth ars 0) 0)))
      (let [ikind (nth (nth ars 1) 0) inode (nth (nth ars 1) 1)]
        [:double (cond-> (assoc node1 :fl-aget true)
                   (or (= ikind :long)
                       (and (int-lit? inode) (fixnum-lit? (get inode :val))))
                   (assoc :fl-idx-long true))])
      ;; (aget ^longs/^ints/^bytes/^objects a i) -> the boxed-vector read
      ;; (jolt-vaget), skipping jolt-nth's index nil-check, coercion and
      ;; pvec/string/cseq/record dispatch walk. NO result kind: nothing narrows a
      ;; value entering an int/long array, so an element is not provably a fixnum.
      (and (= nm "aget") (= n 2) (contains? boxed-akinds (nth (nth ars 0) 0)))
      [nil (assoc node1 :v-aget true)]
      ;; (aset ^longs/^ints/^objects a i v) -> the boxed-vector write, returning the
      ;; stored value (JVM contract). ^bytes is absent on purpose: a byte array
      ;; narrows its elements to signed 8 bits at the store (na-elem-of), and that
      ;; narrowing lives on the generic path.
      (and (= nm "aset") (= n 3) (contains? boxed-aset-kinds (nth (nth ars 0) 0)))
      [nil (assoc node1 :v-aset true)]
      ;; (aset ^doubles-array i v) -> unboxed flvector-set!; returns the stored value
      ;; (:double), so an accumulator over the aset result types too. A proven-:long
      ;; (or fixnum-literal) index tags :fl-idx-long and a proven-:double value tags
      ;; :fl-val-double; when BOTH hold the back end emits (flvector-set! ...) inline
      ;; (returning the stored value — JVM contract). Otherwise it keeps (jolt-flaset
      ;; ...), which owns exact->inexact for an int / non-double value.
      (and (= nm "aset") (= n 3) (= :doubles (nth (nth ars 0) 0)))
      (let [ikind (nth (nth ars 1) 0) inode (nth (nth ars 1) 1)
            vkind (nth (nth ars 2) 0)]
        [:double (cond-> (assoc node1 :fl-aset true)
                   (or (= ikind :long)
                       (and (int-lit? inode) (fixnum-lit? (get inode :val))))
                   (assoc :fl-idx-long true)
                   (= vkind :double)
                   (assoc :fl-val-double true))])
      (nil? nm) [nil node1]
      :else
       (let [;; per-operand class: :double / :long / :bigdec (typed), :wild (a
             ;; fixnum-range integer literal, valid in any kind), :wild-big (a bignum
             ;; integer literal — valid for :double/:bigdec, but NOT :long: the fx ops
             ;; take only fixnums, so a bignum literal operand blocks :long), or :no.
             cls (mapv (fn [r] (let [k (nth r 0) nd (nth r 1)]
                                 (cond (= k :double) :double
                                       (= k :long) :long
                                       (= k :bigdec) :bigdec
                                       (int-lit? nd) (if (fixnum-lit? (get nd :val)) :wild :wild-big)
                                       :else :no)))
                       ars)
            ;; :long needs every operand a fixnum-range literal (:wild) or a :long
            ;; (the fx ops take only fixnums; a bignum literal would crash fx+).
            long-ok? (and (pos? n)
                          (some (fn [c] (= c :long)) cls)
                          (every? (fn [c] (or (= c :wild) (= c :long))) cls))
            ;; :bigdec tolerates any integer literal (:wild or :wild-big).
            bd-ok? (and (pos? n)
                        (some (fn [c] (= c :bigdec)) cls)
                        (every? (fn [c] (or (= c :wild) (= c :wild-big) (= c :bigdec))) cls))
            ds (dbl-spec nm n)
            ls (lng-spec nm n)
            bs (bd-spec nm n)]
        (cond
           ;; double specialization. Operands are :double, :long (a proven fixnum,
           ;; widened to flonum — JVM long->double widening == fixnum->flonum, so this
           ;; is value-neutral), :wild (an integer literal coerced to flonum), or a
           ;; bigdec LITERAL — double contagion: (+ 1.5M 2.0) => 3.5 Double, the bigdec
           ;; contributing its double value. A let-bound bigdec (kind :bigdec, not a
           ;; literal) can't be turned into a compile-time flonum, so it de-opts to the
           ;; generic bigdec-aware op. min/max return the ORIGINAL operand and `=` is
           ;; exactness-aware (0 != 0.0), so int/bigdec-literal/:long contagion is
           ;; blocked for those — every operand must be pure :double (a :long widened
           ;; to flonum would also change exactness there). fl< and friends compare
           ;; numerically, so coercing stays sound.
           (and ds (pos? n)
                (some (fn [c] (= c :double)) cls)
                (every? (fn [[c nd]] (or (= c :double) (= c :long) (= c :wild) (= c :wild-big)
                                         (and (= c :bigdec) (bigdec-lit? nd))))
                        (map vector cls argnodes))
                (or (not (contains? #{"min" "max" "="} nm))
                    (every? (fn [c] (= c :double)) cls)))
           ;; coerce integer-literal and bigdec-literal operands to a flonum const, and
           ;; tag a :long operand so the back end wraps it in (fixnum->flonum ...) — so
           ;; fl-ops only ever see flonums (an exact int, a fixnum, or a jbigdec record
           ;; would crash fl+). The :long contract guarantees a fixnum, so the coercion
           ;; is sound (the #3%fl per-site-unsafe proof obligation still holds).
           (let [args' (mapv (fn [[c nd]] (cond (int-lit? nd) (assoc nd :val (double (get nd :val)))
                                                (bigdec-lit? nd) (bigdec-lit->flonum nd)
                                                (= c :long) (assoc nd :fl-coerce true)
                                                :else nd))
                             (map vector cls argnodes))]
             [(propagate ds) (assoc node1 :args args' :num-kind :double)])
           (and ls long-ok?)
           [(propagate ls) (assoc node1 :num-kind :long)]
           ;; bigdec: every operand a bigdec (integer literals allowed, coerced at
           ;; runtime). A flonum operand blocks this (double contagion) and falls
           ;; through to the generic op.
           (and bs bd-ok?)
            [(propagate bs) (assoc node1 :num-kind :bigdec)]
          :else [nil node1])))))

;; Returns [kind node'] — kind is :double, :long, or nil.
(defn- an [node tenv]
  (let [op (get node :op)]
    (cond
      (= op :const) [(if (float-lit? node) :double nil) node]
      ;; a bigdec (M) literal seeds the :bigdec kind so call-position arithmetic
      ;; over it (and let-bound copies of it) dispatches to the bigdec engine.
      (= op :bigdec) [:bigdec node]
      (= op :local) [(get tenv (get node :name)) node]
      (= op :coerce) [(get node :kind) (assoc node :expr (nth (an (get node :expr) tenv) 1))]
      (= op :invoke) (an-invoke node tenv)
      (= op :let)
      (let [res (reduce (fn [acc b]
                          (let [te (nth acc 0) binds (nth acc 1)
                                ir (an (nth b 1) te)
                                ;; a ^doubles/… let binding (analyzer tagged its init
                                ;; :akind) seeds the array kind, overriding the init's
                                ;; own numeric kind — so (aget it i) in the body unboxes.
                                k (or (get (nth b 1) :akind) (nth ir 0))]
                            [(assoc te (nth b 0) k) (conj binds [(nth b 0) (nth ir 1)])]))
                        [tenv []] (get node :bindings))
            br (an (get node :body) (nth res 0))]
        [(nth br 0) (assoc node :bindings (nth res 1) :body (nth br 1))])
      (= op :loop)
      ;; inits evaluate in the OUTER env; loop vars get their fixpoint kinds for the body.
      (let [binds (get node :bindings)
            names (mapv (fn [b] (nth b 0)) binds)
            ik (mapv (fn [b]
                       (let [init (nth b 1) k (nth (an init tenv) 0)]
                         ;; seed each var with its init kind, OR :long for an
                         ;; integer-literal init that fits the fixnum range (JVM loop
                         ;; semantics — a literal-init loop var is a primitive long, so
                         ;; the fx/jolt-l ops raise on overflow rather than promoting).
                         ;; The bounded fixpoint below demotes a slot whose recur arg
                         ;; doesn't agree, and a literal past the range stays generic.
                         (or k (when (and (int-lit? init) (fixnum-lit? (get init :val))) :long))))
                     binds)
            ;; drive the bounded fixpoint: each seed kind survives only if every
            ;; recur arg in its slot agrees, else the slot demotes to generic.
            lk (loop-kinds names ik (get node :body) tenv)
            te (reduce (fn [t i] (assoc t (nth names i) (nth lk i))) tenv (range (count names)))]
        [nil (assoc node
                    :bindings (mapv (fn [b] [(nth b 0) (nth (an (nth b 1) tenv) 1)]) binds)
                    :body (nth (an (get node :body) te) 1))])
      (= op :if)
      (let [tr (an (get node :test) tenv)
            thn (an (get node :then) tenv)
            els (an (get node :else) tenv)
            tk (nth thn 0) ek (nth els 0)]
        [(if (= tk ek) tk nil)
         (assoc node :test (nth tr 1) :then (nth thn 1) :else (nth els 1))])
      (= op :do)
      (let [stmts (mapv (fn [s] (nth (an s tenv) 1)) (get node :statements))
            r (an (get node :ret) tenv)]
        [(nth r 0) (assoc node :statements stmts :ret (nth r 1))])
      (= op :fn)
      ;; bind the self-name too (a (fn f [] …) reference shadows any same-named
      ;; outer local), mirroring jolt.passes.types — else it inherits the outer
      ;; kind and arithmetic over the fn mis-specializes.
      (let [self (get node :name)]
        [nil (assoc node :arities
                    (mapv (fn [a]
                            (let [e (arity-env tenv a)
                                  e (if self (assoc e self nil) e)]
                              (assoc a :body (nth (an (get a :body) e) 1))))
                          (get node :arities)))])
      (= op :def) [nil (assoc node :init (nth (an (get node :init) tenv) 1))]
      ;; a proven-receiver interop call answering a fixnum is a :long operand, so
      ;; (+ (.length s) 1) lowers to the fx path rather than generic jolt-n+.
      (= op :host-call)
      [(interop-fx-kind (get node :target-type) (get node :method))
       (map-ir-children (fn [c] (nth (an c tenv) 1)) node)]
      ;; every other op introduces no bindings and isn't numeric: descend with the
      ;; same env to specialize nested arithmetic, no kind.
      :else [nil (map-ir-children (fn [c] (nth (an c tenv) 1)) node)])))

(defn annotate
  "Tag arithmetic nodes with :num-kind from local numeric type-flow. Returns the
  rewritten IR (no kind escapes to the caller)."
  [node]
  (nth (an node {}) 1))
