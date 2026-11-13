(ns jolt.backend-scheme.hoist
  "Per-def scopes the Scheme back end hoists into: the lazy cache cells a site
  resolves into once, and the constant pool a pure construction (a keyword, a
  constant literal, a var cell, a record descriptor) is bound in once per def
  rather than rebuilt per evaluation. Also which top-level expressions get such
  a scope at all."
  (:require [clojure.string :as str]
            [jolt.backend-scheme.names :refer [chez-str-lit]]
            [jolt.backend-scheme.session :refer [fresh-label var-cache?]]))

;; Per-site cache cells collected while emitting one top-level def. A site that
;; resolves a STABLE value — a devirtualized impl (constant tag/proto/method) or a
;; var cell (interned, so the cell never changes even when the var is redefined) —
;; resolves it once, not per call, the inline cache the JVM gets for free. When a
;; def init is being emitted this holds an atom; each site appends a fresh cell name
;; (bound to #f in a let wrapping the def, so it persists across calls and is shared
;; by every invocation) and resolves into it on first use. nil outside a def (a site
;; there falls back to a per-call resolve).
;;
;; THREAD-LOCAL, like every other piece of emit-session scratch in this file
;; (*fnsrc-counter*, *callsites*, *tail?*, …) and like the reference compiler, which
;; keeps its whole compilation state in thread-bound Vars. These two lived on the
;; unit — one object for the whole process — and emit-with-cells swapped them in and
;; out around each def. That save/restore is a read-modify-write on shared state, so
;; two threads compiling at once traded collectors: one thread's hoisted constant was
;; registered into the other's pool, the `let` that should have bound it never saw
;; it, and a namespace that compiled cleanly alone died with "variable _kc$81 is not
;; bound". Rebinding also unwinds on a throw, which the reset! pair did not — a
;; failed emit used to leave the unit pointing at the abandoned def's collector, and
;; every later def in the process appended cells to it.
;;
;; The gensym counter deliberately stays shared on the unit: swap! is atomic, and one
;; counter per process is what keeps a registered anon-fn name globally unique.
(def ^:dynamic *cache-cells* nil)

(def ^:dynamic *const-pool* nil)

;; Emit a def's init (via the supplied thunk) under a fresh cache-cell collector,
;; then wrap the result in a let binding any cells its body registered so they
;; persist in the def's closure. The binding nests for nested defs and unwinds on a
;; throw. Used by both the runtime def emit and the direct-link top-level emit.
;; Hoist a PURE CONSTANT construction out of its use site: the site emits a bare
;; variable read, and the def's wrapper binds it once. The pool is keyed by the
;; emitted expression, so a constant appearing at ten sites in one def is built
;; once and shared.
;;
;; Unlike the lazy cache cells above these bind EAGERLY — a constant has no
;; resolution to defer, so there is nothing for an (or cell …) branch to save and
;; a plain variable read is cheaper at every use.
;;
;; Only for constructions that are pure AND whose result is safe to share.
;; Keyword literals qualify on both counts: `keyword` interns, so it already
;; returns the same object for the same name and sharing changes nothing
;; observable. Symbols deliberately do NOT route through here — jolt-symbol
;; allocates a fresh symbol per call and symbols carry metadata, so sharing one
;; across sites could let meta leak between them.
;;
;; Outside a def (no pool) the raw expression is returned unchanged.
;; The pool value is [name insertion-index]. The index is what orders the emitted
;; bindings, and it has to be insertion order rather than binding NAME, because a
;; hoisted constant may now REFERENCE another one: a map literal is hoisted as
;; (jolt-hash-map _kc$1 1 …) over its already-hoisted keyword constants. A child is
;; always interned before its parent (the inner emit returns first), so insertion
;; order is a topological order. Sorting by name is not — fresh-label counts, so
;; "_kc$10" sorts before "_kc$2" — and it only worked while keywords, which
;; reference nothing, were the sole hoisted constant.
(defn hoist-const [expr]
  (let [pool *const-pool*]
    (if pool
      (or (first (get @pool expr))
          (let [nm (fresh-label "_kc$")]
            (swap! pool assoc expr [nm (count @pool) expr])
            nm))
      expr)))

;; Hoist a constant construction WITHOUT sharing it between sites: each call gets
;; its own binding, keyed by a token no other call can produce.
;;
;; Interning is wrong for a collection, and the reference implementation says so.
;; Each literal site there is its own constant-pool entry, so evaluating ONE site
;; twice yields the identical object while two distinct sites yield two objects —
;; verified: (defn f [] [\a ##NaN]) has (= (f) (f)) true, and
;; (let [a [\a ##NaN] b [\a ##NaN]] (= a b)) false. That difference is observable
;; wherever = short-circuits on identity but the contents are not equal to
;; themselves, which is exactly a collection holding ##NaN. Interning by emitted
;; text made those two sites one object and turned the second answer true; it is
;; what clojure.core-test/not-eq's `[\a ##NaN] [\a ##NaN]` row catches.
;;
;; Keywords keep using the interning hoist-const above: they are interned already,
;; so sharing one across sites changes nothing observable.
(defn hoist-const-per-site [expr]
  (let [pool *const-pool*]
    (if pool
      (let [nm (fresh-label "_kc$")]
        (swap! pool assoc [::site nm] [nm (count @pool) expr])
        nm)
      expr)))

;; ...and the same hoist keyed by the SOURCE OBJECT, which is what the reference
;; compiler does: Compiler.registerConstant keys its pool on an IdentityHashMap,
;; so one form object is one constant however many times it appears in the code
;; being compiled, while two forms that merely READ alike stay two constants.
;;
;; Both halves matter and per-site had only the second. A macro that mentions its
;; argument more than once — clojure.test/is names the tested form five times,
;; across :form, :expected and three :actual arms — splices the SAME object into
;; every one, and per-site emitted a fresh construction for each. Measured on one
;; deftest holding 800 (is (= n n)): 6410 hoisted bindings of which 812 were
;; distinct, 87% redundant. They all land in one let*, and Chez's compile is
;; quadratic in that, so the waste is squared rather than added.
;;
;; The distinction per-site exists to protect is preserved exactly, because it is
;; a distinction between OBJECTS: (defn f [] [\a ##NaN]) and a second literal
;; written elsewhere read as two forms, so they stay two constants and
;; (= (f) (f)) keeps answering true while two separate sites answer false. That
;; is clojure.core-test/not-eq's row, and it passes on identity keying for the
;; same reason it passes on the JVM.
;;
;; Keyed through jolt.host/identity-hash with an identical? check rather than a
;; map with identity semantics: java.util.IdentityHashMap is value-keyed here (a
;; recorded divergence), and a jolt seq does not cache its hash, so an
;; equality-keyed map would walk the subtree this exists to avoid walking.
;;
;; The index is its OWN atom and not another key shape in the pool: the pool is
;; flushed to let* bindings by position, so anything else living there would have
;; to be filtered back out at that seam.
(def ^:dynamic *const-ids* nil)

(defn hoist-const-for [obj expr]
  (let [pool *const-pool* ids *const-ids*]
    (if (or (nil? pool) (nil? ids) (nil? obj))
      (hoist-const-per-site expr)
      (let [k (jolt.host/identity-hash obj)]
        (or (first (keep (fn [r] (when (identical? (nth r 0) obj) (nth r 1)))
                         (get @ids k)))
            (let [nm (hoist-const-per-site expr)]
              (swap! ids update k (fnil conj []) [obj nm])
              nm))))))

;; Is this literal a CONSTANT construction — one whose value is fully determined at
;; emit time, so building it once per def and sharing it is indistinguishable from
;; building it per evaluation? True for a scalar :const and for a collection literal
;; all of whose elements are themselves constant, recursively.
;;
;; Sharing is sound because these are persistent values. Nothing mutates one in
;; place: with-meta returns a new collection, and a transient over a hoisted map
;; does not touch the source (enode-claim converts a persistent hnode into a fresh
;; editable node rather than writing through it — transients.ss).
;;
;; A :const only ever holds a scalar (emit-const rejects anything else), so this
;; never sweeps up a symbol — which hoist-const deliberately refuses, since symbols
;; carry metadata and sharing one across sites would let meta leak between them.
;;
;; A :quote node qualifies too: its :form is raw reader data, fully determined at
;; emit time, so a literal HOLDING one (#{:for 'for}, [:a 'b]) is as constant as
;; one holding only scalars. The hoist these arms use is PER-SITE
;; (hoist-const-per-site), so accepting quoted symbols cannot leak meta across
;; sites — each site keeps its own object, which is the reference compiler's
;; behavior (a quoted form is a ConstantExpr in the class constant pool; one site
;; evaluated twice yields the identical object).
;; A literal the inline pass stamped with a site id (jolt.passes
;; stamp-site-consts) comes from the process-wide table, so every spliced copy
;; and the fn itself hand back one object; the hoist makes it one lookup per def
;; load. Unstamped, the construction is the site's own.
(defn site-const [node s]
  (if-let [id (:site-id node)]
    (str "(jolt-site-const " (chez-str-lit id) " (lambda () " s "))")
    s))

(defn const-coll-node? [n]
  (and (map? n)
       (case (:op n)
         :const true
         :quote true
         ;; a #"…" literal is a constant object too (the reader hands the
         ;; reference compiler a Pattern, which lands in the constant pool), so
         ;; a collection holding one is as constant as one holding scalars
         :regex true
         :vector (every? const-coll-node? (:items n))
         :set (every? const-coll-node? (:items n))
         :map (every? const-coll-node? (apply concat (:pairs n)))
         false)))

;; A :map literal whose keys are all constant keywords, pairwise distinct
;; (the reader refuses a repeat; checked anyway, since the direct build below
;; skips the runtime's scan), and few enough to stay in array mode whatever the
;; values are: collections.ss pam-literal-kvs? keeps an all-keyword literal in
;; array mode up to array-map-limit-kw (64) pairs.
(def ^:private array-map-limit-kw 64)
(defn unique-keyword-key-literal? [node]
  (let [ks (map first (:pairs node))]
    (and (seq ks)
         (<= (count ks) array-map-limit-kw)
         (every? (fn [k] (and (= :const (:op k)) (keyword? (:val k)))) ks)
         (apply distinct? (map :val ks)))))

;; Literal keys (or set elements) that cannot collide: at most one, or every one
;; a :const and no two equal, which the reader has already checked. Any other literal is built by the checked
;; constructor, which throws IllegalArgumentException when keys computed at run
;; time collide, as the JVM's createWithCheck does.
(defn distinct-const-keys? [ks]
  (or (empty? (rest ks))
      (and (every? (fn [k] (= :const (:op k))) ks)
           (apply distinct? (map :val ks)))))

;; The pool's bindings as flat `let` layers by dependency depth: layer 0 holds the
;; constants that reference no other constant, layer k those whose deepest
;; reference is in layer k-1. A pool is mostly keywords, symbols and var cells
;; that reference nothing, so this is two or three layers.
;;
;; It was one let*, which Chez compiles as one nested scope per binding, and that
;; is quadratic in the pool: 1600 bindings took 56 ms where the same bindings in a
;; flat let took 10, 3200 took 230 against 36. A bare top-level fn has had a pool
;; since the keyword re-intern fix, so a (fn [] …) holding 400 (is …) forms went
;; from 850 ms to 1.4 s of Chez compile on output that had got SMALLER, and
;; compilescaling's 1x->4x ratio climbed from 4.3 to 7 on CI. A reference to a
;; label inside a string literal can only put an entry a layer too deep, which is
;; still bound before everything that reads it.
;;
;; The labels are found by a string scan, not a regex: the compiler runs in
;; profiles built without the regex group (gambitprofile).
(defn- pool-label-refs [expr]
  (let [n (count expr)]
    (loop [from 0 acc []]
      (if-let [i (str/index-of expr "_kc$" from)]
        (let [j (loop [j (+ i 4)]
                  (if (and (< j n) (let [c (nth expr j)] (and (>= (int c) 48) (<= (int c) 57))))
                    (recur (inc j))
                    j))]
          (recur j (if (> j (+ i 4)) (conj acc (subs expr i j)) acc)))
        acc))))

(defn- pool-layers [consts]
  (let [depth (volatile! {})]
    (reduce (fn [layers [nm _ expr]]
              (let [d (reduce (fn [d ref] (if-let [rd (get @depth ref)] (max d (inc rd)) d))
                              0
                              (pool-label-refs expr))]
                (vswap! depth assoc nm d)
                (update layers d (fnil conj []) (str "(" nm " " expr ")"))))
            []
            consts)))

(defn- emit-with-scope [cells? emit-thunk]
  (let [cells (when cells? (atom []))
        pool (atom {})
        ids (atom {})
        raw (binding [*cache-cells* cells
                      *const-pool* pool
                      *const-ids* ids]
              (emit-thunk))
        ;; constants bind eagerly (value first); lazy cache cells start #f. Walked
        ;; in INSERTION order, which is topological: a constant that references an
        ;; earlier one (a hoisted collection literal over its hoisted keywords) was
        ;; interned after it; see hoist-const. Deterministic for a given emit,
        ;; which is what the seed fixpoint needs.
        consts (map val (sort-by (comp second val) @pool))
        layers (pool-layers consts)
        layers (if (and cells (seq @cells))
                 (update layers 0 (fnil into []) (map (fn [c] (str "(" c " #f)")) @cells))
                 layers)]
    (if (seq layers)
      (reduce (fn [inner layer] (str "(let (" (str/join " " layer) ") " inner ")"))
              raw
              (rseq layers))
      raw)))
(defn emit-with-cells [emit-thunk] (emit-with-scope true emit-thunk))

;; A cache-cell scope for a top-level EXPRESSION (jolt-g3u). Without it the
;; collector exists only inside a def, so every var / protocol / ctor site in a
;; bare top-level form resolves PER ACCESS. Measured: a var-ref call at such a
;; site costs 101-125 ns against 28 ns at a cached site and 11 ns through a local,
;; and 3M calls do 3M var-table reads against 2 at a cached site. A bare top-level
;; (loop …) went 120.8 -> 25.1 ns/op with this.
;;
;; TWO restrictions, both of which cost real measurements to find.
;;
;; Not a def. A direct-link def emits a bare (define …), which cannot sit inside a
;; let, and both :def paths already wrap their own INIT — the part that repeats.
;;
;; And only when the form contains a LOOP (or a self-recurring fn), not merely a
;; :fn. That looks over-cautious and is not. A cell has to be paid for: the emitted
;; (var-cell-deref (or c …)) is several times the text of a (var-deref …), and a
;; top-level form is compiled by Chez every time it is loaded. So the cell wins
;; only if its site actually runs again, and a loop is the one thing that proves
;; it will — a top-level fn may never be called. Wrapping on :fn too cost 14% on
;; `require clojure.test` (0.44 -> 0.50s), verified by switching the wrapper at
;; runtime inside ONE binary so no build variance was involved, and bought that
;; path nothing: a namespace being loaded is defs and defmethods whose bodies run
;; once at load. Confirmed it was the cells and not the const pool by trying a
;; cells-only variant, which was equally slow.
;;
;; Gated on var-cache? for the same reason the :var site is: the seed mint runs
;; with it OFF and must stay a byte-fixpoint, and this wrapper would otherwise
;; change mint output through the const pool (which no other flag gates) even
;; though no cache cell would be registered.
(def ^:private repeat-ops #{:loop :recur})

;; Keys whose value is PAYLOAD hanging off a node rather than IR under it. They
;; have to be skipped, and :src-form is not an optimization — it is the difference
;; between linear and quadratic. Every :fn node carries its whole source form, so a
;; fn literal written inside another is in both, and walking them all costs the sum
;; of the subtree sizes. 240 nested literals spent 89 ms of a 90 ms emit right here,
;; and it showed up as 633k reads of clojure.core/map?, sequential?, vector? and
;; seq? — this walk's own operations, called O(source size) times per fn.
;;
;; Skipping it cannot lose a loop. :src-form is the SOURCE the image rebuilds a
;; closure from; a loop written in it is already in the IR under :arities, which is
;; walked. :free-names is a vector of name strings and holds no nodes at all.
;; Checked rather than asserted: over 247 top-level forms from eight real files the
;; two walks never disagreed.
;;
;; They CAN disagree in one direction, and it is the harmless one. A source form
;; carrying a map literal that happens to look like IR — (fn [] {:op :loop}) — used
;; to answer "repeats" off the DATA, and now does not. The wrapper this drives is an
;; optimization, so the old answer only bought that form some cache cells it could
;; not use; nothing reads the cells that is not also emitted by the same pass. A
;; quoted {:op :recur} still answers true, through the :quote node rather than
;; through :src-form — pre-existing, equally harmless, and left alone.
(def ^:private node-payload-keys #{:src-form :free-names :live-names})

(defn- node-tree-any?
  "Does any node in this tree satisfy pred? Walks map values and sequential
  children, which covers every shape the analyzer builds — minus the payload keys
  above, which hold source and names rather than IR."
  [pred x]
  (cond
    (and (map? x) (:op x) (pred x)) true
    ;; a :const holds a literal, which cannot contain a loop and can be arbitrarily
    ;; large (a 10k-element vector literal at top level). Stop rather than walk it.
    (and (map? x) (= :const (:op x))) false
    (map? x) (boolean (some (fn [e] (and (not (contains? node-payload-keys (key e)))
                                         (node-tree-any? pred (val e))))
                            x))
    (sequential? x) (boolean (some (fn [v] (node-tree-any? pred v)) x))
    :else false))

;; Does this top-level form contain something whose body PROVABLY runs more than
;; once? A loop (or a self-recurring fn) does. A plain :fn does not — it may never
;; be called, and that distinction is the whole design; see emit-top-cells.
(defn- top-form-repeats? [node]
  (node-tree-any? (fn [n] (contains? repeat-ops (:op n))) node))

(defn- top-form-has-fn? [node]
  (node-tree-any? (fn [n] (= :fn (:op n))) node))

;; A fn literal in a bare top-level form gets the CONSTANT pool, not the cells.
;; Such a form is how every deftype/defrecord method, every extend-type and
;; extend-protocol impl, every defmethod body and every reify-in-a-registration
;; reaches the runtime — code that runs on every call of the method — and without
;; a pool each keyword literal in it re-interned per evaluation: a deftype method
;; binds its fields with (__deftype-field this :f), so a type of eight fields paid
;; eight intern lookups (a string hash each) on every method entry. core.logic's
;; Substitutions is that type, and the lookups were a fifth of a propagation run.
;; The pool is cheap where the cells are not: a hoisted keyword is one binding and
;; one variable read in place of the (keyword …) call text, so the form Chez
;; compiles barely grows — the 14% load cost measured above was the cells.
(defn emit-top-cells [node emit-thunk]
  (cond
    ;; never a def: the :def cases already wrap their INIT, which is the part
    ;; that repeats. A def's remaining pieces run once.
    (or (not (var-cache?)) (= :def (:op node))) (emit-thunk)
    (top-form-repeats? node) (emit-with-cells emit-thunk)
    (top-form-has-fn? node) (emit-with-scope false emit-thunk)
    :else (emit-thunk)))

;; Hoist the VAR CELL a late-bound reference reads through, so the name lookup
;; (string-append + string-hash + a hashtable probe, measured at ~102ns) runs once
;; per def instead of once per access. Rides the same interning pool as the keyword
;; literals above, and for the same reason: jolt-var INTERNS, so it already returns
;; one stable object per ns/name and sharing it across sites changes nothing. Ten
;; references to clojure.core/str in one def therefore bind one cell, not ten.
;;
;; Eager (the pool binds at the top of the def) rather than the lazy
;; (or cell (set! cell …)) shape this replaces. Three reasons:
;;   - a cell has no resolution to defer that an (or …) branch could save; the
;;     lookup is the same lookup whenever it runs,
;;   - the eager form is an IMMUTABLE binding, so the def's closure needs no
;;     assignable frame slot, and
;;   - the emitted text is a quarter the size, which is what kept the seed and
;;     every built binary from growing.
;; Interning a cell early is not observable: jolt-var creates an UNBOUND cell, and
;; ns-publics / ns-interns / ns-map / resolve all filter on var-cell-defined?
;; (ns.ss ns-vars-pmap-when), so a hoisted reference to a var nothing ever defines
;; stays invisible exactly as it does today.
;; A record type's descriptor, by its tag, as an expression: inside a def, read
;; once into a per-site cell (a later read is one variable load); at top level,
;; the table lookup itself. #f while the type is not yet defined.
(defn cached-tag-desc [tag]
  (let [lookup (str "(hashtable-ref chez-tag-desc " (chez-str-lit tag) " #f)")
        cells *cache-cells*]
    (if cells
      (let [c (fresh-label "_cdesc$")]
        (swap! cells conj c)
        (str "(or " c " (let ((_d " lookup ")) (set! " c " _d) _d))"))
      lookup)))
(defn hoist-var-cell [ns nm]
  (str "(var-cell-deref " (hoist-const (str "(jolt-var " (chez-str-lit ns) " " (chez-str-lit nm) ")")) ")"))
;; A direct-linked SEED var: the def binds the var's root procedure once, at
;; load (jolt-seed-root checks it is one), and the site calls it like any Scheme
;; procedure. Only where jolt.host/seed-callable? said so — see emit-invoke.
(defn hoist-seed-root [ns nm]
  (hoist-const (str "(jolt-seed-root (jolt-var " (chez-str-lit ns) " " (chez-str-lit nm) "))")))
