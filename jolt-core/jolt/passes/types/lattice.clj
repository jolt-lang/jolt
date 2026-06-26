(ns jolt.passes.types.lattice
  "Structural type lattice for jolt.passes.types: scalar/struct/vec/set/union
  types, join, depth-cap, shape, and the numeric/vector return-fn name sets. Pure
  (no inference state) — the inference + checker in jolt.passes.types build on it.")

;; ---------------------------------------------------------------------------
;; Collection-type inference, intra-procedural. A forward,
;; soft-typing-style pass (simplified HM: monovariant, never-fails, lattice top
;; = :any) that types expressions from literals/arithmetic and flows the type
;; through let bindings and if-joins. Where a keyword-lookup subject is PROVEN a
;; plain struct map it sets :hint :struct (the same channel a manual hint uses,
;; so the back end drops the guard); where the type is :any it leaves the
;; dynamic guard in place. Sound by construction: a concrete type is assigned
;; only when proven, so a wrong bare get is impossible.
;;
;; Recursive STRUCTURAL types (RFC 0005). A type mirrors the data tree:
;;   compound: {:struct {field -> T}}  (raw-get-safe map, field types)
;;             {:vec T}                (vector of T)
;;             {:set T}                (set of T)
;;   scalar:   :num :str :kw :truthy   (all provably non-nil/non-false)
;;             :phm                    (persistent hash map; NOT raw-get-safe)
;;   :any (top), nil (bottom, identity for join).
;; Compound types are small jolt maps, so they compare by value on both the
;; Clojure and the host (orchestrator) side. struct/vec/set use distinct keys so
;; a type is recognised by which key it carries.
;; (get t :KEY) is nil for a keyword type and the child for a compound, so a
;; compound is detected by some? — no map?/contains? needed.
(defn velem [t] (get t :vec))
(defn selem [t] (get t :set))
(defn sfields [t] (get t :struct))
(defn vec-type? [t] (some? (velem t)))
(defn set-type? [t] (some? (selem t)))
(defn struct-type? [t] (some? (sfields t)))
(defn mk-vec [t] {:vec (if t t :any)})
(defn mk-set [t] {:set (if t t :any)})
(defn mk-struct [fs] {:struct fs})

;; Bounded union types (RFC 0006). A union {:union #{T...}} records
;; that a value is provably one of a small, fixed set of SCALAR types — what
;; differing if-branches used to collapse to :any. It exists so the success
;; checker can reject a use where EVERY member is in the op's error domain
;; ((inc (if c "a" :k))) while still accepting one where any member is valid
;; ((inc (if c 1 "x"))). Scalars only, capped cardinality: the member space is
;; the five scalar tags, so the lattice stays finite and the inter-procedural
;; fixpoint terminates. A union is opaque to every STRUCTURAL predicate
;; (struct-type?/vec-type?/set-type? key on :struct/:vec/:set, which a union
;; lacks), so specialization treats it exactly like :any — codegen is
;; unchanged; only the checker reads inside it.
(def union-cap 4)
(defn scalar-t? [t] (or (= t :num) (= t :str) (= t :kw) (= t :truthy) (= t :phm)))
(defn union-type? [t] (some? (get t :union)))
(defn umembers [t] (get t :union))
(defn union-of
  "Normalize a seq of member types into a lattice value: flatten nested unions,
  keep only scalars (any non-scalar member collapses the whole thing to :any,
  the conservative top), then return the lone member if one, {:union #{...}}
  for 2..cap distinct scalars, or :any past the cap."
  [ts]
  (let [flat (reduce (fn [acc t]
                       (if (union-type? t)
                         (reduce conj acc (umembers t))
                         (conj acc t)))
                     #{} ts)]
    (cond
      (not (every? scalar-t? flat)) :any
      (= 0 (count flat)) :any
      (= 1 (count flat)) (first flat)
      (> (count flat) union-cap) :any
      :else {:union flat})))

(declare join-t)
(defn merge-fields
  "Per-field join of two field maps (a key in only one side joins with :any)."
  [fa fb]
  (let [m1 (reduce (fn [m k] (assoc m k (join-t (get fa k :any) (get fb k :any)))) {} (keys fa))]
    (reduce (fn [m k] (if (get m k) m (assoc m k (join-t (get fa k :any) (get fb k :any))))) m1 (keys fb))))
(defn join-t [a b]
  (cond
    (= a b) a
    (nil? a) b
    (nil? b) a
    ;; :double is a flonum refinement of :num: two doubles stay :double (caught by
    ;; = above), but a double joined with anything else loses the flonum guarantee
    ;; and widens to :num before joining — so a param is :double only when EVERY
    ;; contributing value is a flonum, which is what makes the hintless fl-op sound.
    (or (= a :double) (= b :double))
      (join-t (if (= a :double) :num a) (if (= b :double) :num b))
    (and (struct-type? a) (struct-type? b))
      (let [merged (mk-struct (merge-fields (sfields a) (sfields b)))]
        ;; joining two values of the SAME complete shape preserves it — the
        ;; merged struct has the same key set. Different shapes
        ;; (or an incomplete side) drop it, as the layout is no longer proven.
        (if (and (get a :shape) (= (get a :shape) (get b :shape)))
          (assoc merged :shape (get a :shape))
          merged))
    (and (vec-type? a) (vec-type? b)) (mk-vec (join-t (velem a) (velem b)))
    (and (set-type? a) (set-type? b)) (mk-set (join-t (selem a) (selem b)))
    ;; differing kinds: form a scalar union when both sides reduce to scalars
    ;; (or scalar unions); anything compound on either side stays :any
    :else (let [ma (cond (union-type? a) (umembers a) (scalar-t? a) #{a} :else nil)
                mb (cond (union-type? b) (umembers b) (scalar-t? b) #{b} :else nil)]
            (if (and ma mb) (union-of (reduce conj ma mb)) :any))))
(defn join [a b] (join-t a b))
;; depth cap (RFC 0005): truncate a type below depth d to :any, so recursive data
;; can't make an infinite type and the inter-procedural fixpoint stays finite.
(def type-depth 4)
(defn cap [t d]
  (cond
    (<= d 0) (if (or (struct-type? t) (vec-type? t) (set-type? t)) :any t)
    (struct-type? t)
      ;; capping truncates VALUES below depth d, but the KEY SET is unchanged, so
      ;; a complete :shape survives — keep it so nested/container field reads can
      ;; still bare-index. cap recurses into fields, so a nested
      ;; shaped value (a vec3 inside a hit-info) keeps its own :shape too.
      (let [capped (mk-struct (reduce (fn [m k] (assoc m k (cap (get (sfields t) k) (dec d))))
                                      {} (keys (sfields t))))
            ;; the record :type tag (and :shape) are independent of field-value
            ;; depth, so they survive truncation — a record read from a deep
            ;; container keeps its identity, so devirtualization, record? folding,
            ;; and the record fast path still fire on it.
            capped (if (get t :shape) (assoc capped :shape (get t :shape)) capped)
            capped (if (get t :type) (assoc capped :type (get t :type)) capped)]
        capped)
    (vec-type? t) (mk-vec (cap (velem t) (dec d)))
    (set-type? t) (mk-set (cap (selem t) (dec d)))
    :else t))
;; raw-get-safe (a struct / record): a struct type. The field type of key
;; k, if known, else :any.
(defn struct-safe? [t] (struct-type? t))
(defn field-type [t k] (if (struct-type? t) (get (sfields t) k :any) :any))
;; Shape (hidden class). A struct type built from a map LITERAL carries
;; its complete layout — :shape, the canonical (str-sorted) key vector. The back
;; end represents such a map as a shape tuple and reads a field by bare index.
;; A struct type from a JOIN or from field-access inference has no :shape
;; (incomplete: the full key set isn't proven), so it keeps the dynamic path —
;; never a bare index. No shape is hardcoded; any constant key set is one.
(defn shape-order
  "Canonical key order for a shape: keys sorted by their string form, so two
  literals with the same keys in any order intern to the same shape."
  [ks] (vec (sort (fn [a b] (compare (str a) (str b))) ks)))
(defn type-shape [t] (get t :shape))
;; tag a node (any expression, not just a :local) so the back end can specialize
;; a lookup whose SUBJECT is that node — this is what makes nested access work:
;; (:direction ray) is tagged struct, so (:r (:direction ray)) drops its guard.
;; tag a lookup subject as a struct, carrying the complete shape when known
;; (so the back end bare-indexes).
(defn mark-struct [node t]
  (let [n (assoc node :hint :struct)]
    (if (get t :shape) (assoc n :shape (get t :shape)) n)))
;; a value provably neither nil nor false — the back end only builds a struct
;; (vs a phm) when every value is non-nil/non-false, so a map literal is a struct
;; only when all its values have such a type. Collections are non-nil.
(defn truthy-type? [t]
  (or (= t :num) (= t :double) (= t :str) (= t :kw) (= t :truthy) (= t :phm)
      (struct-type? t) (vec-type? t) (set-type? t)))

;; core fns whose result is a number (so it is non-nil/non-false and, for the
;; success-type checker, provably numeric).
(def num-ret-fns
  #{"+" "-" "*" "/" "inc" "dec" "mod" "rem" "quot" "min" "max" "abs"
    "bit-and" "bit-or" "bit-xor" "count"})
(def vector-ret-fns #{"vec" "vector" "mapv" "filterv" "subvec"})
