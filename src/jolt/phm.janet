# PersistentHashMap for Jolt — a HAMT (hash array mapped trie), the structure
# Clojure/ClojureScript/jank use. 32-way branching, 5 hash bits per level, with
# structural sharing: assoc/dissoc/get are O(log32 n) ~ effectively constant.
# Replaces the old flat copy-on-write bucket array, which was O(n) per assoc
# (O(n^2) to build — jolt-684u). Translated from the ClojureScript
# PersistentHashMap (cljs.core: BitmapIndexedNode / ArrayNode / HashCollisionNode).
#
# REP vs API: this file is ONLY the map representation (phm-* primitives). The
# Clojure-facing map ops (assoc/dissoc/get/conj/count/seq dispatch, nil-key,
# merge) live in core_coll.janet / core_types.janet, which recognize phm by its
# `:jolt/deftype` string and call these primitives. PersistentHashSet is layered
# on top in phs.janet. Transients are a separate mutable-table rep (core_types),
# so they don't touch this file.
#
# Node representation (Janet tuples, tagged at index 0; their arrays are built
# fresh on every modify and never mutated in place, so sharing is safe):
#   [:bin bitmap arr]   bitmap-indexed: arr is [k v k v ...]; a nil k means v is
#                       a sub-node (the slot recurses one level deeper).
#   [:an  cnt arr]      array-node: arr is 32 slots of sub-node-or-nil.
#   [:hcn hash cnt arr] hash-collision: arr is [k v k v ...] of same-hash keys.
# The map itself stays a table tagged :jolt/phm with :cnt (read directly by
# core), plus :root (the trie, nil when empty) and :has-nil/:nil-val (Clojure
# maps allow a nil key, which the trie can't store because a nil k marks a
# sub-node).
#
# Note on Janet `=`: it is STRUCTURAL on tuples, not identity, so cljs's
# `(identical? n node)` "nothing changed" early-outs are dropped here — we just
# rebuild the O(log32 n) path (an extra alloc only when overwriting an identical
# value). nil returns (a node that became empty) are kept; those are real nils.

(def- DEFTYPE "jolt.lang.persistent-hash-map.PersistentHashMap")

(defn phm? [x]
  (and (table? x) (= DEFTYPE (x :jolt/deftype))))

# Keys are hashed and compared by VALUE. Scalars are value-hashable in Janet;
# collection keys (a phm/pvec/plist/vector) are Janet tables hashed by identity,
# so they're canonicalized to a value-hashable struct/tuple first.
# `canonicalize-key` is injected by core (which knows those types); phm stays
# dependency-free. Keys are STORED as-is, so retrieval/iteration return originals.
(var canonicalize-key nil)
(defn set-canonicalize-key!
  "Install the value-canonicalizer for collection keys (called by core)."
  [f]
  (set canonicalize-key f))
(defn- ck [k]
  (if (and canonicalize-key (or (table? k) (struct? k) (array? k) (tuple? k)))
    (canonicalize-key k)
    k))
(defn canon
  "Public canonicalizer: maps a key to its value-hashable form (identity for
  scalars). Used by callers that index the same canonicalized tables phm uses."
  [k] (ck k))
# Identity/scalar equality first (the common case), then value equality.
(defn- key= [a b] (or (= a b) (= (ck a) (ck b))))
# Janet bit ops are 32-bit and split awkwardly: band/bor/bxor want SIGNED
# operands, brushift wants UNSIGNED — and `hash` is a signed 32-bit int. So we
# carry the hash as an UNSIGNED 32-bit value, extract the 5-bit level index with
# arithmetic (mask), and test/count bits with band against (1<<i) only — never
# brushift on a possibly-negative bitmap.
(defn- khash [k] (let [h (hash (ck k))] (if (< h 0) (+ h 0x100000000) h)))

# --- HAMT node machinery (translated from cljs.core) ------------------------
(def- EMPTY-BIN [:bin 0 (tuple)])

(defn- mask [h shift] (% (math/floor (/ h (blshift 1 shift))) 32))
(defn- bitpos [h shift] (blshift 1 (mask h shift)))
# popcount of a 32-bit bitmap; bidx = popcount of the bits below level index m.
(defn- popcount [bm]
  (var c 0) (var i 0)
  (while (< i 32) (when (not= 0 (band bm (blshift 1 i))) (++ c)) (++ i))
  c)
(defn- bidx [bitmap m]
  (var c 0) (var i 0)
  (while (< i m) (when (not= 0 (band bitmap (blshift 1 i))) (++ c)) (++ i))
  c)

(defn- cset1 [arr i a] (def n (array ;arr)) (put n i a) n)
(defn- cset2 [arr i a j b] (def n (array ;arr)) (put n i a) (put n j b) n)
# remove the pair at pair-index p (elements 2p, 2p+1)
(defn- remove-pair [arr p]
  (def out (array)) (def a (* 2 p)) (def b (+ a 1)) (def L (length arr))
  (var x 0) (while (< x L) (unless (or (= x a) (= x b)) (array/push out (in arr x))) (++ x))
  out)
# element-index of key k in a collision arr [k v k v ...], or -1
(defn- hcn-find [arr cnt k]
  (var i 0) (def L (* 2 cnt)) (var r -1)
  (while (< i L) (when (key= k (in arr i)) (set r i) (break)) (+= i 2))
  r)
# rebuild a bin-node from an array-node's slots when it shrinks (<=8) — each
# surviving sub-node becomes a nil-key slot.
(defn- pack-array-node [arr removed-idx]
  (def out (array)) (var bitmap 0) (var i 0)
  (while (< i 32)
    (when (and (not= i removed-idx) (not (nil? (in arr i))))
      (array/push out nil) (array/push out (in arr i))
      (set bitmap (bor bitmap (blshift 1 i))))
    (++ i))
  [:bin bitmap out])

# mutual recursion across node types
(var node-assoc nil) (var node-lookup nil) (var node-without nil) (var create-node nil)

(defn- bin-assoc [node shift h key val added]
  (def bitmap (in node 1)) (def arr (in node 2))
  (def m (mask h shift)) (def bit (blshift 1 m))
  (def idx (bidx bitmap m))
  (if (zero? (band bitmap bit))
    (let [nkeys (popcount bitmap)]
      (if (>= nkeys 16)
        # expand to an array-node
        (let [nodes (array/new-filled 32 nil)
              jdx (mask h shift)]
          (put nodes jdx (bin-assoc EMPTY-BIN (+ shift 5) h key val added))
          (var i 0) (var j 0)
          (while (< i 32)
            (unless (zero? (band bitmap (blshift 1 i)))
              (let [ek (in arr (* 2 j)) ev (in arr (+ 1 (* 2 j)))]
                (put nodes i (if (nil? ek) ev
                               (bin-assoc EMPTY-BIN (+ shift 5) (khash ek) ek ev added)))
                (++ j)))
            (++ i))
          [:an (+ nkeys 1) nodes])
        # insert (key val) into the bin arr at pair-index idx
        (let [out (array)]
          (array/concat out (array/slice arr 0 (* 2 idx)))
          (array/push out key) (array/push out val)
          (array/concat out (array/slice arr (* 2 idx)))
          (put added 0 true)
          [:bin (bor bitmap bit) out])))
    # slot occupied
    (let [kon (in arr (* 2 idx)) von (in arr (+ 1 (* 2 idx)))]
      (cond
        (nil? kon)
          [:bin bitmap (cset1 arr (+ 1 (* 2 idx)) (node-assoc von (+ shift 5) h key val added))]
        (key= key kon)
          [:bin bitmap (cset1 arr (+ 1 (* 2 idx)) val)]
        (do (put added 0 true)
            [:bin bitmap (cset2 arr (* 2 idx) nil (+ 1 (* 2 idx))
                               (create-node (+ shift 5) kon von h key val))])))))

(defn- an-assoc [node shift h key val added]
  (def cnt (in node 1)) (def arr (in node 2))
  (def idx (mask h shift)) (def sub (in arr idx))
  (if (nil? sub)
    [:an (+ cnt 1) (cset1 arr idx (bin-assoc EMPTY-BIN (+ shift 5) h key val added))]
    [:an cnt (cset1 arr idx (node-assoc sub (+ shift 5) h key val added))]))

(defn- hcn-assoc [node shift h key val added]
  (def chash (in node 1)) (def cnt (in node 2)) (def arr (in node 3))
  (if (= h chash)
    (let [idx (hcn-find arr cnt key)]
      (if (= idx -1)
        (let [out (array ;arr)] (array/push out key) (array/push out val)
             (put added 0 true) [:hcn chash (+ cnt 1) out])
        (if (= (in arr (+ 1 idx)) val) node
          [:hcn chash cnt (cset1 arr (+ 1 idx) val)])))
    # different hash at this level: wrap in a bin node, then assoc
    (bin-assoc [:bin (bitpos chash shift) (array nil node)] shift h key val added)))

(set create-node (fn [shift k1 v1 k2h k2 v2]
  (def k1h (khash k1))
  (if (= k1h k2h)
    [:hcn k1h 2 (array k1 v1 k2 v2)]
    (let [added @[false]
          n1 (bin-assoc EMPTY-BIN shift k1h k1 v1 added)]
      (bin-assoc n1 shift k2h k2 v2 added)))))

(set node-assoc (fn [node shift h key val added]
  (case (in node 0)
    :bin (bin-assoc node shift h key val added)
    :an (an-assoc node shift h key val added)
    :hcn (hcn-assoc node shift h key val added))))

(defn- bin-lookup [node shift h key nf]
  (def bitmap (in node 1)) (def arr (in node 2))
  (def m (mask h shift)) (def bit (blshift 1 m))
  (if (zero? (band bitmap bit)) nf
    (let [idx (bidx bitmap m) kon (in arr (* 2 idx)) von (in arr (+ 1 (* 2 idx)))]
      (cond
        (nil? kon) (node-lookup von (+ shift 5) h key nf)
        (key= key kon) von
        nf))))

(defn- an-lookup [node shift h key nf]
  (def sub (in (in node 2) (mask h shift)))
  (if (nil? sub) nf (node-lookup sub (+ shift 5) h key nf)))

(defn- hcn-lookup [node shift h key nf]
  (def idx (hcn-find (in node 3) (in node 2) key))
  (if (< idx 0) nf (in (in node 3) (+ 1 idx))))

(set node-lookup (fn [node shift h key nf]
  (case (in node 0)
    :bin (bin-lookup node shift h key nf)
    :an (an-lookup node shift h key nf)
    :hcn (hcn-lookup node shift h key nf))))

(defn- bin-without [node shift h key]
  (def bitmap (in node 1)) (def arr (in node 2))
  (def m (mask h shift)) (def bit (blshift 1 m))
  (if (zero? (band bitmap bit)) node
    (let [idx (bidx bitmap m) kon (in arr (* 2 idx)) von (in arr (+ 1 (* 2 idx)))]
      (cond
        (nil? kon)
          (let [nn (node-without von (+ shift 5) h key)]
            (cond (not (nil? nn)) [:bin bitmap (cset1 arr (+ 1 (* 2 idx)) nn)]
                  (= bitmap bit) nil
                  [:bin (bxor bitmap bit) (remove-pair arr idx)]))
        (key= key kon)
          (if (= bitmap bit) nil [:bin (bxor bitmap bit) (remove-pair arr idx)])
        node))))

(defn- an-without [node shift h key]
  (def cnt (in node 1)) (def arr (in node 2))
  (def idx (mask h shift)) (def sub (in arr idx))
  (if (nil? sub) node
    (let [nn (node-without sub (+ shift 5) h key)]
      (cond
        (nil? nn) (if (<= cnt 8) (pack-array-node arr idx) [:an (- cnt 1) (cset1 arr idx nil)])
        [:an cnt (cset1 arr idx nn)]))))

(defn- hcn-without [node shift h key]
  (def chash (in node 1)) (def cnt (in node 2)) (def arr (in node 3))
  (def idx (hcn-find arr cnt key))
  (cond (= idx -1) node
        (= cnt 1) nil
        [:hcn chash (- cnt 1) (remove-pair arr (brshift idx 1))]))

(set node-without (fn [node shift h key]
  (case (in node 0)
    :bin (bin-without node shift h key)
    :an (an-without node shift h key)
    :hcn (hcn-without node shift h key))))

# depth-first walk: call (f k v) for every entry (the nil key is handled at the
# map level, not in the trie).
(defn- node-each [node f]
  (case (in node 0)
    :bin (let [arr (in node 2) L (length arr)]
           (var i 0)
           (while (< i L)
             (let [k (in arr i) v (in arr (+ i 1))]
               (if (nil? k) (when v (node-each v f)) (f k v)))
             (+= i 2)))
    :an (let [arr (in node 2)]
          (var i 0) (while (< i 32) (when (in arr i) (node-each (in arr i) f)) (++ i)))
    :hcn (let [arr (in node 3) L (* 2 (in node 2))]
           (var i 0) (while (< i L) (f (in arr i) (in arr (+ i 1))) (+= i 2)))))

# --- map value + public API -------------------------------------------------
(defn- mk [cnt root has-nil nil-val meta]
  @{:jolt/type :jolt/phm :jolt/deftype DEFTYPE
    :cnt cnt :root root :has-nil has-nil :nil-val nil-val :_meta meta})

(defn phm-get [m k &opt default]
  (default default nil)
  (if (nil? k)
    (if (m :has-nil) (m :nil-val) default)
    (let [root (m :root)]
      (if root (node-lookup root 0 (khash k) k default) default))))

(def- NF (gensym))
(defn phm-contains? [m k]
  (if (nil? k) (truthy? (m :has-nil))
    (let [root (m :root)]
      (if root (not= (node-lookup root 0 (khash k) k NF) NF) false))))

(defn phm-assoc [m k v]
  (if (nil? k)
    (mk (if (m :has-nil) (m :cnt) (+ (m :cnt) 1)) (m :root) true v (m :_meta))
    (let [added @[false]
          root (or (m :root) EMPTY-BIN)
          nroot (node-assoc root 0 (khash k) k v added)]
      (mk (if (in added 0) (+ (m :cnt) 1) (m :cnt)) nroot (m :has-nil) (m :nil-val) (m :_meta)))))

(defn phm-dissoc [m k]
  (if (nil? k)
    (if (m :has-nil) (mk (- (m :cnt) 1) (m :root) false nil (m :_meta)) m)
    (let [root (m :root)]
      (if (and root (phm-contains? m k))
        (mk (- (m :cnt) 1) (node-without root 0 (khash k) k) (m :has-nil) (m :nil-val) (m :_meta))
        m))))

(defn phm-entries [m]
  (def out @[])
  (when (m :has-nil) (array/push out [nil (m :nil-val)]))
  (when (m :root) (node-each (m :root) (fn [k v] (array/push out [k v]))))
  out)

(defn phm-to-struct [m]
  # a Janet struct can't hold a nil key (matches Clojure struct/keys behavior);
  # every other entry carries over.
  (def t @{})
  (when (m :root) (node-each (m :root) (fn [k v] (put t k v))))
  (table/to-struct t))

(defn phm-count [m] (m :cnt))

# --- bulk bottom-up build (jolt-5vsp collections) ---------------------------
# Build the HAMT in one pass from a native array of entries, instead of n
# incremental phm-assoc calls (each of which rebuilt the O(log32 n) path with
# fresh arrays AND allocated a fresh map wrapper). The structure produced is
# IDENTICAL to the incremental one: the trie shape is a function of the key set
# (hash-partitioned, with the same bin<=16 / array-node>=17 promotion threshold
# at each level), so nth/lookup/assoc/without/each read it unchanged. Only a
# hash-collision node's internal order is insertion-dependent, and we preserve
# insertion order there too. Validated against phm-assoc across the size and
# branching boundaries (see the throwaway script in the PR).
#
# Entries are @[h key val] triples (h = khash key). build-node takes a non-empty
# group all destined for the same parent slot and returns its node.
(defn- all-same-hash? [entries]
  (def h0 (in (in entries 0) 0))
  (var same true) (var i 1) (def L (length entries))
  (while (< i L) (when (not= (in (in entries i) 0) h0) (set same false) (break)) (++ i))
  same)

(defn- build-node [entries shift]
  (if (and (> (length entries) 1) (all-same-hash? entries))
    # all keys share a full 32-bit hash -> collision node (insertion order)
    (let [arr (array)]
      (each e entries (array/push arr (in e 1)) (array/push arr (in e 2)))
      [:hcn (in (in entries 0) 0) (length entries) arr])
    # partition by the 5-bit mask at this level; iterate slots 0..31 ascending
    # so the bin arr / array-node slots land in canonical (bidx) order.
    (let [groups @{}]
      (each e entries
        (def m (mask (in e 0) shift))
        (if-let [g (in groups m)] (array/push g e) (put groups m @[e])))
      (def occupied (array))
      (var i 0) (while (< i 32) (when (in groups i) (array/push occupied i)) (++ i))
      (def s (length occupied))
      (if (>= s 17)
        # array-node: every occupied slot is a sub-node (a lone key becomes its
        # own single-key bin node at shift+5, matching the expand path).
        (let [slots (array/new-filled 32 nil)]
          (each m occupied (put slots m (build-node (in groups m) (+ shift 5))))
          [:an s slots])
        # bitmap-indexed node: lone key -> leaf pair, group -> nil + sub-node.
        (let [arr (array)]
          (var bitmap 0)
          (each m occupied
            (def g (in groups m))
            (set bitmap (bor bitmap (blshift 1 m)))
            (if (= 1 (length g))
              (do (array/push arr (in (in g 0) 1)) (array/push arr (in (in g 0) 2)))
              (do (array/push arr nil) (array/push arr (build-node g (+ shift 5))))))
          [:bin bitmap arr])))))

(defn phm-from-pairs [pairs &opt meta]
  "Bulk-build a phm from an indexed collection of native [k v] pairs. Duplicate
  keys follow assoc semantics (last value wins, first-seen key object kept). The
  caller must pass native tuples/arrays (phm stays free of the value layer)."
  (default meta nil)
  (def entries @[])
  (def seen @{})              # canon-key -> index into entries (dedup)
  (var has-nil false) (var nil-val nil)
  (each p pairs
    (def k (in p 0)) (def v (in p 1))
    (if (nil? k)
      (do (set has-nil true) (set nil-val v))
      (let [c (ck k)]
        (if-let [idx (in seen c)]
          (put (in entries idx) 2 v)                      # last value wins
          (do (put seen c (length entries))
              (array/push entries @[(khash k) k v]))))))
  (def cnt (+ (length entries) (if has-nil 1 0)))
  (def root (if (= 0 (length entries)) nil (build-node entries 0)))
  (mk cnt root has-nil nil-val meta))

(defn make-phm [&opt kvs]
  (default kvs nil)
  (if (or (nil? kvs) (= 0 (length kvs)))
    (mk 0 nil false nil nil)
    # pair up the flat [k0 v0 k1 v1 ...] array and bulk-build
    (let [pairs (array) n (length kvs)]
      (var i 0) (while (< i n) (array/push pairs [(in kvs i) (in kvs (+ i 1))]) (+= i 2))
      (phm-from-pairs pairs))))
