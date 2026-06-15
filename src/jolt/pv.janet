# Persistent vector — a 32-way branching trie with a tail buffer, modeled on
# Clojure's PersistentVector. Immutable: every update returns a new vector that
# structurally shares unchanged subtrees with the old one, so conj/assoc/pop are
# O(log32 n) and share memory instead of copying the whole vector.
#
# Layout:
#   @{:jolt/type :jolt/pvec
#     :cnt   n          number of elements
#     :shift s          bits to shift the index for the root level (5 * depth)
#     :root  node       trie root: a tuple of up to 32 children
#     :tail  tail}      a tuple of up to 32 trailing elements (append fast-path)
#
# Trie nodes are immutable tuples so unchanged subtrees are shared by identity.
#
# REP vs API: this file is ONLY the trie representation (pv-* primitives). The
# Clojure-facing vector ops (nth/conj/assoc/subvec, the count/seq/first/rest
# dispatch, and tuple↔pvec polymorphism) live in core_coll.janet /
# core_types.janet, which dispatch on `:jolt/type :jolt/pvec`.

(def- bits 5)
(def- width 32)         # 2^bits
(def- mask 31)          # width - 1

(def empty-node [])

(defn pvec? [x]
  (and (table? x) (= :jolt/pvec (get x :jolt/type))))

(defn make-pv [cnt shift root tail]
  @{:jolt/type :jolt/pvec :cnt cnt :shift shift :root root :tail tail})

(def EMPTY (make-pv 0 bits empty-node []))

(defn pv-count [pv] (get pv :cnt))

# Index of the first element held in the tail (everything before lives in the trie).
(defn- tail-offset [cnt]
  (if (< cnt width) 0 (blshift (brshift (- cnt 1) bits) bits)))

# Return the 32-element leaf array containing index i.
(defn- leaf-for [pv i]
  (if (>= i (tail-offset (get pv :cnt)))
    (get pv :tail)
    (do
      (var node (get pv :root))
      (var level (get pv :shift))
      (while (> level 0)
        (set node (get node (band (brshift i level) mask)))
        (set level (- level bits)))
      node)))

(defn pv-nth [pv i &opt dflt]
  (if (and (>= i 0) (< i (get pv :cnt)))
    (get (leaf-for pv i) (band i mask))
    dflt))

# --- conj -------------------------------------------------------------------

# Push the full tail down into the trie, returning a new root node array.
(defn- push-tail [level parent tail cnt]
  (def sub-idx (band (brshift (- cnt 1) level) mask))
  (def child
    (if (= level bits)
      tail
      (let [c (get parent sub-idx)]
        (if c
          (push-tail (- level bits) c tail cnt)
          (push-tail (- level bits) empty-node tail cnt)))))
  (def arr (array/slice parent))
  (put arr sub-idx child)
  (tuple/slice arr))

(defn pv-conj [pv val]
  (def cnt (get pv :cnt))
  (def tail (get pv :tail))
  (if (< (length tail) width)
    # Room in the tail: just append to it.
    (make-pv (+ cnt 1) (get pv :shift)
             (get pv :root)
             (tuple/slice (tuple ;tail val)))
    # Tail is full: push it into the trie, start a fresh tail with val.
    (let [shift (get pv :shift)
          root (get pv :root)]
      (if (> (brshift cnt bits) (blshift 1 shift))
        # Root overflow: grow the trie one level taller.
        (let [new-root (tuple root (push-tail shift empty-node tail cnt))]
          (make-pv (+ cnt 1) (+ shift bits) new-root [val]))
        (make-pv (+ cnt 1) shift (push-tail shift root tail cnt) [val])))))

# --- assoc ------------------------------------------------------------------

(defn- assoc-in-node [level node i val]
  (def arr (array/slice node))
  (if (= level 0)
    (put arr (band i mask) val)
    (let [sub-idx (band (brshift i level) mask)]
      (put arr sub-idx (assoc-in-node (- level bits) (get node sub-idx) i val))))
  (tuple/slice arr))

(defn pv-assoc [pv i val]
  (def cnt (get pv :cnt))
  (cond
    (= i cnt) (pv-conj pv val)
    (and (>= i 0) (< i cnt))
    (if (>= i (tail-offset cnt))
      (let [tail (array/slice (get pv :tail))]
        (put tail (band i mask) val)
        (make-pv cnt (get pv :shift) (get pv :root) (tuple/slice tail)))
      (make-pv cnt (get pv :shift)
               (assoc-in-node (get pv :shift) (get pv :root) i val)
               (get pv :tail)))
    (error (string "Index " i " out of bounds for vector of length " cnt))))

# --- pop --------------------------------------------------------------------

(defn- pop-tail [level node cnt]
  (def sub-idx (band (brshift (- cnt 2) level) mask))
  (cond
    (> level bits)
    (let [child (pop-tail (- level bits) (get node sub-idx) cnt)]
      (if (and (nil? child) (= sub-idx 0))
        nil
        (let [arr (array/slice node)] (put arr sub-idx child) (tuple/slice arr))))
    (= sub-idx 0) nil
    (let [arr (array/slice node)] (put arr sub-idx nil) (tuple/slice arr))))

(defn pv-pop [pv]
  (def cnt (get pv :cnt))
  (cond
    (= cnt 0) (error "Can't pop empty vector")
    (= cnt 1) EMPTY
    (> (- cnt (tail-offset cnt)) 1)
    # More than one element in the tail: drop the last tail element.
    (let [tail (get pv :tail)]
      (make-pv (- cnt 1) (get pv :shift) (get pv :root)
               (tuple/slice tail 0 (- (length tail) 1))))
    # Tail has one element: the new tail is the last leaf of the trie.
    (let [shift (get pv :shift)
          new-tail (leaf-for pv (- cnt 2))
          new-root-raw (pop-tail shift (get pv :root) cnt)
          new-root (if (nil? new-root-raw) empty-node new-root-raw)]
      (if (and (> shift bits) (nil? (get new-root 1)))
        # Trie lost a level: promote the single remaining child to root.
        (make-pv (- cnt 1) (- shift bits) (get new-root 0) new-tail)
        (make-pv (- cnt 1) shift new-root new-tail)))))

# --- conversions ------------------------------------------------------------

(defn pv->array [pv]
  (def out @[])
  (def cnt (get pv :cnt))
  (var i 0)
  (while (< i cnt)
    (array/push out (pv-nth pv i))
    (++ i))
  out)

(defn pv-from-indexed [xs]
  # Build a pvec from any Janet-indexed collection (tuple/array).
  (var pv EMPTY)
  (def n (length xs))
  (var i 0)
  (while (< i n) (set pv (pv-conj pv (in xs i))) (++ i))
  pv)
