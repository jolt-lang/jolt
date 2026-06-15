# Persistent list — an immutable singly-linked cons cell, modeled on Clojure's
# PersistentList. The whole point is O(1) prepend (conj/cons): a new node simply
# points at the existing list as its tail, sharing all of it, so building a list
# with repeated conj is O(n) total instead of O(n²) array copies.
#
# A node is:
#   @{:jolt/type :jolt/plist
#     :first x          head element
#     :rest  r          tail: another plist, or a Janet array/tuple (a list that
#                       was conj'd onto), or nil for the empty tail
#     :count n}         element count, or nil when unknown (cons onto a lazy tail)
#
# `:rest` may be a plain array/tuple so `(conj some-list x)` needn't copy the
# original list — the node just references it. pl->array materializes the chain.

(defn plist? [x]
  (and (table? x) (= :jolt/plist (get x :jolt/type))))

(defn- counted
  "Count of a tail value if known in O(1), else nil."
  [r]
  (cond
    (nil? r) 0
    (plist? r) (get r :count)
    (or (array? r) (tuple? r) (string? r) (buffer? r)) (length r)
    nil))

(defn pl-cons
  "Prepend x onto tail r (a plist / array / tuple / nil). O(1)."
  [x r]
  (def c (counted r))
  @{:jolt/type :jolt/plist :first x :rest r :count (if c (+ c 1) nil)})

(def EMPTY-PLIST @{:jolt/type :jolt/plist :first nil :rest nil :count 0})

(defn pl-empty? [p] (= 0 (get p :count)))
(defn pl-first [p] (get p :first))

(defn pl->array
  "Materialize the cons chain to a fresh Janet array."
  [p]
  (def out @[])
  (var cur p)
  (while (plist? cur)
    (if (= 0 (get cur :count))
      (set cur nil)
      (do (array/push out (get cur :first)) (set cur (get cur :rest)))))
  # cur is now a non-plist tail (array/tuple) or nil
  (when (and (not (nil? cur)) (or (array? cur) (tuple? cur)))
    (each x cur (array/push out x)))
  out)

(defn pl-count [p]
  (def c (get p :count))
  (if (nil? c) (length (pl->array p)) c))

(defn pl-rest
  "The tail as a seqable (array). Returns an empty array for a one-element list."
  [p]
  (if (or (= 0 (get p :count)) (nil? (get p :rest)))
    @[]
    (get p :rest)))

(defn pl-from-indexed
  "Build a plist from a Janet array/tuple, preserving order. O(n)."
  [xs]
  (var p EMPTY-PLIST)
  (var i (- (length xs) 1))
  (while (>= i 0)
    (set p (pl-cons (in xs i) p))
    (-- i))
  p)
