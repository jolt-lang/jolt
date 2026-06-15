# LazySeq — cell-by-cell lazy sequence (Clojure-compatible)
#
# Model: a thunk returns nil (empty) or a [first-val, rest-thunk] pair; each
# step produces one element + a thunk for the rest. Supports self-referencing
# sequences like fib-seq. Self-contained (janet builtins only) — the Clojure
# seq layer (core.janet) and the interpreter build on these primitives.
#
# Extracted from phm.janet (jolt-bvek): a lazy sequence has nothing to do with
# hash maps; both were tagged tables, which is why they shared a file.

(defn lazy-seq?
  "Check if x is a LazySeq."
  [x]
  (and (table? x) (= :jolt/lazy-seq (x :jolt/type))))

(defn make-lazy-seq [thunk]
  @{:jolt/type :jolt/lazy-seq :fn thunk :realized false :val nil})

(defn realize-ls
  "Force a LazySeq cell. Returns nil (empty) or [first-val, rest-thunk].
  If the thunk returns another lazy-seq, recursively realize it.
  Uses :jolt/pending sentinel to detect self-referencing cycles."
  [ls]
  (if (get ls :realized)
    (ls :val)
    (do
      (put ls :val :jolt/pending)
      (put ls :realized true)
      (let [raw ((ls :fn))
            v (if (lazy-seq? raw) (realize-ls raw) raw)]
        (put ls :val v)
        v))))

(defn ls-first [ls]
  (let [cell (realize-ls ls)]
    (if (or (nil? cell) (= :jolt/pending cell) (= 0 (length cell))) nil (in cell 0))))

# The memoized rest wrapper for a node whose cell yielded rest-thunk rt.
# EVERY walk must go through this (not a fresh make-lazy-seq) or independent
# walks re-run the shared thunks and side effects duplicate.
(defn ls-rest-cached [ls rt]
  (or (get ls :rest-ls)
      (let [w (make-lazy-seq rt)]
        (put ls :rest-ls w)
        w)))

(defn ls-rest [ls]
  (let [cell (realize-ls ls)]
    (if (or (nil? cell) (= 0 (length cell))) nil
      (let [rt (in cell 1)]
        (if (nil? rt) nil
          # Memoized wrapper (see ls-rest-cached): a fresh table per call gave
          # every independent walk its own realization state, so the shared
          # rest-thunks re-ran — duplicating side effects (a doall'd seq of
          # futures re-spawned them on the deref walk, serializing pmap).
          (ls-rest-cached ls rt))))))

(defn ls-seq [ls]
  (var result @[])
  (var cur ls)
  (while (not (nil? cur))
    (let [cell (realize-ls cur)]
      (if (nil? cell) (break))
      (array/push result (in cell 0))
      (set cur (ls-rest cur))))
  (if (= 0 (length result)) nil result))

(defn ls-count [ls]
  (var cnt 0)
  (var cur ls)
  (while (not (nil? cur))
    (let [cell (realize-ls cur)]
      (if (nil? cell) (break))
      (++ cnt)
      (set cur (ls-rest cur))))
  cnt)

(defn lazy-cons
  "Returns a LazySeq whose first element is x and whose rest is produced by
  rest-thunk (a 0-arg function returning nil or a LazySeq)."
  [x rest-thunk]
  (make-lazy-seq (fn [] @[x rest-thunk])))
