# Jolt Core — arrays, bit ops, coercions, hash, atoms/refs
# Extracted from core.janet (jolt-nma8, phase 2b split).

(use ./types)
(use ./phm)
(use ./phs)
(use ./lazyseq)
(use ./regex)
(use ./config)
(use ./pv)
(use ./plist)
(use ./core_types)
(use ./core_coll)
(use ./core_print)
(use ./core_io)
# Java-style arrays — backed by Janet's C primitives. Byte arrays use Janet
# buffers (contiguous, O(1) indexed get/put — genuinely fast); object and
# numeric arrays use Janet arrays. aget/aset/alength/aclone work over both.
# ============================================================

# alength / aget / aset now live in the Clojure collection tier — count/nth reads
# and an aset write through jolt.host/ref-put!. The typed/object array constructors
# below stay native (they build the mutable backing).

(defn core-aclone [arr]
  (cond
    (buffer? arr) (buffer/slice arr)
    (pvec? arr) (array ;(pv->array arr))
    (array/slice arr)))

# Numeric / object arrays: (T-array size) | (T-array size init) | (T-array seq)
(defn- make-num-array [a rest init]
  (if (number? a)
    (array/new-filled a (if (> (length rest) 0) (in rest 0) init))
    (array ;(realize-for-iteration a))))
(defn core-object-array [a & rest] (make-num-array a rest nil))
(defn core-int-array [a & rest] (make-num-array a rest 0))
(defn core-long-array [a & rest] (make-num-array a rest 0))
(defn core-short-array [a & rest] (make-num-array a rest 0))
(defn core-double-array [a & rest] (make-num-array a rest 0))
(defn core-float-array [a & rest] (make-num-array a rest 0))
(defn core-char-array [a & rest]
  # JVM char-array also accepts a STRING/char-seq (char[] of its characters) —
  # selmer's parse-str does (char-array template).
  (cond
    (string? a) (map make-char (string/bytes a))
    (buffer? a) (map make-char (string/bytes (string a)))
    (make-num-array a rest (make-char 0))))
(defn core-boolean-array [a & rest] (make-num-array a rest false))

# Byte arrays — Janet buffers (each element a 0..255 byte).
(defn core-byte-array [a & rest]
  (if (number? a)
    (buffer/new-filled a (band (if (> (length rest) 0) (in rest 0) 0) 0xff))
    (let [b (buffer/new 0)]
      (each x (realize-for-iteration a) (buffer/push-byte b (band x 0xff)))
      b)))

(defn core-aset-byte [arr i v] (put arr i (band v 0xff)) v)
(defn core-aset-int [arr i v] (put arr i v) v)
(defn core-aset-long [arr i v] (put arr i v) v)
(defn core-aset-short [arr i v] (put arr i v) v)
(defn core-aset-double [arr i v] (put arr i v) v)
(defn core-aset-float [arr i v] (put arr i v) v)
(defn core-aset-char [arr i v] (put arr i v) v)
(defn core-aset-boolean [arr i v] (put arr i v) v)

(defn core-make-array [a & rest]
  # (make-array len) or (make-array type len ...); ignore the type tag
  (let [len (if (number? a) a (in rest 0))] (array/new-filled len nil)))

(defn core-into-array [a & rest]
  (let [s (if (> (length rest) 0) (in rest 0) a)]
    (array ;(realize-for-iteration s))))

(defn core-to-array [coll]
  (def arr @[]) (each x (realize-for-iteration coll) (array/push arr x)) arr)
# to-array-2d lives in the Clojure collection tier (core/20-coll.clj).

# Array-element casts — identity on arrays; `bytes` coerces to a byte buffer.
(defn core-bytes [x] (if (buffer? x) x (core-byte-array x)))
(defn core-booleans [x] x)
(defn core-ints [x] x)
(defn core-longs [x] x)
(defn core-shorts [x] x)
(defn core-doubles [x] x)
(defn core-floats [x] x)
(defn core-chars [x] x)

# Scalar numeric coercions
(defn core-byte [x] (let [b (band (math/trunc x) 0xff)] (if (>= b 128) (- b 256) b)))
(defn core-short [x] (let [s (band (math/trunc x) 0xffff)] (if (>= s 0x8000) (- s 0x10000) s)))
# The masking unchecked-byte/short/char and float/double coercions live in
# the Clojure collection tier (core/20-coll.clj).

# 64-bit integers (Janet int/s64 — C-backed)
(defn core-bigint [x] (int/s64 x))
(defn core-biginteger [x] (int/s64 x))
# bigdec now lives in the Clojure collection tier (no BigDecimal: a double).

# Chunked seqs — Jolt does not chunk, so these are simple eager equivalents.
(defn core-chunk-buffer [capacity] @[])
(defn core-chunk-append [b x] (array/push b x) b)
(defn core-chunk [b] b)
# chunked-seq? now lives in the Clojure collection tier (always false on Jolt).
(defn core-chunk-first [s] (core-first s))
(defn core-chunk-rest [s] (core-rest s))
(defn core-chunk-next [s] (core-next s))
(defn core-chunk-cons [chunk rest] (core-concat (realize-for-iteration chunk) rest))

# More clojure.core: real implementations backed by existing Jolt machinery.
(defn core-boolean [x] (if x true false))
(defn core-cat [rf]
  (fn [& a]
    (case (length a)
      0 (rf) 1 (rf (a 0))
      (do (var acc (a 0)) (each x (realize-for-iteration (a 1)) (set acc (rf acc x))) acc))))
(defn core-reader-conditional [form splicing?]
  @{:jolt/type :jolt/reader-conditional :form form :splicing? splicing?})
# reader-conditional? now lives in the Clojure collection tier (tagged-value predicate).
# sorted-map-by / sorted-set-by (and all other sorted-coll constructors and
# semantics) now live in the Clojure sorted tier (core/25-sorted.clj).
# array-seq / seque live in the Clojure collection tier (core/20-coll.clj).
# supers now lives in the Clojure collection tier (no class hierarchy: #{}).
(defn core-class [x]
  (cond
    (nil? x) nil (number? x) "java.lang.Number" (string? x) "java.lang.String"
    (boolean? x) "java.lang.Boolean" (keyword? x) "clojure.lang.Keyword"
    (function? x) "clojure.lang.IFn" (buffer? x) "[B"
    (string (type x))))
# clojure-version / munge / test now live in the Clojure collection tier
# (core/20-coll.clj).


# ============================================================
# Bit operations (needed for persistent data structures)  
# ============================================================

(def core-bit-and (fn [a b] (band a b)))
(def core-bit-or (fn [a b] (bor a b)))
(def core-bit-xor (fn [a b] (bxor a b)))
(def core-bit-not (fn [a] (bnot a)))
(def core-bit-shift-left (fn [x n] (blshift x n)))
(def core-bit-shift-right (fn [x n] (brshift x n)))
(def core-bit-clear (fn [x n] (band x (bnot (blshift 1 n)))))
(def core-bit-set (fn [x n] (bor x (blshift 1 n))))
(def core-bit-flip (fn [x n] (bxor x (blshift 1 n))))
(def core-bit-test (fn [x n] (not= 0 (band x (blshift 1 n)))))
(def core-bit-and-not (fn [a b] (band a (bnot b))))
(def core-unsigned-bit-shift-right (fn [x n] (brushift x n)))

# ============================================================
# Integer coercion
# ============================================================

(def core-int (fn [x] (if (core-char? x) (x :ch) (math/trunc x))))
(def core-long (fn [x] (if (core-char? x) (x :ch) (math/trunc x))))
(def core-double (fn [x] (* 1.0 (if (core-char? x) (x :ch) x))))
(def core-float core-double)
# num and the unchecked-*/promoting-' arithmetic live in the Clojure
# collection tier (core/20-coll.clj) — jolt numbers don't overflow.
(defn core-char [x]
  "(char code-or-char) -> a character value."
  (cond
    (core-char? x) x
    (number? x) (make-char (math/trunc x))
    (string? x) (make-char (in x 0))
    (error "char expects a number or character")))

# ============================================================
# Hash
# ============================================================

(def core-hash (fn [x] (hash x)))


# ============================================================
# Atom
# ============================================================

(defn core-atom
  "Create an atom. Accepts optional :validator fn and :meta map."
  [val & opts]
  (var atm @{:jolt/type :jolt/atom :value val :watches @{} :validator nil})
  (var i 0)
  (while (< i (length opts))
    (case (opts i)
      :validator (put atm :validator (opts (+ i 1)))
      :meta (let [m (opts (+ i 1))]
              (var meta-tab @{})
              (each k (keys m) (put meta-tab k (get m k)))
              (table/setproto atm meta-tab)
              (put atm :jolt/meta m)))
    (+= i 2))
  atm)

# atom? now lives in the Clojure collection tier (tagged-value predicate).

# Futures — run the body on a real OS thread (ev/thread) for true parallelism.
# Janet threads have separate heaps, so the thunk and the state it closes over are
# MARSHALLED (copied) to the worker thread and the result is marshalled back. A
# future therefore sees a *snapshot* of captured state and communicates only via
# its return value — mutating a captured atom does not propagate to the parent.
# Coordination uses two channels: a thread-chan carries the single [:ok v] /
# [:error e] result back, and a parent-local chan acts as a broadcast latch that
# is closed when the result lands so any number of deref-ers can unpark.
(defn core-future? [x] (and (table? x) (= :jolt/future (x :jolt/type))))

(defn core-future-call [thunk]
  (def tc (ev/thread-chan 1))          # worker thread -> collector (shared, thread-safe)
  (def latch (ev/chan))                # parent-local: closed when the result is in
  (def fut @{:jolt/type :jolt/future :latch latch :cached false :res nil :cancelled false})
  # Worker: compute on a fresh OS thread, send back a marshalled result. The give
  # is guarded so a non-marshallable value can't strand deref-ers forever.
  (ev/spawn-thread
    (def res (try [:ok (thunk)] ([e] [:error e])))
    (try (ev/give tc res)
      ([_] (ev/give tc [:error "future result is not marshallable across threads"]))))
  # Collector: a parent-side fiber bridges the single result into the box and
  # closes the latch to wake every waiter. If the future was already cancelled,
  # the box is finalized — drop the late result and don't re-close the latch.
  (ev/spawn
    (def res (ev/take tc))
    (when (not (fut :cancelled))
      (put fut :res res)
      (put fut :cached true)
      (try (ev/chan-close latch) ([_] nil))))
  fut)

(defn- future-result [fut]
  (def res (fut :res))
  (if (= :error (in res 0)) (error (in res 1)) (in res 1)))

# future-done? / future-cancelled? now live in the Clojure collection tier (pure
# reads of :cached/:cancelled). core-future? stays — deref/future-cancel call it.
# Janet OS threads can't be interrupted, so the worker still runs to completion
# in the background; we can only mark the *future* cancelled (done) so deref
# raises and realized?/future-done?/future-cancelled? reflect it. Returns false
# if the future has already completed (matching Clojure).
(defn core-future-cancel [x]
  (if (and (core-future? x) (not (x :cached)) (not (x :cancelled)))
    (do
      (put x :cancelled true)
      (put x :res [:error "future cancelled"])
      (put x :cached true)
      (try (ev/chan-close (x :latch)) ([_] nil))
      true)
    false))

# future macro: (future body...) -> (future-call (fn* [] body...))
(defn core-deref [ref & opts]
  (cond
    (and (table? ref) (= :jolt/reduced (ref :jolt/type)))
    (ref :val)
    (and (table? ref) (= :jolt/atom (ref :jolt/type)))
    (ref :value)
    (and (table? ref) (= :jolt/volatile (ref :jolt/type)))
    (ref :val)
    (and (table? ref) (= :jolt/delay (ref :jolt/type)))
    (if (ref :realized) (ref :val)
      (let [v ((ref :fn))] (put ref :val v) (put ref :realized true) v))
    (and (table? ref) (= :jolt/future (ref :jolt/type)))
    (if (empty? opts)
      (do (when (not (ref :cached)) (ev/take (ref :latch))) (future-result ref))
      # (deref future timeout-ms timeout-val): wait at most timeout-ms. The
      # deadline cancels the parked take; if the result still hasn't landed we
      # return the supplied timeout value (the future keeps running).
      (let [timeout-val (in opts 1)]
        (when (not (ref :cached))
          (try (ev/with-deadline (/ (in opts 0) 1000) (ev/take (ref :latch))) ([_] nil)))
        (if (ref :cached) (future-result ref) timeout-val)))
    (and (table? ref) (= :jolt/var (ref :jolt/type)))
    (ref :root)
    ref))

(defn- atom-validate
  "Call validator on atm. Returns the value if valid, errors otherwise."
  [atm val]
  (let [v (atm :validator)]
    (if v
      (if (v val) val
        (error "Validator rejected value"))
      val)))

(defn- atom-notify-watches
  [atm old-val new-val]
  (loop [[k w] :pairs (atm :watches)]
    (w k atm old-val new-val)))

(defn core-reset! [atm val]
  (let [old-val (atm :value)]
    (atom-validate atm val)
    (put atm :value val)
    (atom-notify-watches atm old-val val)
    val))

(defn core-swap! [atm f & args]
  (var old-val (atm :value))
  (var new-val (apply f old-val args))
  (atom-validate atm new-val)
  (put atm :value new-val)
  (atom-notify-watches atm old-val new-val)
  new-val)

# Atom peripheral ops (swap-vals!/reset-vals!/compare-and-set!/get-validator/
# add-watch/remove-watch/set-validator!) now live in the Clojure collection tier —
# composed over the native atom ops + jolt.host/ref-put!. atom/swap!/reset!/deref
# and the atom-validate/atom-notify-watches helpers stay native (compiler-critical).

# ============================================================
