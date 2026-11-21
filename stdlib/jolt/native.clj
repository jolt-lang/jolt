(ns jolt.native
  "Native regions: a block of numeric code compiled to C by jolt build, with
  Carp-style ownership so the arrays it creates are freed deterministically and
  nothing it allocates can escape.

      (require '[jolt.native :refer [native]])

      (defn norm [^doubles xs]
        (native [^doubles xs]
          (let [n (alength xs)]
            (loop [i 0 s 0.0]
              (if (< i n)
                (recur (inc i) (+ s (* (aget xs i) (aget xs i))))
                (Math/sqrt s))))))

  The vector names the locals the region reads, each with its type: ^long,
  ^double, ^boolean, ^doubles or ^longs (an array the region borrows: it reads
  and writes the caller's array in place, and cannot keep it). A borrowed
  long-array holds fixnums, so storing a value outside -2^60..2^60-1 into one
  raises; widen it outside the region first. The body is a
  subset of Clojure: let, if, when, do, loop/recur, dotimes, and/or/not,
  arithmetic and comparisons, quot/rem/mod, long/double, aget/aset/alength,
  double-array/long-array/aclone (arrays the region owns), and Math/sqrt, sin,
  cos, tan, exp, log, floor, ceil, pow, atan2. It returns a long, double,
  boolean, nil, or an array it owns, which arrives as a fresh double-array or
  long-array.

  The region is checked when it is compiled — in the REPL too — and a program
  that would use an array after giving it away, or let one escape, is a compile
  error. Long arithmetic overflows as on the JVM (ArithmeticException) rather
  than promoting to a bignum, and every failure raises the JVM's exception.

  By default, while a region runs as C, other threads keep running and
  collecting, and the arrays it borrows are pinned in place for the call
  (about 45ns a call). Mark the vector ^:hold-gc for a plain call (about 2.5ns)
  that holds every other thread's collection until the region returns, as
  GHC's unsafe foreign calls do; use it only for regions that finish quickly.
  Either way the region is memory safe.

      (native ^:hold-gc [^double x] (* x x))

  In a binary built with jolt build the body runs as C. Anywhere else (the
  REPL, jolt run) the same checked program runs as Jolt, so the two agree. jolt
  build needs a C compiler for a program with regions."
  (:require [jolt.native.ast :as ast]
            [jolt.native.memory :as memory]
            [jolt.native.emit :as emit]
            [jolt.native.fallback :as fallback]
            [jolt.native.rt]
            [jolt.ffi]
            [jolt.scheme]))

(defn- fnv32
  "FNV-1a over the string's chars, kept in 32 bits: a stable name for a region
  across processes (the build's and the binary's must agree)."
  [s offset]
  (reduce (fn [h c] (bit-and (* (bit-xor h (int c)) 16777619) 0xffffffff))
          offset s))

(defn- region-sym [src]
  (str "jn_" (format "%08x%08x" (fnv32 src 2166136261) (fnv32 src 84696351))))

(defn- param-kind [t]
  (cond (= t [:ref :double]) :doubles (= t [:ref :long]) :longs
        (= t :long) :long (= t :double) :double (= t :bool) :bool))

(defn- result-kind [t]
  (cond (= t [:array :double]) :doubles (= t [:array :long]) :longs
        (#{:unit :never} t) :unit :else t))

(defn compile-region
  "Check and lower a region. Answers {:sym :src :region} — the C entry's name
  and source, and the checked IR. Throws the region's compile error."
  [params body]
  (let [r (memory/check-region (ast/analyze-region params body))
        sym (region-sym (emit/emit-region r "JN_SYM"))]
    {:sym sym :src (emit/emit-region r sym) :region r}))

(defmacro native
  "Run body as C in a built binary, as checked Jolt elsewhere. params names the
  locals it reads, each hinted ^long ^double ^boolean ^doubles or ^longs."
  [params & body]
  (let [{:keys [sym src region]} (compile-region params body)
        names (mapv :name (:params region))
        plain (mapv #(with-meta % nil) names)]
    ;; (jolt.native/__region sym src [param-kinds] result-kind [args] fallback-fn raise! mode)
    ;; the analyzer derives the Chez foreign types from the kinds
    (list 'jolt.native/__region
          sym src
          (mapv (comp param-kind :type) (:params region))
          (result-kind (:type region))
          plain
          (list 'fn* names (fallback/region-form region))
          'jolt.native.rt/raise!
          (if (:hold-gc (meta params)) :hold-gc :pinned))))

(defmacro defnative
  "A helper native regions can call. Its parameters need no hints: the helper
  is compiled once per distinct set of argument types it is called with, as
  Carp concretizes a generic function, so (defnative dot [xs ys] ...) works on
  double arrays in one region and long arrays in another. An array argument is
  borrowed unless the body gives it away (returns it, or passes it to a helper
  that takes it); then the helper takes ownership and the caller's binding is
  moved, which the compiler infers and names in its error. A helper can create
  and return arrays of its own. A helper that calls itself has its
  result type inferred from its base case; a return hint, (defnative ^double f
  [x] ...), is optional and checked.

  Outside a region, name is an ordinary fn over the same body, with Jolt's own
  semantics."
  [name params & body]
  (let [helper {:params (mapv #(with-meta % nil) params)
                :body (vec body)
                :ns (ns-name *ns*)
                :ret (:tag (meta name))}]
    `(def ~(vary-meta name assoc :jolt.native/helper (list 'quote helper))
       (fn ~(mapv #(with-meta % nil) params) ~@body))))

(def ^:private extern-types {'long :long 'double :double 'boolean :bool})
(def ^:private ffi-types {:long :int64 :double :double :bool :bool :unit :void})

(defn- extern-type [sym what]
  (let [tag (:tag (meta sym)) tag (if (string? tag) (symbol tag) tag)]
    (or (extern-types tag)
        (throw (ex-info (str "defextern: " what " needs a hint of ^long ^double or ^boolean, got "
                             (pr-str tag))
                        {:jolt.native/error true})))))

(defmacro defextern
  "Declare a C function native regions can call:

      (defextern ^double fast-erf [^double x])
      (defextern ^long next-seed [^long s] {:c \"xs_next\" :header \"xorshift.h\"})

  Parameters and the result are scalars (^long ^double ^boolean; no result
  hint is a void function, answering nil). The region's C calls the function
  directly: it is declared from these hints, or by its :header when one is
  given (as Ferret's native-header does). A jolt build links it like any other
  symbol, so it must come from a :static :jolt/native library (or libc/libm).
  Outside a region, and in a region's Jolt fallback, name is a jolt.ffi
  binding of the same function, so both halves call the same code.

  The checker cannot see into the C: a region that calls one is memory safe
  only as far as that function is."
  [name params & [opts]]
  (let [c (or (:c opts) (str name))
        header (:header opts)
        ret (if (:tag (meta name)) (extern-type name (str "the result of " name)) :unit)
        ps (mapv #(extern-type % (str "parameter " %)) params)]
    (when-not (re-matches #"[A-Za-z_][A-Za-z0-9_]*" c)
      (throw (ex-info (str "defextern: " (pr-str c) " is not a C identifier; name it with {:c \"...\"}")
                      {:jolt.native/error true})))
    (when (and header (not (re-matches #"<?[A-Za-z0-9_./+-]+>?" header)))
      (throw (ex-info (str "defextern: bad header " (pr-str header)) {:jolt.native/error true})))
    `(jolt.ffi/defcfn ~(vary-meta name assoc :jolt.native/extern
                                  (list 'quote {:c c :header header :params ps :ret ret}))
       ~c ~(mapv ffi-types ps) ~(ffi-types ret))))

(defn linked-regions
  "Each native region whose call site has loaded in this process, as a map of
  its C symbol to whether that region runs as C here (true in a binary built
  with jolt build, false in the REPL or under jolt run)."
  []
  (into {} (jolt.scheme/call "jolt-native-linked")))
