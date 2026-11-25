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
  and writes the caller's array in place, and cannot keep it), or ^bytes (a
  byte-array the region only reads, each element a signed byte as a long). A borrowed
  long-array holds fixnums, so storing a value outside -2^60..2^60-1 into one
  raises; widen it outside the region first. The body is a
  subset of Clojure: let, if, when, do, loop/recur, dotimes, and/or/not,
  arithmetic and comparisons, quot/rem/mod, bit-and/bit-or/bit-xor/bit-not and
  the three shifts, long/double, aget/aset/alength,
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

  A region always runs as native code, from the same C. jolt build compiles it
  with the C compiler and links it into the binary, so a build needs a C compiler
  for a program with regions. Anywhere else (the REPL, jolt run) jolt compiles
  the region in process with MIR (github.com/vnmakarov/mir) when its call site
  loads, so a hot loop runs at native speed while you tune it. Where there is
  no native path (the gambit target, a platform MIR does not support) calling a
  region raises UnsupportedOperationException."
  (:require [clojure.string :as str]
            [jolt.native.ast :as ast]
            [jolt.native.memory :as memory]
            [jolt.native.emit :as emit]
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
  (cond (= t [:ref :double]) :doubles (= t [:ref :long]) :longs (= t [:ref :byte]) :bytes
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
  "Run body as native code: linked into a jolt build binary, compiled by the
  JIT elsewhere. params names the locals it reads, each hinted ^long ^double
  ^boolean ^doubles or ^longs."
  [params & body]
  (let [{:keys [sym src region]} (compile-region params body)
        plain (mapv #(with-meta (:name %) nil) (:params region))]
    ;; (jolt.native/__region sym src [param-kinds] result-kind [args] raise! mode)
    ;; the analyzer derives the Chez foreign types from the kinds
    (list 'jolt.native/__region
          sym src
          (mapv (comp param-kind :type) (:params region))
          (result-kind (:type region))
          plain
          'jolt.native.rt/raise!
          (if (:hold-gc (meta params)) :hold-gc :pinned))))

(defn- on-form? [x] (and (seq? x) (= 'on (first x))))

(defn- on-variants
  "A helper's (on guard body...) forms as [{:guard g :body [...]}], or nil when
  it has a plain body. A guard is a C preprocessor condition, or :default."
  [name body]
  (when (some on-form? body)
    (when-not (every? on-form? body)
      (throw (ex-info (str "defnative " name ": with (on ...) bodies, every form must be one")
                      {:jolt.native/error true})))
    (let [vs (mapv (fn [[_ guard & forms]]
                     (when-not (or (= :default guard)
                                   (and (string? guard) (not (str/blank? guard))
                                        (not (re-find #"[\r\n]" guard))))
                       (throw (ex-info (str "defnative " name ": a guard is a one-line C condition"
                                            " string or :default, got " (pr-str guard))
                                       {:jolt.native/error true})))
                     (when (empty? forms)
                       (throw (ex-info (str "defnative " name ": (on " (pr-str guard) ") has no body")
                                       {:jolt.native/error true})))
                     {:guard guard :body (vec forms)})
                   body)]
      (when (< 1 (count (filter #(= :default (:guard %)) vs)))
        (throw (ex-info (str "defnative " name ": more than one (on :default ...)")
                        {:jolt.native/error true})))
      vs)))

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

  A helper can give a body per platform, chosen by the C compiler, as Ferret's
  defnative does:

      (defnative sum [xs]
        (on \"defined(__aarch64__)\" ...)
        (on :default ...))

  Each guard is a C preprocessor condition; the first that holds is compiled,
  and a platform no guard covers is a build error unless there is an
  (on :default ...). The bodies must agree on their result type and on which
  arguments they take.

  Outside a region, name is an ordinary fn over the same body (the :default
  one, or the first), with Jolt's own semantics."
  [name params & body]
  (let [variants (on-variants name body)
        default (or (first (filter #(= :default (:guard %)) variants)) (first variants))
        helper (cond-> {:params (mapv #(with-meta % nil) params)
                        :body (if variants (:body default) (vec body))
                        :ns (ns-name *ns*)
                        :ret (:tag (meta name))}
                 variants (assoc :variants variants))]
    `(def ~(vary-meta name assoc :jolt.native/helper (list 'quote helper))
       (fn ~(mapv #(with-meta % nil) params) ~@(:body helper)))))

(def ^:private extern-types {'long :long 'double :double 'boolean :bool})
(def ^:private extern-arrays {'doubles [:array :double] 'longs [:array :long]})
(def ^:private ffi-types {:long :int64 :double :double :bool :bool :unit :void})

(defn- extern-type [sym what arrays?]
  (let [tag (:tag (meta sym)) tag (if (string? tag) (symbol tag) tag)]
    (or (extern-types tag)
        (and arrays? (extern-arrays tag))
        (throw (ex-info (str "defextern: " what " needs a hint of ^long ^double or ^boolean"
                             (when arrays? ", or ^doubles ^longs") ", got " (pr-str tag))
                        {:jolt.native/error true})))))

(defn extern-call
  "Call raw, a jolt.ffi binding whose array parameters each take a pointer
  and a length, with jolt arrays: each array is copied into foreign memory for
  the call and copied back after it, so what the C wrote shows, as it does in
  a region, where the C gets the array's own storage. kinds says which
  arguments are arrays (:doubles, :longs) and which are scalars (nil)."
  [raw kinds args]
  (let [bufs (mapv (fn [k a]
                     (when k
                       (let [n (alength a) t (if (= k :doubles) :double :int64)
                             p (jolt.ffi/alloc (* 8 (max 1 n)))]
                         (jolt.ffi/write-array p t a)
                         {:p p :n n :t t :arr a})))
                   kinds args)
        cargs (mapcat (fn [b a] (if b [(:p b) (:n b)] [a])) bufs args)]
    (try
      (apply raw cargs)
      (finally
        (doseq [{:keys [p n t arr]} (remove nil? bufs)]
          (let [back (jolt.ffi/read-array p t n)]
            (dotimes [i n] (aset arr i (aget back i))))
          (jolt.ffi/free p))))))

(defmacro defextern
  "Declare a C function native regions can call:

      (defextern ^double fast-erf [^double x])
      (defextern ^long next-seed [^long s] {:c \"xs_next\" :header \"xorshift.h\"})
      (defextern scale! [^doubles xs ^double k])   ; C: void scale_(double *, int64_t, double)

  Scalars are ^long ^double ^boolean; no result hint is a void function,
  answering nil. An array parameter (^doubles, ^longs) reaches C as a pointer
  to its elements and their count (int64_t), for the length of the call: the
  function may read and write them but must not keep the pointer. A region
  passes an array's own storage; a borrowed ^longs (stored as fixnums) has to
  be passed as (aclone xs).

  The region's C calls the function directly: it is declared from these hints,
  or by its :header when one is given (as Ferret's native-header does). A jolt
  build links it like any other symbol, so it must come from a :static
  :jolt/native library (or libc/libm); the JIT finds it among the libraries
  this process has loaded. Outside a region, name calls the same function
  through jolt.ffi, copying arrays in and back out.

  Outside a build the JIT reads a :header itself, with MIR's C compiler,
  which cannot parse every system's headers (Android's bionic ones among
  them); a region whose header it cannot read raises there. A header that
  only declares the function can be swapped for the hinted prototype.

  The C name is the symbol's with each character C does not allow as _
  (fast-erf is fast_erf), unless :c names it. Without a :header the
  prototype comes from the hints, so a function libc already declares
  (abs, labs) needs its own :header or a wrapper of another name: the
  hinted prototype would conflict with the one the region's C includes.

  The same array passed twice reaches C as one pointer from a region, but
  name called outside one copies each argument into its own buffer: a
  function that reads one while it writes the other sees different values in
  the two, so pass distinct arrays.

  The checker cannot see into the C: a region that calls one is memory safe
  only as far as that function is."
  [name params & [opts]]
  (let [c (or (:c opts) (str/replace (str name) #"[^A-Za-z0-9_]" "_"))
        header (:header opts)
        ret (if (:tag (meta name)) (extern-type name (str "the result of " name) false) :unit)
        ps (mapv #(extern-type % (str "parameter " %) true) params)
        kinds (mapv #(case % [:array :double] :doubles [:array :long] :longs nil) ps)
        info (list 'quote {:c c :header header :params ps :ret ret})]
    (when-not (re-matches #"[A-Za-z_][A-Za-z0-9_]*" c)
      (throw (ex-info (str "defextern: " (pr-str c) " is not a C identifier; name it with {:c \"...\"}")
                      {:jolt.native/error true})))
    (when (and header (not (re-matches #"<?[A-Za-z0-9_./+-]+>?" header)))
      (throw (ex-info (str "defextern: bad header " (pr-str header)) {:jolt.native/error true})))
    (if (some some? kinds)
      (let [raw (symbol (str name "__c"))
            args (mapv #(with-meta (symbol (str "a" %)) nil) (range (count ps)))]
        `(do (jolt.ffi/defcfn ~(vary-meta raw assoc :private true) ~c
               ~(vec (mapcat #(if (vector? %) [:pointer :int64] [(ffi-types %)]) ps))
               ~(ffi-types ret))
             (def ~(vary-meta name assoc :jolt.native/extern info)
               (fn ~args (extern-call ~raw ~kinds ~args)))))
      `(jolt.ffi/defcfn ~(vary-meta name assoc :jolt.native/extern info)
         ~c ~(mapv ffi-types ps) ~(ffi-types ret)))))

(defn jit?
  "Whether this process can compile a native region itself: true in jolt (the
  REPL, jolt run) on a machine MIR supports; false in a binary jolt build made,
  which runs only the regions linked into it."
  []
  (boolean (jolt.scheme/call "jolt-native-jit?")))

(defn native-fn
  "Compile a native region at runtime, from data: params is the vector native
  takes (hinted symbols) and body a seq of forms. Answers a fn of the params
  that runs the region as native code, compiled by the JIT before native-fn
  returns, so a program can generate a region from what it learns at runtime
  (a schema, a record layout, a size) and run it with no interpretation left.
  A region's compile error throws here. Names in body (defnative helpers,
  defexterns, macros) resolve in ns, by default the current namespace.

      (def sq (native-fn '[^long n] '((* n n))))
      (sq 7) ;=> 49

  Needs the JIT (jit?): a binary jolt build made has none, and raises
  UnsupportedOperationException."
  ([params body] (native-fn params body *ns*))
  ([params body ns]
   (when-not (jit?)
     (throw (UnsupportedOperationException.
             "jolt.native/native-fn: this process has no JIT (a jolt build binary runs only the regions linked into it)")))
   (binding [*ns* (the-ns ns)]
     ;; checked first, so a bad program throws the region's own error; in ns,
     ;; where its helpers and macros resolve
     (compile-region params body)
     (eval (list 'fn* (mapv #(with-meta % nil) params)
                 (list* 'jolt.native/native params body))))))

(defn linked-regions
  "Each native region whose call site has loaded in this process, as a map of
  its C symbol to how it runs here: :linked (compiled into a binary jolt build
  made), :jit (compiled in this process, in the REPL or under jolt run), or
  nil (no native path here; calling it raises)."
  []
  (into {} (map (fn [[sym how]] [sym (some-> how keyword)]))
        (jolt.scheme/call "jolt-native-linked")))
