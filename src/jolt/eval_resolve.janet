# Jolt Evaluator — symbol/var resolution, params, destructuring, class lookup
# Extracted from evaluator.janet (jolt-oudv, phase 2a split).

(use ./types)
(use ./phm)
(use ./phs)
(use ./lazyseq)
(use ./pv)
(use ./plist)
(use ./config)
(use ./reader)
(use ./regex)
(use ./eval_base)
(register-tagged-methods! :jolt/iterator
  @{"hasNext" (fn [self] (< (self :pos) (length (self :items))))
    "next"    (fn [self]
                (def x (in (self :items) (self :pos)))
                (put self :pos (+ 1 (self :pos)))
                x)})
# Class names evaluate to their CANONICAL NAME STRING — the same value
# core-class returns — so (defmethod m String ...) keys match a
# (defmulti m (comp class :body)) dispatch (ring.util.request does this).
# `new` resolves the actual constructor from class-ctors by short name.
(def class-canonical-names
  @{"String" "java.lang.String" "Number" "java.lang.Number"
    "Boolean" "java.lang.Boolean" "Long" "java.lang.Long"
    "Integer" "java.lang.Integer" "Double" "java.lang.Double"
    "InputStream" "java.io.InputStream" "OutputStream" "java.io.OutputStream"
    "File" "java.io.File" "Reader" "java.io.Reader" "Writer" "java.io.Writer"
    "ISeq" "clojure.lang.ISeq" "Keyword" "clojure.lang.Keyword"
    "Symbol" "clojure.lang.Symbol" "MapEntry" "clojure.lang.MapEntry"
    "StringReader" "java.io.StringReader" "StringWriter" "java.io.StringWriter"
    "StringBuilder" "java.lang.StringBuilder"
    "StringTokenizer" "java.util.StringTokenizer"
    "Charset" "java.nio.charset.Charset" "Base64" "java.util.Base64"
    "Exception" "java.lang.Exception"
    "IllegalArgumentException" "java.lang.IllegalArgumentException"
    "InterruptedException" "java.lang.InterruptedException"
    "Throwable" "java.lang.Throwable"})
# A class used as a VALUE should evaluate to what (clojure.core/type instance)
# returns for its instances, so a registry keyed by class (e.g. malli's
# class-schemas) matches a value's (type ...). For jolt's native tagged types the
# class maps to its :jolt/type keyword — Pattern <-> a compiled regex.
(def- class-value-overrides
  @{"Pattern" :jolt/regex "java.util.regex.Pattern" :jolt/regex})
(defn class-value-for
  "The value a class-name symbol evaluates to: a type override, else its canonical
  name string."
  [nm]
  (or (get class-value-overrides nm)
      (get class-canonical-names nm)
      # qualified already, or unknown: the name itself is the token
      nm))
(defn ctor-for-class-token
  "Constructor fn for a class token (a canonical-name string): try the full
  name, then the short name after the last dot."
  [tok]
  (or (in class-ctors tok)
      (let [parts (string/split "." tok)]
        (in class-ctors (last parts)))))

# java.lang.String method surface for clj-compat interop: (.toLowerCase s),
# (.indexOf s x), ... — the methods portable cljc libraries actually call.
# Case mapping is ASCII (the whole engine is byte-based); indexOf returns -1
# on miss, as on the JVM.
(defn- str-needle [x]
  (cond
    (and (struct? x) (= :jolt/char (get x :jolt/type))) (string/from-bytes (x :ch))
    # (.indexOf s 61): an int needle is a char CODE on the JVM, not its decimal
    # text (ring-codec splits k=v pairs this way)
    (number? x) (string/from-bytes (math/trunc x))
    (string x)))
# java.lang.Number surface (ring-codec: (.byteValue (Integer/valueOf s 16))).
(def number-methods
  {"byteValue"   (fn [n] (let [b (band (math/trunc n) 0xff)] (if (> b 127) (- b 256) b)))
   "shortValue"  (fn [n] (let [v (band (math/trunc n) 0xffff)] (if (> v 32767) (- v 65536) v)))
   "intValue"    (fn [n] (math/trunc n))
   "longValue"   (fn [n] (math/trunc n))
   "floatValue"  (fn [n] (* 1.0 n))
   "doubleValue" (fn [n] (* 1.0 n))
   "toString"    (fn [n &opt radix] (if (= radix 16) (string/format "%x" (math/trunc n)) (string n)))})

# Universal java.lang.Object / exception / persistent-collection methods that
# reitit's :clj branches call on non-string targets: (.getMessage e),
# (.assoc m k v), (.get m k). Consulted in the method-dispatch fallthrough.
(def object-methods
  {"getMessage"  (fn [e] (cond (and (table? e) (= :jolt/ex-info (get e :jolt/type))) (get e :message)
                               (string? e) e
                               (string e)))
   "getCause"    (fn [e] (and (table? e) (get e :cause)))
   "toString"    (fn [x] (string x))
   "equals"      (fn [a b] (deep= a b))
   "hashCode"    (fn [x] (hash x))
   # (.iterator coll) -> a jolt iterator (see :jolt/iterator above). Materializes
   # the collection to an indexable array via the late-bound core realizer.
   "iterator"    (fn [coll] @{:jolt/type :jolt/iterator :pos 0
                              :items (if coll-realizer (coll-realizer coll) @[])})})

(def string-methods
  {"getBytes"    (fn [s &opt charset] (buffer s))
   "toString"    (fn [s] s)
   "toLowerCase" (fn [s] (string/ascii-lower s))
   "toUpperCase" (fn [s] (string/ascii-upper s))
   "trim"        (fn [s] (string/trim s))
   "intern"      (fn [s] s)
   # file-path surface: io/file returns plain path strings, so the java.io.File
   # / java.net.URL methods selmer's template cache calls land here
   "toURI"       (fn [s] s)
   "toURL"       (fn [s] s)
   "getPath"     (fn [s] s)
   "getName"     (fn [s] (if-let [i (string/find "/" (string/reverse s))]
                           (string/slice s (- (length s) i)) s))
   "exists"      (fn [s] (not (nil? (os/stat s))))
   "lastModified" (fn [s] (if-let [st (os/stat s)] (math/floor (* 1000 (st :modified))) 0))
   # JVM String.split takes a REGEX string; trailing empties dropped like the JVM
   "split"       (fn [s re &opt limit]
                   (def parts (re-split (re-pattern re) s))
                   (while (and (> (length parts) 0) (= "" (last parts)))
                     (array/pop parts))
                   parts)
   "length"      (fn [s] (length s))
   "isEmpty"     (fn [s] (= 0 (length s)))
   "charAt"      (fn [s i] {:jolt/type :jolt/char :ch (s i)})
   "codePointAt" (fn [s i] (s i))
   "indexOf"     (fn [s x &opt from] (or (string/find (str-needle x) s (or from 0)) -1))
   "lastIndexOf" (fn [s x]
                   (let [n (str-needle x)]
                     (var found -1) (var i 0)
                     (while (< i (length s))
                       (let [f (string/find n s i)]
                         (if f (do (set found f) (set i (+ f 1))) (set i (length s)))))
                     found))
   "substring"   (fn [s start &opt end] (string/slice s start end))
   "startsWith"  (fn [s p] (string/has-prefix? p s))
   "endsWith"    (fn [s p] (string/has-suffix? p s))
   "contains"    (fn [s sub] (not (nil? (string/find (str-needle sub) s))))
   "concat"      (fn [s o] (string s o))
    "replace"     (fn [s a b] (string/replace-all (str-needle a) (str-needle b) s))
    "replaceAll"  (fn [s regex replacement] (re-replace-all (re-pattern regex) s replacement))
    "replaceFirst" (fn [s regex replacement] (re-replace-first (re-pattern regex) s replacement))
    "matches"     (fn [s regex] (not (nil? (re-matches (re-pattern regex) s))))
   "compareTo"   (fn [s o] (cond (< s o) -1 (> s o) 1 0))
   "equalsIgnoreCase" (fn [s o] (= (string/ascii-lower s) (string/ascii-lower (string o))))})

(defn resolve-sym
  [ctx bindings sym-s]
  (let [name (sym-s :name) ns (sym-s :ns)]
    # Math/Thread/System/Long and every other class resolve through the generic
    # class-statics registry (host_interop registers them at load); no special-case.
    (if (get class-statics ns)
      (let [v (get (get class-statics ns) name)]
        (if (nil? v) (error (string "Unsupported member: " ns "/" name)) v))
    (if (not (nil? ns))
      (let [current-ns (ctx-find-ns ctx (ctx-current-ns ctx))
            aliased-ns (or (ns-alias-lookup current-ns ns) (ns-import-lookup current-ns ns))
            target-ns (ctx-find-ns ctx (or aliased-ns ns))
            v (and target-ns (ns-find target-ns name))]
        (if v (var-get v)
          # Explicit Janet interop. The `janet` namespace segment marks every
          # crossing into host code, where Clojure semantics no longer hold:
          #   janet/<name>          -> Janet root binding   (janet/slurp, janet/type)
          #   janet.<module>/<name> -> Janet module binding (janet.net/server,
          #                                                   janet.os/clock)
          # This makes the whole Janet stdlib reachable from Clojure while keeping
          # the interop boundary visible at the call site.
          (if (or (= ns "janet") (string/has-prefix? "janet." ns))
            (let [jname (if (= ns "janet") name (string (string/slice ns 6) "/" name))
                  # worker fibers may carry no env (fiber/new without :e inherit)
                  # — fall back to the env captured at module load
                  # four-step resolution: the runtime fiber's env (when it
                  # has one), the evaluator's module env (worker/connection
                  # fibers carry a foreign or empty env — net/server handler
                  # fibers resolve janet/struct through here), the autoload
                  # cache, then a jpm-module require on first miss
                  entry (or (when-let [fe (fiber/getenv (fiber/current))]
                              (in fe (symbol jname)))
                            (in module-load-env (symbol jname))
                            (in janet-bridge-extras jname)
                            (bridge-autoload jname))]
              (if (not (nil? entry))
                (if (table? entry) (entry :value) entry)
                (error (string "Unable to resolve Janet symbol: " jname))))
            # syntax-quote ns-qualifies bare class names inside macros
            # (selmer.util/StringBuilder); class names never belong to an ns —
            # fall back to the constructor / statics shims before giving up.
            (if (or (in class-ctors name) (get class-canonical-names name) (get class-value-overrides name))
              (class-value-for name)
              (error (string "Unable to resolve symbol: " ns "/" name))))))
      # Use :jolt/not-found sentinel to distinguish nil binding from absent binding
      (let [local (get bindings name :jolt/not-found-1)
            local (if (= local :jolt/not-found-1) (binding-get bindings name) local)]
        (if (not= local :jolt/not-found)
          (if (= local :jolt/nil) nil local)
          (let [current-ns (ctx-current-ns ctx) ns (ctx-find-ns ctx current-ns) v (ns-find ns name)]
            (if v (var-get v)
              # Check clojure.core as auto-referred fallback
              (let [core-ns (ctx-find-ns ctx "clojure.core")
                    core-v (ns-find core-ns name)]
                (if core-v
                  (var-get core-v)
                  # Try class-name resolution: Foo.Bar.Baz -> ns "Foo.Bar", name "Baz"
                  (let [dot-idx (string/find "." name)]
                    (if dot-idx
                      (let [last-dot (do
                                       (var idx dot-idx)
                                       (var next-dot (string/find "." name (+ idx 1)))
                                       (while (not (nil? next-dot))
                                         (set idx next-dot)
                                         (set next-dot (string/find "." name (+ idx 1))))
                                       idx)
                            class-ns (string/slice name 0 last-dot)
                            class-name (string/slice name (+ last-dot 1))]
                        (let [target-ns (ctx-find-ns ctx class-ns) tv (ns-find target-ns class-name)]
                          (if tv (var-get tv) tv)))
                      # No implicit Janet fallback (Stage 3): an unresolved
                      # Clojure symbol is an error. Host access is the explicit
                      # janet/ prefix above.
                      (if (or (in class-ctors name) (get class-canonical-names name) (get class-value-overrides name))
                        (class-value-for name)
                        (error (string "Unable to resolve symbol: " name " in this context")))))))))))))))
(defn- parse-arg-names
  "Parse a parameter vector, handling & rest args.
  Returns {:fixed [names...] :rest name-or-nil :all [names...]}"
  [args-form]
  (var fixed @[])
  (var rest-name nil)
  (var i 0)
  (while (< i (length args-form))
    (let [a (in args-form i)]
      (if (and (struct? a) (= :symbol (a :jolt/type)) (= "&" (a :name)))
        (do
          (+= i 1)
          (if (< i (length args-form))
            (do
              (set rest-name ((in args-form i) :name))
              (+= i 1))
            (error "& without argument in parameter list")))
        (do
          (if (and (struct? a) (= :symbol (a :jolt/type)))
            (array/push fixed (a :name))
            # destructuring form: recurse into it
            (when (indexed? a)
              (var di 0)
              (while (< di (length a))
                (def inner (in a di))
                (if (and (struct? inner) (= :symbol (inner :jolt/type)) (= "&" (inner :name)))
                  (do
                    (+= di 1)
                    (if (< di (length a))
                      (do
                        (set rest-name ((in a di) :name))
                        (+= di 1))
                      (error "& without argument in parameter list")))
                  (do
                    (if (and (struct? inner) (= :symbol (inner :jolt/type)))
                      (array/push fixed (inner :name))
                      # nested destructuring - extract names
                      (when (indexed? inner)
                        (each sym inner
                          (when (and (struct? sym) (= :symbol (sym :jolt/type)))
                            (array/push fixed (sym :name))))))
                    (+= di 1))))))
          (+= i 1)))))
  (var all @[])
  (each n fixed (array/push all n))
  (if rest-name (array/push all rest-name))
  {:fixed (tuple/slice (tuple ;fixed)) :rest rest-name :all (tuple/slice (tuple ;all))})

# ============================================================
# Destructuring (Clojure-compatible, recursive)
# ============================================================

(defn parse-params
  "Parse a parameter vector into raw patterns: {:fixed [pat...] :rest pat-or-nil}.
  Unlike parse-arg-names, patterns are kept intact (not flattened) so they can
  be destructured against the corresponding argument."
  [args-form]
  (var fixed @[])
  (var rest-pat nil)
  (var i 0)
  (while (< i (length args-form))
    (let [a (in args-form i)]
      (if (and (struct? a) (= :symbol (a :jolt/type)) (= "&" (a :name)))
        (do (+= i 1)
            (when (< i (length args-form)) (set rest-pat (in args-form i)))
            (+= i 1))
        (do (array/push fixed a) (+= i 1)))))
  {:fixed (tuple/slice (tuple ;fixed)) :rest rest-pat})

(defn rest-args-val
  "What a rest param binds to: nil when no args remain (Clojure semantics —
  (fn [& r]) called with nothing gives r = nil, never an empty seq)."
  [args i]
  (when (> (length args) i) (tuple/slice args i)))

(defn plain-sym? [p] (and (struct? p) (= :symbol (p :jolt/type))))

(defn require-symbol-params
  "fn* is a primitive: its params must be plain symbols. The fn/defn MACROS desugar
  destructuring into plain params + a body let before emitting fn*, so fn* never
  legitimately sees a pattern — matching Clojure, where (fn* [[a b]] ...) is the
  compile error 'fn params must be Symbols'. Enforcing it here keeps the interpreter
  consistent with the self-hosted analyzer (which also requires plain fn* params)
  and with Clojure, instead of leniently destructuring a form Clojure rejects."
  [param-info]
  (each p (param-info :fixed)
    (unless (plain-sym? p) (error "fn params must be Symbols")))
  (let [r (param-info :rest)]
    (when (and r (not (plain-sym? r))) (error "fn params must be Symbols"))))

(defn- d-get
  "Look up key k in a map-like value (phm/struct/table/nil)."
  [m k]
  (cond
    (phm? m) (phm-get m k)
    (or (struct? m) (table? m)) (get m k)
    true nil))

(defn- find-or-default
  "Find the :or default expression for binding name nm, or :jolt/none."
  [or-map nm]
  (var result :jolt/none)
  (when or-map
    (each k (keys or-map)
      (when (and (struct? k) (= :symbol (k :jolt/type)) (= nm (k :name)))
        (set result (get or-map k)))))
  result)

(var destructure-bind nil)
(set destructure-bind
  (fn dbind [ctx bindings pat val]
    (cond
      # plain symbol
      (and (struct? pat) (= :symbol (pat :jolt/type)))
        (bind-put bindings (pat :name) val)
      # sequential pattern (vector of sub-patterns)
      (indexed? pat)
        (let [rv (d-realize val)
              seqable? (indexed? rv)]
          (var di 0) (var vi 0)
          (def n (length pat))
          (while (< di n)
            (let [elem (in pat di)]
              (cond
                 # & rest
                 (and (struct? elem) (= :symbol (elem :jolt/type)) (= "&" (elem :name)))
                   (do
                     # rest binds a seq (jolt list = array), per Clojure semantics.
                     # For lazy-seqs, preserve laziness: walk vi steps via ls-rest
                     # instead of slicing the eagerly-realized array.
                     (destructure-bind ctx bindings (in pat (+ di 1))
                       (if (lazy-seq? val)
                         (do
                           (var c val) (var i 0)
                           (while (< i vi)
                             (let [nxt (ls-rest c)]
                               (if (nil? nxt) (break)
                                 (do (set c nxt) (++ i)))))
                           c)
                         (if (and seqable? (< vi (length rv)))
                           (array/slice (if (tuple? rv) (array/slice rv) rv) vi)
                           @[])))
                    (set di (+ di 2)))
                # :as whole
                (= elem :as)
                  (do
                    (destructure-bind ctx bindings (in pat (+ di 1)) val)
                    (set di (+ di 2)))
                # positional element
                true
                  (do
                    (destructure-bind ctx bindings elem
                      (if (and seqable? (< vi (length rv))) (in rv vi) nil))
                    (+= di 1) (+= vi 1))))))
      # map pattern (struct/table that isn't a symbol)
      (or (struct? pat) (table? pat))
        (let [rv (d-realize val)
              # Destructuring a sequential value as a map treats it as kwargs:
              # alternating k/v pairs, or a single trailing map (Clojure's
              # `[& {:keys ...}]`). A real map value is used as-is.
              mval (if (and (indexed? rv) (not (or (struct? rv) (table? rv))))
                     (if (and (= 1 (length rv))
                              (let [e (in rv 0)] (or (struct? e) (table? e) (phm? e))))
                       (in rv 0)
                       (let [m @{}]
                         (var i 0)
                         (while (< (+ i 1) (length rv))
                           (put m (in rv i) (in rv (+ i 1)))
                           (+= i 2))
                         m))
                     val)]
          (def or-map (get pat :or))
          (def as-sym (get pat :as))
          (when as-sym (destructure-bind ctx bindings as-sym mval))
          # :keys (keyword), :strs (string), :syms (symbol). A namespaced symbol
          # in :keys/:syms (x/y) looks up the namespaced key but binds local y.
          (each spec [[:keys :kw] [:strs :str] [:syms :sym]]
            (let [kw (in spec 0) kind (in spec 1) names (get pat kw)]
              (when (and names (indexed? names))
                (each s names
                  (let [sym? (and (struct? s) (= :symbol (s :jolt/type)))
                        local (if sym? (s :name) (string s))
                        nsp (and sym? (s :ns))
                        key (case kind
                              :kw (keyword (if nsp (string nsp "/" local) local))
                              :str local
                              :sym {:jolt/type :symbol :ns nsp :name local})
                        v (d-get mval key)
                        v (if (nil? v)
                            (let [d (find-or-default or-map local)]
                              (if (= d :jolt/none) nil (eval-form ctx bindings d)))
                            v)]
                    (bind-put bindings local v))))))
          # direct {local-pattern key-expr} entries (local may itself be a
          # nested vector/map pattern). Special keys are keywords; skip them.
          (each k (keys pat)
            (when (not (keyword? k))
              (let [key-val (eval-form ctx bindings (get pat k))
                    v (d-get mval key-val)]
                (if (and (struct? k) (= :symbol (k :jolt/type)))
                  # symbol target: apply :or default if missing
                  (let [nm (k :name)
                        v (if (nil? v)
                            (let [d (find-or-default or-map nm)]
                              (if (= d :jolt/none) nil (eval-form ctx bindings d)))
                            v)]
                    (bind-put bindings nm v))
                  # nested pattern target
                  (destructure-bind ctx bindings k v))))))
      true (error (string "Unsupported destructuring pattern: " (string/format "%q" pat))))))

# ---- host-type protocol extension (extend-protocol String/Number/... ) ----
(def host-type-names
  {"Long" true "Integer" true "Short" true "Byte" true "BigInteger" true "BigInt" true
   "Double" true "Float" true "Number" true "BigDecimal" true "Ratio" true
   "String" true "CharSequence" true "Boolean" true "Character" true
   "Keyword" true "Symbol" true "Object" true "IFn" true "Fn" true
   "PersistentVector" true "PersistentList" true "PersistentHashMap" true
   "PersistentHashSet" true "IPersistentMap" true "IPersistentVector" true
   "IPersistentSet" true "IPersistentCollection" true "ISeq" true "Atom" true "nil" true
   # java.util interfaces + seq types ring & friends extend on
   "Map" true "Set" true "List" true "Collection" true "LazySeq" true
   "APersistentMap" true})
