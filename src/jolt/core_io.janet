# Jolt Core — I/O, files, JDBC, compare, type
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
# I/O — minimal wrappers
# ============================================================

# print/println use str semantics (bare strings); pr/prn use readable (quoted).
# All space-separate their args, like Clojure.
# print/println live in the Clojure collection tier (core/20-coll.clj) over
# the __write / __pr-str1 host seams; str-render-one stays for core-str.
(defn core-write [s] (prin s) nil)

(defn core-eprint [s] (eprint s) nil)
(defn core-eprintf [fmt & args] (eprintf fmt ;args) nil)

# next.jdbc host shims (the db library's next.jdbc compat layer builds on these).
# A connection is a tagged table wrapping a jdbc.core conn (:raw) plus clj
# callbacks: :exec runs one SQL string in the transaction (so the janet-side
# Statement.executeBatch can run SQL without a janet->clj call), :close closes
# the underlying conn, :product is the JDBC database product name. instance?
# Connection (evaluator) and the :jolt/jdbc-conn tagged methods (host_interop) key
# off this shape.
(defn core-jdbc-wrap-conn [raw exec closef product]
  @{:jolt/type :jolt/jdbc-conn :raw raw :exec exec :close closef
    :product product :closed @[false]})
# Robust unwrap: a wrapped conn -> its :raw; anything else passes through (so the
# next.jdbc fns accept either a wrapped conn or a bare jdbc.core conn/spec).
(defn core-jdbc-conn-raw [x]
  (if (and (table? x) (= :jolt/jdbc-conn (get x :jolt/type))) (get x :raw) x))
(defn core-jdbc-make-stmt [w]
  # :close lets with-open close the statement (core-close-resource calls :close);
  # nothing to release — executeBatch already ran the commands.
  @{:jolt/type :jolt/jdbc-stmt :exec (get w :exec) :cmds @[] :close (fn [] nil)})

# java.io.File model (jolt-hjw). io/file and (File. …) build a tagged :jolt/file
# value so (instance? File x) works and migratus's File-vs-jar branching takes
# the filesystem path. The File method surface + nio glob live in host_interop; here
# are the constructor/predicate builtins and the path coercion str/slurp use.
(defn core-file-path
  "The path string of a :jolt/file, or (string x) for anything else."
  [x]
  (if (and (table? x) (= :jolt/file (get x :jolt/type))) (get x :path) (string x)))
(defn core-make-file [path &opt child]
  (def base (core-file-path path))
  @{:jolt/type :jolt/file :path (if child (string base "/" (core-file-path child)) base)})
(defn core-file? [x] (and (table? x) (= :jolt/file (get x :jolt/type))))

# newline lives in the Clojure collection tier (core/20-coll.clj).

# Clojure 1.11 string->scalar parsers: nil on malformed input, throw on a
# non-string. Validation is strict (scan-number alone accepts 0x10 etc.).
(defn- parse-arg-str [s who]
  (if (or (string? s) (buffer? s)) (string s)
    (error (string who " requires a string, got " (type s)))))

(defn core-parse-long [s]
  (def str* (parse-arg-str s "parse-long"))
  (def n (length str*))
  (def start (if (and (> n 0) (or (= 43 (in str* 0)) (= 45 (in str* 0)))) 1 0))
  (if (and (> n start)
           (do (var ok true)
               (for i start n (when (or (< (in str* i) 48) (> (in str* i) 57)) (set ok false)))
               ok))
    (scan-number str*)
    nil))

(defn core-parse-double [s]
  (def str* (parse-arg-str s "parse-double"))
  # strict float shape: [+-] digits [. digits] [eE [+-] digits] — at least one
  # digit overall; "Infinity"/"-Infinity"/"NaN" accepted like the reference.
  (cond
    (= str* "Infinity") math/inf
    (= str* "-Infinity") (- math/inf)
    (= str* "NaN") math/nan
    (do
      (def pat (peg/compile ~(sequence (opt (set "+-")) (choice (sequence (some :d) (opt (sequence "." (any :d)))) (sequence "." (some :d))) (opt (sequence (set "eE") (opt (set "+-")) (some :d))) -1)))
      (if (peg/match pat str*) (scan-number str*) nil))))

# parse-boolean lives in the Clojure collection tier (core/20-coll.clj).

# Host time source for the `time` macro (monotonic, milliseconds).
(defn core-current-time-ms [] (* 1000 (os/clock :monotonic)))

# Host IO (host-classified in the spec): path-based slurp/spit, *out* flush.
# Opts (:encoding ...) are accepted and ignored — everything is UTF-8 here.
# Reader shims (java.io.StringReader / PushbackReader / anything carrying
# :s + :pos) DRAIN instead of opening a file: Ring middleware slurps request
# bodies, and the jolt Ring adapter hands those over as StringReaders.
(defn core-slurp [src & opts]
  (cond
    (core-file? src) (string (slurp (core-file-path src)))
    (and (table? src) (string? (get src :s)) (number? (get src :pos)))
      (let [s (src :s) p (src :pos)]
        (put src :pos (length s))
        (string/slice s p))
    (string (slurp src))))

(defn core-spit [path content & opts]
  (def append? (do (var a false) (var i 0)
                   (while (< i (length opts))
                     (when (and (= :append (in opts i)) (in opts (+ i 1))) (set a true))
                     (+= i 2))
                   a))
  (def f (file/open (core-file-path path) (if append? :a :w)))
  (file/write f (str-render-one content))
  (file/close f)
  nil)

(defn core-flush []
  (def out (dyn :out))
  (when out (file/flush out))
  nil)

# Thread-binding introspection over the frame stack (types/cur-binding-stack).
(defn core-get-thread-bindings []
  # Innermost frame wins: merge frames oldest-first. The result is a Janet
  # STRUCT keyed by the var tables themselves — the exact frame representation
  # var-get reads (identity-keyed get) — so the map can be re-pushed by
  # with-bindings*/bound-fn* and remains lookup-able with (get m the-var).
  (def acc @{})
  (each frame (snapshot-bindings)
    (each entry (realize-for-iteration frame)
      (put acc (in entry 0) (in entry 1))))
  (table/to-struct acc))

(defn core-thread-bound?* [v]
  (var found false)
  (each frame (snapshot-bindings)
    (each entry (realize-for-iteration frame)
      (when (= (in entry 0) v) (set found true))))
  found)

# Directory primitives for file-seq (paths, not File objects — host-classified).
(defn core-dir? [path]
  (def st (os/stat path))
  (and st (= :directory (st :mode))))

(defn core-list-dir [path]
  (def entries (os/dir path))
  (map (fn [e] (string path "/" e)) (sort entries)))

# Clojure compare: a total order over comparable values. nil sorts first;
# numbers numerically; strings/keywords lexically; symbols by ns then name;
# booleans false<true; chars by codepoint; vectors by length then elementwise;
# uuids by canonical string; insts by epoch ms. Cross-type comparison throws
# (like Clojure's ClassCastException).
(var core-compare nil)
(set core-compare (fn ccompare [a b]
  (defn cmp3 [x y] (cond (< x y) -1 (> x y) 1 0))
  (cond
    (and (nil? a) (nil? b)) 0
    (nil? a) -1
    (nil? b) 1
    (and (number? a) (number? b)) (cmp3 a b)
    (and (or (string? a) (buffer? a)) (or (string? b) (buffer? b)))
      (cmp3 (string a) (string b))
    (and (keyword? a) (keyword? b)) (cmp3 (string a) (string b))
    (and (core-symbol? a) (core-symbol? b))
      (let [r (cmp3 (string (or (a :ns) "")) (string (or (b :ns) "")))]
        (if (= 0 r) (cmp3 (a :name) (b :name)) r))
    (and (boolean? a) (boolean? b))
      (cond (= a b) 0 (= a false) -1 1)
    (and (core-char? a) (core-char? b)) (cmp3 (a :ch) (b :ch))
    (and (struct? a) (= :jolt/uuid (get a :jolt/type))
         (struct? b) (= :jolt/uuid (get b :jolt/type)))
      (cmp3 (a :str) (b :str))
    (and (struct? a) (= :jolt/inst (get a :jolt/type))
         (struct? b) (= :jolt/inst (get b :jolt/type)))
      (cmp3 (a :ms) (b :ms))
    (and (jvec? a) (jvec? b))
      (let [la (vcount a) lb (vcount b)]
        (if (not= la lb)
          (cmp3 la lb)
          (do
            (var r 0) (var i 0)
            (while (and (= r 0) (< i la))
              (set r (ccompare (vnth a i) (vnth b i)))
              (++ i))
            r)))
    (error (string "Cannot compare " (type a) " with " (type b))))))

# Clojure type: the :type metadata when present, else the value's type. With no
# class objects on this host, the "class" is a symbol: a deftype/record value
# yields its type tag symbol; everything else a taxonomy keyword
# (host-classified — see spec coverage).
(defn core-type [x]
  (def m (core-meta x))
  (def override (and m (core-get m :type)))
  (if (not (nil? override))
    override
    (cond
      (and (table? x) (get x :jolt/deftype))
        {:jolt/type :symbol :ns nil :name (get x :jolt/deftype)}
      (nil? x) nil
      (boolean? x) :boolean
      (number? x) :number
      (or (string? x) (buffer? x)) :string
      (keyword? x) :keyword
      (core-symbol? x) :symbol
      (core-char? x) :char
      (and (struct? x) (get x :jolt/type)) (get x :jolt/type)
      (jvec? x) :vector
      (core-map? x) :map
      (set? x) :set
      (core-seq? x) :seq
      (or (function? x) (cfunction? x)) :fn
      (table? x) (or (get x :jolt/type) :table)
      :else (keyword (type x)))))

# Capture *out*: run thunk with Janet's :out dynamic bound to a buffer, so all
# print/println/pr/prn output (which go through `prin` -> (dyn :out)) is collected
# and returned as a string. The with-out-str macro (overlay) wraps a body thunk.
(defn core-with-out-str [thunk]
  (def buf @"")
  (with-dyns [:out buf] (thunk))
  (string buf))

# pr/prn/pr-str live in the Clojure collection tier (core/20-coll.clj); the
# renderer itself stays host (representation-coupled, shared with hot str).
(defn core-pr-str1 [x] (let [b @""] (pr-render b x) (string b)))

# ============================================================
