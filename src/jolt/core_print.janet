# Jolt Core — string + print rendering (pr-str/str)
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
# String functions
# ============================================================

# Readable rendering of a value (Clojure pr semantics): strings quoted,
# keywords with leading ':', symbols by name, collections with their reader
# syntax. Used by both pr-str (readable) and str (collection elements).
# A namespace's :name may be a string or a symbol struct depending on the
# creation path — normalize for display.
(defn- ns-display-name [ns]
  (def n (ns :name))
  (if (and (struct? n) (= :symbol (get n :jolt/type))) (n :name) (string n)))

# print-method callback (jolt-g1r): set by api/init AFTER the overlay loads,
# to a (fn [v emit] handled?) that looks for a USER-registered print-method
# multimethod entry for v's dispatch value and renders through it (emit takes
# string pieces). The renderer consults it only on the record/tagged
# fallthrough, so built-in rendering pays nothing.
(var print-method-cb nil)
(defn set-print-method-cb! [f] (set print-method-cb f))

# Late-bound hook to a record's custom Object/toString (jolt-rt6n). Returns the
# string a deftype's (toString [_] ...) produces, or nil when the type defines
# none. core can't reach the ctx type-registry directly, so install-print-method-cb!
# wires this per-ctx. str routes records through it; the data repr is the fallback.
(var record-tostring-cb nil)
(defn set-record-tostring-cb! [f] (set record-tostring-cb f))

(def- pr-char-escapes
  {34 "\\\"" 92 "\\\\" 10 "\\n" 9 "\\t" 13 "\\r" 12 "\\f" 8 "\\b"})
(var pr-render nil)

# Format a number the way Clojure prints it: infinity and NaN have named forms
# (Janet renders them "inf"/"-inf"/"nan").
(defn- fmt-number [v]
  (cond
    (not (number? v)) (string v)
    (= v math/inf) "Infinity"
    (= v (- math/inf)) "-Infinity"
    (not= v v) "NaN"
    (string v)))

(defn- pr-render-seq [buf items open close]
  (buffer/push-string buf open)
  (var first true)
  (each x items
    (if first (set first false) (buffer/push-string buf " "))
    (pr-render buf x))
  (buffer/push-string buf close))

(defn- pr-render-pairs [buf pairs]
  (buffer/push-string buf "{")
  (var first true)
  (each pair pairs
    (if first (set first false) (buffer/push-string buf ", "))
    (pr-render buf (in pair 0))
    (buffer/push-string buf " ")
    (pr-render buf (in pair 1)))
  (buffer/push-string buf "}"))

(defn- name-of
  "Extract a plain name string from a string, symbol struct, or a namespace/var
  table (reading its :name) — never recurses into the cyclic ns structure."
  [x]
  (cond
    (nil? x) nil
    (string? x) x
    (and (struct? x) (= :symbol (get x :jolt/type))) (x :name)
    (or (struct? x) (table? x)) (name-of (get x :name))
    (string x)))

(defn- var-display
  "Render a Jolt var as #'ns/name. A var's :meta/:ns refs are cyclic, so this
  reads only its :name and :ns name — printing the var's pairs would loop."
  [v]
  (let [nm (name-of (v :name))
        ns (name-of (v :ns))]
    (if ns (string "#'" ns "/" nm) (string "#'" nm))))

(defn- pr-push-escaped
  "Readable string body: escape per char-escapes (quote, backslash, \\n & co),
  so pr-str round-trips through the reader (this was unescaped, jolt pre-r6)."
  [buf s]
  (each c (string/bytes s)
    (if-let [esc (get pr-char-escapes c)]
      (buffer/push-string buf esc)
      (buffer/push-byte buf c))))

(set pr-render
  (fn [buf v]
    (cond
      (nil? v) (buffer/push-string buf "nil")
      (= true v) (buffer/push-string buf "true")
      (= false v) (buffer/push-string buf "false")
      (string? v) (do (buffer/push-string buf "\"") (pr-push-escaped buf v) (buffer/push-string buf "\""))
      (buffer? v) (do (buffer/push-string buf "\"") (pr-push-escaped buf (string v)) (buffer/push-string buf "\""))
      (keyword? v) (do (buffer/push-string buf ":") (buffer/push-string buf (string v)))
      (core-char? v) (do (buffer/push-string buf "\\")
                         (buffer/push-string buf
                           (case (v :ch)
                             10 "newline" 32 "space" 9 "tab" 13 "return"
                             12 "formfeed" 8 "backspace" 0 "nul"
                             (char->string v))))
      (number? v) (buffer/push-string buf (fmt-number v))
      (and (struct? v) (= :symbol (v :jolt/type)))
        (buffer/push-string buf (if (v :ns) (string (v :ns) "/" (v :name)) (v :name)))
      (and (struct? v) (= :jolt/inst (v :jolt/type)))
        (do (buffer/push-string buf "#inst \"") (buffer/push-string buf (inst->rfc3339 v))
            (buffer/push-string buf "\""))
      (= :jolt/namespace (get v :jolt/type))
        (do (buffer/push-string buf "#namespace[")
            (buffer/push-string buf (ns-display-name v))
            (buffer/push-string buf "]"))
      (and (table? v) (= :jolt/var (get v :jolt/type))) (buffer/push-string buf (var-display v))
      (shape-rec? v)
        (let [rtag (record-tag v)
              pairs (map (fn [k] [k (shape-get v k nil)]) (shape-keys v))]
          (cond
            # a record shape-rec prints Clojure-style: #ns.Type{:k v, ...}
            (and rtag print-method-cb (print-method-cb v (fn [piece] (buffer/push-string buf piece)))) nil
            rtag (do (buffer/push-string buf (string "#" rtag))
                     (pr-render-pairs buf pairs))
            (pr-render-pairs buf pairs)))
      (core-sorted-map? v) (pr-render-pairs buf
                             (map (fn [e] [(vnth e 0) (vnth e 1)]) (sorted-entries-arr v)))
      (core-sorted-set? v) (pr-render-seq buf (sorted-entries-arr v) "#{" "}")
      (lazy-seq? v) (pr-render-seq buf (realize-for-iteration v) "(" ")")
      (set? v) (pr-render-seq buf (phs-seq v) "#{" "}")
      (phm? v) (pr-render-pairs buf (phm-entries v))
      (pvec? v) (pr-render-seq buf (pv->array v) "[" "]")
      (plist? v) (pr-render-seq buf (pl->array v) "(" ")")
      (and (table? v) (get v :jolt/deftype))
        (if (and print-method-cb (print-method-cb v (fn [piece] (buffer/push-string buf piece))))
          nil
          # Clojure's record syntax: #ns.Type{:k v, ...} (fields only, the
          # deftype tag elided). This used to print the raw janet table.
          (do
            (buffer/push-string buf (string "#" (get v :jolt/deftype)))
            (pr-render-pairs buf
              (filter (fn [pair] (not= :jolt/deftype (in pair 0))) (pairs v)))))
      (tuple? v) (pr-render-seq buf v "[" "]")
      # mutable mode: arrays are vectors -> print with [] (else lists -> ())
      (array? v) (if mutable? (pr-render-seq buf v "[" "]") (pr-render-seq buf v "(" ")"))
      # Any remaining TAGGED value dispatches through print-method when the
      # hook is wired: the io tier owns the cold renderings (uuid, regex,
      # transient, channel — branches that used to live here), and user
      # defmethods on any :jolt/* tag fire from inside nested values. Before
      # the overlay loads (init-time error messages) these fall through to
      # the raw pairs view below.
      (and print-method-cb (get v :jolt/type)
           (print-method-cb v (fn [piece] (buffer/push-string buf piece))))
        nil
      (struct? v) (pr-render-pairs buf (pairs v))
      (table? v) (pr-render-pairs buf (pairs v))
      true (buffer/push-string buf (string v)))))

(defn str-render-one
  "Render one value with Clojure's `str`/.toString semantics (bare strings,
  nil -> empty, keywords/symbols by name, collections via pr-render)."
  [v]
  (cond
    (nil? v) ""
    (string? v) v
    (buffer? v) (string v)
    (core-char? v) (char->string v)
    (keyword? v) (string ":" (string v))
    (and (struct? v) (= :symbol (v :jolt/type)))
      (if (v :ns) (string (v :ns) "/" (v :name)) (v :name))
    (and (struct? v) (= :jolt/uuid (v :jolt/type))) (v :str)
    (and (struct? v) (= :jolt/inst (v :jolt/type))) (inst->rfc3339 v)
    # a java.io.File renders as its path (Clojure's File.toString)
    (and (table? v) (= :jolt/file (get v :jolt/type))) (get v :path)
    # (str pattern) -> raw regex source (no #"" delimiters), so libraries that
    # compose patterns via (re-pattern (str p1 ...)) work (Pattern.toString).
    (and (table? v) (= :jolt/regex (get v :jolt/type))) (get v :source)
    (= :jolt/namespace (get v :jolt/type)) (ns-display-name v)
    (and (table? v) (= :jolt/var (get v :jolt/type))) (var-display v)
    (number? v) (fmt-number v)
    (= true v) "true"
    (= false v) "false"
    # a record/deftype with a custom Object/toString renders via it (Clojure's
    # str/.toString semantics); plain records fall through to the data repr.
    (if-let [s (and record-tostring-cb (record-tag v) (record-tostring-cb v))]
      s
      (let [buf @""] (pr-render buf v) (string buf)))))

(defn core-str [& xs]
  (if (= 0 (length xs)) ""
    (do
      (var result @[])
      (each x xs (array/push result (str-render-one x)))
      (string/join result ""))))

(defn core-str-join
  "clojure.string/join: stringify each element (Clojure semantics), then join."
  [coll &opt sep]
  (default sep "")
  (let [items (realize-for-iteration coll)
        parts @[]]
    (each x items (array/push parts (str-render-one x)))
    (string/join parts (str-render-one sep))))

(defn core-name
  "Returns the name string of a keyword, symbol, or string (without namespace)."
  [x]
  (cond
    (keyword? x) (let [s (string x) i (string/find "/" s)] (if i (string/slice s (+ i 1)) s))
    (and (struct? x) (= :symbol (x :jolt/type))) (x :name)
    (string? x) x
    ""))

(defn core-namespace
  "Returns the namespace string of a keyword/symbol, or nil if none."
  [x]
  (cond
    (keyword? x) (let [s (string x) i (string/find "/" s)] (if i (string/slice s 0 i) nil))
    (and (struct? x) (= :symbol (x :jolt/type)))
      (if (x :ns) (if (struct? (x :ns)) ((x :ns) :name) (string (x :ns))) nil)
    nil))

(def core-subs
  (fn [& args]
    (when (not (or (= 2 (length args)) (= 3 (length args))))
      (error "Wrong number of args passed to: subs"))
    (let [s (args 0)
          start (get args 1)]
      (when (not (string? s)) (error (string "subs requires a string, got " (type s))))
      (let [len (length s)
            end (if (= 3 (length args)) (args 2) len)]
        # Clojure validates bounds (no negative/from-end/clamping like Janet):
        # 0 <= start <= end <= (count s).
        (when (not (and (number? start) (number? end)
                        (= start (math/floor start)) (= end (math/floor end))
                        (>= start 0) (<= start end) (<= end len)))
          (error "String index out of range"))
        (string/slice s start end)))))

# ============================================================
