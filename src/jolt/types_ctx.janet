# Jolt value layer — Context (+ inst/uuid values)
# Extracted from types.janet (jolt-bvek phase 5a split).

(use ./types_symbols)
(use ./types_var)
(use ./types_ns)
# ============================================================
# Context
# ============================================================

(defn ctx-find-ns
  "Find or create a namespace in the context by name symbol."
  [ctx ns-sym]
  (let [env (ctx :env)
        namespaces (env :namespaces)]
    (or (get namespaces ns-sym)
        (let [ns (make-ns ns-sym)]
          (put namespaces ns-sym ns)
          ns))))

# Instant value: an immutable tagged struct keyed by epoch milliseconds, so
# equality and map-key hashing are by INSTANT (offset-normalized): two #inst
# literals with different offsets denoting the same moment are =.
(defn make-inst [ms]
  {:jolt/type :jolt/inst :ms ms})

(defn parse-inst
  "Parse an RFC3339 timestamp with Clojure's partial defaults
  (yyyy[-MM[-dd[Thh[:mm[:ss[.fff]]]]]][Z|+hh:mm|-hh:mm]) to an inst value.
  Errors on a malformed timestamp."
  [ts]
  (def pat (peg/compile
    ~(sequence
       (capture (repeat 4 :d))                                   # year
       (opt (sequence "-" (capture (repeat 2 :d))))               # month
       (opt (sequence "-" (capture (repeat 2 :d))))               # day
       (opt (sequence "T" (capture (repeat 2 :d))                 # hour
                      (opt (sequence ":" (capture (repeat 2 :d))  # min
                        (opt (sequence ":" (capture (repeat 2 :d)) # sec
                          (opt (sequence "." (capture (some :d)))))))))) # frac
       (opt (choice (capture "Z")
                    (sequence (capture (set "+-")) (capture (repeat 2 :d))
                              ":" (capture (repeat 2 :d)))))
       -1)))
  (def m (peg/match pat ts))
  (when (nil? m) (error (string "Unrecognized #inst timestamp: " ts)))
  # captures arrive positionally; classify by shape: digits runs + offset parts.
  (var year nil) (var month 1) (var day 1)
  (var hh 0) (var mm 0) (var ss 0) (var frac "0")
  (var off-sign nil) (var off-h 0) (var off-m 0)
  (var i 0)
  (def fields @[:year :month :day :hh :mm :ss])
  (var fi 0)
  (while (< i (length m))
    (def part (in m i))
    (cond
      (= part "Z") nil
      (or (= part "+") (= part "-"))
        (do (set off-sign part)
            (set off-h (scan-number (in m (+ i 1))))
            (set off-m (scan-number (in m (+ i 2))))
            (+= i 2))
      # fractional seconds arrive right after :ss was filled
      (and (>= fi 6))
        (set frac part)
      (do
        (def v (scan-number part))
        (case (in fields fi)
          :year (set year v) :month (set month v) :day (set day v)
          :hh (set hh v) :mm (set mm v) :ss (set ss v))
        (++ fi)))
    (++ i))
  (when (nil? year) (error (string "Unrecognized #inst timestamp: " ts)))
  (def base-s (os/mktime {:year year :month (- month 1) :month-day (- day 1)
                          :hours hh :minutes mm :seconds ss}))
  # fractional part -> milliseconds (truncate beyond 3 digits)
  (def frac3 (string/slice (string frac "000") 0 3))
  (def ms-frac (scan-number frac3))
  (def off-s (* (if (= off-sign "-") -1 1) (+ (* off-h 3600) (* off-m 60))))
  (make-inst (- (+ (* base-s 1000) ms-frac) (* off-s 1000))))

(defn inst->rfc3339
  "Canonical print form: yyyy-MM-ddThh:mm:ss.fff-00:00 (UTC, like Clojure)."
  [inst]
  (def ms (inst :ms))
  (def s (math/floor (/ ms 1000)))
  (def frac (- ms (* s 1000)))
  (def d (os/date s))
  (string/format "%04d-%02d-%02dT%02d:%02d:%02d.%03d-00:00"
                 (d :year) (+ 1 (d :month)) (+ 1 (d :month-day))
                 (d :hours) (d :minutes) (d :seconds) frac))

# UUID value: an immutable tagged struct. Lowercased at construction so
# equality and map-key hashing are case-insensitive by value (struct equality),
# matching Clojure (java.util.UUID equality / cljs UUID).
(defn make-uuid [s]
  {:jolt/type :jolt/uuid :str (string/ascii-lower s)})

(defn make-ctx
  "Create a new evaluation context.
  (make-ctx)       — empty context with 'user namespace
  (make-ctx opts)  — context with initial namespaces from opts
  
  opts may contain:
    :namespaces — struct of {ns-symbol → {sym → value, ...}, ...}"
  [&opt opts]
  (default opts nil)
  (let [compile? (if opts (get opts :compile?) false)
        # Direct-linking (call-site/unit property, like Clojure). :aot-core?
        # (default true; JOLT_AOT_CORE=0 disables) compiles the core tiers +
        # compiler with direct-linking on. :direct-linking? is the per-unit flag
        # the back end reads while emitting; it defaults to the user-code setting
        # (off unless opted in) and load-core-overlay! flips it on around core.
        aot-core? (let [o (if opts (get opts :aot-core?) nil)]
                    (if (nil? o) (not (= "0" (os/getenv "JOLT_AOT_CORE"))) o))
        # Macro expanders compile in EVERY mode (macros are ordinary compiled
        # fns, as in Clojure) — including interpret mode, where evaluation stays
        # interpreted but expansion runs native. :compile-macros? false (or
        # JOLT_INTERPRET_MACROS=1) opts back into the fully-interpreted oracle.
        compile-macros? (let [o (if opts (get opts :compile-macros?) nil)]
                          (if (nil? o)
                            (not (= "1" (os/getenv "JOLT_INTERPRET_MACROS")))
                            o))
        env @{:namespaces @{}
              :class->opts @{}
              :current-ns "user"
              :compile? compile?
              :aot-core? aot-core?
              :compile-macros? compile-macros?
              # User-code direct-linking default (off unless opted in), the
              # apples-to-apples analog of jank's -Odirect-call / Clojure's
              # :direct-linking. JOLT_DIRECT_LINK=1 turns it on for user units;
              # this is also the gate the inline pass reads (a call is only
              # inline-safe when the callee won't be redefined). load-core-overlay!
              # still flips core to :aot-core? around the tiers and restores this.
              :direct-linking? (let [o (if opts (get opts :direct-linking?) nil)]
                                 (if (nil? o) (= "1" (os/getenv "JOLT_DIRECT_LINK")) o))
              # Inline + scalar-replacement passes (jolt-87f). OFF for all of init
              # (core load + self-hosted compiler recompile), so core/bootstrap
              # compile exactly as before; api/init flips it on to the user
              # direct-linking setting AFTER init, so only opted-in user code
              # inlines. The inline pass also reads this (via host/inline-enabled?).
              :inline? false
              # Ordered roots searched (after the stdlib) to resolve a namespace
              # to a .clj/.cljc file. jolt-core holds the portable Clojure layer
              # (analyzer/IR/core); deps.edn resolution appends dep src dirs.
              :source-paths @["jolt-core" "src/jolt"]
              :type-registry @{}
              :data-readers (let [dr @{}]
                              (put dr (keyword "#inst") (fn [s] (parse-inst s)))
                              (put dr (keyword "#uuid") (fn [s] (make-uuid s)))
                              dr)}
        # create the user namespace via a partial context
        _ (ctx-find-ns {:env env} "user")]
    # initialize from opts
    (when opts
      (when-let [ns-opts (get opts :namespaces)]
        (loop [[ns-sym mappings] :pairs ns-opts]
          (let [ns (ctx-find-ns {:env env} ns-sym)]
            (loop [[sym val] :pairs mappings]
              (ns-intern ns sym val))))))
    {:jolt/type :jolt/context
     :env env}))

(defn ctx?
  "Check if x is a Jolt Context."
  [x]
  (and (struct? x) (= :jolt/context (x :jolt/type))))

(defn ctx-env
  "Return the env atom from the context."
  [ctx]
  (ctx :env))

(defn ctx-current-ns
  "Get the current namespace symbol."
  [ctx]
  (get (ctx :env) :current-ns))

(defn ctx-set-current-ns
  "Set the current namespace symbol. Also keeps the *ns* dynamic var's root in
  sync (the var table is cached on the env by install-stateful-fns! — one table
  put on this hot path, no ns lookup chain)."
  [ctx ns-sym]
  (put (ctx :env) :current-ns ns-sym)
  (when-let [v (get (ctx :env) :ns-var)]
    (put v :root (ctx-find-ns ctx ns-sym))))

(defn all-ns
  "Return a list of all namespaces in the context."
  [ctx]
  (let [namespaces (get (ctx :env) :namespaces)
        result @[]]
    (loop [[_ ns] :pairs namespaces]
      (array/push result ns))
    result))

(defn remove-ns
  "Remove a namespace from the context by name string."
  [ctx ns-name]
  (put (get (ctx :env) :namespaces) ns-name nil) nil)

(defn create-ns
  "Create a new namespace."
  [ctx ns-name]
  (ctx-find-ns ctx ns-name))

(defn the-ns
  "Return the current namespace object."
  [ctx]
  (ctx-find-ns ctx (ctx-current-ns ctx)))

(defn ns-interns
  "Return the map of all interned vars in the current namespace."
  [ctx]
  (let [ns (ctx-find-ns ctx (ctx-current-ns ctx))]
    (ns :mappings)))

(defn ns-aliases
  "Return the alias map of the current namespace."
  [ctx]
  (let [ns (ctx-find-ns ctx (ctx-current-ns ctx))]
    (ns :aliases)))

(defn find-var
  "Resolve a symbol to a var in the current context.
  Looks in current namespace first, then clojure.core."
  [ctx sym-s]
  (let [name (sym-s :name)
        ns-sym (sym-s :ns)]
    (if ns-sym
      (let [ns (ctx-find-ns ctx ns-sym)]
        (ns-find ns name))
      (let [current-ns (ctx-find-ns ctx (ctx-current-ns ctx))
            v (ns-find current-ns name)]
        (if v v
          (let [core-ns (ctx-find-ns ctx "clojure.core")]
            (ns-find core-ns name)))))))


# ============================================================
