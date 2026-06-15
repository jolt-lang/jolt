# Jolt Evaluator — special forms (eval-list dispatch)
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
(use ./eval_resolve)
(use ./eval_runtime)
(defn- unwrap-meta-name
  "Recursively unwrap (with-meta sym meta) forms to extract the underlying symbol.
  Returns the symbol struct, or the original form if it's not a with-meta wrapper."
  [form]
  (if (and (array? form) (> (length form) 0)
           (struct? (in form 0))
           (= :symbol ((in form 0) :jolt/type))
           (= "with-meta" ((in form 0) :name)))
    (unwrap-meta-name (in form 1))
    form))


# --- special-form handlers (exploded from eval-list, jolt-oudv) ---
(defn eval-do [ctx bindings form]
  (do
             (var result nil)
             (var i 1)
             (let [len (length form)]
               (while (< i len)
                 (set result (eval-form ctx bindings (in form i)))
                 (++ i)))
             result))

(defn eval-if [ctx bindings form]
  (do
             # 2 or 3 argument forms only (spec 03-special-forms X1)
             (when (or (< (length form) 3) (> (length form) 4))
               (error (string "Wrong number of args (" (dec (length form)) ") passed to: if")))
             (let [test-val (eval-form ctx bindings (in form 1))]
               (if (and (not (nil? test-val)) (not (= false test-val)))
                 (eval-form ctx bindings (in form 2))
                 (if (> (length form) 3) (eval-form ctx bindings (in form 3)) nil)))))

(defn eval-def [ctx bindings form]
  (let [raw-name (in form 1)
                  name-sym (unwrap-meta-name raw-name)
                  # Metadata on the name: keyword/type-hint metadata rides on the
                  # symbol (:meta); a ^{:map} reads as a with-meta form we evaluate.
                  sym-meta (or (and (struct? name-sym) (get name-sym :meta)) {})
                  wm-meta (if (and (array? raw-name) (> (length raw-name) 0)
                                   (sym-name? (first raw-name) "with-meta"))
                            (let [mv (protect (eval-form ctx bindings (last raw-name)))]
                              (if (and (mv 0) (or (table? (mv 1)) (struct? (mv 1)))) (mv 1) {}))
                            {})
                  name-meta (merge wm-meta sym-meta)
                  dynamic? (truthy? (get name-meta :dynamic))
                  ns-name (ctx-current-ns ctx)
                  ns (ctx-find-ns ctx ns-name)
                  # Create var first (unbound) so self-referencing defs resolve
                  v (ns-intern ns (name-sym :name))]
              # (def name) with no init interns the var and leaves any existing
              # root binding alone (Clojure semantics — this is what declare
              # expands to, so compiled forward refs bind to the var instead of
              # falling through to a like-named host builtin).
              (if (= 2 (length form))
                (do
                  (when (not (empty? name-meta))
                    (put v :meta (merge (or (get v :meta) {}) name-meta)))
                  (when dynamic? (put v :dynamic true))
                  v)
                (let [# (def name docstring value): docstring form 2, value form 3
                      has-doc (and (> (length form) 3) (string? (in form 2)))
                      val-form (in form (if has-doc 3 2))
                      val (eval-form ctx bindings val-form)]
                  (bind-root v val)
                  # Staged bootstrap (jolt-4j3): pre/at-kernel overlay defns load
                  # interpreted; stash the fn source so backend/recompile-defns! can
                  # compile them once the analyzer is alive — the defn analog of
                  # :macro-src. Only set while api/load-core-overlay! loads the early
                  # tiers (the flag scopes it away from user code).
                  (when (and (get (ctx :env) :stash-defn-src?)
                             (function? val)
                             (array? val-form) (> (length val-form) 0)
                             (or (sym-name? (first val-form) "fn")
                                 (sym-name? (first val-form) "fn*")))
                    (put v :defn-src val-form))
                  (let [extra (if has-doc (merge name-meta {:doc (in form 2)}) name-meta)]
                    (when (not (empty? extra))
                      (put v :meta (merge (or (get v :meta) {}) extra))))
                  (when dynamic?
                    (put v :dynamic true))
                  # def returns the var (Clojure semantics); REPL prints #'ns/name
                  v))))

(defn eval-defmacro [ctx bindings form]
  (let [# ^{:map} metadata on the name reads as a (with-meta sym …)
                       # form (jolt-8w2); unwrap to the bare symbol like def does.
                       name-sym (unwrap-meta-name (in form 1))
                       after-name (tuple/slice form 2)
                       # Skip an optional leading docstring (string) then an optional
                       # attr-map (a struct that is not a symbol — a map literal reads
                       # as a struct), matching defn. Real macros use both, e.g.
                       # (defmacro info "doc" {:arglists '(...)} [& args] …).
                       a1 (if (and (> (length after-name) 0) (string? (first after-name)))
                            (tuple/slice after-name 1) after-name)
                       after-meta (if (and (> (length a1) 0)
                                           (struct? (first a1))
                                           (not= :symbol (get (first a1) :jolt/type)))
                                    (tuple/slice a1 1) a1)
                       # What remains is either a params VECTOR (tuple) + body, or one
                       # or more arity CLAUSES (each a list, i.e. a janet array). Build
                       # a uniform arity list [{:params … :body …} …].
                       multi? (and (> (length after-meta) 0) (array? (first after-meta)))
                       arities (if multi?
                                 (map (fn [cl] {:params (first cl) :body (tuple/slice cl 1)})
                                      after-meta)
                                 @[{:params (first after-meta) :body (tuple/slice after-meta 1)}])
                       defining-ns (ctx-current-ns ctx)]
                   (def interp-fn (fn [& macro-args]
                     (def n (length macro-args))
                     # Pick the arity: an exact fixed-count match wins; otherwise the
                     # first variadic arity that accepts n args (Clojure fn dispatch).
                     (var chosen nil)
                     (each ar arities
                       (def pi (parse-params (ar :params)))
                       (when (and (nil? chosen) (not (pi :rest)) (= n (length (pi :fixed))))
                         (set chosen [pi (ar :body)])))
                     (when (nil? chosen)
                       (each ar arities
                         (def pi (parse-params (ar :params)))
                         (when (and (nil? chosen) (pi :rest) (>= n (length (pi :fixed))))
                           (set chosen [pi (ar :body)]))))
                     (when (nil? chosen)
                       (error (string "no matching arity for macro " (name-sym :name)
                                      " (" n " args)")))
                     (def pi (chosen 0))
                     (def body (chosen 1))
                     (var new-bindings @{})
                     (table/setproto new-bindings bindings)
                     (put new-bindings "&env" @{})  # implicit &env for macro bodies (table — nil-safe)
                     (var i 0)
                     # Destructure macro params (like fn), so [& [a & more :as all]]
                     # and {:keys …} rest forms work in macro arglists.
                     (each pat (pi :fixed)
                       (destructure-bind ctx new-bindings pat (macro-args i))
                       (++ i))
                     (when (pi :rest)
                       (destructure-bind ctx new-bindings (pi :rest) (rest-args-val macro-args i)))
                     # Use defining namespace for symbol resolution
                     (def saved-ns (ctx-current-ns ctx))
                     (ctx-set-current-ns ctx defining-ns)
                     # Plain trailing restore (NOT defer/try — those build a fiber per
                     # call and blow the C stack on deep interpreted recursion). An
                     # unwinding throw is repaired once at the TOP-LEVEL boundary
                     # (loader/eval-toplevel restores the ns on error).
                     (var result nil)
                     (each bf body
                       (set result (eval-form ctx new-bindings bf)))
                     (ctx-set-current-ns ctx saved-ns)
                     result))
                   # A COMPILED expander (native-speed) is only built for the
                   # single-arity case (the compile hook + recompile path take one
                   # [args body]); multi-arity macros use the interpreted expander.
                   (def single? (= 1 (length arities)))
                   (def args-form (and single? ((first arities) :params)))
                   (def body (and single? ((first arities) :body)))
                   (def uses-env (do (var u false)
                                     (each ar arities
                                       (when (or (form-uses-sym? (ar :body) "&env")
                                                 (form-uses-sym? (ar :body) "&form"))
                                         (set u true)))
                                     u))
                   (def compiled-fn
                     (when (and macro-compile-hook single? (not uses-env))
                       (macro-compile-hook ctx args-form body)))
                   (def macro-fn (or compiled-fn interp-fn))
                    (let [ns-name (ctx-current-ns ctx)
                         ns (ctx-find-ns ctx ns-name)]
                     (def v (ns-intern ns (name-sym :name) macro-fn))
                     (put v :macro true)
                     # Stash the expander source so backend/recompile-macros! can
                     # compile it once the analyzer is alive (staged bootstrap): a
                     # macro defined WHILE the analyzer is still being built gets an
                     # interpreted closure now, a compiled expander later. uses-env
                     # macros stay interpreted (the compiled fn* has no &env/&form);
                     # multi-arity macros keep the interpreted dispatch (no single
                     # [args body] to recompile).
                     (when single? (put v :macro-src @[args-form body]))
                     (put v :macro-uses-env uses-env)
                     (when compiled-fn (put v :macro-compiled true))
                     # A (re)defined macro invalidates any cached expansions.
                     (table/clear macro-cache)
                     (var-get v))))

(defn eval-fn* [ctx bindings form]
  (let [# optional name: (fn* name [args] ...) / (fn* name ([args] ...)...)
                  named? (and (struct? (in form 1)) (= :symbol ((in form 1) :jolt/type)))
                  fn-name (if named? ((in form 1) :name) nil)
                  form (if named? (array/concat @[(in form 0)] (tuple/slice form 2)) form)]
            (if (array? (in form 1))
               # Multi-arity: (fn* ([args] body...) ([args] body...)...)
               (let [pairs (tuple/slice form 1)
                     arities @{}
                     defining-ns (ctx-current-ns ctx)]
                 (var self nil)
                 # The (single) variadic clause is dispatched separately: it handles
                 # any arg count >= its fixed count. Storing it in `arities` by
                 # fixed-count would collide with a same-fixed-count fixed clause and
                 # only match that exact count.
                 (var variadic-fn nil)
                 (var variadic-min 0)
                 (each pair pairs
                   (let [args-form (in pair 0)
                         body (tuple/slice pair 1)
                         param-info (parse-params args-form)
                         _ (require-symbol-params param-info)
                         fixed-pats (param-info :fixed)
                         rest-pat (param-info :rest)
                         n-fixed (length fixed-pats)
                         # recur-entry: where (recur ...) re-enters THIS arity. For
                         # a fixed arity it's the dispatcher (exact count re-selects
                         # it). For the VARIADIC arity, recur takes n-fixed + 1 args
                         # with the LAST bound DIRECTLY as the rest seq (Clojure) —
                         # re-entering through the varargs collector would wrap it
                         # in a fresh 1-element rest list and the seq never empties
                         # (the jolt-4df hang).
                         recur-entry-box @[nil]
                         run-clause (fn [fn-bindings]
                            (put fn-bindings :jolt/loop-fn (in recur-entry-box 0))
                            (when fn-name (bind-put fn-bindings fn-name self))
                            # Use defining namespace for symbol resolution
                            (def saved-ns (ctx-current-ns ctx))
                            (ctx-set-current-ns ctx defining-ns)
                            # Plain trailing restore (NOT defer/try — those build a fiber per
                            # call and blow the C stack on deep interpreted recursion). An
                            # unwinding throw is repaired once at the TOP-LEVEL boundary
                            # (loader/eval-toplevel restores the ns on error).
                            (var result nil)
                            (each body-form body
                              (set result (eval-form ctx fn-bindings body-form)))
                            (ctx-set-current-ns ctx saved-ns)
                            result)
                         f (fn [& fn-args]
                            (var fn-bindings @{})
                            (table/setproto fn-bindings bindings)
                            (var i 0)
                            (each pat fixed-pats
                              (destructure-bind ctx fn-bindings pat (fn-args i))
                              (++ i))
                            (when rest-pat
                              (destructure-bind ctx fn-bindings rest-pat (rest-args-val fn-args i)))
                            (run-clause fn-bindings))]
                     (if rest-pat
                       (do
                         (put recur-entry-box 0
                              (fn [& recur-args]
                                (var fn-bindings @{})
                                (table/setproto fn-bindings bindings)
                                (var i 0)
                                (each pat fixed-pats
                                  (destructure-bind ctx fn-bindings pat (recur-args i))
                                  (++ i))
                                (destructure-bind ctx fn-bindings rest-pat (get recur-args i))
                                (run-clause fn-bindings)))
                         (set variadic-fn f) (set variadic-min n-fixed))
                       (do
                         (put recur-entry-box 0 (fn [& recur-args] (apply self recur-args)))
                         (put arities n-fixed f)))))
                 (set self (fn [& fn-args]
                   (let [n (length fn-args)
                         f (get arities n)]
                     (cond
                       f (apply f fn-args)
                       (and variadic-fn (>= n variadic-min)) (apply variadic-fn fn-args)
                       (error (string "Wrong number of args (" n ") passed to: "
                                      (or fn-name "fn")))))))
                 self)
               # Single-arity: (fn* [args] body...)
               (let [args-form (in form 1)
                     body (tuple/slice form 2)
                     param-info (parse-params args-form)
                     _ (require-symbol-params param-info)
                     fixed-pats (param-info :fixed)
                     rest-pat (param-info :rest)
                     defining-ns (ctx-current-ns ctx)]
                 (var self nil)
                 (var recur-entry nil)
                 (def run-body (fn [fn-bindings]
                   (put fn-bindings :jolt/loop-fn recur-entry)
                   (when fn-name (bind-put fn-bindings fn-name self))
                   # Use defining namespace for symbol resolution
                   (def saved-ns (ctx-current-ns ctx))
                   (ctx-set-current-ns ctx defining-ns)
                   # Plain trailing restore (NOT defer/try — those build a fiber per
                   # call and blow the C stack on deep interpreted recursion). An
                   # unwinding throw is repaired once at the TOP-LEVEL boundary
                   # (loader/eval-toplevel restores the ns on error).
                   (var result nil)
                   (each body-form body
                     (set result (eval-form ctx fn-bindings body-form)))
                   (ctx-set-current-ns ctx saved-ns)
                   result))
                 (def n-fixed (length fixed-pats))
                 (set self (fn [& fn-args]
                   # ArityException semantics (jolt-6xn): a fixed arity takes
                   # exactly its params, a variadic one at least its fixed params.
                   # The compiled path enforces this natively (janet fn arity);
                   # this keeps the interpreter oracle in agreement.
                   (def n (length fn-args))
                   (when (if rest-pat (< n n-fixed) (not= n n-fixed))
                     (error (string "Wrong number of args (" n ") passed to: "
                                    (or fn-name "fn"))))
                   (var fn-bindings @{})
                   (table/setproto fn-bindings bindings)
                   (var i 0)
                   (each pat fixed-pats
                     (destructure-bind ctx fn-bindings pat (fn-args i))
                     (++ i))
                   (when rest-pat
                     (destructure-bind ctx fn-bindings rest-pat (rest-args-val fn-args i)))
                   (run-body fn-bindings)))
                 # recur re-enters here: for a variadic fn it takes n-fixed + 1
                 # args, the LAST bound DIRECTLY as the rest seq (Clojure) — going
                 # back through the varargs collector wrapped the seq in a fresh
                 # 1-element rest list, so it never emptied (the jolt-4df hang).
                 (set recur-entry
                   (if rest-pat
                     (fn [& recur-args]
                       (var fn-bindings @{})
                       (table/setproto fn-bindings bindings)
                       (var i 0)
                       (each pat fixed-pats
                         (destructure-bind ctx fn-bindings pat (recur-args i))
                         (++ i))
                       (destructure-bind ctx fn-bindings rest-pat (get recur-args i))
                       (run-body fn-bindings))
                     self))
                self))))

(defn eval-let* [ctx bindings form]
  (let [bind-vec (in form 1)
                    body (tuple/slice form 2)]
                (var new-bindings @{})
                (table/setproto new-bindings bindings)
                (var i 0)
                (let [len (length bind-vec)]
                  (while (< i len)
                    (let [pat (bind-vec i)]
                      # let* is a primitive (the let macro desugars destructuring);
                      # its binding names must be plain symbols, as in Clojure.
                      (unless (plain-sym? pat) (error "Bad binding form, expected symbol"))
                      (def val (eval-form ctx new-bindings (bind-vec (+ i 1))))
                      (destructure-bind ctx new-bindings pat val)
                      (+= i 2))))
               (var result nil)
               (each body-form body
                 (set result (eval-form ctx new-bindings body-form)))
               result))

(defn eval-loop* [ctx bindings form]
  (let [bind-vec (in form 1)
                    body (tuple/slice form 2)
                    init-vals @[]
                    patterns @[]
                    # Inits are evaluated sequentially in an accumulating scope (like
                    # let*), so a later init can reference an earlier binding —
                    # matching Clojure's loop.
                    seq-bindings @{}]
                (table/setproto seq-bindings bindings)
                (var i 0)
                (while (< i (length bind-vec))
                  # loop* is a primitive (the loop macro desugars destructuring);
                  # its binding names must be plain symbols, as in Clojure.
                  (unless (plain-sym? (bind-vec i)) (error "Bad binding form, expected symbol"))
                  (def v (eval-form ctx seq-bindings (bind-vec (+ i 1))))
                  (bind-put seq-bindings ((bind-vec i) :name) v)
                  (array/push init-vals v)
                  (array/push patterns (bind-vec i))
                  (+= i 2))
                (var loop-fn nil)
                (set loop-fn (fn [& args]
                  (var loop-bindings @{})
                  (table/setproto loop-bindings bindings)
                  (var j 0)
                  (each pat patterns
                    (destructure-bind ctx loop-bindings pat (in args j))
                    (++ j))
                  (put loop-bindings :jolt/loop-fn loop-fn)
                  (var result nil)
                  (each body-form body
                    (set result (eval-form ctx loop-bindings body-form)))
                  result))
                (apply loop-fn init-vals)))

(defn eval-recur [ctx bindings form]
  (let [loop-fn (get bindings :jolt/loop-fn)]
                (if (nil? loop-fn)
                  (error "recur used outside of loop* or fn*")
                  (let [args (map |(eval-form ctx bindings $) (tuple/slice form 1))]
                    (apply loop-fn args)))))

(defn eval-try [ctx bindings form]
  (let [# The body is EVERY form between `try` and the first catch/finally
                  # clause (not just form 1 — a multi-form body before the clauses,
                  # e.g. (try (foo) (bar) (catch …)), dropped all but the first).
                  forms (tuple/slice form 1)
                  clause? (fn [c]
                            (and (array? c) (> (length c) 0)
                                 (struct? (first c)) (= :symbol ((first c) :jolt/type))
                                 (or (= "catch" ((first c) :name))
                                     (= "finally" ((first c) :name)))))
                  split (do (var k 0)
                            (while (and (< k (length forms)) (not (clause? (in forms k)))) (++ k))
                            k)
                  body-forms (tuple/slice forms 0 split)
                  clauses (tuple/slice forms split)
                  # current-ns is dynamic state. The interpreter rebinds it to a
                  # fn's defining ns while that fn runs and restores it on normal
                  # return, but a fn that THROWS unwinds past its own restore — so
                  # the ns can leak. try is the unwind boundary: restore the ns that
                  # was current at try entry before running catch/finally, so caught
                  # code (and the harness's is/thrown?) sees the right namespace.
                  try-ns (ctx-current-ns ctx)]
              (var catch-sym nil)
              (var catch-body nil)
              (var finally-body nil)
              (each clause clauses
                (when (and (array? clause) (> (length clause) 0))
                  (let [head (first clause)]
                    (when (and (struct? head) (= :symbol (head :jolt/type)))
                      (match (head :name)
                        "catch" (do
                          (set catch-sym (in clause 2))
                          (set catch-body (tuple/slice clause 3)))
                        "finally" (set finally-body (tuple/slice clause 1)))))))
              (defn eval-body []
                (var result nil)
                (each bf body-forms (set result (eval-form ctx bindings bf)))
                result)
              (defn run-finally []
                (when finally-body
                  (each fb finally-body (eval-form ctx bindings fb))))
              (defn run-protected []
                (if catch-sym
                  (try
                    (eval-body)
                    ([err]
                     (ctx-set-current-ns ctx try-ns)
                     (var new-bindings @{})
                     (table/setproto new-bindings bindings)
                     # bind the originally-thrown value (unwrap the :jolt/exception
                     # envelope) so (catch … e (throw e)) rethrows the same value
                     # rather than nesting another envelope
                     (def caught
                       (if (and (or (table? err) (struct? err)) (= :jolt/exception (get err :jolt/type)))
                         (get err :value)
                         err))
                     (put new-bindings (catch-sym :name) caught)
                     (var result nil)
                     (each cb catch-body
                       (set result (eval-form ctx new-bindings cb)))
                     result))
                  # no catch: restore the ns on an unwinding error, then re-raise
                  (try (eval-body) ([err] (ctx-set-current-ns ctx try-ns) (error err)))))
              # finally ALWAYS runs (success, caught error, or rethrow) — defer so it
              # fires even if a catch body throws. Without a finally, just run.
              (if finally-body
                (defer (run-finally) (run-protected))
                (run-protected))))

(defn eval-set! [ctx bindings form]
  (let [target (in form 1)
                    val (eval-form ctx bindings (in form 2))]
                # Handle (set! (.-field obj) val) — .-field shorthand as a list
                (if (and (array? target) (> (length target) 1)
                         (struct? (first target)) (= :symbol ((first target) :jolt/type))
                         (> (length ((first target) :name)) 1)
                         (= (string/slice ((first target) :name) 0 2) ".-"))
                  (let [obj (eval-form ctx bindings (in target 1))
                        field-name (string/slice ((first target) :name) 2)
                        field-key (keyword field-name)]
                    (if (get obj :jolt/deftype)
                      (do (put obj field-key val) val)
                      (error (string "Can't set! field on non-deftype: " (type obj)))))
                  # (set! (. obj -field) val) — instance field mutation
                  (if (and (array? target) (> (length target) 0)
                           (struct? (first target))
                           (= :symbol ((first target) :jolt/type))
                           (= "." ((first target) :name)))
                    (let [obj (eval-form ctx bindings (in target 1))
                          field-sym (in target 2)
                          field-name (field-sym :name)
                          field-key (keyword (if (and (> (length field-name) 0) (= "-" (string/slice field-name 0 1)))
                                             (string/slice field-name 1)
                                             field-name))]
                      (if (get obj :jolt/deftype)
                        (do (put obj field-key val) val)
                        (error (string "Can't set! field on non-deftype: " (type obj)))))
                    # (set! var val) — normal var mutation
                    (let [target-sym target
                          v (resolve-var ctx bindings target-sym)]
                      (if v
                        (do (var-set v val) val)
                        # Auto-create var if it doesn't exist
                        (let [ns-name (ctx-current-ns ctx)
                              ns (ctx-find-ns ctx ns-name)]
                          (def new-v (ns-intern ns (target-sym :name) val))
                          val)))))))

(defn eval-new [ctx bindings form]
  (let [type-sym (in form 1)
                  args (map |(eval-form ctx bindings $) (tuple/slice form 2))
                  ctor (eval-form ctx bindings type-sym)
                  ctor (if (string? ctor) (or (ctor-for-class-token ctor) ctor) ctor)]
              (apply ctor args)))

# Member dispatch shared by the two `.` forms (jolt-eos3). `args` is the
# (possibly empty) tuple of already-evaluated arguments; `has-args` is true for
# the call form `(. obj method arg...)` and false for the bare form
# `(. obj member)`. The two forms agree on the string/number/object/tagged-shim
# dispatch chain (single-sourced here, so an interop change touches one place)
# but diverge in the tail: the call form tries record → native-field →
# coll-interop(args); the bare form tries zero-arg coll-interop → field /
# zero-arg method. The guards that differed between the old copy-pasted arms are
# keyed off `has-args` so behavior is identical (note: the object-methods guard
# checks `table?` only, while tagged dispatch checks table-or-struct — both kept
# verbatim from the original arms).
# A record's own implementation of `field-name` (its instance fn, a reified fn,
# or a protocol method from the type registry), or nil. A deftype/defrecord
# method must win over the generic object-methods table — e.g. a custom
# (Object (toString [_] ...)) over the default toString (jolt-rt6n).
(defn- record-member [ctx target field-name]
  (when (record-tag target)
    (let [mk (keyword field-name)
          own (get target mk)
          reified (get (get target :jolt/protocol-methods) mk)]
      (cond
        (or (function? own) (cfunction? own)) own
        (or (function? reified) (cfunction? reified)) reified
        (find-method-any-protocol ctx (record-tag target) field-name)))))

(defn dispatch-member [ctx bindings target member-raw member-name field-name args has-args]
  (cond
    # java.lang.String surface for string/buffer targets
    (or (string? target) (buffer? target))
      (let [m (get string-methods field-name)]
        (if m
          (m (string target) ;args)
          (if-let [om (get object-methods field-name)]
            (om (string target) ;args)
            (error (string "Unsupported String method ." field-name)))))
    # numeric methods
    (and (number? target) (get number-methods field-name))
      ((get number-methods field-name) target ;args)
    # universal object methods — skipped when a shim tag-table owns the member,
    # OR when the target is a record that implements the member itself (so a
    # deftype's own toString/equals/hashCode wins over the generic one, jolt-rt6n).
    # Call form defers to tagged dispatch whenever a tag-table exists; bare form
    # only when the tag-table actually carries this member, so zero-arg
    # toString/hashCode still reach object-methods on shim objects.
    (and (get object-methods field-name)
         (not (and (table? target) (get tagged-methods (get target :jolt/type))
                   (or has-args (get (get tagged-methods (get target :jolt/type)) field-name))))
         (not (record-member ctx target field-name)))
      ((get object-methods field-name) target ;args)
    # registered shim objects (java.time etc.): tag-keyed method tables
    (and (or (table? target) (struct? target))
         (get tagged-methods (get target :jolt/type))
         (or has-args (get (get tagged-methods (get target :jolt/type)) field-name)))
      (let [m (get (get tagged-methods (get target :jolt/type)) field-name)]
        (if m
          (m target ;args)
          (error (string "Unsupported method ." field-name " on " (string (get target :jolt/type))))))
    # --- divergent tail ---
    has-args
      # (. obj method args...): record protocol dispatch, else native field/method
      (if (record-tag target)
        # deftype/reify methods live in the protocol registry (or the instance's
        # reified-fns table), not on the instance. get is safe on a shape-rec
        # tuple (returns nil for the method/protocol keys).
        (let [method-key (keyword field-name)
              own (get target method-key)
              reified (get (get target :jolt/protocol-methods) method-key)
              m (cond
                  (or (function? own) (cfunction? own)) own
                  (or (function? reified) (cfunction? reified)) reified
                  (find-method-any-protocol ctx (record-tag target) field-name))]
          (if m
            (apply m target args)
            (error (string "No method ." field-name " on " (record-tag target)))))
        # Janet-native interop: try field lookup + call
        (if (or (table? target) (struct? target))
          (let [method (get target (keyword field-name))]
            (if (or (function? method) (cfunction? method))
              (method target ;args)
              # If stored as fn* form (array), compile to function then call
              (if (array? method)
                (let [method-fn (eval-form ctx bindings method)]
                  (if (or (function? method-fn) (cfunction? method-fn))
                    (method-fn target ;args)
                    (error (string "Cannot call non-function " field-name " on " (type target)))))
                (let [r (if coll-interop (coll-interop target field-name args) :jolt/ci-none)]
                  (if (= r :jolt/ci-none)
                    (error (string "Cannot call non-function " field-name " on " (type target)))
                    r)))))
          (error (string "Cannot call method " field-name " on " (type target)))))
    # (. obj member) with no extra args: a symbol member naming a function is a
    # zero-arg method call (receiver passed as self); a keyword or `-field`
    # member is plain field access.
    true
      # zero-arg Java collection interop (.count/.seq/… on a jolt collection)
      # before field lookup — coll-interop returns :jolt/ci-none if not its kind
      (let [ci (if coll-interop (coll-interop target field-name @[]) :jolt/ci-none)]
        (if (not= ci :jolt/ci-none) ci
          (let [v (if (record-tag target)
                    (coll-lookup target (keyword field-name) nil)
                    (get target (keyword field-name)))]
            (if (and (struct? member-raw) (= :symbol (member-raw :jolt/type))
                     (not (string/has-prefix? "-" member-name)))
              (cond
                (or (function? v) (cfunction? v)) (v target)
                # zero-arg deftype/reify method via the protocol registry
                (record-tag target)
                  (let [reified (get (get target :jolt/protocol-methods) (keyword field-name))
                        m (if (or (function? reified) (cfunction? reified)) reified
                            (find-method-any-protocol ctx (record-tag target) field-name))]
                    (if m (m target) v))
                # value stored as an unevaluated fn* form: compile then call
                (array? v) (let [f (eval-form ctx bindings v)]
                             (if (or (function? f) (cfunction? f)) (f target) f))
                v)
              v))))))

(defn eval-dot [ctx bindings form]
  (let [target (eval-form ctx bindings (in form 1))
        member-raw (in form 2)
        # Resolve member name: symbols have :name, keywords use string, strings as-is
        member-name (if (and (struct? member-raw) (= :symbol (member-raw :jolt/type)))
                      (member-raw :name)
                      (if (keyword? member-raw)
                        (string member-raw)
                        member-raw))
        field-name (if (and (string? member-name) (> (length member-name) 0) (= "-" (string/slice member-name 0 1)))
                     (string/slice member-name 1)
                     member-name)
        has-args (> (length form) 3)
        args (if has-args (map |(eval-form ctx bindings $) (tuple/slice form 3)) @[])]
    (dispatch-member ctx bindings target member-raw member-name field-name args has-args)))

(defn eval-list
  [ctx bindings form]
  (def first-form (first form))
  # Safe name extraction: non-symbol heads (e.g. keywords) fall through to default.
  # A head qualified to a NON-core namespace (e.g. clojure.edn/read-string) must
  # resolve to that var, not the like-named clojure.core special form — so only
  # unqualified or clojure.core-qualified heads dispatch as special forms.
  (def name (if (and (struct? first-form) (= :symbol (first-form :jolt/type)))
              (let [ns (first-form :ns)]
                (if (or (nil? ns) (= ns "clojure.core")) (first-form :name) nil))
              nil))
  (match name
    "quote" (in form 1)
    # Interpreter builds the form directly (self-contained, no core dependency).
    # The COMPILE path instead lowers syntax-quote to construction code (via
    # syntax-quote-lower) so a backtick body is compilable; the two are kept in
    # sync and cross-checked by conformance (interpret vs compile modes).
    "syntax-quote" (syntax-quote* ctx bindings (in form 1))
    "unquote" (error "Unquote not valid outside of syntax-quote")
    "unquote-splicing" (error "Unquote-splicing not valid outside of syntax-quote")
    "eval" (eval-form ctx bindings (eval-form ctx bindings (in form 1)))
    # read-string/macroexpand-1 are ctx-capturing clojure.core fns and defonce
    # an overlay macro now (Stage 2 tier 6c) — no special-form arms.
    "do" (eval-do ctx bindings form)
    "if" (eval-if ctx bindings form)
    "def" (eval-def ctx bindings form)
    "defmacro" (eval-defmacro ctx bindings form)
    # ns is now a macro (clojure.core, 30-macros) expanding to in-ns + require/use/
    # import/refer-clojure calls — all ctx-capturing fns — so it compiles. No
    # special-form arm; an (ns ...) head falls through to the macro-expansion path.
    # require / in-ns are now ordinary clojure.core fns (install-stateful-fns!) —
    # no special-form arm; they compile + interpret as plain invokes.
    # all-ns/the-ns/create-ns/remove-ns/ns-interns/ns-aliases/ns-imports/
    # ns-resolve/resolve/find-ns/refer are ctx-capturing clojure.core fns now
    # (install-stateful-fns!) with evaluated-arg Clojure semantics — they fall
    # through to the function-call default and compile as plain invokes
    # (Stage 2 tier 6b).
    "fn*" (eval-fn* ctx bindings form)
    "let*" (eval-let* ctx bindings form)
    "loop*" (eval-loop* ctx bindings form)
    "recur" (eval-recur ctx bindings form)
    "throw" (let [val (eval-form ctx bindings (in form 1))]
              (error {:jolt/type :jolt/exception :value val}))
    "try" (eval-try ctx bindings form)
    "set!" (eval-set! ctx bindings form)
    "var" (let [target-sym (in form 1)
                 v (resolve-var ctx bindings target-sym)]
             (if v v (error (string "Unable to resolve var: " (sym-name-str target-sym) " in var"))))
    # var-get/var-set/var?/alter-var-root/alter-meta!/reset-meta! are plain
    # clojure.core fns; find-var/intern are ctx-capturing clojure.core fns
    # (install-stateful-fns!) — they fall through to the function-call default
    # and compile as ordinary invokes (Stage 2 tier 6).
    # set?/disj are plain clojure.core fns now (core-set?/core-disj) — no longer
    # special-cased here, the analyzer, or compiler.janet (jolt-g3h).
    # protocol-dispatch / register-method / make-reified are now ordinary
    # clojure.core fns (install-stateful-fns!) — the defprotocol/extend-type/reify
    # macros call them with name STRINGS, so they compile + interpret as plain
    # invokes (no special-form arms).
    # satisfies?/instance?/locking and the multimethod table ops
    # (prefer-method/remove-method/remove-all-methods/get-method/methods) are
    # clojure.core fns / overlay macros now (Stage 2 tier 6c) — no special arms.
    # deftype is now a macro (30-macros) over make-deftype-ctor + extend-type —
    # compiles as a plain (do …); no special-form arm.
    "new" (eval-new ctx bindings form)
    "." (eval-dot ctx bindings form)
    # default: function application — check for macros
    (if (and (struct? first-form) (= :symbol (first-form :jolt/type)))
      (let [sym-name (first-form :name)]
        # Handle .-fieldName accessor: (.-cnt obj) → (. obj -cnt)
        (if (and (> (length sym-name) 1) (= (string/slice sym-name 0 2) ".-")
                 (> (length form) 1))
          (let [field-name (string/slice sym-name 2)
                target (eval-form ctx bindings (in form 1))]
            (get target (keyword field-name)))
        # (.method obj args...) sugar -> (. obj method args...): desugar and
        # re-enter the dot special form (which holds the String surface, the
        # deftype method path, and the map-fn fallback).
        (if (and (> (length sym-name) 1)
                 (= (string/slice sym-name 0 1) ".")
                 (not= sym-name "..")
                 (> (length form) 1))
          (eval-form ctx bindings
                     (array/concat @[{:jolt/type :symbol :ns nil :name "."}
                                     (in form 1)
                                     {:jolt/type :symbol :ns nil :name (string/slice sym-name 1)}]
                                   (tuple/slice form 2)))
        # Handle ClassName. constructor syntax (".." is the member-threading
        # macro, not a constructor named ".")
        (if (and (> (length sym-name) 1) (not= sym-name "..")
                 (= (sym-name (- (length sym-name) 1)) 46))
          (let [type-name (string/slice sym-name 0 (- (length sym-name) 1))
                type-sym {:jolt/type :symbol :ns (first-form :ns) :name type-name}
                ctor (eval-form ctx bindings type-sym)
                # class names evaluate to canonical-name STRINGS now; the
                # constructor itself comes from the ctor registry
                ctor (if (string? ctor) (or (ctor-for-class-token ctor) ctor) ctor)
                args (map |(eval-form ctx bindings $) (tuple/slice form 1))]
            (apply ctor args))
          (let [v (resolve-var ctx bindings first-form)]
            (if (and v (var-macro? v))
              # Expand once (cached by call-form identity), then evaluate the
              # macro-free expansion with the current bindings each call.
              (let [cached (in macro-cache form)]
                (if (not (nil? cached))
                  (eval-form ctx bindings cached)
                  (let [expanded (apply (var-get v) (tuple/slice form 1))]
                    (put macro-cache form expanded)
                    (eval-form ctx bindings expanded))))
              (let [f (eval-form ctx bindings first-form)
                    args (map |(eval-form ctx bindings $) (tuple/slice form 1))]
                (jolt-invoke ctx f args))))))))
      (let [f (eval-form ctx bindings first-form)
            args (map |(eval-form ctx bindings $) (tuple/slice form 1))]
        (jolt-invoke ctx f args)))))

# Build a map value from an array of evaluated [k v k v ...]. A phm (not a Janet
# struct) is used when a key is a collection (value-based hashing) OR a key/value
# is nil (Janet structs drop nil; phm preserves it, matching Clojure). The common
# scalar/nil-free case stays a struct.
