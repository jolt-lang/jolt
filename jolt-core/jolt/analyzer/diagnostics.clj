(ns jolt.analyzer.diagnostics
  "The analyzer's compile-time diagnostics: the per-compilation boxes that say
  WHERE analysis is (innermost positioned form, macro expansion, running macro),
  the kinded ex-info every analyzer error is raised as, the \"did you mean?\"
  unresolved-symbol report, and the wrapper that turns any throw out of analysis
  into a positioned diagnostic. No analysis of its own — jolt.analyzer and its
  helper namespaces raise through it."
  (:require [jolt.host :refer [form-position compile-ns resolvable-names]]))

;; The innermost list form currently being analyzed that carries reader position
;; metadata — what a diagnostic raised anywhere below it should name. Without it
;; the only position available to the reporter is the one the LOADER records per
;; TOP-LEVEL form, so an unresolved symbol 280 lines inside a defn reported the
;; defn's own first line.
;;
;; The form is stored, not its position map: hc-form-position allocates, and this
;; is written for every positioned list form the analyzer walks, whereas the map is
;; wanted only if something actually throws. analyze-list saves and restores it
;; around its children so a completed sibling subtree cannot leave a deeper
;; position behind; the reference compiler does the same thing by pushing thread
;; bindings of LINE/COLUMN in analyzeSeq, which is strictly more expensive.
;;
;; Only forms that HAVE a line are recorded, so descending through a macro-built
;; form (which carries none) keeps the nearest enclosing real position rather than
;; erasing it.
;;
;; The box is per COMPILATION, bound by jolt.analyzer/analyze, not one atom for the
;; process. Shared, two threads compiling at once overwrote each other between the
;; save and the restore, and a diagnostic from one namespace reported a line from
;; whatever the other thread happened to be analyzing. The var holds the box rather
;; than the form so the hot path stays what it was — one atom write per positioned
;; form, not a thread-binding push per form, which is the cost the reference
;; compiler pays to bind LINE/COLUMN in analyzeSeq.
(def ^:dynamic *positioned-form-box* nil)

;; The innermost macro expansion currently being analyzed, as [macro-name form],
;; or nil. A diagnostic raised inside expanded code names the MACRO CALL — the
;; expansion has no position of its own — which leaves the reader looking at a
;; form that is correct as written, with no hint that the error came from code
;; the macro generated. Recording which macro was expanding turns that into a
;; note: "expanded from (my-def ...)".
;;
;; Stored like the position box, and for the same reason: written per expansion,
;; read only when something throws.
(def ^:dynamic *expansion-box* nil)

;; The macro whose expander is RUNNING, as [qualified-symbol head-as-written
;; form], or nil between runs. Distinct from the expansion box, which covers analysis of what
;; a macro returned: this one covers the macro function's own execution, which
;; is the one window the reference compiler files under a different PHASE
;; (:macro-syntax-check / :macroexpansion, naming the macro) from everything
;; the analyzer itself raises (:compile-syntax-check). Set before the expander
;; is applied and cleared after, so a throw out of the expander leaves the
;; macro's name in the box for the diagnostic to read.
(def ^:dynamic *macro-run-box* nil)

;; The position a diagnostic should carry: the innermost positioned form's, or nil
;; when nothing under analysis had reader metadata (a macro-built form, or a form
;; handed straight to eval). nil box means nothing is under analysis on this thread.
(defn current-form-position []
  (let [f (when *positioned-form-box* @*positioned-form-box*)]
    (when (some? f) (form-position f))))

;; Raise a compile-time diagnostic. Two things this fixes over the bare string it
;; replaces — (throw (str "jolt/uncompilable: " why)):
;;
;; It is a real THROWABLE. A program that caught a compile error got a Scheme
;; string: (class e) answered String, (ex-message e) nil, (instance? Exception e)
;; false. The message survived only because the uncaught reporter pr-str'd the raw
;; value, so the report looked right while nothing could read it.
;;
;; It carries a KIND — a namespaced keyword registered in
;; test/conformance/error-kinds.edn, gated by `make errorkinds` — so tooling and
;; the reporter can key on the error rather than parse its prose, and so the
;; wording can be improved without breaking either.
;;
;; :type stays :analysis-error for the reporter, which already keys on it to drop
;; the analyzer's own recursion from the trace.
;; The diagnostic keys are namespaced and FLAT — :jolt.error/kind beside
;; :jolt.error/line, not a nested map under one key. Namespacing is what keeps
;; jolt's metadata from colliding with the thrower's own ex-data, which this
;; preserves; nesting would have been solving a problem the namespace already
;; solves. The reference spells its own the same way (:clojure.error/line).
(defn- current-expansion []
  (when *expansion-box* @*expansion-box*))

(defn- current-macro-run []
  (when *macro-run-box* @*macro-run-box*))

;; How the reference spells :clojure.error/symbol for a throw out of a macro's
;; run depends on WHOSE complaint it is. Its own check on the macro's arguments
;; (the spec check, checkSpecs) names the VAR — clojure.core/let for a bare
;; `let`; anything the expander itself threw names the head AS WRITTEN — mm,
;; u/mm, user/mm — the op of the form being expanded. jolt's kinded
;; diagnostics out of let/defn/fn's own argument checks are the counterpart of
;; the first; the :analyze/internal-failure kind is exactly "the expander
;; threw something else" (as-analysis-diagnostic), and gets the second.
(defn- macro-run-symbol [run kind]
  (if (= kind :analyze/internal-failure) (second run) (first run)))

;; The reference compiler's phase for what E raised. Everything the analyzer
;; says about a form is :compile-syntax-check. What a macro's own run raised is
;; :macro-syntax-check when it is the kind of complaint a macro makes about its
;; arguments — the classes Compiler.macroexpand1 files there: IllegalArgument,
;; IllegalState, ExceptionInfo and a bare Exception — and :macroexpansion for
;; anything else escaping the expander. The analyzer's own diagnostics
;; (analysis-error, E nil) never come out of a running expander: a macro that
;; re-enters the analyzer gets a fresh macro-run box (see analyze).
(defn- error-phase [e]
  (if (current-macro-run)
    (if (or (nil? e)
            (instance? IllegalArgumentException e)
            (instance? IllegalStateException e)
            (instance? clojure.lang.ExceptionInfo e)
            (= (class e) Exception))
      :macro-syntax-check
      :macroexpansion)
    :compile-syntax-check))

;; The reference's own keys ride beside jolt's, so a JVM-portable reader of a
;; compile error — clojure.main/ex-triage and everything built on it (REPL
;; :caught hooks, test runners, editor middleware) — finds the phase and
;; position under the names it already knows. The two spellings differ in
;; one respect: the reference WRAPS the cause in a CompilerException carrying
;; these keys, where jolt raises the one positioned throwable, with the cause
;; as ITS cause (as-analysis-diagnostic); so ex-triage reads the phase off the
;; top of the chain and the class/message off its end, on both.
;; :clojure.error/source is the file when the position has one and absent
;; otherwise, the reference's shape for a form read with no path.
(defn- reference-error-keys [m pos e kind]
  (let [run (current-macro-run)]
    (cond-> (assoc m :clojure.error/phase (error-phase e))
      (:file pos) (assoc :clojure.error/source (:file pos))
      (:line pos) (assoc :clojure.error/line (:line pos))
      (:column pos) (assoc :clojure.error/column (:column pos))
      run (assoc :clojure.error/symbol (macro-run-symbol run kind)))))

(defn diagnostic-data
  ([kind pos extra] (diagnostic-data kind pos extra nil))
  ([kind pos extra e]
  (let [exp (current-expansion)]
    (cond-> (reference-error-keys (or extra {}) pos e kind)
      true (assoc :jolt.error/kind kind :jolt.error/type :analysis-error)
      ;; Name the macro ONLY when the position being reported is the macro call
      ;; itself. That is exactly the case where the failing form was GENERATED —
      ;; it has no position of its own, so the innermost positioned form is the
      ;; call — and exactly the case where the reader is otherwise left staring
      ;; at a line that is correct as written.
      ;;
      ;; When the user's own form is what failed, it has its own position and is
      ;; not the call, so no note: (if) inside a defn body is the user's `if`,
      ;; not something defn generated, even though it is analyzed inside defn's
      ;; expansion. Without this test every diagnostic in ordinary code was
      ;; labelled "expanding the `defn` macro", which is noise.
      ;; Compared by POSITION, not object identity: a macro that builds its
      ;; expansion with syntax-quote propagates the call site's line onto the
      ;; generated form, so the form being reported is a different object that
      ;; nonetheless sits exactly where the call does. Identity said "not the
      ;; call" and dropped the note on the very case it exists for.
      (and exp (= pos (form-position (second exp))))
      (assoc :jolt.error/macro (first exp))
      (:line pos) (assoc :jolt.error/line (:line pos))
      (:column pos) (assoc :jolt.error/column (:column pos))
      (:file pos) (assoc :jolt.error/file (:file pos))))))

(defn analysis-error
  ([kind msg] (analysis-error kind msg nil))
  ([kind msg extra]
   (throw (ex-info msg (diagnostic-data kind (current-form-position) extra)))))

;; --- unresolved-symbol diagnostics ("did you mean?") ------------------------
;; A Carp-inspired touch: when a bare symbol doesn't resolve, offer the closest
;; in-scope names by Levenshtein distance instead of a bare "unable to resolve".

;; Levenshtein edit distance over two strings, computed on char vectors (jolt's
;; strings are codepoint-modeled, so index via vec, not nth-on-string). Only ever
;; called on the error path, so the O(la*lb) DP is fine.
(defn- edit-distance [sa sb]
  (let [a (vec sa) b (vec sb) la (count a) lb (count b)]
    (cond
      (zero? la) lb
      (zero? lb) la
      :else
      (loop [i 1 prev (vec (range (inc lb)))]
        (if (> i la)
          (peek prev)
          (let [ca (nth a (dec i))
                row (reduce
                      (fn [cur j]
                        (let [cost (if (= ca (nth b (dec j))) 0 1)]
                          (conj cur (min (inc (peek cur))          ; insertion
                                         (inc (nth prev j))        ; deletion
                                         (+ cost (nth prev (dec j))))))) ; substitution
                      [i]
                      (range 1 (inc lb)))]
            (recur (inc i) row)))))))

;; Distance a candidate may be from the typed name to count as a "did you mean":
;; scales with length so a 2-char name can't match everything, and a longer name
;; tolerates a couple of typos.
(defn- suggest-threshold [n]
  (cond (<= n 3) 1 (<= n 6) 2 :else 3))

;; Up to 3 candidate names closest to nm within the threshold, nearest first,
;; ties broken alphabetically for a stable message.
(defn- suggestions-for [nm candidates]
  (let [thr (suggest-threshold (count nm))
        scored (filter (fn [p] (<= (second p) thr))
                       (map (fn [c] [c (edit-distance nm c)])
                            (remove #(= % nm) (distinct candidates))))
        ordered (sort (fn [a b]
                        (if (= (second a) (second b))
                          (compare (first a) (first b))
                          (compare (second a) (second b))))
                      scored)]
    (mapv first (take 3 ordered))))

;; The names an unresolved symbol is matched against: everything resolvable from
;; the compile ns (its own interns + clojure.core's publics, via the host) plus
;; the lexical locals in scope.
(defn- candidate-pool [ctx env]
  (concat (seq (resolvable-names ctx)) (seq (:locals env))))

;; Throw the structured "unable to resolve symbol" diagnostic. The human message
;; keeps the JVM wording (with any suggestions appended); the ex-data carries a
;; machine-readable :jolt.error/* keys the CLI reporter emits as EDN under
;; JOLT_DIAG=edn, so editors/tools get the symbol, suggestions, and ns as data.
;; :line/:column/:file come from the innermost enclosing positioned form, so the
;; report names where the unknown name is WRITTEN. The reporter otherwise falls
;; back to the loader's per-top-level-form position, which in a long defn is its
;; opening line and says nothing about the offending expression.
(defn resolve-error [ctx nm env]
  (let [sugg (suggestions-for nm (candidate-pool ctx env))
        base (str "Unable to resolve symbol: " nm " in this context")
        msg (if (seq sugg)
              (str base " (did you mean " (apply str (interpose ", " sugg)) "?)")
              base)
        extra {:jolt.error/symbol nm
               :jolt.error/suggestions (vec sugg)
               :jolt.error/ns (compile-ns ctx)}]
    (throw (ex-info msg (diagnostic-data :analyze/unresolved-symbol
                                         (current-form-position) extra)))))

;; Anything raised while analyzing a form is a COMPILE-time failure, and the
;; reporter can only tell — and only knows where to point — when the throw carries
;; a :jolt.error/kind. With one it names the innermost positioned form and drops the
;; analyzer's own recursion from the trace; without one it falls back to the
;; LOADER's per-top-level-form position and prints thirty lines of jolt internals.
;; resolve-error was the only thing that built one, so every other compile failure
;; — an uncompilable form, a destructuring pattern the desugarer rejects, a macro
;; that threw while expanding — reported the top-level form's opening line over a
;; trace naming nothing the reader can act on.
;;
;; Attaching it here, at the one entry every top-level analysis goes through, costs
;; a single try per top-level form and covers all of them. A throw that already
;; carries a POSITIONED kind passes through untouched: it knows where it failed.
(defn- throw-message [e]
  (cond (string? e) e
        ;; an ex-info reports as its message alone; anything else keeps the
        ;; "java.lang.Foo: msg" text the reporter would have printed for it
        (some? (ex-data e)) (ex-message e)
        :else (str e)))

;; No position to add (a macro-built form, or a form handed straight to eval) means
;; nothing to improve on for a throw that is not yet a diagnostic, so leave it
;; exactly as it was rather than trading its trace for a diagnostic that says no
;; more than the fallback already does.
;; The rebuilt ex-info keeps the ORIGINAL ex-data and adds the :jolt.error/* keys.
;; Replacing it outright discarded whatever the thrower attached: a macro that
;; raised (ex-info "m" {:orig true}) reported no :orig at all, and any library
;; that hangs explain-data or a error code off its compile-time throw lost it the
;; moment the form was analyzed. The position and kind are jolt's to add, not the
;; thrower's data to overwrite.
;; It also keeps the ORIGINAL THROWABLE, as its cause. The reference's
;; CompilerException wraps what the macro threw, so the class survives: a
;; ClassCastException out of an expander is still one at the end of the cause
;; chain, and clojure.main/ex-triage names it (and its bare message) from
;; there. Folded into the message alone, as this used to do, it read back as an
;; ExceptionInfo whose cause line was "java.lang.ClassCastException: msg". A
;; kinded diagnostic rebuilt only to gain its position keeps its own cause
;; rather than becoming one: it IS the throwable, not a wrapper around it.
(defn as-analysis-diagnostic [e]
  (let [pos (current-form-position)
        orig (ex-data e)
        kind (when (map? orig) (:jolt.error/kind orig))]
    (cond
      ;; Already positioned: it knows where it happened better than the box does.
      (and kind (:jolt.error/line orig)) e
      ;; A diagnostic with a KIND but no position. Raised from a macro — the
      ;; `let`/`loop` destructurer and `defn`'s clause check are clojure.core code
      ;; and cannot reach the analyzer's position box — so it names the error but
      ;; not the place. Filling the position in here is what the box is for; the
      ;; earlier "already a diagnostic, leave it alone" test skipped these
      ;; entirely, and (defn f [] (let [a 1 b] a)) reported the DEFN's line 3
      ;; where the reference names the let on line 4. Its own kind is kept.
      ;; Rebuilt even with no position to add: it is a diagnostic already (the
      ;; reporter shows no trace for one either way), and the phase and the
      ;; macro it came out of are worth carrying on their own.
      kind (ex-info (throw-message e) (diagnostic-data kind pos orig e) (ex-cause e))
      (nil? pos) e
      ;; No diagnostic at all: anything else raised while analyzing. Keep the
      ;; thrower's ex-data and add the position beside it.
      :else (ex-info (throw-message e)
                     (diagnostic-data :analyze/internal-failure pos
                                      (when (map? orig) orig) e)
                     (when (instance? Throwable e) e)))))
