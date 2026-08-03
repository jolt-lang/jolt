; Jolt Standard Library: clojure.test
;
; A practical subset of clojure.test for running real test suites under Jolt:
; deftest / is / testing / are / use-fixtures / run-tests, with class-aware
; (thrown? Class body) and (thrown-with-msg? Class re body) inside `is`. Class
; matching is by simple-name (last dotted segment), since Jolt has no JVM class
; objects — Exception/Throwable match any thrown value.
;
; Also exposes the counter/registry API the internal clojure-test-suite harness
; uses (reset-report!, run-registered, n-pass/n-fail/n-error, failures), so this
; is a drop-in superset.

(ns clojure.test
  (:require [clojure.string :as str]
            [clojure.template :as temp]))

;; --- state -----------------------------------------------------------------

(def counters (atom {:test 0 :pass 0 :fail 0 :error 0 :fails []}))
(def jolt-report counters)                ;; alias used by the suite harness
(def ctx-stack (atom []))
(def registry (atom []))                ;; [{:name sym :fn thunk}]
(def once-fixtures (atom {}))           ;; ns-sym -> [fixture-fns]
(def each-fixtures (atom {}))           ;; ns-sym -> [fixture-fns]

;; clojure.test/*testing-vars* — the stack of vars under test. Real clojure.test
;; binds it around each test var; test.check's default reporter reads it, so a
;; defspec run through its :test metadata doesn't blow up on an unbound var.
(def ^:dynamic *testing-vars* (list))
(def ^:dynamic *report-counters* nil)
;; the stack of testing strings, innermost first — bindable like the JVM's
;; (test.chuck rebinds it around its property reports).
(def ^:dynamic *testing-contexts* (list))

;; The assertion's source position, stashed by the `is` macro from (meta &form).
;; The reference computes :file/:line with a stack walk in do-report; jolt has no
;; walk, so `is` reads the reader metadata (line/column always, :file when the
;; form came from a required file) and do-report merges it into :fail/:error maps.
(def ^:dynamic *report-pos* nil)

;; Where test reporting goes. A library that captures test output binds this and
;; wraps its printing in with-test-out — test.check's clojure-test integration
;; does exactly that — so everything this namespace prints goes through it too.
(def ^:dynamic *test-out* *out*)

(defmacro with-test-out
  "Runs body with *out* bound to the value of *test-out*."
  [& body]
  `(binding [*out* *test-out*] ~@body))

;; Bind to false when loading production code and with-test / deftest / deftest- /
;; set-test create nothing.
(def ^:dynamic *load-tests* true)

;; Accepted for parity and inert here: jolt reports a crash by message, not by
;; printing a stack trace, so there is no depth to cap.
(def ^:dynamic *stack-trace-depth* nil)

(defn reset-report! []
  (reset! counters {:test 0 :pass 0 :fail 0 :error 0 :fails []})
  (reset! ctx-stack [])
  (reset! registry [])
  (reset! once-fixtures {})
  (reset! each-fixtures {}))

(defn- ctx-str []
  (if (seq *testing-contexts*)
    (str/join " " (reverse *testing-contexts*))
    (str/join " " @ctx-stack)))

;; Message of a thrown value: ex-info's message, else a raw host condition's text
;; (ex-message is nil for those), else its printed form — so a crash is never
;; reported with a blank message.
(defn err-text [e]
  (or (ex-message e)
      (jolt.host/condition-message e)
      (str e)))

(defn- report-value [v]
  ;; A throwable renders through its own toString, like the reference's
  ;; :error report — "clojure.lang.ExceptionInfo: boom {}", not the class token
  ;; followed by the message. jolt cannot print the frames underneath it (tail
  ;; calls leave none), which is the documented part of this divergence.
  (if (instance? Throwable v)
    (str v)
    (pr-str v)))

(defn- report-line [m]
  (str (when (:message m) (str (:message m)
                               (when (or (:form m) (contains? m :expected) (contains? m :actual)) " ")))
       (when (:form m) (pr-str (:form m)))
       (when (contains? m :expected) (str " expected: " (pr-str (:expected m))))
       (when (contains? m :actual) (str " actual: " (report-value (:actual m))))))

(defn testing-contexts-str
  "Returns a string representation of the current test context, innermost last."
  []
  (str/join " " (reverse *testing-contexts*)))

(defn source-file-name
  "Base name of the file being compiled, or nil outside a file load. The
  reference gets this off a stack frame; jolt reads *file*, which the loader
  binds to the full path."
  []
  (when-let [v (resolve '*file*)]
    (when-let [f (deref v)]
      (when (string? f)
        (let [i (str/last-index-of f "/")]
          (if i (subs f (inc i)) f))))))

(defn testing-vars-str
  "Returns a string representation of the current test: the names in
  *testing-vars* as a list, then the source file and line of the assertion."
  [m]
  (let [{:keys [file line]} m]
    (str (reverse (map (fn [v] (or (:name (meta v)) (:name v))) *testing-vars*))
         " (" file ":" line ")")))

(def *initial-report-counters* {:test 0, :pass 0, :fail 0, :error 0})

;; The reference's report methods bump *report-counters* and nothing else; jolt's
;; also feed a process-wide atom that the harnesses read through n-pass/n-fail/
;; failures. Keeping those separate is what stops a count landing twice: the
;; report methods call bump-counters! (ref only) and then inc-pass!/fail!/err!
;; (atom only).
(defn- bump-counters! [k]
  (when *report-counters*
    (dosync (commute *report-counters* assoc k (inc (or (@*report-counters* k) 0))))))

(defn inc-report-counter
  "Bump a counter by key. With *report-counters* bound this moves that ref, as
  the reference does; without one it moves jolt's process-wide tally, which is
  what a caller outside a run is asking for."
  [k]
  (if *report-counters*
    (bump-counters! k)
    (swap! counters update k (fnil inc 0))))

(defn inc-pass! [] (swap! counters update :pass inc))
(defn fail! [m]
  (let [line (str (ctx-str) (when (or (seq *testing-contexts*) (seq @ctx-stack)) " ") "FAIL: " (report-line m))]
    (swap! counters (fn [r] (-> r (update :fail inc) (update :fails conj line))))
    (with-test-out
      (println "\nFAIL in" (testing-vars-str m))
      (when (seq *testing-contexts*) (println (testing-contexts-str)))
      (when-let [message (:message m)] (println message))
      (println "expected:" (pr-str (:expected m)))
      (println "  actual:" (pr-str (:actual m))))))
(defn err! [m]
  (let [line (str (ctx-str) (when (or (seq *testing-contexts*) (seq @ctx-stack)) " ") "ERROR: " (report-line m))]
    (swap! counters (fn [r] (-> r (update :error inc) (update :fails conj line))))
    (with-test-out
      (println "\nERROR in" (testing-vars-str m))
      (when (seq *testing-contexts*) (println (testing-contexts-str)))
      (when-let [message (:message m)] (println message))
      (println "expected:" (pr-str (:expected m)))
      (println "  actual:" (report-value (:actual m))))))

(defn n-pass [] (:pass @counters))
(defn n-fail [] (:fail @counters))
(defn n-error [] (:error @counters))
(defn failures [] (:fails @counters))


;; clojure.test/report multimethod — present so suites that add reporting
;; methods (defmethod clojure.test/report :begin-test-var ...) load. The runner
;; below does its own console output and doesn't dispatch through it.
;;
;; ^:dynamic like the reference's: a suite that installs its own reporter for the
;; duration of a run rebinds this rather than adding methods (test.check's
;; clojure-test suite does, and so does every TAP/JUnit reporter).
(defmulti ^:dynamic report :type)
(defmethod report :default [_m] nil)

;; do-report routes a {:type …} report map through the report multimethod — the
;; seam clojure.test assertions emit through. The built-in :pass/:fail/:error
;; methods feed jolt's counters; a library can add report types (test.check's
;; ::trial/::shrunk/::complete) and they dispatch here.
;; A Throwable in :actual (an assertion that threw) renders as its class and
;; message rather than as printed data — pr-str of a condition says nothing
;; about what went wrong.

(defmethod report :pass [_m] (bump-counters! :pass) (inc-pass!))
(defmethod report :fail [m] (bump-counters! :fail) (fail! m))
(defmethod report :error [m] (bump-counters! :error) (err! m))
(defn do-report
  "Add source position to a test result and call report. The reference walks
  the stack for :file/:line; jolt's `is` macro stashes the assertion's position
  (from (meta &form)) in *report-pos*, merged here for :fail/:error reports."
  [m]
  (let [m (if (or (:file m) (:line m))
            m
            (if (and (or (= :fail (:type m)) (= :error (:type m))) *report-pos*)
              (merge *report-pos* m)
              m))]
    (report m)))

;; assert-expr is the macro-level extension point: `is` expands a form by calling
;; (assert-expr msg form), dispatched on the form's first symbol (or :default /
;; :always-fail). A library registers a custom assertion via
;; (defmethod assert-expr 'my-pred [msg form] <code returning an assertion form>).
;; 2-arg [msg form] signature matches clojure.test. `is` routes here only for a
;; symbol with an explicitly registered method, so built-in forms are unaffected.
(defmulti assert-expr (fn [_msg form]
                        (cond (nil? form) :always-fail
                              (and (seq? form) (symbol? (first form))) (first form)
                              :else :default)))
(defmethod assert-expr :always-fail [msg form]
  `(clojure.test/do-report {:type :fail :message ~msg :form '~form}))
(defmethod assert-expr :default [msg form]
  `(try
     (if ~form
       (clojure.test/do-report {:type :pass})
       (clojure.test/do-report {:type :fail :message ~msg :form '~form}))
     (catch Throwable e#
       (clojure.test/do-report {:type :error :message ~msg :form '~form
                                :actual (clojure.test/err-text e#)}))))

;; The building blocks a library uses when it writes its own assert-expr method:
;; assert-predicate for a functional predicate (args evaluated so the report shows
;; the values), assert-any for anything else, try-expr to wrap either so an
;; unexpected throw reports :error instead of escaping.
(defn assert-predicate
  "Returns generic assertion code for any functional predicate. :expected is the
  original form, :actual the form with its sub-forms evaluated."
  [msg form]
  (let [args (rest form)
        pred (first form)]
    `(let [values# (list ~@args)
           result# (apply ~pred values#)]
       (if result#
         (clojure.test/do-report {:type :pass :message ~msg
                                  :expected '~form :actual (cons ~pred values#)})
         (clojure.test/do-report {:type :fail :message ~msg :form '~form
                                  :expected '~form
                                  :actual (list '~'not (cons '~pred values#))}))
       result#)))

(defn assert-any
  "Returns generic assertion code for any test, including macros, host method
  calls, or isolated symbols."
  [msg form]
  `(let [value# ~form]
     (if value#
       (clojure.test/do-report {:type :pass :message ~msg
                                :expected '~form :actual value#})
       (clojure.test/do-report {:type :fail :message ~msg :form '~form
                                :expected '~form :actual value#}))
     value#))

(defmacro try-expr
  "Used by `is` to catch unexpected exceptions. You don't call this."
  [msg form]
  `(try ~(assert-expr msg form)
        (catch Throwable t#
          (clojure.test/do-report {:type :error :message ~msg :form '~form
                                   :expected '~form
                                   :actual (clojure.test/err-text t#)}))))

;; The common pure predicates whose args `is` evaluates so a failure shows the
;; actual values — (is (= expected got)) prints `got`, not just the form. A macro
;; head (not in this set) keeps the plain form-only path.
(def ^:private reported-preds
  '#{= not= == < > <= >= identical? contains? instance? nil? some? empty? even? odd? pos? neg? zero?})

;; --- class matching for thrown? --------------------------------------------

(defn- last-seg [s]
  (let [s (str s)
        i (str/last-index-of s ".")]
    (if i (subs s (inc i)) s)))

(defn class-match?
  "True when a raw Chez condition (no mapped jolt throwable class) was caught via
  __catch-broad? and the wanted class is one of the three universal triage types:
  Throwable, Exception, or RuntimeException. R3's typed throws + this round's
  Class value model let instance? cover everything else."
  [e wanted]
  (let [w (last-seg wanted)]
    (and (or (= w "Exception") (= w "Throwable") (= w "RuntimeException"))
         (not (instance? Throwable e)))))

;; --- assertion macros ------------------------------------------------------

(defn- thrown-form? [form sym]
  (and (seq? form) (symbol? (first form)) (= sym (name (first form)))))

(defmacro is-impl
  ([form] `(is-impl ~form nil))
  ([form msg]
   (cond
     ;; a library-registered custom assertion (the assert-expr extension point)
     ;; wins over every inline path, like clojure.test, where each `is` dispatches
     ;; assert-expr on the exact head symbol and the built-ins are just
     ;; pre-registered methods. In particular a registered alias-qualified
     ;; `p/thrown?` must not be captured by the by-name thrown? path below.
     (and (seq? form) (symbol? (first form))
          (contains? (methods clojure.test/assert-expr) (first form)))
     (clojure.test/assert-expr msg form)

      ;; (is (thrown? Class body...))
      (thrown-form? form "thrown?")
      (let [klass-sym (second form)
            klass (name klass-sym)
            body  (nthrest form 2)]
        `(try
           ~@body
           (clojure.test/do-report {:type :fail :message (str "expected " '~form " to throw" (when ~msg (str " — " ~msg)))
                                    :expected '~form :actual nil})
           (catch Throwable e#
             ;; instance? honors the exception hierarchy (a literal class symbol), so
             ;; (thrown? IllegalArgumentException …) matches an ArityException subclass
             ;; like the JVM; class-match? is the simple-name fallback for a class jolt
             ;; models only by name.
             (if (or (clojure.core/instance? ~klass-sym e#)
                     (clojure.test/class-match? e# ~klass))
               (clojure.test/do-report {:type :pass :message ~msg :expected '~form :actual e#})
               (clojure.test/do-report {:type :fail :message (str "expected throw of " ~klass " but got " (clojure.core/class e#))
                                        :expected '~form :actual e#})))))

      ;; (is (thrown-with-msg? Class re body...))
      (thrown-form? form "thrown-with-msg?")
      (let [klass-sym (second form)
            klass (name klass-sym)
            re    (nth form 2)
            body  (nthrest form 3)]
        `(try
           ~@body
           (clojure.test/do-report {:type :fail :message (str "expected " '~form " to throw")
                                    :expected '~form :actual nil})
           (catch Throwable e#
             (let [m# (or (clojure.core/ex-message e#) (str e#))]
               ;; honor the class hierarchy (ExceptionInfo IS a RuntimeException),
               ;; then fall back to a simple-name match like thrown? does.
               (if (and (or (clojure.core/instance? ~klass-sym e#)
                            (clojure.test/class-match? e# ~klass))
                        (re-find ~re m#))
                 (clojure.test/do-report {:type :pass :message ~msg :expected '~form :actual e#})
                 (clojure.test/do-report {:type :fail :message (str "expected throw of " ~klass " matching " ~re " but got " (clojure.core/class e#) ": " m#)
                                          :expected '~form :actual e#}))))))

      ;; instance? gets a dedicated report path for a clearer fail message
      ;; (mirrors thrown? above); it is a function now, but keep the explicit form.
      (and (seq? form) (= 'instance? (first form)))
      `(try
         (let [object# ~(nth form 2)
               result# (instance? ~(second form) object#)]
           ;; :actual is the object's CLASS either way, like clojure.test's own
           ;; instance? assertion — "expected a String, got a Long" is the useful
           ;; report, not "got false".
           (if result#
             (clojure.test/do-report {:type :pass :message ~msg :expected '~form
                                      :actual (clojure.core/class object#)})
             (clojure.test/do-report {:type :fail :message ~msg :form '~form
                                      :expected '~form :actual (clojure.core/class object#)}))
           result#)
         (catch Throwable e#
           (clojure.test/do-report {:type :error :message ~msg :form '~form
                                    :expected '~form :actual e#})))

      ;; a predicate call — (= a b), (< x y), (pred? v): evaluate the args so a
      ;; failure shows the actual values, like clojure.test's assert-predicate.
      ;; :expected is the form as written and :actual the form with its arguments
      ;; evaluated, which is the contract every clojure.test reporter reads —
      ;; a report that folded them into :message left a custom reporter (CIDER's
      ;; test op, test.check, matcher-combinators) with nothing to show.
      (and (seq? form) (contains? clojure.test/reported-preds (first form)))
      `(try
         (let [vs# (list ~@(rest form))
               result# (apply ~(first form) vs#)]
           (if result#
             (clojure.test/do-report {:type :pass :message ~msg
                                      :expected '~form :actual (cons '~(first form) vs#)})
             (clojure.test/do-report {:type :fail :message ~msg :form '~form
                                      :expected '~form
                                      :actual (list '~'not (cons '~(first form) vs#))}))
           result#)
         (catch Throwable e#
           (clojure.test/do-report {:type :error :message ~msg :form '~form
                                    :expected '~form :actual e#})))

     ;; `is` yields the value it tested (clojure.test's does), so it composes:
     ;; (let [x (is (find-thing))] …)
     :else
     `(try
        (let [value# ~form]
          (if value#
            (clojure.test/do-report {:type :pass :message ~msg :expected '~form :actual value#})
            (clojure.test/do-report {:type :fail :message ~msg :form '~form
                                     :expected '~form :actual value#}))
          value#)
        (catch Throwable e#
          (clojure.test/do-report {:type :error :message ~msg :form '~form
                                   :expected '~form :actual e#}))))))

(defmacro is
  "Test any expression, returning true if it does not throw or returns
   logical true. Stashes the assertion's source position (from (meta &form))
   so :fail/:error reports carry the reference's (file:line) header."
  ([form] `(is ~form nil))
  ([form msg]
   ;; is-impl is a MACRO, so emit a CALL to it and let it expand in the normal
   ;; pipeline — invoking it here (~(is-impl form msg)) would apply the macro as
   ;; a function and its assert-expr dispatch would never run.
   ;;
   ;; The reader stamps :line/:column on the form; the file comes from *file*,
   ;; reduced to its base name because that is what the reference prints and what
   ;; tooling matches on (test.check asserts #"\(clojure_test_test\.cljc:\d+\)$").
   (let [pos (assoc (select-keys (meta &form) [:line])
                    :file (source-file-name))]
     `(binding [clojure.test/*report-pos* ~pos]
        (is-impl ~form ~msg)))))

(defmacro testing [s & body]
  `(binding [clojure.test/*testing-contexts* (conj clojure.test/*testing-contexts* ~s)]
     ~@body))



(defmacro deftest [name & body]
  (when *load-tests*
    `(do
       (defn ~name [] ~@body)
       ;; the var carries :test metadata like clojure.test's deftest, so tooling
       ;; that discovers tests by scanning var meta finds it.
       (alter-meta! (var ~name) assoc :test ~name)
       (swap! clojure.test/registry conj {:name '~name
                                          :ns (clojure.core/ns-name clojure.core/*ns*)
                                          :fn ~name})
       (var ~name))))

(defmacro deftest-
  "Like deftest but the var is private."
  [name & body]
  (when *load-tests*
    `(doto (clojure.test/deftest ~name ~@body)
       (alter-meta! assoc :private true))))

;; with-test attaches a test body as :test metadata on a var-defining form (which
;; must return the var), like clojure.test's — schema's tests wrap s/defn this way.
(defmacro with-test [definition & body]
  (if *load-tests*
    `(doto ~definition (alter-meta! assoc :test (fn [] ~@body)))
    definition))

(defmacro set-test
  "Sets the :test metadata of an existing var to a fn with the given body. Does
  not change the var's value. Ignored when *load-tests* is false."
  [name & body]
  (when *load-tests*
    `(alter-meta! (var ~name) assoc :test (fn [] ~@body))))

;; Template substitution (not let-binding), so argv symbols substitute inside
;; quote and nested forms: (are [x] (special-symbol? 'x) if def) tests 'if.
(defmacro are [argv expr & args]
  (if (or (and (empty? argv) (empty? args))
          (and (pos? (count argv))
               (pos? (count args))
               (zero? (mod (count args) (count argv)))))
    `(clojure.template/do-template ~argv (clojure.test/is ~expr) ~@args)
    (throw (IllegalArgumentException.
            "The number of args doesn't match are's argv or neither are empty"))))

;; --- fixtures + run --------------------------------------------------------

;; Fixtures are per-namespace, like clojure.test (which stores them in ns
;; metadata): use-fixtures records them under the calling ns, and only that
;; ns's tests run through them — a suite loading many test namespaces into one
;; process doesn't cross-apply or clobber another ns's fixtures.
(defn use-fixtures [kind & fns]
  (let [n (ns-name *ns*)]
    (cond
      (= kind :once) (swap! once-fixtures assoc n (vec fns))
      (= kind :each) (swap! each-fixtures assoc n (vec fns)))))

(defn- wrap-fixtures [fixtures body-fn]
  (if (empty? fixtures)
    (body-fn)
    ((first fixtures) (fn [] (wrap-fixtures (rest fixtures) body-fn)))))

(defn compose-fixtures
  "Composes two fixture functions into one that combines their behavior."
  [f1 f2]
  (fn [g] (f1 (fn [] (f2 g)))))

(defn join-fixtures
  "Composes a collection of fixtures, in order. Always returns a valid fixture
  function, even for an empty collection."
  [fixtures]
  (reduce compose-fixtures (fn [f] (f)) fixtures))

(defn- run-one [t]
  (bump-counters! :test)
  (swap! counters update :test inc)
  (wrap-fixtures (get @each-fixtures (:ns t) [])
    (fn []
      ;; bind *testing-vars* the way test-var does, so a failure inside a
      ;; registry-run test still names it in the "FAIL in (name)" header. It must
      ;; be the real VAR: test.check's reporter reads this stack and treats the
      ;; entries as vars, so a stand-in map fails as "cannot be cast to Named".
      ;; A test whose var no longer resolves leaves the stack alone.
      (binding [*testing-vars* (let [v (try (ns-resolve (:ns t) (:name t))
                                            (catch Throwable _ nil))]
                                 (if v (conj *testing-vars* v) *testing-vars*))]
        (try
          ((:fn t))
          (catch Throwable e
            (err! {:type :error
                   :message (str (:name t) " crashed")
                   :expected nil :actual e})))))))

;; A registered test still counts only while its var carries :test metadata.
;; clojure.test discovers tests by scanning vars for that key, so removing it is
;; how tooling DESELECTS a test — the Cognitect test-runner's -v/-i/-e options
;; dissoc :test from every var that doesn't match and restore it afterwards.
;; Running straight from the registry ignored that, so those options silently
;; selected nothing and every test ran. A var that no longer resolves is kept:
;; the registry is the only record of it, and dropping it would lose a test.
(defn- selected? [t]
  (let [v (try (ns-resolve (:ns t) (:name t)) (catch Throwable _ nil))]
    (or (nil? v) (some? (:test (meta v))))))

;; Run the registered tests grouped by namespace (registration order preserved
;; within each ns), each group wrapped in its ns's :once fixtures. ns-set nil
;; means all.
(defn- run-selected [ns-set]
  (let [ts (filter selected?
                   (if ns-set (filter (fn [t] (contains? ns-set (:ns t))) @registry) @registry))]
    (doseq [n (distinct (map :ns ts))]
      (wrap-fixtures (get @once-fixtures n [])
        (fn [] (doseq [t ts :when (= n (:ns t))] (run-one t))))))
  nil)

;; Tests attached to a namespace's vars via :test metadata but never registered
;; through deftest — clojure.test discovers tests by scanning ns-interns, so a
;; suite that interns test vars directly (yamltest-style intern + vary-meta)
;; must be visible to (run-tests 'ns) too. deftest'd vars also carry :test
;; meta, so names already in the registry are excluded.
(defn- interned-tests [n]
  (let [known (set (map :name (filter #(= n (:ns %)) @registry)))]
    (->> (ns-interns n)
         (keep (fn [[s v]]
                 (when-let [t (:test (meta v))]
                   (when-not (contains? known s)
                     {:name s :ns n :fn t}))))
         (sort-by (fn [t] (str (:name t)))))))

(defn run-registered [] (run-selected nil))

;; (run-tests 'ns1 'ns2 …) runs only those namespaces' tests, like clojure.test.
;; With no args it runs everything registered (a deliberate superset of the
;; JVM's current-ns default — jolt's harnesses load then run whole suites).
;; Prints and returns THIS call's summary; the global counters stay cumulative
;; for the n-pass/n-fail harness API.
(defn run-tests [& nses]
  (let [before @counters
        ns-syms (map (fn [n] (if (symbol? n) n (ns-name n))) nses)
        ns-set (when (seq ns-syms) (set ns-syms))]
    (run-selected ns-set)
    ;; interned (:test meta) tests discovered up front and run inside the same
    ;; :once fixtures as registered tests, matching JVM test-ns.
    (doseq [n ns-syms
            :let [its (interned-tests n)]
            :when (seq its)]
      (wrap-fixtures (get @once-fixtures n [])
        (fn [] (doseq [t its] (run-one t)))))
    (let [r @counters
          d {:type :summary
             :test  (- (:test r)  (:test before))
             :pass  (- (:pass r)  (:pass before))
             :fail  (- (:fail r)  (:fail before))
             :error (- (:error r) (:error before))}]
      (with-test-out
        (println)
        (println (str "Ran " (:test d) " tests. "
                      (:pass d) " assertions passed, "
                      (:fail d) " failures, " (:error d) " errors.")))
      d)))

;; --- var-level API (clojure.test parity) -------------------------------------


(defn test-var
  "Run the test attached to var v via its :test metadata, with *testing-vars*
  bound like clojure.test."
  [v]
  (when-let [t (:test (meta v))]
    (binding [*testing-vars* (conj *testing-vars* v)]
      (bump-counters! :test)
      (swap! counters update :test inc)
      (try
        (t)
        (catch Throwable e
          (err! {:type :error
                 :message (str (:name (meta v)) " crashed")
                 :expected nil :actual e}))))))

(defn test-vars
  "Run the vars' :test fns, each namespace group wrapped in its :once fixtures
  and each var in its :each fixtures."
  [vars]
  (doseq [[n vs] (group-by (fn [v] (:ns (meta v))) vars)]
    (let [n (cond (nil? n) nil
                  (symbol? n) n
                  :else (ns-name n))]
      (wrap-fixtures (get @once-fixtures n [])
        (fn []
          (doseq [v vs]
            (wrap-fixtures (get @each-fixtures n [])
              (fn [] (test-var v)))))))))

(defmacro run-test
  "Run a single test var: (run-test my-test)."
  [v]
  `(clojure.test/test-var (var ~v)))

(defn run-test-var
  "Run a single test var, given the var itself."
  [v]
  (test-vars [v]))

(defn get-possibly-unbound-var
  "Like var-get but returns nil if the var is unbound."
  [v]
  (try (var-get v) (catch Throwable _ nil)))

(defn function?
  "True when x is a function, or a symbol resolving to one (not a macro)."
  [x]
  (if (symbol? x)
    (when-let [v (resolve x)]
      (when-let [value (get-possibly-unbound-var v)]
        (and (fn? value) (not (:macro (meta v))))))
    (fn? x)))

(defn successful?
  "True when the test summary reports no failures and no errors."
  [summary]
  (and (zero? (:fail summary 0)) (zero? (:error summary 0))))

(defn test-all-vars
  "Calls test-vars on every var interned in the namespace, with fixtures."
  [n]
  (test-vars (vals (ns-interns (if (symbol? n) n (ns-name n))))))

(defn test-ns
  "If the namespace defines test-ns-hook, calls that; otherwise tests every var in
  it. Returns this call's summary counts.

  Unlike the reference this does not bind *report-counters* to a fresh ref —
  jolt's counters live in one atom and the summary is a before/after delta — so a
  reporter that pokes at *report-counters* sees the cumulative atom rather than a
  per-namespace ref."
  [n]
  (let [before @counters
        ns-sym (if (symbol? n) n (ns-name n))]
    (if-let [hook (find-var (symbol (str ns-sym) "test-ns-hook"))]
      ((var-get hook))
      (test-all-vars ns-sym))
    (let [r @counters]
      {:type :summary
       :test  (- (:test r)  (:test before))
       :pass  (- (:pass r)  (:pass before))
       :fail  (- (:fail r)  (:fail before))
       :error (- (:error r) (:error before))})))

(defn run-all-tests
  "Runs the tests in every loaded namespace, or in those whose name matches re."
  ([] (apply run-tests (map ns-name (all-ns))))
  ([re] (apply run-tests (filter (fn [n] (re-matches re (name n)))
                                 (map ns-name (all-ns))))))
