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

(defn inc-pass! [] (swap! counters update :pass inc))
(defn fail! [form]
  (let [line (str (ctx-str) (when (or (seq *testing-contexts*) (seq @ctx-stack)) " ") "FAIL: " form)]
    (swap! counters (fn [r] (-> r (update :fail inc) (update :fails conj line))))
    (println line)))
(defn err! [form]
  (let [line (str (ctx-str) (when (or (seq *testing-contexts*) (seq @ctx-stack)) " ") "ERROR: " form)]
    (swap! counters (fn [r] (-> r (update :error inc) (update :fails conj line))))
    (println line)))

(defn n-pass [] (:pass @counters))
(defn n-fail [] (:fail @counters))
(defn n-error [] (:error @counters))
(defn failures [] (:fails @counters))

;; Message of a thrown value: ex-info's message, else a raw host condition's text
;; (ex-message is nil for those), else its printed form — so a crash is never
;; reported with a blank message.
(defn err-text [e]
  (or (ex-message e)
      (jolt.host/condition-message e)
      (str e)))

;; clojure.test/report multimethod — present so suites that add reporting
;; methods (defmethod clojure.test/report :begin-test-var ...) load. The runner
;; below does its own console output and doesn't dispatch through it.
(defmulti report :type)
(defmethod report :default [_m] nil)

;; do-report routes a {:type …} report map through the report multimethod — the
;; seam clojure.test assertions emit through. The built-in :pass/:fail/:error
;; methods feed jolt's counters; a library can add report types (test.check's
;; ::trial/::shrunk/::complete) and they dispatch here.
(defn- report-line [m]
  (str (when (:message m) (str (:message m)
                               (when (or (:form m) (contains? m :expected) (contains? m :actual)) " ")))
       (when (:form m) (pr-str (:form m)))
       (when (contains? m :expected) (str " expected: " (pr-str (:expected m))))
       (when (contains? m :actual) (str " actual: " (pr-str (:actual m))))))
(defmethod report :pass [_m] (inc-pass!))
(defmethod report :fail [m] (fail! (report-line m)))
(defmethod report :error [m] (err! (report-line m)))
(defn do-report [m] (report m))

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

(defmacro is
  ([form] `(is ~form nil))
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
          (clojure.test/fail! (str "expected " '~form " to throw" (when ~msg (str " — " ~msg))))
          (catch Throwable e#
            ;; instance? honors the exception hierarchy (a literal class symbol), so
            ;; (thrown? IllegalArgumentException …) matches an ArityException subclass
            ;; like the JVM; class-match? is the simple-name fallback for a class jolt
            ;; models only by name.
            (if (or (clojure.core/instance? ~klass-sym e#)
                    (clojure.test/class-match? e# ~klass))
              (clojure.test/inc-pass!)
              (clojure.test/fail! (str "expected throw of " ~klass " but got " (clojure.core/class e#)))))))

     ;; (is (thrown-with-msg? Class re body...))
     (thrown-form? form "thrown-with-msg?")
     (let [klass-sym (second form)
           klass (name klass-sym)
           re    (nth form 2)
           body  (nthrest form 3)]
       `(try
          ~@body
          (clojure.test/fail! (str "expected " '~form " to throw"))
          (catch Throwable e#
            (let [m# (or (clojure.core/ex-message e#) (str e#))]
              ;; honor the class hierarchy (ExceptionInfo IS a RuntimeException),
              ;; then fall back to a simple-name match like thrown? does.
              (if (and (or (clojure.core/instance? ~klass-sym e#)
                           (clojure.test/class-match? e# ~klass))
                       (re-find ~re m#))
                (clojure.test/inc-pass!)
                (clojure.test/fail! (str "expected throw of " ~klass " matching " ~re " but got " (clojure.core/class e#) ": " m#)))))))

     ;; a predicate call — (= a b), (< x y), (pred? v): evaluate the args so a
     ;; failure shows the actual values, like clojure.test's assert-predicate.
     (and (seq? form) (contains? clojure.test/reported-preds (first form)))
     `(try
        (let [vs# (list ~@(rest form))]
          (if (apply ~(first form) vs#)
            (clojure.test/inc-pass!)
            (clojure.test/fail! (str (pr-str (list '~'not (cons '~(first form) vs#)))
                                     (when ~msg (str " — " ~msg))))))
        (catch Throwable e#
          (clojure.test/err! (str (pr-str '~form) " threw: " (clojure.test/err-text e#)))))

     :else
     `(try
        (if ~form
          (clojure.test/inc-pass!)
          (clojure.test/fail! (str (pr-str '~form) (when ~msg (str " — " ~msg)))))
        (catch Throwable e#
          (clojure.test/err! (str (pr-str '~form) " threw: " (clojure.test/err-text e#))))))))

(defmacro testing [s & body]
  `(binding [clojure.test/*testing-contexts* (conj clojure.test/*testing-contexts* ~s)]
     ~@body))

(defn testing-contexts-str
  "Returns a string representation of the current test context, innermost last."
  []
  (str/join " " (reverse *testing-contexts*)))

(defmacro deftest [name & body]
  `(do
     (defn ~name [] ~@body)
     ;; the var carries :test metadata like clojure.test's deftest, so tooling
     ;; that discovers tests by scanning var meta finds it.
     (alter-meta! (var ~name) assoc :test ~name)
     (swap! clojure.test/registry conj {:name '~name
                                        :ns (clojure.core/ns-name clojure.core/*ns*)
                                        :fn ~name})
     (var ~name)))

;; with-test attaches a test body as :test metadata on a var-defining form (which
;; must return the var), like clojure.test's — schema's tests wrap s/defn this way.
(defmacro with-test [definition & body]
  `(doto ~definition (alter-meta! assoc :test (fn [] ~@body))))

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

(defn- run-one [t]
  (swap! counters update :test inc)
  (wrap-fixtures (get @each-fixtures (:ns t) [])
    (fn []
      (try
        ((:fn t))
        (catch Throwable e
          (err! (str (:name t) " crashed: " (err-text e))))))))

;; Run the registered tests grouped by namespace (registration order preserved
;; within each ns), each group wrapped in its ns's :once fixtures. ns-set nil
;; means all.
(defn- run-selected [ns-set]
  (let [ts (if ns-set (filter (fn [t] (contains? ns-set (:ns t))) @registry) @registry)]
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
    ;; interned (:test meta) tests run after the registered ones, per ns,
    ;; through the same each-fixtures path.
    (doseq [n ns-syms
            t (interned-tests n)]
      (run-one t))
    (let [r @counters
          d {:type :summary
             :test  (- (:test r)  (:test before))
             :pass  (- (:pass r)  (:pass before))
             :fail  (- (:fail r)  (:fail before))
             :error (- (:error r) (:error before))}]
      (println)
      (println (str "Ran " (:test d) " tests. "
                    (:pass d) " assertions passed, "
                    (:fail d) " failures, " (:error d) " errors."))
      d)))

;; --- var-level API (clojure.test parity) -------------------------------------

(def *initial-report-counters* {:test 0, :pass 0, :fail 0, :error 0})

(defn inc-report-counter [k]
  (swap! counters update k (fnil inc 0)))

(defn test-var
  "Run the test attached to var v via its :test metadata, with *testing-vars*
  bound like clojure.test."
  [v]
  (when-let [t (:test (meta v))]
    (binding [*testing-vars* (conj *testing-vars* v)]
      (swap! counters update :test inc)
      (try
        (t)
        (catch Throwable e
          (err! (str (:name (meta v)) " crashed: " (err-text e))))))))

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
