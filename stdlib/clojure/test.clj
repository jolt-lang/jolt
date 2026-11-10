;;   Portions of this file are from Clojure's src/clj/clojure/test.clj,
;;   Copyright (c) Rich Hickey. All rights reserved.
;;   The use and distribution terms for that software are covered by the
;;   Eclipse Public License 1.0 (licenses/EPL-1.0.txt).

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
;; The ref run-tests / test-ns bound for the run in progress. A result counts
;; toward jolt's process-wide tally only while *report-counters* is this ref (or
;; unbound): a caller that binds *report-counters* to a ref of its own, as
;; test.check's suite does to capture one test var's results, keeps them private.
(def ^:dynamic ^:private *run-counters* nil)
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
        ;; the path renders with "\\" on Windows, where "/" may appear too
        (let [i (max (or (str/last-index-of f "/") -1)
                     (if (= "\\" java.io.File/separator) (or (str/last-index-of f "\\") -1) -1))]
          (if (neg? i) f (subs f (inc i))))))))

(defn file-position
  "Returns a vector [filename line-number] for the nth call up the
  stack.

  Deprecated in 1.2: The information needed for test reporting is
  now on :file and :line keys in the result map. Jolt's stack traces
  may hold fewer frames than the JVM's, and a frame it lacks reads
  [nil nil]."
  {:added "1.0"
   :deprecated "1.2"}
  [n]
  (let [s (nth (.getStackTrace (new java.lang.Throwable)) n nil)]
    [(when s (.getFileName s)) (when s (.getLineNumber s))]))

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

(defn- global-tally? []
  (or (nil? *report-counters*) (identical? *report-counters* *run-counters*)))

(defn inc-report-counter
  "Bump a counter by key: *report-counters* when bound, as the reference does,
  and jolt's process-wide tally unless the caller bound a ref of its own."
  [k]
  (bump-counters! k)
  (when (global-tally?)
    (swap! counters update k (fnil inc 0))))

(defn inc-pass! [] (when (global-tally?) (swap! counters update :pass inc)))
(defn fail! [m]
  (let [line (str (ctx-str) (when (or (seq *testing-contexts*) (seq @ctx-stack)) " ") "FAIL: " (report-line m))]
    (when (global-tally?)
      (swap! counters (fn [r] (-> r (update :fail inc) (update :fails conj line)))))
    (with-test-out
      (println "\nFAIL in" (testing-vars-str m))
      (when (seq *testing-contexts*) (println (testing-contexts-str)))
      (when-let [message (:message m)] (println message))
      (println "expected:" (pr-str (:expected m)))
      (println "  actual:" (pr-str (:actual m))))))
(defn err! [m]
  (let [line (str (ctx-str) (when (or (seq *testing-contexts*) (seq @ctx-stack)) " ") "ERROR: " (report-line m))]
    (when (global-tally?)
      (swap! counters (fn [r] (-> r (update :error inc) (update :fails conj line)))))
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
             ;; The thrown thing on a PASS, nil otherwise. `is`'s own docstring
             ;; promises the first half -- "then returns the thing thrown" --
             ;; and the value used to be do-report's instead, a counters map,
             ;; so the bind-and-assert shape silently stopped asserting:
             ;;
             ;;     (let [ex (is (thrown? ExceptionInfo (f)))]
             ;;       (is (= "..." (ex-message ex))))
             ;;
             ;; ex-message of a map is nil, so the inner `is` compared nil to a
             ;; string and never tested the message (jolt#1091).
             ;;
             ;; nil on the non-pass branch is the JVM's answer too, by a route
             ;; jolt does not share: there the catch names the expected class, so
             ;; a different exception is not caught here at all -- `is`'s own
             ;; outer guard takes it, reports :error and answers nil. jolt
             ;; catches Throwable to report a fail that names the class it got,
             ;; which is the friendlier report and a divergence that predates
             ;; this. Returning the exception there would be a NEW one.
             (if (or (clojure.core/instance? ~klass-sym e#)
                     (clojure.test/class-match? e# ~klass))
               (do (clojure.test/do-report {:type :pass :message ~msg :expected '~form :actual e#})
                   e#)
               (do (clojure.test/do-report {:type :fail :message (str "expected throw of " ~klass " but got " (clojure.core/class e#))
                                            :expected '~form :actual e#})
                   nil)))))

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
             (let [m# (or (clojure.core/ex-message e#) (str e#))
                   ;; honor the class hierarchy (ExceptionInfo IS a RuntimeException),
                   ;; then fall back to a simple-name match like thrown? does.
                   class-ok?# (or (clojure.core/instance? ~klass-sym e#)
                                  (clojure.test/class-match? e# ~klass))]
               (if (and class-ok?# (re-find ~re m#))
                 (clojure.test/do-report {:type :pass :message ~msg :expected '~form :actual e#})
                 (clojure.test/do-report {:type :fail :message (str "expected throw of " ~klass " matching " ~re " but got " (clojure.core/class e#) ": " m#)
                                          :expected '~form :actual e#}))
               ;; the thrown thing whenever the CLASS matched, message or not, and
               ;; nil otherwise. The JVM's catch names the class and its e# sits
               ;; after the message test, so a wrong message still answers the
               ;; exception there; a wrong class never reaches that catch, and
               ;; `is`'s outer guard answers nil (see thrown? above). #1091
               ;; reports thrown?; this half had the same defect.
               (when class-ok?# e#)))))

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



;; deftest records itself here. A reload must REPLACE the entry for a namespace's
;; test name rather than add a second one. clojure.test on the JVM discovers
;; tests through var metadata, so redefining the var replaces the test; this
;; registry is a different design and needs the same answer, because
;; reload-and-run is the whole point of a live image.
;;
;; Appending made a reloaded namespace run its tests once more per reload, and
;; report the extra runs as extra tests. The count going up is the harmless half.
;; The other half is that a test run twice in one invocation reads as a pass if
;; EITHER run passed, and the reload loop is exactly where someone is chasing a
;; flake (jolt#1096).
;;
;; A replaced entry keeps its POSITION, so reloading does not reorder a
;; namespace's tests against each other.
(defn register-test!
  "Add the test f as ns-sym/name, replacing any entry already registered under
  that namespace and name. deftest calls this; a tool that interns test vars
  itself can too."
  [ns-sym name f]
  (swap! registry
         (fn [r]
           (let [entry {:name name :ns ns-sym :fn f}
                 i (first (keep-indexed
                           (fn [i t] (when (and (= name (:name t)) (= ns-sym (:ns t))) i))
                           r))]
             (if i (assoc r i entry) (conj r entry)))))
  nil)

(defn- def-test-form
  "The (def ...) deftest and deftest- expand to, shaped like clojure.test's: the
  var's VALUE runs it as a test through test-var and the body lives in :test.
  So a test-ns-hook that calls its tests by name, (defn test-ns-hook [] (a) (b)),
  counts each one, brackets it in begin/end-test-var, and reports an uncaught
  throw as an :error instead of losing the rest of the run.

  The thunk is a NAMED fn only so the def registers its source: a stack read
  (test.check's reporter walks one for an assertion's file:line) then maps the
  body's frame, which lives in the def's metadata, back to this file."
  [name body extra-meta]
  `(do
     (def ~(vary-meta name merge extra-meta {:test `(fn [] ~@body)})
       (fn ~name [] (clojure.test/test-var (var ~name))))
     (clojure.test/register-test! (clojure.core/ns-name clojure.core/*ns*)
                                  '~name (:test (meta (var ~name))))
     (var ~name)))

(defmacro deftest [name & body]
  (when *load-tests*
    (def-test-form name body nil)))

(defmacro deftest-
  "Like deftest but the var is private."
  [name & body]
  (when *load-tests*
    (def-test-form name body {:private true})))

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
  (wrap-fixtures (get @each-fixtures (:ns t) [])
    (fn []
      ;; bind *testing-vars* the way test-var does, so a failure inside a
      ;; registry-run test still names it in the "FAIL in (name)" header. It must
      ;; be the real VAR: test.check's reporter reads this stack and treats the
      ;; entries as vars, so a stand-in map fails as "cannot be cast to Named".
      ;; A test whose var no longer resolves leaves the stack alone.
      (let [v (try (ns-resolve (:ns t) (:name t)) (catch Throwable _ nil))]
        (binding [*testing-vars* (if v (conj *testing-vars* v) *testing-vars*)]
          (when v (do-report {:type :begin-test-var :var v}))
          (inc-report-counter :test)
          (try
            ((:fn t))
            (catch Throwable e
              (do-report {:type :error
                          :message "Uncaught exception, not in assertion."
                          :expected nil :actual e})))
          (when v (do-report {:type :end-test-var :var v})))))))

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
                   (if ns-set (filter (fn [t] (contains? ns-set (:ns t))) @registry) @registry))
        ;; grouped once: re-filtering the whole list per namespace made the
        ;; loop O(namespaces x tests) before a single body ran. distinct keeps
        ;; the namespace ORDER (group-by's map order does not survive >8 keys),
        ;; and group-by keeps registration order within each group.
        by-ns (group-by :ns ts)]
    (doseq [n (distinct (map :ns ts))]
      (wrap-fixtures (get @once-fixtures n [])
        (fn [] (doseq [t (get by-ns n)] (run-one t))))))
  nil)

;; Tests attached to a namespace's vars via :test metadata but never registered
;; through deftest — clojure.test discovers tests by scanning ns-interns, so a
;; suite that interns test vars directly (yamltest-style intern + vary-meta)
;; must be visible to (run-tests 'ns) too. deftest'd vars also carry :test
;; meta, so names already in the registry are excluded.
;; registered-by-ns is (group-by :ns @registry), built ONCE by the caller —
;; run-all-tests hands every loaded namespace through here, and a fresh
;; whole-registry scan per namespace was O(namespaces x tests).
(defn- interned-tests [n registered-by-ns]
  (let [known (set (map :name (get registered-by-ns n)))]
    (->> (ns-interns n)
         (keep (fn [[s v]]
                 (when-let [t (:test (meta v))]
                   (when-not (contains? known s)
                     {:name s :ns n :fn t}))))
         (sort-by (fn [t] (str (:name t)))))))

(defn run-registered [] (run-selected nil))

;; A namespace's tests: its test-ns-hook when it defines one, which then decides
;; what runs, like clojure.test's test-ns; otherwise its registered and interned
;; tests inside its :once fixtures.
(defn- ns-hook [ns-sym]
  (when (find-ns ns-sym)
    (find-var (symbol (str ns-sym) "test-ns-hook"))))

(defn- run-ns [n reg-by-ns]
  (do-report {:type :begin-test-ns :ns (find-ns n)})
  (if-let [hook (ns-hook n)]
    ((var-get hook))
    (let [ts (concat (filter selected? (get reg-by-ns n))
                     (interned-tests n reg-by-ns))]
      (when (seq ts)
        (wrap-fixtures (get @once-fixtures n [])
          (fn [] (doseq [t ts] (run-one t)))))))
  (do-report {:type :end-test-ns :ns (find-ns n)}))

;; (run-tests 'ns1 'ns2 …) runs only those namespaces' tests, like clojure.test,
;; and (run-tests) is (run-tests *ns*). run-registered runs everything registered.
;; Counts go to a ref bound for this call, whose contents are the summary; the
;; process-wide counters stay cumulative for the n-pass/n-fail harness API.
(defn run-tests
  ([] (run-tests *ns*))
  ([& nses]
   (let [ns-syms (map (fn [n] (if (symbol? n) n (ns-name n))) nses)
         rc (ref *initial-report-counters*)]
     (binding [*report-counters* rc
               *run-counters* rc]
       (let [reg-by-ns (group-by :ns @registry)]
         (doseq [n ns-syms] (run-ns n reg-by-ns))))
     (let [d (assoc @rc :type :summary)]
       (do-report d)
       d))))

(defmethod report :summary [m]
  (with-test-out
    (println)
    (println (str "Ran " (:test m) " tests. "
                  (:pass m) " assertions passed, "
                  (:fail m) " failures, " (:error m) " errors."))))

;; --- var-level API (clojure.test parity) -------------------------------------


(defn test-var
  "Run the test attached to var v via its :test metadata, with *testing-vars*
  bound like clojure.test. An exception escaping the test is reported as an
  :error through report."
  [v]
  (when-let [t (:test (meta v))]
    (binding [*testing-vars* (conj *testing-vars* v)]
      (do-report {:type :begin-test-var :var v})
      (inc-report-counter :test)
      (try
        (t)
        (catch Throwable e
          (do-report {:type :error
                      :message "Uncaught exception, not in assertion."
                      :expected nil :actual e})))
      (do-report {:type :end-test-var :var v}))))

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
            ;; only a test var runs inside the :each fixtures, like
            ;; clojure.test's (when (:test (meta v)) ...)
            (when (:test (meta v))
              (wrap-fixtures (get @each-fixtures n [])
                (fn [] (test-var v))))))))))

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
  it. Counts go to a ref bound for the call, whose contents are returned."
  [n]
  (let [ns-sym (if (symbol? n) n (ns-name n))
        rc (ref *initial-report-counters*)]
    (binding [*report-counters* rc
              *run-counters* rc]
      (do-report {:type :begin-test-ns :ns (find-ns ns-sym)})
      (if-let [hook (ns-hook ns-sym)]
        ((var-get hook))
        (test-all-vars ns-sym))
      (do-report {:type :end-test-ns :ns (find-ns ns-sym)}))
    @rc))

(defn run-all-tests
  "Runs the tests in every loaded namespace, or in those whose name matches re."
  ([] (apply run-tests (map ns-name (all-ns))))
  ([re] (apply run-tests (filter (fn [n] (re-matches re (name n)))
                                 (map ns-name (all-ns))))))
