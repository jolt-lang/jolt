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
  (:require [clojure.string :as str]))

;; --- state -----------------------------------------------------------------

(def counters (atom {:test 0 :pass 0 :fail 0 :error 0 :fails []}))
(def jolt-report counters)                ;; alias used by the suite harness
(def ctx-stack (atom []))
(def registry (atom []))                ;; [{:name sym :fn thunk}]
(def once-fixtures (atom []))
(def each-fixtures (atom []))

;; clojure.test/*testing-vars* — the stack of vars under test. Real clojure.test
;; binds it around each test var; test.check's default reporter reads it, so a
;; defspec run through its :test metadata doesn't blow up on an unbound var.
(def ^:dynamic *testing-vars* (list))
(def ^:dynamic *report-counters* nil)

(defn reset-report! []
  (reset! counters {:test 0 :pass 0 :fail 0 :error 0 :fails []})
  (reset! ctx-stack [])
  (reset! registry [])
  (reset! once-fixtures [])
  (reset! each-fixtures []))

(defn- ctx-str [] (str/join " " @ctx-stack))

(defn inc-pass! [] (swap! counters update :pass inc))
(defn fail! [form]
  (let [line (str (ctx-str) (when (seq @ctx-stack) " ") "FAIL: " form)]
    (swap! counters (fn [r] (-> r (update :fail inc) (update :fails conj line))))
    (println line)))
(defn err! [form]
  (let [line (str (ctx-str) (when (seq @ctx-stack) " ") "ERROR: " form)]
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

;; --- class matching for thrown? --------------------------------------------

(defn- last-seg [s]
  (let [s (str s)
        i (str/last-index-of s ".")]
    (if i (subs s (inc i)) s)))

(defn class-match?
  "True if thrown value `e` matches the wanted class simple-name `wanted`.
  Exception/Throwable match anything."
  [e wanted]
  (let [w (last-seg wanted)]
    (if (or (= w "Exception") (= w "Throwable"))
      true
      (let [c (class e)
            cn (cond (nil? c) nil (string? c) c :else (.getName c))]
        (and cn (= (last-seg cn) w))))))

;; --- assertion macros ------------------------------------------------------

(defn- thrown-form? [form sym]
  (and (seq? form) (symbol? (first form)) (= sym (name (first form)))))

(defmacro is
  ([form] `(is ~form nil))
  ([form msg]
   (cond
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

     :else
     `(try
        (if ~form
          (clojure.test/inc-pass!)
          (clojure.test/fail! (str (pr-str '~form) (when ~msg (str " — " ~msg)))))
        (catch Throwable e#
          (clojure.test/err! (str (pr-str '~form) " threw: " (clojure.test/err-text e#))))))))

(defmacro testing [s & body]
  `(do
     (swap! clojure.test/ctx-stack conj ~s)
     (try
       (do ~@body)
       (finally (swap! clojure.test/ctx-stack pop)))))

(defmacro deftest [name & body]
  `(do
     (defn ~name [] ~@body)
     (swap! clojure.test/registry conj {:name '~name :fn ~name})
     (var ~name)))

(defmacro are [argv expr & data]
  (let [n (count argv)
        rows (partition n data)]
    `(do ~@(map (fn [row]
                  `(let [~@(interleave argv row)]
                     (clojure.test/is ~expr)))
                rows))))

;; --- fixtures + run --------------------------------------------------------

(defn use-fixtures [kind & fns]
  (cond
    (= kind :once) (reset! once-fixtures (vec fns))
    (= kind :each) (reset! each-fixtures (vec fns))))

(defn- wrap-fixtures [fixtures body-fn]
  (if (empty? fixtures)
    (body-fn)
    ((first fixtures) (fn [] (wrap-fixtures (rest fixtures) body-fn)))))

(defn- run-one [t]
  (swap! counters update :test inc)
  (wrap-fixtures @each-fixtures
    (fn []
      (try
        ((:fn t))
        (catch Throwable e
          (err! (str (:name t) " crashed: " (err-text e))))))))

(defn run-registered []
  (doseq [t @registry] (run-one t))
  nil)

(defn run-tests [& _nses]
  (wrap-fixtures @once-fixtures (fn [] (run-registered)))
  (let [r @counters]
    (println)
    (println (str "Ran " (:test r) " tests. "
                  (:pass r) " assertions passed, "
                  (:fail r) " failures, " (:error r) " errors."))
    r))

(defn run-test [& _] nil)
(defn test-var [& _] nil)
