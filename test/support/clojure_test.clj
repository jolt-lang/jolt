;; Minimal `clojure.test` + `clojure.core-test.portability` shims.
;;
;; These exist solely so Jolt can load the external clojure-test-suite
;; (https://github.com/lread/clojure-test-suite, EPL) — 240 per-function
;; `.cljc` files written against `clojure.test`. They are NOT a full
;; clojure.test: just enough surface (deftest/is/testing/are + the suite's
;; portability helpers) to run the suite and tally pass/fail/error.
;;
;; Loaded by test/integration/clojure-test-suite-test.janet, which pre-loads
;; this file so the suite's `(require [clojure.test ...])` finds it already
;; populated (Jolt's require is a no-op for already-loaded namespaces).

(ns clojure.test)

;; --- result accumulator (plain atoms; no dynamic vars needed) -------------

(def jolt-report (atom {:pass 0 :fail 0 :error 0 :fails []}))
(def jolt-ctx (atom []))        ;; stack of `testing` context strings
(def registry (atom []))        ;; deftest fns defined since last reset

(defn reset-report! []
  (reset! jolt-report {:pass 0 :fail 0 :error 0 :fails []})
  (reset! jolt-ctx [])
  (reset! registry []))

(defn- ctx-str [] (apply str (interpose " " @jolt-ctx)))

(defn inc-pass! [] (swap! jolt-report update :pass inc))

(defn fail! [form]
  (swap! jolt-report
         (fn [r] (-> r (update :fail inc)
                     (update :fails conj (str (ctx-str) " FAIL: " form))))))

(defn err! [form]
  (swap! jolt-report
         (fn [r] (-> r (update :error inc)
                     (update :fails conj (str (ctx-str) " ERROR: " form))))))

(defn n-pass [] (:pass @jolt-report))
(defn n-fail [] (:fail @jolt-report))
(defn n-error [] (:error @jolt-report))
(defn failures [] (:fails @jolt-report))

;; --- assertion macros ------------------------------------------------------

;; `(is form)` / `(is form msg)` — the optional msg is absorbed and ignored.
(defmacro is [form & _]
  `(try
     (if ~form
       (clojure.test/inc-pass!)
       (clojure.test/fail! (pr-str '~form)))
     (catch :default e#
       (clojure.test/err! (str (pr-str '~form) " threw")))))

(defmacro testing [s & body]
  `(do
     (swap! clojure.test/jolt-ctx conj ~s)
     (try
       (do ~@body)
       (finally (swap! clojure.test/jolt-ctx pop)))))

(defmacro deftest [name & body]
  `(do
     (def ~name (fn [] ~@body))
     (swap! clojure.test/registry conj ~name)
     ~name))

;; `(are [bindings] expr & data)` — substitute each row of `data` into the
;; template and assert via `is`. Expands to a `do` of let+is forms.
(defmacro are [argv expr & data]
  (let [n (count argv)
        rows (partition n data)]
    `(do ~@(map (fn [row]
                  `(let [~@(interleave argv row)]
                     (clojure.test/is ~expr)))
                rows))))

;; Run every deftest registered since the last reset, isolating crashes.
(defn run-registered []
  (doseq [t @registry]
    (try (t) (catch :default e (clojure.test/err! "deftest crashed"))))
  nil)

;; clojure.test entry points the suite may call — no-ops; the Janet runner
;; drives execution via run-registered instead.
(defn run-tests [& _] nil)
(defn run-test [& _] nil)
(defn test-var [& _] nil)


;; --- clojure.core-test.portability ----------------------------------------

(ns clojure.core-test.portability)

;; Gate a test on whether its target var exists in this dialect. Jolt only
;; implements a subset of clojure.core, so unimplemented fns get skipped
;; cleanly rather than erroring.
(defmacro when-var-exists [var-sym & body]
  (if (resolve var-sym)
    `(do ~@body)
    `(println "SKIP -" '~var-sym)))

;; `(thrown? body)` — true iff evaluating body throws. The suite always uses
;; the single-arg (no exception-class) form via this portability helper.
(defmacro thrown? [& body]
  `(try (do ~@body false) (catch :default e# true)))

(defn big-int? [n]
  (and (integer? n) (not (int? n))))

(defn lazy-seq? [x]
  ;; Jolt has no public LazySeq type test; approximate with seq?-of-non-vector.
  (and (seq? x) (not (vector? x))))

(defn sleep [ms] nil)
