;; regen-corpus.clj — set test/chez/corpus.edn :expected to what reference JVM
;; Clojure produces, making the JVM the source of truth for the spec.
;;
;; Runs in ONE JVM process: a single `clojure -M` invocation reads the corpus and
;; evaluates every row's :actual once, in-process. The per-row watchdog is a thread
;; (future) with a wall-clock deadline — not a new JVM per case.
;;
;; For each row:
;;   - JVM evaluates :actual to a value  -> :expected := a self-evaluating source
;;     string for that value (lists/lazy-seqs are vectorized at every nesting depth,
;;     matching the corpus convention; jolt='s cross-type sequential equality makes
;;     a vector :expected match a seq :actual). Only adopted if the rendered string
;;     round-trips (re-reads+evals back to an = value) — else the existing :expected
;;     is kept.
;;   - JVM throws AND the row already expected :throws -> :throws (unchanged).
;;   - Otherwise (JVM can't run the case: jolt-specific host interop / reader, a
;;     timeout, or a throw where jolt has a value) -> the existing :expected is kept.
;;     These are the non-portable rows (profile-tagged) — JVM is not the oracle for
;;     them.
;;
;; Run from the repo root:
;;   clojure -M test/conformance/regen-corpus.clj            ; rewrites corpus.edn
;;   clojure -M test/conformance/regen-corpus.clj --dry      ; report only, no write
(ns regen-corpus
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.walk :as walk]))

(def corpus-path "test/chez/corpus.edn")
(def dry? (some #{"--dry"} *command-line-args*))

;; --- isolated JVM evaluation (same model as certify.clj) ---------------------
(defn read-program [src]
  (read-string {:read-cond :allow} (str "(do " src ")")))

(defn eval-isolated [src]
  (let [sink (java.io.StringWriter.)
        empty-in (java.io.PushbackReader. (java.io.StringReader. ""))]
    (try
      (remove-ns 'user)
      (let [the-ns (create-ns 'user)]
        (binding [*ns* the-ns *out* sink *err* sink *in* empty-in]
          (clojure.core/refer-clojure)
          ;; make the stdlib namespaces a corpus case may reference by qualified name
          ;; resolvable (clojure.math/floor etc.) so they evaluate to the real JVM
          ;; value instead of throwing Unable-to-resolve (-> existing :expected kept).
          (doseq [n '[clojure.string clojure.set clojure.walk clojure.edn
                      clojure.math clojure.pprint]]
            (try (require n) (catch Throwable _ nil)))
          (let [form (try (read-program src)
                          (catch Throwable t (throw (ex-info "read" {::read t}))))]
            [:ok (eval form)])))
      (catch clojure.lang.ExceptionInfo e
        (if (::read (ex-data e)) [:read-error (::read (ex-data e))] [:throw e]))
      (catch Throwable t [:throw t]))))

(def ^:const case-timeout-ms 5000)

;; --- rendering a JVM value to a self-evaluating :expected source string ------
;; Vectorize every list/lazy-seq (at any nesting) so the rendered form is
;; self-evaluating (no bare call forms) and jolt= to the seq it represents.
(defn vectorize [v]
  (walk/postwalk (fn [x] (if (and (seq? x) (not (vector? x))) (vec x) x)) v))

(defn render [v] (binding [*print-length* nil *print-level* nil] (pr-str (vectorize v))))

;; Adopt the JVM value only if its rendered form round-trips back to an = value on
;; the JVM (guards against records / tagged literals / opaque objects that pr-str
;; can't reproduce as readable source).
(defn round-trips? [s v]
  (try
    (let [rt (binding [*ns* (create-ns 'user)] (eval (read-string {:read-cond :allow} s)))]
      (= rt v))
    (catch Throwable _ false)))

;; Evaluate AND render inside one watchdog'd future: a row whose :actual returns an
;; unforced infinite lazy seq evaluates fast (not a timeout) but render/postwalk
;; would then realize it forever — so rendering must be inside the deadline too.
;; Returns one of [:value s] / [:throw] / [:read-error] / [:timeout] / [:unrenderable].
(defn eval+render [src]
  (let [f (future
            (try
              (let [r (eval-isolated src)]
                (if (= (first r) :ok)
                  (let [v (second r) s (render v)]
                    (if (round-trips? s v) [:value s] [:unrenderable]))
                  [(first r)]))
              (catch Throwable _ [:unrenderable])))
        res (deref f case-timeout-ms ::timeout)]
    (if (= res ::timeout) (do (future-cancel f) [:timeout]) res)))

(defn regen-row [row]
  (let [{:keys [expected actual]} row
        r (eval+render actual)]
    (case (first r)
      :value (let [s (second r)] [(assoc row :expected s) (if (= s expected) :same :updated)])
      :throw (if (= expected :throws) [row :throws] [row :kept])
      ;; read-error / timeout / unrenderable -> JVM is not the oracle; keep existing.
      [row :kept])))

;; --- corpus writer (preserves the one-row-per-line layout) -------------------
(defn row-str [{:keys [suite label expected actual]}]
  (str "  {:suite " (pr-str suite)
       " :label " (pr-str label)
       " :expected " (if (= expected :throws) ":throws" (pr-str expected))
       " :actual " (pr-str actual) "}"))

(defn -main [& _]
  (let [corpus (edn/read-string (slurp corpus-path))
        n (count corpus)
        results (mapv regen-row corpus)
        rows (mapv first results)
        tally (frequencies (map second results))]
    (println (format "Regenerated %d rows from JVM Clojure %s" n (clojure-version)))
    (doseq [k [:updated :same :throws :kept :unrenderable]]
      (println (format "  %-13s %5d" (name k) (get tally k 0))))
    (when-not dry?
      (spit corpus-path (str "[\n" (str/join "\n" (map row-str rows)) "\n]\n"))
      (println (format "\nwrote %s" corpus-path)))))

(apply -main *command-line-args*)
