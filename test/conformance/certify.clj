;; certify.clj (jolt-xsfe) — certify the jolt corpus against reference JVM Clojure.
;;
;; The corpus (test/chez/corpus.edn) carries hand-written :expected values. This
;; script runs each row's :actual and :expected through REAL JVM Clojure and checks
;; whether jolt's :expected matches what canonical Clojure produces. It turns the
;; corpus from "our test cases" into "a suite pinned to the reference implementation"
;; — and surfaces any row where the hand-written answer is actually wrong.
;;
;; Each row is evaluated in a throwaway namespace so top-level defs don't leak.
;; Buckets per row:
;;   :certified        jolt :expected == JVM result (the good case)
;;   :certified-throws :expected is :throws and JVM also throws
;;   :divergent        both evaluate but jolt :expected != JVM result (CORPUS BUG)
;;   :throws-mismatch  :expected :throws but JVM did NOT throw (or vice versa)
;;   :jvm-error        :actual errors on vanilla Clojure (jolt-specific / host-coupled
;;                     / not certifiable against the JVM) — informational, not a bug
;;   :read-error       :actual or :expected won't even read on the JVM reader
;;
;; Run from the repo root:
;;   clojure -M test/conformance/certify.clj [corpus.edn] [--edn out.edn]
(ns certify
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.set]
            [clojure.pprint :as pp]))

(def corpus-path
  (or (first (remove #(str/starts-with? % "--") *command-line-args*))
      "test/chez/corpus.edn"))

(def edn-out
  (let [args (vec *command-line-args*)
        i (.indexOf args "--edn")]
    (when (and (>= i 0) (< (inc i) (count args))) (nth args (inc i)))))

;; Classified allowlist of known divergences (deliberate jolt-specific / host-model
;; differences + tracked bugs). The gate fails only on a NEW (unlisted) divergence
;; or throws-mismatch. Keyed by [suite label].
(def allowlist-path "test/conformance/known-divergences.edn")
(def allowlist-entries
  (if (.exists (java.io.File. allowlist-path))
    (:entries (edn/read-string (slurp allowlist-path)))
    []))
;; Non-flaky known divergences: gated for both NEW and STALE.
(def known
  (->> allowlist-entries (remove :flaky) (map (juxt :suite :label)) set))
;; Flaky entries: the JVM result is inherently nondeterministic (e.g. future-cancel
;; racing future completion), so they are always tolerated whether or not they
;; diverge on a given run — never NEW, never stale.
(def flaky
  (->> allowlist-entries (filter :flaky) (map (juxt :suite :label)) set))

;; Read a Clojure source string into a single form, wrapping multi-form bodies in
;; (do ...) so a case like "(def x 1) (inc x)" evaluates as one program. Reader
;; conditionals are allowed (a few corpus rows carry #?(:clj ...)).
(defn read-program [src]
  (read-string {:read-cond :allow} (str "(do " src ")")))

;; Evaluate a Clojure source string in a FRESH `user` namespace, with output and
;; stdin sunk (a case may println, (time ...), or (read) — none should touch the
;; report or block on the terminal). The ns must be named `user` (not a gensym),
;; because that is jolt's default ns and the corpus :expected values bake it in:
;; *ns*, record print tags (#user.Pt), syntax-quote qualification (user/foo), and
;; var print (#'user/v) all render the current ns name. Recreating `user` per case
;; both names it correctly AND drops the previous case's defs. Never throws;
;; returns [:ok value] / [:throw throwable] / [:read-error throwable].
(defn eval-isolated [src]
  (let [sink (java.io.StringWriter.)
        empty-in (java.io.PushbackReader. (java.io.StringReader. ""))]
    (try
      (remove-ns 'user)
      (let [the-ns (create-ns 'user)]
        (binding [*ns* the-ns *out* sink *err* sink *in* empty-in]
          (clojure.core/refer-clojure)
          (let [form (try (read-program src)
                          (catch Throwable t (throw (ex-info "read" {::read t}))))]
            [:ok (eval form)])))
      (catch clojure.lang.ExceptionInfo e
        (if (::read (ex-data e)) [:read-error (::read (ex-data e))] [:throw e]))
      (catch Throwable t [:throw t]))))

(def ^:const case-timeout-ms 5000)

;; Per-case wall-clock guard: an infinite lazy seq forced, a blocking read, or a
;; deadlocked future would otherwise hang the whole run. Returns [:timeout nil] if
;; the case exceeds the budget (the worker thread is cancelled best-effort).
(defn eval-safe [src]
  (let [f (future (eval-isolated src))
        r (deref f case-timeout-ms ::timeout)]
    (if (= r ::timeout)
      (do (future-cancel f) [:timeout nil])
      r)))

(defn classify [row]
  (let [{:keys [expected actual]} row
        throws? (= expected :throws)
        a (eval-safe actual)]
    (cond
      ;; actual exceeded the per-case time budget (infinite seq / blocking / deadlock)
      (= (first a) :timeout)
      {:bucket :timeout :detail (str "exceeded " case-timeout-ms "ms")}

      ;; actual won't read on the JVM
      (= (first a) :read-error)
      {:bucket :read-error :detail (str "actual read: " (.getMessage ^Throwable (second a)))}

      throws?
      (if (= (first a) :throw)
        {:bucket :certified-throws}
        {:bucket :throws-mismatch
         :detail (str "jolt says :throws, JVM returned " (pr-str (second a)))})

      ;; actual threw on the JVM — either jolt-specific/host-coupled (informational)
      (= (first a) :throw)
      {:bucket :jvm-error
       :detail (let [m (.getMessage ^Throwable (second a))]
                 (if m (str/replace m #"\s+" " ") (str (class (second a)))))}

      :else
      (let [e (eval-safe expected)]
        (cond
          (= (first e) :read-error)
          {:bucket :read-error :detail (str "expected read: " (.getMessage ^Throwable (second e)))}
          (= (first e) :throw)
          {:bucket :jvm-error :detail (str "expected eval threw: " (.getMessage ^Throwable (second e)))}
          (= (second e) (second a))
          {:bucket :certified}
          :else
          {:bucket :divergent
           :detail (str "jolt-expected=" (pr-str (second e))
                        " JVM-result=" (pr-str (second a)))})))))

(defn -main [& _]
  (let [corpus (edn/read-string (slurp corpus-path))
        results (mapv (fn [row] (assoc (classify row) :row row)) corpus)
        by (group-by :bucket results)
        n (count results)
        cnt #(count (get by % []))]
    (println (format "Certifying %d corpus rows against JVM Clojure %s\n" n (clojure-version)))
    (println (format "  certified        %5d  (jolt expected == JVM)" (cnt :certified)))
    (println (format "  certified-throws %5d  (:throws, JVM also throws)" (cnt :certified-throws)))
    (println (format "  jvm-error        %5d  (actual not certifiable on vanilla Clojure)" (cnt :jvm-error)))
    (println (format "  read-error       %5d  (won't read on JVM reader)" (cnt :read-error)))
    (println (format "  timeout          %5d  (exceeded %dms — infinite/blocking)" (cnt :timeout) case-timeout-ms))
    (println (format "  throws-mismatch  %5d  <-- jolt/JVM disagree on throwing" (cnt :throws-mismatch)))
    (println (format "  DIVERGENT        %5d  <-- corpus :expected disagrees with JVM" (cnt :divergent)))
    (let [certifiable (+ (cnt :certified) (cnt :certified-throws) (cnt :divergent) (cnt :throws-mismatch))]
      (println (format "\n  certifiable rows: %d  (certified %d / divergent %d / throws-mismatch %d)"
                       certifiable (+ (cnt :certified) (cnt :certified-throws))
                       (cnt :divergent) (cnt :throws-mismatch))))

    ;; Partition divergences/throws-mismatches into known (allowlisted) vs NEW.
    (let [flagged (concat (get by :divergent []) (get by :throws-mismatch []))
          key-of (fn [{:keys [row]}] [(:suite row) (:label row)])
          new? (fn [r] (let [k (key-of r)] (and (not (known k)) (not (flaky k)))))
          news (filter new? flagged)
          flagged-keys (set (map key-of flagged))
          stale (clojure.set/difference known flagged-keys)]
      (println (format "\n  allowlist: %d entries (%d flaky); %d of %d divergences known, %d NEW, %d stale"
                       (+ (count known) (count flaky)) (count flaky)
                       (- (count flagged) (count news)) (count flagged) (count news) (count stale)))
      (when (seq news)
        (println "\n=== NEW divergences (not in allowlist) — gate FAILS ===")
        (doseq [{:keys [row detail]} news]
          (println (format "  [%s] %s\n      actual: %s\n      %s"
                           (:suite row) (:label row) (:actual row) detail))))
      (when (seq stale)
        (println "\n=== STALE allowlist entries (no longer diverging — remove them) ===")
        (doseq [[s l] (sort stale)] (println (format "  [%s] %s" s l))))
      (def new-divergences news)
      (def stale-entries stale))

    ;; Full per-row divergence detail goes to the --edn report (for triage); the
    ;; console stays quiet about KNOWN divergences (the NEW/STALE sections above are
    ;; what matters for the gate).
    (when edn-out
      (spit edn-out (with-out-str
                      (pp/pprint {:corpus corpus-path
                                  :clojure-version (clojure-version)
                                  :counts (into {} (map (fn [[k v]] [k (count v)]) by))
                                  :divergent (mapv (fn [r] (assoc (:row r) :detail (:detail r))) (get by :divergent))
                                  :throws-mismatch (mapv (fn [r] (assoc (:row r) :detail (:detail r))) (get by :throws-mismatch))})))
      (println (format "\nwrote machine-readable report to %s" edn-out)))

    ;; Gate: fail only on a NEW (unlisted) divergence or a stale allowlist entry.
    ;; Every current divergence is either intentional (classified in the allowlist)
    ;; or a tracked bug — so a clean run means the corpus matches reference Clojure
    ;; everywhere it claims to, modulo the documented jolt-specific deltas.
    (System/exit (if (or (seq new-divergences) (seq stale-entries)) 1 0))))

(apply -main *command-line-args*)
