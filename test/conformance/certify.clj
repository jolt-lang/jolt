;; certify.clj — certify the jolt corpus against reference JVM Clojure.
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
;;   :throws-mismatch  :expected :throws but JVM did NOT throw
;;   :jvm-raises       :expected is a value but the JVM RAN the row and raised — the
;;                     mirror of :throws-mismatch, and gated the same way
;;   :uncertifiable    the JVM cannot express the row at all (a jolt-only fn, a class
;;                     jolt auto-imports, a lib off the classpath) — informational
;;   :expected-error   the :expected SOURCE does not evaluate on the JVM (CORPUS BUG:
;;                     :expected is evaluated, so an unquoted seq asserts nothing)
;;   :read-error       :actual or :expected won't even read on the JVM reader
;;
;; Run from the repo root:
;;   clojure -M test/conformance/certify.clj [corpus.edn] [--edn out.edn]
;;   clojure -M test/conformance/certify.clj --profile test/conformance/profile.edn
;;   clojure -M test/conformance/certify.clj --self-test   ; check the classifier
(ns certify
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.set]
            [clojure.pprint :as pp]))

;; Flags that take a following VALUE. The corpus path is positional, and finding it
;; by "first arg that doesn't start with --" swallowed those values: `--profile p`
;; with no explicit corpus made p the corpus, so certify read the profile file as if
;; it were the corpus, certified its handful of rows and wrote a profile with almost
;; nothing left in it. Skipping a valued flag together with its argument is what
;; makes the documented `[corpus.edn] [--edn out.edn]` usage work in either order.
(def valued-flags #{"--edn" "--profile"})

(defn positional-args
  "ARGS with every flag — and the argument of a value-taking flag — removed."
  [args]
  (loop [[a & more] (seq args) out []]
    (cond (nil? a)                  out
          (valued-flags a)          (recur (next more) out)
          (str/starts-with? a "--") (recur more out)
          :else                     (recur more (conj out a)))))

(def corpus-path
  (or (first (positional-args *command-line-args*)) "test/chez/corpus.edn"))

(def edn-out
  (let [args (vec *command-line-args*)
        i (.indexOf args "--edn")]
    (when (and (>= i 0) (< (inc i) (count args))) (nth args (inc i)))))

;; Classified allowlist of known divergences (deliberate jolt-specific / host-model
;; differences + tracked bugs). The gate fails only on a NEW (unlisted) divergence,
;; throws-mismatch or jvm-raises. Keyed by [suite label].
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

;; The Compiler wraps the real failure, so everything that reasons about WHY a case
;; failed walks the whole cause chain rather than trusting the top throwable's
;; message ("Syntax error compiling at …").
(defn causes [^Throwable t]
  (take-while some? (iterate #(some-> ^Throwable % .getCause) t)))

(defn ex-summary [^Throwable t]
  (let [c (last (causes t))
        m (.getMessage c)]
    (if m (str (.getSimpleName (class c)) ": " (str/replace m #"\s+" " ")) (str (class c)))))

;; Read and evaluate a case's forms ONE AT A TIME, the way a REPL or a loaded file
;; does, answering the last form's value. Wrapping the body in a single (do ...)
;; instead would make it one COMPILATION unit, so a case that requires a namespace
;; and then uses its alias, or defines a deftype and then calls its ctor, fails to
;; compile before any of it runs — a property of that wrapper, not of the case.
;; jolt unrolls a top-level do the same way. Reader conditionals are allowed (a few
;; corpus rows carry #?(:clj ...)); a read failure is tagged so the caller can tell
;; it from an eval failure.
(defn eval-program [src]
  (let [r (java.io.PushbackReader. (java.io.StringReader. src))
        opts {:read-cond :allow :eof ::eof}]
    (loop [v nil]
      (let [form (try (read opts r)
                      (catch Throwable t (throw (ex-info "read" {::read t}))))]
        (if (= form ::eof) v (recur (eval form)))))))

;; Names jolt resolves out of the box and a bare JVM `user` does not:
;; host-static-classes.ss registers every modelled class under its simple name as
;; well as its FQN, and a stdlib namespace loads on demand. Supplying them is not
;; translating the case — resolution is the only thing that changes — and it lets
;; the oracle certify the case's VALUE instead of filing it as "the JVM has no
;; such name". Supplied ON DEMAND (a case that fails on one is retried with that
;; one name added) because the environment is itself observable: (count
;; (ns-imports 'user)) is a corpus row, and it pins jolt's 96 against the JVM's 96.
(def bare-class-names
  {"StringWriter" "java.io.StringWriter"
   "StringReader" "java.io.StringReader"
   "PushbackReader" "java.io.PushbackReader"
   "InputStream" "java.io.InputStream"
   "HashMap" "java.util.HashMap"
   "Map" "java.util.Map"
   "StringTokenizer" "java.util.StringTokenizer"
   "Base64" "java.util.Base64"
   "Pattern" "java.util.regex.Pattern"
   "URLEncoder" "java.net.URLEncoder"
   "URLDecoder" "java.net.URLDecoder"
   "Charset" "java.nio.charset.Charset"
   "MapEntry" "clojure.lang.MapEntry"})

(def on-demand-requires #{"clojure.math"})

;; The name a failure says the JVM could not resolve, if it is one we can supply:
;; [:import fqn] or [:require ns]. nil for anything else — an unresolved jolt-only
;; fn stays unresolved, which is the whole point of the :uncertifiable bucket.
(defn missing-name [^Throwable t]
  (some (fn [^Throwable c]
          (let [m (or (.getMessage c) "")
                nm (or (second (re-find #"Unable to resolve classname: ([^\s,]+)" m))
                       (second (re-find #"No such namespace: ([^\s,]+)" m))
                       (second (re-find #"Unable to resolve symbol: ([^\s,]+)" m))
                       (when (instance? ClassNotFoundException c) (str/trim m)))]
            (cond
              (nil? nm) nil
              (bare-class-names nm) [:import (bare-class-names nm)]
              (on-demand-requires nm) [:require nm])))
        (causes t)))

;; Evaluate a Clojure source string in a FRESH `user` namespace, with output and
;; stdin sunk (a case may println, (time ...), or (read) — none should touch the
;; report or block on the terminal). The ns must be named `user` (not a gensym),
;; because that is jolt's default ns and the corpus :expected values bake it in:
;; *ns*, record print tags (#user.Pt), syntax-quote qualification (user/foo), and
;; var print (#'user/v) all render the current ns name. Recreating `user` per case
;; both names it correctly AND drops the previous case's defs. Never throws;
;; Realize a value depth-first. A row's program can return an UNREALIZED lazy seq
;; that throws only when forced — (distinct #{}) is exactly one: it builds fine on
;; the JVM and raises "nth not supported" at realization. Left unforced inside
;; eval-once's try, that exception escapes the per-row handler and fires later, in
;; classify's = or in pr-str, aborting the entire run instead of bucketing the one
;; row. Depth-first so a lazy seq nested in a vector or a map counts too.
;; eval-safe's per-case deadline covers the forcing, which is what makes this safe
;; on an infinite seq — the comment there already anticipates one being forced.
(defn force-deep [v]
  (cond
    (map? v)  (doseq [[k vv] v] (force-deep k) (force-deep vv))
    (coll? v) (doseq [x v] (force-deep x))
    :else     nil)
  v)

;; returns [:ok value] / [:throw throwable] / [:read-error throwable].
(defn eval-once [src imports]
  (let [sink (java.io.StringWriter.)
        empty-in (java.io.PushbackReader. (java.io.StringReader. ""))]
    (try
      (remove-ns 'user)
      (let [the-ns (create-ns 'user)]
        (binding [*ns* the-ns *out* sink *err* sink *in* empty-in]
          (clojure.core/refer-clojure)
          (doseq [c imports] (.importClass the-ns (Class/forName c)))
          [:ok (force-deep (eval-program src))]))
      (catch clojure.lang.ExceptionInfo e
        (if (::read (ex-data e)) [:read-error (::read (ex-data e))] [:throw e]))
      (catch Throwable t [:throw t]))))

;; Retry a case that failed on a name we can supply, with that one name supplied —
;; the case starts over from a fresh `user`, so a retry sees no state from the try
;; that failed. Each pass adds a name it did not already have, so this terminates.
;; All passes share the one per-case deadline in eval-safe below.
;; Answers [status value supplied], where `supplied` is the set of names the case
;; needed and a bare JVM `user` did not have — a case that needed one is certified
;; for its VALUE but is not portable Clojure source, and the profile says so.
(defn eval-isolated [src]
  (loop [imports #{} required #{}]
    (let [r (eval-once src imports)]
      (if (= (first r) :throw)
        (let [[kind nm] (missing-name (second r))]
          (cond
            (and (= kind :import) (not (imports nm))) (recur (conj imports nm) required)
            (and (= kind :require) (not (required nm)))
            (do (require (symbol nm)) (recur imports (conj required nm)))
            :else (conj r (into imports required))))
        (conj r (into imports required))))))

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

;; Did the JVM fail because it lacks the VOCABULARY rather than because it disagrees
;; about behavior? A jolt-only core fn, a class jolt auto-imports and the JVM does
;; not, a namespace off the classpath: the JVM never got to run the program, so it
;; has no opinion on the row and the oracle must stay silent. Anything else means the
;; JVM understood the program and rejected it, which IS an opinion.
(defn uncertifiable-reason [^Throwable t]
  (some (fn [^Throwable c]
          (let [m (or (.getMessage c) "")]
            (cond
              (instance? ClassNotFoundException c) :class-not-found
              (instance? java.io.FileNotFoundException c) :lib-not-on-classpath
              (re-find #"Unable to resolve symbol" m) :unresolved-symbol
              (re-find #"Unable to resolve classname|Unable to resolve var|Unable to find static field|No such namespace|No such var" m) :unresolved-name)))
        (causes t)))

;; --- the :documented half of known-divergences.edn ---------------------------
;; :entries names corpus rows, and everything above gates them. :documented is
;; prose about divergences that are NOT corpus rows, and until now nothing ever
;; ran it. Prose with no oracle behind it decays silently: of the 33 entries that
;; could be made checkable, two documented a divergence that no longer existed and
;; five recorded a JVM or jolt answer no run reproduces — one claiming the JVM
;; throws on (ex-info "m" nil), which returns {}, and one claiming resolve throws
;; ClassNotFoundException, which it never does.
;;
;; So every :documented entry now declares how it is verified:
;;   :check {:expr "<expr>" :jvm "<rendered>" :jolt "<rendered>"}
;;   :check :prose  :why "<reason it cannot be an expression>"
;; and an entry with neither fails the gate, so a new one cannot arrive unchecked.
;;
;; This half evaluates :expr on reference Clojure and requires it to render
;; exactly :jvm. host/chez/run-documented.ss does the same for :jolt. Both halves
;; also require :jvm and :jolt to DIFFER — an entry whose sides have converged is
;; documenting a divergence that no longer exists, and belongs deleted.
(def registry
  (if (.exists (java.io.File. allowlist-path))
    (edn/read-string (slurp allowlist-path))
    {}))
(def documented-entries (:documented registry))

;; Render as host/chez/run-documented.ss does: pr-str for a value, "throws
;; <SimpleName>" for a raise. The name is the ROOT cause's — the Compiler wraps a
;; macroexpansion-time throw, and jolt has no such wrapper to match.
(defn render-documented [src]
  (let [r (eval-safe src)]
    (case (first r)
      :ok (pr-str (second r))
      :timeout "timeout"
      (str "throws " (.getSimpleName (class (last (causes (second r)))))))))

;; Every :category in use must have a :legend line explaining it. A category is
;; how a reader decides whether a divergence is deliberate, and one with no legend
;; entry says nothing — :restrictive was in use with no legend line until this
;; check went in.
(defn check-legend []
  (let [legend (set (keys (:legend registry)))
        used (into (sorted-set) (map :category (concat (:documented registry)
                                                       (:entries registry))))]
    (for [c used :when (not (legend c))]
      (str "  [:legend] category " c " is used but has no :legend entry"))))

(defn check-documented
  "Problems with the :documented list, as printable lines. Empty means it holds."
  []
  (for [e documented-entries
        :let [label (or (:behavior e) "<no :behavior>")
              {:keys [expr jvm jolt]} (when (map? (:check e)) (:check e))]
        msg (cond
              (nil? (:check e))
              ["no :check — add {:expr .. :jvm .. :jolt ..}, or :prose with a :why"]

              (= :prose (:check e))
              (when-not (string? (:why e))
                [":check :prose needs a :why saying why it is not machine-checkable"])

              (not (map? (:check e)))
              [":check must be a map or :prose"]

              (not (every? string? [expr jvm jolt]))
              [":check needs string :expr, :jvm and :jolt"]

              (= jvm jolt)
              [(str "STALE — :jvm and :jolt agree (" jvm "), so this is no longer a divergence")]

              :else
              (let [got (render-documented expr)]
                (when-not (= got jvm)
                  [(str "JVM side: want `" jvm "` got `" got "`\n      expr: " expr)])))]
    (str "  [" label "]\n      " msg)))

(defn record-documented []
  (doseq [e documented-entries
          :when (map? (:check e))]
    (println (format "  :expr %s\n  :jvm %s\n"
                     (pr-str (:expr (:check e)))
                     (pr-str (render-documented (:expr (:check e)))))))
  (System/exit 0))

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

      ;; actual threw on the JVM: either the JVM lacks the vocabulary (no opinion) or
      ;; it ran the row and rejected it while jolt answers a value (a divergence).
      (= (first a) :throw)
      (if-let [why (uncertifiable-reason (second a))]
        {:bucket :uncertifiable :reason why :detail (ex-summary (second a))}
        {:bucket :jvm-raises
         :detail (str "jolt-expected=" (pr-str expected) " JVM raised " (ex-summary (second a)))})

      :else
      (let [e (eval-safe expected)]
        (cond
          (= (first e) :read-error)
          {:bucket :read-error :detail (str "expected read: " (.getMessage ^Throwable (second e)))}
          (= (first e) :throw)
          {:bucket :expected-error :detail (str "expected eval threw " (ex-summary (second e)))}
          (= (second e) (second a))
          (let [supplied (nth a 2 nil)]
            (cond-> {:bucket :certified} (seq supplied) (assoc :supplied supplied)))
          :else
          {:bucket :divergent
           :detail (str "jolt-expected=" (pr-str (second e))
                        " JVM-result=" (pr-str (second a)))})))))

(def profile-out
  (let [args (vec *command-line-args*)
        i (.indexOf args "--profile")]
    (when (and (>= i 0) (< (inc i) (count args))) (nth args (inc i)))))

;; Allowlist category -> conformance feature (for divergent / throws-mismatch /
;; jvm-raises rows whose nature a human classified in known-divergences.edn).
(def category->feature
  {:numeric-model :numerics/double-only
   :concurrency-model :concurrency/snapshot
   :reader-model :reader/jolt
   :printer-model :printer/jolt
   :strictness :strictness/jolt
   :impl-detail :impl/representation
   :host-model :host/jvm-interop
   :bug :bug})
(def allow-category
  (into {} (map (fn [e] [[(:suite e) (:label e)] (:category e)]) allowlist-entries)))

;; Coarse feature for a row the JVM couldn't express (uncertifiable) — by scanning
;; the source for what host capability it exercises. Defaults to generic host interop.
(defn uncertifiable-feature [actual]
  (let [a (str/lower-case actual)]
    (cond
      (re-find #"chan|>!|<!|go-loop|\(go |alts!|core\.async" a) :async/core-async
      (re-find #"load-string|\beval\b|read-string.*eval" a) :runtime/eval
      (re-find #"-array|into-array|to-array|aget|aset|aclone|alength" a) :host/arrays
      (re-find #"proxy|reify|gen-class|definterface|deftype.*:|\.\.|bean" a) :host/jvm-interop
      (re-find #"class|forname|instance\?|cast|\.getclass" a) :host/jvm-interop
      :else :host/jvm-interop)))

;; The full conformance feature(s) a row requires (empty = portable). Certified
;; rows are portable; everything else gets a feature so a runtime knows what host
;; capability the case assumes. A row certified only because the oracle supplied a
;; name jolt resolves out of the box is NOT portable source — its value agrees with
;; Clojure, but a plain Clojure file would need the import or require spelled out.
(defn row-features [bucket suite label actual supplied]
  (cond
    (seq supplied) [:host/implicit-imports]
    :else
    (case bucket
      (:certified :certified-throws) []
      (:divergent :throws-mismatch :jvm-raises)
      [(category->feature (allow-category [suite label] :host/jvm-interop) :host/jvm-interop)]
      :read-error [:reader/jolt]
      :timeout [:perf/unbounded]
      :uncertifiable [(uncertifiable-feature actual)]
      [:host/jvm-interop])))

;; The classifier IS the gate's judgment, so it carries a fixture: one row per
;; bucket, checked by `--self-test`. The :jvm-raises row is the case the bucket
;; split exists for — (.-value {:value 41}) answers 41 on jolt and raises on the
;; JVM, and used to be filed as "not certifiable" and skipped.
(def self-test-rows
  [{:bucket :certified        :expected "3"       :actual "(+ 1 2)"}
   {:bucket :certified-throws :expected :throws   :actual "(/ 1 0)"}
   {:bucket :divergent        :expected "4"       :actual "(+ 1 2)"}
   {:bucket :throws-mismatch  :expected :throws   :actual "(+ 1 2)"}
   {:bucket :jvm-raises       :expected "41"      :actual "(.-value {:value 41})"}
   {:bucket :uncertifiable    :expected "true"    :actual "(atom? (atom 1))"}
   {:bucket :uncertifiable    :expected "2"       :actual "(try 1 (catch :default e 2))"}
   {:bucket :uncertifiable    :expected "1"       :actual "(do (require '[no.such.lib]) 1)"}
   {:bucket :expected-error   :expected "(1 2)"   :actual "(list 1 2)"}
   {:bucket :read-error       :expected "1"       :actual "(1 ]"}
   {:bucket :timeout          :expected "nil"     :actual "(Thread/sleep 60000)"}])

;; A valued flag's argument is not the corpus path. Checked here because getting it
;; wrong is silent: certify runs happily against the wrong file and reports a clean
;; but meaningless result.
(def arg-test-cases
  [[[] nil]
   [["c.edn"] "c.edn"]
   [["--self-test"] nil]
   [["--record-documented"] nil]
   [["--edn" "out.edn"] nil]
   [["--profile" "p.edn"] nil]
   [["c.edn" "--profile" "p.edn"] "c.edn"]
   [["--profile" "p.edn" "c.edn"] "c.edn"]
   [["--edn" "o.edn" "--profile" "p.edn"] nil]])

(defn arg-self-test []
  (for [[args want] arg-test-cases
        :let [got (first (positional-args args))]
        :when (not= got want)]
    (format "  self-test FAIL: args %s — expected corpus %s, got %s"
            (pr-str args) (pr-str want) (pr-str got))))

(defn self-test []
  (let [got (mapv (fn [row] (assoc row :got (:bucket (classify row)))) self-test-rows)
        bad (remove #(= (:bucket %) (:got %)) got)
        arg-bad (arg-self-test)]
    (doseq [{:keys [bucket got actual]} bad]
      (println (format "  self-test FAIL: %s — expected bucket %s, got %s" actual bucket got)))
    (doseq [m arg-bad] (println m))
    (println (format "certify self-test: %d/%d bucket fixtures pass, %d/%d arg-parse cases pass"
                     (- (count got) (count bad)) (count got)
                     (- (count arg-test-cases) (count arg-bad)) (count arg-test-cases)))
    (System/exit (if (or (seq bad) (seq arg-bad)) 1 0))))

;; The corpus is measured on this JDK or newer: java.util.SequencedCollection and
;; its List/Deque methods (JDK 21) have rows. An older oracle would report them
;; as NEW divergences, which is not a fact about jolt — refuse to judge instead.
;; CI keeps its JAVA_HOME on the JDK it installs for this; the `clojure` launcher
;; runs whichever java JAVA_HOME names, not the newest one on the machine.
(def oracle-jdk-floor 21)

(defn check-oracle-jdk! []
  (let [feature (.feature (Runtime/version))]
    (when (< feature oracle-jdk-floor)
      (println (format "certify: the oracle is JDK %s (%s), but the corpus is measured on JDK %d or newer."
                       (System/getProperty "java.runtime.version") (System/getProperty "java.home") oracle-jdk-floor))
      (println "        Point JAVA_HOME at a newer JDK; the clojure launcher runs $JAVA_HOME/bin/java.")
      (System/exit 2))))

(defn -main [& _]
  (check-oracle-jdk!)
  (when (some #{"--self-test"} *command-line-args*) (self-test))
  (when (some #{"--record-documented"} *command-line-args*) (record-documented))
  (let [;; forced here, not left lazy: these evaluate programs, and a gate should
        ;; run its checks where it says it does rather than wherever the seq is
        ;; first realized.
        documented-problems (vec (concat (check-legend) (check-documented)))
        corpus (edn/read-string (slurp corpus-path))
        results (mapv (fn [row] (assoc (classify row) :row row)) corpus)
        by (group-by :bucket results)
        n (count results)
        cnt #(count (get by % []))]
    (println (format "Certifying %d corpus rows against JVM Clojure %s on JDK %s\n" n (clojure-version)
                     (System/getProperty "java.runtime.version")))
    (println (format "  certified        %5d  (jolt expected == JVM)" (cnt :certified)))
    (println (format "  certified-throws %5d  (:throws, JVM also throws)" (cnt :certified-throws)))
    (println (format "  uncertifiable    %5d  (JVM lacks the vocabulary — jolt-only fn/class/lib)" (cnt :uncertifiable)))
    (println (format "  read-error       %5d  (won't read on JVM reader)" (cnt :read-error)))
    (println (format "  timeout          %5d  (exceeded %dms — infinite/blocking)" (cnt :timeout) case-timeout-ms))
    (println (format "  throws-mismatch  %5d  <-- :throws but the JVM returned a value" (cnt :throws-mismatch)))
    (println (format "  jvm-raises       %5d  <-- jolt answers a value, the JVM raises" (cnt :jvm-raises)))
    (println (format "  DIVERGENT        %5d  <-- corpus :expected disagrees with JVM" (cnt :divergent)))
    (println (format "  expected-error   %5d  <-- :expected source does not evaluate (CORPUS BUG)" (cnt :expected-error)))
    (let [certifiable (+ (cnt :certified) (cnt :certified-throws) (cnt :divergent)
                         (cnt :throws-mismatch) (cnt :jvm-raises))]
      (println (format "\n  certifiable rows: %d  (certified %d / divergent %d / throws-mismatch %d / jvm-raises %d)"
                       certifiable (+ (cnt :certified) (cnt :certified-throws))
                       (cnt :divergent) (cnt :throws-mismatch) (cnt :jvm-raises))))
    ;; Why the JVM had no opinion, so the shape of what the oracle can't reach stays
    ;; visible rather than being one opaque number.
    (when (pos? (cnt :uncertifiable))
      (println (format "  uncertifiable by reason: %s"
                       (str/join ", " (map (fn [[k v]] (format "%s %d" (name k) v))
                                           (sort-by key (frequencies (map :reason (get by :uncertifiable)))))))))

    ;; An :expected that doesn't evaluate asserts nothing on either runner — the jolt
    ;; side crashes on it and this side can't compare it. Always a corpus bug.
    (when (pos? (cnt :expected-error))
      (println "\n=== :expected source does not evaluate — gate FAILS ===")
      (doseq [{:keys [row detail]} (get by :expected-error)]
        (println (format "  [%s] %s\n      expected: %s\n      %s"
                         (:suite row) (:label row) (:expected row) detail))))

    ;; A row that timed out asserted NOTHING on this run, and the budget is
    ;; wall-clock on a shared JVM: future-cancel is best-effort, so an earlier
    ;; runaway keeps its thread (and its allocation) for the rest of the run and
    ;; can push a later row over the budget on a slower or busier machine. That
    ;; makes the set environment-dependent, which is why every timed-out row is
    ;; named here rather than left as a count, and why staleness below ignores
    ;; them: an allowlist entry for a row the oracle never finished is not
    ;; evidence the divergence went away.
    (when (pos? (cnt :timeout))
      (println (format "\n=== rows the oracle did not finish in %dms (no opinion this run) ===" case-timeout-ms))
      (doseq [{:keys [row]} (sort-by (comp (juxt :suite :label) :row) (get by :timeout))]
        (println (format "  [%s] %s\n      actual: %s" (:suite row) (:label row) (:actual row)))))

    ;; Partition the rows the JVM has an opinion on into known (allowlisted) vs NEW.
    (let [flagged (concat (get by :divergent []) (get by :throws-mismatch []) (get by :jvm-raises []))
          key-of (fn [{:keys [row]}] [(:suite row) (:label row)])
          new? (fn [r] (let [k (key-of r)] (and (not (known k)) (not (flaky k)))))
          news (filter new? flagged)
          flagged-keys (set (map key-of flagged))
          ;; Rows the oracle had no opinion on: it never ran them to a value, so
          ;; they are evidence of nothing in either direction.
          silent-keys (set (map key-of (concat (get by :timeout []) (get by :read-error [])
                                               (get by :uncertifiable []))))
          stale (clojure.set/difference known flagged-keys silent-keys)]
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

    ;; Conformance profile: every NON-portable case keyed by [suite label] -> its
    ;; feature(s) + bucket. Certified (portable) cases are omitted — they are the
    ;; baseline every runtime must pass. A runtime computes its conformance LEVEL by
    ;; subtracting the features it doesn't implement. Written when --profile is given.
    (when profile-out
      (let [entries (->> results
                         (remove #(and (#{:certified :certified-throws} (:bucket %))
                                       (empty? (:supplied %))))
                         (map (fn [{:keys [bucket row supplied]}]
                                (let [{:keys [suite label actual]} row]
                                  {:suite suite :label label :bucket bucket
                                   :features (row-features bucket suite label actual supplied)})))
                         (sort-by (juxt :suite :label)) vec)
            feat-counts (->> entries (mapcat :features) frequencies (into (sorted-map)))]
        (spit profile-out
              (with-out-str
                (pp/pprint
                  {:doc (str "Conformance profile for test/chez/corpus.edn, generated by certify.clj. "
                             "Each entry is a NON-portable case (keyed by [suite label]) and the host "
                             "feature(s) it requires. Cases NOT listed are portable — they pass on any "
                             "faithful Clojure. A runtime's conformance LEVEL = portable + the feature "
                             "families it implements. See SPEC.md.")
                   :clojure-version (clojure-version)
                   ;; certified rows MINUS the ones that only certified because the
                   ;; oracle supplied a name — those are listed as non-portable.
                   :portable-count (count (filter #(and (#{:certified :certified-throws} (:bucket %))
                                                        (empty? (:supplied %)))
                                                  results))
                   :non-portable-count (count entries)
                   :feature-counts feat-counts
                   :entries entries})))
        (println (format "\nwrote conformance profile (%d non-portable cases) to %s" (count entries) profile-out))))

    ;; Full per-row divergence detail goes to the --edn report (for triage); the
    ;; console stays quiet about KNOWN divergences (the NEW/STALE sections above are
    ;; what matters for the gate).
    (when edn-out
      (spit edn-out (with-out-str
                      (pp/pprint {:corpus corpus-path
                                  :clojure-version (clojure-version)
                                  :counts (into {} (map (fn [[k v]] [k (count v)]) by))
                                  :divergent (mapv (fn [r] (assoc (:row r) :detail (:detail r))) (get by :divergent))
                                  :throws-mismatch (mapv (fn [r] (assoc (:row r) :detail (:detail r))) (get by :throws-mismatch))
                                  :jvm-raises (mapv (fn [r] (assoc (:row r) :detail (:detail r))) (get by :jvm-raises))
                                  :uncertifiable (mapv (fn [r] (assoc (:row r) :reason (:reason r) :detail (:detail r)))
                                                       (get by :uncertifiable))})))
      (println (format "\nwrote machine-readable report to %s" edn-out)))

    ;; Gate: fail on a NEW (unlisted) divergence, a stale allowlist entry, or an
    ;; :expected that doesn't evaluate. Every current divergence is either
    ;; intentional (classified in the allowlist) or a tracked bug — so a clean run
    ;; means the corpus matches reference Clojure everywhere it claims to, modulo the
    ;; documented jolt-specific deltas.
    ;; The :documented list is gated too: its entries are prose about divergences
    ;; that are not corpus rows, so nothing above would notice one going stale.
    (println (format "\ndocumented-divergence gate (JVM half): %d machine-checked, %d prose"
                     (count (filter #(map? (:check %)) documented-entries))
                     (count (filter #(= :prose (:check %)) documented-entries))))
    (when (seq documented-problems)
      (println (format "\n%d DOCUMENTED-DIVERGENCE FAILURE(S):" (count documented-problems)))
      (doseq [m documented-problems] (println m)))

    (System/exit (if (or (seq new-divergences) (seq stale-entries)
                         (seq documented-problems) (pos? (cnt :expected-error)))
                   1 0))))

(apply -main *command-line-args*)
