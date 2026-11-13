;; metrics-gate.clj — fail the build on what ci/metrics-allow.edn rules out:
;;
;;   jolt ci/metrics-gate.clj <report.edn> <ci/metrics-allow.edn>
;;
;; The report is jolt-metrics' --format edn output. Two checks:
;;
;; * Critical findings. Under each path prefix in :critical-free, any CRITICAL
;;   finding in the refactoring queue fails the gate (a function past a hard
;;   complexity, length, nesting or parameter limit, or a file past the file
;;   length limit), and so does a file jolt-metrics could not read, since it
;;   went unscored. There is no allowlist for these: fix the code.
;;
;; * Unused code. The allowlist names each unused require, unused private
;;   function and unreferenced public function that stays on purpose, with the
;;   reason: a caller the tool can't see (a var-deref from Scheme, a #' lookup),
;;   a deliberate re-export, upstream code kept as written. A finding not on it
;;   fails the gate, and so does an entry that no longer matches a finding, so
;;   the list can't go stale.
(require '[clojure.edn :as edn]
         '[clojure.string :as str])

(defn- finding-key [kind f]
  (case kind
    :unused-requires [(:path f) (:lib f)]
    (:unused-private :unused-public) [(:path f) (:name f)]))

(defn- under? [prefixes path]
  (some #(str/starts-with? path %) prefixes))

(defn- critical-problems [report prefixes]
  (concat
   (for [f (:queue report)
         :when (and (= :critical (:severity f)) (under? prefixes (:path f)))]
     (str "critical " (name (:kind f)) " " (:path f) ":" (:line f) " " (:name f) " — "
          (str/join ", " (for [v (:violations f) :when (= :critical (:band v))]
                           (str (name (:rule v)) " " (:value v) " > " (:limit v))))))
   (for [e (:errors report) :when (under? prefixes (:path e))]
     (str "unreadable " (:path e) " — " (:error e)))))

(defn- unused-problems [cleanup allow allow-path]
  (for [kind [:unused-requires :unused-private :unused-public]
        :let [found   (into {} (map (fn [f] [(finding-key kind f) f]) (get cleanup kind)))
              allowed (into {} (map (fn [a] [(finding-key kind a) a]) (get allow kind)))]
        p (concat
           (for [[k f] (sort-by key found) :when (not (contains? allowed k))]
             (str "new " (name kind) ": " (pr-str (dissoc f :line))
                  " — remove it, or add it to " allow-path " with the reason it stays"))
           (for [[k _] (sort-by key allowed) :when (not (contains? found k))]
             (str "stale " (name kind) " entry " (pr-str k)
                  " — it is no longer reported; remove it from " allow-path)))]
    p))

(defn -main [report-path allow-path]
  (let [report (edn/read-string (slurp report-path))
        allow (edn/read-string (slurp allow-path))
        problems (concat (critical-problems report (:critical-free allow))
                         (unused-problems (:cleanup report) allow allow-path))]
    (doseq [p problems] (println p))
    (if (seq problems)
      (do (println (str "metrics gate: " (count problems) " problem(s)")) (System/exit 1))
      (println (str "metrics gate: no critical findings under "
                    (str/join ", " (:critical-free allow))
                    " and no unused code beyond the allowlist")))))

(apply -main *command-line-args*)
