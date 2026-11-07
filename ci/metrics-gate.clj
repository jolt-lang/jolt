;; metrics-gate.clj — fail on unused code jolt-metrics finds that isn't on the
;; allowlist.
;;
;;   jolt ci/metrics-gate.clj <report.edn> <ci/metrics-allow.edn>
;;
;; The report is jolt-metrics' --format edn output. The allowlist names each
;; unused require, unused private function and unreferenced public function that
;; stays on purpose, with the reason: a caller the tool can't see (a var-deref from Scheme, a #' lookup), a
;; deliberate re-export, upstream code kept as written. A finding not on it fails
;; the gate, and so does an entry that no longer matches a finding, so the list
;; can't go stale.
(require '[clojure.edn :as edn])

(defn- finding-key [kind f]
  (case kind
    :unused-requires [(:path f) (:lib f)]
    (:unused-private :unused-public) [(:path f) (:name f)]))

(defn -main [report-path allow-path]
  (let [cleanup (:cleanup (edn/read-string (slurp report-path)))
        allow   (edn/read-string (slurp allow-path))
        problems
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
          p)]
    (doseq [p problems] (println p))
    (if (seq problems)
      (do (println (str "metrics gate: " (count problems) " problem(s)")) (System/exit 1))
      (println "metrics gate: no unused code beyond the allowlist"))))

(apply -main *command-line-args*)
