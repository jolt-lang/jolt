;; Report the (ns ...) forms that jolt's ns macro refuses, without loading them.
;;
;;   jolt tools/ns_audit.clj [DIR-OR-FILE ...]     (default: stdlib jolt-core)
;;
;; An unescaped quote in an ns docstring splits it into strings and bare symbols,
;; and the file still reads. This finds that, and every other shape ns refuses, in
;; code nobody has loaded yet: a library checkout, or a namespace no test requires.
;; ns itself decides, so this has no rules of its own to fall out of step with it,
;; and it has to run on a jolt whose ns refuses them.
;;
;; Prints path:line: reason for each one and exits 1 if there were any. A file that
;; cannot be read up to its ns form is a finding as well: an odd number of unescaped
;; quotes looks like that. Reading is host-conditional, so a branch of a .cljc that
;; only another host reads is not checked.
(ns ns-audit
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def source-file? #"\.clj[c]?$")
(def skipped-dir? #{".git" ".cpcache" "target" "node_modules" ".lsp" ".clj-kondo"})

(defn- source-files [root]
  (let [walk (fn walk [^java.io.File f]
               (cond
                 (.isDirectory f)
                 (when-not (skipped-dir? (.getName f))
                   (mapcat walk (sort-by #(.getName %) (.listFiles f))))
                 (re-find source-file? (.getName f)) [f]))]
    (walk (io/file root))))

(defn- ns-form? [form]
  (and (seq? form) (= 'ns (first form))))

(defn- reason [e]
  (str/replace (or (ex-message (or (ex-cause e) e)) (str e)) #"\s*\n\s*" " "))

(defn- check-form
  "nil when ns accepts form, else a finding. macroexpand-1 only expands: the form's
  clauses do not run and its metadata is not evaluated."
  [path form]
  (try
    (macroexpand-1 form)
    nil
    (catch Throwable e
      (str path ":" (:line (meta form) "?") ": " (reason e)))))

(defn- audit-file
  "Findings for every ns form in the file. Past its first ns form a file may use
  aliases or tagged literals this reader cannot resolve, so a read failure there
  ends the scan instead of counting."
  [^java.io.File f]
  (let [path (.getPath f)
        eof (Object.)]
    (with-open [r (java.io.PushbackReader. (io/reader f))]
      (binding [*default-data-reader-fn* (fn [_ v] v)]
        (loop [seen-ns? false found []]
          (let [form (try
                       (read {:eof eof :read-cond :allow} r)
                       (catch Throwable e
                         (if seen-ns?
                           eof
                           {::unreadable (reason e)})))]
            (cond
              (identical? form eof) found

              (and (map? form) (::unreadable form))
              (conj found (str path ": cannot be read: " (::unreadable form)))

              (ns-form? form)
              (let [finding (check-form path form)]
                (recur true (if finding (conj found finding) found)))

              :else (recur seen-ns? found))))))))

(defn -main [& roots]
  (let [files (mapcat source-files (or (seq roots) ["stdlib" "jolt-core"]))
        findings (doall (mapcat audit-file files))]
    (doseq [finding findings] (println finding))
    (println (str "ns-audit: " (count files) " files, " (count findings) " findings"))
    (when (seq findings) (System/exit 1))))

(apply -main *command-line-args*)
