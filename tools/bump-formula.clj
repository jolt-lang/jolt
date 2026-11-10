;; bump-formula.clj — point the Homebrew formula at a release's tarballs.
;; The release workflow's `update homebrew tap` job runs it with the jolt it just
;; released, against the tap checkout:
;;
;;   jolt run tools/bump-formula.clj tap/Formula/jolt.rb \
;;        https://github.com/jolt-lang/jolt/releases/download/v0.8.6 v0.8.6 \
;;        <mac-sha> <linux-sha> <intel-mac-sha>
;;
;; Rewrites, for each of the three targets, the `url` line whose asset name ends
;; in <target>.tar.gz and the `sha256` line that follows the new url. Idempotent:
;; running it twice with the same arguments changes nothing. The one-time
;; joltc -> jolt rename of the pre-0.5 formula is kept for the same reason it
;; was: a formula already on the new name has no joltc tokens left.
(require '[clojure.string :as str])

(defn- bump [s base ver target sha]
  (let [url (str base "/jolt-" ver "-" target ".tar.gz")
        s (str/replace s (re-pattern (str "url \"[^\"]*" (java.util.regex.Pattern/quote target) "\\.tar\\.gz\""))
                       (str "url \"" url "\""))]
    (str/replace s
                 (re-pattern (str "(jolt-" (java.util.regex.Pattern/quote ver) "-" (java.util.regex.Pattern/quote target)
                                  "\\.tar\\.gz\"\\s*\\n\\s*sha256 \")[0-9a-f]{64}"))
                 (fn [[_ head]] (str head sha)))))

(defn -main [& [path base ver mac lin intel-mac & more]]
  (if (or (nil? intel-mac) (seq more))
    (do (binding [*out* *err*] (println "usage: bump-formula.clj FORMULA.rb BASE-URL VERSION MAC-SHA LINUX-SHA INTEL-MAC-SHA"))
        (System/exit 2))
    (let [_ (doseq [[target sha] [["aarch64-macos" mac] ["x86_64-linux" lin] ["x86_64-macos" intel-mac]]]
              ;; An empty or truncated sha (a failed .sha256 download inside a
              ;; $(...) the workflow's set -e does not see) would be written
              ;; into the tap as is.
              (when-not (re-matches #"[0-9a-f]{64}" sha)
                (binding [*out* *err*]
                  (println (str "bump-formula.clj: " target " sha256 is not 64 hex digits: \"" sha "\"")))
                (System/exit 1)))
          before (slurp path)
          ;; x86_64-macos is bumped only once the formula has an url line for
          ;; it: a tap that predates the Intel tarball keeps releasing instead of
          ;; failing the job, with a warning to add the on_intel block.
          intel? (re-find #"url \"[^\"]*x86_64-macos\.tar\.gz\"" before)
          targets (cond-> [["aarch64-macos" mac] ["x86_64-linux" lin]]
                    intel? (conj ["x86_64-macos" intel-mac]))
          after (reduce (fn [s [target sha]] (bump s base ver target sha))
                        (str/replace before "joltc" "jolt")
                        targets)]
      (when-not intel?
        (binding [*out* *err*]
          (println (str "bump-formula.clj: warning: " path " has no x86_64-macos url line; "
                        "add an on_macos/on_intel block to ship the Intel tarball"))))
      (doseq [[target sha] targets]
        (when-not (str/includes? after (str "jolt-" ver "-" target ".tar.gz\"\n"))
          (binding [*out* *err*] (println (str "bump-formula.clj: no url line for " target " in " path)))
          (System/exit 1))
        (when-not (str/includes? after (str "sha256 \"" sha "\""))
          (binding [*out* *err*] (println (str "bump-formula.clj: the " target " sha256 line was not rewritten")))
          (System/exit 1)))
      (spit path after)
      (println (str path ": " (if (= before after) "unchanged" (str "bumped to " ver)))))))

(apply -main *command-line-args*)
