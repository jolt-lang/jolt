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
    (let [before (slurp path)
          after (-> before
                    (str/replace "joltc" "jolt")
                    (bump base ver "aarch64-macos" mac)
                    (bump base ver "x86_64-linux" lin)
                    (bump base ver "x86_64-macos" intel-mac))]
      (doseq [[target sha] [["aarch64-macos" mac] ["x86_64-linux" lin] ["x86_64-macos" intel-mac]]]
        (when-not (str/includes? after (str "jolt-" ver "-" target ".tar.gz\"\n"))
          (binding [*out* *err*] (println (str "bump-formula.clj: no url line for " target " in " path)))
          (System/exit 1))
        (when-not (str/includes? after (str "sha256 \"" sha "\""))
          (binding [*out* *err*] (println (str "bump-formula.clj: the " target " sha256 line was not rewritten")))
          (System/exit 1)))
      (spit path after)
      (println (str path ": " (if (= before after) "unchanged" (str "bumped to " ver)))))))

(apply -main *command-line-args*)
