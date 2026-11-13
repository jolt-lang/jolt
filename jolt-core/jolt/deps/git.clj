(ns jolt.deps.git
  "Git coordinates for jolt.deps: the sha a coordinate pins (tags resolved
  through git ls-remote), the clone cache and its staged, atomically published
  checkouts, reuse of an existing tools.gitlibs checkout, the tools.deps
  host-prefix URL inference, and commit ancestry for version comparison."
  (:require [clojure.string :as str]
            [jolt.deps.io :refer [getenv file-exists? sh mkdirs! rm-rf mv! touch!
                                  sh-out* sanitize net-attempts with-net-retries]]))

;; --- git cache --------------------------------------------------------------
;; jolt's own clone cache. $GITLIBS (the tools.gitlibs location knob) is
;; respected for WHERE the cache lives — under a jolt/ subdir so tools.gitlibs'
;; own _repos/ and libs/ namespaces are never written to. JOLT_GITLIBS_DIR
;; pins an exact directory.
(defn gitlibs-dir
  ([] (gitlibs-dir (getenv "JOLT_GITLIBS_DIR")
                   (getenv "GRENADINE_GITLIBS_DIR")
                   (getenv "GITLIBS")
                   (getenv "HOME")))
  ([jolt-cache grenadine-cache gitlibs home]
   (or jolt-cache
       (when grenadine-cache (str grenadine-cache "/jolt"))
       (when gitlibs (str gitlibs "/jolt"))
       (str (or home ".") "/.jolt/gitlibs"))))

(defn gitlibs-shared-checkout
  "An existing tools.gitlibs checkout for lib@sha ($GITLIBS or ~/.gitlibs,
  layout libs/<group>/<name>/<sha>) — reused read-only when the JVM toolchain
  already fetched this dep. jolt never writes there: tools.gitlibs keeps its
  own bookkeeping (_repos bare clones + worktrees) that a foreign writer could
  corrupt, so jolt's own fetches go to its cache below."
  [lib sha]
  (when (and lib (namespace lib))
    (let [base (or (getenv "GRENADINE_GITLIBS_DIR")
                   (getenv "GITLIBS")
                   (str (or (getenv "HOME") ".") "/.gitlibs"))
          dir (str base "/libs/" (namespace lib) "/" (name lib) "/" sha)]
      (when (file-exists? dir) dir))))

(defn- full-sha? [s] (and (string? s) (= 40 (count s)) (re-matches #"[0-9a-f]+" s)))

(defn git-coord
  "A git coordinate with tools.deps' legacy spellings folded into the namespaced
  keys. deps.edn files in the wild still write :sha and :tag — malli pins
  spec-alpha2 as {:git/url … :sha …} — and tools.deps accepts both, so a
  coordinate that resolves there has to resolve here. The namespaced key wins if
  both are somehow present."
  [coord]
  (cond-> coord
    (and (:sha coord) (not (:git/sha coord))) (assoc :git/sha (:sha coord))
    (and (:tag coord) (not (:git/tag coord))) (assoc :git/tag (:tag coord))))

(defn- read-tag-cache
  "The [commit tag-object] pair a tag cache file records, or nil when there is
  no file or it holds the single token an older jolt wrote (see
  resolve-git-tag)."
  [cache]
  (when (file-exists? cache)
    (let [[commit obj] (str/split (str/trim (slurp cache)) #"\s+")]
      (when obj [commit (when (not= obj "-") obj)]))))

;; "the tag is not there" and "we could not go look" are different answers.
;; Collapsing them reported a transient ls-remote failure — a reset, a rate
;; limit, an unreachable host — as a tag that does not exist, which sends
;; the reader to the repository and the pin instead of to the fetch. (The
;; v0.7.7 release failed this way against a tag two weeks old.) Same
;; distinction the Maven path in jolt.deps makes, and for the same reason.
(defn- ls-remote-tag
  "git ls-remote's output for refs/tags/TAG and its peeled ^{} ref at URL,
  retried like every network step. Throws when the listing itself fails; an
  absent tag is a successful listing with no lines."
  [url tag]
  (let [{:keys [exit out err]}
        (with-net-retries #(zero? (:exit %))
          #(sh-out* (str "git ls-remote " (pr-str url) " "
                         (pr-str (str "refs/tags/" tag)) " "
                         (pr-str (str "refs/tags/" tag "^{}")))))]
    (when-not (zero? exit)
      (throw (ex-info (str "git dep: could not list " url " (git ls-remote exited "
                           exit " after " net-attempts " attempts"
                           (when-not (str/blank? err)
                             (str ": " (first (str/split-lines err))))
                           ")")
                      {:type :jolt.deps/git-ls-remote-failed
                       :url url :tag tag :exit exit :err err :attempts net-attempts})))
    out))

(defn- parse-tag-refs
  "[commit tag-object] from ls-remote OUT for TAG — tag-object nil for a
  lightweight tag — or nil when the tag is not listed."
  [out tag]
  (let [lines (str/split-lines (or out ""))
        parse (fn [suffix]
                (some (fn [l] (let [[sha ref] (str/split l #"\s+")]
                                (when (and ref (str/ends-with? ref suffix)) sha)))
                      lines))
        peeled (parse "^{}")
        obj (parse (str "refs/tags/" tag))
        commit (or peeled obj)
        obj (when (and peeled obj (not= peeled obj)) obj)]
    (when commit [commit obj])))

(defn- resolve-git-tag
  "Resolve a tag via git ls-remote to [commit-sha tag-object-sha]. An annotated
  tag carries both: the peeled ^{} ref is the commit it points at, the unpeeled
  ref is the tag object itself. A deps.edn may pin EITHER — `git ls-remote`
  prints the tag object for refs/tags/X, so that is what a coordinate written
  from its output holds (cognitect-labs/test-runner v0.5.0 is one), and
  tools.deps accepts both. tag-object-sha is nil for a lightweight tag, where
  the two coincide. Cached under the gitlibs dir as one line of two tokens —
  the commit, then the tag object or \"-\" when there is none. A one-token file
  is what an older jolt wrote, which recorded only the commit; it is re-resolved
  rather than trusted, so a coordinate pinning the tag object is not rejected
  off a cache that predates knowing about it."
  [url tag]
  (let [cache (str (gitlibs-dir) "/tags/" (sanitize url) "/" (sanitize tag))]
    (or (read-tag-cache cache)
        (when-let [[commit obj :as resolved] (parse-tag-refs (ls-remote-tag url tag) tag)]
          (mkdirs! (str (gitlibs-dir) "/tags/" (sanitize url)))
          (spit cache (str commit " " (or obj "-")))
          resolved))))

(defn resolve-git-sha
  "The full commit sha a git coordinate pins: a full :git/sha as-is; with a
  :git/tag, a short (prefix) sha is completed from the tag's commit and a full
  sha is verified against it — like tools.deps, a tag alone doesn't pin."
  [coord url {:git/keys [sha tag] :as spec}]
  (cond
    (and sha (full-sha? sha) (not tag)) sha
    (and tag sha)
    (let [[tag-sha obj-sha]
          (or (resolve-git-tag url tag)
              (throw (ex-info (str "git dep " coord ": tag " tag " not found in " url)
                              {:coord coord :spec spec})))]
      ;; Either sha an annotated tag carries pins it, as in tools.deps. What gets
      ;; checked out is always the commit.
      (if (or (str/starts-with? tag-sha sha)
              (and obj-sha (str/starts-with? obj-sha sha)))
        tag-sha
        (throw (ex-info (str "git dep " coord ": :git/sha " sha " does not match tag "
                             tag " (" tag-sha
                             (when obj-sha (str ", tag object " obj-sha)) ")")
                        {:coord coord :spec spec :tag-sha tag-sha
                         :tag-object-sha obj-sha}))))
    (full-sha? sha) sha
    :else
    (throw (ex-info
             (str "git dep " coord " needs :git/sha — the full commit sha, or a "
                  "prefix of it alongside :git/tag"
                  (when (and tag (not sha)) " (a :git/tag alone doesn't pin a commit)") ".")
             {:coord coord :spec spec}))))

(defn checkout-complete?
  "Does `dir` hold a finished checkout? jolt publishes a checkout by moving a
  fully populated staging dir into place and marks it with `.jolt-git-ok`, so
  anything jolt wrote there is complete. A checkout an older jolt cloned in
  place carries no marker, and its `.git` stands in — which still rejects the
  empty directory an interrupted in-place clone left behind. That leftover is
  why this check exists: git only cleans up a clone directory it created itself,
  the in-place clone pre-created it with mkdir -p, and a plain existence test
  then read the empty remains as a valid checkout. The dep resolved to nothing
  on every later run, and the failure surfaced much later as `Could not locate`
  on one of its namespaces."
  [dir]
  (or (file-exists? (str dir "/.jolt-git-ok"))
      (file-exists? (str dir "/.git"))))

(defn fetch-git!
  "Clone url@sha under `repo-dir` and return the checkout dir. Silent: the
  caller (jolt.deps/ensure-git) prints the progress line before calling. The work happens
  in a staging dir beside it (same filesystem, so publishing is a rename) and
  any failure removes it: the cache only ever holds finished checkouts. The
  staging name carries a nonce so two jolt processes fetching the same dep at
  once don't clone over each other."
  [url sha repo-dir]
  (let [dir (str repo-dir "/" sha)
        stage (str repo-dir "/.part-" sha "-" (System/currentTimeMillis) "-" (rand-int 100000))
        scrub #(rm-rf stage)
        fail (fn [msg data] (scrub) (throw (ex-info msg (merge {:url url :sha sha} data))))]
    (mkdirs! repo-dir)
    ;; the clone talks to the remote, so it gets the same bounded retry the tag
    ;; listing does — scrubbing first, since a failed clone leaves a partial
    ;; staging dir that the next attempt would refuse to clone into.
    (when-not (zero? (with-net-retries zero? (fn [] (scrub) (sh (str "git clone --quiet " (pr-str url) " " (pr-str stage))))))
      (fail (str "git clone failed after " net-attempts " attempts: " url) nil))
    ;; local, so no retry: the objects are already in the staging clone.
    (when-not (zero? (sh (str "git -C " (pr-str stage) " checkout --quiet " (pr-str sha))))
      (fail (str "git checkout failed: " sha " in " url) nil))
    ;; submodules are pinned in the checkout; pull them if the dep uses any. This
    ;; one fetches too, and is idempotent, so it retries in place.
    (when-not (zero? (with-net-retries zero? #(sh (str "git -C " (pr-str stage) " submodule update --init --recursive --quiet"))))
      (fail (str "git submodule update failed after " net-attempts " attempts for " url) nil))
    (touch! (str stage "/.jolt-git-ok"))
    (if (checkout-complete? dir)
      (do (scrub) dir)                    ; another process published it first
      (do
        ;; clears the incomplete remains of an interrupted in-place clone
        (rm-rf dir)
        (when-not (mv! stage dir)
          (fail (str "git checkout could not be moved into the cache: " dir) {:dir dir}))
        dir))))

;; --- git URL inference ------------------------------------------------------
;; tools.deps lets a git coordinate omit :git/url when the lib name encodes a
;; known host: `io.github.OWNER/REPO` resolves to https://github.com/OWNER/REPO.git,
;; and similarly for GitLab, Bitbucket, and Sourcehut. jolt honors the same
;; convention so a deps.edn copied from a tools.deps project resolves unchanged.
;; See https://clojure.org/reference/deps_edn#deps_git.
(def ^:private git-url-hosts
  ;; [namespace-prefix  url-prefix  url-suffix] — the URL is
  ;; url-prefix + OWNER + "/" + REPO + url-suffix, where OWNER is the coordinate
  ;; namespace with its host prefix stripped and REPO is the coordinate name.
  [["io.github."    "https://github.com/"    ".git"]
   ["com.github."   "https://github.com/"    ".git"]
   ["io.gitlab."    "https://gitlab.com/"    ".git"]
   ["com.gitlab."   "https://gitlab.com/"    ".git"]
   ["io.bitbucket."  "https://bitbucket.org/" ".git"]
   ["org.bitbucket." "https://bitbucket.org/" ".git"]
   ;; Sourcehut lib names carry the leading ~ in the owner, e.g. ht.sr.~owner/repo,
   ;; so stripping "ht.sr." leaves "~owner" and no .git suffix is used.
   ["ht.sr."        "https://git.sr.ht/"     ""]])

(defn infer-git-url
  "The git clone URL a coordinate's lib name implies via the tools.deps host
  convention (io.github.OWNER/REPO -> https://github.com/OWNER/REPO.git), or nil
  when the namespace names no known host."
  [coord]
  (when-let [ns (namespace coord)]
    (some (fn [[prefix url-prefix url-suffix]]
            (when (str/starts-with? ns prefix)
              (str url-prefix (subs ns (count prefix)) "/" (name coord) url-suffix)))
          git-url-hosts)))

;; sh-out* rather than sh: this asks a question whose "no" arrives as both a
;; non-zero exit and a message on stderr, and the 2>/dev/null that used to
;; silence it is a POSIX path, not a portable way to discard output.
(defn git-ancestor? [dir a b]
  (zero? (:exit (sh-out* (str "git -C " (pr-str dir) " merge-base --is-ancestor " a " " b)))))
