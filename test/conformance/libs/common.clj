;; Shared by the library-conformance drivers: run.clj (the tally gate) and
;; timing.clj (the jolt-vs-JVM per-test timing report). Where the checkouts
;; are, how a manifest path resolves, which namespaces a suite defines, and the
;; jolt command line that runs one library's suites. Loaded with load-file.
(ns lib-conformance-common
  (:require [clojure.string :as str]
            [jolt.process :as p]))

(def repo-root
  (or (System/getenv "JOLT_REPO_ROOT") (System/getProperty "user.dir")))

(def here (str repo-root "/test/conformance/libs"))

;; The library checkouts sit beside the MAIN jolt checkout. A git worktree has its
;; own root (.claude/worktrees/<x>), whose parent holds none of them, so ask git
;; for the common dir: it names the main checkout's .git from either kind.
(def checkout-parent
  (let [r (try (p/sh {:out :string :err :string :dir repo-root}
                     "git" "rev-parse" "--path-format=absolute" "--git-common-dir")
               (catch Exception _ nil))
        common (some-> r :out str/trim)]
    (if (and r (zero? (:exit r)) (seq common))
      (.getParent (.getParentFile (java.io.File. common)))
      (.getParent (java.io.File. repo-root)))))

(def libs-root
  (or (System/getenv "JOLT_CONFORMANCE_LIBS")
      (str checkout-parent "/conformance-libraries")))

(def siblings-root
  ;; first-party jolt libraries (xml, time, db, ...) sit beside the jolt checkout
  (or (System/getenv "JOLT_SIBLING_LIBS") checkout-parent))

;; Absolute: each child runs with :dir set to the library's own root, so a
;; relative JOLT_BIN (what the Makefile passes) would not resolve there.
(def jolt-bin
  (let [b (or (System/getenv "JOLT_BIN") "target/release/jolt")]
    (if (str/starts-with? b "/") b (str repo-root "/" b))))

;; ---------------------------------------------------------------- paths

(defn exists? [p] (.exists (java.io.File. p)))

(defn resolve-path
  "A manifest path is relative to the library's own root unless it starts with
  `@` (relative to the conformance-libraries root), `~` (a first-party jolt
  sibling library), `!` (this repo's root) or `/` (absolute)."
  [lib-root p]
  (cond
    (str/starts-with? p "/") p
    (str/starts-with? p "@") (str libs-root "/" (subs p 1))
    (str/starts-with? p "~") (str siblings-root "/" (subs p 1))
    (str/starts-with? p "!") (str repo-root "/" (subs p 1))
    :else (str lib-root "/" p)))

(defn lib-root [{:keys [name root]}]
  (str libs-root "/" (or root name)))

;; ---------------------------------------------------------------- ns discovery

;; The name in an `ns` form may sit behind metadata — `(ns ^{:doc "..."} the.name`
;; is how several contrib suites are written — so skip any leading ^{...} / ^:kw.
(def ^:private ns-form-re
  #"\(ns\s+(?:\^(?:\{[^}]*\}|[^\s]+)\s+)*([a-zA-Z][^\s\(\)\[\]{},;]*)")

(defn- ns-name-of [file]
  (let [src (slurp file)]
    (when-let [m (re-find ns-form-re src)]
      (second m))))

(defn- test-file?
  "Does this file define tests? Matches `deftest`/`defspec` however it is
  qualified — plenty of suites call it `t/deftest` through an alias rather than
  referring it in."
  [src]
  (boolean (re-find #"\(\s*(?:[\w.\-]+/)?(?:deftest|defspec)[\s\n]" src)))

(defn- walk-files [dir]
  (let [f (java.io.File. dir)]
    (if (.isDirectory f)
      (mapcat walk-files (map str (.listFiles f)))
      [dir])))

(defn discover-nses
  "Every namespace under `test-paths` that defines tests, by its own ns form."
  [test-dirs]
  (->> test-dirs
       (filter exists?)
       (mapcat walk-files)
       (filter #(or (str/ends-with? % ".clj") (str/ends-with? % ".cljc")))
       (keep (fn [f]
               (let [src (slurp f)]
                 (when (test-file? src) (ns-name-of f)))))
       distinct
       sort))

;; ---------------------------------------------------------------- jolt command

(defn plan
  "How to run ENTRY's suites on jolt: {:missing [paths]}, {:no-tests true}, or
  {:cmd-prefix [...] :nses [...] :dir d :env {...}}. The runner main and its own
  arguments go between :cmd-prefix and the namespaces."
  [{:keys [dir paths deps local-deps extra-deps nses exclude-nses shims] :as entry}]
  (let [lib-root (lib-root entry)
        srcs (map #(resolve-path lib-root %) (or paths ["src" "test"]))
        ;; A shim stands in for something the library expects from the JVM that jolt
        ;; has no equivalent for — a Java logging backend, a JSON library built on
        ;; Jackson. It goes AHEAD of the library's own sources and of its resolved
        ;; deps so it wins the namespace, which is the whole point: the alternative
        ;; is editing the checkout, and an edit there silently turns the recorded
        ;; tally into a measurement of our own source. Shims live on jolt-owned
        ;; paths (`!` = this repo, `~` = a first-party jolt library), never inside
        ;; a checkout.
        shims (map #(resolve-path lib-root %) (or shims []))
        deps (map #(resolve-path lib-root %) (or deps []))
        ;; A first-party jolt library must go on as a real :local/root dependency,
        ;; not a bare source path: its deps.edn is what declares :jolt/native, and
        ;; without that the shared library it binds is never loaded. jolt-crypto on
        ;; :paths alone found whatever libcrypto the dynamic loader had — on macOS
        ;; that is Apple's BoringSSL, which aborts the process inside EVP_Digest.
        local-deps (map #(resolve-path lib-root %) (or local-deps []))
        test-dirs (map #(resolve-path lib-root %) (:test-paths entry ["test"]))
        cp (vec (concat [(str here "/runner")] shims srcs deps))
        missing (remove exists? (concat cp local-deps))
        nses (remove (set (or exclude-nses []))
                     (or nses (discover-nses test-dirs)))]
    (cond
      (seq missing) {:missing (vec missing)}
      (empty? nses) {:no-tests true}
      :else
      (let [locals (into {} (map (fn [p] [(symbol "jolt-lang" (.getName (java.io.File. p)))
                                          {:local/root p}])
                                 local-deps))
            all-deps (merge (or extra-deps {}) locals)
            sdeps (cond-> {:paths cp} (seq all-deps) (assoc :deps all-deps))]
        {:cmd-prefix [jolt-bin "-Sdeps" (pr-str sdeps)]
         :nses (vec nses)
         ;; A suite that opens a file by a project-relative path needs the
         ;; working directory its own build uses — in a multi-module repo that
         ;; is the MODULE root, not the repo root (ring-core reads
         ;; test/ring/assets/…, which only resolves from ring/ring-core).
         :dir (if dir (resolve-path lib-root dir) lib-root)
         ;; JOLT_MAX_HEAP=off: this is a stress harness, not a user
         ;; workload. 0.8.5 gave jolt a heap ceiling defaulting to 25% of
         ;; RAM (the share the JVM's MaxRAMPercentage uses), which is the
         ;; right default for a program but wrong here — malli's suite alone
         ;; has a live set around 2.5GB, so on any machine with under ~10GB
         ;; the ceiling would fail the suite before it could report a tally,
         ;; and the tally is the whole output. A library that needs a bound
         ;; can ask for one; the harness does not impose one.
         :env {"JOLT_NO_USER_DEPS" "1" "JOLT_MAX_HEAP" "off"}}))))
