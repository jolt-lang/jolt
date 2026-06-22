(ns jolt.deps
  "Resolve a deps.edn into an ordered list of source roots — git + local deps
  only, no Maven. A reduced tools.deps: :paths, :deps (`:git/url`+`:git/sha` /
  `:local/root`), :aliases (:extra-paths / :extra-deps / :main-opts), :tasks.

  The deps walk is breadth-first so a top-level coordinate registers before any
  transitive one (a top-level pin wins). Git deps clone into a sha-immutable
  cache ($JOLT_GITLIBS, else ~/.jolt/gitlibs) shared across projects. Resolution
  shells out to `git` through jolt.host/sh; nothing here touches the JVM."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]))

;; --- small host seams -------------------------------------------------------
(defn- getenv [n] (jolt.host/getenv n))
(defn- file-exists? [p] (jolt.host/file-exists? p))
(defn- sh [cmd] (jolt.host/sh cmd))           ; exit code, inherits stdout/stderr
(defn- sh-out [cmd] (jolt.host/sh-out cmd))   ; captured stdout
(defn- warn [& xs] (println (str "[jolt.deps] " (apply str xs))))

(defn- read-edn [path]
  (when (file-exists? path)
    (try (edn/read-string (slurp path))
         (catch :default e (warn "could not read " path ": " (ex-message e)) nil))))

(defn- abspath [dir p]
  (if (str/starts-with? p "/") p (str dir "/" p)))

;; --- git cache --------------------------------------------------------------
(defn- gitlibs-dir []
  (or (getenv "JOLT_GITLIBS")
      (str (or (getenv "HOME") ".") "/.jolt/gitlibs")))

(defn- alnum? [c]
  (let [n (int c)]
    (or (and (>= n 48) (<= n 57))     ; 0-9
        (and (>= n 65) (<= n 90))     ; A-Z
        (and (>= n 97) (<= n 122))))) ; a-z
(defn- sanitize [s]
  (str/join (map (fn [c] (if (or (alnum? c) (= c \.) (= c \-)) c \_)) (seq s))))

(defn- ensure-git
  "Clone url at sha into the cache (once); return the checkout dir."
  [url sha]
  (let [dir (str (gitlibs-dir) "/" (sanitize url) "/" sha)]
    (if (file-exists? dir)
      dir
      (do
        (warn "fetching " url " @ " (subs sha 0 (min 12 (count sha))))
        (sh (str "mkdir -p " (pr-str dir)))
        (when-not (zero? (sh (str "git clone --quiet " (pr-str url) " " (pr-str dir))))
          (throw (ex-info (str "git clone failed: " url) {:url url})))
        (when-not (zero? (sh (str "git -C " (pr-str dir) " checkout --quiet " (pr-str sha))))
          (throw (ex-info (str "git checkout failed: " sha " in " url) {:url url :sha sha})))
        ;; submodules are pinned in the checkout; pull them if the dep uses any.
        (sh (str "git -C " (pr-str dir) " submodule update --init --recursive --quiet"))
        dir))))

;; --- coordinate -> root dir -------------------------------------------------
(defn- coord-root
  "The on-disk root directory for one dependency coordinate, or nil to skip."
  [coord spec base-dir]
  (cond
    (:local/root spec) (abspath base-dir (:local/root spec))
    (and (:git/url spec) (:git/sha spec))
    (let [checkout (ensure-git (:git/url spec) (:git/sha spec))]
      (if-let [root (:deps/root spec)] (str checkout "/" root) checkout))
    (:jolt/module spec)
    (do (warn "skipping janet dependency " coord " (:jolt/module is obsolete on Chez)") nil)
    :else
    (do (warn "skipping unsupported coordinate " coord " " (pr-str spec)) nil)))

(defn- dep-source-roots
  "Source roots a resolved dep contributes: its deps.edn :paths (default [\"src\"])
  resolved under its root dir."
  [root]
  (let [edn (read-edn (str root "/deps.edn"))
        paths (or (:paths edn) ["src"])]
    (map #(abspath root %) paths)))

(defn- resolve-deps
  "Breadth-first walk of a deps map; returns {:roots [...] :natives [...]} — the
  ordered, de-duplicated source-root directories and the collected :jolt/native
  declarations from every dep's deps.edn. `base-dir` resolves :local/root and is
  replaced by a dep's own root as the walk descends."
  [deps base-dir]
  (loop [queue (mapv (fn [[c s]] [c s base-dir]) (seq deps))
         seen #{}
         roots []
         natives []]
    (if (empty? queue)
      {:roots (distinct roots) :natives natives}
      (let [[coord spec bd] (first queue)
            queue (subvec (vec queue) 1)]
        (if (contains? seen coord)
          (recur queue seen roots natives)
          (let [root (coord-root coord spec bd)]
            (if (nil? root)
              (recur queue (conj seen coord) roots natives)
              (let [edn (read-edn (str root "/deps.edn"))
                    child (mapv (fn [[c s]] [c s root]) (seq (:deps edn)))]
                (recur (into queue child)
                       (conj seen coord)
                       (into roots (dep-source-roots root))
                       (into natives (:jolt/native edn)))))))))))

;; --- public -----------------------------------------------------------------
(defn resolve-project
  "Resolve `project-dir`'s deps.edn with the selected alias keywords. Returns
  {:roots [...] :main-opts [...] :tasks {...} :natives [...]}; :main-opts is the
  last selected alias's, else nil; :natives are the project's + deps' :jolt/native
  shared-library declarations."
  ([project-dir] (resolve-project project-dir []))
  ([project-dir alias-kws]
   (let [edn (read-edn (str project-dir "/deps.edn"))
         aliases (:aliases edn)
         selected (keep #(get aliases %) alias-kws)
         extra-paths (mapcat :extra-paths selected)
         extra-deps (apply merge (map :extra-deps selected))
         main-opts (some :main-opts (reverse selected))
         project-paths (concat (or (:paths edn) ["src"]) extra-paths)
         project-roots (map #(abspath project-dir %) project-paths)
         all-deps (merge (:deps edn) extra-deps)
         {dep-roots :roots dep-natives :natives} (resolve-deps all-deps project-dir)]
     {:roots (vec (distinct (concat project-roots dep-roots)))
      :main-opts main-opts
      :tasks (:tasks edn)
      :natives (vec (distinct (concat (:jolt/native edn) dep-natives)))})))

(defn has-deps-edn? [project-dir]
  (file-exists? (str project-dir "/deps.edn")))
