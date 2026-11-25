(ns jolt.deps.declarations
  "What a resolution's libraries declare to the runtime, reconciled across the
  project and every dependency: :jolt/native shared libraries (one load per
  library, platform candidates folded together), :jolt/provides host-class
  providers (RFC 0014; a class claimed twice is refused), and
  :jolt/tree-shake {:allow-dynamic […]} entries."
  (:require [clojure.string :as str]))

;; The platform keys a :jolt/native spec declares its candidates under: exactly
;; the values main.clj's current-platform selects with, so the identity below
;; reads the same keys the loader does. It listed :win, which current-platform
;; never produces, so on Windows a spec that declared candidates only under
;; :windows keyed on nothing at all (jolt-ajd).
(def ^:private native-platform-keys [:darwin :linux :windows])

(defn- native-key
  "Identity of a :jolt/native spec. A :process lib (the running process's own
  symbols, e.g. libc) keys on that flag; a file lib on its :name, else on its
  platform candidate paths — two deps naming the same lib reconcile to one load.
  A spec with neither a :name nor a candidate under any platform key (it declares
  only :static, or only a key no platform selects) keys on its own shape, minus
  the declaring root: distinct specs stay distinct instead of collapsing to one
  under dedup-by, and two deps declaring the same lib still reconcile."
  [spec]
  (letfn [(cands [k] (let [v (get spec k)] (cond (string? v) [v] (sequential? v) (vec v) :else [])))]
    (if (:process spec)
      [:process (:name spec)]
      [:native (or (:name spec)
                   (let [paths (vec (sort (mapcat cands native-platform-keys)))]
                     (if (seq paths) paths (dissoc spec :jolt.deps/root :jolt.deps/lib
                                                   :jolt.deps/paths))))])))

;; Reconcile two declarations of the same library instead of dropping the second.
;; Specs arrive closest-first (the project's own, then each dependency before the
;; libraries IT depends on — jolt.deps orders them so), and the closer declaration
;; wins every key it declares while a farther one fills in the keys it is silent
;; about: an app that only wanted to ADD the :windows candidates a dependency is
;; missing used to have to restate that dependency's :darwin and :linux lists
;; verbatim (jolt-lang/jolt#989), and one that only pinned the :linux path of a
;; library lost the dependency's :static archive and :build task with it.
;;
;; A value that names a FILE resolves against the deps.edn that wrote it, so the
;; root travels with every path-valued key it is taken from: the platform
;; candidates and :static under :jolt.deps/roots, keyed like the spec. A :build
;; task builds the files its OWN deps.edn declares, so each one is kept under the
;; root it was declared in (:jolt.deps/builds {root task}): the task that runs for
;; a missing library is the one that produces the candidates this platform took,
;; never a dependency's task re-run for a path the app pinned somewhere else.
(def ^:private native-path-keys (conj native-platform-keys :static))

(defn- native-builds [spec]
  (or (:jolt.deps/builds spec)
      (when (contains? spec :build) {(:jolt.deps/root spec) (:build spec)})))

(defn- overlay-native [a b]
  (let [r (:jolt.deps/root b)
        acc (reduce-kv
             (fn [acc k v]
               (cond
                 (or (contains? acc k) (= "jolt.deps" (namespace k)) (= :build k)) acc
                 (and r (some #{k} native-path-keys))
                 (-> acc (assoc k v) (assoc-in [:jolt.deps/roots k] r))
                 :else (assoc acc k v)))
             a b)
        builds (merge (native-builds b) (native-builds a))]
    (if (seq builds) (assoc acc :jolt.deps/builds builds) acc)))

(defn- lazy-native? [spec] (boolean (and (seq (:jolt.deps/paths spec)) (not (:eager spec)))))

;; The source roots whose first require loads a reconciled spec. One shared
;; object declared by two dependencies is loaded by whichever is required first
;; — the union of their roots — and a declaration that has to be there at
;; startup (the project's own, or :eager) makes the whole spec eager.
(defn- overlay-native-roots [a b]
  (cond
    ;; a project's local overrides (deps.local.edn) say WHERE a library is,
    ;; not WHEN it loads: overriding a dependency's library keeps it loading on
    ;; that dependency's first require, rather than making it the project's own
    ;; and opening it at startup.
    ;; two local specs for one library are still one override, of nothing yet
    (and (:jolt.deps/local a) (:jolt.deps/local b))
    a

    (:jolt.deps/local a)
    (cond-> (dissoc a :jolt.deps/local :jolt.deps/paths)
      (lazy-native? b) (assoc :jolt.deps/paths (:jolt.deps/paths b))
      (:jolt.deps/lib b) (assoc :jolt.deps/lib (:jolt.deps/lib b)))

    (and (lazy-native? a) (lazy-native? b))
    (assoc a :jolt.deps/paths (vec (distinct (concat (:jolt.deps/paths a)
                                                     (:jolt.deps/paths b)))))
    :else (dissoc a :jolt.deps/paths)))

(defn reconcile-natives
  "dedup-by native-key, but folding each dropped spec's unclaimed platform keys
  into the one that kept the identity. First-inclusion order is preserved."
  [specs]
  (let [[order by-key]
        (reduce (fn [[order by-key] spec]
                  (let [k (native-key spec)]
                    (if (contains? by-key k)
                      [order (assoc by-key k (overlay-native-roots
                                              (overlay-native (get by-key k) spec)
                                              spec))]
                      [(conj order k) (assoc by-key k spec)])))
                [[] {}] specs)]
    (mapv by-key order)))

(defn provides-entries
  "A deps.edn :jolt/provides map as provider-table rows: [install-ns lib class ...].
  lib is the declaring dependency's coordinate, or nil for the project's own."
  [edn lib]
  ;; Both names are stringified here: the coordinate is a symbol in deps.edn and
  ;; the runtime string-appends it into the "declared but failed to load"
  ;; message. nil stays nil — it means jolt's own, which has no coordinate.
  (map (fn [[install-ns classes]]
         (into [(str install-ns) (when lib (str lib))] (map str classes)))
       (:jolt/provides edn)))

(defn allow-dynamic-entries
  "The defs a deps.edn vouches never resolve vars at runtime in a built binary
  — :jolt/tree-shake {:allow-dynamic [ns/name …]} — as \"ns/name\" strings, the
  shape the build driver's bail scan (dce.ss) keys on. Symbols are the
  documented form; a string is taken as written. Nothing else is validated
  here: an entry that names no def is inert, and the bail diagnostic prints the
  exact key for the sites that remain, so a misspelling corrects itself on the
  next build rather than needing a checker of its own."
  [edn]
  (mapv str (:allow-dynamic (:jolt/tree-shake edn))))

(declare drop-overridden-claims host-class-providers*)

(defn host-class-providers
  "Reconcile the :jolt/provides declarations (RFC 0014) into the provider table
  the runtime autoloads from, as [install-ns lib class-name ...].

  Two dependencies claiming the same host class is refused rather than resolved.
  Whichever won would decide what `java.sql.Connection` MEANS for the whole
  program, and the loser's shim would be half-installed — registered for the
  classes nobody else claimed and silently absent for the rest. That is the
  shape of bug this table exists to stop being possible, so it is an error at
  resolve time, where both claimants can be named.

  Entries are [install-ns lib class ...]; lib is nil for the project's own.

  With OVERRIDES? — (overrides? lib-a lib-b), true when lib-a's declaration is
  closer to the project than lib-b's, the project (nil) being closest to every
  dependency and a library closer than the ones it depends on — a claim is not a
  conflict with a farther one: the closer claim takes the class and the farther
  row drops it. That is the same precedence every other declaration resolves
  with, and it is how an app swaps a library's shim for its own. Two claimants
  neither of which is closer (two unrelated libraries) are still refused."
  ([entries] (host-class-providers entries nil))
  ([entries overrides?]
   (host-class-providers* (if overrides? (drop-overridden-claims entries overrides?) entries))))

(defn- drop-overridden-claims [entries overrides?]
  (let [owners (reduce (fn [acc [install-ns lib & classes]]
                         (reduce #(update %1 %2 (fnil conj #{}) [install-ns lib]) acc classes))
                       {} entries)
        lost? (fn [install-ns lib c]
                (some (fn [[ns2 lib2]] (and (not= ns2 install-ns) (overrides? lib2 lib)))
                      (get owners c)))]
    (vec (keep (fn [[install-ns lib & classes]]
                 (when-let [cs (seq (remove #(lost? install-ns lib %) classes))]
                   (into [install-ns lib] cs)))
               entries))))

(defn- host-class-providers*
  [entries]
  (let [claims (reduce (fn [acc [install-ns lib & classes]]
                         (reduce (fn [a c] (update a c (fnil conj #{}) [install-ns lib]))
                                 acc classes))
                       {} entries)
        conflicts (filter (fn [[_ owners]] (< 1 (count (set (map first owners)))))
                          claims)]
    (when (seq conflicts)
      (let [[c owners] (first (sort-by key conflicts))
            who (fn [[install-ns lib]] (str (or lib "this project") " (" install-ns ")"))]
        (throw (ex-info
                (str "two dependencies both claim to provide the host class " c
                     ": " (str/join " and " (sort (map who owners)))
                     ". A host class can have one provider; drop one of them.")
                {:jolt/class c
                 :jolt/claimed-by (vec (sort (map who owners)))}))))
    (vec (distinct entries))))

;; --- precedence ---------------------------------------------------------------
;; jolt.deps orders the selected libraries with these so every first-wins
;; reconciliation above reads closest-first.

(defn dependents-first
  "ORDER re-ordered so each library precedes every library it depends on, the
  original order breaking ties. A dependency cycle is broken at its earliest
  member rather than dropped."
  [order children]
  (let [parents (reduce-kv (fn [acc p cs] (reduce #(update %1 %2 (fnil conj #{}) p) acc cs))
                           {} children)]
    (loop [remaining (vec order) out []]
      (if (empty? remaining)
        out
        (let [left (set remaining)
              ready? #(not-any? left (get parents %))
              pick (or (first (filter ready? remaining)) (first remaining))]
          (recur (filterv #(not= pick %) remaining) (conj out pick)))))))

(defn descendants-by-lib
  "\"lib\" -> #{\"lib\" …} every library it transitively depends on, as the
  strings :jolt/provides rows carry."
  [children]
  (letfn [(walk [seen lib]
            (reduce (fn [seen c] (if (seen c) seen (walk (conj seen c) c)))
                    seen (get children lib)))]
    (into {} (for [lib (keys children)
                   :let [ds (disj (walk #{} lib) lib)]
                   :when (seq ds)]
               [(str lib) (into #{} (map str) ds)]))))
