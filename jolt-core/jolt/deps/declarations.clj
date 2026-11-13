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
;; dedup-by is winner-takes-all, and the project's own specs come first — so an
;; app that only wanted to ADD the :windows candidates a dependency is missing
;; had to restate that dependency's :darwin and :linux lists verbatim, or break
;; those platforms instead (jolt-lang/jolt#989). The winner still wins every key
;; it declares; a later spec only fills in the platform keys the winner is
;; silent about, and the root those filled-in candidates resolve against travels
;; with them, so a dependency's relative "native/libfoo.so" still resolves
;; against that dependency rather than against whoever won the identity.
(defn- overlay-native [a b]
  (reduce (fn [acc k]
            (if (or (contains? acc k) (not (contains? b k)))
              acc
              (let [acc (assoc acc k (get b k))]
                (if-let [r (:jolt.deps/root b)]
                  (assoc acc :jolt.deps/roots (assoc (:jolt.deps/roots acc) k r))
                  acc))))
          a
          native-platform-keys))

(defn- lazy-native? [spec] (boolean (and (seq (:jolt.deps/paths spec)) (not (:eager spec)))))

;; The source roots whose first require loads a reconciled spec. One shared
;; object declared by two dependencies is loaded by whichever is required first
;; — the union of their roots — and a declaration that has to be there at
;; startup (the project's own, or :eager) makes the whole spec eager.
(defn- overlay-native-roots [a b]
  (if (and (lazy-native? a) (lazy-native? b))
    (assoc a :jolt.deps/paths (vec (distinct (concat (:jolt.deps/paths a)
                                                     (:jolt.deps/paths b)))))
    (dissoc a :jolt.deps/paths)))

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

(defn host-class-providers
  "Reconcile the :jolt/provides declarations (RFC 0014) into the provider table
  the runtime autoloads from, as [install-ns lib class-name ...].

  Two dependencies claiming the same host class is refused rather than resolved.
  Whichever won would decide what `java.sql.Connection` MEANS for the whole
  program, and the loser's shim would be half-installed — registered for the
  classes nobody else claimed and silently absent for the rest. That is the
  shape of bug this table exists to stop being possible, so it is an error at
  resolve time, where both claimants can be named.

  Entries are [install-ns lib class ...]; lib is nil for the project's own."
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
