(ns jolt.deps.min-version
  ":jolt/min-version: the oldest jolt a project or library declares it runs on,
  checked against the running runtime when a project resolves."
  (:require [clojure.string :as str]
            [jolt.deps.io :refer [getenv]]))

;; --- :jolt/min-version ------------------------------------------------------
;; A project or a library declares the oldest jolt it works on, and a runtime
;; below that refuses to load it rather than run it. Not every breaking change is
;; visible at the call site — ffi/write's argument order moved in 0.8.0, and the
;; old and new spellings are both integers, so an older runtime writes to the
;; wrong place and reports nothing — and a declared floor turns the NEXT break of
;; that shape into a message.
;;
;; Honoured by the jolt that READS the key, so it protects from 0.8.0 onward and
;; not before: an older jolt ignores it, as it ignores every key it does not
;; know. Note what that excludes — this key landed in the commit AFTER the one
;; that moved ffi/write, so no floor catches that particular break: every runtime
;; with the old argument order skips the key. Pinning the toolchain is the answer
;; there. The floor is for the breaks that come after a reader exists on both
;; sides, which is the reason to add it now rather than at the next one.
(defn version-parts
  "The leading numeric components of a version string, as a vector of longs.
  Tolerates a `v` prefix and reads each component's numeric PREFIX, stopping at
  the first component that has none — so \"v0.7.29-24-gabc\" is [0 7 29], the git
  describe suffix riding along on the last component, and \"0.8\" is [0 8]. A
  string with no numeric components at all (a source build's \"dev\") is []."
  [v]
  (let [v (str v)
        v (if (and (pos? (count v)) (= "v" (subs v 0 1))) (subs v 1) v)]
    (loop [parts (str/split v #"\.") acc []]
      (if (empty? parts)
        acc
        (let [digits (re-find #"^[0-9]+" (first parts))]
          (if digits
            (recur (rest parts) (conj acc (parse-long digits)))
            acc))))))

(defn version<
  "True when version a orders before version b on their numeric components, a
  missing component reading as 0 ([0 8] and [0 8 0] are the same version)."
  [a b]
  (let [x (version-parts a) y (version-parts b)
        n (max (count x) (count y))
        at (fn [v i] (or (get v i) 0))]
    (loop [i 0]
      (cond
        (= i n) false
        (< (at x i) (at y i)) true
        (> (at x i) (at y i)) false
        :else (recur (inc i))))))

;; The decision, pure and separate from the throw: a source build fails no floor,
;; so this is the only way the refusal is testable at all.
(defn min-version-failure
  "nil when every declared floor is met, else [message ex-data] for the HIGHEST
  unmet one — one message naming the newest thing needed, not one per dependency.
  `declared` is a seq of [who version], `who` being a lib symbol or nil for the
  project itself."
  [running declared]
  ;; A runtime naming no version — a source build answers "dev" — is never
  ;; refused: it reads as 0.0.0, the oldest possible, while in practice it is the
  ;; newest. The floor protects a released runtime, and those always name one.
  (when (seq (version-parts running))
    (let [unmet (filter (fn [[_ v]] (version< running v)) declared)]
      (when (seq unmet)
        ;; max-key would compare version VECTORS as numbers and throw the moment
        ;; two floors were unmet — and never compare at all with one, so the
        ;; crash hid behind every single-dependency project.
        (let [[who wanted] (reduce (fn [a b] (if (version< (second a) (second b)) b a))
                                   unmet)]
          [(str "this project needs jolt " wanted " or newer, and this is "
                running
                (when who (str " (required by " who ")"))
                ". Upgrade jolt, or set JOLT_SKIP_MIN_VERSION=1 to run anyway.")
           {:jolt/running running
            :jolt/required wanted
            :jolt/required-by who
            :jolt/unmet (vec unmet)}])))))

(defn check-min-versions!
  "Refuse to run when the project, or any dependency, declares a jolt floor the
  running runtime is below. JOLT_SKIP_MIN_VERSION runs anyway, for a runtime
  whose version string understates what it actually carries."
  [declared]
  (when-not (getenv "JOLT_SKIP_MIN_VERSION")
    (when-let [[msg data] (min-version-failure (jolt.host/jolt-version) declared)]
      (throw (ex-info msg data)))))
