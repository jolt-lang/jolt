;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns jolt.deps.ext
  "The coordinate-type SPI for dependency expansion, following
  clojure.tools.deps.extensions: a coordinate type registers its identifying
  keys (coord-type-keys) and implements dep-id / coord-deps / coord-root /
  compare-versions. jolt.deps registers :mvn / :git / :local; a test harness
  can register a fake type against the same expansion engine, exactly like
  tools.deps' faken extension.

  Also carries a Maven version comparator (the ComparableVersion ordering
  tools.deps gets from maven-resolver) so :mvn/version conflicts resolve
  newest-wins without the JVM."
  (:require [clojure.set :as set]
            [clojure.string :as str]))

;;;; Methods switching on coordinate type — lifted from
;;;; clojure.tools.deps.extensions (multimethod table read via `methods`
;;;; instead of the JVM .getMethodTable).

(defmulti coord-type-keys
  "Takes a coordinate type and returns valid set of keys indicating that coord type"
  (fn [type] type))

(defmethod coord-type-keys :default [type] #{})

(defn procurer-types
  "Returns set of registered procurer types (results may change if procurer methods are registered)."
  []
  (disj (-> (methods coord-type-keys) keys set) :default))

(defn coord-type
  "Determine the coordinate type of the coordinate, based on the self-published procurer
  keys from coord-type-keys."
  [coord]
  (when (map? coord)
    (let [exts (procurer-types)
          coord-keys (-> coord keys set)
          matches (reduce (fn [ms type]
                            (cond-> ms
                              (seq (set/intersection (coord-type-keys type) coord-keys))
                              (conj type)))
                    [] exts)]
      (case (count matches)
        0 (throw (ex-info (str "Coord of unknown type: " (pr-str coord)) {:coord coord}))
        1 (first matches)
        (throw (ex-info (str "Coord type is ambiguous: " (pr-str coord)) {:coord coord}))))))

(defn- throw-bad-coord
  [lib coord]
  (if (map? coord)
    (throw (ex-info (str "No coordinate type found for library " lib " in coordinate " (pr-str coord))
                    {:lib lib :coord coord}))
    (throw (ex-info (str "Bad coordinate for library " lib ", expected map: " (pr-str coord))
                    {:lib lib :coord coord}))))

(defmulti dep-id
  "Returns an identifier value that can be used to detect a lib/coord cycle while
  expanding deps."
  (fn [lib coord] (coord-type coord)))

(defmethod dep-id :default [lib coord] (throw-bad-coord lib coord))

(defmulti coord-deps
  "Returns the child dependencies of a procured coordinate as a seq of
  [lib coord] entries. Procures (clone/fetch/extract) as a side effect;
  jolt.deps memoizes per [lib dep-id]."
  (fn [lib coord] (coord-type coord)))

(defmethod coord-deps :default [lib coord] (throw-bad-coord lib coord))

(defmulti coord-info
  "Returns procurement info for a coordinate: {:root dir-or-nil, :manifest kw,
  :natives [...], :prep coords-with-:deps/prep-lib}. :root nil means the
  coordinate contributes nothing (an intrinsic or sourceless dep)."
  (fn [lib coord] (coord-type coord)))

(defmethod coord-info :default [lib coord] (throw-bad-coord lib coord))

(defmulti compare-versions
  "Given two coordinates for the same library, return a comparator value
  (negative, zero, positive) ordering them by version. Throws when no ordering
  exists between the coordinate types/values."
  (fn [lib coord-x coord-y] [(coord-type coord-x) (coord-type coord-y)]))

(defmethod compare-versions :default
  [lib coord-x coord-y]
  (throw (ex-info (str "Unable to compare versions for " lib ": "
                       (pr-str coord-x) " and " (pr-str coord-y))
                  {:lib lib :coord-x coord-x :coord-y coord-y})))

;;;; Maven version ordering (org.apache.maven.artifact.versioning
;;;; ComparableVersion semantics: dot/dash tokenization, digit<->letter
;;;; transitions split, numeric items compare numerically, known qualifiers
;;;; order below release, null-padding).

(def ^:private qualifier-order
  ;; ComparableVersion QUALIFIERS: alpha < beta < milestone < rc < snapshot
  ;; < '' (release) < sp; an unlisted qualifier sorts after sp, lexically.
  {"alpha" 0 "beta" 1 "milestone" 2 "rc" 3 "cr" 3 "snapshot" 4
   "" 5 "final" 5 "ga" 5 "release" 5 "sp" 6})

(defn- expand-alias
  ;; a/b/m followed by a digit are shorthand qualifiers (1.0a1 = 1.0-alpha-1)
  [s]
  (case s "a" "alpha" "b" "beta" "m" "milestone" s))

(defn- tokenize-version
  "Split a version string into items: longs for numeric runs, lowercase strings
  for qualifier runs. Dots, dashes, and digit<->letter transitions separate."
  [s]
  (let [s (str/lower-case s)
        n (count s)
        release-item? (fn [it] (or (and (number? it) (zero? it))
                                   (contains? #{"" "final" "ga" "release"} it)))
        trim-trailing (fn [items]
                        ;; ComparableVersion normalization: trailing zeros and
                        ;; release-equivalent qualifiers drop, so 1.0 = 1.0.0 =
                        ;; 1.0-ga = 1.0.0-release.
                        (loop [v items]
                          (if (and (seq v) (release-item? (peek v)))
                            (recur (pop v))
                            v)))]
    (loop [i 0 start 0 items [] prev nil]
      (if (>= i n)
        (let [items (if (> i start) (conj items (subs s start i)) items)]
          (trim-trailing
            (mapv (fn [it]
                    (if (re-matches #"[0-9]+" it)
                      (parse-long it)
                      (expand-alias it)))
                  items)))
        (let [c (get s i)
              kind (cond (Character/isDigit c) :digit
                         (or (= c \.) (= c \-)) :sep
                         :else :letter)]
          (cond
            (= kind :sep)
            (recur (inc i) (inc i)
                   (if (> i start) (conj items (subs s start i)) items)
                   nil)
            (and prev (not= kind prev))
            (recur (inc i) i (conj items (subs s start i)) kind)
            :else
            (recur (inc i) start items kind)))))))

(defn- item-compare
  [a b]
  (cond
    (and (nil? a) (nil? b)) 0
    ;; null padding: a number pads as 0; a qualifier compares against release
    (nil? a) (- (item-compare b nil))
    (nil? b) (if (number? a)
               (if (zero? a) 0 1)
               (compare (get qualifier-order a 7) (get qualifier-order "" 5)))
    (and (number? a) (number? b)) (compare a b)
    ;; a number is newer than any qualifier (1.0.1 > 1.0-rc)
    (number? a) 1
    (number? b) -1
    :else (let [qa (get qualifier-order a) qb (get qualifier-order b)]
            (cond
              (and qa qb) (compare qa qb)
              qa (compare qa 7)
              qb (compare 7 qb)
              :else (compare a b)))))

(defn compare-mvn-versions
  "Compare two Maven version strings by ComparableVersion ordering."
  [x y]
  (let [xs (tokenize-version x) ys (tokenize-version y)
        n (max (count xs) (count ys))]
    (loop [i 0]
      (if (>= i n)
        0
        (let [c (item-compare (get xs i) (get ys i))]
          (if (zero? c) (recur (inc i)) c))))))
