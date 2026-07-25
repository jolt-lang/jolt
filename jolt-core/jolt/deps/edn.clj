;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns jolt.deps.edn
  "deps.edn manipulation: merging deps.edn maps and combining alias maps.

  The merge and alias-combination functions are taken directly from
  clojure.tools.deps.edn (org.clojure/tools.deps.edn) so jolt's alias
  semantics match the reference tools.deps implementation key for key —
  :extra-deps/:override-deps/:default-deps/:replace-deps merge as maps,
  :extra-paths/:replace-paths append uniquely, :main-opts/:exec-fn are
  last-wins. Reading/validation stay in jolt.deps (jolt has its own edn
  reader; spec validation needs spec.alpha, not bundled)."
  (:require [clojure.string :as str]
            [clojure.walk :as walk]))

;;;; Canonicalize

(defn- printerrln
  [& msgs]
  (binding [*out* *err*]
    (println (str/join " " msgs))))

(defn- canonicalize-sym
  [s & opts]
  (if (simple-symbol? s)
    (let [cs (as-> (name s) n (symbol n n))]
      (printerrln "DEPRECATED: Libs must be qualified, change" s "=>" cs
        (if-let [path (:path opts)] (str "(" path ")") ""))
      cs)
    s))

(defn- canonicalize-exclusions
  [{:keys [exclusions] :as coord} & opts]
  (if (seq (filter simple-symbol? exclusions))
    (assoc coord :exclusions (mapv #(canonicalize-sym % opts) exclusions))
    coord))

(defn- canonicalize-dep-map
  [deps-map & opts]
  (when deps-map
    (reduce-kv (fn [acc lib coord]
                 (let [new-lib (if (simple-symbol? lib) (canonicalize-sym lib opts) lib)
                       new-coord (canonicalize-exclusions coord opts)]
                   (assoc acc new-lib new-coord)))
      {} deps-map)))

(defn canonicalize
  "Canonicalize a deps.edn map (convert simple lib symbols to qualified lib symbols).
  Returns the deps-edn map.

  Opts:
    :path String path to file being read"
  [deps-edn & opts]
  (walk/postwalk
    (fn [x]
      (if (map? x)
        (reduce (fn [xr k]
                  (if-let [xm (get xr k)]
                    (assoc xr k (canonicalize-dep-map xm opts))
                    xr))
          x #{:deps :default-deps :override-deps :extra-deps :classpath-overrides})
        x))
    deps-edn))

;;;; deps edn manipulation

(defn- merge-or-replace
  "If maps, merge, otherwise replace"
  [& vals]
  (when (some identity vals)
    (reduce (fn [ret val]
              (if (and (map? ret) (map? val))
                (merge ret val)
                (or val ret)))
      nil vals)))

(defn merge-edns
  "Merge multiple deps edn maps from left to right into a single deps edn map."
  [deps-edn-maps]
  (apply merge-with merge-or-replace (remove nil? deps-edn-maps)))

;;;; Aliases

;; per-key binary merge-with rules

(def ^:private last-wins (comp last #(remove nil? %) vector))
(def ^:private append (comp vec concat))
(def ^:private append-unique (comp vec distinct concat))

(def ^:private merge-alias-rules
  {:deps merge ;; FUTURE: remove
   :replace-deps merge ;; formerly :deps
   :extra-deps merge
   :override-deps merge
   :default-deps merge
   :classpath-overrides merge
   :paths append-unique ;; FUTURE: remove
   :replace-paths append-unique ;; formerly :paths
   :extra-paths append-unique
   :jvm-opts append
   :main-opts last-wins
   :exec-fn last-wins
   :exec-args merge-or-replace
   :ns-aliases merge
   :ns-default last-wins})

(defn- choose-rule [alias-key val]
  (or (merge-alias-rules alias-key)
    (if (map? val)
      merge
      (fn [_v1 v2] v2))))

(defn merge-alias-maps
  "Like merge-with, but using custom per-alias-key merge function"
  [& ms]
  (reduce
    #(reduce
       (fn [m [k v]] (update m k (choose-rule k v) v))
       %1 %2)
    {} ms))

(defn combine-aliases
  "Find, read, and combine alias maps identified by alias keywords from
  a deps edn map into a single args map."
  [edn-map alias-kws]
  (->> alias-kws
    (map #(get-in edn-map [:aliases %]))
    (apply merge-alias-maps)))
