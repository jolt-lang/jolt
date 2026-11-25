;; Compare the C two checkouts emit for the same native regions (make
;; nativeemitgate). Each dump directory holds <key>.c and <key>.edn per region,
;; written by test/chez/native-test.clj under NATIVE_EMIT_DUMP; the key hashes
;; the region's source, so a key names the same program on both sides.
;;
;;   bin/jolt run tools/native-emit-diff.clj BASE-DIR CANDIDATE-DIR [ALLOW]
;;
;; A region whose C differs fails the run unless ALLOW, a regex, matches its
;; source; the allowed ones are listed. Regions only one side compiled (a row
;; the change adds, or one the other checkout cannot compile) are listed and do
;; not fail it. The idea is scriptc's zero-cost gate (scripts/zero-cost-gate.mjs).
(ns native-emit-diff
  (:require [clojure.string :as str]))

(defn- dump [dir]
  (let [fs (.listFiles (java.io.File. ^String dir))]
    (when-not fs
      (println "native-emit-diff: no dump at" dir)
      (System/exit 2))
    (into (sorted-map)
          (for [^java.io.File f fs
                :let [n (.getName f)]
                :when (str/ends-with? n ".c")
                :let [k (subs n 0 (- (count n) 2))]]
            [k {:c (slurp f) :src (slurp (str dir "/" k ".edn"))}]))))

(defn- first-difference [a b]
  (let [as (str/split-lines a) bs (str/split-lines b)
        i (or (first (keep-indexed (fn [i [x y]] (when (not= x y) i)) (map vector as bs)))
              (min (count as) (count bs)))]
    (str "line " (inc i) ":\n    base:      " (get as i "<end>") "\n    candidate: " (get bs i "<end>"))))

(defn- short-src [s] (let [s (str/trim s)] (if (> (count s) 160) (str (subs s 0 157) "...") s)))

(let [[base-dir cand-dir allow] *command-line-args*
      allow (when-not (str/blank? allow) (re-pattern allow))
      base (dump base-dir)
      cand (dump cand-dir)
      both (filter #(contains? base %) (keys cand))
      differs (remove #(= (:c (base %)) (:c (cand %))) both)
      {allowed true failing false} (group-by #(boolean (and allow (re-find allow (:src (cand %))))) differs)
      added (remove #(contains? base %) (keys cand))
      removed (remove #(contains? cand %) (keys base))]
  (println "native-emit-diff:" (count both) "regions on both sides,"
           (- (count both) (count differs)) "identical," (count allowed) "allowed,"
           (count failing) "differ;" (count added) "only in the candidate," (count removed) "only in the base")
  (doseq [k allowed] (println "  allowed" k (short-src (:src (cand k)))))
  (doseq [k added] (println "  only in the candidate" k (short-src (:src (cand k)))))
  (doseq [k removed] (println "  only in the base" k (short-src (:src (base k)))))
  (doseq [k failing]
    (println "  DIFFERS" k (short-src (:src (cand k))))
    (println "   " (first-difference (:c (base k)) (:c (cand k)))))
  (System/exit (if (seq failing) 1 0)))
