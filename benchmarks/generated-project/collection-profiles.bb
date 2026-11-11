(ns generated-project.collection-profiles
  (:require [babashka.fs :as fs]
            [clojure.string :as str]))

(def sizes [8 9 32 33 1024 32768])

;; The generated hot loop binds base, k (a precomputed probe) and items
;; (prebuilt input for a bulk build). Each expression consumes its result.
(def operations
  (sorted-map
   "collections/control/probe" 'k
   "collections/map/get-hit" '(get base k 0)
   "collections/map/get-miss" '(get base (+ n k) 0)
   "collections/map/assoc-existing" '(get (assoc base k (+ k 2)) k)
   "collections/map/assoc-new" '(let [key (+ n k)] (get (assoc base key (+ k 2)) key))
   "collections/map/dissoc" '(get (dissoc base k) k 0)
   "collections/map/reduce-kv" '(reduce-kv (fn [acc _ v] (+ acc v)) k base)
   "collections/map/build-transient" '(get (persistent! (reduce conj! (transient {}) items)) k)
   "collections/vector/nth" '(nth base k)
   "collections/vector/assoc" '(nth (assoc base k (+ k 2)) k)
   "collections/vector/conj" '(peek (conj base k))
   "collections/vector/pop" '(get (pop base) k 0)
   "collections/vector/reduce" '(reduce + k base)
   "collections/vector/build-transient" '(nth (persistent! (reduce conj! (transient []) items)) k)
   "collections/set/contains-hit" '(if (contains? base k) 1 0)
   "collections/set/contains-miss" '(if (contains? base (+ n k)) 1 0)
   "collections/set/conj" '(let [key (+ n k)] (if (contains? (conj base key) key) 1 0))
   "collections/set/disj" '(if (contains? (disj base k) k) 1 0)
   "collections/set/reduce" '(reduce + k base)
   "collections/set/build-transient" '(if (contains? (persistent! (reduce conj! (transient #{}) items)) k) 1 0)))

(defn bulk? [op]
  (contains? #{"reduce" "reduce-kv" "build-transient"} (last (str/split op #"/"))))

(defn gcd [a b] (if (zero? b) a (recur b (mod a b))))

(defn schedule [seed n]
  (let [rng (java.util.Random. (bit-xor seed n))
        offset (.nextInt rng n)
        stride (first (filter #(= 1 (gcd % n)) (iterate inc (inc (.nextInt rng n)))))]
    {:offset offset :stride stride}))

(defn kernel-name [op] (symbol (str/replace op "/" "-")))

(defn kernel-source [op]
  (str "(defn " (kernel-name op) " [base items n ^longs probes calls]\n"
       "  (loop [i 0 total 0]\n"
       "    (if (< i calls)\n"
       "      (let [k (aget probes i)]\n"
       "        (recur (inc i) (+ total " (pr-str (operations op)) ")))\n"
       "      total)))\n"))

(defn generate! [kit root {:keys [seed categories collection-sizes]}]
  (let [profiles (vec (for [n collection-sizes :let [probe (schedule seed n)] op categories]
                        (assoc probe :op op :size n :bulk (bulk? op))))
        model {:format :collection-sizes-v1 :seed seed :profiles profiles}
        kernels (into (sorted-map) (map (juxt identity kernel-name) categories))]
    (when (empty? profiles) (throw (ex-info "No collection operations selected" {})))
    (fs/create-dirs (fs/path root "src/workload"))
    (spit (fs/file root "src/workload/collections.clj")
          (str "(ns workload.collections)\n\n" (str/join "\n" (map kernel-source categories))
               "\n" (pr-str (list 'def 'kernels kernels)) "\n"))
    (spit (fs/file root "manifest.edn") (str (pr-str model) "\n"))
    (spit (fs/file root "deps.edn") "{:paths [\"src\"]}\n")
    (doseq [[from to] [["collection-measure.clj" "measure.clj"] ["collection-oracle.bb" "oracle.bb"]
                       ["run.bb" "run.bb"] ["LICENSE" "LICENSE"]]]
      (fs/copy (fs/path kit from) (fs/path root to)))
    (spit (fs/file root "README.md")
          (str "# Generated collection workloads\n\nSeed " seed ", sizes " (str/join ", " collection-sizes)
               ". Each operation and size has its own checksum and timing.\n\n"
               "```sh\nbb run.bb . 42 32768 --warmup 10 --samples 7 --launches 3 --csv samples.csv\n```\n"
               "\nThe positional arguments are the initial probe offset and work budget per sample.\n"))
    model))
