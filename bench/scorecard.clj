;; scorecard.clj — render bench/README.md from bench/README.tmpl and the output
;; of one benchmark sitting: `bench/run.sh` (the AOT rows and `startup`) and
;; `bench/testcheck.sh` (the run-mode rows). A Selmer template, so the prose
;; lives in the template and this script only supplies the rows.
;;
;;   JOLT_BIN=target/release/jolt bench/run.sh > run.log
;;   JOLT_BIN=target/release/jolt bench/testcheck.sh > tc.log
;;   jolt run bench/scorecard.clj run.log tc.log
;;   jolt run bench/scorecard.clj run.log tc.log --print   # stdout only
;;
;; One same-sitting run is the contract — both logs from one machine in one
;; sitting, or the ratios mean nothing — so this refuses a partial one: every
;; bench in `run.sh --list` (plus `startup`) must have a row in the logs and a
;; description below, and a `jolt build FAILED` line anywhere stops it. Rows are
;; sorted by ratio (jolt ÷ JVM, two decimals under 1), AOT first, then
;; `startup`, then the run-mode rows in the harness's order.
;;
;; Every figure in the README's prose comes from the same logs, never from the
;; template: the "Measured …" line from run.sh's `env` lines, the noise from each
;; row's `runs:`, the plain-build gap from its `release:` lines (so the sitting
;; must be a MODE_A=1 run), the gc-arrays and parallel-colls readings from what
;; those benches print above `mean:`, and the V8 reference from the cst-format
;; row's node lines. A log missing any of them is refused like a missing row.
;;
;; Selmer needs java.time, which jolt-lang/time provides; requiring jolt.time
;; ahead of it installs those classes. The first run needs ~/.m2 (or network).
(require 'jolt.deps)
(jolt.deps/add-deps '{:deps {io.github.jolt-lang/time {:git/tag "v0.0.8" :git/sha "33c5e9831c046cfcbe68bc0c9c33e92568eaebb1"}
                             selmer/selmer {:mvn/version "1.13.5"}}})
(require 'jolt.time)
(require '[selmer.parser :as selmer]
         '[selmer.util :as selmer-util]
         '[clojure.string :as str])

;; What each row measures — the table's last column. A bench without an entry
;; here is refused, so adding one to run.sh means saying what it is for.
(def descriptions
  {"fib" "recursion: call overhead + integer arith"
   "tak" "deep three-way self-recursion + integer arith"
   "loop-recur" "tight `loop`/`recur` with `mod`/`quot`/`bit-xor` per iteration"
   "mandelbrot" "pure float compute, no allocation or dispatch"
   "arrays" "primitive `double-array` throughput (hinted `aget`/`aset`)"
   "arrays-unhinted" "the same array code without type hints"
   "byte-arrays" "raw bytes in bulk: block copies, a drained stream, `String`↔`byte[]`, hinted `^bytes` access"
   "gc-arrays" "major-collection pause with a large typed array live (read jolt ms only, see below)"
   "mathfns" "`java.lang.Math` sqrt/sin/cos/log/pow/atan2 over doubles"
   "mathfns-unhinted" "the same math without type hints"
   "collections" "persistent map/vector churn + map/filter/take/reduce over the result"
   "vecops" "vector concat (`into`), `subvec` windows, split/rejoin (the RRB axis)"
   "seqs" "lazy-seq + HOF pipelines: `map`/`filter`/`reduce`, `every?`, `iterate`/`take`, `mapcat`"
   "lazy-threads" "lazy pipelines after a `Thread` has existed (cells claimed by CAS, no mutex per cell)"
   "apply-rest" "`apply` of `+ max min < <=` and a user variadic over a million-element rest (streamed, not materialized)"
   "sorted-access" "shape-answered reads: `count`/`drop` on a vector seq, `rseq`, `first` of a sorted map/set"
   "coll-shapes" "shape-answered ops on a big collection: `into` a small step, queue boundary `pop`, `count` of a list/range, first of a hash/sorted map, sorted `hash`/`=`, `nthrest`, a 64-way string `case`"
   "sorted-build" "`into` a sorted-map/sorted-set in and out of key order, `sorted-map-by`, replace-every-key (one tree walk per insert)"
   "nth-access" "`nth` on a vector, small and large, with and without a default"
   "transducers" "transducer pipelines (`comp` of `map`/`filter`/`take`)"
   "transients" "bulk map/set building through `into`, `assoc!`/`conj!`, `zipmap`/`frequencies`/`group-by`"
   "keyed-lookup" "hashing keywords/symbols/strings and looking them up in small maps"
   "hash-eq" "hashing vectors/maps/sets/records/seqs, collection-keyed lookups, `=` on equal and unequal collections"
   "literals" "constant map/vector/set literals and quoted forms in a fn body, boolean predicates (per-form constant pool)"
   "string-build" "`StringBuilder` in a loop and transducer-over-`join`"
   "string-ops" "`.indexOf`/`.startsWith`/`.substring`/`.toLowerCase` on hinted strings, `clojure.string`, keyword `.getName`"
   "string-ops-unhinted" "the same string interop without type hints"
   "char-scan" "`.charAt` per code point with the `int`/`long`/`unchecked-*` casts, a `case` state machine"
   "char-scan-unhinted" "the same scan without type hints"
   "printing" "`pr-str` over scalars and namespaced maps, `print` into a rebound `*out*`, `format` with numeric directives and flags"
   "mono-dispatch" "monomorphic protocol dispatch (devirt / inline cache can fire)"
   "dispatch" "megamorphic protocol dispatch"
   "binary-trees" "escaping short-lived records: allocation / GC pressure"
   "typed-records" "records with `^double`/`^long`/`^String` field types at construction and every read"
   "typed-records-unhinted" "the same records without field types"
   "stm" "ref creation, `dosync` `ref-set`/`alter`, `deref` in a loop"
   "executors" "`java.util.concurrent`: fire-and-forget enqueue, submit/get, growth to 64 blocking tasks, four producers on one pool"
   "compile-forms" "**compiling**, not running: `load-string` of 200 top-level defns and of one `deftest` holding 200 `is` forms"
   "host-io" "reading THROUGH the `java.io` shim: a form off a reader, a chunked `char[]` drain, `String`↔`char[]`"
   "string-scan" "`clojure.string` over a large payload: split/replace/trim, and whether a literal pattern reaches the regex engine"
   "cst-format" "the source-formatter shape: a CST of one 10-key map per token, then an atom-per-node mutating walk building the output with `str`"
   "coll-dispatch" "kind dispatch on small collections: `get`/`assoc` on a 4-key map, a first/next walk, `count`/`conj`/`nth`/`=` on short lists and vectors"
   "metadata" "`with-meta`/`meta`/`vary-meta`, ops that carry meta (`assoc`/`conj`/`into` on a meta-bearing coll), a positioned form tree rebuilt with meta kept"
   "parallel-colls" "eight threads, each on its own values: `assoc`/`conj`/`swap!`/`str`/`hash`/`re-find`/`with-meta` (what the runtime shares behind their backs)"
   "atom-ops" "atom paths per event: counter `swap!`, `deref` of a small map, `swap! assoc` into and `get` from a 1k-key map, a deref read whole"
   "watched-swap" "a disk-backed atom: every `swap!` fires a watch that `spit`s the `pr-str` of the new state"
   "pool-tasks" "64 small tasks fanned out and joined, onto a warm fixed pool of 8 and through `future`"
   "sorted-set-build" "sorted sets built by `into` (scrambled longs, strings), `conj` in order, and `disj` of half"
   "int-map-build" "long-keyed hash maps built by `into {}`, `assoc`, transient `assoc!` and `zipmap`, then a `get` of every key"
   "startup" "a built hello-world, whole process from exec to exit, best of {{startup-reps}} (JVM: `java -cp … clojure.main -m hello`)"})

(def run-mode-descriptions
  {"mix-64" "SplitMix `mix-64`: 64-bit integer arithmetic (heap bignums past the 61-bit fixnum)"
   "deftype+protocol" "open-world deftype allocation + protocol dispatch"
   "split + rand-long" "the PRNG: bignum 64-bit arithmetic + dispatch"
   "gen/large-integer" "`gen/large-integer`: arithmetic + rose-tree generator machinery"
   "(gen/vector gen/large-integer)" "element generation + generator machinery"})

(defn- die [& msg]
  (binding [*out* *err*] (println (apply str "scorecard.clj: " msg)))
  (System/exit 1))

(defn- ratio-str [j v]
  (let [x (/ (Double/parseDouble j) (Double/parseDouble v))]
    (if (< x 1.0) (format "%.2f" x) (format "%.1f" x))))

(def ^:private row-re #"^(\S+)\s+jolt\s+([\d.]+) ms\s+jvm\s+([\d.]+) ms\s+\S+$")

(defn- parse-aot
  "The run.sh rows, each with the indented detail lines under it: :release (the
  MODE_A mean) and :detail, host -> the lines that host's bench printed."
  [text]
  (->> (str/split-lines text)
       (reduce (fn [rows line]
                 (if-let [[_ name j v] (re-find row-re line)]
                   (conj rows {:name name :jolt j :jvm v :detail {}
                               :sort (/ (Double/parseDouble j) (Double/parseDouble v))})
                   (if-let [row (peek rows)]
                     (if-let [[_ ms] (re-find #"^  release: ([\d.]+) ms$" line)]
                       (conj (pop rows) (assoc row :release (Double/parseDouble ms)))
                       (if-let [[_ host l] (re-find #"^  (jolt|jvm|node): (.*)$" line)]
                         (conj (pop rows) (update-in row [:detail host] (fnil conj []) l))
                         rows))
                     rows)))
               [])))

(defn- parse-env [text]
  (into {} (for [[_ k v] (re-seq #"(?m)^env ([\w-]+): ?(.*)$" text)] [k (str/trim v)])))

(defn- detail-line
  "The first line HOST's bench printed under ROW that matches RE; its groups."
  [row host re]
  (some #(re-find re %) (get-in row [:detail host])))

(defn- runs-of [row host]
  (when-let [[_ xs] (detail-line row host #"^runs: \[(.*)\]$")]
    (mapv #(Double/parseDouble %) (re-seq #"[\d.]+" xs))))

(defn- spread [xs]
  (let [lo (apply min xs)] (when (pos? lo) (/ (apply max xs) lo))))

(defn- x2 [x] (format "%.2f" (double x)))
(defn- x1 [x] (let [x (double x)] (cond (< x 1.0) (format "%.2f" x) (< x 100.0) (format "%.1f" x) :else (format "%.0f" x))))

(defn- median [xs]
  (let [v (vec (sort xs)) n (count v)]
    (if (odd? n) (v (quot n 2)) (/ (+ (v (dec (quot n 2))) (v (quot n 2))) 2.0))))

(defn- derived
  "The prose figures, all computed from this sitting's rows. Dies on anything
  missing, so the README can never render a figure the logs did not produce."
  [aot env dir]
  (let [by-name (into {} (map (juxt :name identity)) aot)
        need (fn [what x] (or x (die "the logs have no " what)))
        timed (remove #(= "startup" (:name %)) aot)
        runs (for [r timed] [(:name r) (need (str "jolt runs: for " (:name r)) (runs-of r "jolt"))])
        run-counts (set (map (comp count second) runs))
        spreads (sort-by second > (keep (fn [[n xs]] (when-let [s (spread xs)] [n s])) runs))
        release (for [r timed]
                  (let [v (Double/parseDouble (:jvm r))
                        rel (need (str "release: mean for " (:name r) " (run the sitting with MODE_A=1)") (:release r))]
                    [(:name r) (Math/abs (- (/ rel v) (/ (Double/parseDouble (:jolt r)) v)))]))
        [gap-name gap] (apply max-key second release)
        floor (fn [host] (Double/parseDouble
                          (second (need (str "gc-arrays per-collect floor from " host)
                                        (detail-line (by-name "gc-arrays") host #"^per-collect us: floor ([\d.]+)")))))
        slowdown (fn [host] (Double/parseDouble
                             (second (need (str "parallel-colls slowdown from " host)
                                           (detail-line (by-name "parallel-colls") host #"per-thread slowdown at \d+ threads: ([\d.]+)x")))))
        cst (by-name "cst-format")
        verify (fn [host] (second (need (str "cst-format verify: from " host) (detail-line cst host #"verify: (.*)$"))))
        node-ms (Double/parseDouble (second (need "cst-format node reference (is node on PATH?)"
                                                  (detail-line cst "node" #"^mean: ([\d.]+) ms"))))
        chars (Long/parseLong (second (detail-line cst "jolt" #"^payload chars: (\d+)")))
        cst-jolt (Double/parseDouble (:jolt cst))
        cst-jvm (Double/parseDouble (:jvm cst))
        run-src (slurp (java.io.File. dir "run.sh"))
        gate-src (slurp (java.io.File. (.getParentFile dir) "ci/bench-gate.sh"))]
    (when (not= 1 (count run-counts)) (die "rows timed different numbers of runs: " run-counts))
    (when-not (= (verify "jolt") (verify "jvm") (verify "node"))
      (die "cst-format hosts disagree on the parse: jolt " (verify "jolt") " jvm " (verify "jvm") " node " (verify "node")))
    (doseq [k ["date" "machine" "jolt" "jvm" "chez" "node"]]
      (need (str "env " k ": line (from bench/run.sh)") (not-empty (env k))))
    {:measured (str "Measured " (env "date") " on " (env "machine") ": jolt "
                    (str/replace (env "jolt") #"^jolt v?" "")
                    ", OpenJDK " (or (second (re-find #"version \"([^\"]+)\"" (env "jvm"))) (env "jvm"))
                    ", Chez " (env "chez") ", node " (str/replace (env "node") #"^v" "") ".")
     :runs-per-row (first run-counts)
     :mode-a {:gap (format "%.2f" gap) :name gap-name}
     :noise {:median (x2 (median (map second spreads)))
             :worst (let [ws (map (fn [[n s]] (str "`" n "` (" (x2 s) "×)")) (take 3 spreads))]
                      (str (str/join ", " (butlast ws)) " and " (last ws)))}
     :gc {:jolt-floor (x1 (floor "jolt")) :jvm-floor (x1 (floor "jvm")) :factor (x1 (/ (floor "jvm") (floor "jolt")))}
     :parallel {:jolt (x2 (slowdown "jolt")) :jvm (x2 (slowdown "jvm"))
                :spread (x2 (spread (runs-of (by-name "parallel-colls") "jolt")))}
     :cst {:kb (Math/round (/ chars 1000.0)) :node (str node-ms) :jvm (:jvm cst) :jolt (:jolt cst)
           :idiom (x1 (/ cst-jvm node-ms)) :jolt-vs-jvm (x1 (/ cst-jolt cst-jvm))
           :jolt-vs-node (x1 (/ cst-jolt node-ms))}
     :startup-reps (second (need "STARTUP_REPS default in run.sh" (re-find #"(?m)^STARTUP_REPS=\"\$\{STARTUP_REPS:-(\d+)\}\"" run-src)))
     :gate {:max (second (need "the max-ratio default in ci/bench-gate.sh" (re-find #"max=\"\$\{3:-([\d.]+)\}\"" gate-src)))
            :rounds (second (need "the rounds default in ci/bench-gate.sh" (re-find #"rounds=\"\$\{BENCH_GATE_ROUNDS:-(\d+)\}\"" gate-src)))}}))

(defn- parse-run-mode [text]
  (for [[_ label n j v] (re-seq #"(?m)^(.+?)\s+x(\d+)\s+jolt\s+([\d.]+) ms\s+jvm\s+([\d.]+) ms\s+\S+$" text)]
    {:label (str/trim label) :n n :jolt j :jvm v}))

(defn- bench-names [dir]
  (let [src (slurp (java.io.File. dir "run.sh"))
        benches (second (re-find #"(?m)^BENCHES=\"(.*)\"$" src))]
    (conj (mapv #(subs % 0 (str/index-of % ":")) (str/split benches #" ")) "startup")))

(defn -main [& args]
  (let [[logs opts] (loop [as args logs [] opts {}]
                      (cond (empty? as) [logs opts]
                            (= "--print" (first as)) (recur (next as) logs (assoc opts :print true))
                            :else (recur (next as) (conj logs (first as)) opts)))
        _ (when (empty? logs)
            (die "usage: scorecard.clj RUN.log [TESTCHECK.log ...] [--print]"))
        dir (.getParentFile (.getCanonicalFile (java.io.File. *file*)))
        text (str/join "\n" (map slurp logs))
        _ (when-let [[_ b] (re-find #"(?m)^(\S+)\s+jolt build FAILED" text)]
            (die "the run has a build failure: " b))
        aot (parse-aot text)
        run-mode (parse-run-mode text)
        have (set (map :name aot))
        wanted (bench-names dir)
        missing-rows (remove have wanted)
        missing-desc (remove descriptions wanted)
        row (fn [{:keys [name jolt jvm]}]
              {:name name :ratio (ratio-str jolt jvm) :jolt jolt :jvm jvm :what (descriptions name)})]
    (when (seq missing-rows) (die "no row in the logs for: " (str/join ", " missing-rows)))
    (when (seq missing-desc) (die "no description for: " (str/join ", " missing-desc)))
    (when (empty? run-mode) (die "no run-mode rows (bench/testcheck.sh output) in the logs"))
    (doseq [{:keys [label]} run-mode]
      (when-not (run-mode-descriptions label) (die "no description for the run-mode row: " label)))
    (selmer-util/turn-off-escaping!)
    (let [facts (derived aot (parse-env text) dir)
          row (fn [r] (update (row r) :what selmer/render facts))
          md (selmer/render
              (slurp (java.io.File. dir "README.tmpl"))
              (assoc facts
               :rows (map row (sort-by :sort (remove #(= "startup" (:name %)) aot)))
               :startup (row (first (filter #(= "startup" (:name %)) aot)))
               :run-rows (map (fn [{:keys [label n jolt jvm]}]
                                {:label label :n n :ratio (ratio-str jolt jvm) :jolt jolt :jvm jvm
                                 :what (run-mode-descriptions label)})
                              run-mode)))]
      (if (:print opts)
        (print md)
        (let [out (java.io.File. dir "README.md")]
          (spit out md)
          (println (str "wrote " out ": " (count aot) " AOT rows + " (count run-mode) " run-mode rows")))))))

(apply -main *command-line-args*)
