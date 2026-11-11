(ns generated-project.features
  (:require [clojure.string :as str]))

;; Categories govern generated operations. Their operands can still need basic
;; literals, arithmetic, bindings or collection constructors.
(def expression-groups
  {"core/arithmetic" #{:add :sub :mul :compare}
   "core/collections" #{:count :nth :last :get :meta :set :setop :dvec :dmap
                        :vlit :vrange :vconj :vassoc :vtransient :vvals
                        :mlit :mzip :massoc :mdissoc :mupdate :mmerge :mselect :mfreq
                        :mgroup :minto :mupdate-in :mtransient}
   "core/sequences" #{:sum :reduce :reduce-early :reduce-kv :transduce :for
                      :vmap :vfor :vfilter :vtake :vdrop :vsort :vmapcat :vkeep
                      :vreductions :viterate :vunfold :vdistinct :vinterleave :vpartition :vindexed}
   "core/control" #{:let :if :cond :case :case-lit :condp :cond-> :some-> :if-some :when-let :loop :letfn :try}
   "core/functions" #{:call :invoke :invoke2 :vjuxt}
   "core/state" #{:watch :delay :binding}
   "core/text" #{:index-of :slit :sstr :sjoin :sfn :sreplace :ssubs :sname :klit :kpick :kmake}
   "dispatch/protocols" #{:proto :satisfies :acc :rinvoke :field :mrec :rnew :rassoc :rupdate}
   "dispatch/multimethods" #{:multi :isa}
   "syntax/macros" #{:blend :twice :chain :argc :or-local :sum-all :doubled}})

;; Each form takes a bounded nonnegative integer x and a positive salt. Text
;; results are folded to integers so the full returned text affects the checksum.
(def calls
  (sorted-map
   "jvm/math/abs" '(Math/abs (long (- x salt)))
   "jvm/math/min" '(Math/min (long x) (long salt))
   "jvm/math/max" '(Math/max (long x) (long salt))
   "jvm/math/floor-mod" '(Math/floorMod (long (- salt x)) (long (inc salt)))
   "jvm/long/bit-count" '(Long/bitCount (long x))
   "jvm/long/parse-long" '(Long/parseLong (str x salt))
   "jvm/string/length" '(.length (str x salt))
   "jvm/string/index-of" '(.indexOf (str x) (str (mod salt 10)))
   "jvm/string/last-index-of" '(.lastIndexOf (str x) (str (mod salt 10)))
   "jvm/string/starts-with" '(if (.startsWith (str x) (str (mod salt 10))) 1 0)
   "jvm/string/ends-with" '(if (.endsWith (str x) (str (mod salt 10))) 1 0)
   "jvm/string-builder/reverse" '(text-code (str (.reverse (.append (StringBuilder. (str x)) (str salt)))))
   "jvm/byte-buffer/get-short" '(let [b (java.nio.ByteBuffer/allocate 4)]
                                  (.putInt b (int x))
                                  (.getShort b (int (mod salt 3))))
   "jvm/byte-buffer/flip" '(let [b (java.nio.ByteBuffer/allocate 8)]
                             (.putInt b (int x))
                             (.putShort b (short salt))
                             (.flip b)
                             (+ (.remaining b) (.getInt b) (.getShort b)))
   "jvm/array-deque/remove-first" '(let [d (java.util.ArrayDeque.)]
                                     (.addLast d x)
                                     (.addFirst d salt)
                                     (+ (* 31 (.removeFirst d)) (.peekLast d)))
   "jvm/array-list/get" '(let [a (java.util.ArrayList.)]
                           (.add a x)
                           (.add a salt)
                           (long (.get a (int (mod x 2)))))
   "jvm/duration/to-millis" '(.toMillis (.plusMillis (java.time.Duration/ofSeconds (long x)) (long salt)))
   "jvm/instant/plus-seconds" '(.getEpochSecond (.plusSeconds (java.time.Instant/ofEpochSecond (long x)) (long salt)))
   "jvm/uuid/least-significant-bits" '(.getLeastSignificantBits (java.util.UUID. (long x) (long (- x salt))))
   "jvm/atomic-long/add-and-get" '(.addAndGet (java.util.concurrent.atomic.AtomicLong. (long x)) (long (- salt)))
   "jvm/crc32/update" '(let [crc (java.util.zip.CRC32.)]
                         (.update crc (int x))
                         (.update crc (int salt))
                         (.getValue crc))
   "babashka/fs/file-name" '(text-code (fs/file-name (str "dir/" x "-" salt)))
   "babashka/fs/parent" '(text-code (str (fs/parent (str "dir" x "/" salt))))
   "babashka/fs/extension" '(text-code (fs/extension (str "file." x "-" salt)))
   "babashka/fs/strip-ext" '(text-code (fs/strip-ext (str x "-" salt ".edn")))
   "babashka/fs/normalize" '(text-code (str (fs/normalize (str "dir" salt "/../p" x))))))

(def leaves (into (sorted-set "load/unused") (concat (keys expression-groups) (keys calls))))

(def fs-dependency {'babashka/fs {:mvn/version "0.3.17"}})

(defn subtree? [parent leaf]
  (or (= parent leaf) (str/starts-with? leaf (str parent "/"))))

(defn expand [selectors]
  (into #{} (mapcat (fn [selector]
                      (let [matches (filter #(subtree? selector %) leaves)]
                        (when (empty? matches)
                          (throw (ex-info (str "Unknown category '" selector "' (see --list-categories)") {})))
                        matches)))
        selectors))

(defn selection [only exclude]
  (let [included (if (seq only) (expand only) leaves)
        excluded (expand exclude)]
    (vec (remove excluded (sort included)))))

(defn call-name [id] (symbol (str/replace id "/" "-")))

(defn api-forms [ids]
  (let [fs? (some #(subtree? "babashka/fs" %) ids)
        text? (or fs? (some #{"jvm/string-builder/reverse"} ids))]
    (concat [(if fs? '(ns workload.api (:require [babashka.fs :as fs])) '(ns workload.api))]
            (when text?
              ['(defn- text-code [s]
                  (reduce (fn [h c] (mod (+ (* h 31) (int c)) 1000003)) 0 s))])
            (for [id (sort ids)]
              (list 'defn (call-name id) '[x salt] (calls id))))))
