#!/usr/bin/env jolt
(ns generated-project.oracle
  "Evaluates model.edn without loading or invoking the generated Clojure source."
  (:require [babashka.fs :as fs]
            [clojure.edn :as edn]
            [clojure.set :as set]
            [clojure.string :as str]))

(def modulus 1000003)
(def common-ns "workload.common")
(defn norm [x] (mod x modulus))
(defn add [a b] (norm (+ a b)))
(def ^:dynamic *model* nil)

;; Record values. Fields and added keys live in one map.
(defrecord Rec [type m])
(defn kount [x] (if (instance? Rec x) (count (:m x)) (count x)))

(declare ev test? call-fn)
(defn lookup [env nm]
  (let [l (:l env)]
    (if (contains? l nm) (l nm) (throw (ex-info "Unbound local in model" {:name nm})))))
(defn bind [env nm v] (assoc-in env [:l nm] v))
(defn invoke [env f & args] (apply f (:d env) args))
(defn field-locals [r] (into {} (map (fn [f] [f (get (:m r) (keyword f))]) (get-in *model* [:records (:type r) :fields]))))

(defn builtin [nm args]
  (case nm
    "norm" (norm (first args))
    "add" (if (= 1 (count args)) (first args) (add (first args) (second args)))))

(defn call-fn [ref args dyn]
  (let [ref (loop [r ref] (if-let [t (get-in *model* [:aliases r])] (recur t) r))]
    (if (= common-ns (first ref))
      (builtin (second ref) args)
      (let [{:keys [arities macro]} (get-in *model* [:fns ref])
            n (count args)
            ar (or (first (filter #(and (not (:rest %)) (= n (count (:params %)))) arities))
                   (first (filter #(and (:rest %) (>= n (count (:params %)))) arities))
                   (throw (ex-info "No arity in model" {:fn ref :args n})))
            fixed (:params ar)
            l (cond-> (zipmap fixed args)
                (:rest ar) (assoc (:rv ar) (vec (drop (count fixed) args))))
            v (ev (:body ar) {:l l :d dyn})]
        (if macro (norm (+ (get-in *model* [:macros macro :salt]) v)) v)))))

(defn proto-impl [P recv]
  (let [impls (:impls *model*)
        t (cond (integer? recv) :number (string? recv) :string (instance? Rec recv) (:type recv))]
    (or (and t (get impls [P t])) (get impls [P :default])
        (throw (ex-info "No protocol implementation in model" {:protocol P :receiver recv})))))

(defn call-method [P m recv args env]
  (let [impl (proto-impl P recv)
        {:keys [this params body]} (get-in impl [:methods m (count args)])
        l (cond-> (zipmap params args)
            (:inline impl) (merge (field-locals recv))
            (not= this "_") (assoc this recv))]
    (ev body {:l l :d (:d env)})))

(defn tag-isa? [tag parent]
  (loop [t tag] (cond (nil? t) false (= t parent) true :else (recur (get-in *model* [:parents t])))))

(defn find-method [mm tag]
  (let [ms (get-in *model* [:methods mm])]
    (loop [t tag]
      (cond (nil? t) (ms "default")
            (contains? ms t) (ms t)
            :else (recur (get-in *model* [:parents t]))))))

(defn xf [env [kind a b]]
  (case kind
    :map (let [f (ev a env)] (map #(invoke env f %)))
    :filter (filter #(test? b (bind env a %)))
    :remove (remove #(test? b (bind env a %)))
    :take (take a)
    :drop (drop a)))

(defn test? [t env]
  (let [e #(ev % env)]
    (case (:t t)
      :odd (odd? (e (:x t))) :even (even? (e (:x t))) :lt (< (e (:a t)) (e (:b t)))
      :eq (= (e (:a t)) (e (:b t))) :zmod (zero? (mod (e (:x t)) (:k t))) :pos (pos? (e (:x t)))
      :empty (empty? (e (:v t))) :has (contains? (e (:m t)) (keyword (:k t)))
      :starts (str/starts-with? (e (:s t)) (:p t)) :includes (str/includes? (e (:s t)) (:p t))
      :not (not (test? (:x t) env)) :and (and (test? (:a t) env) (test? (:b t) env))
      :or (or (test? (:a t) env) (test? (:b t) env)) :str= (= (e (:a t)) (e (:b t)))
      :seq (boolean (seq (e (:v t)))))))

(defn macro-of [n] (get-in *model* [:macros (:mac n)]))
(defn sum [xs] (reduce add 0 xs))

;; Reference semantics for the method pool. Keep Java dispatch and path APIs
;; out of these computations so a faulty shim can disagree with the model.
(defn crc32 [bytes]
  (let [step (fn [crc byte]
               (loop [v (bit-xor crc (bit-and byte 255)) bits 8]
                 (if (zero? bits) v
                     (recur (bit-xor (bit-shift-right v 1) (if (odd? v) 0xedb88320 0))
                            (dec bits)))))]
    (bit-xor 0xffffffff (reduce step 0xffffffff bytes))))

(defn api-result [method x salt]
  (let [digits (str x) suffix (str salt) digit (str (mod salt 10))
        text-code (fn [s] (reduce (fn [h c] (norm (+ (* 31 h) (int c)))) 0 s))]
    (case method
      "jvm/math/abs" (abs (- x salt))
      "jvm/math/min" (min x salt)
      "jvm/math/max" (max x salt)
      "jvm/math/floor-mod" (mod (- salt x) (inc salt))
      "jvm/long/bit-count" (loop [n x ones 0]
                             (if (zero? n) ones (recur (quot n 2) (+ ones (mod n 2)))))
      "jvm/long/parse-long" (+ (* x (reduce * (repeat (count suffix) 10))) salt)
      "jvm/string/length" (+ (count digits) (count suffix))
      "jvm/string/index-of" (or (first (keep-indexed #(when (= (str %2) digit) %1) digits)) -1)
      "jvm/string/last-index-of" (or (last (keep-indexed #(when (= (str %2) digit) %1) digits)) -1)
      "jvm/string/starts-with" (if (= (str (first digits)) digit) 1 0)
      "jvm/string/ends-with" (if (= (str (last digits)) digit) 1 0)
      "jvm/string-builder/reverse" (text-code (reverse (str x salt)))
      "jvm/byte-buffer/get-short" (let [shift (* 8 (- 2 (mod salt 3)))
                                        unsigned (bit-and 65535 (bit-shift-right x shift))]
                                    (if (< unsigned 32768) unsigned (- unsigned 65536)))
      "jvm/byte-buffer/flip" (+ 6 x salt)
      "jvm/array-deque/remove-first" (+ (* 31 salt) x)
      "jvm/array-list/get" (if (even? x) x salt)
      "jvm/duration/to-millis" (+ (* 1000 x) salt)
      "jvm/instant/plus-seconds" (+ x salt)
      ("jvm/uuid/least-significant-bits" "jvm/atomic-long/add-and-get") (- x salt)
      "jvm/crc32/update" (crc32 [x salt])
      ("babashka/fs/file-name" "babashka/fs/extension" "babashka/fs/strip-ext") (text-code (str x "-" salt))
      "babashka/fs/parent" (text-code (str "dir" x))
      "babashka/fs/normalize" (text-code (str "p" x)))))

(defn ev [n env]
  (let [e #(ev % env)]
    (case (:op n)
      :lit (:v n)
      :local (lookup env (:name n))
      :dyn (let [v (:var n)] (if (contains? (:d env) v) ((:d env) v) (get-in *model* [:dyns v])))
      :add (norm (reduce + (map e (:xs n))))
      :sub (norm (- (e (:a n)) (e (:b n))))
      :mul (norm (* (e (:a n)) (e (:b n))))
      :count (kount (e (:x n)))
      :sum (norm (reduce + 0 (e (:v n))))
      :nth (nth (e (:v n)) (mod (e (:i n)) 6) (:d n))
      :last (or (peek (e (:v n))) (:d n))
      :get (get (e (:m n)) (keyword (:k n)) (:d n))
      :field (get (:m (e (:x n))) (keyword (:f n)))
      :call (call-fn (:fn n) (mapv e (:args n)) (:d env))
      :invoke (invoke env (e (:f n)) (e (:x n)))
      :invoke2 (invoke env (e (:f n)) (e (:a n)) (e (:b n)))
      :let (ev (:body n) (reduce (fn [env [nm x]] (bind env nm (ev x env))) env (:binds n)))
      :if (if (test? (:test n) env) (e (:then n)) (e (:else n)))
      ;; Integer results are never nil or false, so some finds a chosen branch.
      :cond (or (some (fn [[t x]] (when (test? t env) (e x))) (:clauses n)) (e (:else n)))
      :case (let [i (mod (e (:x n)) (:k n))]
              (if (< i (count (:branches n))) (e (nth (:branches n) i)) (e (:else n))))
      :case-lit (let [i (mod (e (:i n)) (count (:lits n)))]
                  (if (< i (count (:branches n))) (e (nth (:branches n) i)) (e (:else n))))
      :condp (let [x (e (:x n)) f (if (= :lt (:pred n)) < >)]
               (or (some (fn [[t r]] (when (f t x) (e r))) (map vector (:ts n) (:es n))) (e (:else n))))
      :cond-> (norm (reduce (fn [acc {:keys [test f k]}]
                              (if (test? test env) (case f :add (+ acc k) :sub (- acc k) :mul (* acc k)) acc))
                            (e (:x n)) (:steps n)))
      :some-> (let [v (get (e (:m n)) (keyword (:k n)))] (norm (if (nil? v) (:d n) (+ v (:n n)))))
      :if-some (let [v (get (e (:m n)) (keyword (:k n)))]
                 (if (some? v) (ev (:body n) (bind env (:name n) v)) (e (:else n))))
      :when-let (let [v (first (filter #(test? (:test n) (bind env (:p n) %)) (e (:v n))))]
                  (if v (ev (:body n) (bind env (:name n) v)) (:d n)))
      :loop (let [lim (+ 2 (mod (e (:n n)) 5))]
              (loop [i 0 acc (e (:init n))]
                (if (< i lim) (recur (inc i) (ev (:body n) (-> env (bind (:acc n) acc) (bind (:i n) i)))) acc)))
      :letfn (loop [k (mod (e (:n n)) 5) v (e (:init n)) f? true]
               (if (zero? k) v (recur (dec k) (ev (if f? (:b1 n) (:b2 n)) (bind env (:vp n) v)) (not f?))))
      :reduce (reduce (fn [acc x] (ev (:body n) (-> env (bind (:acc n) acc) (bind (:x n) x)))) (e (:init n)) (e (:v n)))
      :reduce-early (reduce (fn [acc x] (if (> acc (:limit n)) (reduced acc) (ev (:body n) (-> env (bind (:acc n) acc) (bind (:x n) x)))))
                            (e (:init n)) (e (:v n)))
      :reduce-kv (reduce-kv (fn [acc k v] (ev (:body n) (-> env (bind (:acc n) acc) (bind (:k n) k) (bind (:val n) v))))
                            (e (:init n)) (into (sorted-map) (e (:m n))))
      :transduce (norm (transduce (apply comp (map #(xf env %) (:steps n))) + (e (:init n)) (e (:v n))))
      :for (sum (for [x (e (:v n)) j (if (:j n) (range (:n n)) [nil])
                      :let [env1 (cond-> (bind env (:x n) x) (:j n) (bind (:j n) j))
                            env2 (bind env1 (:y n) (ev (:ye n) env1))]
                      :when (test? (:test n) env2)]
                  (ev (:body n) env2)))
      :watch (let [xs (e (:v n))]
               (add (reduce (fn [acc x] (ev (:body n) (-> env (bind (:acc n) acc) (bind (:x n) x)))) (e (:init n)) xs)
                    (count xs)))
      :delay (norm (+ (* 2 (e (:e n))) (:k n)))
      :binding (ev (:body n) (assoc-in env [:d (:var n)] (e (:val n))))
      :try (let [res (if (test? (:test n) env)
                       (let [cb (ev (:cbody n) (bind env (:name n) (e (:v n))))]
                         (if (:msglen n) (add cb (count (:msg n))) cb))
                       (e (:then n)))]
             (add res 1))
      :proto (call-method (:p n) (:m n) (e (:recv n)) (mapv e (:args n)) env)
      ;; Object extensions also apply to generated records.
      :satisfies (if (let [impls (:impls *model*) P (:p n)]
                       (or (contains? impls [P (:type (e (:recv n)))]) (contains? impls [P :default])))
                   (e (:then n)) (e (:else n)))
      :multi (let [tag (nth (:tags n) (mod (e (:i n)) (count (:tags n))))
                   a (e (:a n)) b (e (:b n))
                   m (find-method (:mm n) tag)]
               (ev (:body m) {:l {(:a m) a (:b m) b} :d (:d env)}))
      :isa (if (tag-isa? (nth (:tags n) (mod (e (:i n)) (count (:tags n)))) (:parent n)) (e (:then n)) (e (:else n)))
      :acc (let [{:keys [total step kt ks x body]} (get-in *model* [:types (:t n)])
                 s (e (:b n))
                 final (reduce (fn [t v] (ev body {:l {total t step s x v} :d (:d env)})) (e (:a n)) (e (:v n)))
                 key-value #(cond (= % kt) final (= % ks) s :else %2)]
             (case (:proj n)
               :total final
               :step s
               :missing (key-value (:mk n) (:d n))))
      :rinvoke (let [r (e (:r n)) {:keys [p body]} (get-in *model* [:records (:type r) :invoke])]
                 (ev body {:l (assoc (field-locals r) p (e (:x n))) :d (:d env)}))
      :index-of (or (str/index-of (e (:s n)) (:sub n)) (:d n))
      :compare (let [order (compare (e (:a n)) (e (:b n)))]
                 (cond (neg? order) 1 (pos? order) 3 :else 2))
      :api-call (let [x (add (lookup env (:input n)) (e (:x n)))]
                  (add x (api-result (:method n) x (:salt n))))
      ;; Older standalone projects carry the original four-method operation.
      :java-mix (let [salt (:salt n)
                      magnitude (abs (- (e (:x n)) salt))
                      suffix (str salt)
                      digits (str magnitude suffix)
                      scale (reduce * (repeat (count suffix) 10))]
                  (norm (+ (* magnitude scale) salt (count digits) (str/index-of digits suffix))))
      :blend (let [{:keys [salt]} (macro-of n)] (norm (+ (e (:a n)) (* (e (:b n)) (:k n)) salt)))
      :twice (norm (+ (* 2 (e (:e n))) (:salt (macro-of n))))
      :chain (let [{:keys [k]} (macro-of n)] (reduce (fn [v s] (norm (+ (* v k) (e s)))) (e (:x n)) (:steps n)))
      :argc (+ (:salt (macro-of n)) 1 (count (:syms n)))
      :or-local (if (:local n) (lookup env (:sym n)) (:fallback n))
      :sum-all (let [{:keys [k]} (macro-of n)] (norm (reduce + (map #(* k (e %)) (:xs n)))))
      :doubled (ev (:body n) (bind env (:name n) (norm (* 2 (e (:init n))))))
      :meta (norm (+ (count (e (:v n))) (:tag n)))
      :set (let [v (e (:v n)) a (e (:a n)) b (e (:b n))] (sum (disj (conj (set v) a) b)))
      :setop (count ((case (:kind n) :union set/union :intersection set/intersection :difference set/difference)
                     (set (e (:v n))) (set (e (:w n)))))
      :dvec (let [v (e (:v n))
                  env1 (-> env (bind (:p n) (or (nth v 0 nil) (:d1 n))) (bind (:q n) (or (nth v 1 nil) (:d2 n))))]
              (add (ev (:body n) env1) (count (drop 2 v))))
      :dmap (let [m (e (:m n))
                  env1 (reduce (fn [env [nm k d]] (bind env nm (get m (keyword k) d))) env (:pairs n))
                  b (ev (:body n) env1)]
              (if (:as n) (norm (+ b (count m))) b))
      ;; vectors
      :vlit (mapv e (:xs n))
      :vrange (vec (range (:n n)))
      :vmap (let [f (e (:f n))] (mapv #(invoke env f %) (e (:v n))))
      :vfor (mapv #(ev (:body n) (bind env (:x n) %)) (e (:v n)))
      :vfilter (filterv #(test? (:test n) (bind env (:p n) %)) (e (:v n)))
      :vconj (into (e (:v n)) (map e (:xs n)))
      :vassoc (let [v (e (:v n)) i (e (:i n)) x (e (:x n))] (if (seq v) (assoc v (mod i (count v)) x) [x]))
      :vtake (vec (take (:n n) (e (:v n))))
      :vdrop (vec (drop (:n n) (e (:v n))))
      :vsort (let [v (e (:v n))]
               (case (:kind n)
                 :asc (vec (sort v))
                 :desc (vec (sort > v))
                 :by (let [f (e (:f n))] (vec (sort-by #(invoke env f %) v)))
                 :by-desc (let [f (e (:f n))] (vec (sort-by #(invoke env f %) > v)))))
      :vmapcat (vec (take 32 (mapcat (fn [x] (let [env1 (bind env (:p n) x)] [(ev (:a n) env1) (ev (:b n) env1)])) (e (:v n)))))
      :vkeep (vec (keep #(let [env1 (bind env (:p n) %)] (when (test? (:test n) env1) (ev (:body n) env1))) (e (:v n))))
      :vreductions (let [f (e (:f n))] (vec (reductions #(invoke env f %1 %2) (e (:init n)) (e (:v n)))))
      (:viterate :vunfold) (let [f (e (:f n))] (vec (take (:n n) (iterate #(invoke env f %) (e (:x n))))))
      :vdistinct (vec (distinct (e (:v n))))
      :vinterleave (vec (take 32 (interleave (e (:v n)) (e (:w n)))))
      :vpartition (mapv sum (partition-all (:n n) (e (:v n))))
      :vindexed (vec (map-indexed (fn [i x] (norm (+ x (* i (:k n))))) (e (:v n))))
      :vtransient (into (e (:v n)) (map e (:xs n)))
      :vvals (vec (vals (into (sorted-map) (e (:m n)))))
      :vjuxt (let [fs (mapv e (:fs n)) x (e (:x n))] (mapv #(invoke env % x) fs))
      ;; maps
      :mlit (into {} (map (fn [[k x]] [(keyword k) (e x)]) (:entries n)))
      :mzip (zipmap (map keyword (:ks n)) (e (:v n)))
      :massoc (assoc (e (:m n)) (keyword (:k n)) (e (:x n)))
      :mdissoc (dissoc (e (:m n)) (keyword (:k n)))
      :mupdate (let [m (e (:m n)) x (e (:x n))] (update m (keyword (:k n)) #(add (or % 0) x)))
      :mmerge (if (:with n) (merge-with add (e (:a n)) (e (:b n))) (merge (e (:a n)) (e (:b n))))
      :mselect (select-keys (e (:m n)) (map keyword (:ks n)))
      :mfreq (let [ks (:ks n)] (frequencies (map #(keyword (nth ks (mod % (count ks)))) (e (:v n)))))
      :mgroup (let [ks (:ks n)]
                (into {} (map (fn [[k xs]] [k (sum xs)]) (group-by #(keyword (nth ks (mod % (count ks)))) (e (:v n))))))
      :minto (let [ks (:ks n)] (into {} (map (fn [x] [(keyword (nth ks (mod x (count ks)))) x]) (e (:v n)))))
      :mupdate-in (let [k1 (keyword (:k1 n)) m (e (:m n)) x (e (:x n))]
                    (get (update-in {k1 m} [k1 (keyword (:k2 n))] #(add (or % 0) x)) k1))
      :mtransient (assoc (e (:m n)) (keyword (:k n)) (e (:x n)))
      :mrec (:m (e (:r n)))
      ;; strings and keywords
      :slit (:s n)
      :sstr (apply str (map e (:parts n)))
      :sjoin (str/join (:sep n) (map e (:parts n)))
      :sfn ((case (:f n) :upper str/upper-case :lower str/lower-case :cap str/capitalize :rev str/reverse :trim str/trim)
            (e (:s n)))
      :sreplace (str/replace (e (:s n)) (:from n) (:to n))
      :ssubs (let [s (e (:s n))] (subs s 0 (min (:n n) (count s))))
      :sname (name (e (:k n)))
      :klit (keyword (:k n))
      :kpick (keyword (nth (:ks n) (mod (e (:i n)) (count (:ks n)))))
      :kmake (keyword (e (:s n)))
      ;; records
      :rnew (->Rec (:r n) (zipmap (map keyword (get-in *model* [:records (:r n) :fields])) (mapv e (:args n))))
      :rassoc (let [r (e (:x n)) v (e (:v n))] (assoc-in r [:m (keyword (:k n))] v))
      :rupdate (let [r (e (:x n)) v (e (:v n))] (update-in r [:m (keyword (:f n))] #(add % v)))
      ;; function values take the caller's dynamic bindings first
      :lambda (fn [dyn x] (ev (:body n) (-> env (assoc :d dyn) (bind (:p n) x))))
      :lambda2 (fn [dyn a b] (ev (:body n) (-> env (assoc :d dyn) (bind (first (:ps n)) a) (bind (second (:ps n)) b))))
      :fref (fn [dyn & args] (call-fn (:fn n) args dyn))
      :partial (let [a (e (:a n))] (fn [dyn & args] (call-fn (:fn n) (cons a args) dyn)))
      :comp (let [f (e (:f n)) g (e (:g n))] (fn [dyn x] (f dyn (g dyn x))))
      :juxtsum (let [fs (mapv e (:fs n))] (fn [dyn x] (sum (map #(% dyn x) fs))))
      (throw (ex-info "Unknown model operation" {:node n})))))

(defn run-worker [x {:keys [kernels]}]
  (reduce (fn [v k] (norm (+ (* 31 v) (call-fn k [v] {})))) x kernels))

(defn expected [model input iterations]
  (when-not (and (#{3 4} (:format model)) (seq (:workers model)))
    (throw (ex-info "Unsupported or empty generated-project model" {})))
  (when-not (and (integer? input) (<= 0 input (dec modulus))
                 (integer? iterations) (<= 1 iterations 1000000))
    (throw (ex-info "INPUT must be 0..1000002; ITERATIONS must be 1..1000000" {})))
  (binding [*model* model]
    (reduce (fn [v iteration] (reduce run-worker (norm (+ v iteration)) (:workers model)))
            input (range iterations))))

(defn main [args]
  (when (> (count args) 2)
    (throw (ex-info "Usage: jolt oracle.bb [INPUT=1 [ITERATIONS=1]]" {})))
  (let [dir (fs/parent (fs/absolutize *file*))
        model (edn/read-string (slurp (fs/file dir "model.edn")))
        input (if (first args) (parse-long (first args)) 1)
        iterations (if (second args) (parse-long (second args)) 1)]
    (println (expected model input iterations))))

;; Run as a script, not loaded by test.bb. bb names the script being run in
;; babashka.file, and jolt in jolt.file. A jolt older than jolt.file names
;; neither, so the script runs unless test.bb has marked the load.
(when (if-let [running (or (System/getProperty "jolt.file") (System/getProperty "babashka.file"))]
        (= (fs/absolutize *file*) (fs/absolutize running))
        (not (System/getProperty "generated-project.loading")))
  (main *command-line-args*))
