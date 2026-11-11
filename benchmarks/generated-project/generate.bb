#!/usr/bin/env bb
(ns generated-project.generate
  (:require [babashka.fs :as fs]
            [clojure.pprint :as pprint]
            [clojure.string :as str]))

(def kit (fs/parent (fs/absolutize *file*)))
(load-file (str (fs/path kit "features.bb")))
(alias 'features 'generated-project.features)
(load-file (str (fs/path kit "collection-profiles.bb")))
(alias 'coll-profiles 'generated-project.collection-profiles)
(def all-leaves (into features/leaves (keys coll-profiles/operations)))
(def modulus 1000003)
(def common-ns "workload.common")
(def defaults {:seed 1 :lines 5000 :namespace-lines 400 :categories (vec features/leaves)})
(def help
  "Usage: bb benchmarks/generated-project/generate.bb OUTPUT-DIR [OPTIONS]

  --seed N             Signed 64-bit seed (default 1)
  --lines N            Approximate nonblank source lines, e.g. 5k, 200k, 800k (default 5k)
  --namespace-lines N  Approximate lines per worker namespace (default 400; minimum 200)
  --collection-sizes Ns  Generate an operation-by-size suite at comma-separated element counts
  --only A,B           Enable these categories or subtrees (default all)
  --exclude A,B        Disable these categories or subtrees after --only
  --list-categories    List supported category paths
  --help               Show this help

OUTPUT-DIR must be absent or empty. Generation requires only Babashka.
Every project includes portable Clojure source, a reference interpreter,
and a timing and checksum runner for Jolt, JVM Clojure and Babashka.")

(defn size-number [s]
  (when-let [[_ digits suffix] (and s (re-matches #"([0-9]+)([kKmM]?)" s))]
    (when-let [n (parse-long digits)]
      (try (Math/multiplyExact n (long (case (str/lower-case suffix) "k" 1000 "m" 1000000 1)))
           (catch ArithmeticException _ nil)))))

(defn parse-options [args]
  (loop [[arg & more] args options defaults]
    (cond
      (nil? arg)
      (do (when-not (:output options) (throw (ex-info "An output directory is required" {})))
          (when-not (<= 1000 (:lines options) 10000000)
            (throw (ex-info "--lines must be 1000..10000000" {})))
          (when-not (<= 200 (:namespace-lines options) 20000)
            (throw (ex-info "--namespace-lines must be 200..20000" {})))
          (let [{:keys [only exclude collection-sizes]} options
                matrix? (or collection-sizes (some #(features/subtree? "collections" %) only))
                catalog (if matrix? (keys coll-profiles/operations) features/leaves)]
            (cond-> (assoc (dissoc options :only :exclude) :categories (features/selection catalog all-leaves only exclude))
              matrix? (assoc :collection-sizes (or collection-sizes coll-profiles/sizes)))))
      (= arg "--help") {:help true}
      (= arg "--list-categories") {:list-categories true}
      (= arg "--collection-sizes")
      (let [sizes (mapv size-number (str/split (or (first more) "") #"," -1))]
        (when-not (every? #(and % (<= 1 % 1000000)) sizes)
          (throw (ex-info "Collection sizes must be comma-separated integers in 1..1000000" {})))
        (recur (rest more) (assoc options :collection-sizes (vec (distinct sizes)))))
      (#{"--only" "--exclude"} arg)
      (let [selectors (mapv str/trim (str/split (or (first more) "") #"," -1))]
        (recur (rest more) (update options (keyword (subs arg 2)) (fnil into []) selectors)))
      (#{"--seed" "--lines" "--namespace-lines"} arg)
      (let [v ((if (= arg "--seed") parse-long size-number) (or (first more) ""))]
        (when-not v (throw (ex-info (str "Invalid or missing value for " arg) {})))
        (recur (rest more) (assoc options (keyword (subs arg 2)) v)))
      (str/starts-with? arg "-") (throw (ex-info (str "Unknown option: " arg) {}))
      (:output options) (throw (ex-info (str "Unexpected argument: " arg) {}))
      :else (recur more (assoc options :output arg)))))

;;; Seeded choices. Everything below draws from one java.util.Random in a
;;; fixed order, so a seed and the size options determine every byte.

(defn rint [^java.util.Random rng n] (.nextInt rng (int n)))
(defn between [rng lo hi] (+ lo (rint rng (inc (- hi lo)))))
(defn chance [^java.util.Random rng p] (< (.nextDouble rng) p))
(defn pick [rng xs] (nth xs (rint rng (count xs))))
(defn shuffled [rng xs]
  ;; Fisher-Yates; no dependency on Clojure's global random source.
  (loop [v (vec xs) i (dec (count xs))]
    (if (<= i 0) v
        (let [j (rint rng (inc i))]
          (recur (assoc v i (v j) j (v i)) (dec i))))))
(defn sample [rng n xs] (vec (take n (shuffled rng xs))))
(defn weighted [rng pairs]
  (let [r (rint rng (reduce + (map second pairs)))]
    (loop [[[x w] & more] pairs acc 0]
      (if (< r (+ acc w)) x (recur more (+ acc w))))))

;;; Names. Identifiers are pronounceable seeded words, unique per namespace,
;;; and disjoint from core names and generated support definitions.

(def onsets ["b" "d" "f" "g" "h" "j" "k" "l" "m" "n" "p" "r" "s" "t" "v" "z"
             "br" "dr" "kl" "st" "tr" "sk" "pl" "gr"])
(def vowels ["a" "e" "i" "o" "u" "a" "o" "ai" "ei" "ou"])
(def codas ["" "" "" "" "n" "r" "l" "s" "m" "k"])
(def reserved
  (into #{} (concat
             (map str (keys (ns-publics 'clojure.core)))
             (str/split "def if do let quote var fn loop recur throw try catch finally new set! this
nil true false js goog cljs clj module require exports process global window document console
arguments eval undefined null delete void with yield case default export import super static
enum await async function return typeof instanceof in of const class extends implements
interface package private protected public native debugger switch break continue while for
else char byte long short int float double boolean abstract final goto synchronized throws
transient volatile str set array aget aset alength specify exists this-as uuid munge demunge
object number string run main tag" #"\s+"))))

(defn raw-word [rng]
  (apply str (repeatedly (between rng 2 3) #(str (pick rng onsets) (pick rng vowels) (pick rng codas)))))
(defn pascal [w] (str/join (map str/capitalize (str/split w #"-"))))
(defn fresh! [rng used kind]
  (loop []
    (let [w (raw-word rng)
          w (if (and (= kind :var) (chance rng 0.15)) (str w "-" (raw-word rng)) w)
          s (case kind (:var :ns) w :type (pascal w) :dyn (str "*" w "*"))]
      (if (or (reserved w) (contains? @used w) (contains? @used s))
        (recur)
        (do (swap! used conj w s) s)))))

;;; Code is built as Clojure data and laid out by a small pretty printer.

(defrecord Raw [s])
(defrecord MapF [pairs])
(defrecord BVec [pairs])
(defrecord SetF [xs])
(defrecord Short [body])
(defn raw [s] (->Raw s))
(defn mapf [pairs] (->MapF (vec pairs)))
(defn bvec [pairs] (->BVec (vec pairs)))
(defn setf [xs] (->SetF (vec xs)))
(defn L [& xs] (apply list xs))
(defn at [s] (raw (str "@" s)))

(defn atom-text [x] (cond (nil? x) "nil" (string? x) (pr-str x) :else (str x)))
(declare doc)
(defn seq-doc [k open close xs]
  (let [kids (mapv doc xs)]
    {:k k :open open :close close :kids kids
     :flat (str open (str/join " " (map :flat kids)) close)}))
(defn pairs-doc [k open close pairs]
  (let [kids (mapv (fn [[a b]] [(doc a) (doc b)]) pairs)]
    {:k k :open open :close close :kids kids
     :flat (str open (str/join " " (map (fn [[a b]] (str (:flat a) " " (:flat b))) kids)) close)}))
(defn doc [f]
  (cond
    (instance? Raw f) {:k :atom :flat (:s f)}
    (instance? Short f) (let [b (doc (:body f))] {:k :short :body b :flat (str "#" (:flat b))})
    (instance? MapF f) (pairs-doc :map "{" "}" (:pairs f))
    (instance? BVec f) (pairs-doc :bvec "[" "]" (:pairs f))
    (instance? SetF f) (seq-doc :set "#{" "}" (:xs f))
    (seq? f) (seq-doc :list "(" ")" f)
    (vector? f) (seq-doc :vec "[" "]" f)
    :else {:k :atom :flat (atom-text f) :sym (when (symbol? f) (str f))}))

(def width 80)
(def pad (memoize (fn [n] (apply str (repeat n \space)))))
(defn end-col [^String s col]
  (let [i (.lastIndexOf s "\n")] (if (neg? i) (+ col (count s)) (- (count s) i 1))))
(declare lay)
(defn lay-items [docs col] (str/join (str "\n" (pad col)) (map #(lay % col) docs)))
(defn lay-pairs [pairs col]
  (str/join (str "\n" (pad col))
            (map (fn [[a b]]
                   (let [as (lay a col) c (inc (end-col as col))]
                     (if (and (> c (+ col 16)) (> (+ c (count (:flat b))) width))
                       (str as "\n" (pad (inc col)) (lay b (inc col)))
                       (str as " " (lay b c)))))
                 pairs)))
(defn lay-atoms [kids col]
  (loop [[k & more] kids out "" c col]
    (if-not k out
      (let [f (:flat k)]
        (cond (= out "") (recur more f (+ c (count f)))
              (<= (+ c 1 (count f)) width) (recur more (str out " " f) (+ c 1 (count f)))
              :else (recur more (str out "\n" (pad col) f) (+ col (count f))))))))
(def head-fixed {"let" 1 "loop" 1 "binding" 1 "for" 1 "doseq" 1 "dotimes" 1 "if-let" 1 "when-let" 1
                 "if-some" 1 "when-some" 1 "letfn" 1 "if" 1 "if-not" 1 "when" 1 "when-not" 1
                 "defrecord" 2 "deftype" 2 "defprotocol" 1 "extend-type" 2 "extend-protocol" 1
                 "ns" 1 "catch" 2 "do" 0 "try" 0 "finally" 0 "defmulti" 1 "defmacro" 2})
(def pair-heads {"cond" 0 "case" 1 "condp" 2 "cond->" 1})
(def fn-heads #{"defn" "defn-" "fn" "defmethod"})
(defn header [d]
  (let [[h & args] (:kids d) n (count args) s (:sym h) atom? #(= :atom (:k %))]
    (cond
      (nil? s) nil
      (contains? pair-heads s) [:pairs (min n (pair-heads s))]
      (= s "def") [:body (min (dec n) (count (take-while atom? args)))]
      (fn-heads s) (let [a (count (take-while atom? args))]
                     [:body (min n (+ a (if (#{:vec :bvec} (:k (nth args a nil))) 1 0)))])
      (contains? head-fixed s) [:body (min n (head-fixed s))]
      :else (let [i (first (keep-indexed (fn [i a] (when (#{:vec :bvec} (:k a)) i)) (take 3 args)))]
              (when (and i (every? atom? (take i args))) [:body (inc i)])))))
(defn lay-clauses [xs c]
  (apply str (for [pair (partition-all 2 xs)]
               (if (= 2 (count pair))
                 (let [[t e] pair ts (lay t c) ec (inc (end-col ts c))]
                   (if (and (not (str/includes? ts "\n")) (<= (+ ec (count (:flat e))) width))
                     (str "\n" (pad c) ts " " (:flat e))
                     (str "\n" (pad c) ts "\n" (pad (+ c 2)) (lay e (+ c 2)))))
                 (str "\n" (pad c) (lay (first pair) c))))))
(defn lay-list [d col]
  (let [[h & args] (:kids d)
        hs (lay h (inc col))]
    (if-let [[kind n] (header d)]
      (let [[hdr body] (split-at n args)
            line (reduce (fn [s a] (str s " " (lay a (inc (end-col s col))))) (str "(" hs) hdr)
            bc (+ col 2)]
        (str line
             (if (= kind :pairs)
               (lay-clauses body bc)
               (apply str (map #(str "\n" (pad bc) (lay % bc)) body)))
             ")"))
      (let [hl (count (:flat h))]
        (if (and (= :atom (:k h)) (<= hl 18) (seq args) (<= (+ col 2 hl) 40))
          (str "(" hs " " (lay-items args (+ col 2 hl)) ")")
          (str "(" hs (apply str (map #(str "\n" (pad (inc col)) (lay % (inc col))) args)) ")"))))))
(defn lay [d col]
  (if (or (= :atom (:k d)) (<= (+ col (count (:flat d))) width))
    (:flat d)
    (case (:k d)
      :short (str "#" (lay (:body d) (inc col)))
      (:vec :set) (let [c (+ col (count (:open d))) kids (:kids d)]
                    (str (:open d) (if (every? #(= :atom (:k %)) kids) (lay-atoms kids c) (lay-items kids c)) (:close d)))
      (:map :bvec) (str (:open d) (lay-pairs (:kids d) (inc col)) (:close d))
      :list (lay-list d col))))
(defn text [form] (lay (doc form) 0))

(defn code-lines [text]
  (count (remove #(or (str/blank? %) (str/starts-with? (str/triml %) ";"))
                 (str/split-lines text))))

(defn form-walk [f x]
  (cond (instance? Short x) (->Short (form-walk f (:body x)))
        (instance? MapF x) (->MapF (mapv (fn [[a b]] [(form-walk f a) (form-walk f b)]) (:pairs x)))
        (instance? BVec x) (->BVec (mapv (fn [[a b]] [(form-walk f a) (form-walk f b)]) (:pairs x)))
        (instance? SetF x) (->SetF (mapv #(form-walk f %) (:xs x)))
        (instance? Raw x) x
        (seq? x) (apply list (map #(form-walk f %) x))
        (vector? x) (mapv #(form-walk f %) x)
        :else (f x)))
(defn form-seq [x]
  (tree-seq #(or (seq? %) (vector? %) (instance? Short %) (instance? MapF %) (instance? BVec %) (instance? SetF %))
            #(cond (instance? Short %) [(:body %)]
                   (or (instance? MapF %) (instance? BVec %)) (apply concat (:pairs %))
                   (instance? SetF %) (:xs %)
                   :else (seq %))
            x))

;;; The model. Expression nodes are maps with :op; oracle.bb interprets the
;;; same data. Global entities are [namespace name] pairs.

(defn new-world []
  {:fns {} :aliases {} :protocols {} :records {} :impls {} :types {} :methods {}
   :parents {} :dyns {[common-ns "*knob*"] 7} :macros {} :mm-tags {} :workers []})

(defn node-seq [x]
  (filter #(and (map? %) (:op %))
          (tree-seq (some-fn map? vector?) #(if (map? %) (vals %) %) x)))

;;; Expression generation. A generation context (gc) carries the lexical
;;; scope, what may be called, and the iteration depth: bodies that run per
;;; element add a level, user functions cost one level, and kernels allow two,
;;; so runtime grows linearly with project size.

(defn rnd [gc] (:rng gc))
(defn world [gc] @(:W gc))
(defn iter? [gc] (< (:loops gc) (:max-loops gc)))
(defn inner [gc] (update gc :loops inc))
(defn fn1-key [gc] (keyword (str "fn1@" (:loops gc))))
(defn with-local [gc t nm]
  (update-in gc [:scope (if (= t :fn1) (fn1-key gc) t)] (fnil conj []) nm))
(defn locals [gc t] (get-in gc [:scope t]))
(defn fresh [gc] (fresh! (rnd gc) (:names gc) :var))
(defn kd [gc d] (if (chance (rnd gc) 0.3) (rint (rnd gc) d) (dec d)))
(defn a-key [gc] (pick (rnd gc) (:keys gc)))
(defn lit [gc] {:op :lit :v (rint (rnd gc) 997)})
(defn rec-types [gc] (get-in gc [:vis :records]))
(defn fields-of [gc R] (get-in (world gc) [:records R :fields]))
(defn rec-locals [gc]
  (sort-by first (for [[t nms] (:scope gc) :when (vector? t) nm nms] [nm (second t)])))
(defn dispatch? [gc] (and (:dispatch? gc) (iter? gc)))
(defn fref-ok? [gc] (and (:dispatch? gc) (<= (+ 2 (:loops gc)) (:max-loops gc))))
(defn mac [gc kind] (get-in gc [:macs kind]))

(defn fn-arities [W ref]
  (let [ref (loop [r ref] (if-let [t (get-in W [:aliases r])] (recur t) r))]
    (if (= common-ns (first ref))
      ({"norm" [{:n 1}] "add" [{:n 1} {:n 2}]} (second ref))
      (vec (for [a (get-in W [:fns ref :arities])] {:n (count (:params a)) :variadic (boolean (:rest a))})))))
(defn refs-with-arity [gc n]
  (let [W (world gc)]
    (filterv (fn [r] (some #(and (= n (:n %)) (not (:variadic %))) (fn-arities W r))) (:calls gc))))

(defn receivers [W P recs]
  (let [impl? #(contains? (:impls W) [P %]) dflt (impl? :default)]
    (concat (for [R recs :when (or dflt (impl? R))] [:rec R])
            (when (or dflt (impl? :number)) [:int])
            (when (or dflt (impl? :string)) [:str])
            (when dflt [:vec]))))
(defn proto-options* [gc]
  (let [W (world gc)]
    (vec (for [P (get-in gc [:vis :protos])
               rt (receivers W P (rec-types gc))
               {:keys [name arities]} (get-in W [:protocols P :methods])
               a arities]
           [P name a rt]))))
(defn sat-options* [gc]
  (vec (for [P (get-in gc [:vis :protos]) R (rec-types gc)] [P R])))
(defn proto-options [gc] @(:proto-options gc))
(defn sat-options [gc] @(:sat-options gc))
(defn invocable [gc] (filterv #(get-in (world gc) [:records % :invoke]) (rec-types gc)))
(defn tagged-multis [gc] (filterv #(seq (get-in (world gc) [:mm-tags %])) (get-in gc [:vis :multis])))

(declare gx gtest gf1 gf2)

(def tables
  {:int [[:add 6 :any] [:sub 2 :any] [:mul 3 :any] [:count 3 :any] [:sum 3 :any] [:nth 2 :any]
         [:last 1 :any] [:get 3 :any] [:field 2 :rec] [:call 7 :call] [:invoke 2 :iter]
         [:invoke2 2 :iter] [:let 6 :any] [:if 4 :any] [:cond 2 :any] [:case 2 :any]
         [:case-lit 2 :any] [:condp 1 :any] [:cond-> 2 :any] [:some-> 1 :any] [:if-some 1 :any]
         [:when-let 1 :iter] [:loop 2 :iter] [:letfn 1 :iter] [:reduce 4 :iter]
         [:reduce-early 1 :iter] [:reduce-kv 1 :iter] [:transduce 2 :iter] [:for 2 :iter]
         [:watch 1 :iter] [:delay 1 :any] [:binding 2 :dyn] [:try 2 :any] [:proto 6 :proto]
         [:satisfies 1 :sat] [:multi 5 :multi] [:isa 1 :isa] [:acc 2 :acc] [:rinvoke 2 :rinv]
         [:index-of 1 :any] [:compare 2 :any] [:blend 2 :mac] [:twice 1 :mac] [:chain 1 :mac]
         [:argc 1 :mac] [:or-local 1 :mac] [:sum-all 1 :mac] [:doubled 1 :mac] [:meta 1 :any]
         [:set 2 :any] [:setop 1 :any] [:dvec 2 :any] [:dmap 2 :any]]
   :vec [[:vlit 3 :any] [:vrange 2 :any] [:vmap 3 :iter] [:vfor 2 :iter] [:vfilter 3 :iter]
         [:vconj 2 :any] [:vassoc 1 :any] [:vtake 2 :any] [:vdrop 1 :any] [:vsort 2 :any]
         [:vmapcat 1 :iter] [:vkeep 1 :iter] [:vreductions 1 :iter] [:viterate 1 :iter]
         [:vunfold 1 :iter] [:vdistinct 1 :any] [:vinterleave 1 :any] [:vpartition 1 :any]
         [:vindexed 1 :any] [:vtransient 1 :any] [:vvals 1 :any] [:vjuxt 1 :iter]]
   :map [[:mlit 3 :any] [:mzip 2 :any] [:massoc 2 :any] [:mdissoc 1 :any] [:mupdate 2 :any]
         [:mmerge 2 :any] [:mselect 1 :any] [:mfreq 1 :any] [:mgroup 1 :any] [:minto 1 :any]
         [:mupdate-in 1 :any] [:mtransient 1 :any] [:mrec 1 :rec]]
   :str [[:slit 2 :any] [:sstr 3 :any] [:sjoin 2 :any] [:sfn 3 :any] [:sreplace 1 :any]
         [:ssubs 2 :any] [:sname 1 :any]]
   :kw [[:klit 3 :any] [:kpick 2 :any] [:kmake 1 :any]]
   :rec [[:rnew 4 :any] [:rassoc 2 :any] [:rupdate 1 :any]]})

(defn ok? [gc req]
  (case req
    :any true
    :iter (iter? gc)
    :call (and (iter? gc) (seq (:calls gc)) (pos? @(:call-budget gc)))
    :rec (seq (rec-types gc))
    :dyn (seq (get-in gc [:vis :dyns]))
    :proto (and (dispatch? gc) (seq (proto-options gc)))
    :sat (seq (sat-options gc))
    :multi (and (dispatch? gc) (seq (tagged-multis gc)))
    :isa (seq (tagged-multis gc))
    :acc (and (dispatch? gc) (seq (get-in gc [:vis :types])))
    :rinv (and (dispatch? gc) (seq (invocable gc)))
    :mac (some? (:macs gc))))

(defn leaf-fn1 [gc]
  (let [rng (rnd gc) ls (locals gc (fn1-key gc)) refs (when (fref-ok? gc) (refs-with-arity gc 1))]
    (case (weighted rng (cond-> [[:lambda 4] [:norm 1]] (seq ls) (conj [:local 3]) (seq refs) (conj [:fref 3])))
      :lambda (let [p (fresh gc)] {:op :lambda :p p :body (gx (with-local (inner gc) :int p) :int 0)})
      :norm {:op :fref :fn [common-ns "norm"]}
      :local {:op :local :name (pick rng ls)}
      :fref {:op :fref :fn (pick rng refs)})))

(defn gf1 [gc d]
  (let [rng (rnd gc) ls (locals gc (fn1-key gc))
        refs1 (when (fref-ok? gc) (refs-with-arity gc 1))
        refs2 (when (fref-ok? gc) (refs-with-arity gc 2))
        kind (weighted rng (cond-> [[:lambda 6] [:norm 1]]
                             (seq ls) (conj [:local 2])
                             (seq refs1) (conj [:fref 3])
                             (seq refs2) (conj [:partial 2])
                             (pos? d) (into [[:comp 1] [:juxtsum 1]])))]
    (case kind
      :lambda (let [p (fresh gc) self (when (chance rng 0.25) (fresh gc))]
                {:op :lambda :p p :self self :body (gx (with-local (inner gc) :int p) :int d)})
      :norm {:op :fref :fn [common-ns "norm"]}
      :local {:op :local :name (pick rng ls)}
      :fref {:op :fref :fn (pick rng refs1)}
      :partial {:op :partial :fn (pick rng refs2) :a (gx gc :int (max 0 (dec d)))}
      :comp {:op :comp :f (gf1 gc (dec d)) :g (gf1 gc (dec d))}
      :juxtsum {:op :juxtsum :fs [(gf1 gc (dec d)) (gf1 gc (dec d))]})))

(defn gf2 [gc d]
  (let [rng (rnd gc) refs (when (fref-ok? gc) (refs-with-arity gc 2))]
    (case (weighted rng (cond-> [[:lambda2 4] [:add 2]] (seq refs) (conj [:fref 2])))
      :lambda2 (let [a (fresh gc) b (fresh gc)]
                 {:op :lambda2 :ps [a b] :body (gx (-> (inner gc) (with-local :int a) (with-local :int b)) :int d)})
      :add {:op :fref :fn [common-ns "add"]}
      :fref {:op :fref :fn (pick rng refs)})))

(defn leaf [gc t]
  (let [rng (rnd gc) ls (locals gc t)]
    (cond
      (= t :fn1) (leaf-fn1 gc)
      (= t :fn2) (gf2 gc 0)
      (and (seq ls) (chance rng 0.6)) {:op :local :name (pick rng ls)}
      :else
      (case (if (vector? t) :rec t)
        :int (let [r (rint rng 9) dyns (get-in gc [:vis :dyns]) vs (locals gc :vec)
                   ss (locals gc :str) recs (rec-locals gc)]
               (cond (and (= r 0) (seq dyns)) {:op :dyn :var (pick rng dyns)}
                     (and (= r 1) (seq vs)) {:op :count :x {:op :local :name (pick rng vs)}}
                     (and (= r 2) (seq recs)) (let [[nm R] (pick rng recs)]
                                                {:op :field :x {:op :local :name nm} :f (pick rng (fields-of gc R))})
                     (and (= r 3) (seq ss)) {:op :count :x {:op :local :name (pick rng ss)}}
                     :else (lit gc)))
        :vec (if (chance rng 0.6)
               {:op :vlit :xs (vec (repeatedly (between rng 2 4) #(leaf gc :int)))}
               {:op :vrange :n (between rng 3 10)})
        :map {:op :mlit :entries (mapv (fn [k] [k (leaf gc :int)]) (sample rng (between rng 1 4) (:keys gc)))}
        :str {:op :slit :s (raw-word rng)}
        :kw {:op :klit :k (a-key gc)}
        :rec (let [R (second t)] {:op :rnew :r R :args (mapv (fn [_] (leaf gc :int)) (fields-of gc R))})))))

(defn binding-type [gc]
  (let [rng (rnd gc) recs (rec-types gc)]
    (weighted rng (cond-> [[:int 5] [:vec 3] [:map 2] [:str 2] [:kw 1]]
                    (seq recs) (conj [[:rec (pick rng recs)] 2])
                    (iter? gc) (conj [:fn1 2])))))

(defn gtest [gc d]
  (let [rng (rnd gc) e (max 0 (dec d)) I #(gx gc :int e) S #(gx gc :str e)]
    (case (rint rng (if (pos? d) 15 6))
      0 {:t :odd :x (I)} 1 {:t :even :x (I)} 2 {:t :lt :a (I) :b (I)} 3 {:t :eq :a (I) :b (lit gc)}
      4 {:t :zmod :x (I) :k (between rng 2 7)} 5 {:t :pos :x (I)}
      6 {:t :empty :v (gx gc :vec e)} 7 {:t :has :m (gx gc :map e) :k (a-key gc)}
      8 {:t :starts :s (S) :p (pick rng onsets)} 9 {:t :includes :s (S) :p (pick rng vowels)}
      10 {:t :not :x (gtest gc e)} 11 {:t :and :a (gtest gc e) :b (gtest gc e)}
      12 {:t :or :a (gtest gc e) :b (gtest gc e)} 13 {:t :str= :a (S) :b (S)}
      14 {:t :seq :v (gx gc :vec e)})))

(defn xf-step [gc in d]
  (let [rng (rnd gc)]
    (case (rint rng 5)
      0 [:map (gf1 gc (kd gc d))]
      1 (let [p (fresh gc)] [:filter p (gtest (with-local in :int p) (kd gc d))])
      2 (let [p (fresh gc)] [:remove p (gtest (with-local in :int p) (kd gc d))])
      3 [:take (between rng 1 8)]
      4 [:drop (between rng 1 3)])))

(defn gen-op [gc t op d]
  (let [rng (rnd gc) k #(kd gc d)
        I #(gx gc :int (k)) V #(gx gc :vec (k)) M #(gx gc :map (k)) S #(gx gc :str (k))
        K #(gx gc :kw (k)) T #(gtest gc (k)) F1 #(gf1 gc (k)) F2 #(gf2 gc (k))
        in (inner gc) n997 #(rint rng 997)
        names #(vec (repeatedly % (fn [] (fresh gc))))
        ints #(vec (repeatedly % I))
        keys-of #(sample rng % (:keys gc))
        R (when (vector? t) (second t))]
    (case op
      :add {:op :add :xs (ints (between rng 2 4))}
      :sub {:op :sub :a (I) :b (I)}
      :mul {:op :mul :a (I) :b (I)}
      :count {:op :count :x (gx gc (weighted rng (cond-> [[:vec 3] [:map 2] [:str 2]]
                                                   (seq (rec-types gc)) (conj [[:rec (pick rng (rec-types gc))] 1])))
                                 (k))}
      :sum {:op :sum :v (V)}
      :nth {:op :nth :v (V) :i (I) :d (n997)}
      :last {:op :last :v (V) :d (n997)}
      :get {:op :get :m (M) :k (a-key gc) :d (n997)}
      :field (let [R (pick rng (rec-types gc))] {:op :field :x (gx gc [:rec R] (k)) :f (pick rng (fields-of gc R))})
      :call (let [f (pick rng (:calls gc)) ar (pick rng (fn-arities (world gc) f))]
              (swap! (:call-budget gc) dec)
              {:op :call :fn f :args (ints (+ (:n ar) (if (:variadic ar) (rint rng 3) 0)))})
      :invoke {:op :invoke :f (F1) :x (I)}
      :invoke2 {:op :invoke2 :f (F2) :a (I) :b (I)}
      :let (loop [g gc binds [] n (between rng 1 3)]
             (if (zero? n)
               {:op :let :binds binds :body (gx g :int (k))}
               (let [bt (binding-type g) nm (fresh g) e (gx g bt (k))]
                 (recur (with-local g bt nm) (conj binds [nm e]) (dec n)))))
      :if {:op :if :test (T) :then (I) :else (I)}
      :cond {:op :cond :clauses (vec (repeatedly (between rng 2 3) #(vector (T) (I)))) :else (I)}
      :case (let [n (between rng 2 4)] {:op :case :x (I) :k (+ n (rint rng 2)) :branches (ints n) :else (I)})
      :case-lit (let [kind (pick rng [:kw :str]) n (between rng 2 4)
                      lits (if (= kind :kw) (keys-of n) (vec (distinct (repeatedly n #(raw-word rng)))))]
                  {:op :case-lit :kind kind :lits lits :i (I)
                   :branches (ints (if (chance rng 0.5) (count lits) (dec (count lits)))) :else (I)})
      :condp (let [n (between rng 2 3)]
               {:op :condp :pred (pick rng [:lt :gt]) :x (I)
                :ts (vec (sort (take n (distinct (repeatedly #(rint rng modulus))))))
                :es (ints n) :else (I)})
      :cond-> (let [n (between rng 1 3) mul-at (rint rng (* 2 n))]
                {:op :cond-> :x (I)
                 :steps (vec (for [i (range n)]
                               (let [f (if (= i mul-at) :mul (pick rng [:add :sub]))]
                                 {:test (T) :f f :k (if (= f :mul) (between rng 2 31) (n997))})))})
      :some-> {:op :some-> :m (M) :k (a-key gc) :n (n997) :d (n997)}
      :if-some (let [nm (fresh gc)]
                 {:op :if-some :m (M) :k (a-key gc) :name nm :body (gx (with-local gc :int nm) :int (k)) :else (I)})
      :when-let (let [[p nm] (names 2)]
                  {:op :when-let :v (V) :p p :test (gtest (with-local in :int p) (k)) :name nm
                   :body (gx (with-local gc :int nm) :int (k)) :d (n997) :style (rint rng 2)})
      :loop (let [[acc i lim] (names 3)]
              {:op :loop :lim lim :n (I) :init (I) :acc acc :i i
               :body (gx (-> in (with-local :int acc) (with-local :int i)) :int (k))})
      :letfn (let [[f g np vp] (names 4) bg (with-local in :int vp)]
               {:op :letfn :f f :g g :np np :vp vp :n (I) :init (I) :b1 (gx bg :int (k)) :b2 (gx bg :int (k))})
      :reduce (let [[acc x tmp] (names 3)]
                {:op :reduce :v (V) :init (I) :acc acc :x x :tmp tmp :style (rint rng 4)
                 :body (gx (-> in (with-local :int acc) (with-local :int x)) :int (k))})
      :reduce-early (let [[acc x] (names 2)]
                      {:op :reduce-early :v (V) :init (I) :limit (between rng 100 900000) :acc acc :x x
                       :body (gx (-> in (with-local :int acc) (with-local :int x)) :int (k))})
      :reduce-kv (let [[acc kk val] (names 3)]
                   {:op :reduce-kv :m (M) :init (I) :acc acc :k kk :val val
                    :body (gx (-> in (with-local :int acc) (with-local :int val) (with-local :kw kk)) :int (k))})
      :transduce {:op :transduce :v (V) :init (I) :steps (vec (repeatedly (between rng 1 3) #(xf-step gc in d)))}
      :for (let [[x j y] (names 3) two? (chance rng 0.5)
                 g1 (cond-> (with-local in :int x) two? (with-local :int j))
                 g2 (with-local g1 :int y)]
             {:op :for :x x :v (V) :j (when two? j) :n (between rng 1 3) :y y
              :ye (gx g1 :int (k)) :test (gtest g2 (k)) :body (gx g2 :int (k))})
      :watch (let [[evs st acc x] (names 4)]
               {:op :watch :evs evs :st st :wk (a-key gc) :acc acc :x x :v (V) :init (I)
                :body (gx (-> in (with-local :int acc) (with-local :int x)) :int (k))})
      :delay (let [[cnt dl] (names 2)] {:op :delay :cnt cnt :dl dl :e (I) :k (between rng 1 31)})
      :binding {:op :binding :var (pick rng (get-in gc [:vis :dyns])) :val (I) :body (I)}
      :try (let [[hits res e nm] (names 4)]
             {:op :try :hits hits :res res :e e :name nm :ek (a-key gc) :msg (raw-word rng)
              :msglen (chance rng 0.5) :test (T) :v (I) :then (I)
              :cbody (gx (with-local gc :int nm) :int (k))})
      :proto (let [[P m ar rt] (pick rng (proto-options gc))]
               {:op :proto :p P :m m :recv (gx gc rt (k)) :args (ints ar)})
      :satisfies (let [[P R] (pick rng (sat-options gc))]
                   {:op :satisfies :p P :recv (gx gc [:rec R] (k)) :then (I) :else (I)})
      :multi (let [mm (pick rng (tagged-multis gc)) all (get-in (world gc) [:mm-tags mm])
                   tags (cond-> (sample rng (min (count all) (between rng 2 4)) all)
                          (chance rng 0.3) (conj (a-key gc)))]
               {:op :multi :mm mm :tags tags :i (I) :a (I) :b (I)})
      :isa (let [mm (pick rng (tagged-multis gc)) all (get-in (world gc) [:mm-tags mm])]
             {:op :isa :tags (sample rng (min (count all) (between rng 2 3)) all) :i (I)
              :parent (pick rng all) :then (I) :else (I)})
      :acc (let [[acc x] (names 2)]
             {:op :acc :t (pick rng (get-in gc [:vis :types])) :acc acc :x x :style (rint rng 3)
              :a (I) :b (I) :v (V) :proj (pick rng [:total :step :missing]) :mk (a-key gc) :d (n997)})
      :rinvoke (let [R (pick rng (invocable gc))] {:op :rinvoke :r (gx gc [:rec R] (k)) :x (I)})
      :index-of {:op :index-of :s (S) :sub (pick rng (concat vowels ["k" "r" "n" "la" "or"])) :d (n997)}
      :compare (let [kind (pick rng [:str :kw :int])] {:op :compare :a (gx gc kind (k)) :b (gx gc kind (k))})
      :blend {:op :blend :mac (mac gc :blend) :a (I) :b (I) :k (between rng 2 97)}
      :twice {:op :twice :mac (mac gc :twice) :e (I)}
      :chain {:op :chain :mac (mac gc :chain) :x (I) :steps (ints (between rng 1 3))}
      :argc {:op :argc :mac (mac gc :argc)
             :syms (vec (repeatedly (between rng 0 4)
                                    #(if-let [ls (seq (locals gc :int))] (pick rng ls) (fresh gc))))}
      :or-local (let [ls (locals gc :int) local? (boolean (and (seq ls) (chance rng 0.6)))]
                  {:op :or-local :mac (mac gc :or-local) :sym (if local? (pick rng ls) (fresh gc))
                   :local local? :fallback (n997)})
      :sum-all {:op :sum-all :mac (mac gc :sum-all) :xs (ints (between rng 1 4))}
      :doubled (let [nm (fresh gc)]
                 {:op :doubled :mac (mac gc :doubled) :name nm :init (I) :body (gx (with-local gc :int nm) :int (k))})
      :meta {:op :meta :tmp (fresh gc) :v (V) :mk (a-key gc) :tag (n997)}
      :set {:op :set :style (rint rng 2) :v (V) :a (I) :b (I)}
      :setop {:op :setop :kind (pick rng [:union :intersection :difference]) :v (V) :w (V)}
      :dvec (let [[p q more] (names 3)]
              {:op :dvec :v (V) :p p :q q :more more :d1 (n997) :d2 (n997)
               :body (gx (-> gc (with-local :int p) (with-local :int q)) :int (k))})
      :dmap (let [keys? (chance rng 0.5) ks (keys-of (between rng 1 3))
                  nms (if keys? ks (names (count ks))) as (when (chance rng 0.4) (fresh gc))]
              {:op :dmap :m (M) :style (if keys? :keys :explicit)
               :pairs (mapv (fn [nm kk] [nm kk (n997)]) nms ks) :as as
               :body (gx (reduce #(with-local %1 :int %2) gc nms) :int (k))})
      ;; vectors
      :vlit {:op :vlit :xs (ints (between rng 2 5))}
      :vrange {:op :vrange :n (between rng 3 10)}
      :vmap {:op :vmap :v (V) :f (F1)}
      :vfor (let [x (fresh gc)] {:op :vfor :x x :v (V) :body (gx (with-local in :int x) :int (k))})
      :vfilter (let [p (fresh gc)] {:op :vfilter :v (V) :p p :test (gtest (with-local in :int p) (k))})
      :vconj {:op :vconj :v (V) :xs (ints (between rng 1 3))}
      :vassoc {:op :vassoc :tmp (fresh gc) :v (V) :i (I) :x (I)}
      :vtake {:op :vtake :v (V) :n (between rng 1 8)}
      :vdrop {:op :vdrop :v (V) :n (between rng 1 3)}
      :vsort (let [kind (pick rng (if (iter? gc) [:asc :desc :by :by-desc] [:asc :desc]))]
               (cond-> {:op :vsort :kind kind :v (V)} (#{:by :by-desc} kind) (assoc :f (F1))))
      :vmapcat (let [p (fresh gc) g (with-local in :int p)]
                 {:op :vmapcat :v (V) :p p :a (gx g :int (k)) :b (gx g :int (k))})
      :vkeep (let [p (fresh gc) g (with-local in :int p)]
               {:op :vkeep :v (V) :p p :test (gtest g (k)) :body (gx g :int (k))})
      :vreductions {:op :vreductions :v (V) :init (I) :f (F2)}
      :viterate {:op :viterate :f (F1) :x (I) :n (between rng 2 8)}
      :vunfold {:op :vunfold :f (F1) :x (I) :n (between rng 2 8)}
      :vdistinct {:op :vdistinct :v (V)}
      :vinterleave {:op :vinterleave :v (V) :w (V)}
      :vpartition {:op :vpartition :part (fresh gc) :v (V) :n (between rng 2 4)}
      :vindexed (let [[i x] (names 2)] {:op :vindexed :i i :x x :v (V) :k (between rng 1 97)})
      :vtransient {:op :vtransient :v (V) :xs (ints (between rng 1 2))}
      :vvals {:op :vvals :m (M)}
      :vjuxt {:op :vjuxt :fs (vec (repeatedly (between rng 2 3) F1)) :x (I)}
      ;; maps with keyword keys
      :mlit {:op :mlit :entries (mapv (fn [kk] [kk (I)]) (keys-of (if (chance rng 0.15) (between rng 9 12) (between rng 1 5))))}
      :mzip {:op :mzip :ks (keys-of (between rng 2 12)) :v (V)}
      :massoc {:op :massoc :m (M) :k (a-key gc) :x (I)}
      :mdissoc {:op :mdissoc :m (M) :k (a-key gc)}
      :mupdate {:op :mupdate :m (M) :k (a-key gc) :x (I)}
      :mmerge {:op :mmerge :with (chance rng 0.6) :a (M) :b (M)}
      :mselect {:op :mselect :m (M) :ks (keys-of (between rng 1 4))}
      :mfreq {:op :mfreq :p (fresh gc) :v (V) :ks (keys-of (between rng 2 4))}
      :mgroup (let [[p acc kk xs] (names 4)] {:op :mgroup :p p :acc acc :kk kk :xs xs :v (V) :ks (keys-of (between rng 2 4))})
      :minto {:op :minto :p (fresh gc) :v (V) :ks (keys-of (between rng 2 4))}
      :mupdate-in {:op :mupdate-in :m (M) :k1 (a-key gc) :k2 (a-key gc) :x (I)}
      :mtransient {:op :mtransient :m (M) :k (a-key gc) :x (I)}
      :mrec (let [R (pick rng (rec-types gc))] {:op :mrec :r (gx gc [:rec R] (k))})
      ;; strings and keywords
      :slit {:op :slit :s (raw-word rng)}
      :sstr {:op :sstr :parts (vec (repeatedly (between rng 2 3) #(gx gc (pick rng [:str :str :int :kw]) (min 1 (k)))))}
      :sjoin {:op :sjoin :sep (pick rng ["-" "." ":" "_" " " ", "])
              :parts (vec (repeatedly (between rng 2 3) #(gx gc :str (min 1 (k)))))}
      :sfn {:op :sfn :f (pick rng [:upper :lower :cap :rev :trim]) :s (S)}
      :sreplace {:op :sreplace :s (S) :from (pick rng ["a" "e" "o" "u" "i"]) :to (pick rng ["" "y" "oo" "ae"])}
      :ssubs {:op :ssubs :tmp (fresh gc) :s (S) :n (between rng 1 6)}
      :sname {:op :sname :k (K)}
      :klit {:op :klit :k (a-key gc)}
      :kpick {:op :kpick :ks (keys-of (between rng 2 4)) :i (I)}
      :kmake {:op :kmake :s (S)}
      ;; records
      :rnew {:op :rnew :r R :args (mapv (fn [_] (I)) (fields-of gc R))}
      :rassoc {:op :rassoc :x (gx gc t (k)) :k (if (chance rng 0.6) (pick rng (fields-of gc R)) (a-key gc)) :v (I)}
      :rupdate {:op :rupdate :x (gx gc t (k)) :f (pick rng (fields-of gc R)) :v (I)})))

(defn gx [gc t d]
  (swap! (:budget gc) dec)
  (cond
    (= t :fn1) (gf1 gc (max 0 d))
    (= t :fn2) (gf2 gc (max 0 d))
    (or (<= d 0) (neg? @(:budget gc))) (leaf gc t)
    :else (let [table (get (:tables gc) (if (vector? t) :rec t))
                choices (filterv (fn [[_ _ req]] (ok? gc req)) table)]
            (if (seq choices)
              (gen-op gc t (weighted (rnd gc) (map (fn [[op w]] [op w]) choices)) d)
              (leaf gc t)))))

;;; Rendering an expression node to code. Equivalent spellings are chosen
;;; here; the oracle never sees them.

(defn gref [rc [ns n]]
  (if (= ns (:ns rc)) (symbol n)
      (symbol (or (get (:alias rc) ns) (throw (ex-info "Missing alias" {:ns ns :in (:ns rc)}))) n)))
(defn C [rc n] (gref rc [common-ns n]))
(defn norm-of [rc x] (L (C rc "norm") x))
(defn tagf [rc tag]
  (if-let [[_ ns n] (re-matches #"(.+)/(.+)" tag)]
    (cond (= ns (:ns rc)) (raw (str "::" n))
          (and (get (:alias rc) ns) (zero? (rint (:rng rc) 2))) (raw (str "::" (get (:alias rc) ns) "/" n))
          :else (keyword ns n))
    (keyword tag)))
(defn has-short? [form] (some #(instance? Short %) (rest (form-seq form))))
(defn uses? [form s] (some #(= s %) (form-seq form)))
(defn lam [rc params body self]
  (let [ps (mapv symbol params) st (rint (:rng rc) 4)]
    (cond
      (and (zero? st) (seq? body) (not (has-short? body)) (not (instance? Short body))
           (uses? body (peek ps)))
      (let [m (zipmap ps (if (= 1 (count ps)) ['%] ['%1 '%2]))] (->Short (form-walk #(get m % %) body)))
      (and (= st 1) self) (L 'fn (symbol self) ps body)
      :else (L 'fn ps body))))

(declare rf)
(defn rt [rc t]
  (let [r #(rf rc %) tt #(rt rc %)]
    (case (:t t)
      :odd (L 'odd? (r (:x t))) :even (L 'even? (r (:x t))) :lt (L '< (r (:a t)) (r (:b t)))
      :eq (L '= (r (:a t)) (r (:b t))) :zmod (L 'zero? (L 'mod (r (:x t)) (:k t))) :pos (L 'pos? (r (:x t)))
      :empty (L 'empty? (r (:v t))) :has (L 'contains? (r (:m t)) (keyword (:k t)))
      :starts (L 'str/starts-with? (r (:s t)) (:p t)) :includes (L 'str/includes? (r (:s t)) (:p t))
      :not (L 'not (tt (:x t))) :and (L 'and (tt (:a t)) (tt (:b t))) :or (L 'or (tt (:a t)) (tt (:b t)))
      :str= (L '= (r (:a t)) (r (:b t))) :seq (L 'seq (r (:v t))))))

(defn xf-form [rc [kind a b]]
  (case kind
    :map (L 'map (rf rc a))
    :filter (L 'filter (lam rc [a] (rt rc b) nil))
    :remove (L 'remove (lam rc [a] (rt rc b) nil))
    :take (L 'take a)
    :drop (L 'drop a)))

(defn rf [rc n]
  (let [r #(rf rc %) t #(rt rc %) s #(rint (:rng rc) %) y symbol
        add (C rc "add")]
    (case (:op n)
      :lit (:v n)
      :local (y (:name n))
      :dyn (gref rc (:var n))
      :add (let [xs (mapv r (:xs n))]
             (case (s 3)
               0 (norm-of rc (apply list '+ xs))
               1 (if (= 2 (count xs)) (apply list add xs) (L 'reduce add xs))
               2 (L '-> (first xs) (apply list '+ (rest xs)) (C rc "norm"))))
      :sub (norm-of rc (L '- (r (:a n)) (r (:b n))))
      :mul (if (zero? (s 2)) (norm-of rc (L '* (r (:a n)) (r (:b n))))
               (L '-> (r (:a n)) (L '* (r (:b n))) (C rc "norm")))
      :count (L 'count (r (:x n)))
      :sum (case (s 3) 0 (norm-of rc (L 'reduce '+ 0 (r (:v n)))) 1 (norm-of rc (L 'apply '+ (r (:v n))))
                 2 (L 'reduce add 0 (r (:v n))))
      :nth (L (pick (:rng rc) '[nth get]) (r (:v n)) (L 'mod (r (:i n)) 6) (:d n))
      :last (L 'or (L (pick (:rng rc) '[peek last]) (r (:v n))) (:d n))
      :get (let [m (r (:m n)) kk (keyword (:k n))]
             (case (s 3) 0 (L 'get m kk (:d n)) 1 (L kk m (:d n)) 2 (L m kk (:d n))))
      :field (L (keyword (:f n)) (r (:x n)))
      :api-call (let [v 'api-value]
                  (L 'let (bvec [[v (L add (y (:input n)) (r (:x n)))]])
                     (L add v (L (symbol "api" (str (features/call-name (:method n)))) v (:salt n)))))
      :call (apply list (gref rc (:fn n)) (map r (:args n)))
      :invoke (let [f (r (:f n)) x (r (:x n))]
                (case (s 3) 0 (L f x) 1 (L 'apply f [x]) 2 (L (C rc "pipeline") x [f])))
      :invoke2 (let [f (r (:f n)) a (r (:a n)) b (r (:b n))]
                 (case (s 3) 0 (L f a b) 1 (L 'reduce f [a b]) 2 (L 'apply f a [b])))
      :let (L 'let (bvec (map (fn [[nm e]] [(y nm) (r e)]) (:binds n))) (r (:body n)))
      :if (let [c (t (:test n)) a (r (:then n)) b (r (:else n))]
            (case (s 4) 0 (L 'if c a b) 1 (L 'if-not c b a) 2 (L 'cond c a :else b) 3 (L 'or (L 'when c a) b)))
      :cond (apply list 'cond (concat (mapcat (fn [[c e]] [(t c) (r e)]) (:clauses n)) [:else (r (:else n))]))
      :case (apply list 'case (L 'mod (r (:x n)) (:k n))
                   (concat (mapcat (fn [i e] [i (r e)]) (range) (:branches n)) [(r (:else n))]))
      :case-lit (let [lits (mapv (if (= :kw (:kind n)) keyword identity) (:lits n))]
                  (apply list 'case (L 'nth lits (L 'mod (r (:i n)) (count lits)))
                         (concat (mapcat (fn [l e] [l (r e)]) lits (:branches n)) [(r (:else n))])))
      :condp (apply list 'condp (if (= :lt (:pred n)) '< '>) (r (:x n))
                    (concat (mapcat (fn [th e] [th (r e)]) (:ts n) (:es n)) [(r (:else n))]))
      :cond-> (norm-of rc (apply list 'cond-> (r (:x n))
                                 (mapcat (fn [{:keys [test f k]}] [(t test) (L ({:add '+ :sub '- :mul '*} f) k)]) (:steps n))))
      :some-> (norm-of rc (L 'or (L 'some-> (r (:m n)) (keyword (:k n)) (L '+ (:n n))) (:d n)))
      :if-some (L 'if-some (bvec [[(y (:name n)) (L 'get (r (:m n)) (keyword (:k n)))]]) (r (:body n)) (r (:else n)))
      :when-let (let [found (L 'first (L 'filter (lam rc [(:p n)] (t (:test n)) nil) (r (:v n))))
                      bv (bvec [[(y (:name n)) found]])]
                  (if (zero? (:style n))
                    (L 'or (L 'when-let bv (r (:body n))) (:d n))
                    (L 'if-let bv (r (:body n)) (:d n))))
      :loop (let [[acc i lim] (map (comp y n) [:acc :i :lim])]
              (L 'let (bvec [[lim (L '+ 2 (L 'mod (r (:n n)) 5))]])
                 (L 'loop (bvec [[i 0] [acc (r (:init n))]])
                    (L 'if (L '< i lim) (L 'recur (L 'inc i) (r (:body n))) acc))))
      :letfn (let [[f g np vp] (map (comp y n) [:f :g :np :vp])]
               (L 'letfn [(L f [np vp] (L 'if (L 'zero? np) vp (L g (L 'dec np) (r (:b1 n)))))
                          (L g [np vp] (L 'if (L 'zero? np) vp (L f (L 'dec np) (r (:b2 n)))))]
                  (L f (L 'mod (r (:n n)) 5) (r (:init n)))))
      :reduce (let [[acc x tmp] (map (comp y n) [:acc :x :tmp]) v (r (:v n)) init (r (:init n)) body (r (:body n))]
                (case (:style n)
                  0 (L 'reduce (lam rc [(:acc n) (:x n)] body nil) init v)
                  1 (L 'let (bvec [[tmp (L 'atom init)]])
                       (L 'doseq (bvec [[x v]]) (L 'swap! tmp (L 'fn [acc] body))) (at tmp))
                  2 (L 'let (bvec [[tmp (L 'volatile! init)]])
                       (L 'run! (L 'fn [x] (L 'vswap! tmp (L 'fn [acc] body))) v) (at tmp))
                  3 (L 'loop (bvec [[acc init] [tmp (L 'seq v)]])
                       (L 'if tmp (L 'let (bvec [[x (L 'first tmp)]]) (L 'recur body (L 'next tmp))) acc))))
      :reduce-early (let [[acc x] (map (comp y n) [:acc :x])]
                      (L 'reduce (L 'fn [acc x] (L 'if (L '> acc (:limit n)) (L 'reduced acc) (r (:body n))))
                         (r (:init n)) (r (:v n))))
      :reduce-kv (L 'reduce-kv (L 'fn (mapv (comp y n) [:acc :k :val]) (r (:body n)))
                    (r (:init n)) (L 'into (L 'sorted-map) (r (:m n))))
      :transduce (let [steps (mapv #(xf-form rc %) (:steps n))
                       xf (if (= 1 (count steps)) (first steps) (apply list 'comp steps))
                       v (r (:v n)) init (r (:init n))]
                   (norm-of rc (case (s 3)
                                 0 (L 'transduce xf '+ init v)
                                 1 (L 'reduce '+ init (L 'sequence xf v))
                                 2 (L 'reduce '+ init (L 'into [] xf v)))))
      :for (L 'reduce add 0
              (L 'for (bvec (concat [[(y (:x n)) (r (:v n))]]
                                    (when (:j n) [[(y (:j n)) (L 'range (:n n))]])
                                    [[:let (bvec [[(y (:y n)) (r (:ye n))]])] [:when (t (:test n))]]))
                 (r (:body n))))
      :watch (let [[evs st acc x] (map (comp y n) [:evs :st :acc :x])]
               (L 'let (bvec [[evs (L 'atom 0)] [st (L 'atom (r (:init n)))]])
                  (L 'add-watch st (keyword (:wk n)) (L 'fn '[_ _ _ _] (L 'swap! evs 'inc)))
                  (L 'doseq (bvec [[x (r (:v n))]]) (L 'swap! st (L 'fn [acc] (r (:body n)))))
                  (L 'remove-watch st (keyword (:wk n)))
                  (L add (at st) (at evs))))
      :delay (let [[cnt dl] (map (comp y n) [:cnt :dl])]
               (L 'let (bvec [[cnt (L 'volatile! 0)] [dl (L 'delay (L 'vswap! cnt 'inc) (r (:e n)))]])
                  (norm-of rc (L '+ (at dl) (at dl) (L '* (:k n) (at cnt))))))
      :binding (L 'binding (bvec [[(gref rc (:var n)) (r (:val n))]]) (r (:body n)))
      :try (let [[hits res e nm] (map (comp y n) [:hits :res :e :name]) ek (keyword (:ek n)) cb (r (:cbody n))]
             (L 'let (bvec [[hits (L 'volatile! 0)]
                            [res (L 'try
                                    (L 'if (t (:test n)) (L 'throw (L 'ex-info (:msg n) (mapf [[ek (r (:v n))]]))) (r (:then n)))
                                    (L 'catch 'Exception e
                                       (L 'let (bvec [[nm (L ek (L 'ex-data e))]])
                                          (if (:msglen n) (L add cb (L 'count (L 'ex-message e))) cb)))
                                    (L 'finally (L 'vswap! hits 'inc)))]])
                (L add res (at hits))))
      :proto (apply list (gref rc [(first (:p n)) (:m n)]) (r (:recv n)) (map r (:args n)))
      :satisfies (L 'if (L 'satisfies? (gref rc (:p n)) (r (:recv n))) (r (:then n)) (r (:else n)))
      :multi (L (gref rc (:mm n)) (L 'nth (mapv #(tagf rc %) (:tags n)) (L 'mod (r (:i n)) (count (:tags n))))
                (r (:a n)) (r (:b n)))
      :isa (L 'if (L 'isa? (L 'nth (mapv #(tagf rc %) (:tags n)) (L 'mod (r (:i n)) (count (:tags n))))
                     (tagf rc (:parent n)))
              (r (:then n)) (r (:else n)))
      :acc (let [{:keys [kt ks]} (get-in (:W rc) [:types (:t n)])
                 [acc x] (map (comp y n) [:acc :x])
                 ctor (L (gref rc [(first (:t n)) (str "->" (second (:t n)))]) (L 'volatile! (r (:a n))) (r (:b n)))
                 v (r (:v n))
                 advance (C rc "advance")
                 proj (L (C rc "read-acc") acc
                         (keyword (case (:proj n) :total kt :step ks :missing (:mk n))) (:d n))]
             (case (:style n)
               0 (L 'let (bvec [[acc ctor]]) (L 'doseq (bvec [[x v]]) (L advance acc x)) proj)
               1 (L 'let (bvec [[acc ctor]]) (L 'run! (L 'partial advance acc) v) proj)
               2 (L 'let (bvec [[acc (L 'reduce advance ctor v)]]) proj)))
      :rinvoke (L (C rc "call-record") (r (:r n)) (r (:x n)))
      :index-of (L 'or (L 'str/index-of (r (:s n)) (:sub n)) (:d n))
      :compare (L '+ 2 (L 'compare (L 'compare (r (:a n)) (r (:b n))) 0))
      :blend (L (gref rc (:mac n)) [(r (:a n)) (r (:b n))] (:k n))
      :twice (L (gref rc (:mac n)) (r (:e n)))
      :chain (apply list (gref rc (:mac n)) (r (:x n)) (map r (:steps n)))
      :argc (apply list (gref rc (:mac n)) (map y (:syms n)))
      :or-local (L (gref rc (:mac n)) (y (:sym n)) (:fallback n))
      :sum-all (apply list (gref rc (:mac n)) (map r (:xs n)))
      :doubled (L (gref rc (:mac n)) [(y (:name n)) (r (:init n))] (r (:body n)))
      :meta (let [tmp (y (:tmp n)) mk (keyword (:mk n))]
              (L 'let (bvec [[tmp (L 'with-meta (r (:v n)) (mapf [[mk (:tag n)]]))]])
                 (norm-of rc (L '+ (L 'count tmp) (L mk (L 'meta tmp))))))
      :set (let [v (r (:v n)) a (r (:a n)) b (r (:b n))]
             (L 'reduce add 0 (if (zero? (:style n))
                                (L 'disj (L 'conj (L 'set v) a) b)
                                (L '-> (L 'into (setf []) v) (L 'conj a) (L 'disj b)))))
      :setop (L 'count (L (y "set" (name (:kind n))) (L 'set (r (:v n))) (L 'set (r (:w n)))))
      :dvec (let [[p q more] (map (comp y n) [:p :q :more])]
              (L 'let (bvec [[[p q '& more] (r (:v n))] [p (L 'or p (:d1 n))] [q (L 'or q (:d2 n))]])
                 (L add (r (:body n)) (L 'count more))))
      :dmap (let [{:keys [pairs as]} n
                  pat (mapf (concat (if (= :keys (:style n))
                                      [[:keys (mapv (comp y first) pairs)]]
                                      (map (fn [[nm kk]] [(y nm) (keyword kk)]) pairs))
                                    [[:or (mapf (map (fn [[nm _ d]] [(y nm) d]) pairs))]]
                                    (when as [[:as (y as)]])))
                  body (r (:body n))]
              (L 'let (bvec [[pat (r (:m n))]]) (if as (norm-of rc (L '+ body (L 'count (y as)))) body)))
      ;; vectors
      :vlit (mapv r (:xs n))
      :vrange (if (zero? (s 2)) (L 'vec (L 'range (:n n))) (L 'into [] (L 'range (:n n))))
      :vmap (let [f (r (:f n)) v (r (:v n))]
              (case (s 3) 0 (L 'mapv f v) 1 (L 'vec (L 'map f v)) 2 (L 'into [] (L 'map f) v)))
      :vfor (L 'vec (L 'for (bvec [[(y (:x n)) (r (:v n))]]) (r (:body n))))
      :vfilter (let [p (:p n) v (r (:v n)) c (t (:test n))]
                 (case (s 4)
                   0 (L 'filterv (lam rc [p] c nil) v)
                   1 (L 'vec (L 'filter (lam rc [p] c nil) v))
                   2 (L 'into [] (L 'filter (lam rc [p] c nil)) v)
                   3 (L 'vec (L 'remove (lam rc [p] (L 'not c) nil) v))))
      :vconj (apply list 'conj (r (:v n)) (map r (:xs n)))
      :vassoc (let [tmp (y (:tmp n))]
                (L 'let (bvec [[tmp (r (:v n))]])
                   (L 'if (L 'seq tmp) (L 'assoc tmp (L 'mod (r (:i n)) (L 'count tmp)) (r (:x n))) [(r (:x n))])))
      :vtake (if (zero? (s 2)) (L 'vec (L 'take (:n n) (r (:v n)))) (L 'into [] (L 'take (:n n)) (r (:v n))))
      :vdrop (if (zero? (s 2)) (L 'vec (L 'drop (:n n) (r (:v n)))) (L 'into [] (L 'drop (:n n)) (r (:v n))))
      :vsort (let [v (r (:v n))]
               (case (:kind n)
                 :asc (if (zero? (s 2)) (L 'vec (L 'sort v)) (L 'vec (L 'sort '< v)))
                 :desc (L 'vec (L 'sort '> v))
                 :by (L 'vec (L 'sort-by (r (:f n)) v))
                 :by-desc (L 'vec (L 'sort-by (r (:f n)) '> v))))
      :vmapcat (L 'vec (L 'take 32 (L 'mapcat (L 'fn [(y (:p n))] [(r (:a n)) (r (:b n))]) (r (:v n)))))
      :vkeep (L 'vec (L 'keep (lam rc [(:p n)] (L 'when (t (:test n)) (r (:body n))) nil) (r (:v n))))
      :vreductions (L 'vec (L 'reductions (r (:f n)) (r (:init n)) (r (:v n))))
      :viterate (L 'vec (L 'take (:n n) (L 'iterate (r (:f n)) (r (:x n)))))
      :vunfold (L 'vec (L 'take (:n n) (L (C rc "unfold") (r (:f n)) (r (:x n)))))
      :vdistinct (if (zero? (s 2)) (L 'vec (L 'distinct (r (:v n)))) (L 'into [] (L 'distinct) (r (:v n))))
      :vinterleave (L 'vec (L 'take 32 (L 'interleave (r (:v n)) (r (:w n)))))
      :vpartition (let [part (y (:part n))]
                    (L 'mapv (L 'fn [part] (L 'reduce add 0 part)) (L 'partition-all (:n n) (r (:v n)))))
      :vindexed (let [[i x] (map (comp y n) [:i :x])]
                  (L 'vec (L 'map-indexed (L 'fn [i x] (norm-of rc (L '+ x (L '* i (:k n))))) (r (:v n)))))
      :vtransient (L 'persistent! (L 'reduce 'conj! (L 'transient (r (:v n))) (mapv r (:xs n))))
      :vvals (if (zero? (s 2)) (L 'vec (L 'vals (L 'into (L 'sorted-map) (r (:m n)))))
                 (L 'mapv 'val (L 'sort-by 'key (r (:m n)))))
      :vjuxt (L (apply list 'juxt (map r (:fs n))) (r (:x n)))
      ;; maps
      :mlit (mapf (map (fn [[kk e]] [(keyword kk) (r e)]) (:entries n)))
      :mzip (L 'zipmap (mapv keyword (:ks n)) (r (:v n)))
      :massoc (L 'assoc (r (:m n)) (keyword (:k n)) (r (:x n)))
      :mdissoc (L 'dissoc (r (:m n)) (keyword (:k n)))
      :mupdate (L 'update (r (:m n)) (keyword (:k n)) (L 'fnil add 0) (r (:x n)))
      :mmerge (if (:with n) (L 'merge-with add (r (:a n)) (r (:b n))) (L 'merge (r (:a n)) (r (:b n))))
      :mselect (L 'select-keys (r (:m n)) (mapv keyword (:ks n)))
      :mfreq (let [p (y (:p n)) ks (mapv keyword (:ks n))]
               (L 'frequencies (L 'map (L 'fn [p] (L 'nth ks (L 'mod p (count ks)))) (r (:v n)))))
      :mgroup (let [[p acc kk xs] (map (comp y n) [:p :acc :kk :xs]) ks (mapv keyword (:ks n))]
                (L 'reduce-kv (L 'fn [acc kk xs] (L 'assoc acc kk (L 'reduce add 0 xs))) (mapf [])
                   (L 'group-by (L 'fn [p] (L 'nth ks (L 'mod p (count ks)))) (r (:v n)))))
      :minto (let [p (y (:p n)) ks (mapv keyword (:ks n))]
               (L 'into (mapf []) (L 'map (L 'fn [p] [(L 'nth ks (L 'mod p (count ks))) p])) (r (:v n))))
      :mupdate-in (let [k1 (keyword (:k1 n))]
                    (L k1 (L 'update-in (mapf [[k1 (r (:m n))]]) [k1 (keyword (:k2 n))] (L 'fnil add 0) (r (:x n)))))
      :mtransient (L 'persistent! (L 'assoc! (L 'transient (r (:m n))) (keyword (:k n)) (r (:x n))))
      :mrec (L 'into (mapf []) (r (:r n)))
      ;; strings and keywords
      :slit (:s n)
      :sstr (apply list 'str (map r (:parts n)))
      :sjoin (if (zero? (s 2)) (L 'str/join (:sep n) (mapv r (:parts n)))
                 (L 'apply 'str (L 'interpose (:sep n) (mapv r (:parts n)))))
      :sfn (L ({:upper 'str/upper-case :lower 'str/lower-case :cap 'str/capitalize :rev 'str/reverse :trim 'str/trim} (:f n))
              (r (:s n)))
      :sreplace (L 'str/replace (r (:s n)) (:from n) (:to n))
      :ssubs (if (zero? (s 2))
               (let [tmp (y (:tmp n))] (L 'let (bvec [[tmp (r (:s n))]]) (L 'subs tmp 0 (L 'min (:n n) (L 'count tmp)))))
               (L (C rc "clip") (r (:s n)) (:n n)))
      :sname (L 'name (r (:k n)))
      :klit (keyword (:k n))
      :kpick (L 'nth (mapv keyword (:ks n)) (L 'mod (r (:i n)) (count (:ks n))))
      :kmake (L 'keyword (r (:s n)))
      ;; records
      :rnew (let [[ns R] (:r n) args (mapv r (:args n))]
              (case (s 2)
                0 (apply list (gref rc [ns (str "->" R)]) args)
                1 (L (gref rc [ns (str "map->" R)])
                     (mapf (map (fn [f a] [(keyword f) a]) (get-in (:W rc) [:records (:r n) :fields]) args)))))
      :rassoc (L 'assoc (r (:x n)) (keyword (:k n)) (r (:v n)))
      :rupdate (L 'update (r (:x n)) (keyword (:f n)) add (r (:v n)))
      ;; function values
      :lambda (lam rc [(:p n)] (r (:body n)) (:self n))
      :lambda2 (lam rc (:ps n) (r (:body n)) nil)
      :fref (gref rc (:fn n))
      :partial (L 'partial (gref rc (:fn n)) (r (:a n)))
      :comp (L 'comp (r (:f n)) (r (:g n)))
      :juxtsum (L 'comp (L 'partial 'reduce add 0) (apply list 'juxt (map r (:fs n)))))))

;;; Namespace builders

(defn file-entry-of [b role]
  {:path (str "src/" (str/replace (str/replace (:ns b) "." "/") "-" "_") ".clj") :role role :ns (:ns b)
   :source (str (str/join "\n\n" @(:forms b)) "\n")})

(defn builder [S nsname alias-of]
  (let [names (atom #{})]
    (doseq [a (vals alias-of)] (swap! names conj a))
    {:ns nsname :names names :forms (atom []) :lines (atom 0)
     :rc {:rng (:rng S) :ns nsname :alias alias-of :W nil}}))
(defn rc-of [S b] (assoc (:rc b) :W @(:W S)))
(defn emit! [b form]
  (let [t (text form) n (code-lines t)]
    (swap! (:forms b) conj t)
    (swap! (:lines b) + n)
    n))

(defn marker! [S b kind category]
  (let [id (swap! (:marker-id S) inc)
        m (str "GENERATED_WASTE_" (:tag b) "_" (str/upper-case (str/replace (name kind) "-" "_")) id "_END")
        expect (case category
                 (:helper :kernel :record :chain :waste-only :common :idle) :removable
                 (:arity :protocol-impl :extension) :best-effort
                 :defmethod :retained
                 :macro :compile-time
                 :orphan :never-loaded)]
    (swap! (:markers S) conj {:marker m :category category :expect expect :namespace (:ns b)})
    m))

(defn gc-for [S b mode & {:keys [scope calls max-loops budget call-budget]}]
  (let [rng (:rng S)
        gc {:rng rng :W (:W S) :tables (:tables S) :names (:names b) :ns (:ns b) :scope (or scope {}) :loops 0
            :max-loops (or max-loops (if (= mode :kernel) 2 1))
            :calls (vec calls) :dispatch? (not= mode :impl) :vis @(:vis b) :macs (:macs b)
            :keys (:keys b) :budget (atom (or budget 20)) :call-budget (atom (or call-budget 1000))}]
    (assoc gc :proto-options (delay (proto-options* gc)) :sat-options (delay (sat-options* gc)))))

(defn int-body [S b mode depth & opts]
  (let [gc (apply gc-for S b mode opts)]
    (gx gc :int depth)))

(defn sigs-of [rng b methods]
  (for [{:keys [name arities]} methods]
    (apply list (symbol name)
           (for [a arities] (vec (cons 'this (repeatedly a #(symbol (fresh! rng (:names b) :var)))))))))

(defn protocol! [S b waste?]
  (let [rng (:rng S) P (fresh! rng (:names b) :type) ref [(:ns b) P]
        methods (vec (for [_ (range (if waste? 1 (between rng 1 3)))]
                       {:name (fresh! rng (:names b) :var)
                        :arities (if waste? [(rint rng 2)] (vec (sort (sample rng (between rng 1 2) [0 1 2]))))}))]
    (swap! (:W S) assoc-in [:protocols ref] {:methods methods})
    (emit! b (if (and (not waste?) (:macs b) (chance rng 0.4))
               (apply list (gref (:rc b) (get-in b [:macs :defproto])) (symbol P) (sigs-of rng b methods))
               (apply list 'defprotocol (symbol P) (sigs-of rng b methods))))
    ref))

(defn impl-methods! [S b P this-scope extra-scope]
  ;; Generate the model for every method arity in this implementation.
  (let [rng (:rng S) W @(:W S)]
    (into {} (for [{:keys [name arities]} (get-in W [:protocols P :methods])]
               [name (into (sorted-map)
                           (for [a arities]
                             (let [this (if this-scope (fresh! rng (:names b) :var) "_")
                                   params (vec (repeatedly a #(fresh! rng (:names b) :var)))
                                   scope (cond-> (merge-with into {:int params} extra-scope)
                                           this-scope (update this-scope (fnil conj []) this))]
                               [a {:this this :params params
                                   :body (int-body S b :impl (between rng 1 3) :scope scope :budget (between rng 4 12))}])))]))))

(defn method-forms [rc methods grouped?]
  (for [[m ars] methods
        :let [fs (for [[_ {:keys [this params body]}] ars]
                   [(vec (cons (symbol this) (map symbol params))) (rf rc body)])]
        form (if (and grouped? (> (count fs) 1))
               [(apply list (symbol m) (map #(apply list %) fs))]
               (map #(apply list (symbol m) %) fs))]
    form))

(defn record! [S b protos waste-proto]
  (let [rng (:rng S) names (:names b)
        R (fresh! rng names :type) ref [(:ns b) R]
        fields (vec (repeatedly (between rng 1 4) #(fresh! rng names :var)))
        _ (swap! (:W S) assoc-in [:records ref] {:fields fields})
        impls (vec (for [P protos] [P (impl-methods! S b P nil {:int fields})]))
        invoke (when (chance rng 0.4)
                 (let [p (fresh! rng names :var)]
                   {:p p :body (int-body S b :impl (between rng 1 3) :scope {:int (conj fields p)} :budget (between rng 4 12))}))
        wm (when waste-proto (first (get-in @(:W S) [:protocols waste-proto :methods])))]
    (swap! (:W S) (fn [W] (reduce (fn [W [P ms]] (assoc-in W [:impls [P ref]] {:inline true :methods ms}))
                                  (cond-> W invoke (assoc-in [:records ref :invoke] invoke))
                                  impls)))
    (let [rc (rc-of S b)
          body (concat
                (mapcat (fn [[P ms]] (cons (gref rc P) (method-forms rc ms false))) impls)
                (when invoke [(C rc "CallableRecord") (L 'call-record ['_ (symbol (:p invoke))] (rf rc (:body invoke)))])
                (when wm
                  [(gref rc waste-proto)
                   (L (symbol (:name wm)) (vec (cons '_ (repeatedly (first (:arities wm)) #(symbol (fresh! rng names :var)))))
                      (L 'str (marker! S b :pimpl :protocol-impl) (symbol (first fields))))]))]
      (emit! b (apply list (if (and (:macs b) (chance rng 0.3)) (gref rc (get-in b [:macs :defshape])) 'defrecord)
                      (symbol R) (mapv symbol fields) body)))
    ref))

(defn deftype! [S b]
  (let [rng (:rng S) names (:names b) fresh #(fresh! rng names :var)
        T (fresh! rng names :type) ref [(:ns b) T]
        [total step x this k nf cell] (repeatedly 7 fresh)
        [kt ks] [(fresh) (fresh)]
        body (int-body S b :impl (between rng 1 3) :scope {:int [total step x]} :max-loops 0 :budget (between rng 4 10))
        y symbol]
    (swap! (:W S) assoc-in [:types ref] {:total total :step step :kt kt :ks ks :x x :body body})
    (emit! b (L 'deftype (y T) [(y cell) (y step)]
                (C (rc-of S b) "Accumulator")
                (L 'advance [(y this) (y x)]
                   (L 'vswap! (y cell) (L 'fn [(y total)] (rf (rc-of S b) body))) (y this))
                (L 'read-acc ['_ (y k) (y nf)]
                   (L 'case (y k) (keyword kt) (at cell) (keyword ks) (y step) (y nf)))))
    ref))

(defn multi! [S b]
  (let [rng (:rng S) names (:names b) mm (fresh! rng names :var) ref [(:ns b) mm]
        tags (vec (repeatedly (between rng 2 4) #(str (:ns b) "/" (fresh! rng names :var))))
        parents (into {} (for [i (range 1 (count tags)) :when (chance rng 0.5)] [(tags i) (tags (rint rng i))]))
        dispatch (fresh! rng names :var)]
    (swap! (:W S) #(-> % (assoc-in [:mm-tags ref] tags) (update :parents merge parents)))
    (let [with-methods (conj (sample rng (between rng 1 (count tags)) tags) "default")
          methods (into {} (for [tag with-methods]
                             (let [a (fresh! rng names :var) bb (fresh! rng names :var)]
                               [tag {:a a :b bb :body (int-body S b :impl (between rng 1 3) :scope {:int [a bb]} :budget (between rng 4 12))}])))
          rc (rc-of S b)]
      (swap! (:W S) assoc-in [:methods ref] methods)
      (emit! b (if (and (:macs b) (chance rng 0.4))
                 (L (gref rc (get-in b [:macs :defdispatch])) (symbol mm))
                 (L 'defmulti (symbol mm) (L 'fn [(symbol dispatch) '_ '_] (symbol dispatch)))))
      (doseq [[c p] (sort parents)] (emit! b (L 'derive (tagf rc c) (tagf rc p))))
      (doseq [tag with-methods :let [m (methods tag)]]
        (emit! b (L 'defmethod (symbol mm) (if (= tag "default") :default (tagf rc tag)) ['_ (symbol (:a m)) (symbol (:b m))]
                    (rf rc (:body m))))))
    ref))

(def extension-class {:number 'Number :string 'String :default 'Object})

(defn extend-deps! [S b dep-infos own-protos]
  (let [rng (:rng S) names (:names b)]
    ;; Extend dependency protocols to built-in types.
    (doseq [P (mapcat :protos dep-infos) :when (chance rng 0.3)]
      (let [W @(:W S)
            free (filterv #(not (contains? (:impls W) [P %])) [:number :string :default])]
        (when (seq free)
          (let [targets (sample rng (between rng 1 (min 2 (count free))) free)
                impls (vec (for [tg targets]
                             [tg (impl-methods! S b P (case tg :number :int :string :str nil) {})]))]
            (swap! (:W S) (fn [W] (reduce (fn [W [tg ms]] (assoc-in W [:impls [P tg]] {:inline false :methods ms})) W impls)))
            (let [rc (rc-of S b)]
              (emit! b (if (and (= 1 (count impls)) (chance rng 0.5))
                         (let [[[tg ms]] impls]
                           (apply list 'extend-type (extension-class tg) (gref rc P) (method-forms rc ms true)))
                         (apply list 'extend-protocol (gref rc P)
                                (mapcat (fn [[tg ms]] (cons (extension-class tg) (method-forms rc ms true))) impls)))))))))
    ;; Implement a visible protocol on a dependency's record.
    (doseq [R (mapcat :records dep-infos) :when (chance rng 0.3)]
      (let [W @(:W S)
            free (filterv #(not (contains? (:impls W) [% R])) (concat own-protos (mapcat :protos dep-infos)))]
        (when (seq free)
          (let [P (pick rng free) ms (impl-methods! S b P [:rec R] {})]
            (swap! (:W S) assoc-in [:impls [P R]] {:inline false :methods ms})
            (let [rc (rc-of S b)]
              (emit! b (apply list 'extend-type (symbol (str/join "." R)) (gref rc P) (method-forms rc ms true))))))))
    ;; Add methods and derived dispatch values to dependency multimethods.
    (doseq [mm (mapcat :multis dep-infos) :when (chance rng 0.4)]
      (let [W @(:W S) tags (get-in W [:mm-tags mm])
            missing (filterv #(not (contains? (get-in W [:methods mm]) %)) tags)
            derive? (or (empty? missing) (chance rng 0.5))
            tag (if derive? (str (:ns b) "/" (fresh! rng names :var)) (pick rng missing))
            parent (when derive? (pick rng tags))
            method? (or (not derive?) (chance rng 0.6))]
        (when derive?
          (swap! (:W S) #(-> % (update-in [:mm-tags mm] conj tag) (assoc-in [:parents tag] parent)))
          (emit! b (L 'derive (tagf (rc-of S b) tag) (tagf (rc-of S b) parent))))
        (when method?
          (let [a (fresh! rng names :var) bb (fresh! rng names :var)
                body (int-body S b :impl (between rng 1 3) :scope {:int [a bb]} :budget (between rng 4 12))]
            (swap! (:W S) assoc-in [:methods mm tag] {:a a :b bb :body body})
            (let [rc (rc-of S b)]
              (emit! b (L 'defmethod (gref rc mm) (tagf rc tag) ['_ (symbol a) (symbol bb)] (rf rc body))))))))))

(defn fn-form [rc nm arities & {:keys [private macro waste-arity]}]
  (let [y symbol
        ar-form (fn [{:keys [params rest rv body]}]
                  [(vec (concat (map y params) (when rest ['& (y rest)])))
                   (if rest (L 'let (bvec [[(y rv) (L 'vec (y rest))]]) (rf rc body)) (rf rc body))])
        fs (cond-> (mapv ar-form arities) waste-arity (conj waste-arity))]
    (cond
      macro (apply list (gref rc macro) (y nm) (first fs))
      (= 1 (count fs)) (apply list (if private 'defn- 'defn) (y nm) (first fs))
      :else (apply list (if private 'defn- 'defn) (y nm) (map #(apply list %) fs)))))

(defn helpers! [S b]
  (let [rng (:rng S) names (:names b)
        hs (vec (repeatedly (between rng 2 4) #(fresh! rng names :var)))
        refs (mapv #(vector (:ns b) %) hs)
        params (mapv (fn [_] (vec (repeatedly (between rng 1 2) #(fresh! rng names :var)))) hs)]
    (doseq [i (range (count hs))]
      (swap! (:W S) assoc-in [:fns (refs i)] {:arities [{:params (params i) :body nil}]}))
    (doseq [i (reverse (range (count hs)))]
      (let [body (int-body S b :helper (between rng 2 3) :scope {:int (params i)} :calls (subvec refs (inc i))
                           :call-budget 1 :budget (between rng 6 14))]
        (swap! (:W S) assoc-in [:fns (refs i) :arities 0 :body] body)))
    (let [W @(:W S)
          called (set (for [r refs n (node-seq (get-in W [:fns r])) :when (= :call (:op n))] (:fn n)))
          forward (filterv called (rest refs))]
      (when (seq forward) (emit! b (apply list 'declare (map (comp symbol second) forward))))
      (doseq [r refs] (emit! b (fn-form (rc-of S b) (second r) (get-in W [:fns r :arities]) :private true))))
    refs))

(defn api! [S b helpers]
  (let [rng (:rng S) names (:names b) fresh #(fresh! rng names :var)]
    (vec
     (for [_ (range (between rng 2 4))]
       (let [nm (fresh) ref [(:ns b) nm]
             shape (weighted rng (cond-> [[:single 4] [:multi 3] [:variadic 1]] (:macs b) (conj [:calc 2])))
             body #(int-body S b :api (between rng 2 4) :scope %1 :calls helpers :call-budget 2 :budget (between rng 10 24))
             arities (case shape
                       (:single :calc) (let [ps (vec (repeatedly (between rng 1 3) fresh))] [{:params ps :body (body {:int ps})}])
                       :multi (let [ps (vec (repeatedly (between rng 1 2) fresh)) extra (fresh)]
                                [{:params ps :body {:op :call :fn ref :args (conj (mapv #(hash-map :op :local :name %) ps) {:op :lit :v (rint rng 997)})}}
                                 {:params (conj ps extra) :body (body {:int (conj ps extra)})}])
                       :variadic (let [ps (vec (repeatedly (between rng 1 2) fresh)) r (fresh) rv (fresh)]
                                   [{:params ps :rest r :rv rv :body (body {:int ps :vec [rv]})}]))
             macro (when (= shape :calc) (get-in b [:macs :defcalc]))
             waste-arity (when (and ((:enabled S) "load/unused") (= shape :multi) (chance rng 0.5))
                           (let [ps (repeatedly (+ 2 (count (:params (peek arities)))) #(symbol (fresh)))]
                             [(vec ps) (apply list 'str (marker! S b :arity :arity) ps)]))]
         (swap! (:W S) assoc-in [:fns ref] (cond-> {:arities arities} macro (assoc :macro macro)))
         (emit! b (fn-form (rc-of S b) nm arities :macro macro :waste-arity waste-arity))
         ref)))))

(defn waste-kernel! [S b calls]
  (let [rng (:rng S) nm (fresh! rng (:names b) :var) x (fresh! rng (:names b) :var)
        body (int-body S b :kernel (between rng 3 5) :scope {:int [x]} :calls calls :budget (between rng 15 40))]
    (swap! (:waste-trees S) conj body)
    (emit! b (L 'defn (symbol nm) [(symbol x)] (L 'if (L '= (symbol x) -1) (marker! S b :kernel :kernel) (rf (rc-of S b) body))))))

(defn unused-defs! [S b api calls dep-infos multi]
  (let [rng (:rng S) W (:W S) nsname (:ns b) y symbol
        tbl (fresh! rng (:names b) :var)
        _ (emit! b (L 'def (y tbl) (mapf [[:marker (marker! S b :table :helper)] [:rows [[1 2 3] [4 5 (rint rng 997)]]]])))
        _ (let [x (y (fresh! rng (:names b) :var))]
            (emit! b (L 'defn (y (fresh! rng (:names b) :var)) [x] (L 'str (marker! S b :helper :helper) (L :rows (y tbl)) x))))
        _ (when ((:enabled S) "dispatch/protocols")
            (let [R (fresh! rng (:names b) :type) f (y (fresh! rng (:names b) :var)) z (y (fresh! rng (:names b) :var))]
              (emit! b (L 'defrecord (y R) [f] (C (:rc b) "CallableRecord")
                          (L 'call-record ['_ z] (L 'str (marker! S b :record :record) f z))))))
        wo (let [nm (fresh! rng (:names b) :var) x (fresh! rng (:names b) :var)
                 body (int-body S b :kernel (between rng 2 4) :scope {:int [x]} :calls calls :budget (between rng 10 25))]
             (swap! (:waste-trees S) conj body)
             (emit! b (L 'defn (y nm) [(y x)] (L 'if (L '= (y x) -1) (marker! S b :wasteonly :waste-only) (rf (rc-of S b) body))))
             nm)
        chain (let [nm (fresh! rng (:names b) :var) x (y (fresh! rng (:names b) :var))
                    fref (when (seq api) (pick rng api))
                    dep-chain (some->> (seq (keep :chain dep-infos)) (pick rng))]
                (emit! b (L 'defn (y nm) [x]
                        (apply list 'str (marker! S b :chain :chain) (L (y wo) x)
                               (concat (when dep-chain [(L (gref (:rc b) dep-chain) x)])
                                       (when fref
                                         [(apply list (gref (:rc b) fref) (repeat (:n (first (fn-arities @W fref))) x))])))))
                [nsname nm])
        _ (when-let [mm (some->> (seq (concat (when multi [multi]) (mapcat :multis dep-infos))) (pick rng))]
            (let [tag (str nsname "/" (fresh! rng (:names b) :var)) a (y (fresh! rng (:names b) :var))]
              (emit! b (L 'defmethod (gref (:rc b) mm) (tagf (:rc b) tag) ['_ a '_] (L 'str (marker! S b :method :defmethod) a)))))]
    chain))

(defn worker-file [b imports methods]
  (let [file (file-entry-of b :worker)
        body (:source file)
        requires (concat (when (str/includes? body "str/") '[[clojure.string :as str]])
                         (when (str/includes? body "set/") '[[clojure.set :as set]])
                         (when (seq methods) '[[workload.api :as api]])
                         (for [ns imports] [(symbol ns) :as (symbol (get-in b [:rc :alias ns]))]))
        header (L 'ns (symbol (:ns b)) (apply list :require requires))]
    (assoc file :source (str (text header) "\n\n" body))))

(defn worker! [S idx dep-ids macro-ns target]
  (let [rng (:rng S) W (:W S)
        nsname (str "workload." (fresh! rng (:global S) :ns))
        dep-infos (mapv #(nth @(:infos S) %) dep-ids)
        enabled (:enabled S)
        protocols? (enabled "dispatch/protocols")
        unused? (enabled "load/unused")
        ;; A worker's ranking is stable across category changes. Sampling its
        ;; methods and salts never advances the Clojure expression generator.
        api-rng (java.util.Random. (.nextLong ^java.util.Random (:api-rng S)))
        method-count (between api-rng 2 4)
        ranked (shuffled api-rng (keys features/calls))
        methods (vec (take method-count (filter enabled ranked)))
        imports (concat [common-ns] (when macro-ns [(:ns macro-ns)]) (map :ns dep-infos))
        tmp (atom #{"api" "str" "set"})
        alias-of (into {} (for [ns imports] [ns (fresh! rng tmp :var)]))
        b (merge (builder S nsname alias-of)
                 {:tag idx :macs (:macs macro-ns) :vis (atom nil)})
        _ (swap! (:names b) into @tmp)
        b (assoc b :keys (vec (repeatedly 12 #(fresh! rng (:names b) :var))))
        y symbol]
    (reset! (:vis b) {:protos (vec (mapcat :protos dep-infos)) :records (vec (mapcat :records dep-infos))
                      :types (vec (mapcat :types dep-infos)) :multis (vec (mapcat :multis dep-infos))
                      :dyns (vec (concat (when (enabled "core/state") [[common-ns "*knob*"]]) (mapcat :dyns dep-infos)))})
    (let [dyn (when (and (enabled "core/state") (chance rng 0.5))
                (let [v (fresh! rng (:names b) :dyn) ref [nsname v] d (rint rng 997)]
                  (swap! W assoc-in [:dyns ref] d)
                  (emit! b (L 'def (raw "^:dynamic") (y v) d))
                  (swap! (:vis b) update :dyns conj ref)
                  ref))
          own-protos (when protocols? (vec (repeatedly (between rng 1 2) #(protocol! S b false))))
          waste-proto (when (and unused? protocols?) (protocol! S b true))
          _ (swap! (:vis b) update :protos into own-protos)
          visible-protos (:protos @(:vis b))
          records (vec (for [i (range (if protocols? (between rng 1 2) 0))]
                         (let [r (record! S b (sample rng (between rng 1 (min 2 (count visible-protos))) visible-protos)
                                          (when (zero? i) waste-proto))]
                           (swap! (:vis b) update :records conj r)
                           r)))
          type (when (and protocols? (chance rng 0.5)) (let [t (deftype! S b)] (swap! (:vis b) update :types conj t) t))
          multi (when (and (enabled "dispatch/multimethods") (or (zero? idx) (chance rng 0.6))) (let [m (multi! S b)] (swap! (:vis b) update :multis conj m) m))
          _ (extend-deps! S b dep-infos own-protos)
          wm (first (get-in @W [:protocols waste-proto :methods]))
          _ (when wm
              (let [ps (repeatedly (first (:arities wm)) #(y (fresh! rng (:names b) :var))) this (y (fresh! rng (:names b) :var))]
                (emit! b (L 'extend-protocol (y (second waste-proto)) 'String
                            (L (y (:name wm)) (vec (cons this ps)) (L 'str (marker! S b :ext :extension) this))))))
          helpers (when (enabled "core/functions") (helpers! S b))
          api (when (enabled "core/functions") (api! S b helpers))
          aliases (vec (for [_ (range (rint rng 3)) :let [deps-api (vec (mapcat :api dep-infos))] :when (seq deps-api)]
                         (let [a (fresh! rng (:names b) :var) target (pick rng deps-api)]
                           (swap! W assoc-in [:aliases [nsname a]] target)
                           (emit! b (L 'def (y a) (gref (:rc b) target)))
                           [nsname a])))
          calls (vec (concat api helpers aliases (mapcat :api dep-infos)))
          kernels (loop [ks []]
                    (if (and (>= (count ks) (max 2 (count methods))) (>= (+ @(:lines b) 40) target))
                      ks
                      (let [nm (fresh! rng (:names b) :var) x (fresh! rng (:names b) :var) ref [nsname nm]
                            macro (when (chance rng 0.15) (get-in b [:macs :defcalc]))
                            expr (int-body S b :kernel (between rng 3 5) :scope {:int [x]} :calls calls :budget (between rng 18 45))
                            body (if (seq methods)
                                   {:op :api-call :method (nth methods (mod (count ks) (count methods)))
                                    :input x :x expr :salt (between api-rng 1 997)}
                                   expr)]
                        (swap! W assoc-in [:fns ref] (cond-> {:arities [{:params [x] :body body}]} macro (assoc :macro macro)))
                        (emit! b (fn-form (rc-of S b) nm [{:params [x] :body body}] :macro macro))
                        (when (and unused? (= 2 (mod (count ks) 3))) (waste-kernel! S b calls))
                        (recur (conj ks ref)))))
          _ (let [x (y (fresh! rng (:names b) :var))]
              (emit! b (L 'defn 'run [x] (L (C (:rc b) "fold") x (mapv (comp y second) kernels)))))
          chain (when unused? (unused-defs! S b api calls dep-infos multi))]
      (swap! W update :workers conj {:ns nsname :kernels kernels :api-methods methods})
      (swap! (:infos S) conj {:ns nsname :protos own-protos :records records :types (if type [type] [])
                              :multis (if multi [multi] []) :dyns (if dyn [dyn] []) :api api :chain chain})
      (worker-file b imports methods))))

;;; Ordinary Clojure macro namespaces loaded through :require.

(def macro-kinds [:blend :twice :chain :argc :or-local :sum-all :doubled :defcalc :defproto :defshape :defdispatch])

(defn macro-ns! [S]
  (let [rng (:rng S) ns (str "workload." (fresh! rng (:global S) :ns)) names (atom #{})
        nm (into {} (for [k (conj macro-kinds :wrap :unused1 :unused2)] [k (fresh! rng names :var)]))
        salt (between rng 1 991) k (between rng 2 31)
        b {:ns ns :tag "M"}
        unused? ((:enabled S) "load/unused")
        src (str
             "(ns " ns ")\n\n"
             "(defn " (nm :wrap) " [form]\n  (list 'workload.common/norm form))\n\n"
             "(defmacro " (nm :blend) " [[a b] k]\n  `(workload.common/norm (+ ~a (* ~b ~k) " salt ")))\n\n"
             "(defmacro " (nm :twice) " [expr]\n  `(let [v# ~expr]\n     (workload.common/norm (+ v# v# " salt "))))\n\n"
             "(defmacro " (nm :chain) " [x & steps]\n  (if (seq steps)\n"
             "    `(" (nm :chain) " (workload.common/norm (+ (* ~x " k ") ~(first steps))) ~@(rest steps))\n    x))\n\n"
             "(defmacro " (nm :argc) " [& _]\n  (+ " salt " (count &form)))\n\n"
             "(defmacro " (nm :or-local) " [sym fallback]\n  (if (contains? &env sym) sym fallback))\n\n"
             "(defmacro " (nm :sum-all) " [& xs]\n  `(workload.common/norm (+ ~@(map (fn [x] `(* " k " ~x)) xs))))\n\n"
             "(defmacro " (nm :doubled) " [[sym init] & body]\n  `(let [~sym (workload.common/norm (* 2 ~init))]\n     ~@body))\n\n"
             "(defmacro " (nm :defcalc) " [name args & body]\n  `(defn ~name ~args ~(" (nm :wrap) " `(+ " salt " (do ~@body)))))\n\n"
             "(defmacro " (nm :defproto) " [pname & sigs]\n  `(defprotocol ~pname ~@sigs))\n\n"
             "(defmacro " (nm :defshape) " [rname fields & impls]\n  `(defrecord ~rname ~fields ~@impls))\n\n"
             "(defmacro " (nm :defdispatch) " [mname]\n  `(defmulti ~mname (fn [tag# & _#] tag#)))\n\n"
             (when unused?
               (str "(defmacro " (nm :unused1) " [x]\n  `(str " (pr-str (marker! S b :macro :macro)) " ~x))\n\n"
                    "(defmacro " (nm :unused2) " [& xs]\n  `(vector " (pr-str (marker! S b :macro :macro)) " ~@xs))\n")))]
    (swap! (:W S) update :macros merge
           (into {} (for [kk macro-kinds] [[ns (nm kk)] {:kind kk :salt salt :k k}])))
    {:ns ns :macs (into {} (for [kk macro-kinds] [kk [ns (nm kk)]]))
     :file {:path (str "src/" (str/replace ns "." "/") ".clj") :role :macro :ns ns :source src}}))

;;; Fixed namespaces

(defn common-file [S]
  (let [rng (:rng S) b (assoc (builder S common-ns {}) :tag "C")]
    (emit! b '(ns workload.common))
    (emit! b (L 'def 'modulus modulus))
    (emit! b '(defn norm [x] (mod x modulus)))
    (when ((:enabled S) "dispatch/protocols")
      (emit! b '(defprotocol Accumulator (advance [acc x]) (read-acc [acc k fallback])))
      (emit! b '(defprotocol CallableRecord (call-record [record x]))))
    (emit! b '(defn add ([a] a) ([a b] (norm (+ a b)))))
    (emit! b '(defn clip [s n] (subs s 0 (min n (count s)))))
    (emit! b '(defn unfold [f x] (lazy-seq (cons x (unfold f (f x))))))
    (emit! b '(defn pipeline [x fs] (reduce (fn [v f] (f v)) x fs)))
    (emit! b '(defn fold [x fs] (reduce (fn [v f] (norm (+ (* 31 v) (f v)))) x fs)))
    (when ((:enabled S) "core/state") (emit! b (L 'def (raw "^:dynamic") '*knob* 7)))
    (dotimes [_ (if ((:enabled S) "load/unused") 2 0)]
      (let [nm (symbol (fresh! rng (:names b) :var))]
        (emit! b (L 'defn nm '[x] (L 'str (marker! S b :common :common) (L 'norm 'x))))))
    (file-entry-of b :support)))

(defn side-ns! [S role]
  ;; Idle namespaces are required but never called; orphans are never required.
  (let [rng (:rng S) ns (str "workload." (fresh! rng (:global S) :ns)) c (fresh! rng (atom #{}) :var)
        b (assoc (builder S ns {common-ns c}) :tag (if (= role :idle) "I" "O"))
        cat (if (= role :idle) :idle :orphan)
        fresh #(symbol (fresh! rng (:names b) :var))
        [tbl f1 f2 m fld x] (repeatedly 6 fresh)
        P (symbol (fresh! rng (:names b) :type)) R (symbol (fresh! rng (:names b) :type))]
    (emit! b (L 'ns (symbol ns) (L :require [(symbol common-ns) :as (symbol c)])))
    (emit! b (L 'def tbl (mapf [[:marker (marker! S b :table cat)] [:values (vec (repeatedly 5 #(rint rng 997)))]])))
    (when ((:enabled S) "dispatch/protocols")
      (emit! b (L 'defprotocol P (L m '[this])))
      (emit! b (L 'defrecord R [fld] P (L m '[_] (L 'str (marker! S b :record cat) fld)))))
    (emit! b (L 'defn f1 [x] (L 'reduce (symbol c "add") x (L :values tbl))))
    (emit! b (L 'defn f2 [x] (apply list 'str (marker! S b :helper cat) (L f1 x)
                                    (when ((:enabled S) "dispatch/protocols") [(L m (L (symbol (str "->" R)) x))]))))
    (file-entry-of b role)))

(defn group-file [S gid worker-nss idle]
  (let [b (assoc (builder S (str "workload.g" gid) {}) :tag "G")
        aliases (map-indexed (fn [i ns] [ns (str "n" i)]) worker-nss)]
    (emit! b (L 'ns (symbol (:ns b))
                (apply list :require [(symbol common-ns) :as 'c]
                       (concat (map (fn [[ns a]] [(symbol ns) :as (symbol a)]) aliases)
                               (when idle [[(symbol idle) :as 'idle]])))))
    (emit! b (L 'defn 'run '[x] (L 'c/pipeline 'x (mapv (fn [[_ a]] (symbol a "run")) aliases))))
    (file-entry-of b :group)))

(defn main-file [S groups]
  (let [b (assoc (builder S "workload.main" {}) :tag "E")
        gs (map #(str "g" %) (range groups))]
    (emit! b (L 'ns 'workload.main
                (apply list :require [(symbol common-ns) :as 'c] (map #(vector (symbol (str "workload." %)) :as (symbol %)) gs))))
    (emit! b (L 'defn 'run '[input iterations]
                (L 'loop '[i 0 v input]
                   (L 'if '(< i iterations)
                      (L 'recur '(inc i) (L 'c/pipeline '(c/norm (+ v i)) (mapv #(symbol % "run") gs)))
                      'v))))
    (emit! b '(defn arguments [args]
                (let [input (parse-long (or (first args) "1"))
                      iterations (parse-long (or (second args) "1"))]
                  (when-not (and (<= (count args) 2) input (<= 0 input 1000002)
                                 iterations (<= 1 iterations 1000000))
                    (throw (ex-info "INPUT must be 0..1000002; ITERATIONS must be 1..1000000" {})))
                  [input iterations])))
    (emit! b '(defn -main [& args] (println (apply run (arguments args)))))
    (file-entry-of b :entry)))

(defn api-file [methods]
  {:path "src/workload/api.clj" :role :api :ns "workload.api"
   :source (str (str/join "\n\n" (map text (features/api-forms methods))) "\n")})

(defn project [{:keys [seed lines namespace-lines categories] :or {categories features/leaves}}]
  (let [enabled (set categories)
        ops (into #{} (mapcat features/expression-groups) categories)
        S {:rng (java.util.Random. seed) :W (atom (new-world)) :global (atom #{"g" "main" "common" "api"})
           :enabled enabled :api-rng (java.util.Random. seed)
           :tables (into {} (for [[t rows] tables] [t (filterv #(ops (first %)) rows)]))
           :infos (atom []) :markers (atom []) :marker-id (atom 0) :waste-trees (atom [])}
        rng (:rng S)
        common (common-file S)]
    (loop [files [common] total (code-lines (:source common)) macro nil]
      (let [n (count @(:infos S))
            groups (quot (+ n 15) 16)
            ;; Group, idle, orphan and entry namespaces are written at the end.
            overhead (+ 26 (* 3 n) (* 40 groups))]
        (if (and (pos? n) (>= (+ total overhead) lines))
          (let [ids (partition-all 16 (map :ns @(:infos S)))
                extra (vec (apply concat
                                  (map-indexed (fn [gid nss]
                                                 (if (enabled "load/unused")
                                                   (let [idle (side-ns! S :idle)]
                                                     [idle (group-file S gid nss (:ns idle)) (side-ns! S :orphan)])
                                                   [(group-file S gid nss nil)]))
                                               ids)))
                api-ids (into (sorted-set) (mapcat :api-methods) (:workers @(:W S)))
                files (cond-> (-> files (into extra) (conj (main-file S (count ids))))
                        (seq api-ids) (conj (api-file api-ids)))
                W @(:W S)]
             {:model (-> W (dissoc :mm-tags) (assoc :format 4 :seed seed :modulus modulus))
             :files files :markers @(:markers S) :waste-trees @(:waste-trees S)})
          (let [new-macro (when (and (enabled "syntax/macros") (zero? (mod n 8))) (macro-ns! S))
                macro (or new-macro macro)
                available (range (quot (inc n) 2))
                deps (vec (sort (take (min (count available) (inc (rint rng 3))) (shuffled rng available))))
                remaining (- lines total overhead)
                worker (worker! S n deps macro (max 200 (min namespace-lines remaining)))
                added (cond-> [worker] new-macro (conj (:file new-macro)))]
            (recur (into files added)
                   (+ total (reduce + (map #(code-lines (:source %)) added)))
                   macro)))))))

;;; Output

(defn write-file! [root path text]
  (let [file (fs/path root path)]
    (fs/create-dirs (fs/parent file))
    (spit (str file) text)))
(defn edn-text [value] (with-out-str (pprint/pprint value)))
(defn sha256 [text]
  (let [digest (java.security.MessageDigest/getInstance "SHA-256")]
    (apply str (map #(format "%02x" %) (.digest digest (.getBytes text "UTF-8"))))))

(defn op-counts [trees]
  (->> (mapcat node-seq trees) (map :op) frequencies (into (sorted-map))))
(defn live-trees [model]
  (concat (for [[_ {:keys [arities]}] (:fns model) a arities] (:body a))
          (for [[_ {:keys [methods]}] (:impls model) [_ ars] methods [_ a] ars] (:body a))
          (for [[_ ms] (:methods model) [_ m] ms] (:body m))
          (for [[_ t] (:types model)] (:body t))
          (keep (comp :body :invoke val) (:records model))))

(defn source-metrics [sources]
  (let [ls (for [s sources l (str/split-lines s) :when (not (str/blank? l))] l)
        normal (set (map #(str/replace (str/triml %) #"[0-9]+" "N") ls))]
    {:code-lines (count ls)
     :distinct-normalized-lines (count normal)
     :distinct-line-ratio (/ (Math/round (* 10000.0 (/ (count normal) (max 1 (count ls))))) 10000.0)
     :top-level-forms (count (filter #(str/starts-with? % "(") ls))
     :bytes (reduce + (map #(count (.getBytes ^String % "UTF-8")) sources))}))

(defn generated-readme [{:keys [seed lines namespace-lines]} manifest]
  (str "# Generated Clojure workload\n\n"
       "Seed " seed ", target " lines " nonblank lines, " namespace-lines " per worker.\n"
       "Generated " (:code-lines manifest) " lines across " (:worker-namespaces manifest) " workers.\n\n"
       "The manifest records the enabled categories, each worker's methods and their call-site counts.\n\n"
       "From this project directory, compare Jolt, JVM Clojure and Babashka.\n\n"
       "```sh\nbb run.bb . 42 3\n```\n\n"
       "For a warmed benchmark, add `--warmup 3 --samples 7` to the runner command.\n\n"
       "Individual implementations and the reference model can also run directly.\n\n"
       "```sh\njolt -m workload.main 42 3\nclojure -Srepro -M"
       (when (:jvm-deps manifest) ":bench-jvm") " -m workload.main 42 3\n"
       "bb -cp src -m workload.main 42 3\nbb oracle.bb 42 3\n```\n"
       "\nThe arguments are input and iterations, both defaulting to 1.\n"))

(defn generate-mixed! [root options]
  (let [{:keys [files model markers waste-trees]} (project options)
        sources (map :source files)
        metrics (source-metrics sources)
        worker-methods (into (sorted-map) (map (juxt :ns :api-methods)) (:workers model))
        jvm-deps (when (some #(features/subtree? "babashka/fs" %) (mapcat val worker-methods))
                   features/fs-dependency)
        manifest (merge
                  {:format 4 :generator-options (dissoc options :output)
                   :worker-api-methods worker-methods :jvm-deps jvm-deps
                   :api-call-sites (into (sorted-map)
                                         (frequencies (keep :method (mapcat node-seq (live-trees model)))))
                   :generator-sha256 (into (sorted-map)
                                           (for [file ["generate.bb" "features.bb" "oracle.bb" "run.bb" "measure.clj"]]
                                             [file (sha256 (slurp (fs/file kit file)))]))
                   :worker-namespaces (count (:workers model))
                   :source-namespaces (count files)
                   :physical-lines (reduce + (map #(count (str/split-lines %)) sources))
                   :operation-counts {:live (op-counts (live-trees model)) :waste (op-counts waste-trees)}
                   :waste-expectations (into (sorted-map) (frequencies (map :expect markers)))
                   :waste-markers markers
                   :files (mapv (fn [{:keys [source] :as file}]
                                  (-> file (dissoc :source)
                                      (assoc :sha256 (sha256 source) :code-lines (code-lines source)
                                             :bytes (count (.getBytes ^String source "UTF-8"))))) files)}
                  metrics)]
    (doseq [{:keys [path source]} files] (write-file! root path source))
    (write-file! root "model.edn" (str (pr-str model) "\n"))
    (write-file! root "manifest.edn" (edn-text manifest))
    (doseq [file ["oracle.bb" "run.bb" "measure.clj" "LICENSE"]]
      (write-file! root file (slurp (fs/file kit file))))
    (write-file! root "deps.edn" (edn-text (cond-> {:paths ["src"]}
                                             jvm-deps (assoc :aliases {:bench-jvm {:extra-deps jvm-deps}}))))
    (write-file! root ".gitignore" ".cpcache/\n.jolt/\n")
    (write-file! root "README.md" (generated-readme options manifest))
    manifest))

(defn generate! [{:keys [output collection-sizes] :as options}]
  (let [root (fs/absolutize output)]
    (when (and (fs/exists? root) (or (not (fs/directory? root)) (seq (fs/list-dir root))))
      (throw (ex-info "Output directory must be absent or empty; nothing was overwritten" {:output output})))
    (if collection-sizes
      (coll-profiles/generate! kit root (merge {:seed 1 :categories (keys coll-profiles/operations)} options))
      (generate-mixed! root (merge defaults options)))))

(defn main [args]
  (try
    (let [options (parse-options args)]
      (cond
        (:help options) (println help)
        (:list-categories options) (doseq [category all-leaves]
                                     (println category))
        (:collection-sizes options)
        (let [manifest (generate! options)]
          (println (count (:profiles manifest)) "collection profiles in" (:output options)))
        :else
        (let [manifest (generate! options)]
          (println (format "Generated %d nonblank lines (%d physical, %d bytes, %d top-level forms), %d worker namespaces in %s"
                           (:code-lines manifest) (:physical-lines manifest) (:bytes manifest)
                           (:top-level-forms manifest) (:worker-namespaces manifest) (:output options)))
          (println (format "Distinct normalized lines: %d (ratio %s). Waste markers: %s. See README.md and manifest.edn."
                           (:distinct-normalized-lines manifest) (:distinct-line-ratio manifest)
                           (pr-str (:waste-expectations manifest)))))))
    (catch Exception e
      (binding [*out* *err*] (println (str "generate: " (ex-message e))))
      (System/exit 1))))

(when (= (fs/absolutize *file*)
         (some-> (System/getProperty "babashka.file") fs/absolutize))
  (main *command-line-args*))
