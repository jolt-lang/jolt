(ns jolt.native.ast
  "Analyzer for native regions: a Clojure-subset body plus its typed bindings in,
  a typed IR tree out. Every node carries :op, :type and a unique :id, so the
  ownership pass (jolt.native.memory) can name the value each node produces the
  way Carp's varOfXObj names an XObj.

  Types are :long, :double, :bool, :unit, [:array e] (an array the region OWNS:
  created inside it, freed by it) and [:ref e] (an array the region BORROWS: a
  parameter, which stays the caller's). e is :double or :long, or :byte for a
  borrowed ^bytes parameter, which a region reads (each element a signed byte,
  as a long) and never writes, clones or creates.

  Typing is bottom-up with Clojure's numeric rules, not Carp's: a long and a
  double combine to a double, a literal 1 is a long and 1.0 a double, and a
  loop var keeps the type of its init. Where a C answer could differ from the
  JVM's the analyzer refuses the form (long division making a ratio, = across
  number types, a non-boolean test) rather than pick one.")

(def ^:dynamic *ids* nil)
;; native helper instances analyzed for the current region, [helper-sym param-types]
;; -> {:iid :ret :params :body}; Carp's Concretize: one instance per call-site types
(def ^:dynamic *instances* nil)
(defn- next-id [] (swap! *ids* inc))

(def error-data
  "The ex-data every native region compile error carries. The kind marks it as
  the program's mistake, so the analyzer reports it under its own name rather
  than as :analyze/internal-failure, a fault in the compiler."
  {:jolt.native/error true :jolt.error/kind :native/invalid-region})

;; the line of the innermost enclosing form that has one: a literal (a keyword,
;; a string) carries no metadata, so its error reports the form it sits in
(def ^:dynamic *line* nil)

(defn fail
  "Raise a region compile error pointing at form."
  [form msg & {:as data}]
  (let [line (or (:line (meta form)) *line*)]
    (throw (ex-info (str "native region: " msg
                         (when line (str " (line " line ")")))
                    (merge error-data {:form form} data)))))

(defn owned? [t] (and (vector? t) (= :array (first t))))
;; [:ref e] is an array borrowed from Jolt (a region parameter: the flvector or
;; fxvector itself); [:loan e] is a region-owned array lent to a native helper
;; (plain C storage). Both are borrowed: never freed or returned by the borrower.
(defn borrowed? [t] (and (vector? t) (contains? #{:ref :loan} (first t))))
(defn array-type? [t] (or (owned? t) (borrowed? t)))
(defn elem-type [t] (second t))
(defn numeric? [t] (or (= t :long) (= t :double)))
(defn byte-array-type? [t] (and (vector? t) (= :byte (second t))))
(defn value-type
  "The type an element of array type t reads as: a byte is a long."
  [t]
  (let [e (elem-type t)] (if (= :byte e) :long e)))

(defn- node [op type form & {:as kvs}]
  (merge {:op op :type type :id (next-id) :form form} kvs))

;; --- parameters ------------------------------------------------------------

(def ^:private hint-types
  {'long :long 'double :double 'boolean :bool
   'doubles [:ref :double] 'longs [:ref :long] 'bytes [:ref :byte]})

(defn param-type [sym]
  (let [tag (:tag (meta sym))
        tag (if (string? tag) (symbol tag) tag)]
    (or (hint-types tag)
        (fail sym (str "parameter " sym " needs a type hint: one of "
                       "^long ^double ^boolean ^doubles ^longs ^bytes")))))

;; --- expressions -----------------------------------------------------------

(declare analyze)

(defn- local-sym? [x] (and (symbol? x) (nil? (namespace x))))

(defn- analyze-body
  "An implicit do: statements then a value."
  [env forms tail form]
  (cond
    (empty? forms) (node :const :unit form :val nil)
    (= 1 (count forms)) (analyze env (first forms) tail)
    :else (let [stmts (mapv #(analyze env % false) (butlast forms))
                ret (analyze env (last forms) tail)]
            (node :do (:type ret) form :statements stmts :ret ret))))

;; :never as an operand is a recursive helper call whose result type is still
;; being solved (instantiate! iterates from it to a fixpoint): it is "no type
;; yet", which every check lets through and contagion ignores
(defn- unknown? [t] (= t :never))

(defn- check-num [form args what]
  (doseq [a args]
    (when-not (or (numeric? (:type a)) (unknown? (:type a)))
      (fail form (str what " takes numbers, got " (pr-str (:type a)))))))

(defn- contagion [args]
  (cond (some #(= :double (:type %)) args) :double
        (some #(= :long (:type %)) args) :long
        :else :never))

(defn- bindings-of [form v what]
  (when-not (and (vector? v) (even? (count v)))
    (fail form (str what " needs a vector of name/value pairs")))
  (doseq [n (take-nth 2 v)]
    (when-not (local-sym? n)
      (fail form (str what " binds plain symbols only (no destructuring), got " (pr-str n)))))
  (partition 2 v))

(defn- analyze-let [env [_ bv & body :as form] tail]
  (let [[env bs] (reduce (fn [[env bs] [n init]]
                           (let [init (analyze env init false)
                                 _ (when (= :unit (:type init))
                                     (fail form (str "let binding " n " has no value")))
                                 id (next-id)]
                             [(assoc env n {:id id :type (:type init) :name n})
                              (conj bs {:name n :id id :init init})]))
                         [env []] (bindings-of form bv "let"))
        body (analyze-body env body tail form)]
    (node :let (:type body) form :bindings bs :body body)))

(defn- unify-branches [form t e]
  (let [tt (:type t) et (:type e)]
    (cond
      (= tt et) tt
      (= tt :never) et
      (= et :never) tt
      (or (= tt :unit) (= et :unit)) :unit
      :else (fail form (str "if branches have different types: "
                            (pr-str tt) " and " (pr-str et))))))

(defn- analyze-test [env test-form form]
  (let [t (analyze env test-form false)]
    (when-not (= :bool (:type t))
      (fail form (str "a test must be a boolean, got " (pr-str (:type t))
                      " (every non-nil, non-false value is truthy in Clojure)")))
    t))

(defn- analyze-if [env [_ c t e :as form] tail]
  (when-not (<= 3 (count form) 4) (fail form "if takes a test, a then and an optional else"))
  (let [c (analyze-test env c form)
        t (analyze env t tail)
        e (if (= 4 (count form)) (analyze env e tail) (node :const :unit form :val nil))]
    (node :if (unify-branches form t e) form :test c :then t :else e)))

(defn- analyze-loop [env [_ bv & body :as form]]
  (let [loop-id (next-id)
        [env bs] (reduce (fn [[env bs] [n init]]
                           (let [init (analyze env init false)
                                 id (next-id)]
                             (when (= :unit (:type init))
                               (fail form (str "loop binding " n " has no value")))
                             [(assoc env n {:id id :type (:type init) :name n})
                              (conj bs {:name n :id id :init init})]))
                         [env []] (bindings-of form bv "loop"))
        env (assoc env ::loop {:id loop-id :bindings bs})
        body (analyze-body env body true form)]
    (node :loop (:type body) form :loop-id loop-id :bindings bs :body body)))

(defn- analyze-recur [env [_ & args :as form] tail]
  (let [lp (::loop env)]
    (when-not lp (fail form "recur outside a loop"))
    (when-not tail (fail form "recur must be in tail position"))
    (when-not (= (count args) (count (:bindings lp)))
      (fail form (str "recur takes " (count (:bindings lp)) " values, got " (count args))))
    (let [args (mapv #(analyze env % false) args)]
      (doseq [[b a] (map vector (:bindings lp) args)]
        (let [want (:type (:init b))]
          (when-not (or (= want (:type a)) (and (= want :double) (= :long (:type a))))
            (fail form (str "recur passes " (pr-str (:type a)) " for loop var " (:name b)
                            ", which its init made " (pr-str want)
                            (when (and (= want :long) (= :double (:type a)))
                              " (initialize it with a double, e.g. 0.0)"))))))
      (node :recur :never form :loop-id (:id lp) :args args))))

(defn- analyze-dotimes [env [_ bv & body :as form] tail]
  (when-not (and (vector? bv) (= 2 (count bv)) (local-sym? (first bv)))
    (fail form "dotimes takes [name count]"))
  (let [[i n] bv
        lim (gensym "lim")
        ;; plain lists, not syntax-quote, so i and lim keep their names; the heads
        ;; are clojure.core's, as the dispatch reads them, so a local named let or
        ;; inc does not shadow them (it would not in clojure.core/dotimes either)
        r (analyze env
                   (with-meta
                     (list 'clojure.core/let [lim n]
                           (list 'clojure.core/loop [i 0]
                                 (list 'if (list 'clojure.core/< i lim)
                                       (concat ['do] body [(list 'recur (list 'clojure.core/inc i))])
                                       nil)))
                     (meta form))
                   tail)]
    ;; clojure.core/dotimes counts to (long n): 2.5 runs twice, where (< i 2.5)
    ;; would run three times. Ask for the cast rather than guess it.
    (when-not (= :long (-> r :bindings first :init :type))
      (fail form "dotimes takes a long count; use (long n) for a double"))
    r))

(def ^:private math-fns
  {'Math/sqrt 1 'Math/sin 1 'Math/cos 1 'Math/tan 1 'Math/exp 1 'Math/log 1
   'Math/floor 1 'Math/ceil 1 'Math/pow 2 'Math/atan2 2})

(defn- arr-arg [form a what]
  (when-not (array-type? (:type a))
    (fail form (str what " takes an array, got " (pr-str (:type a)))))
  a)

(defn- long-arg [form a what]
  (when-not (or (= :long (:type a)) (unknown? (:type a)))
    (fail form (str what " takes a long here, got " (pr-str (:type a)))))
  a)

(declare analyze-body analyze-extern-call)

;; --- native helpers (jolt.native/defnative) ----------------------------------

(defn helper-of
  "The defnative helper a call head names, or nil."
  [env f]
  (when (and (symbol? f) (not (contains? env f)))
    (when-let [v (resolve f)]
      (when-let [h (:jolt.native/helper (meta v))]
        (assoc h :sym (symbol (str (:ns (meta v))) (str (:name (meta v))))
                 :line (:line (meta v)))))))

(defn extern-of
  "The C function (jolt.native/defextern) a call head names, or nil."
  [env f]
  (when (and (symbol? f) (not (contains? env f)))
    (when-let [v (resolve f)]
      (when-let [x (:jolt.native/extern (meta v))]
        (assoc x :sym (symbol (str (:ns (meta v))) (str (:name (meta v)))))))))

(def ^:private ret-hints
  {'long :long 'double :double 'boolean :bool 'doubles [:array :double] 'longs [:array :long]})

(defn- loan-type [t] (if (owned? t) [:loan (elem-type t)] t))
(defn- first-or-nil [t] (when (vector? t) (first t)))

(declare attempt-instance)

(defn- instantiate!
  "Analyze helper h's body for these parameter types, once per types. A
  recursive helper's result type is solved by iteration: its own calls are
  first typed :never (no type yet), the base case gives the body a type, and
  the body is re-analyzed assuming that type until it stops changing. Instances
  analyzed under an assumption that then changed are dropped and redone."
  [h key ptypes form]
  (let [hint (when-let [tag (:ret h)] (or (ret-hints (if (string? tag) (symbol tag) tag))
                                          (fail form (str "bad return hint on " (:sym h)))))]
    (if hint
      (attempt-instance h key ptypes form hint true (atom false))
      (loop [assumed :never n 0]
        (let [snap @*instances*
              recursed (atom false)
              inst (attempt-instance h key ptypes form assumed false recursed)
              t (:ret inst)]
          (cond
            (not @recursed) inst
            (= t assumed)
            (if (= t :never)
              (fail form (str "native helper " (:sym h) " never returns: every path calls itself"))
              inst)
            (> n 4) (fail form (str "native helper " (:sym h) "'s result type does not settle; give it a return type hint"))
            :else (do (reset! *instances* snap) (recur t (inc n)))))))))

(defn- variant-type
  "The result type of a helper's bodies, which must agree (a body that never
  returns agrees with any)."
  [h form bodies]
  (let [ts (distinct (remove #{:never} (map :type bodies)))]
    (when (< 1 (count ts))
      (fail form (str "native helper " (:sym h) "'s platform bodies return different types: "
                      (clojure.string/join ", " (map pr-str ts)))))
    (or (first ts) :never)))

(defn- attempt-instance
  "One analysis of a helper instance, its own calls typed `assumed`: the hint
  when hinted? (then binding), else the current guess of instantiate!'s
  fixpoint."
  [h key ptypes form assumed hinted? recursed]
  (let [iid (next-id)]
    (swap! *instances* assoc key {:iid iid :ret assumed :pending true :recursed recursed})
    ;; an array the region owns arrives typed as owned: whether the helper takes
    ;; it (gives it away on some path) or only borrows it is inferred by the
    ;; ownership pass (jolt.native.memory), which strips the ones it never gives
    ;; away. An array the region borrowed from Jolt ([:ref e]) can only be lent.
    (let [ps (mapv (fn [p t] {:name p :id (next-id)
                              :type (if (= :loan (first-or-nil t)) [:array (elem-type t)] t)
                              :candidate (= :loan (first-or-nil t))})
                   (:params h) ptypes)
          env (into {} (map (fn [p] [(:name p) p]) ps))
          analyze-in (fn [forms] (binding [*ns* (the-ns (:ns h)) *line* (:line h)]
                                         (analyze-body env forms false form)))
          variants (when (:variants h)
                     (mapv (fn [v] (assoc v :body (analyze-in (:body v)))) (:variants h)))
          body (if variants
                 (:body (or (first (filter #(= :default (:guard %)) variants)) (first variants)))
                 (analyze-in (:body h)))
          t (variant-type h form (if variants (map :body variants) [body]))]
      (when (borrowed? t)
        (fail form (str "native helper " (:sym h) " returns an array the region only borrowed from"
                        " its caller (a ^doubles or ^longs parameter); pass (aclone x) so the helper"
                        " gets one it can own")))
      (when (and hinted? (not= assumed t) (not= :never t))
        (fail form (str "native helper " (:sym h) " is hinted " (pr-str assumed) " but returns " (pr-str t))))
      (let [inst (cond-> {:iid iid :ret (if hinted? assumed t) :params ps :body body :sym (:sym h) :types ptypes}
                   variants (assoc :variants variants))]
        (swap! *instances* assoc key inst)
        inst))))

(defn- analyze-helper-call [h args form]
  (when-not (= (count args) (count (:params h)))
    (fail form (str (:sym h) " takes " (count (:params h)) " argument(s), got " (count args))))
  (doseq [a args]
    (when (= :unit (:type a)) (fail form (str "an argument to " (:sym h) " has no value"))))
  (let [ptypes (mapv (comp loan-type :type) args)
        key [(:sym h) ptypes]
        inst (or (get @*instances* key) (instantiate! h key ptypes form))]
    (when (:pending inst) (reset! (:recursed inst) true))
    (node :call (:ret inst) form :instance (:iid inst) :args args :helper-sym (:sym h))))

(declare builtins)

(defn- analyze-call [env [f & raw :as form] tail]
  ;; the head first: an unsupported call is the error, not its arguments
  (when-not (or (builtins f) (math-fns f) (helper-of env f) (extern-of env f))
    (fail form (str "not allowed in a native region: " (pr-str f))))
  (let [args (mapv #(analyze env % false) raw)
        n (count args)
        prim (fn [t p] (node :prim t form :prim p :args args))]
    (case f
      (+ *) (do (check-num form args (str f))
                (if (zero? n)
                  (node :const :long form :val (if (= f '+) 0 1))
                  (prim (contagion args) (keyword (str f)))))
      - (do (check-num form args "-")
            (when (zero? n) (fail form "- needs at least one argument"))
            (prim (contagion args) (if (= 1 n) :neg :-)))
      / (do (check-num form args "/")
            (when (zero? n) (fail form "/ needs at least one argument"))
            ;; a left fold: only the first step can see two longs, and (/ long long)
            ;; is a ratio in Clojure (so is (/ long) unary)
            (when (every? #(= :long (:type %)) (take 2 args))
              (fail form "(/ long long) is a ratio in Clojure; use quot, or make an operand a double"))
            (prim :double :div))
      (quot rem mod) (do (when-not (= 2 n) (fail form (str f " takes two arguments")))
                         (doseq [a args] (long-arg form a (str f)))
                         (prim :long (keyword (str f))))
      (inc dec) (do (when-not (= 1 n) (fail form (str f " takes one argument")))
                    (check-num form args (str f))
                    (prim (:type (first args)) (keyword (str f))))
      (< <= > >= ==) (do (when (zero? n) (fail form (str f " needs an argument")))
                         (check-num form args (str f))
                         (prim :bool (keyword (str (if (= f '==) "num=" f)))))
      (= not=) (do (when-not (= 2 n) (fail form (str f " takes two arguments here")))
                   (let [[a b] args]
                     (when-not (or (unknown? (:type a)) (unknown? (:type b))
                                   (and (= (:type a) (:type b)) (#{:long :double :bool} (:type a))))
                       (fail form (str f " compares two values of the same scalar type, got "
                                       (pr-str (:type a)) " and " (pr-str (:type b))
                                       " (use == to compare numbers across types)"))))
                   (prim :bool (if (= f '=) :eq :ne)))
      (zero? pos? neg?) (do (when-not (= 1 n) (fail form (str f " takes one argument")))
                            (check-num form args (str f))
                            (prim :bool (keyword (str f))))
      (bit-and bit-or bit-xor)
      (do (when (< n 2) (fail form (str f " takes two or more longs")))
          (doseq [a args] (long-arg form a (str f)))
          (prim :long (keyword (str f))))
      bit-not (do (when-not (= 1 n) (fail form "bit-not takes one long"))
                  (long-arg form (first args) "bit-not")
                  (prim :long :bit-not))
      (bit-shift-left bit-shift-right unsigned-bit-shift-right)
      (do (when-not (= 2 n) (fail form (str f " takes a long and a shift count")))
          (doseq [a args] (long-arg form a (str f)))
          (prim :long (keyword (str f))))
      not (do (when-not (and (= 1 n) (= :bool (:type (first args))))
                (fail form "not takes one boolean"))
              (prim :bool :not))
      double (do (when-not (= 1 n) (fail form "double takes one argument"))
                 (check-num form args "double")
                 (prim :double :to-double))
      long (do (when-not (= 1 n) (fail form "long takes one argument"))
               (check-num form args "long")
               (prim :long :to-long))
      aget (do (when-not (= 2 n) (fail form "aget takes an array and an index"))
               (arr-arg form (args 0) "aget") (long-arg form (args 1) "aget index")
               (node :aget (value-type (:type (args 0))) form :arr (args 0) :idx (args 1)))
      aset (do (when-not (= 3 n) (fail form "aset takes an array, an index and a value"))
               (arr-arg form (args 0) "aset") (long-arg form (args 1) "aset index")
               (when (byte-array-type? (:type (args 0)))
                 (fail form "a region reads a ^bytes parameter but cannot write it"))
               (let [et (elem-type (:type (args 0))) vt (:type (args 2))]
                 (when-not (or (= et vt) (and (= et :double) (= vt :long)) (unknown? vt))
                   (fail form (str "aset stores " (pr-str et) " into this array, got " (pr-str vt)))))
               (node :aset (elem-type (:type (args 0))) form
                     :arr (args 0) :idx (args 1) :val (args 2)))
      alength (do (when-not (= 1 n) (fail form "alength takes an array"))
                  (arr-arg form (args 0) "alength")
                  (node :alength :long form :arr (args 0)))
      (double-array long-array)
      (do (when-not (= 1 n) (fail form (str f " takes a size here")))
          (long-arg form (args 0) (str f " size"))
          (let [e (if (= f 'double-array) :double :long)]
            (node :new-array [:array e] form :elem e :len (args 0))))
      aclone (do (when-not (= 1 n) (fail form "aclone takes an array"))
                 (arr-arg form (args 0) "aclone")
                 (when (byte-array-type? (:type (args 0)))
                   (fail form "a region cannot clone a ^bytes parameter: it only reads one"))
                 (node :aclone [:array (elem-type (:type (args 0)))] form :arr (args 0)))
      (if-let [arity (math-fns f)]
        (do (when-not (= arity n) (fail form (str f " takes " arity " argument(s)")))
            (check-num form args (str f))
            (node :math :double form :fn (name f) :args args))
        (if-let [h (helper-of env f)]
          (analyze-helper-call h args form)
          (analyze-extern-call (extern-of env f) args form))))))

(defn- extern-arg-ok? [want t]
  (if (vector? want)
    (and (array-type? t) (= (elem-type want) (elem-type t)))
    (or (= want t) (and (= want :double) (= :long t)) (unknown? t))))

(defn- analyze-extern-call
  "A call to a declared C function: scalars and arrays in (an array as its
  storage and length), a scalar (or nothing) out."
  [x args form]
  (when-not (= (count args) (count (:params x)))
    (fail form (str (:sym x) " takes " (count (:params x)) " argument(s), got " (count args))))
  (doseq [[want a] (map vector (:params x) args)]
    (when-not (extern-arg-ok? want (:type a))
      (fail form (str (:sym x) " takes " (pr-str want) " here, got " (pr-str (:type a)))))
    ;; a long-array borrowed from Jolt holds fixnums (value * 8), not int64_t
    (when (= [:ref :long] (:type a))
      (fail form (str (:sym x) " gets a long-array's int64_t elements; a ^longs borrowed from"
                      " Jolt stores fixnums, so pass (aclone it)"))))
  (node :extern (:ret x) form :c (:c x) :header (:header x) :params (:params x)
        :args args :sym (:sym x)))

(def ^:private builtins
  "Heads analyze-call handles itself: never macroexpanded."
  '#{+ * - / quot rem mod inc dec < <= > >= == = not= zero? pos? neg? not
     double long aget aset alength double-array long-array aclone
     bit-and bit-or bit-xor bit-not bit-shift-left bit-shift-right unsigned-bit-shift-right})

(def ^:private special-heads
  '#{let let* if when when-not do loop loop* recur dotimes and or})

(def ^:private clojure-specials
  "Heads Clojure never lets a local shadow."
  '#{if do recur let* loop*})

(defn- norm-head
  "A clojure.core-qualified head (what a syntax-quoting macro emits) read as
  the unqualified name the region language uses."
  [op]
  (if (and (symbol? op) (= "clojure.core" (namespace op))) (symbol (name op)) op))

(defn- user-macro?
  "A head that names a macro, not a region local, builtin or special form: the
  region expands it and analyzes what it expands to."
  [env op]
  (and (symbol? op) (not (contains? env op))
       (not (builtins op)) (not (special-heads op))
       (when-let [v (resolve op)] (:macro (meta v)))))

(defn analyze
  "Analyze one region form in env (symbol -> {:id :type :name})."
  [env form tail]
  (cond
    (instance? Boolean form) (node :const :bool form :val form)
    (integer? form) (do (when-not (<= Long/MIN_VALUE form Long/MAX_VALUE)
                          (fail form "integer literal outside the long range"))
                        (node :const :long form :val form))
    (float? form) (node :const :double form :val (double form))
    (nil? form) (node :const :unit form :val nil)
    (symbol? form)
    (if-let [b (get env form)]
      (node :local (:type b) form :name form :binding (:id b))
      (fail form (str "unknown name " form
                      " (a region sees only its parameters and its own locals)")))
    (and (seq? form) (:line (meta form)) (not= *line* (:line (meta form))))
    (binding [*line* (:line (meta form))] (analyze env form tail))
    ;; a local shadows the builtin, helper or macro of its name, as in Clojure,
    ;; where the call would invoke the local's value; only the special forms
    ;; cannot be shadowed
    (and (seq? form) (contains? env (first form)) (not (clojure-specials (first form))))
    (fail form (str (first form) " is a local here, not a function: a region has no function values"))
    (and (seq? form) (user-macro? env (norm-head (first form))))
    (analyze env (macroexpand-1 form) tail)
    (seq? form)
    (let [op (norm-head (first form))
          more (rest form)
          form (if (= op (first form)) form (with-meta (cons op more) (meta form)))]
      (case op
        let (analyze-let env form tail)
        let* (analyze-let env form tail)
        if (analyze-if env form tail)
        when (analyze-if env (with-meta (list 'if (first more) (cons 'do (rest more))) (meta form)) tail)
        when-not (analyze-if env (with-meta (list 'if (list 'clojure.core/not (first more)) (cons 'do (rest more))) (meta form)) tail)
        do (analyze-body env more tail form)
        (loop loop*) (analyze-loop env form)
        recur (analyze-recur env form tail)
        dotimes (analyze-dotimes env form tail)
        and (analyze env (if (empty? more) true
                             (reduce (fn [acc x] (with-meta (list 'if x acc false) (meta form)))
                                     (reverse more)))
                     tail)
        or (analyze env (if (empty? more) false
                            (reduce (fn [acc x] (with-meta (list 'if x true acc) (meta form)))
                                    (reverse more)))
                    tail)
        (analyze-call env form tail)))
    :else (fail form (str "not allowed in a native region: " (pr-str form)))))

(defn analyze-region
  "Analyze (native [params...] body...) given as params + body forms. Answers
  {:params [{:name :id :type}] :body node :type t}."
  [params body]
  (binding [*ids* (atom 0) *instances* (atom {})]
    (let [ps (mapv (fn [p]
                     (when-not (local-sym? p) (fail p "region parameters are plain symbols"))
                     {:name p :id (next-id) :type (param-type p)})
                   params)
          env (into {} (map (fn [p] [(:name p) p]) ps))
          body (analyze-body env body false (cons 'native body))
          t (:type body)]
      (when (borrowed? t)
        (fail (:form body) "a region cannot return an array it borrowed; return (aclone it)"))
      {:params ps :body body :type t
       :instances (vec (sort-by :iid (vals @*instances*)))})))
