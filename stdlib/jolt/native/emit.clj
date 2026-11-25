;;   The region-to-C lowering follows the shape of Carp's src/Emit.hs
;;   (https://github.com/carp-lang/Carp), Copyright (c) Erik Svedäng and
;;   contributors, Apache License 2.0 (licenses/Apache-2.0.txt).

(ns jolt.native.emit
  "C for a checked native region (jolt.native.memory output).

  Every value lands in a C variable named x<id> (the id the ownership pass used
  for it), declared at the top of the function, so a free in a :delete set is
  `free(x<id>.data)`. Expressions are flattened to one temp per node; the C
  compiler folds them back.

  The generated function is

      RET <sym>(ARGS...)

  A borrowed array arrives as the Chez flvector or fxvector itself (a raw ptr)
  and is read in place. The call site keeps it from moving: a ^:hold-gc region
  is a plain call, so the thread stays active and no collection runs; a pinned
  one locks each borrowed array around a __collect_safe call. An owned array
  result comes back as a malloc'd buffer the stub copies into a fresh array,
  then frees through <sym>_copy_out. On a failure C frees every live owned
  value (the node's :live set), records the code and its operands in the
  thread's status (<sym>_status, _err_l, _err_d), and returns 0; the stub
  raises the matching JVM exception.

  Arithmetic follows the JVM's primitive rules exactly: a long op that leaves
  the long range raises (the C uses the overflow builtins), and a variadic op
  folds left pair by pair, so (+ Long/MAX_VALUE 1 1.5) overflows on the first
  pair as it does on the JVM. Compile the output with -ffp-contract=off: a fused
  multiply-add rounds once where Jolt rounds twice."
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [jolt.native.ast :as ast]
            [jolt.native.range :as ranges]
            [jolt.native.stack :as stack]))

(def err-codes
  {:oob 1 :overflow 2 :div-zero 3 :neg-size 4 :long-range 5 :oom 6 :fixnum-store 7 :stack-overflow 8
   :mod-zero 9})

;; A borrowed long-array is a Chez fxvector: each slot a fixnum, the value times
;; 8 (scheme.h Sfixnum/Sfixnum_value). jolt widens the backing once a value
;; leaves the fixnum range, which C cannot do, so a region's store there raises.
(def fixnum-min "(-1152921504606846976LL)")
(def fixnum-max "1152921504606846975LL")
(defn- fx-borrowed? [t] (= t [:ref :long]))

(def ^:dynamic *decls* nil)
(def ^:dynamic *lines* nil)
(def ^:dynamic *region* nil)
(def ^:dynamic *tmp* nil)
;; the function being emitted: :entry (the region's exported C entry, which
;; returns an array result as its data pointer) or the helper instance's result
;; type (a static function, which returns an owned array struct by value)
(def ^:dynamic *fn-ret* :entry)
(def ^:dynamic *sym* nil)
;; range facts per function (jolt.native.range), and the facts in force while
;; the fast copy of a function is emitted (nil in the checked copy)
(def ^:dynamic *ranges* nil)
(def ^:dynamic *fast* nil)
;; stack arrays per function (jolt.native.stack), and the current function's
(def ^:dynamic *stacks* nil)
(def ^:dynamic *stack* nil)
;; the helper instances that can call themselves (directly or through others),
;; whose C checks the stack on entry against the limit the region's entry read
(def ^:dynamic *recursive* #{})
;; how the current function reaches the thread's status: the entry names the
;; thread-local, a helper the pointer to it the entry passed down (jn_st), so
;; a helper call costs no thread-local lookup
(def ^:dynamic *st* "jn_tls.")

(defn- proven? [node k] (and *fast* (get-in *fast* [(:id node) k])))

(defn- line! [& parts] (let [l (apply str parts)] (when-not (str/blank? l) (swap! *lines* conj l))))

(defn- ctype [t]
  (case t
    :long "int64_t" :double "double" :bool "int" :unit "int" :never "int"
    (if (ast/array-type? t)
      (case (ast/elem-type t) :double "jn_arr_d" :long "jn_arr_l" :byte "jn_arr_b")
      (throw (ex-info "no C type" {:type t})))))

(defn- var-name [id] (str "x" id))

(defn- declare-var! [id t]
  (let [n (var-name id)]
    (swap! *decls* assoc n
           (str (ctype t) " " n " = "
                (if (ast/array-type? t) (str "{0, 0}") "0") ";"))
    n))

(defn- fresh! [t]
  (let [id (str "t" (swap! *tmp* inc))]
    (swap! *decls* assoc id (str (ctype t) " " id " = 0;"))
    id))

(defn- ret-zero
  "What a failing function returns: 0 (NULL for the entry's array result), or
  an empty struct from a helper that returns an owned array. The caller reads
  the status before it looks at the value."
  []
  (if (and (not= :entry *fn-ret*) (ast/owned? *fn-ret*))
    (str "(" (ctype *fn-ret*) "){0, 0}")
    "0"))

(defn- helper-name [iid] (str *sym* "_h" iid))

(defn- frees
  "Free the owned arrays named by ids, except those only ever holding a stack
  array, which the frame frees."
  [ids]
  (str/join " " (map #(str "JN_FREE(" (var-name %) ".data);")
                     (sort (remove (or (:holders *stack*) #{}) ids)))))

(defn- raise! [node code & {:keys [a b d]}]
  (str "{ " (frees (:live node))
       " " *st* "code = " (err-codes code) ";"
       (when a (str " " *st* "a = " a ";"))
       (when b (str " " *st* "b = " b ";"))
       (when d (str " " *st* "d = " d ";"))
       " return " (ret-zero) "; }"))

(declare emit fresh-copy! as-double-if emit-entry emit-stack-array emit-heap-array emit-aclone
         scalar-site elem-local)

(defn- c-double-lit [v]
  (cond (Double/isNaN v) "(0.0/0.0)"
        (Double/isInfinite v) (if (pos? v) "(1.0/0.0)" "(-1.0/0.0)")
        :else (let [s (pr-str v)] (if (re-find #"[.eE]" s) s (str s ".0")))))

(defn- c-long-lit [v]
  (if (= v Long/MIN_VALUE) "INT64_MIN" (str v "LL")))

(defn- as-double [atom t] (if (= t :long) (str "((double)" atom ")") atom))

(defn- arith-pair
  "One step of a left fold: acc (type at) op b (type bt). Answers [atom type].
  safe: the range facts prove this long step cannot overflow."
  [node op [a at] [b bt] safe]
  (if (and (= at :long) (= bt :long))
    (let [r (fresh! :long)
          builtin (case op :+ "__builtin_add_overflow" :- "__builtin_sub_overflow"
                          :* "__builtin_mul_overflow")]
      (if safe
        (line! r " = " a " " (case op :+ "+" :- "-" :* "*") " " b ";")
        (line! "if (" builtin "(" a ", " b ", &" r ")) " (raise! node :overflow)))
      [r :long])
    (let [r (fresh! :double)
          cop (case op :+ "+" :- "-" :* "*" :div "/")]
      (line! r " = " (as-double a at) " " cop " " (as-double b bt) ";")
      [r :double])))

(defn- emit-prim [node]
  (let [args (mapv (fn [a] [(emit a) (:type a)]) (:args node))
        p (:prim node)
        t (:type node)
        one (first args)]
    (case p
      (:+ :- :*) (let [oks (proven? node :no-ov)]
                   (first (reduce (fn [acc [i b]] (arith-pair node p acc b (get oks i)))
                                  (first args) (map-indexed vector (rest args)))))
      :div (first (reduce #(arith-pair node :div %1 %2 false)
                          (if (= 1 (count args)) (cons ["1.0" :double] args) args)))
      :neg (if (= t :long)
             (let [r (fresh! :long)]
               (when-not (first (proven? node :no-ov))
                 (line! "if (" (first one) " == INT64_MIN) " (raise! node :overflow)))
               (line! r " = -(" (first one) ");")
               r)
             ;; -(x), not -x: x may be a negative literal, and --1.5 is a decrement
             (str "(-(" (first one) "))"))
      (:inc :dec) (if (= t :long)
                    (let [r (fresh! :long)]
                      (if (first (proven? node :no-ov))
                        (line! r " = " (first one) (if (= p :inc) " + 1;" " - 1;"))
                        (line! "if (" (if (= p :inc) "__builtin_add_overflow" "__builtin_sub_overflow")
                               "(" (first one) ", 1, &" r ")) " (raise! node :overflow)))
                      r)
                    (str "(" (first one) (if (= p :inc) " + 1.0)" " - 1.0)")))
      (:quot :rem :mod)
      ;; operands in temps: gcc refuses a literal 2LL / 0LL under -Werror even
      ;; behind the guard that makes it unreachable
      (let [[[a0 _] [b0 _]] args r (fresh! :long) a (fresh! :long) b (fresh! :long)]
        (line! a " = " a0 "; " b " = " b0 ";")
        ;; quot and rem are Java's long / and % ("/ by zero"); mod is a Clojure
        ;; fn over Objects, whose zero is Numbers' "Divide by zero"
        (line! "if (" b " == 0) " (raise! node (if (= p :mod) :mod-zero :div-zero)))
        (case p
          ;; INT64_MIN / -1 is undefined in C; the JVM's long / wraps to INT64_MIN
          :quot (line! r " = (" b " == -1) ? (int64_t)(0ULL - (uint64_t)" a ") : " a " / " b ";")
          ;; INT64_MIN % -1 is undefined in C; the JVM answers 0
          :rem (line! r " = (" b " == -1) ? 0 : " a " % " b ";")
          ;; clojure.core/mod: (let [m (rem n d)] (if (or (zero? m) (= (pos? n) (pos? d))) m (+ m d)))
          :mod (line! "{ int64_t m = (" b " == -1) ? 0 : " a " % " b "; "
                      r " = (m == 0 || ((" a " > 0) == (" b " > 0))) ? m : m + " b "; }"))
        r)
      (:< :<= :> :>= :num=)
      (let [cop (case p :< "<" :<= "<=" :> ">" :>= ">=" :num= "==")
            pairs (partition 2 1 args)]
        (if (empty? pairs)
          "1"
          (str "(" (str/join " && "
                             (map (fn [[[a at] [b bt]]]
                                    (if (= at bt)
                                      (str "(" a " " cop " " b ")")
                                      (str "(" (as-double a at) " " cop " " (as-double b bt) ")")))
                                  pairs))
               ")")))
      :eq (str "(" (ffirst args) " == " (first (second args)) ")")
      :ne (str "(" (ffirst args) " != " (first (second args)) ")")
      :zero? (str "(" (first one) " == 0)")
      :pos? (str "(" (first one) " > 0)")
      :neg? (str "(" (first one) " < 0)")
      :not (str "(!" (first one) ")")
      ;; two's complement on int64_t, as the JVM's long ops; a shift count is
      ;; masked to 0..63 as the JVM masks it (C leaves 64 and up undefined), and
      ;; a left shift goes through uint64_t, where C defines the wrap
      (:bit-and :bit-or :bit-xor)
      (str "(" (str/join (case p :bit-and " & " :bit-or " | " :bit-xor " ^ ") (map first args)) ")")
      :bit-not (str "(~" (first one) ")")
      :bit-shift-left (str "((int64_t)((uint64_t)" (ffirst args) " << (" (first (second args)) " & 63)))")
      :bit-shift-right (str "(" (ffirst args) " >> (" (first (second args)) " & 63))")
      :unsigned-bit-shift-right
      (str "((int64_t)((uint64_t)" (ffirst args) " >> (" (first (second args)) " & 63)))")
      :to-double (as-double (first one) (second one))
      :to-long (if (= :long (second one))
                 (first one)
                 ;; proven equal to RT.longCast(double): test/native/proofs/long-cast.smt2
                 (let [d (first one) r (fresh! :long)]
                   (line! "if (" d " != " d ") " r " = 0;"
                          " else if (" d " >= -9223372036854775808.0 && " d " < 9223372036854775808.0) "
                          r " = (int64_t)" d ";"
                          " else if (" d " == 9223372036854775808.0) " r " = INT64_MAX;"
                          " else " (raise! node :long-range :d d))
                   r)))))

(defn- emit-index-cast
  "aget/aset take their index through RT.intCast, which on the JVM runs as the
  index argument is evaluated: before a later argument (aset's value) runs, so
  its side effects never happen when the index overflows. An array kept in
  locals is only ever indexed by constants in range (jolt.native.stack)."
  [node idx]
  (when-not (or (proven? node :idx-int) (scalar-site (:arr node)))
    (line! "if (" idx " < INT32_MIN || " idx " > INT32_MAX) " (raise! node :overflow))))

(defn- emit-array-index
  "The bounds check, inside the store or load: after every argument ran."
  [node arr idx]
  (when-not (or (proven? node :in-bounds) (scalar-site (:arr node)))
    (line! "if (" idx " < 0 || " idx " >= " arr ".len) " (raise! node :oob :a idx :b (str arr ".len")))))

(defn- bind!
  "Bind id to atom. A name for an array kept in locals has nothing to copy:
  its elements are the site's locals and its length a constant."
  [id t atom]
  (let [n (declare-var! id t)]
    (when-not (get (:scalar *stack*) id) (line! n " = " atom ";"))
    n))

(defn- emit-loop [node]
  (let [result (when-not (#{:unit :never} (:type node)) (declare-var! (:id node) (:type node)))]
    (doseq [b (:bindings node)]
      (bind! (:id b) (:type (:init b)) (emit (:init b))))
    (line! "for (;;) {")
    (let [v (emit (:body node))]
      (when-not (= :never (:type (:body node)))
        (when result (line! result " = " v ";"))
        (line! "break;")))
    (line! "}")
    (line! (frees (:delete node)))
    (or result "0")))

(defn- emit-recur [node]
  (let [loop-node (get-in @*region* [:loops (:loop-id node)])
        atoms (mapv (fn [a] (let [v (emit a) t (fresh-copy! a v)] t)) (:args node))]
    (line! (frees (:delete node)))
    (doseq [[b atom a] (map vector (:bindings loop-node) atoms (:args node))]
      (line! (var-name (:id b)) " = "
             (as-double-if (:type (:init b)) atom (:type a)) ";"))
    (line! "continue;")
    nil))

(defn- fresh-copy! [arg atom]
  (let [t (:type arg)
        r (if (ast/array-type? t)
            (let [n (str "t" (swap! *tmp* inc))]
              (swap! *decls* assoc n (str (ctype t) " " n " = {0, 0};"))
              n)
            (fresh! t))]
    (line! r " = " atom ";")
    r))

(defn emit
  "Emit statements for node into *out*; answer a C atom holding its value."
  [node]
  (case (:op node)
    :const (case (:type node)
             :long (c-long-lit (:val node))
             :double (c-double-lit (:val node))
             :bool (if (:val node) "1" "0")
             :unit "0")
    :local (var-name (:binding node))
    :let (do (doseq [b (:bindings node)]
               (bind! (:id b) (:type (:init b)) (emit (:init b))))
             (let [v (emit (:body node))]
               (if (= :never (:type node))
                 "0"
                 (let [r (if (#{:unit} (:type node)) "0" (bind! (:id node) (:type node) v))]
                   (line! (frees (:delete node)))
                   r))))
    :do (do (doseq [s (:statements node)] (emit s))
            (let [v (emit (:ret node))]
              ;; the ownership pass moved an owned result to this node's id
              (if (ast/owned? (:type node)) (bind! (:id node) (:type node) v) v)))
    :if (let [c (emit (:test node))
              t (:type node)
              r (when-not (#{:unit :never} t) (declare-var! (:id node) t))
              branch (fn [b dels]
                       (let [v (emit b)]
                         (when-not (= :never (:type b))
                           (when (and r (not (get (:scalar *stack*) (:id node))))
                             (line! r " = " (if (and (= t :double) (= :long (:type b)))
                                                    (str "(double)" v) v) ";"))
                           (line! (frees dels)))))]
          (line! "if (" c ") {")
          (branch (:then node) (:delete-then node))
          (line! "} else {")
          (branch (:else node) (:delete-else node))
          (line! "}")
          (or r "0"))
    :loop (do (swap! *region* assoc-in [:loops (:loop-id node)] node)
              (emit-loop node))
    :recur (emit-recur node)
    :prim (let [v (emit-prim node) r (fresh! (:type node))]
            (line! r " = " v ";")
            r)
    :extern (let [args (mapv (fn [a want]
                               (let [v (emit a)]
                                 ;; an array crosses as its storage and length
                                 (if (vector? want) (str v ".data, " v ".len") (as-double-if want v (:type a)))))
                             (:args node) (:params node))
                  call (str (:c node) "(" (str/join ", " args) ")")]
              (if (#{:unit :never} (:type node))
                (do (line! call ";") "0")
                (let [r (fresh! (:type node))] (line! r " = " call ";") r)))
    :math (let [args (mapv (fn [a] (as-double (emit a) (:type a))) (:args node))
                r (fresh! :double)
                f (case (:fn node) "sqrt" "sqrt" "sin" "sin" "cos" "cos" "tan" "tan"
                        "exp" "exp" "log" "log" "floor" "floor" "ceil" "ceil"
                        "pow" "jn_pow" "atan2" "atan2")]
            (line! r " = " f "(" (str/join ", " args) ");")
            r)
    ;; a temp like every other value: inlined, (aget a (alength a)) emitted
    ;; `a.len >= a.len`, which compilers flag as a tautology
    :alength (let [r (fresh! :long) a (emit (:arr node))]
               (line! r " = " (if-let [site (scalar-site (:arr node))]
                                (str (get-in *stack* [:sites site]) "LL")
                                (str a ".len"))
                      ";")
               r)
    :aget (let [arr (emit (:arr node)) idx (emit (:idx node)) r (fresh! (:type node))]
            (emit-index-cast node idx)
            (emit-array-index node arr idx)
            (if-let [site (scalar-site (:arr node))]
              (line! r " = " (elem-local site (:val (:idx node))) ";")
              (line! r " = " arr ".data[" idx "]" (when (fx-borrowed? (:type (:arr node))) " / 8") ";"))
            r)
    :aset (let [arr (emit (:arr node))
                idx (emit (:idx node))
                _ (emit-index-cast (assoc node :live (:live-idx node)) idx)
                v (emit (:val node))
                r (fresh! (:type node))]
            (emit-array-index node arr idx)
            (line! r " = " (as-double-if (:type node) v (:type (:val node))) ";")
            (cond
              (scalar-site (:arr node))
              (line! (elem-local (scalar-site (:arr node)) (:val (:idx node))) " = " r ";")
              (fx-borrowed? (:type (:arr node)))
              (do (line! "if (" r " < " fixnum-min " || " r " > " fixnum-max ") "
                         (raise! node :fixnum-store :a r))
                  (line! arr ".data[" idx "] = (int64_t)((uint64_t)" r " * 8);"))
              :else (line! arr ".data[" idx "] = " r ";"))
            r)
    ;; a native helper instance: a failure there has freed its own values and
    ;; set the status; the caller frees what it holds and passes it on
    :call (let [args (mapv emit (:args node))
                t (:type node)
                r (if (ast/owned? t) (declare-var! (:id node) t) (fresh! (if (#{:unit :never} t) :long t)))]
            (line! r " = " (helper-name (:instance node)) "("
                   (str/join ", " (conj args (if (= *st* "jn_tls.") "&jn_tls" "jn_st"))) ");")
            (line! "if (" *st* "code) { " (frees (:live node)) " return " (ret-zero) "; }")
            r)
    :new-array (if-let [n-el (get-in *stack* [:sites (:id node)])]
                 (emit-stack-array node n-el)
                 (emit-heap-array node))
    :aclone (emit-aclone node)))

(defn- elem-local [site k] (str (var-name site) "_e" k))

(defn- scalar-site
  "The stack site an array expression names when its elements are kept in C
  locals (jolt.native.stack :scalar), or nil."
  [arr]
  (get (:scalar *stack*) (if (= :local (:op arr)) (:binding arr) (:id arr))))

(defn- emit-stack-array
  "A small constant-size array that stays in this function: a frame buffer,
  zeroed each time it is made, as calloc would. When every use is a constant
  index, its elements are C locals instead, which a compiler keeps in
  registers."
  [node n-el]
  (let [n (declare-var! (:id node) (:type node))
        et (if (= :double (ast/elem-type (:type node))) "double" "int64_t")]
    (emit (:len node))
    (if (get (:scalar *stack*) (:id node))
      (do (doseq [k (range n-el)]
            (swap! *decls* assoc (elem-local (:id node) k) (str et " " (elem-local (:id node) k) " = 0;")))
          (line! (str/join " " (map #(str (elem-local (:id node) %) " = 0;") (range n-el)))))
      (let [buf (str n "_buf")]
        (swap! *decls* assoc buf (str et " " buf "[" (max 1 n-el) "];"))
        (line! n ".len = " n-el "; " n ".data = " buf "; memset(" buf ", 0, sizeof " buf ");")))
    n))

(defn- emit-heap-array
  "The size goes through Number.intValue as the JVM's Numbers.T_array does:
  the low 32 bits, signed, so (double-array 4294967296) is empty."
  [node]
  (let [raw (emit (:len node))
        len (fresh! :long)
        n (declare-var! (:id node) (:type node))]
    (line! "{ int64_t lo = " raw " & 0xffffffffLL; " len " = lo >= 0x80000000LL ? lo - 0x100000000LL : lo; }")
    (line! "if (" len " < 0) " (raise! node :neg-size :a len))
    (line! n ".len = " len "; " n ".data = JN_CALLOC(" len " > 0 ? " len " : 1, 8);")
    (line! "if (!" n ".data) " (raise! node :oom))
    n))

(defn- emit-aclone [node]
  (let [src (emit (:arr node))
        n (declare-var! (:id node) (:type node))]
    (line! n ".len = " src ".len; " n ".data = JN_MALLOC(" src ".len > 0 ? " src ".len * 8 : 8);")
    (line! "if (!" n ".data) " (raise! node :oom))
    (if (fx-borrowed? (:type (:arr node)))
      (line! "for (int64_t k = 0; k < " src ".len; k++) " n ".data[k] = " src ".data[k] / 8;")
      (line! "memcpy(" n ".data, " src ".data, " src ".len * 8);"))
    n))

(defn- as-double-if [want v vt] (if (and (= want :double) (= vt :long)) (str "(double)" v) v))

(def prelude
  "#include <stdint.h>
#ifdef __MIRC__
/* jolt's JIT (host/chez/stub/jolt_mir.c): c2mir declares the few C library
   functions a region calls here, so a machine with no C headers can run one */
#include <stddef.h>
#ifndef JN_MIRC_DECLS
#define JN_MIRC_DECLS
#define JN_UNUSED
void *calloc(size_t, size_t); void *malloc(size_t); void free(void *);
void *memcpy(void *, const void *, size_t); void *memset(void *, int, size_t);
double sqrt(double); double sin(double); double cos(double); double tan(double);
double exp(double); double log(double); double floor(double); double ceil(double);
double pow(double, double); double atan2(double, double); double fabs(double);
static const union { uint64_t u; double d; } jn_nan_bits = {0x7ff8000000000000ULL};
static const union { uint64_t u; double d; } jn_inf_bits = {0x7ff0000000000000ULL};
#define JN_NAN (jn_nan_bits.d)
#define JN_ISNAN(x) ((x) != (x))
#define JN_ISINF(x) (fabs(x) == jn_inf_bits.d)
#endif
#else
#include <stdlib.h>
#include <string.h>
#include <math.h>
/* (== x x) on a double is how NaN is tested; the comparison is meant */
#pragma GCC diagnostic ignored \"-Wtautological-compare\"
#define JN_UNUSED __attribute__((unused))
#define JN_NAN NAN
#define JN_ISNAN(x) isnan(x)
#define JN_ISINF(x) isinf(x)
#endif
#if UINTPTR_MAX != 0xffffffffffffffffu
#error \"jolt native regions need a 64-bit target\"
#endif
#ifndef JN_PRELUDE
#define JN_PRELUDE
typedef struct { int64_t len; double *data; } jn_arr_d;
typedef struct { int64_t len; int64_t *data; } jn_arr_l;
typedef struct { int64_t len; int8_t *data; } jn_arr_b;
typedef struct { int64_t code; int64_t a; int64_t b; double d; int64_t len;
                 uintptr_t stack_lo; uintptr_t stack_thr; } jn_out;
/* The status of the region this thread ran last: code 0, or a failure and its
   operands; and an array result's length. Thread-local rather than an argument
   buffer: a collect-safe call lets the collector run while C works, and a
   bytevector passed in could move under it. jolt build compiles every region
   into one translation unit, so a binary has one. JN_EXTERN_STATUS is for a
   single-threaded host that loads each region as its own shared object (the
   native gate) and defines one plain jn_tls for all of them. */
#if defined(__MIRC__)
/* MIR has no thread-local storage: the JIT keeps the status (jolt_mir.c) */
jn_out *jolt_mir_status(void);
#define jn_tls (*jolt_mir_status())
#elif defined(JN_EXTERN_STATUS)
extern jn_out jn_tls;
#else
static _Thread_local jn_out jn_tls;
#endif
/* The lowest stack address a recursive helper may enter at: the thread's real
   stack bounds less some headroom (a frame, the C it calls, a signal handler),
   read once per thread. Below it a helper fails with StackOverflowError rather
   than run into the guard page. A platform without a way to ask gets no guard. */
#ifndef JN_STACK_HEADROOM
#define JN_STACK_HEADROOM (256 * 1024)
#endif
#if defined(__MIRC__)
/* c2mir has no __builtin_frame_address; the JIT answers both */
uintptr_t jolt_mir_stack_limit(uintptr_t headroom);
uintptr_t jolt_mir_frame(void);
#define jn_stack_limit() jolt_mir_stack_limit(JN_STACK_HEADROOM)
#define JN_FRAME() jolt_mir_frame()
#else
#define JN_FRAME() ((uintptr_t)__builtin_frame_address(0))
#if defined(_WIN32)
__declspec(dllimport) void __stdcall GetCurrentThreadStackLimits(unsigned long long *, unsigned long long *);
__declspec(dllimport) unsigned long __stdcall GetCurrentThreadId(void);
#define JN_THREAD_ID() ((uintptr_t)GetCurrentThreadId())
#else
#include <pthread.h>
#if defined(__linux__)
int pthread_getattr_np(pthread_t, pthread_attr_t *);
#endif
#define JN_THREAD_ID() ((uintptr_t)pthread_self())
#endif
__attribute__((noinline, unused)) static uintptr_t jn_stack_limit_slow(void) {
  uintptr_t lo = 0, size = 0;
#if defined(__APPLE__)
  uintptr_t hi = (uintptr_t)pthread_get_stackaddr_np(pthread_self());
  size = pthread_get_stacksize_np(pthread_self());
  lo = hi - size;
#elif defined(__linux__)
  pthread_attr_t at; void *addr; size_t sz;
  if (pthread_getattr_np(pthread_self(), &at) == 0) {
    if (pthread_attr_getstack(&at, &addr, &sz) == 0) { lo = (uintptr_t)addr; size = sz; }
    pthread_attr_destroy(&at);
  }
#elif defined(_WIN32)
  unsigned long long l = 0, h = 0;
  GetCurrentThreadStackLimits(&l, &h);
  lo = (uintptr_t)l; size = (uintptr_t)(h - l);
#endif
  uintptr_t head = size / 4 < JN_STACK_HEADROOM ? size / 4 : JN_STACK_HEADROOM;
  jn_tls.stack_lo = size ? lo + head : 1;
  jn_tls.stack_thr = JN_THREAD_ID();
  return jn_tls.stack_lo;
}
__attribute__((unused)) static inline uintptr_t jn_stack_limit(void) {
#ifdef JN_EXTERN_STATUS
  /* one status for every thread: only good for the thread that read it */
  return (jn_tls.stack_lo && jn_tls.stack_thr == JN_THREAD_ID()) ? jn_tls.stack_lo : jn_stack_limit_slow();
#else
  return jn_tls.stack_lo ? jn_tls.stack_lo : jn_stack_limit_slow();
#endif
}
#endif
/* Chez's 64-bit flvector layout (scheme.h Sflvector_length / Sflvector_ref) */
#define JN_FLV_LEN(x) ((int64_t)((uintptr_t)(*((intptr_t *)((uintptr_t)(x) + 1))) >> 4))
#define JN_FLV_DATA(x) ((double *)((uintptr_t)(x) + 9))
/* a bytevector: its length is shifted 3 where an flvector's is shifted 4 */
#define JN_BV_LEN(x) ((int64_t)((uintptr_t)(*((intptr_t *)((uintptr_t)(x) + 1))) >> 3))
#define JN_BV_DATA(x) ((int8_t *)((uintptr_t)(x) + 9))
/* Math.pow is Java's, not C's: a NaN exponent, or a base of magnitude 1 to an
   infinite power, is NaN where C's pow answers 1 */
static inline JN_UNUSED double jn_pow(double x, double y) {
  if (JN_ISNAN(y) || (JN_ISINF(y) && fabs(x) == 1.0)) return JN_NAN;
  return pow(x, y);
}
#endif
/* allocation hooks: the test build defines these to track every live block */
#ifndef JN_CALLOC
#define JN_CALLOC(n, s) calloc((n), (s))
#define JN_MALLOC(n) malloc(n)
#define JN_FREE(p) free(p)
#endif
")

(defn- param-c [p]
  (let [t (:type p)]
    (if (ast/borrowed? t) (str "void *p" (:id p)) (str (ctype t) " " (var-name (:id p))))))

(defn- helper-sig [inst]
  (let [t (:ret inst)]
    (str "static " (if (#{:unit :never} t) "int64_t" (ctype t)) " " (helper-name (:iid inst)) "("
         (str/join ", " (conj (mapv (fn [p] (str (ctype (:type p)) " " (var-name (:id p)))) (:params inst))
                              "jn_out *jn_st"))
         ")")))

(defn- calls-of
  "The helper instances a body calls."
  [body]
  (->> (tree-seq coll? (fn [n] (if (map? n) (vals n) n)) body)
       (keep #(when (and (map? %) (= :call (:op %))) (:instance %)))
       set))

(defn recursive-instances
  "The iids of the region's helper instances that can reach themselves through
  their calls: only those can recurse without bound."
  [region]
  (let [g (into {} (map (fn [i] [(:iid i) (apply set/union (calls-of (:body i))
                                                  (map (comp calls-of :body) (:variants i)))])
                        (:instances region)))
        reach (fn [start]
                (loop [seen #{} todo (vec (g start))]
                  (if-let [x (peek todo)]
                    (if (seen x) (recur seen (pop todo)) (recur (conj seen x) (into (pop todo) (g x))))
                    seen)))]
    (set (filter #((reach %) %) (keys g)))))

(defn- stack-guard
  "A recursive helper's entry check, before it holds anything but the arrays
  it takes, which it frees as it fails."
  [inst]
  (when (*recursive* (:iid inst))
    (let [takes (get-in @*region* [:takes (:iid inst) :positions])
          taken (keep-indexed (fn [i p] (when (contains? takes i) (:id p))) (:params inst))]
      (str "if (JN_FRAME() < jn_st->stack_lo) { " (frees taken)
           " jn_st->code = " (err-codes :stack-overflow) "; return " (ret-zero) "; }"))))

(defn- useful? [facts]
  (some (fn [f] (or (:idx-int f) (:in-bounds f) (some true? (:no-ov f)))) (vals facts)))

(defn- version-guard
  "The condition the fast copy's facts assume: each array parameter no longer
  than Integer/MAX_VALUE, and longer than the largest index proven for it."
  [{:keys [needs arrays]}]
  (str/join " && " (concat (map #(str (var-name %) ".len <= INT32_MAX") arrays)
                           (map (fn [[p hi]] (str (var-name p) ".len > " hi "LL")) (sort needs)))))

(defn- emit-versioned
  "A function body, twice when its range facts drop any check: a fast copy
  without those checks, run when the guard holds, and the fully checked copy
  otherwise. Answers the atom holding the body's value."
  [body analysis]
  (if-not (useful? (:facts analysis))
    (emit body)
    (let [t (:type body)
          res (when-not (#{:unit :never} t)
                (swap! *decls* assoc "jn_res"
                       (str (ctype t) " jn_res = " (if (ast/array-type? t) "{0, 0}" "0") ";"))
                "jn_res")
          copy (fn [facts]
                 (binding [*lines* (atom []) *fast* facts]
                   (let [v (emit body)]
                     (when res (line! res " = " v ";"))
                     @*lines*)))
          fast (copy (:facts analysis))
          g (version-guard analysis)]
      (if (str/blank? g)
        (swap! *lines* into fast)
        (let [slow (copy nil)]
          (line! "if (" g ") {")
          (swap! *lines* into fast)
          (line! "} else {")
          (swap! *lines* into slow)
          (line! "}")))
      (or res "0"))))

(defn- emit-helper-fn
  "One static C function for a helper instance, from one of its bodies. key
  names that body's range and stack facts."
  [inst body delete key]
  (binding [*decls* (atom (sorted-map))
            *lines* (atom [])
            *tmp* (atom 0)
            *fn-ret* (:ret inst)
            *st* "jn_st->"
            *stack* (get *stacks* key)]
    (let [t (:ret inst)
          _ (some-> (stack-guard inst) line!)
          v (emit-versioned body (get *ranges* key))
          lines @*lines*
          result (cond
                   (ast/owned? t) [(frees (disj delete (:id body))) (str "return " v ";")]
                   (#{:unit :never} t) [(frees delete) "return 0;"]
                   :else [(str (ctype t) " result = " v ";") (frees delete) "return result;"])]
      (str (helper-sig inst) " {\n"
           (str/join "\n" (map #(str "  " %) (concat (vals @*decls*) lines result)))
           "\n}\n"))))

;; emit-helper's platform-body emission (and defextern's :header) are ported
;; from Ferret, https://github.com/nakkaya/ferret
;; Copyright (c) 2019, Nurullah Akkaya. BSD 2-Clause: licenses/BSD-2-ferret.txt
(defn- emit-helper
  "The static C function for one native helper instance. Its array parameters
  are borrowed structs; an owned array result is returned by value.

  A helper with platform bodies (defnative's on) is emitted as Ferret emits
  one: each guarded body under its #if, defining NAME_ENABLED, the :default
  as the #else, and an #error for a platform no body covers. #elif, not one
  #if per body, so two guards that both hold cannot define it twice."
  [inst]
  (if-let [vs (seq (map-indexed vector (:variants inst)))]
    (let [enabled (str (str/upper-case (helper-name (:iid inst))) "_ENABLED")
          one (fn [[i v]] (str "#define " enabled "\n" (emit-helper-fn inst (:body v) (:delete v) [(:iid inst) i])))
          guarded (remove #(= :default (:guard (second %))) vs)
          default (first (filter #(= :default (:guard (second %))) vs))]
      (str (apply str (map-indexed (fn [k [i v]] (str (if (zero? k) "#if " "#elif ") "(" (:guard v) ")\n" (one [i v])))
                                   guarded))
           (when default (str (if (seq guarded) "#else\n" "") (one default)))
           (when (seq guarded) "#endif\n")
           "#if !defined(" enabled ")\n#error " (:sym inst) " Not Supported on This Platform\n#endif\n"))
    (emit-helper-fn inst (:body inst) (:delete inst) (:iid inst))))

(defn- extern-ctype [t]
  (case t
    :long "int64_t" :double "double" :bool "_Bool" :unit "void"
    (if (= :double (ast/elem-type t)) "double *, int64_t" "int64_t *, int64_t")))

(defn- extern-decls
  "Each C function the region calls: its header's #include (Ferret's
  native-header), or a prototype from its hints."
  [region]
  (let [all (->> (cons (:body region)
                       (mapcat #(cons (:body %) (map :body (:variants %))) (:instances region)))
                 (mapcat #(tree-seq map? (fn [n] (filter map? (concat (vals n) (mapcat (fn [v] (when (sequential? v) v)) (vals n))))) %))
                 (filter #(= :extern (:op %)))
                 (map #(select-keys % [:c :header :params :type]))
                 distinct)]
    (str (apply str (map #(str "#include " (if (str/starts-with? % "<") % (str "\"" % "\"")) "\n")
                         (distinct (keep :header all))))
         (apply str (for [x all :when (nil? (:header x))]
                      (str (extern-ctype (:type x)) " " (:c x) "("
                           (if (seq (:params x)) (str/join ", " (map extern-ctype (:params x))) "void")
                           ");\n"))))))

(defn emit-region
  "C source for a checked region, exporting sym (and sym_copy_out when it
  returns an array), with a static function per native helper instance."
  [region sym]
  (binding [*sym* sym
            *region* (atom (assoc region :loops {}))
            *ranges* (ranges/analyze-region region)
            *stacks* (stack/analyze-region region)
            *recursive* (recursive-instances region)]
    (let [helpers (str (apply str (map #(str (helper-sig %) ";\n") (:instances region)))
                       (apply str (map emit-helper (:instances region))))]
      (str prelude "\n" (extern-decls region) helpers (emit-entry region sym)))))

(defn- emit-entry
  "The region's exported entry and its status accessors."
  [region sym]
  (binding [*decls* (atom (sorted-map))
            *lines* (atom [])
            *tmp* (atom 0)
            *fn-ret* :entry
            *stack* (:entry *stacks*)]
    (let [t (:type region)
          ret-c (cond (= [:array :double] t) "double *" (= [:array :long] t) "int64_t *"
                      (#{:unit :never} t) "int" :else (ctype t))
          _ (doseq [p (:params region)]
              (when (ast/borrowed? (:type p))
                ;; an fxvector has the flvector's layout (length at +1, slots at +9)
                (swap! *decls* assoc (var-name (:id p))
                       (if (ast/byte-array-type? (:type p))
                         (str "jn_arr_b " (var-name (:id p)) " = { JN_BV_LEN(p" (:id p) "), JN_BV_DATA(p" (:id p) ") };")
                         (str (ctype (:type p)) " " (var-name (:id p)) " = { JN_FLV_LEN(p" (:id p) "), "
                              (if (fx-borrowed? (:type p)) "(int64_t *)" "") "JN_FLV_DATA(p" (:id p) ") };")))))
          _ (when (seq *recursive*) (line! "jn_stack_limit();"))
          v (emit-versioned (:body region) (:entry *ranges*))
          body @*lines*
          decls (vals @*decls*)
          result (cond
                   (ast/owned? t) [(str "jn_tls.len = " v ".len;")
                                   (frees (disj (:delete region) (:id (:body region))))
                                   (str "return " v ".data;")]
                   (#{:unit :never} t) [(frees (:delete region)) "return 0;"]
                   :else [(str (ctype t) " result = " v ";")
                          (frees (:delete region))
                          "return result;"])]
      (str ret-c " " sym "(" (if (seq (:params region)) (str/join ", " (map param-c (:params region))) "void") ") {\n"
           (str/join "\n" (map #(str "  " %) (concat ["jn_tls.code = 0;"] decls body result)))
           "\n}\n"
           "int64_t " sym "_status(void) { return jn_tls.code; }\n"
           "int64_t " sym "_err_l(int64_t k) { return k == 0 ? jn_tls.a : k == 1 ? jn_tls.b : jn_tls.len; }\n"
           "double " sym "_err_d(void) { return jn_tls.d; }\n"
           (when (= [:array :double] t)
             (str "\nvoid " sym "_copy_out(void *flv, double *src, int64_t n) {\n"
                  "  if (n > 0) memcpy(JN_FLV_DATA(flv), src, n * 8);\n"
                  "  JN_FREE(src);\n}\n"
                  ;; for the stub, when it cannot make the array to copy into
                  "void " sym "_free(double *src) { JN_FREE(src); }\n"))
           ;; a long result fills an fxvector; a value past the fixnum range
           ;; answers its index instead and the stub widens through jolt's own
           ;; array store, reading the rest with _elem and freeing with _free
           (when (= [:array :long] t)
             (str "\nint64_t " sym "_copy_out(void *fxv, int64_t *src, int64_t n) {\n"
                  "  int64_t *dst = (int64_t *)JN_FLV_DATA(fxv);\n"
                  "  for (int64_t i = 0; i < n; i++) {\n"
                  "    if (src[i] < " fixnum-min " || src[i] > " fixnum-max ") return i;\n"
                  "    dst[i] = (int64_t)((uint64_t)src[i] * 8);\n  }\n"
                  "  JN_FREE(src);\n  return -1;\n}\n"
                  "int64_t " sym "_elem(int64_t *src, int64_t i) { return src[i]; }\n"
                  "void " sym "_free(int64_t *src) { JN_FREE(src); }\n"))))))

(defn foreign-types
  "[argument-types result-type] of the region's C entry as Chez foreign types,
  strings. A borrowed array crosses as the raw flvector or fxvector
  (scheme-object): the call site keeps it from moving, by holding the thread
  active (a ^:hold-gc region) or by locking it around a __collect_safe call.
  The status comes back through sym_status, not an argument."
  [region]
  (let [arg (fn [t] (cond (ast/borrowed? t) "scheme-object"
                          (= t :long) "integer-64" (= t :double) "double" (= t :bool) "boolean"))
        t (:type region)
        ret (cond (ast/owned? t) "uptr" (#{:unit :never} t) "int"
                  (= t :long) "integer-64" (= t :double) "double" (= t :bool) "boolean")]
    [(mapv (comp arg :type) (:params region)) ret]))
