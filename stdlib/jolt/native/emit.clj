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

      RET <sym>(ARGS..., jn_out *out)

  A borrowed double array arrives as the Chez flvector itself (a raw ptr) and is
  read in place; the call must not be __collect_safe, which is what keeps the
  flvector from moving while C holds its address. An owned array result comes
  back as a malloc'd buffer the stub copies into a fresh double-array, then
  frees through <sym>_copy_out. On a failure C frees every live owned value
  (the node's :live set), fills out->code and its operands, and returns 0; the
  stub raises the matching JVM exception.

  Arithmetic follows the JVM's primitive rules exactly: a long op that leaves
  the long range raises (the C uses the overflow builtins), and a variadic op
  folds left pair by pair, so (+ Long/MAX_VALUE 1 1.5) overflows on the first
  pair as it does on the JVM. Compile the output with -ffp-contract=off: a fused
  multiply-add rounds once where Jolt rounds twice."
  (:require [clojure.string :as str]
            [jolt.native.ast :as ast]))

(def err-codes
  {:oob 1 :overflow 2 :div-zero 3 :neg-size 4 :long-range 5 :oom 6})

(def ^:dynamic *decls* nil)
(def ^:dynamic *lines* nil)
(def ^:dynamic *region* nil)
(def ^:dynamic *tmp* nil)

(defn- line! [& parts] (let [l (apply str parts)] (when-not (str/blank? l) (swap! *lines* conj l))))

(defn- ctype [t]
  (case t
    :long "int64_t" :double "double" :bool "int" :unit "int" :never "int"
    (if (ast/array-type? t)
      (if (= :double (ast/elem-type t)) "jn_arr_d" "jn_arr_l")
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
  "What a failing region returns: 0, or NULL for an array result. The stub
  reads out->code before it looks at the value."
  [] "0")

(defn- frees [ids] (str/join " " (map #(str "JN_FREE(" (var-name %) ".data);") (sort ids))))

(defn- raise! [node code & {:keys [a b d]}]
  (str "{ " (frees (:live node))
       " out->code = " (err-codes code) ";"
       (when a (str " out->a = " a ";"))
       (when b (str " out->b = " b ";"))
       (when d (str " out->d = " d ";"))
       " return " (ret-zero) "; }"))

(declare emit fresh-copy! as-double-if)

(defn- c-double-lit [v]
  (cond (Double/isNaN v) "(0.0/0.0)"
        (Double/isInfinite v) (if (pos? v) "(1.0/0.0)" "(-1.0/0.0)")
        :else (let [s (pr-str v)] (if (re-find #"[.eE]" s) s (str s ".0")))))

(defn- c-long-lit [v]
  (if (= v Long/MIN_VALUE) "INT64_MIN" (str v "LL")))

(defn- as-double [atom t] (if (= t :long) (str "((double)" atom ")") atom))

(defn- arith-pair
  "One step of a left fold: acc (type at) op b (type bt). Answers [atom type]."
  [node op [a at] [b bt]]
  (if (and (= at :long) (= bt :long))
    (let [r (fresh! :long)
          builtin (case op :+ "__builtin_add_overflow" :- "__builtin_sub_overflow"
                          :* "__builtin_mul_overflow")]
      (line! "if (" builtin "(" a ", " b ", &" r ")) " (raise! node :overflow))
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
      (:+ :- :*) (first (reduce #(arith-pair node p %1 %2) args))
      :div (first (reduce #(arith-pair node :div %1 %2)
                          (if (= 1 (count args)) (cons ["1.0" :double] args) args)))
      :neg (if (= t :long)
             (let [r (fresh! :long)]
               (line! "if (" (first one) " == INT64_MIN) " (raise! node :overflow))
               (line! r " = -" (first one) ";")
               r)
             (str "(-" (first one) ")"))
      (:inc :dec) (if (= t :long)
                    (let [r (fresh! :long)]
                      (line! "if (" (if (= p :inc) "__builtin_add_overflow" "__builtin_sub_overflow")
                             "(" (first one) ", 1, &" r ")) " (raise! node :overflow))
                      r)
                    (str "(" (first one) (if (= p :inc) " + 1.0)" " - 1.0)")))
      (:quot :rem :mod)
      ;; operands in temps: gcc refuses a literal 2LL / 0LL under -Werror even
      ;; behind the guard that makes it unreachable
      (let [[[a0 _] [b0 _]] args r (fresh! :long) a (fresh! :long) b (fresh! :long)]
        (line! a " = " a0 "; " b " = " b0 ";")
        (line! "if (" b " == 0) " (raise! node :div-zero))
        (case p
          :quot (do (line! "if (" a " == INT64_MIN && " b " == -1) " (raise! node :overflow))
                    (line! r " = " a " / " b ";"))
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
  its side effects never happen when the index overflows."
  [node idx]
  (line! "if (" idx " < INT32_MIN || " idx " > INT32_MAX) " (raise! node :overflow)))

(defn- emit-array-index
  "The bounds check, inside the store or load: after every argument ran."
  [node arr idx]
  (line! "if (" idx " < 0 || " idx " >= " arr ".len) " (raise! node :oob :a idx :b (str arr ".len"))))

(defn- bind! [id t atom]
  (let [n (declare-var! id t)]
    (line! n " = " atom ";")
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
                           (when r (line! r " = " (if (and (= t :double) (= :long (:type b)))
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
    :math (let [args (mapv (fn [a] (as-double (emit a) (:type a))) (:args node))
                r (fresh! :double)
                f (case (:fn node) "sqrt" "sqrt" "sin" "sin" "cos" "cos" "tan" "tan"
                        "exp" "exp" "log" "log" "floor" "floor" "ceil" "ceil"
                        "pow" "pow" "atan2" "atan2")]
            (line! r " = " f "(" (str/join ", " args) ");")
            r)
    ;; a temp like every other value: inlined, (aget a (alength a)) emitted
    ;; `a.len >= a.len`, which compilers flag as a tautology
    :alength (let [r (fresh! :long)] (line! r " = " (emit (:arr node)) ".len;") r)
    :aget (let [arr (emit (:arr node)) idx (emit (:idx node)) r (fresh! (:type node))]
            (emit-index-cast node idx)
            (emit-array-index node arr idx)
            (line! r " = " arr ".data[" idx "];")
            r)
    :aset (let [arr (emit (:arr node))
                idx (emit (:idx node))
                _ (emit-index-cast (assoc node :live (:live-idx node)) idx)
                v (emit (:val node))
                r (fresh! (:type node))]
            (emit-array-index node arr idx)
            (line! r " = " (as-double-if (:type node) v (:type (:val node))) ";")
            (line! arr ".data[" idx "] = " r ";")
            r)
    ;; the size goes through Number.intValue as the JVM's Numbers.T_array does:
    ;; the low 32 bits, signed, so (double-array 4294967296) is empty
    :new-array (let [raw (emit (:len node))
                     len (fresh! :long)
                     n (declare-var! (:id node) (:type node))]
                 (line! "{ int64_t lo = " raw " & 0xffffffffLL; " len " = lo >= 0x80000000LL ? lo - 0x100000000LL : lo; }")
                 (line! "if (" len " < 0) " (raise! node :neg-size :a len))
                 (line! n ".len = " len "; " n ".data = JN_CALLOC(" len " > 0 ? " len " : 1, 8);")
                 (line! "if (!" n ".data) " (raise! node :oom))
                 n)
    :aclone (let [src (emit (:arr node))
                  n (declare-var! (:id node) (:type node))]
              (line! n ".len = " src ".len; " n ".data = JN_MALLOC(" src ".len > 0 ? " src ".len * 8 : 8);")
              (line! "if (!" n ".data) " (raise! node :oom))
              (line! "memcpy(" n ".data, " src ".data, " src ".len * 8);")
              n)))

(defn- as-double-if [want v vt] (if (and (= want :double) (= vt :long)) (str "(double)" v) v))

(def prelude
  "#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
/* (== x x) on a double is how NaN is tested; the comparison is meant */
#pragma GCC diagnostic ignored \"-Wtautological-compare\"
#if UINTPTR_MAX != 0xffffffffffffffffu
#error \"jolt native regions need a 64-bit target\"
#endif
#ifndef JN_PRELUDE
#define JN_PRELUDE
typedef struct { int64_t len; double *data; } jn_arr_d;
typedef struct { int64_t len; int64_t *data; } jn_arr_l;
typedef struct { int64_t code; int64_t a; int64_t b; double d; int64_t len; } jn_out;
/* Chez's 64-bit flvector layout (scheme.h Sflvector_length / Sflvector_ref) */
#define JN_FLV_LEN(x) ((int64_t)((uintptr_t)(*((intptr_t *)((uintptr_t)(x) + 1))) >> 4))
#define JN_FLV_DATA(x) ((double *)((uintptr_t)(x) + 9))
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

(defn emit-region
  "C source for a checked region, exporting sym (and sym_copy_out when it
  returns an array)."
  [region sym]
  (binding [*decls* (atom (sorted-map))
            *lines* (atom [])
            *tmp* (atom 0)
            *region* (atom (assoc region :loops {}))]
    (let [t (:type region)
          ret-c (cond (ast/owned? t) "double *" (#{:unit :never} t) "int" :else (ctype t))
          _ (doseq [p (:params region)]
              (when (ast/borrowed? (:type p))
                (swap! *decls* assoc (var-name (:id p))
                       (str "jn_arr_d " (var-name (:id p)) " = { JN_FLV_LEN(p" (:id p) "), JN_FLV_DATA(p" (:id p) ") };"))))
          v (emit (:body region))
          body @*lines*
          decls (vals @*decls*)
          result (cond
                   (ast/owned? t) [(str "out->len = " v ".len;")
                                   (frees (disj (:delete region) (:id (:body region))))
                                   (str "return " v ".data;")]
                   (#{:unit :never} t) [(frees (:delete region)) "return 0;"]
                   :else [(str (ctype t) " result = " v ";")
                          (frees (:delete region))
                          "return result;"])]
      (str prelude "\n"
           ret-c " " sym "(" (str/join ", " (concat (map param-c (:params region)) ["jn_out *out"])) ") {\n"
           (str/join "\n" (map #(str "  " %) (concat decls body result)))
           "\n}\n"
           (when (ast/owned? t)
             (str "\nvoid " sym "_copy_out(void *flv, double *src, int64_t n) {\n"
                  "  if (n > 0) memcpy(JN_FLV_DATA(flv), src, n * 8);\n"
                  "  JN_FREE(src);\n}\n"))))))

(defn foreign-types
  "[argument-types result-type] of the region's C entry as Chez foreign types,
  strings. A borrowed array crosses as the raw flvector (scheme-object); the
  trailing u8* is the 40-byte out buffer C fills on failure. The call must not
  be __collect_safe: an active thread is what keeps the flvector from moving."
  [region]
  (let [arg (fn [t] (cond (ast/borrowed? t) "scheme-object"
                          (= t :long) "integer-64" (= t :double) "double" (= t :bool) "boolean"))
        t (:type region)
        ret (cond (ast/owned? t) "uptr" (#{:unit :never} t) "int"
                  (= t :long) "integer-64" (= t :double) "double" (= t :bool) "boolean")]
    [(conj (mapv (comp arg :type) (:params region)) "u8*") ret]))

(defn foreign-sig
  "The Chez foreign-procedure form that calls the region's C entry by name."
  [region sym]
  (let [[args ret] (foreign-types region)]
    (str "(foreign-procedure \"" sym "\" (" (str/join " " args) ") " ret ")")))
