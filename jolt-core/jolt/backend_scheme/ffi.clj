(ns jolt.backend-scheme.ffi
  "jolt.ffi lowering for the Scheme back end: foreign procedures — fixed,
  declared-tail varargs and bare-marker varargs — foreign callables, aggregate
  layouts, and the marshaling across the C boundary in both directions."
  (:require [clojure.string :as str]
            [jolt.backend-scheme.names :refer [chez-str-lit]]
            [jolt.backend-scheme.session :refer [fresh-label target]]))

;; jolt.ffi/__cfn -> a Chez foreign-procedure (jolt-ffi). The C symbol + types are
;; compile-time literals from the analyzer, so this emits a real typed binding;
;; the resulting Scheme procedure is callable like any jolt fn. The library must
;; have loaded the shared object (jolt.ffi/load-library) before this def runs.
;; Keep this in agreement with ffi-type->chez in host/chez/java/ffi.ss: these are
;; the compile-time signature and runtime memory halves of one public contract.
;; This table is baked into each checked-in compiler seed.
(def ^:private ffi-types
  {"int" "int" "uint" "unsigned-int"
   "int8" "integer-8" "i8" "integer-8"
   "int16" "integer-16" "short" "integer-16"
   "uint16" "unsigned-16" "ushort" "unsigned-16"
   "int32" "integer-32" "uint32" "unsigned-32"
   "long" "long" "ulong" "unsigned-long"
   "int64" "integer-64" "uint64" "unsigned-64" "size_t" "size_t" "ssize_t" "ssize_t"
   "iptr" "iptr" "uptr" "uptr" "double" "double" "float" "float"
   "pointer" "void*" "void*" "void*" "string" "string" "void" "void"
   "uint8" "unsigned-8" "u8" "unsigned-8" "byte" "unsigned-8" "char" "char"
   ;; :bool is a ONE-BYTE C boolean (C99 _Bool / stdbool.h), so it travels as
   ;; unsigned-8 and the value is converted at the boundary — Chez's own
   ;; `boolean` foreign type is int-sized, which is the wrong width for _Bool
   ;; and would read three bytes of whatever sat next to it on a return.
   "bool" "unsigned-8"})
(defn ffi-type->chez [t]
  (or (ffi-types t) (throw (ex-info (str "jolt.ffi: unknown foreign type :" t) {}))))

(defn- emit-ffi-layout-ftype [type]
  (cond
    (string? type) (ffi-type->chez type)
    ;; A union is the same member list under Chez's other aggregate ftype, so it
    ;; gets its size, alignment and (all-equal) member offsets from ftype-sizeof
    ;; and ftype-&ref exactly as a struct does — no layout arithmetic here.
    (or (= :struct (:ffi-kind type)) (= :union (:ffi-kind type)))
    (str "(" (if (= :union (:ffi-kind type)) "union" "struct") " "
         (str/join
          " "
          (map-indexed
           (fn [i field]
             (str "[f" i " " (emit-ffi-layout-ftype (:type field)) "]"))
           (:fields type)))
         ")")
    (= :array (:ffi-kind type))
    (str "(array " (:count type) " " (emit-ffi-layout-ftype (:type type)) ")")
    :else
    (throw (ex-info "jolt.ffi: invalid analyzed layout type" {:type type}))))

(declare ffi-layout-descendant-entries)

(def ^:private ffi-layout-index-marker ::ffi-layout-index)

(defn- ffi-layout-entry [type public-path emitted-path]
  (cons {:path public-path :emitted-path emitted-path :type type}
        (ffi-layout-descendant-entries type public-path emitted-path)))

(defn- ffi-layout-descendant-entries [type public-path emitted-path]
  (cond
    (string? type) []
    (or (= :struct (:ffi-kind type)) (= :union (:ffi-kind type)))
    (mapcat
     (fn [i field]
       (ffi-layout-entry (:type field)
                         (conj public-path (:name field))
                         (conj emitted-path (str "f" i))))
     (range (count (:fields type)))
     (:fields type))
    (= :array (:ffi-kind type))
    (ffi-layout-entry (:type type)
                      (conj public-path ffi-layout-index-marker)
                      (conj emitted-path 0))
    :else
    (throw (ex-info "jolt.ffi: invalid analyzed layout type" {:type type}))))

(defn- ffi-layout-entries
  ([layout] (ffi-layout-entries layout [] []))
  ([layout public-path emitted-path]
   (mapcat
    (fn [i field]
      (let [pp (conj public-path (:name field))
            ep (conj emitted-path (str "f" i))]
        (ffi-layout-entry (:type field) pp ep)))
    (range (count (:fields layout)))
    (:fields layout))))

(defn- emit-layout-path [path]
  (str "(jolt-vector "
       (str/join " "
                 (map (fn [part]
                        (cond
                          (= part ffi-layout-index-marker)
                          "(keyword \"jolt.ffi\" \"index\")"
                          (string? part)
                          (str "(keyword #f " (chez-str-lit part) ")")
                          :else part))
                      path))
       ")"))

(defn- ffi-layout-array-entries
  ([layout] (ffi-layout-array-entries layout []))
  ([type public-path]
   (cond
     (string? type) []
     (or (= :struct (:ffi-kind type)) (= :union (:ffi-kind type)))
     (mapcat (fn [field]
               (ffi-layout-array-entries
                (:type field) (conj public-path (:name field))))
             (:fields type))
     (= :array (:ffi-kind type))
     (cons {:path public-path :count (:count type) :element-type (:type type)}
           (ffi-layout-array-entries
            (:type type) (conj public-path ffi-layout-index-marker)))
     :else
     (throw (ex-info "jolt.ffi: invalid analyzed layout type" {:type type})))))

(defn- emit-layout-descriptor [type]
  (cond
    (string? type) (str "(keyword #f " (chez-str-lit type) ")")
    (or (= :struct (:ffi-kind type)) (= :union (:ffi-kind type)))
    (str "(jolt-vector (keyword #f "
         (chez-str-lit (if (= :union (:ffi-kind type)) "union" "struct"))
         ") (jolt-vector "
         (str/join
          " "
          (map (fn [field]
                 (str "(jolt-vector (keyword #f " (chez-str-lit (:name field)) ") "
                      (emit-layout-descriptor (:type field)) ")"))
               (:fields type)))
         "))")
    (= :array (:ffi-kind type))
    (str "(jolt-vector (keyword #f \"array\") "
         (emit-layout-descriptor (:type type)) " " (:count type) ")")
    :else
    (throw (ex-info "jolt.ffi: invalid analyzed layout type" {:type type}))))

(defn emit-ffi-layout [node]
  (let [layout (:layout node)
        type-name (fresh-label "jolt_ffi_layout")
        align-name (fresh-label "jolt_ffi_layout_align")
        entries (vec (ffi-layout-entries layout))
        arrays (mapv #(assoc % :element-name (fresh-label "jolt_ffi_layout_element"))
                     (ffi-layout-array-entries layout))
        base (str "(make-ftype-pointer " type-name " 0)")
        offsets
        (str "(jolt-hash-map "
             (str/join
              " "
              (map (fn [entry]
                     (str (emit-layout-path (:path entry)) " "
                          "(ftype-pointer-address (ftype-&ref " type-name " ("
                          (str/join " " (:emitted-path entry)) ") " base "))"))
                   entries))
             ")")
        types
        (str "(jolt-hash-map "
             (str/join
              " "
              (keep (fn [entry]
                      (when (string? (:type entry))
                        (str (emit-layout-path (:path entry)) " "
                             "(keyword #f " (chez-str-lit (:type entry)) ")")))
                    entries))
             ")")
        array-counts
        (str "(jolt-hash-map "
             (str/join
              " "
              (map (fn [entry]
                     (str (emit-layout-path (:path entry)) " " (:count entry)))
                   arrays))
             ")")
        array-strides
        (str "(jolt-hash-map "
             (str/join
              " "
              (map (fn [entry]
                     (str (emit-layout-path (:path entry))
                          " (ftype-sizeof " (:element-name entry) ")"))
                   arrays))
             ")")]
    (str "(let () "
         "(define-ftype " type-name " " (emit-ffi-layout-ftype layout) ") "
         "(define-ftype " align-name " (struct [prefix unsigned-8] [value " type-name "])) "
         (str/join
          " "
          (map (fn [entry]
                 (str "(define-ftype " (:element-name entry) " "
                      (emit-ffi-layout-ftype (:element-type entry)) ")"))
               arrays))
         (when (seq arrays) " ")
         "(jolt-hash-map "
         "(keyword \"jolt.ffi\" \"layout\") #t "
         "(keyword #f \"descriptor\") " (emit-layout-descriptor layout) " "
         "(keyword #f \"size\") (ftype-sizeof " type-name ") "
         "(keyword #f \"alignment\") "
         "(ftype-pointer-address (ftype-&ref " align-name " (value) "
         "(make-ftype-pointer " align-name " 0))) "
         "(keyword \"jolt.ffi\" \"offsets\") " offsets " "
         "(keyword \"jolt.ffi\" \"array-counts\") " array-counts " "
         "(keyword \"jolt.ffi\" \"array-strides\") " array-strides " "
         "(keyword \"jolt.ffi\" \"types\") " types "))")))

(defn- ffi-by-value? [type]
  (and (map? type) (= :by-value (:ffi-kind type))))

;; Marshaling across the C boundary, one fn per direction, shared by foreign
;; procedures (jolt calls C) and callables (C calls jolt) with the roles swapped.
;;
;; jolt -> C. A :string wraps so jolt's nil reaches C as a null char*: Chez's
;; `string` type already accepts #f for that, it just does not know jolt's nil,
;; and raised "invalid foreign-procedure argument" on the sentinel. Plenty of C
;; treats NULL as a real argument (setlocale queries, rlLoadShaderCode's default
;; vertex shader), and binding those as :pointer to get NULL through loses the
;; string marshaling on that parameter. :bool is one byte on the wire and jolt
;; truthiness decides it, so nil and false send 0 and everything else sends 1.
;; (float x) is a boxed java.lang.Float, and a Chez float or double position
;; takes only a flonum.
(defn- ffi-jolt->c [type expr]
  (cond
    (= "string" type) (str "(jolt-ffi-string->c " expr ")")
    (= "bool" type) (str "(jolt-ffi-bool->c " expr ")")
    (#{"float" "double"} type) (str "(jolt-ffi-float->c " expr ")")
    :else expr))

;; C -> jolt. Chez hands back #f for a NULL char*, which is Scheme's false, not
;; jolt's nil (getenv of an unset name returned false to Clojure before this). A
;; C float reaches Clojure on the JVM as a java.lang.Float; Chez hands over the
;; single-precision flonum, so it is boxed as it is (the constructor, not a
;; helper: one call less). A double is already a jolt double.
(defn- ffi-c->jolt [type expr]
  (cond
    (= "string" type) (str "(jolt-ffi-c->string " expr ")")
    (= "bool" type) (str "(jolt-ffi-c->bool " expr ")")
    (= "float" type) (str "(make-jfloat " expr ")")
    :else expr))

;; A :capture-native-error binding's procedure returns two values, the result and
;; the errno/GetLastError captured right after the call; jolt sees them as a
;; [result native-error] vector.
(defn- ffi-capture-result [rettype call]
  (str "(call-with-values (lambda () " call ")"
       " (lambda (result native-error)"
       " (jolt-vector " (ffi-c->jolt rettype "result") " native-error)))"))

(defn- ffi-params [n]
  (mapv (fn [i] (str "a" i)) (range n)))

;; The BARE marker — [:string :int :&], babashka.ffi's form where no types follow
;; and each call's tail is inferred from the values it is given. A
;; foreign-procedure's types are fixed when it is compiled, so this cannot be one
;; procedure: it lowers to a variadic lambda over a per-binding cache, which
;; compiles one foreign-procedure per observed tail shape on first sight and
;; reuses it after (jolt-ffi-varargs-* in host/chez/java/ffi.ss, where the carrier
;; table and the costs are written down).
;;
;; SHAPE OF THE EMITTED BINDING. A rest-argument lambda would make Chez allocate
;; a list for the tail on every call, and the dispatcher would then walk it twice
;; (once to key the shape, once to marshal) and spread it back with `apply` —
;; about 18ns per tail value, which is most of what a bare marker costs. A tail
;; of nought to three values is the whole of real usage, so those arities get
;; their own case-lambda arms that carry the tail as ARGUMENTS: no list is built,
;; nothing is walked, and the foreign procedure is called directly. Longer tails
;; fall through to a rest arm that still works the general way.
;;
;; The arms and the general path agree on the cache key by construction — the
;; arity-specialized lookups are the same base-3 fold, unrolled.
(defn- emit-ffi-bare-varargs-fn [node vi]
  (let [fixed (subvec (:argtypes node) 0 vi)
        n (count fixed)
        params (ffi-params n)
        rettype (:rettype node)
        capture (:capture-native-error node)
        cache-name (fresh-label "jolt_ffi_varargs")
        tail-name (fresh-label "jolt_ffi_tail")
        ;; The fixed arguments convert exactly as they do in a declared-tail
        ;; binding; only the tail is inferred.
        native-args (mapv ffi-jolt->c fixed params)
        fixed-args (if (seq native-args) (str " " (str/join " " native-args)) "")
        cache (str "(jolt-ffi-varargs-cache " (chez-str-lit (:csym node))
                   " (quote (" (str/join " " (map ffi-type->chez fixed)) ")) "
                   "(quote " (ffi-type->chez rettype) ") " n " "
                   (if capture "#t" "#f") ")")
        wrap (fn [call]
               (if capture
                 (ffi-capture-result rettype call)
                 (ffi-c->jolt rettype call)))
        ;; tail names t1..tk for the specialized arms
        tvars (fn [k] (mapv (fn [i] (str tail-name "_" (inc i))) (range k)))
        arm (fn [k]
              (let [ts (tvars k)]
                (str "((" (str/join " " (concat params ts)) ") "
                     (wrap (str "((jolt-ffi-varargs-proc" k " " cache-name
                                (when (seq ts) (str " " (str/join " " ts))) ")"
                                fixed-args
                                (str/join "" (map (fn [t] (str " (jolt-ffi-varargs-arg " t ")")) ts))
                                ")"))
                     ")")))
        rest-arm (str "((" (str/join " " params) " . " tail-name ") "
                      (wrap (str "(apply (jolt-ffi-varargs-procedure " cache-name " " tail-name ")"
                                 fixed-args
                                 " (jolt-ffi-varargs-tail " tail-name "))"))
                      ")")]
    (str "(let ((" cache-name " " cache ")) "
         "(case-lambda " (str/join " " (map arm (range 4))) " " rest-arm "))")))

;; A "varargs" marker in the argtype vector declares the binding variadic and
;; marks the FIXED/VARIADIC boundary: types before it are the named parameters,
;; types after it are the concrete variadic arguments this binding always passes.
;; The call is emitted with Chez's (__varargs_after n) convention (n = the
;; fixed-arg count = the marker's index), so the variadic arguments travel where
;; the callee's va_list reads them — Apple arm64 passes variadic args on the
;; stack, and a fixed-arity binding silently corrupts them (fcntl, ioctl, open).
;;
;; :& is babashka.ffi's spelling of the same marker and means the same thing
;; here, so a signature written for either FFI declares the same call.
(defn- ffi-varargs-index [argtypes]
  (first (keep-indexed (fn [i type] (when (or (= type "varargs") (= type "&")) i))
                       argtypes)))

;; Every shape emit-ffi-fn cannot compile, refused with a message that names the
;; option and the position rather than surfacing Chez's own error from inside the
;; foreign-procedure macro.
(defn- check-ffi-signature! [node vi]
  (let [at (:argtypes node)
        marker (when vi (str ":" (nth at vi)))
        bare? (and vi (= vi (dec (count at))))]
    ;; C requires a named parameter before the ellipsis.
    (when (and vi (zero? vi))
      (throw (ex-info (str "jolt.ffi: " marker " needs at least one fixed argtype before it")
                      {:argtypes at})))
    (when (and bare? (some ffi-by-value? (subvec at 0 vi)))
      (throw (ex-info (str "jolt.ffi: a fixed by-value aggregate cannot combine with a bare "
                           marker " — the tail is compiled at the call, and an aggregate"
                           " argument needs an ftype the binding declared. Declare the tail"
                           " after the marker.")
                      {:argtypes at})))
    ;; __collect_safe cannot combine with a varargs convention.
    (when (and vi (:blocking node))
      (throw (ex-info (str "jolt.ffi: " marker " cannot combine with :blocking")
                      {:argtypes at})))
    ;; Chez refuses a string ARGUMENT on a __collect_safe procedure: the string
    ;; is marshalled into collector-owned memory whose address C holds for the
    ;; whole call, and a DEACTIVATED thread is exactly the one that cannot keep
    ;; the collector off it. Its own message ("string argument not allowed with
    ;; __collect_safe procedure") arrives from inside the macro, with a chi-*
    ;; expander trace and no mention of the option that caused it — and
    ;; :blocking is the first thing one reaches for after a callback stalls
    ;; behind a call that pinned the collector (issue #973), so name the
    ;; argument and what to pass instead. A string RESULT is fine: it is built
    ;; after the thread is active again.
    (when-let [i (and (:blocking node)
                      (first (keep-indexed (fn [i type] (when (= "string" type) i)) at)))]
      (throw (ex-info (str "jolt.ffi: :blocking cannot combine with a :string argument"
                           " (argument " i " of \"" (:csym node) "\") — the collector may"
                           " move the string while the deactivated thread is inside C."
                           " Pass a :pointer instead: jolt.ffi/string->ptr, or an"
                           " arena-owned string, released once the call returns.")
                      {:argtypes at :argument i})))
    (when (and vi (ffi-by-value? (:rettype node)))
      (throw (ex-info (str "jolt.ffi: aggregate returns cannot combine with " marker)
                      {:argtypes at})))
    (when (and vi (some ffi-by-value? (subvec at (inc vi))))
      (throw (ex-info "jolt.ffi: aggregate variadic arguments are not supported"
                      {:argtypes at})))))

;; A by-value aggregate travels as the address of jolt-owned memory, viewed
;; through the ftype the binding declares for it.
(defn- ffi-aggregate-pointer [param type-name role]
  (str "(let ((address (jnum->exact " param "))) "
       "(if (= address 0) "
       "(throw-jvm 'NullPointerException "
       (chez-str-lit (str "jolt.ffi: null by-value " role " pointer")) ") "
       "(make-ftype-pointer " type-name " address)))"))

;; The expression that yields the binding's foreign procedure, resolved on first
;; call and cached in the binding's `p`.
;;
;; Resolution order is the same for every binding, variadic or not: a declared
;; :jolt/native's own dlopen handle first, the process-global table second. A
;; scalar-varargs binding used to skip the scoped branch, which meant a variadic
;; symbol in a library loaded with load-library could not be bound AT ALL --
;; RTLD_LOCAL keeps it out of the global table, so the name found nothing and the
;; call raised "no entry". curl_easy_setopt is that case. The address+convention
;; form this emits is the one the aggregate C witness already covers on each
;; target ABI.
(defn- ffi-procedure-expr [node vi signature]
  (let [csym (chez-str-lit (:csym node))
        capture (:capture-native-error node)
        blocking (:blocking node)
        conv (if vi (str " (__varargs_after " vi ")") "")
        capture-conv (cond
                       blocking "__collect_safe"
                       ;; The outer wrapper already adds one list. A compound
                       ;; convention needs one more so the adapter receives
                       ;; (__varargs_after n) as one datum rather than two
                       ;; flat convention arguments.
                       vi (str "(__varargs_after " vi ")")
                       :else "")
        by-name (if capture
                  (str "(jolt-ffi-native-error-procedure (" capture-conv ") "
                       csym signature ")")
                  (str "(" (if blocking
                              "sa-foreign-procedure-blocking "
                              "sa-foreign-procedure ")
                       conv " " csym signature ")"))
        scoped (str "(let ((a (jolt-ffi-dlsym-native " csym "))) "
                    "(and a "
                    (if capture
                      (str "(jolt-ffi-native-error-procedure (" capture-conv ") a"
                           signature ")")
                      (str "(foreign-procedure"
                           (when blocking " __collect_safe")
                           (when vi conv)
                           " a" signature ")"))
                    "))")]
    (str "(or p (begin (set! p (or " scoped " " by-name ")) p))")))

;; A binding with a declared argument tail (or none): one foreign procedure,
;; wrapped in a lambda that marshals each argument and the result. By-value
;; aggregates get an ftype each, defined once around the lambda; an aggregate
;; RESULT is written into a caller-supplied destination passed ahead of the
;; arguments, and that destination is what the call returns.
(defn- ffi-aggregate-args
  "Index -> {:name :type} for each by-value aggregate argument, each with a fresh
  ftype name."
  [types]
  (->> types
       (map-indexed (fn [i type]
                      (when (ffi-by-value? type)
                        [i {:name (fresh-label "jolt_ffi_arg") :type (:type type)}])))
       (remove nil?)
       (into (sorted-map))))

;; The Chez signature: each argument's foreign type, an aggregate as a pointer to
;; its ftype, then the result's.
(defn- ffi-signature [types aggregates emitted-return]
  (let [emitted-types (map-indexed (fn [i type]
                                     (if-let [entry (get aggregates i)]
                                       (str "(& " (:name entry) ")")
                                       (ffi-type->chez type)))
                                   types)]
    (str " (" (str/join " " emitted-types) ") " emitted-return)))

;; The define-ftype forms for the aggregates a binding passes or returns.
(defn- ffi-ftype-defs [aggregates return-name rettype]
  (concat
   (map (fn [entry]
          (str "(define-ftype " (:name entry) " "
               (emit-ffi-layout-ftype (:type entry)) ")"))
        (vals aggregates))
   (when return-name
     [(str "(define-ftype " return-name " "
           (emit-ffi-layout-ftype (:type rettype)) ")")])))

(defn- emit-ffi-fixed-fn [node vi]
  (let [at (:argtypes node)
        rettype (:rettype node)
        ret-aggregate? (ffi-by-value? rettype)
        types (if vi (vec (concat (subvec at 0 vi) (subvec at (inc vi)))) at)
        params (ffi-params (count types))
        return-param (when ret-aggregate? "destination")
        aggregates (ffi-aggregate-args types)
        return-name (when ret-aggregate? (fresh-label "jolt_ffi_return"))
        signature (ffi-signature types aggregates
                                 (if ret-aggregate?
                                   (str "(& " return-name ")")
                                   (ffi-type->chez rettype)))
        native-args (vec (map-indexed
                          (fn [i param]
                            (if-let [entry (get aggregates i)]
                              (ffi-aggregate-pointer param (:name entry) "aggregate")
                              (ffi-jolt->c (nth types i) param)))
                          params))
        call-args (if ret-aggregate?
                    (into [(ffi-aggregate-pointer return-param return-name "return destination")]
                          native-args)
                    native-args)
        call (str "(" (ffi-procedure-expr node vi signature)
                  (when (seq call-args) (str " " (str/join " " call-args))) ")")
        body (cond
               (:capture-native-error node) (ffi-capture-result rettype call)
               ret-aggregate? (str "(begin " call " " return-param ")")
               :else (ffi-c->jolt rettype call))
        wrapper-params (if return-param (cons return-param params) params)
        binding (str "(let ((p #f)) (lambda (" (str/join " " wrapper-params) ") " body "))")
        ftypes (ffi-ftype-defs aggregates return-name rettype)]
    (if (seq ftypes)
      (str "(let () " (str/join " " ftypes) " " binding ")")
      binding)))

;; jolt.ffi/__fn -> the binding's lambda. The one half of the babashka.ffi varargs
;; convention a declared tail cannot express is the BARE marker, [:string :int :&],
;; where each call infers its own tail from the values, since a
;; foreign-procedure's types are fixed when it is compiled; that shape gets a
;; per-binding cache of procedures instead (emit-ffi-bare-varargs-fn). Only the
;; non-blocking path takes a varargs marker: __collect_safe cannot combine with a
;; varargs convention.
(defn emit-ffi-fn [node]
  (let [at (:argtypes node)
        vi (ffi-varargs-index at)]
    (check-ffi-signature! node vi)
    (if (and vi (= vi (dec (count at))))
      (emit-ffi-bare-varargs-fn node vi)
      (emit-ffi-fixed-fn node vi))))

;; jolt.ffi/__ccallable -> a Chez foreign-callable wrapping the emitted jolt fn,
;; locked + registered (jolt-ffi-register-callable!, host/chez/java/ffi.ss) so the
;; collector neither moves nor reclaims it while C may still call through it. The
;; expression evaluates to the entry-point address — a jolt pointer the caller
;; hands to C. :collect-safe emits the convention that reactivates the thread on
;; entry, for a callback arriving on an inactive thread (see jolt/ffi.clj).
;;
;; A :string position on a callback needs the same NULL translation a foreign-fn
;; gets, with the two directions swapped: C is the CALLER here, so a :string
;; argument converts c->jolt (a null char* arrived as #f, which is jolt false,
;; not nil) and a :string result converts jolt->c (returning nil raised "invalid
;; return value" instead of handing C a null char*). Callbacks are where a C API
;; hands back an optional string — a null path, name or error is ordinary — so
;; the callback could not model the argument at all, and could not decline to
;; answer one.
;;
;; The wrapper lambda is emitted only when the signature has a :string, :bool or
;; :float position or a :double result; every other callable reaches
;; sa-foreign-callable exactly as before. EMIT lowers the wrapped fn node (the
;; back end's own emit: this namespace sits below it).
(defn emit-ffi-callable [emit node]
  ;; The mirror of the :blocking/:string rule in emit-ffi-fn, with the direction
  ;; swapped: on a __collect_safe CALLABLE it is the string RESULT Chez refuses,
  ;; because the callable hands C that string's address as it deactivates the
  ;; thread on the way out. Arguments are fine — C owns those bytes. Same reason
  ;; for saying it here: Chez's message names neither :collect-safe nor the
  ;; position, and arrives under an expander trace.
  (when (and (:collect-safe node) (= "string" (:rettype node)))
    (throw (ex-info (str "jolt.ffi: a :collect-safe callback cannot return :string —"
                         " the collector may move it while C holds the pointer."
                         " Return a :pointer instead: jolt.ffi/string->ptr, or an"
                         " arena-owned string whose lifetime C's use fits inside.")
                    {:rettype (:rettype node) :argtypes (:argtypes node)})))
  (let [argtypes (:argtypes node)
        rettype (:rettype node)
        converted? #{"string" "bool"}
        ;; A float or double RESULT converts too: the fn may answer (float x), a
        ;; boxed Float. A :float argument is boxed into one, as the JVM hands a
        ;; C float to Clojure; a :double argument is already a double.
        converted-position? (or (converted? rettype) (some converted? argtypes)
                                (#{"float" "double"} rettype)
                                (some #{"float"} argtypes))
        target
        (if-not converted-position?
          (emit (:fn node))
          ;; The fn is bound to a fresh name rather than inlined into the lambda
          ;; body so it is evaluated once, at callable-construction time, not on
          ;; every call C makes through the entry point.
          (let [fname (fresh-label "jolt_ffi_cb")
                params (ffi-params (count argtypes))
                args (mapv (fn [param type] (ffi-c->jolt type param)) params argtypes)
                invoke (str "(" fname (when (seq args) (str " " (str/join " " args))) ")")]
            (str "(let ((" fname " " (emit (:fn node)) ")) "
                 "(lambda (" (str/join " " params) ") "
                 (ffi-jolt->c rettype invoke)
                 "))")))
        ;; A :collect-safe callable counts itself in and out (java/ffi.ss
        ;; jolt-ffi-callback-enter!/exit!, a CAS each) so a collection stalled
        ;; by a non-:blocking outbound call can report that a callback is among
        ;; the threads waiting for it (rt.ss jolt-report-gc-stall, issue #1046).
        ;; Bound once, at callable construction; the count is per call, and
        ;; only for this convention — a callback on the calling thread pays
        ;; nothing. The body returns to C by returning (no continuation crosses
        ;; the foreign frame), so the exit always runs.
        ;;
        ;; The entry lambda is bound under one fixed name so its code object
        ;; carries it: a backtrace or a stack sample taken on the library's
        ;; thread then reads jolt-ffi-collect-safe-entry where it would read an
        ;; anonymous frame.
        target
        (if (:collect-safe node)
          (let [fname (fresh-label "jolt_ffi_cs")
                params (mapv (fn [i] (str "c" i)) (range (count argtypes)))]
            (str "(let* ((" fname " " target ") "
                 "(jolt-ffi-collect-safe-entry "
                 "(lambda (" (str/join " " params) ") "
                 "(jolt-ffi-callback-enter!) "
                 "(let ((r (" fname (when (seq params) (str " " (str/join " " params))) "))) "
                 "(jolt-ffi-callback-exit!) r)))) "
                 "jolt-ffi-collect-safe-entry)"))
          target)]
    (str "(jolt-ffi-register-callable! ("
         (if (:collect-safe node) "sa-foreign-callable-collect-safe " "sa-foreign-callable ")
         target
         " (" (str/join " " (map ffi-type->chez argtypes)) ") "
         (ffi-type->chez rettype) "))")))

