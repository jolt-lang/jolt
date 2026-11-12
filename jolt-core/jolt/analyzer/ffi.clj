(ns jolt.analyzer.ffi
  "The literal-only jolt.ffi forms: struct/union/array layout descriptors and the
  __cfn foreign-function signature, validated at compile time into the data the
  back end lowers to Chez ftypes and foreign-procedures."
  (:require [jolt.host :refer [form-vec? form-vec-items form-keyword? form-map?
                               form-map-pairs]]
            [jolt.analyzer.diagnostics :refer [analysis-error]]))

;; Literal, data-only layout descriptors. Keep the analyzer representation free
;; of reader objects so it survives self-hosting and can be embedded in the IR.
(def ^:private ffi-layout-scalars
  #{"int" "uint" "int8" "i8" "uint8" "u8" "byte" "char" "bool"
    "int16" "short" "uint16" "ushort" "int32" "uint32"
    "long" "ulong" "int64" "uint64" "size_t" "ssize_t" "iptr" "uptr"
    "double" "float" "pointer" "void*"})

(declare analyze-ffi-layout-type)

(defn- ffi-layout-form-kind [form]
  (when (form-vec? form)
    (let [parts (vec (form-vec-items form))
          head (first parts)]
      (when (and (form-keyword? head) (nil? (namespace head)))
        (name head)))))

;; A struct and a union differ only in how the ABI lays the members out, so one
;; analyzer reads both and the :ffi-kind it answers is what the back end turns
;; into Chez's (struct ...) or (union ...) ftype. Everything downstream — offsets,
;; size, alignment, field paths — is computed by the SAME ftype machinery, so a
;; union needs no arithmetic of its own here.
(defn- analyze-ffi-layout-aggregate [form]
  (let [kind (ffi-layout-form-kind form)
        kind (when (or (= "struct" kind) (= "union" kind)) kind)]
    (when-not (form-vec? form)
      (analysis-error :ffi/invalid-layout (str "jolt.ffi layout descriptor must be [:struct [[field type] ...]] "
                  "or [:union [[field type] ...]], got " (pr-str form))))
    (let [parts (vec (form-vec-items form))]
      (when-not (and kind (= 2 (count parts)) (form-vec? (nth parts 1)))
        (analysis-error :ffi/invalid-layout (str "jolt.ffi layout descriptor must be [:struct [[field type] ...]] "
                    "or [:union [[field type] ...]], got " (pr-str form))))
      (let [field-forms (vec (form-vec-items (nth parts 1)))]
        (when (empty? field-forms)
          (analysis-error :ffi/invalid-layout (str "jolt.ffi " kind " descriptor must contain at least one field")))
        (loop [remaining field-forms names #{} fields []]
          (if (empty? remaining)
            {:ffi-kind (if (= "union" kind) :union :struct) :fields fields}
            (let [field (first remaining)]
              (when-not (form-vec? field)
                (analysis-error :ffi/invalid-field (str "jolt.ffi " kind " field must be [keyword type], got "
                            (pr-str field))))
              (let [fp (vec (form-vec-items field))]
                (when-not (= 2 (count fp))
                  (analysis-error :ffi/invalid-field (str "jolt.ffi " kind " field must be [keyword type], got "
                              (pr-str field))))
                (let [field-name (nth fp 0)]
                  (when-not (and (form-keyword? field-name)
                                 (nil? (namespace field-name)))
                    (analysis-error :ffi/invalid-field (str "jolt.ffi " kind " field name must be an unqualified keyword, got "
                                (pr-str field-name))))
                  (let [nm (name field-name)]
                    (when (contains? names nm)
                      (analysis-error :ffi/invalid-field (str "jolt.ffi " kind " field names must be unique; duplicate :" nm)))
                    (recur (rest remaining)
                           (conj names nm)
                           (conj fields {:name nm
                                         :type (analyze-ffi-layout-type (nth fp 1))}))))))))))))

(defn- analyze-ffi-layout-array [form]
  (let [parts (vec (form-vec-items form))]
    (when-not (= 3 (count parts))
      (analysis-error :ffi/invalid-array (str "jolt.ffi array descriptor must be [:array element-type positive-count], got "
                  (pr-str form))))
    (let [count (nth parts 2)]
      (when-not (and (integer? count) (pos? count))
        (analysis-error :ffi/invalid-array (str "jolt.ffi array count must be a positive integer literal, got "
                    (pr-str count))))
      {:ffi-kind :array
       :count count
       :type (analyze-ffi-layout-type (nth parts 1))})))

(defn- analyze-ffi-layout-type [form]
  (cond
    (form-keyword? form)
    (let [n (name form)]
      (when-not (and (nil? (namespace form)) (contains? ffi-layout-scalars n))
        (analysis-error :ffi/invalid-type (str "jolt.ffi struct field type must be a fixed-size scalar, nested struct or union, or fixed array; got "
                    (pr-str form))))
      n)

    (form-vec? form)
    (case (ffi-layout-form-kind form)
      "struct" (analyze-ffi-layout-aggregate form)
      "union" (analyze-ffi-layout-aggregate form)
      "array" (analyze-ffi-layout-array form)
      (analysis-error :ffi/invalid-type (str "jolt.ffi struct field type must be a fixed-size scalar, nested struct or union, or fixed array; got "
                  (pr-str form))))

    :else
    (analysis-error :ffi/invalid-type (str "jolt.ffi struct field type must be a fixed-size scalar, nested struct or union, or fixed array; got "
                (pr-str form)))))

(defn analyze-ffi-layout [items]
  (when-not (= 2 (count items))
    (analysis-error :ffi/invalid-layout "jolt.ffi/layout expects one literal struct or union descriptor"))
  {:op :ffi-layout :layout (analyze-ffi-layout-aggregate (nth items 1))})

(defn- ffi-by-value-form? [form]
  (when (form-vec? form)
    (let [parts (vec (form-vec-items form))]
      (and (= 2 (count parts))
           (form-keyword? (nth parts 0))
           (nil? (namespace (nth parts 0)))
           (= "by-value" (name (nth parts 0)))))))

;; A union has no by-value ABI a caller can rely on: which member is live is the
;; program's knowledge, not the type's, and the register classification of the
;; bytes differs by which one it is. babashka.ffi refuses it in a signature for
;; the same reason. So a union reaches C as a :pointer, alone or as a member of
;; a struct that would otherwise travel by value.
(defn- ffi-layout-holds-union? [type]
  (cond
    (string? type) false
    (= :union (:ffi-kind type)) true
    (= :struct (:ffi-kind type))
      (boolean (some (fn [f] (ffi-layout-holds-union? (:type f))) (:fields type)))
    (= :array (:ffi-kind type)) (ffi-layout-holds-union? (:type type))
    :else false))

(defn- analyze-ffi-signature-type [form position]
  (cond
    (form-keyword? form) (name form)
    (ffi-by-value-form? form)
      (let [parts (vec (form-vec-items form))
            analyzed (analyze-ffi-layout-aggregate (nth parts 1))]
        (when (ffi-layout-holds-union? analyzed)
          (analysis-error :ffi/invalid-type (str "jolt.ffi " position
                      " type: a union is not passed by value, alone or inside a struct"
                      " — declare :pointer and read the member you know applies")))
        {:ffi-kind :by-value
         :type analyzed})
    :else
      (analysis-error :ffi/invalid-type (str "jolt.ffi " position
                  " type must be a keyword or [:by-value [:struct ...]], got "
                  (pr-str form)))))

;; jolt.ffi/__cfn: the low-level foreign-function form a jolt library
;; uses (via the jolt.ffi/foreign-fn macro) to bind native code. Shape:
;;   (jolt.ffi/__cfn "c_symbol" [:argtype ...] :rettype)             ; non-blocking
;;   (jolt.ffi/__cfn "c_symbol" [:argtype ...] :rettype :blocking)   ; may block
;;   (jolt.ffi/__cfn "c_symbol" [:argtype ...] :rettype {opts})      ; options map
;; The C symbol is a string literal and the types are literal keywords, read here
;; at compile time; the Chez back end lowers it to a real `foreign-procedure`
;; (typed marshaling, no runtime eval). A :blocking call is emitted __collect_safe
;; so it deactivates the thread for the call — a blocking call (accept/recv/...)
;; must not pin the stop-the-world collector. A leaf IR node.
;;
;; Normalize the optional form to the two literal Boolean flags understood by
;; the back end. Direct __cfn calls and the public macros share this fail-closed
;; validation: keys must be unqualified keywords, values literal Booleans, and
;; unknown keys are rejected rather than ignored.
(defn- ffi-option
  ([]
   {:blocking false :capture-native-error false})
  ([opt]
   (cond
     (and (form-keyword? opt)
          (nil? (namespace opt))
          (= "blocking" (name opt)))
     {:blocking true :capture-native-error false}

     (form-map? opt)
     (reduce
       (fn [res pr]
         (let [k (nth pr 0) v (nth pr 1)]
           (when-not (and (form-keyword? k) (nil? (namespace k)))
             (analysis-error :ffi/invalid-option (str "jolt.ffi: option key must be an unqualified keyword, got: " k)))
           (let [kn (name k)]
             (when-not (or (= kn "blocking") (= kn "capture-native-error"))
               (analysis-error :ffi/invalid-option (str "jolt.ffi: unknown option :" kn)))
             (when-not (or (true? v) (false? v))
               (analysis-error :ffi/invalid-option (str "jolt.ffi: option :" kn
                           " must be a literal Boolean, got: " v)))
             (assoc res
                    (if (= kn "blocking") :blocking :capture-native-error)
                    v))))
       {:blocking false :capture-native-error false}
       (form-map-pairs opt))

     :else
     (analysis-error :ffi/invalid-option (str "jolt.ffi: option must be :blocking or an options map, got: " opt)))))

(defn analyze-ffi-fn [ctx items env]
  (when-not (<= 4 (count items) 5)
    (analysis-error :ffi/invalid-signature (str "jolt.ffi/foreign-fn expects "
                "(foreign-fn \"sym\" [argtypes] rettype [:blocking | {opts}])")))
  (let [rettype (analyze-ffi-signature-type (nth items 3) "return")
        opt (if (= 5 (count items))
              (ffi-option (nth items 4))
              (ffi-option))
        blocking (:blocking opt)
        capture (:capture-native-error opt)]
    (when (and capture (= rettype "void"))
      (analysis-error :ffi/invalid-option (str "jolt.ffi: :capture-native-error is not supported for :void "
                  "(no stable native result to pair with the error code)")))
    (when (and capture (map? rettype))
      (analysis-error :ffi/invalid-option "jolt.ffi: :capture-native-error is not supported for by-value returns"))
    {:op :ffi-fn
     :csym (nth items 1)
     :argtypes (mapv #(analyze-ffi-signature-type % "argument")
                     (form-vec-items (nth items 2)))
     :rettype rettype
     :blocking blocking
     :capture-native-error capture}))
