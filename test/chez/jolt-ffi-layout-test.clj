(ns jolt-ffi-layout-test)

(require '[jolt.ffi :as ffi])

(def helper (System/getenv "JOLT_FFI_LAYOUT_HELPER"))
(when-not helper
  (throw (ex-info "JOLT_FFI_LAYOUT_HELPER is required" {})))
(ffi/load-library helper)

(ffi/defcfn flat-size "jolt_layout_flat_size" [] :size_t)
(ffi/defcfn flat-align "jolt_layout_flat_align" [] :size_t)
(ffi/defcfn flat-month "jolt_layout_flat_month" [] :size_t)
(ffi/defcfn padded-size "jolt_layout_padded_size" [] :size_t)
(ffi/defcfn padded-align "jolt_layout_padded_align" [] :size_t)
(ffi/defcfn padded-value "jolt_layout_padded_value" [] :size_t)
(ffi/defcfn nested-date "jolt_layout_nested_date" [] :size_t)
(ffi/defcfn nested-year "jolt_layout_nested_year" [] :size_t)
(ffi/defcfn arrays-size "jolt_layout_arrays_size" [] :size_t)
(ffi/defcfn arrays-align "jolt_layout_arrays_align" [] :size_t)
(ffi/defcfn arrays-params "jolt_layout_arrays_params" [] :size_t)
(ffi/defcfn arrays-params-3 "jolt_layout_arrays_params_3" [] :size_t)
(ffi/defcfn arrays-name-4 "jolt_layout_arrays_name_4" [] :size_t)
(ffi/defcfn arrays-dates-1-year "jolt_layout_arrays_dates_1_year" [] :size_t)
(ffi/defcfn arrays-matrix-1-2 "jolt_layout_arrays_matrix_1_2" [] :size_t)

(def failures (atom []))
(defmacro check [label expr]
  `(when-not ~expr (swap! failures conj ~label)))
(defn rejects? [f]
  (try (f) false (catch Throwable _ true)))

(def date-layout
  (ffi/layout [:struct [[:year :int32] [:month :uint8] [:day :uint8]]]))
(def padded-layout
  (ffi/layout [:struct [[:tag :uint8] [:value :double] [:tail :uint16]]]))
(def nested-layout
  (ffi/layout [:struct [[:tag :uint8]
                       [:date [:struct [[:year :int32] [:month :uint8] [:day :uint8]]]]
                       [:tail :uint16]]]))
(def arrays-layout
  (ffi/layout [:struct [[:tag :uint8]
                        [:params [:array :float 4]]
                        [:name [:array :char 5]]
                        [:dates [:array [:struct [[:year :int32]
                                                    [:month :uint8]
                                                    [:day :uint8]]] 2]]
                        [:matrix [:array [:array :uint16 3] 2]]
                        [:tail :uint16]]]))
(def huge-array-layout
  (ffi/layout [:struct [[:prefix :uint8]
                        [:payload [:array :uint8 1000000]]
                        [:suffix :uint8]]]))
(def huge-matrix-layout
  (ffi/layout [:struct [[:matrix [:array [:array :uint8 1000] 1000]]]]))

(check "flat size" (= (flat-size) (ffi/layout-size date-layout)))
(check "flat alignment" (= (flat-align) (ffi/layout-alignment date-layout)))
(check "keyword path" (= (flat-month) (ffi/field-offset date-layout :month)))
(check "padded size" (= (padded-size) (ffi/layout-size padded-layout)))
(check "padded alignment" (= (padded-align) (ffi/layout-alignment padded-layout)))
(check "padded field offset" (= (padded-value) (ffi/field-offset padded-layout :value)))
(check "nested struct offset" (= (nested-date) (ffi/field-offset nested-layout [:date])))
(check "nested scalar offset" (= (nested-year) (ffi/field-offset nested-layout [:date :year])))
(check "arrays size" (= (arrays-size) (ffi/layout-size arrays-layout)))
(check "arrays alignment" (= (arrays-align) (ffi/layout-alignment arrays-layout)))
(check "array container offset" (= (arrays-params) (ffi/field-offset arrays-layout :params)))
(check "array scalar offset" (= (arrays-params-3) (ffi/field-offset arrays-layout [:params 3])))
(check "char array element offset" (= (arrays-name-4) (ffi/field-offset arrays-layout [:name 4])))
(check "struct array field offset"
       (= (arrays-dates-1-year) (ffi/field-offset arrays-layout [:dates 1 :year])))
(check "nested array offset"
       (= (arrays-matrix-1-2) (ffi/field-offset arrays-layout [:matrix 1 2])))
(check "descriptor data" (= [:struct [[:year :int32] [:month :uint8] [:day :uint8]]]
                            (:descriptor date-layout)))
(check "million-element array metadata stays compact"
       (= [4 1 1]
          [(count (:jolt.ffi/offsets huge-array-layout))
           (count (:jolt.ffi/array-counts huge-array-layout))
           (count (:jolt.ffi/array-strides huge-array-layout))]))
(check "million-element array resolves its final element arithmetically"
       (= [1000002 1000000 1000001]
          [(ffi/layout-size huge-array-layout)
           (ffi/field-offset huge-array-layout [:payload 999999])
           (ffi/field-offset huge-array-layout :suffix)]))
(check "million-element nested array metadata stays shape-sized"
       (= [3 2 2 1000000 999999]
          [(count (:jolt.ffi/offsets huge-matrix-layout))
           (count (:jolt.ffi/array-counts huge-matrix-layout))
           (count (:jolt.ffi/array-strides huge-matrix-layout))
           (ffi/layout-size huge-matrix-layout)
           (ffi/field-offset huge-matrix-layout [:matrix 999 999])]))

(let [p (ffi/alloc (ffi/layout-size nested-layout))]
  (try
    (ffi/write-field p nested-layout [:date :year] -2147483648)
    (ffi/write-field p nested-layout [:date :month] 255)
    (ffi/write-field p nested-layout :tail 65535)
    (check "signed field roundtrip"
           (= -2147483648 (ffi/read-field p nested-layout [:date :year])))
    (check "byte field roundtrip"
           (= 255 (ffi/read-field p nested-layout [:date :month])))
    (check "unsigned field roundtrip"
           (= 65535 (ffi/read-field p nested-layout :tail)))
    (finally (ffi/free p))))

(ffi/with-layout [p arrays-layout]
  (ffi/write-field p arrays-layout [:params 3] 3.5)
  (ffi/write-field p arrays-layout [:name 4] \A)
  (ffi/write-field p arrays-layout [:dates 1 :year] -123456789)
  (ffi/write-field p arrays-layout [:matrix 1 2] 65535)
  (check "public array element roundtrip"
         (= [3.5 \A -123456789 65535]
            [(ffi/read-field p arrays-layout [:params 3])
             (ffi/read-field p arrays-layout [:name 4])
             (ffi/read-field p arrays-layout [:dates 1 :year])
             (ffi/read-field p arrays-layout [:matrix 1 2])])))

(check "unknown path rejects"
       (rejects? #(ffi/field-offset date-layout :missing)))
(check "struct read rejects"
       (rejects? #(ffi/read-field ffi/null nested-layout :date)))
(check "invalid layout rejects"
       (rejects? #(ffi/layout-size {})))
(check "array container read rejects"
       (rejects? #(ffi/read-field ffi/null arrays-layout :params)))
(check "negative array index rejects"
       (rejects? #(ffi/field-offset arrays-layout [:params -1])))
(check "array upper-bound index rejects"
       (rejects? #(ffi/field-offset arrays-layout [:params 4])))
(check "keyword in array position rejects"
       (rejects? #(ffi/field-offset arrays-layout [:params :missing])))
(check "index after scalar rejects"
       (rejects? #(ffi/field-offset arrays-layout [:tag 0])))
(check "path starting with array index rejects"
       (rejects? #(ffi/field-offset arrays-layout [0])))

(if (empty? @failures)
  (do (println "JOLT-FFI-LAYOUT-TEST OK") (flush) (System/exit 0))
  (do (doseq [failure @failures] (println "FAIL:" failure))
      (flush)
      (System/exit 1)))
