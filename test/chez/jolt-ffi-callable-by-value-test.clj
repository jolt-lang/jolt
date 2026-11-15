;; jolt.ffi callbacks that take and return C structs BY VALUE. Every driver in
;; ffi-callable-by-value-helper.c builds known structs and calls the callback
;; with them by value; the jolt fn receives a pointer to the bytes C passed
;; (valid until it returns) and, for an aggregate result, the destination to
;; write first. C reads the result back, so each row checks both directions.
;;
;; Dimensions: the libghostty paste shape (a GhosttyString and a GhosttyWriter),
;; struct size against the arm64 register cut (8, 16, 24 bytes, two 24-byte
;; structs), float and double HFAs and a float+int struct that is not one,
;; scalars interleaved with structs and with converted positions (:int8,
;; :float, :string, :double, :bool), aggregate results (small, large, HFA),
;; :collect-safe for arguments and results under a :blocking outbound call,
;; foreign-callable, callback (arena) and export!, and the refusals (a union,
;; alone or nested, as an argument or a result).
(ns jolt-ffi-callable-by-value-test
  (:require [clojure.string :as str]))

(require '[jolt.ffi :as ffi])

(def failures (atom []))
(defmacro check [label expr]
  `(when-not (try ~expr (catch Throwable t# (println "THREW in" ~label "-" (ex-message t#)) false))
     (swap! failures conj ~label)))

(defn thrown [f]
  (try (f) nil (catch Throwable t t)))

(defn message-chain [t]
  (str/join " | " (keep ex-message (take-while some? (iterate ex-cause t)))))

(ffi/load-library (or (System/getenv "JOLT_FFI_CALLABLE_BY_VALUE_HELPER")
                      (throw (ex-info "JOLT_FFI_CALLABLE_BY_VALUE_HELPER is required" {}))))

(ffi/defcfn writer-fn "jolt_cbv_writer_fn" [] :pointer)
(ffi/defcfn writer-userdata "jolt_cbv_writer_userdata" [] :pointer)
(ffi/defcfn mime-ptr "jolt_cbv_mime_ptr" [] :pointer)
(ffi/defcfn mime-len "jolt_cbv_mime_len" [] :size_t)
(ffi/defcfn paste "jolt_cbv_paste" [:pointer :pointer] :int32)
(ffi/defcfn drive-date "jolt_cbv_date" [:pointer] :int64)
(ffi/defcfn drive-pair "jolt_cbv_pair" [:pointer] :int64)
(ffi/defcfn drive-large "jolt_cbv_large" [:pointer] :uint64)
(ffi/defcfn drive-two-large "jolt_cbv_two_large" [:pointer] :uint64)
(ffi/defcfn drive-vec2f "jolt_cbv_vec2f" [:pointer] :double)
(ffi/defcfn drive-vec3d "jolt_cbv_vec3d" [:pointer] :double)
(ffi/defcfn drive-mixed "jolt_cbv_mixed" [:pointer] :double)
(ffi/defcfn drive-interleaved "jolt_cbv_interleaved" [:pointer :string] :bool)
(ffi/defcfn drive-ret-date "jolt_cbv_ret_date" [:pointer :int32] :int64)
(ffi/defcfn drive-ret-large "jolt_cbv_ret_large" [:pointer :uint64] :uint64)
(ffi/defcfn drive-ret-vec3d "jolt_cbv_ret_vec3d" [:pointer] :double)
(ffi/defcfn blocking-date "jolt_cbv_blocking_date" [:pointer] :int64 :blocking)
(ffi/defcfn blocking-ret-date "jolt_cbv_blocking_ret_date" [:pointer] :int64 :blocking)

(def string-layout (ffi/layout [:struct [[:ptr :pointer] [:len :size_t]]]))
(def writer-layout (ffi/layout [:struct [[:write :pointer] [:userdata :pointer]]]))
(def date-layout (ffi/layout [:struct [[:year :int32] [:month :uint8] [:day :uint8]]]))
(def pair-layout (ffi/layout [:struct [[:a :int64] [:b :int64]]]))
(def large-layout (ffi/layout [:struct [[:a :uint64] [:b :uint64] [:c :uint64]]]))
(def vec2f-layout (ffi/layout [:struct [[:x :float] [:y :float]]]))
(def vec3d-layout (ffi/layout [:struct [[:x :double] [:y :double] [:z :double]]]))
(def mixed-layout (ffi/layout [:struct [[:f :float] [:i :int32]]]))

(defn date-score [p]
  (+ (* 10000 (ffi/read-field p date-layout :year))
     (* 100 (ffi/read-field p date-layout :month))
     (ffi/read-field p date-layout :day)))
(defn large-score [p]
  (+ (* 100 (ffi/read-field p large-layout :a))
     (* 10 (ffi/read-field p large-layout :b))
     (ffi/read-field p large-layout :c)))
(def date-expected (+ (* 10000 -123456789) (* 100 250) 251))

;; -- the libghostty-vt paste reader ---------------------------------------------
;; bool read_clipboard(void *userdata, GhosttyString mime, GhosttyWriter writer)

(def seen (atom nil))
(defn read-clipboard [userdata mime writer]
  (reset! seen {:userdata userdata
                :mime-ptr (ffi/read-field mime string-layout :ptr)
                :mime (ffi/read-bytes (ffi/read-field mime string-layout :ptr)
                                      (ffi/read-field mime string-layout :len))
                :write (ffi/read-field writer writer-layout :write)
                :writer-userdata (ffi/read-field writer writer-layout :userdata)})
  true)

(let [cb (ffi/foreign-callable read-clipboard
                               [:pointer
                                [:by-value [:struct [[:ptr :pointer] [:len :size_t]]]]
                                [:by-value [:struct [[:write :pointer] [:userdata :pointer]]]]]
                               :bool)]
  (try
    (check "paste shape: C sees the callback's true" (= 1 (paste cb 4242)))
    (check "paste shape: scalar userdata before the structs" (= 4242 (:userdata @seen)))
    (check "paste shape: GhosttyString.ptr" (= (mime-ptr) (:mime-ptr @seen)))
    (check "paste shape: GhosttyString bytes through ptr/len"
           (= "text/plain;charset=utf-8" (:mime @seen)))
    (check "paste shape: GhosttyString.len" (= (mime-len) (count (:mime @seen))))
    (check "paste shape: GhosttyWriter.write" (= (writer-fn) (:write @seen)))
    (check "paste shape: GhosttyWriter.userdata" (= (writer-userdata) (:writer-userdata @seen)))
    (finally (ffi/free-callable cb))))

;; -- struct size and class -------------------------------------------------------

(defmacro with-callable [[sym form] & body]
  `(let [~sym ~form] (try ~@body (finally (ffi/free-callable ~sym)))))

(with-callable [cb (ffi/foreign-callable date-score
                                         [[:by-value [:struct [[:year :int32] [:month :uint8] [:day :uint8]]]]]
                                         :int64)]
  (check "8-byte struct argument" (= date-expected (drive-date cb))))

(with-callable [cb (ffi/foreign-callable
                    (fn [p] (- (ffi/read-field p pair-layout :b) (ffi/read-field p pair-layout :a)))
                    [[:by-value [:struct [[:a :int64] [:b :int64]]]]]
                    :int64)]
  ;; (INT64_MAX - 9) - (INT64_MIN + 7) wraps; compare the two fields instead.
  (let [got (atom nil)]
    (with-callable [cb2 (ffi/foreign-callable
                         (fn [p] (reset! got [(ffi/read-field p pair-layout :a)
                                              (ffi/read-field p pair-layout :b)])
                           0)
                         [[:by-value [:struct [[:a :int64] [:b :int64]]]]]
                         :int64)]
      (drive-pair cb2)
      (check "16-byte struct argument (register pair)"
             (= [(+ Long/MIN_VALUE 7) (- Long/MAX_VALUE 9)] @got)))))

(with-callable [cb (ffi/foreign-callable large-score
                                         [[:by-value [:struct [[:a :uint64] [:b :uint64] [:c :uint64]]]]]
                                         :uint64)]
  (check "24-byte struct argument (memory-passed on arm64)" (= 123 (drive-large cb))))

(with-callable [cb (ffi/foreign-callable
                    (fn [l m] (+ (* 1000 (large-score l)) (large-score m)))
                    [[:by-value [:struct [[:a :uint64] [:b :uint64] [:c :uint64]]]]
                     [:by-value [:struct [[:a :uint64] [:b :uint64] [:c :uint64]]]]]
                    :uint64)]
  (check "two 24-byte struct arguments" (= (+ 123000 4560) (drive-two-large cb))))

(with-callable [cb (ffi/foreign-callable
                    (fn [p] (+ (* 10 (ffi/read-field p vec2f-layout :x)) (ffi/read-field p vec2f-layout :y)))
                    [[:by-value [:struct [[:x :float] [:y :float]]]]]
                    :double)]
  (check "float HFA argument" (= 12.75 (drive-vec2f cb))))

(with-callable [cb (ffi/foreign-callable
                    (fn [p] (+ (* 100 (ffi/read-field p vec3d-layout :x))
                               (* 10 (ffi/read-field p vec3d-layout :y))
                               (ffi/read-field p vec3d-layout :z)))
                    [[:by-value [:struct [[:x :double] [:y :double] [:z :double]]]]]
                    :double)]
  (check "24-byte double HFA argument" (= 10289.5 (drive-vec3d cb))))

(with-callable [cb (ffi/foreign-callable
                    (fn [p] (+ (* 100 (ffi/read-field p mixed-layout :f)) (ffi/read-field p mixed-layout :i)))
                    [[:by-value [:struct [[:f :float] [:i :int32]]]]]
                    :double)]
  (check "float+int struct argument (not an HFA)" (= 33.0 (drive-mixed cb))))

;; -- scalars and structs interleaved -----------------------------------------------

(let [got (atom nil)]
  (with-callable [cb (ffi/foreign-callable
                      (fn [tag d scale s l k]
                        (reset! got [tag (date-score d) scale s (large-score l) k])
                        true)
                      [:int8
                       [:by-value [:struct [[:year :int32] [:month :uint8] [:day :uint8]]]]
                       :float :string
                       [:by-value [:struct [[:a :uint64] [:b :uint64] [:c :uint64]]]]
                       :double]
                      :bool)]
    (check "interleaved: C sees :bool true" (true? (drive-interleaved cb "hello")))
    (check "interleaved: every argument in place and converted"
           (= [-3 20260723 0.5 "hello" 1230 6.25] @got))
    (check "interleaved: :float arrives boxed as a Float"
           (instance? Float (nth @got 2)))
    (drive-interleaved cb nil)
    (check "interleaved: a null :string arrives as nil" (nil? (nth @got 3)))))

;; -- aggregate results ------------------------------------------------------------

(def ret-seen (atom nil))
(with-callable [cb (ffi/foreign-callable
                    (fn [dest year]
                      (reset! ret-seen dest)
                      (ffi/write-field dest date-layout :year year)
                      (ffi/write-field dest date-layout :month 12)
                      (ffi/write-field dest date-layout :day 31)
                      :ignored)
                    [:int32]
                    [:by-value [:struct [[:year :int32] [:month :uint8] [:day :uint8]]]])]
  (check "8-byte struct result, written through the destination"
         (= 20261231 (drive-ret-date cb 2026)))
  (check "struct result: the destination is a non-null pointer"
         (and (integer? @ret-seen) (pos? @ret-seen))))

(with-callable [cb (ffi/foreign-callable
                    (fn [dest k in]
                      (ffi/write-field dest large-layout :a (+ k (ffi/read-field in large-layout :a)))
                      (ffi/write-field dest large-layout :b (+ k (ffi/read-field in large-layout :b)))
                      (ffi/write-field dest large-layout :c (+ k (ffi/read-field in large-layout :c))))
                    [:uint64 [:by-value [:struct [[:a :uint64] [:b :uint64] [:c :uint64]]]]]
                    [:by-value [:struct [[:a :uint64] [:b :uint64] [:c :uint64]]]])]
  (check "24-byte struct result with a struct argument (indirect result on arm64)"
         (= (+ (* 101 1000000) (* 201 1000) 301) (drive-ret-large cb 1))))

(with-callable [cb (ffi/foreign-callable
                    (fn [dest k]
                      (ffi/write-field dest vec3d-layout :x k)
                      (ffi/write-field dest vec3d-layout :y (* 2 k))
                      (ffi/write-field dest vec3d-layout :z 0.5))
                    [:double]
                    [:by-value [:struct [[:x :double] [:y :double] [:z :double]]]])]
  (check "double HFA result" (= 240.5 (drive-ret-vec3d cb))))

;; -- :collect-safe, entered from a :blocking call on this thread ---------------------

(with-callable [cb (ffi/foreign-callable date-score
                                         [[:by-value [:struct [[:year :int32] [:month :uint8] [:day :uint8]]]]]
                                         :int64 :collect-safe)]
  (check ":collect-safe struct argument" (= date-expected (blocking-date cb))))

(with-callable [cb (ffi/foreign-callable
                    (fn [dest year]
                      (ffi/write-field dest date-layout :year year)
                      (ffi/write-field dest date-layout :month 1)
                      (ffi/write-field dest date-layout :day 2))
                    [:int32]
                    [:by-value [:struct [[:year :int32] [:month :uint8] [:day :uint8]]]]
                    :collect-safe)]
  (check ":collect-safe struct result" (= 19990102 (blocking-ret-date cb))))

;; -- callback (arena-owned) and export! ------------------------------------------

(let [arena (ffi/confined-arena)]
  (try
    (let [cb (ffi/callback arena date-score
                           [[:by-value [:struct [[:year :int32] [:month :uint8] [:day :uint8]]]]]
                           :int64)]
      (check "arena callback with a struct argument" (= date-expected (drive-date cb))))
    (let [cb (ffi/callback arena
                           (fn [dest year]
                             (ffi/write-field dest date-layout :year year)
                             (ffi/write-field dest date-layout :month 3)
                             (ffi/write-field dest date-layout :day 4))
                           [:int32]
                           [:by-value [:struct [[:year :int32] [:month :uint8] [:day :uint8]]]]
                           :collect-safe)]
      (check "arena :collect-safe callback with a struct result"
             (= 19990304 (blocking-ret-date cb))))
    (finally (ffi/close-arena arena))))

(let [addr (ffi/export! "cbv-date-score" date-score
                        [[:by-value [:struct [[:year :int32] [:month :uint8] [:day :uint8]]]]]
                        :int64)]
  (check "export! with a struct argument" (= date-expected (drive-date addr))))
(let [addr (ffi/export! "cbv-make-date"
                        (fn [dest year]
                          (ffi/write-field dest date-layout :year year)
                          (ffi/write-field dest date-layout :month 5)
                          (ffi/write-field dest date-layout :day 6))
                        [:int32]
                        [:by-value [:struct [[:year :int32] [:month :uint8] [:day :uint8]]]])]
  (check "export! with a struct result" (= 20260506 (drive-ret-date addr 2026))))

;; -- refusals ---------------------------------------------------------------------

(defn refusal [form]
  (some-> (thrown #(eval form)) message-chain))

(check "union argument refused"
       (str/includes? (str (refusal '(ffi/foreign-callable identity
                                      [[:by-value [:union [[:i :int32] [:f :float]]]]] :int)))
                      "callback argument type: a union is not passed by value"))
(check "union result refused"
       (str/includes? (str (refusal '(ffi/foreign-callable (fn [d] nil) []
                                      [:by-value [:union [[:i :int32] [:f :float]]]])))
                      "callback return type: a union is not passed by value"))
(check "union nested in a struct argument refused"
       (str/includes? (str (refusal '(ffi/foreign-callable identity
                                      [[:by-value [:struct [[:tag :int32]
                                                            [:v [:union [[:i :int32] [:f :float]]]]]]]]
                                      :int)))
                      "a union is not passed by value"))
(check "union refusal does not suggest :pointer for a callback"
       (not (str/includes? (str (refusal '(ffi/foreign-callable identity
                                           [[:by-value [:union [[:i :int32]]]]] :int)))
                           "declare :pointer")))
(check "union refused in export! too"
       (some? (refusal '(ffi/export! "cbv-union" identity
                                     [[:by-value [:union [[:i :int32]]]]] :int))))
(check "a malformed by-value form is refused"
       (str/includes? (str (refusal '(ffi/foreign-callable identity [[:by-value :int32]] :int)))
                      "jolt.ffi"))

(if (empty? @failures)
  (do (println "JOLT-FFI-CALLABLE-BY-VALUE-TEST OK") (flush) (System/exit 0))
  (do (doseq [failure @failures] (println "FAIL:" failure))
      (flush)
      (System/exit 1)))
