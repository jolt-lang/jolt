(ns jolt-ffi-aggregate-test)

(require '[jolt.ffi :as ffi])

(def helper (System/getenv "JOLT_FFI_AGGREGATE_HELPER"))
(when-not helper
  (throw (ex-info "JOLT_FFI_AGGREGATE_HELPER is required" {})))

(def date-layout
  (ffi/layout [:struct [[:year :int32] [:month :uint8] [:day :uint8]]]))
(def packet-layout
  (ffi/layout [:struct [[:tag :uint8]
                        [:params [:array :uint32 4]]
                        [:dates [:array [:struct [[:year :int32]
                                                    [:month :uint8]
                                                    [:day :uint8]]] 2]]]]))

;; The public macros require literal signature data, so keep the descriptor
;; inline rather than referring to date-layout in these declarations. Define
;; the bindings before loading the helper to exercise lazy scoped resolution.
(def date-score
  (ffi/foreign-fn "jolt_agg_date_score"
                  [[:by-value [:struct [[:year :int32] [:month :uint8] [:day :uint8]]]]]
                  :int64))
(ffi/defcfn date-score-defcfn "jolt_agg_date_score"
  [[:by-value [:struct [[:year :int32] [:month :uint8] [:day :uint8]]]]]
  :int64)
(def make-date
  (ffi/foreign-fn "jolt_agg_make_date" [:int32 :uint8 :uint8]
                  [:by-value [:struct [[:year :int32] [:month :uint8] [:day :uint8]]]]))
(def date-plus-varargs
  (ffi/foreign-fn "jolt_agg_date_plus_varargs"
                  [[:by-value [:struct [[:year :int32] [:month :uint8] [:day :uint8]]]]
                   :int :varargs :int :int]
                  :int64))
(def packet-score
  (ffi/foreign-fn "jolt_agg_packet_score"
                  [[:by-value [:struct [[:tag :uint8]
                                        [:params [:array :uint32 4]]
                                        [:dates [:array [:struct [[:year :int32]
                                                                    [:month :uint8]
                                                                    [:day :uint8]]] 2]]]]]]
                  :int64))
(def make-packet
  (ffi/foreign-fn "jolt_agg_make_packet"
                  [:uint8 :uint32
                   [:by-value [:struct [[:year :int32] [:month :uint8] [:day :uint8]]]]]
                  [:by-value [:struct [[:tag :uint8]
                                       [:params [:array :uint32 4]]
                                       [:dates [:array [:struct [[:year :int32]
                                                                   [:month :uint8]
                                                                   [:day :uint8]]] 2]]]]]))

(ffi/load-library helper)

(def failures (atom []))
(defmacro check [label expr]
  `(when-not ~expr (swap! failures conj ~label)))
(defn rejects? [f]
  (try (f) false (catch Throwable _ true)))

(let [input (ffi/alloc (ffi/layout-size date-layout))
      output (ffi/alloc (ffi/layout-size date-layout))]
  (try
    (ffi/write-field input date-layout :year -123456789)
    (ffi/write-field input date-layout :month 250)
    (ffi/write-field input date-layout :day 251)
    (check "public by-value argument"
           (= -1234567864749 (date-score input)))
    (check "public defcfn by-value argument"
           (= -1234567864749 (date-score-defcfn input)))
    (check "public scoped aggregate before varargs"
           (= -1234567864738 (date-plus-varargs input 2 5 6)))
    (check "public by-value return"
           (= [output 2026 7 23]
              [(make-date output 2026 7 23)
               (ffi/read-field output date-layout :year)
               (ffi/read-field output date-layout :month)
               (ffi/read-field output date-layout :day)]))
    (finally (ffi/free input) (ffi/free output))))

(ffi/with-layout [input packet-layout]
  (ffi/with-layout [date date-layout]
    (ffi/with-layout [output packet-layout]
      (ffi/write-field input packet-layout :tag 7)
      (doseq [i (range 4)]
        (ffi/write-field input packet-layout [:params i] (+ 10 i)))
      (doseq [[index year month day] [[0 2026 7 23] [1 2027 8 24]]]
        (ffi/write-field input packet-layout [:dates index :year] year)
        (ffi/write-field input packet-layout [:dates index :month] month)
        (ffi/write-field input packet-layout [:dates index :day] day))
      (check "public fixed-array aggregate argument"
             (= 40545874 (packet-score input)))
      (ffi/write-field date date-layout :year 2026)
      (ffi/write-field date date-layout :month 7)
      (ffi/write-field date date-layout :day 23)
      (check "public fixed-array aggregate return"
             (= [output 7 10 13 2026 2027 8 24]
                [(make-packet output 7 10 date)
                 (ffi/read-field output packet-layout :tag)
                 (ffi/read-field output packet-layout [:params 0])
                 (ffi/read-field output packet-layout [:params 3])
                 (ffi/read-field output packet-layout [:dates 0 :year])
                 (ffi/read-field output packet-layout [:dates 1 :year])
                 (ffi/read-field output packet-layout [:dates 1 :month])
                 (ffi/read-field output packet-layout [:dates 1 :day])])))))

(check "public null argument rejects"
       (rejects? #(date-score ffi/null)))
(check "public null destination rejects"
       (rejects? #(make-date ffi/null 2026 7 23)))
(check "public aggregate callback rejects"
       (rejects?
        #(eval '(ffi/foreign-callable identity
                  [[:by-value [:struct [[:year :int32]]]]]
                  :int))))
(check "public scalar callback control"
       (let [pointer (eval '(ffi/foreign-callable identity [:int] :int))]
         (try (pos? pointer) (finally (ffi/free-callable pointer)))))
(check "public aggregate export rejects"
       (rejects?
        #(eval '(ffi/export! "aggregate" identity
                  [[:by-value [:struct [[:year :int32]]]]]
                  :int))))
(check "public scalar export control"
       (pos? (eval '(ffi/export! "scalar-control" identity [:int] :int))))

(if (empty? @failures)
  (do (println "JOLT-FFI-AGGREGATE-TEST OK") (flush) (System/exit 0))
  (do (doseq [failure @failures] (println "FAIL:" failure))
      (flush)
      (System/exit 1)))
