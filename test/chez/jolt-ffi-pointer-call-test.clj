;; jolt.ffi/pointer-fn: calling a C function through a pointer rather than by
;; its symbol name. Every target in ffi-pointer-call-helper.c is static, so the
;; only way to reach it is the pointer the helper hands out, or a pointer
;; ffi/foreign-callable made from a jolt fn (the round trip back into jolt).
;;
;; The rows probe the dimensions the binding has: where the pointer came from
;; (a C getter, find-symbol, foreign-callable, read out of a struct), the
;; scalar widths and their boundary conversions, by-value aggregate arguments
;; and results, the options (:blocking, :capture-native-error, a declared
;; variadic tail), and the refusals (a null pointer, a bare :&, :blocking with
;; a :string argument).
(ns jolt-ffi-pointer-call-test
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

(ffi/load-library (or (System/getenv "JOLT_FFI_POINTER_CALL_HELPER")
                      (throw (ex-info "JOLT_FFI_POINTER_CALL_HELPER is required" {}))))

(ffi/defcfn get-add "jolt_ptr_get_add" [] :pointer)
(ffi/defcfn get-is-even "jolt_ptr_get_is_even" [] :pointer)
(ffi/defcfn get-scale "jolt_ptr_get_scale" [] :pointer)
(ffi/defcfn get-halve "jolt_ptr_get_halve" [] :pointer)
(ffi/defcfn get-strlen "jolt_ptr_get_strlen" [] :pointer)
(ffi/defcfn get-fail-with "jolt_ptr_get_fail_with" [] :pointer)
(ffi/defcfn get-neg8 "jolt_ptr_get_neg8" [] :pointer)
(ffi/defcfn get-twice16 "jolt_ptr_get_twice16" [] :pointer)
(ffi/defcfn get-flip "jolt_ptr_get_flip" [] :pointer)
(ffi/defcfn get-sum-ints "jolt_ptr_get_sum_ints" [] :pointer)
(ffi/defcfn get-date-score "jolt_ptr_get_date_score" [] :pointer)
(ffi/defcfn get-make-date "jolt_ptr_get_make_date" [] :pointer)
(ffi/defcfn get-large-score "jolt_ptr_get_large_score" [] :pointer)
(ffi/defcfn get-add-large "jolt_ptr_get_add_large" [] :pointer)
(ffi/defcfn get-writer-emit "jolt_ptr_get_writer_emit" [] :pointer)
(ffi/defcfn writer-open "jolt_ptr_writer_open" []
  [:by-value [:struct [[:write :pointer] [:userdata :pointer]]]])
(ffi/defcfn writer-contents "jolt_ptr_writer_contents" [] :string)
(ffi/defcfn reply-offset "jolt_ptr_reply_offset" [] :size_t)
(ffi/defcfn clipboard "jolt_ptr_clipboard" [:pointer :pointer :size_t] :int32)

;; -- scalars through a pointer a C function returned --------------------------

(def call-add (ffi/pointer-fn [:int64 :int64] :int64))
(def call-add-blocking (ffi/pointer-fn [:int64 :int64] :int64 :blocking))
(def call-add-map (ffi/pointer-fn [:int64 :int64] :int64 {:blocking false}))
(check "scalar call through a returned pointer" (= 33 (call-add (get-add) 10 20)))
(check ":blocking scalar call" (= 33 (call-add-blocking (get-add) 10 20)))
(check "options-map binding" (= 33 (call-add-map (get-add) 10 20)))
(check ":int8 width" (= -5 ((ffi/pointer-fn [:int8] :int8) (get-neg8) 5)))

(def call-is-even (ffi/pointer-fn [:int32] :bool))
(def call-is-even-blocking (ffi/pointer-fn [:int32] :bool :blocking))
(check ":bool return answers true" (true? (call-is-even (get-is-even) 4)))
(check ":bool return answers false" (false? (call-is-even (get-is-even) 3)))
(check ":blocking :bool return" (true? (call-is-even-blocking (get-is-even) 8)))

(def call-flip (ffi/pointer-fn [:bool] :bool))
(check ":bool argument, jolt truthiness" (and (false? (call-flip (get-flip) :yes))
                                              (true? (call-flip (get-flip) nil))))

(def call-scale (ffi/pointer-fn [:double :float] :double))
(def call-halve (ffi/pointer-fn [:float] :float))
(check ":double/:float arguments" (= 7.5 (call-scale (get-scale) 3.0 2.5)))
(check ":float result is boxed as a Float"
       (let [r (call-halve (get-halve) (float 3.0))]
         (and (instance? Float r) (= 1.5 (double r)))))

(def call-twice16 (ffi/pointer-fn [:uint16] :uint16))
(check ":uint16 width" (= 65534 (call-twice16 (get-twice16) 32767)))

(def call-strlen (ffi/pointer-fn [:string] :size_t))
(check ":string argument" (= 5 (call-strlen (get-strlen) "hello")))
(check ":string nil reaches C as NULL" (= 999 (call-strlen (get-strlen) nil)))

(def call-fail (ffi/pointer-fn [:int] :int {:capture-native-error true}))
(def call-fail-blocking (ffi/pointer-fn [:int] :int {:capture-native-error true :blocking true}))
(check ":capture-native-error pairs result and errno" (= [-1 34] (call-fail (get-fail-with) 34)))
(check ":capture-native-error + :blocking" (= [-1 22] (call-fail-blocking (get-fail-with) 22)))

(def call-sum (ffi/pointer-fn [:int :& :int :int :int] :int64))
(check "declared variadic tail through a pointer" (= 60 (call-sum (get-sum-ints) 3 10 20 30)))

;; -- a pointer find-symbol answered -------------------------------------------

(check "pointer from find-symbol"
       (= 3 (call-strlen (ffi/find-symbol "strlen") "abc")))

;; -- the round trip: jolt -> C pointer -> jolt --------------------------------

(def mul (ffi/foreign-callable (fn [a b] (* a b)) [:int64 :int64] :int64))
(check "a foreign-callable's pointer calls back into jolt" (= 42 (call-add mul 6 7)))
(check "one binding, the pointer varies per call: not cached by the first"
       (= [33 42 33] [(call-add (get-add) 10 20) (call-add mul 6 7) (call-add (get-add) 10 20)]))
(def odd-cb (ffi/foreign-callable (fn [x] (odd? x)) [:int32] :bool))
(check ":bool through a jolt callable" (and (true? (call-is-even odd-cb 3))
                                            (false? (call-is-even odd-cb 4))))
(def str-cb (ffi/foreign-callable (fn [s] (if s (count s) 999)) [:string] :size_t))
(check ":string through a jolt callable" (and (= 4 (call-strlen str-cb "four"))
                                              (= 999 (call-strlen str-cb nil))))
;; A :blocking call deactivates the thread, so the callable it reaches must
;; reactivate it on entry: :collect-safe is the matching half.
(def mul-safe (ffi/foreign-callable (fn [a b] (* a b)) [:int64 :int64] :int64 :collect-safe))
(check ":blocking call into a :collect-safe callable" (= 56 (call-add-blocking mul-safe 7 8)))
(ffi/free-callable mul)
(ffi/free-callable odd-cb)
(ffi/free-callable str-cb)
(ffi/free-callable mul-safe)

;; -- by-value aggregates -----------------------------------------------------

(def date-layout (ffi/layout [:struct [[:year :int32] [:month :uint8] [:day :uint8]]]))
(def large-layout (ffi/layout [:struct [[:a :uint64] [:b :uint64] [:c :uint64]]]))

(def call-date-score
  (ffi/pointer-fn [[:by-value [:struct [[:year :int32] [:month :uint8] [:day :uint8]]]]] :int64))
(def call-date-score-blocking
  (ffi/pointer-fn [[:by-value [:struct [[:year :int32] [:month :uint8] [:day :uint8]]]]] :int64
                  :blocking))
(def call-make-date
  (ffi/pointer-fn [:int32 :uint8 :uint8]
                  [:by-value [:struct [[:year :int32] [:month :uint8] [:day :uint8]]]]))
(def call-large-score
  (ffi/pointer-fn [[:by-value [:struct [[:a :uint64] [:b :uint64] [:c :uint64]]]]] :uint64))
(def call-add-large
  (ffi/pointer-fn [[:by-value [:struct [[:a :uint64] [:b :uint64] [:c :uint64]]]]
                   [:by-value [:struct [[:a :uint64] [:b :uint64] [:c :uint64]]]]]
                  [:by-value [:struct [[:a :uint64] [:b :uint64] [:c :uint64]]]]
                  :blocking))

(defn put-date [p y m d]
  (ffi/write-field p date-layout [:year] y)
  (ffi/write-field p date-layout [:month] m)
  (ffi/write-field p date-layout [:day] d)
  p)
(defn put-large [p a b c]
  (ffi/write-field p large-layout [:a] a)
  (ffi/write-field p large-layout [:b] b)
  (ffi/write-field p large-layout [:c] c)
  p)

(ffi/with-arena [arena]
  (let [d (put-date (ffi/alloc arena (ffi/layout-size date-layout)) 2026 11 15)
        out (ffi/alloc arena (ffi/layout-size date-layout))
        x (put-large (ffi/alloc arena (ffi/layout-size large-layout)) 1 2 3)
        y (put-large (ffi/alloc arena (ffi/layout-size large-layout)) 4 5 6)
        sum (ffi/alloc arena (ffi/layout-size large-layout))]
    (check "by-value struct argument" (= 20261115 (call-date-score (get-date-score) d)))
    (check ":blocking by-value struct argument"
           (= 20261115 (call-date-score-blocking (get-date-score) d)))
    (check "by-value struct result: destination second, returned"
           (and (= out (call-make-date (get-make-date) out 1999 12 31))
                (= [1999 12 31] [(ffi/read-field out date-layout [:year])
                                 (ffi/read-field out date-layout [:month])
                                 (ffi/read-field out date-layout [:day])])))
    (check "large (memory-class) by-value argument"
           (= 123 (call-large-score (get-large-score) x)))
    (check "large by-value arguments and result, :blocking"
           (and (= sum (call-add-large (get-add-large) sum x y))
                (= [5 7 9] [(ffi/read-field sum large-layout [:a])
                            (ffi/read-field sum large-layout [:b])
                            (ffi/read-field sum large-layout [:c])])))
    (check "null destination for a by-value result raises"
           (instance? NullPointerException
                      (thrown #(call-make-date (get-make-date) 0 1 2 3))))))

;; -- the libghostty-vt shapes ------------------------------------------------

;; GhosttyWriter {write, userdata}: read the function out of the struct and call
;; it with its userdata, then hand the whole struct by value to a function that
;; writes through it.
(def writer-layout (ffi/layout [:struct [[:write :pointer] [:userdata :pointer]]]))
(def call-write (ffi/pointer-fn [:pointer :pointer :size_t] :bool))
(def call-writer-emit
  (ffi/pointer-fn [[:by-value [:struct [[:write :pointer] [:userdata :pointer]]]] :string] :bool))
(ffi/with-arena [arena]
  (let [w (writer-open (ffi/alloc arena (ffi/layout-size writer-layout)))
        data (ffi/string->ptr "paste:")]
    (try
      (check "writer.write(writer.userdata, data, n)"
             (true? (call-write (ffi/read-field w writer-layout [:write])
                                (ffi/read-field w writer-layout [:userdata])
                                data 6)))
      (check "a by-value struct holding the function pointer"
             (true? (call-writer-emit (get-writer-emit) w "ok")))
      (check "both writes landed" (= "paste:ok" (writer-contents)))
      (finally (ffi/free data)))))

;; GhosttyClipboardRead: C calls jolt with `read`; jolt calls read->reply(read, &r).
(def reply-layout (ffi/layout [:struct [[:text :pointer] [:code :int32]]]))
(def call-reply (ffi/pointer-fn [:pointer :pointer] :void))
(def on-read
  (ffi/foreign-callable
   (fn [read]
     (ffi/with-arena [arena]
       (let [reply (ffi/alloc arena (ffi/layout-size reply-layout))
             text (ffi/string->ptr "clipboard text")]
         (try
           (ffi/write-field reply reply-layout [:text] text)
           (ffi/write-field reply reply-layout [:code] 7)
           (call-reply (ffi/read read :pointer (reply-offset)) read reply)
           (finally (ffi/free text))))))
   [:pointer] :void))
(ffi/with-arena [arena]
  (let [out (ffi/alloc arena 64)]
    (check "read->reply(read, &reply) from inside a callback"
           (and (= 7 (clipboard on-read out 64))
                (= "clipboard text" (ffi/ptr->string out))))))
(ffi/free-callable on-read)

;; -- refusals ------------------------------------------------------------------

(check "null pointer raises NullPointerException, not a segfault"
       (let [t (thrown #(call-add 0 1 2))]
         (and (instance? NullPointerException t)
              (str/includes? (ex-message t) "null C function pointer"))))
(check "nil pointer raises NullPointerException"
       (instance? NullPointerException (thrown #(call-add nil 1 2))))
(check ":blocking null pointer raises"
       (instance? NullPointerException (thrown #(call-add-blocking 0 1 2))))
(check "capture null pointer raises"
       (instance? NullPointerException (thrown #(call-fail 0 1))))
(check "by-value null function pointer raises"
       (instance? NullPointerException (thrown #(call-date-score 0 1))))
(check "a non-numeric pointer raises"
       (some? (thrown #(call-add "not a pointer" 1 2))))
(check "wrong arity raises" (some? (thrown #(call-add (get-add) 1))))

(defn rejection [form] (some-> (thrown #(eval form)) message-chain))
(check "bare :& is refused in jolt's words"
       (str/includes? (str (rejection '(jolt.ffi/pointer-fn [:string :int :&] :int)))
                      "bare :& is not supported through a function pointer"))
(check ":blocking with a :string argument is refused"
       (str/includes? (str (rejection '(jolt.ffi/pointer-fn [:string] :int :blocking)))
                      "argument 0 of a function-pointer binding"))
(check "unknown argtype is refused"
       (some? (rejection '(jolt.ffi/pointer-fn [:bogus] :int))))
(check "a union by value is refused"
       (some? (rejection '(jolt.ffi/pointer-fn [[:by-value [:union [[:a :int]]]]] :int))))
(check ":capture-native-error on :void is refused"
       (some? (rejection '(jolt.ffi/pointer-fn [] :void {:capture-native-error true}))))
(check "unknown option is refused"
       (some? (rejection '(jolt.ffi/pointer-fn [] :int {:fast true}))))
(check "a non-vector signature is refused"
       (str/includes? (str (rejection '(jolt.ffi/__cfn-pointer :int :int)))
                      "jolt.ffi/pointer-fn expects"))
(check "a bad trailing option is refused by the macro"
       (some? (rejection '(jolt.ffi/pointer-fn [] :int :blocking :extra))))

(if (empty? @failures)
  (do (println "JOLT-FFI-POINTER-CALL-TEST OK") (flush) (System/exit 0))
  (do (doseq [failure @failures] (println "FAIL:" failure))
      (flush)
      (System/exit 1)))
