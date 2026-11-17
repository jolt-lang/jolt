;; Thread.getStackTrace and java.lang.StackTraceElement. The frames come from the
;; same reconstruction an uncaught error's backtrace uses: the live continuation,
;; plus the TCO-erased callers the callsite tables can name. Class names follow
;; the JVM's ns$fn munging, which is what callers parse (test.check's
;; clojure-test reporter walks the stack for the assertion's file:line).
;;
;; Where the JVM keeps a frame jolt cannot recover — a caller erased by a tail
;; call through a dynamic dispatch, with nothing static pointing at it — the
;; frame is left out rather than guessed. These rows only pin frames jolt does
;; name, in the JVM's order.
;;
;; Prints the STACK-TRACE OK / FAIL sentinel smoke.sh greps.
(ns stack-trace-test
  (:require [clojure.string :as str]
            [clojure.test]))

(def ^:private fails (atom []))
(def ^:private passes (atom 0))

(defn- ok= [got want label]
  (if (= got want)
    (swap! passes inc)
    (swap! fails conj (str label ": want " (pr-str want) " got " (pr-str got)))))

(defn- own-frames
  "This namespace's frames, as [class file] pairs, innermost first."
  [elems]
  (->> elems
       (map (fn [e] [(.getClassName e) (.getFileName e)]))
       (filter (fn [[c _]] (str/starts-with? c "stack_trace_test$")))
       vec))

;; The stack read in a non-tail position, as callers do: its frame stays live.
(defn- here [] (let [st (.getStackTrace (Thread/currentThread))] st))

;; --- non-tail and tail callers ---------------------------------------------------
(defn- inner [] (here))
(defn- middle [] (let [r (inner)] r))
(defn- outer-tail [] (middle))
(defn- probe-chain [] (let [st (outer-tail)] st))

;; inner and outer-tail are erased by their tail calls; the callsite tables name
;; both, between the live frames around them
(let [st (probe-chain)]
  (ok= (mapv first (own-frames st))
       ["stack_trace_test$here" "stack_trace_test$inner" "stack_trace_test$middle"
        "stack_trace_test$outer_tail" "stack_trace_test$probe_chain"]
       "TCO-erased callers are reconstructed, in order")
  (ok= (vec (StackTraceElement->vec (first st)))
       ['java.lang.Thread 'getStackTrace "Thread.java" -1]
       "the first element is Thread.getStackTrace itself, as on the JVM")
  (ok= (set (map second (own-frames st))) #{"stack-trace-test.clj"}
       "a frame's file is the source file's base name")
  (ok= (every? pos? (map (fn [e] (.getLineNumber e))
                         (filter (fn [e] (str/starts-with? (.getClassName e) "stack_trace_test$"))
                                 st)))
       true
       "a mapped frame carries its line")
  (ok= (set (map (fn [e] (.getMethodName e)) (rest st))) #{"invoke"}
       "a Clojure frame's method is invoke"))

;; the stack read in a TAIL position: that fn's frame is gone by the time the
;; host call runs, and the site it stored names it
(defn- here-tail [] (.getStackTrace (Thread/currentThread)))
(defn- via-tail [] (let [st (here-tail)] st))
(ok= (mapv first (own-frames (via-tail)))
     ["stack_trace_test$here_tail" "stack_trace_test$via_tail"]
     "a fn that read the stack in tail position names itself")

;; --- a returned tail call leaves no frame -------------------------------------------
;; returns-via tail-calls leaf-inc and both return before the stack is read; the
;; tail-site pair leaf-inc stored is still in the slot, and the reading fn calls
;; returns-via elsewhere in its body. The JVM shows neither. Both a multi-line and
;; a one-line let, since the reached line can name returns-via either way.
(defn- leaf-inc [x] (inc x))
(defn- returns-via [x] (leaf-inc x))
(defn- read-after [x]
  (let [a (returns-via x)
        st (.getStackTrace (Thread/currentThread))]
    st))
(defn- read-after-1 [x] (let [a (returns-via x) st (.getStackTrace (Thread/currentThread))] st))
(defn- probe-after [] (let [st (read-after 1)] st))
(defn- probe-after-1 [] (let [st (read-after-1 1)] st))
(ok= (mapv first (own-frames (probe-after)))
     ["stack_trace_test$read_after" "stack_trace_test$probe_after"]
     "a tail call that already returned is not reported")
(ok= (mapv first (own-frames (probe-after-1)))
     ["stack_trace_test$read_after_1" "stack_trace_test$probe_after_1"]
     "a returned tail call on the reading line is not reported")
;; the same through a tail call to a fn VALUE, which names no static callee
(defn- returns-dyn [f x] (f x))
(defn- read-after-dyn [x]
  (let [a (returns-dyn inc x)
        st (.getStackTrace (Thread/currentThread))]
    st))
(defn- probe-after-dyn [] (let [st (read-after-dyn 1)] st))
(ok= (mapv first (own-frames (probe-after-dyn)))
     ["stack_trace_test$read_after_dyn" "stack_trace_test$probe_after_dyn"]
     "a returned tail call through a fn value is not reported")

;; --- a deftest body --------------------------------------------------------------
;; deftest's body lives in the var's :test metadata, as on the JVM; its frame still
;; maps to this file (test.check's reporter takes an assertion's file:line from the
;; first frame outside clojure.test)
(def ^:private in-test (atom nil))
(clojure.test/deftest ^:private reads-in-test (let [st (here)] (reset! in-test st)))
(clojure.test/test-var #'reads-in-test)
(ok= (vec (take 2 (own-frames @in-test)))
     [["stack_trace_test$here" "stack-trace-test.clj"]
      ["stack_trace_test$reads_in_test$fn__0" "stack-trace-test.clj"]]
     "a deftest body's frame is named and mapped to its file")

;; --- a caller erased through apply, under a try ------------------------------------
;; The JVM keeps fail and runner; jolt erased both (apply is a tail call, and the
;; try's guard is runner's). The most recent tail site names fail, and runner's
;; try body registers fail as runner's exit, so both come back.
(defn- checking [f]
  (let [conform! (fn [x] (if (= x :bad) (let [st (here)] st) x))]
    (fn [& args] (let [r (conform! (first args))] (if (= :bad (first args)) r (apply f args))))))
(def ^:private checked (checking (fn [& a] a)))
(defn- fail [& args] (apply checked args))
(defn- runner [] (try (fail :bad) (catch Exception e nil)))
(defn- probe-runner [] (let [st (runner)] st))

(ok= (mapv first (own-frames (probe-runner)))
     ["stack_trace_test$here" "stack_trace_test$checking$fn__0"
      "stack_trace_test$checking$fn__1" "stack_trace_test$fail" "stack_trace_test$runner"
      "stack_trace_test$probe_runner"]
     "anonymous fns are named ns$def$fn__n; an apply-erased caller and a try wrapper come back")

;; --- StackTraceElement as a value ---------------------------------------------
(let [e (StackTraceElement. "a.b$c" "invoke" "c.clj" 7)]
  (ok= [(.getClassName e) (.getMethodName e) (.getFileName e) (.getLineNumber e)]
       ["a.b$c" "invoke" "c.clj" 7]
       "the constructor and accessors")
  (ok= (str e) "a.b$c.invoke(c.clj:7)" "toString is class.method(file:line)")
  (ok= (str (StackTraceElement. "a.b$c" "invoke" "c.clj" -1)) "a.b$c.invoke(c.clj)"
       "an unknown line leaves the line out")
  (ok= (str (StackTraceElement. "a.b$c" "invoke" nil -1)) "a.b$c.invoke(Unknown Source)"
       "an unknown file reads Unknown Source")
  (ok= (= e (StackTraceElement. "a.b$c" "invoke" "c.clj" 7)) true "equal elements are =")
  (ok= (= (hash e) (hash (StackTraceElement. "a.b$c" "invoke" "c.clj" 7))) true
       "equal elements hash alike")
  (ok= (instance? StackTraceElement e) true "instance? StackTraceElement")
  (ok= (class e) StackTraceElement "(class e) is StackTraceElement"))

;; another thread's stack is not reachable from here
(let [t (Thread. (fn [] (Thread/sleep 200)))]
  (.start t)
  (ok= (count (.getStackTrace t)) 0 "another thread's stack is empty")
  (.join t))

;; --- a caught Throwable's own frames ------------------------------------------
;; The frames are the ones its throw captured, kept on the throwable itself, so they
;; survive the catch and any number of throws after it. They used to come from a
;; per-thread slot that the next throw replaced and a completed catch cleared, so a
;; caught exception's .getStackTrace was empty (jolt-lang/jolt#1142's second report).
(defn- thrower [] (let [r (throw (ex-info "boom" {:k 1}))] r))
(defn- catch-it [] (let [e (try (thrower) (catch Exception e e))] e))
(let [e (catch-it)
      _ (dotimes [_ 3] (try (throw (ex-info "other" {})) (catch Exception _ nil)))
      st (.getStackTrace e)]
  (ok= (first (own-frames st)) ["stack_trace_test$thrower" "stack-trace-test.clj"]
       "a caught throwable's first own frame is where it was thrown")
  (ok= (boolean (some #(= "stack_trace_test$catch_it" (first %)) (own-frames st))) true
       "...its caller follows")
  (let [m (Throwable->map e)]
    (ok= (pos? (count (:trace m))) true "Throwable->map has a :trace")
    (ok= (every? (fn [[c meth f l]] (and (symbol? c) (symbol? meth) (or (nil? f) (string? f)) (int? l)))
                 (:trace m))
         true
         "each :trace entry is [class-symbol method-symbol file line]")
    (ok= (:at (first (:via m))) (first (:trace m)) "the :via entry's :at is its top frame")))
;; printStackTrace renders the same capture: an exception caught earlier and printed
;; after other throws shows ITS frames, not the last throw's
(defn- other-thrower [] (let [r (throw (ex-info "other" {}))] r))
(let [e (catch-it)
      _ (try (other-thrower) (catch Exception _ nil))
      sw (java.io.StringWriter.)
      _ (.printStackTrace e (java.io.PrintWriter. sw))
      out (str sw)]
  (ok= (boolean (re-find #"thrower" out)) true "printStackTrace names the exception's own thrower")
  (ok= (boolean (re-find #"other-thrower" out)) false "...and not the frames of a later throw"))
;; a throwable that is built and never thrown has the frames of where it was built,
;; as the JVM's constructor fills them in
(defn- builder [] (let [e (ex-info "never thrown" {})] e))
(let [st (.getStackTrace (builder))]
  (ok= (first (own-frames st)) ["stack_trace_test$builder" "stack-trace-test.clj"]
       "a never-thrown throwable's first own frame is where it was constructed"))
(defn- host-builder [] (let [e (RuntimeException. "built")] e))
(ok= (first (own-frames (.getStackTrace (host-builder))))
     ["stack_trace_test$host_builder" "stack-trace-test.clj"]
     "so is a host throwable's")
;; a rethrow keeps the frames of the first throw, as on the JVM
(let [e (catch-it)
      e2 (try (throw e) (catch Exception x x))]
  (ok= (identical? e e2) true "the rethrown object is the same one")
  (ok= (first (own-frames (.getStackTrace e2))) ["stack_trace_test$thrower" "stack-trace-test.clj"]
       "a rethrow keeps the original frames"))
(ok= (StackTraceElement->vec (first (.getStackTrace (catch-it))))
     ['stack_trace_test$thrower 'invoke "stack-trace-test.clj" 158]
     "StackTraceElement->vec names class and method as symbols")

;; --- a tail call that RETURNED is not a frame of a later throw ------------------
;; A tail call stores its site pair and nothing cleared it when the call returned,
;; so the next throw from host code -- with nothing live to check the pair against
;; -- named the returned fn as its innermost frame.
(defn- ret-g [x] x)
(defn- ret-f [x] (ret-g x))
(defn- ret-host [x] (count x))
(defn- frame-names [e] (mapv first (own-frames (.getStackTrace e))))
(let [e (try (do (ret-f 1) (nth [] 3)) (catch Exception x x))]
  (ok= (frame-names e) []
       "a returned tail call (to a jolt fn) is not a frame of the next host throw"))
(let [e (try (do (ret-host [1]) (nth [] 3)) (catch Exception x x))]
  (ok= (frame-names e) []
       "a returned tail call (to a host fn) is not a frame of the next host throw"))
(defn- ret-then-throw [] (ret-f 1) (ret-host [1]) (nth [] 3))
(let [e (try (ret-then-throw) (catch Exception x x))]
  (ok= (first (frame-names e)) "stack_trace_test$ret_then_throw"
       "...nor inside a fn: the fn that was running is the innermost frame")
  (ok= (some #{"stack_trace_test$ret_f" "stack_trace_test$ret_g" "stack_trace_test$ret_host"}
             (frame-names e))
       nil
       "...and the fns it called and that returned are not frames"))
;; ...while a tail call still running when the host throws keeps its erased frames
(defn- tail-host-thrower [] (nth [] 3))
(let [e (try (tail-host-thrower) (catch Exception x x))]
  (ok= (frame-names e) ["stack_trace_test$tail_host_thrower"]
       "a tail call into a throwing host fn keeps its erased frame"))
(let [e (try (do (ret-f 1) (tail-host-thrower)) (catch Exception x x))]
  (ok= (frame-names e) ["stack_trace_test$tail_host_thrower"]
       "...after a returned tail call too"))
(defn- chain-c [v] (nth v 3))
(defn- chain-b [v] (chain-c v))
(defn- chain-a [v] (ret-f 1) (chain-b v))
(let [e (try (chain-a []) (catch Exception x x))]
  (ok= (frame-names e)
       ["stack_trace_test$chain_c" "stack_trace_test$chain_b" "stack_trace_test$chain_a"]
       "a tail-call chain ending in a host throw keeps every erased frame"))
;; a fn erased by its tail call into a fn that is still running sits BELOW that fn:
;; it is the running fn's caller, not a frame above the throw
(defn- live-callee [v] (+ 1 (nth v 3)))
(defn- erases-into-live [v] (live-callee v))
(defn- calls-erases [] (let [r (erases-into-live [])] r))
(let [e (try (calls-erases) (catch Exception x x))]
  (ok= (take 3 (frame-names e))
       ["stack_trace_test$live_callee" "stack_trace_test$erases_into_live"
        "stack_trace_test$calls_erases"]
       "an erased caller of the throwing fn is spliced below it"))
(defn- mid-caller [] (let [r (live-callee [])] r))
(defn- erases-into-mid [] (mid-caller))
(defn- calls-erases-mid [] (let [r (erases-into-mid)] r))
(let [e (try (calls-erases-mid) (catch Exception x x))]
  (ok= (take 4 (frame-names e))
       ["stack_trace_test$live_callee" "stack_trace_test$mid_caller"
        "stack_trace_test$erases_into_mid" "stack_trace_test$calls_erases_mid"]
       "...and below the live frame it called, further out"))
;; a callback that ended in a tail call and returned to untraced code (a core
;; HOF) leaves its pair in the slot; the next callback's throw must not report it
(defn- cb-tail [x] (ret-f x))
(defn- cb-throws [x] (let [v (ret-g x)] (+ v (nth [] 3))))
(let [e (try (let [r ((juxt cb-tail cb-throws) 1)] r) (catch Exception x x))]
  (ok= (first (frame-names e)) "stack_trace_test$cb_throws"
       "a returned callback's tail call is not the next callback's innermost frame"))
;; ...while a tail call that is still running keeps its erased caller when the
;; callee throws from a call of its own: os-caller's frame is erased by its tail
;; call to os-callee, and os-callee is live below os-boom. The reporter used to
;; ask os-boom, the innermost frame, whether it calls os-caller, and dropped it.
;; os-boom throws from a non-tail call: a tail call there would store its own
;; pair, and the slot holds one, so os-caller would be gone either way.
(defn- os-boom [x] (inc (nth [] x)))
(defn- os-callee [x] (let [y (inc x)] (inc (os-boom (dec y)))))
(defn- os-caller [x] (os-callee x))
(defn- os-top [f x] (inc (f x)))
(let [e (try (os-top os-caller 0) (catch Exception x x))]
  (ok= (vec (take 4 (frame-names e)))
       ["stack_trace_test$os_boom" "stack_trace_test$os_callee"
        "stack_trace_test$os_caller" "stack_trace_test$os_top"]
       "a running tail call's erased caller lands below its callee"))
;; ...and when the tail call is to a protocol method: the site names the
;; protocol fn, the live frame is the impl's, from each way an impl is made.
(defprotocol OsP (os-m [this x]) (os-m2 [this] [this x]))
(deftype OsT [] OsP (os-m [this x] (inc (os-boom x))) (os-m2 [this] 0) (os-m2 [this x] 0))
(defrecord OsR [] OsP (os-m [this x] (inc (os-boom x))) (os-m2 [this] 0) (os-m2 [this x] 0))
(defn- os-reify [] (reify OsP (os-m [this x] (inc (os-boom x)))
                     (os-m2 [this] 0) (os-m2 [this x] (inc (os-boom x)))))
(extend-protocol OsP String (os-m [this x] (inc (os-boom x))) (os-m2 ([this] 0) ([this x] 0)))
(defn- os-via-type [x] (os-m (OsT.) x))
(defn- os-via-record [x] (os-m (->OsR) x))
(defn- os-via-reify [x] (os-m (os-reify) x))
(defn- os-via-reify-arity [x] (os-m2 (os-reify) x))
(defn- os-via-extend [x] (os-m "s" x))
(doseq [[f nm] [[os-via-type "os_via_type"] [os-via-record "os_via_record"]
                [os-via-reify "os_via_reify"] [os-via-reify-arity "os_via_reify_arity"]
                [os-via-extend "os_via_extend"]]]
  (let [e (try (os-top f 0) (catch Exception x x))]
    (ok= (boolean (some #{(str "stack_trace_test$" nm)} (frame-names e))) true
         (str "a tail call into a protocol method keeps its erased caller: " nm))))
;; a lookup the back end open-codes runs jolt code too: a deftype's valAt, a
;; sorted map's comparator. The fn it ran returned, so it is not a frame.
(deftype LookupRuns [] clojure.lang.ILookup
  (valAt [_ k] (ret-f 2)) (valAt [_ k d] (ret-f 3)))
(defn- kw-then-throw [t] (let [v (:k t)] (+ v (nth [] 3))))
(defn- get-then-throw [t] (let [v (get t :k)] (+ v (nth [] 3))))
(defn- coll-then-throw [m] (let [v (m 5)] (+ v (nth [] 3))))
(doseq [[label f arg] [["keyword lookup" kw-then-throw (LookupRuns.)]
                       ["get" get-then-throw (LookupRuns.)]
                       ["collection call" coll-then-throw
                        (sorted-map-by (fn [a b] (ret-f 1) (compare a b)) 5 1)]]]
  (let [e (try (f arg) (catch Exception x x))]
    (ok= (some #{"stack_trace_test$ret_f" "stack_trace_test$ret_g"} (frame-names e))
         nil
         (str "a " label " that ran a jolt fn leaves no returned frame"))))
;; a value-semantics seam is jolt code the HOST runs from inside hash/=/str of any
;; value. Its tail call (to boolean) stored a site pair that outlived it, and the
;; next throw from host code reported the predicate as its innermost frame:
;; jolt-lang/time's jt? was in every nREPL error once it loaded.
(defn- seam-claims? [v] (boolean (identical? v ::claimed)))
(__register-hash! seam-claims? (fn [_] 0))
(__register-str! seam-claims? (fn [_] "claimed"))
(def ^:private seam-probe (Object.))
(defn- seam-then-throw []
  (let [h (hash seam-probe) s (str seam-probe) v (nth [] 3)] [h s v]))
(let [e (try (seam-then-throw) (catch Exception x x))
      cls (mapv #(.getClassName %) (.getStackTrace e))]
  (ok= (some #(str/includes? % "seam_claims") cls) nil
       "a seam the host called is not a frame of a later host throw")
  (ok= (first (own-frames (.getStackTrace e)))
       ["stack_trace_test$seam_then_throw" "stack-trace-test.clj"]
       "...the fn that was running is the innermost frame"))
;; ...while a fn that tail-calls the host fn that throws keeps its erased frame
(defn- tail-into-host [v] (nth v 3))
(defn- calls-tail-into-host [] (let [r (tail-into-host [])] r))
(ok= (mapv first (take 2 (own-frames (.getStackTrace (try (calls-tail-into-host)
                                                          (catch Exception x x))))))
     ["stack_trace_test$tail_into_host" "stack_trace_test$calls_tail_into_host"]
     "a tail call into a throwing host fn keeps the erased frame")

;; --- custom assertions inside binding / try-finally ----------------------------
;; matcher-combinators reads Thread.getStackTrace before it calls do-report,
;; dropping its own helper frames. A binding's unnamed dynamic-wind thunk used
;; to erase the test body's frame; macro-generated calls also had no line.
(defn- assertion-location []
  (let [s (first (drop-while
                  (fn [e]
                    (let [c (.getClassName e)]
                      (or (str/starts-with? c "java.lang.")
                          (str/starts-with? c "stack_trace_test$assertion_location"))))
                  (.getStackTrace (Thread/currentThread))))]
    {:file (when s (.getFileName s))
     :line (when s (.getLineNumber s))
     :class (when s (.getClassName s))}))

(defmethod clojure.test/assert-expr 'stack-location? [_ form]
  (let [report `(clojure.test/do-report
                 (assoc (assertion-location) :type :fail
                        :expected-line ~(:line (meta form))))]
    (if (second form)
      `(try (throw (ex-info "mismatch" {})) (catch Exception e# ~report))
      report)))

(clojure.test/deftest ^:private custom-stack-locations
  (clojure.test/is (stack-location?))
  (clojure.test/is (stack-location? :caught)))
(let [reports (atom [])]
  (binding [clojure.test/report (fn [m] (when (= :fail (:type m)) (swap! reports conj m)))]
    (clojure.test/test-var #'custom-stack-locations))
  (ok= (count @reports) 2 "both custom assertion failure paths read their stack")
  (doseq [r @reports]
    (ok= (:file r) "stack-trace-test.clj" "a custom assertion names its test file")
    (ok= (:line r) (:expected-line r) "a custom assertion names its exact source line")
    (ok= (:class r) "stack_trace_test$custom_stack_locations$fn__0"
         "the first caller outside the helper is the test body")))

;; Naming a finally thunk must leave self-calls bound to the original fn.
(defn- bound-recursive [n]
  (binding [*print-length* n]
    (if (pos? n) (bound-recursive (dec n)) *print-length*)))
(ok= (bound-recursive 3) 0 "self-calls inside binding still call the original fn")

(def ^:private finally-stack (atom nil))
(defn- reads-in-finally []
  (try :body
       (finally (reset! finally-stack (here)))))
(reads-in-finally)
(ok= (take 2 (mapv first (own-frames @finally-stack)))
     ["stack_trace_test$here" "stack_trace_test$reads_in_finally"]
     "a finally callback keeps the enclosing fn's frame")

(let [n @passes f @fails]
  (doseq [m f] (println "stack-trace FAIL " m))
  (println "STACK-TRACE-RESULT pass" n "fail" (count f))
  (println (if (zero? (count f)) "STACK-TRACE OK" "STACK-TRACE FAIL"))
  (flush))
