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

(let [n @passes f @fails]
  (doseq [m f] (println "stack-trace FAIL " m))
  (println "STACK-TRACE-RESULT pass" n "fail" (count f))
  (println (if (zero? (count f)) "STACK-TRACE OK" "STACK-TRACE FAIL"))
  (flush))
