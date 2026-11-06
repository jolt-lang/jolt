;; jolt.debug gate: frames with their locals for a caught throwable, and the
;; break hook that runs before the stack unwinds. The fixture is loaded under
;; jolt.profile's :debug mode; this file is not, which is the uninstrumented case.
;; Run: bin/jolt run test/chez/debug-test.clj (smoke.sh greps "DEBUG-TEST OK").
(ns debug-test
  (:require [clojure.string :as str]
            [jolt.debug :as dbg]
            [jolt.profile :as prof]))

(def failures (atom []))
(defn check [label got want]
  (when-not (= got want)
    (swap! failures conj (str label ": want " (pr-str want) " got " (pr-str got)))))

(def fixture "test/chez/fixtures/debug_fixture.clj")
(defn caught [f] (try (f) nil (catch Throwable t t)))
(defn frames-of [f] (dbg/frames (caught f)))
(defn frame [fs sym] (first (filter #(= sym (:fn %)) fs)))
(defn fx [nm] @(resolve (symbol "debug-fixture" nm)))

(check "mode off at start" (prof/instrument-mode) nil)
(check "instrument! :debug" (prof/instrument! :debug) true)
(check "mode is :debug" (prof/instrument-mode) :debug)
(check "instrumenting? in :debug" (prof/instrumenting?) true)
(load-file fixture)
(prof/instrument! false)
(check "mode off after" (prof/instrument-mode) nil)
(check "instrumenting? off after" (prof/instrumenting?) false)

;; a tail (/ y 0) and a tail call into it: both frames stay, with their locals
(let [fs (frames-of #((fx "outer") 20))
      in (frame fs 'debug-fixture/inner)]
  (check "innermost frame" (:fn (first fs)) 'debug-fixture/inner)
  (check "inner locals" (:locals in) '{x 21 y 42})
  (check "outer locals" (:locals (frame fs 'debug-fixture/outer)) '{a 20 b 21})
  (check "inner line" (:line in) 5)
  (check "inner file" (str/ends-with? (str (:file in)) "debug_fixture.clj") true)
  (check "frame keys" (set (keys in)) #{:fn :file :line :locals}))

;; a throw inside recur's args sees the iteration's bindings
(check "loop locals"
       (:locals (frame (frames-of #((fx "looper") 5)) 'debug-fixture/looper))
       '{n 5 i 3 acc [0 1 2]})

;; a catch local and a let inside the catch
(let [ls (:locals (frame (frames-of #((fx "catcher") "nope")) 'debug-fixture/catcher))]
  (check "catch locals" (set (keys ls)) '#{s e msg})
  (check "catch local value" (get ls 's) "nope"))

;; non-recur recursion keeps every frame, as on the JVM
(check "recursion keeps each frame"
       (map (comp 'n :locals)
            (filter #(= 'debug-fixture/bottom-out (:fn %)) (frames-of #((fx "bottom-out") 3))))
       [0 1 2 3])
;; ...and recur stays a jump: a long loop does not grow the stack
(check "recur loop" ((fx "sum-to") 1000000) 499999500000)

(check "a shadowing local wins"
       (:locals (frame (frames-of #((fx "shadow") 1)) 'debug-fixture/shadow)) '{x 20})
(check "a rest param"
       (:locals (frame (frames-of #((fx "variadic") 1 2 3)) 'debug-fixture/variadic))
       '{a 1 more (2 3)})
(check "destructured names"
       (select-keys (:locals (frame (frames-of #((fx "destructure") {:k 7} [1 2]))
                                    'debug-fixture/destructure))
                    '[k p q])
       '{k 7 p 1 q 2})
(let [fs (frames-of #((fx "with-fn") 1))]
  (check "an anonymous fn's frame" (:locals (first fs)) '{y 2})
  (check "...named as Clojure demunges it"
         (str/starts-with? (str (:fn (first fs))) "debug-fixture/with-fn/fn--") true)
  (check "...under the fn it sits in"
         (get (:locals (frame fs 'debug-fixture/with-fn)) 'x) 1))

;; code compiled without the mode: only what Chez kept live, y here, since the
;; frame reads it after the call that threw
(defn plain [x] (let [y (inc x)] (+ y (/ x 0) y)))
(check "uninstrumented frame keeps a live local"
       (get (:locals (frame (frames-of #(plain 1)) 'debug-test/plain)) 'y) 2)

(def boom @(resolve 'debug-fixture/boom))

;; a record method's and a defmethod's bodies are fn literals no def covers: the
;; trace drops their frames, and jolt.debug names them as jolt.profile does
(let [bomb ((resolve 'debug-fixture/->Bomb) 4)
      fs (frames-of #(boom bomb 5))
      [top & below] fs]
  (check "a record method's frame" (str (:fn top)) "debug-fixture/Bomb.boom")
  (check "...its locals" (:locals top) {'this bomb 'n 5 'k 4 'm 9})
  (check "...its line" (:line top) 66)
  (check "the method's locals stay out of the frames below it"
         (some (fn [fr] (some #(identical? bomb %) (vals (:locals fr)))) below) nil))
(let [top (first (frames-of #((fx "area") {:kind :sq :w 3})))]
  (check "a defmethod's frame" (str (:fn top)) "debug-fixture/area[:sq]")
  (check "...its locals" (:locals top) '{w 3 a 9}))
(let [box ((resolve 'debug-fixture/->Box) 3)]
  (check "an extend-type method's frame"
         (str (:fn (first (frames-of #((fx "size-of") box))))) "debug-fixture/Box.size-of"))
(check "an extend-protocol method's frame"
       (str (:fn (first (frames-of #((fx "size-of") "abc"))))) "debug-fixture/String.size-of")

;; --- with-break-handler -------------------------------------------------------
(def seen (atom nil))
(check "break rethrows"
       (try (dbg/with-break-handler
              (fn [t fs] (reset! seen {:msg (ex-message t) :top (first fs)}))
              ((fx "outer") 1))
            (catch ArithmeticException _ :rethrown))
       :rethrown)
(check "break hands the throwable" (:msg @seen) "Divide by zero")
(check "break hands the frames" (:fn (:top @seen)) 'debug-fixture/inner)
(check "break frames have locals" (:locals (:top @seen)) '{x 2 y 4})

(let [seen (atom nil)]
  (try (dbg/with-break-handler (fn [_ fs] (reset! seen fs))
         ((fx "throw-preconstructed") 21))
       (catch Exception _ nil))
  (check "break uses the live throw stack, not the construction stack"
         (:fn (first @seen)) 'debug-fixture/throw-preconstructed)
  (check "break includes locals bound after construction"
         (select-keys (:locals (first @seen)) '[x y]) '{x 21 y 42})
  (check "break omits a constructor frame that already returned"
         (some #(= 'debug-fixture/make-preconstructed (:fn %)) @seen) nil))

(let [seen (atom nil)]
  (try (dbg/with-break-handler (fn [_ fs] (reset! seen fs))
         ((fx "throw-preconstructed-in-loop") 3))
       (catch Exception _ nil))
  (check "break uses the throwing loop iteration"
         (get (:locals (first @seen)) 'i) 3))

;; The hook sits inside a named fn, whose tail call into it leaves its site pair in
;; the slot. That caller is erased, not the innermost frame.
(defn break-wrapper [f] (dbg/with-break-handler (fn [_ fs] (reset! seen fs)) (f)))
(defn nontail-throw [] (let [e (ex-info "x" {})] (+ 1 (throw e))))
(defn tail-throw [] (let [e (ex-info "x" {}) y 1] (throw e)))
(doseq [[label f want] [["a non-tail throw" nontail-throw 'debug-test/nontail-throw]
                        ["a tail throw" tail-throw 'debug-test/tail-throw]
                        ["a debug-compiled throw" #((fx "throw-preconstructed") 1)
                         'debug-fixture/throw-preconstructed]]]
  (reset! seen nil)
  (try (break-wrapper f) (catch Exception _ nil))
  (check (str label " is the innermost frame under a named caller")
         (:fn (first @seen)) want))

(def order (atom []))
(try (dbg/with-break-handler (fn [_ _] (swap! order conj :handler))
       (try (throw (ex-info "x" {})) (finally (swap! order conj :finally))))
     (catch Exception _ nil))
(check "the handler runs before the stack unwinds" @order [:handler :finally])

(def called (atom false))
(check "a catch inside the body"
       (dbg/with-break-handler (fn [_ _] (reset! called true))
         (try (throw (ex-info "x" {})) (catch Exception _ :caught)))
       :caught)
(check "...is not a break" @called false)
(check "a body that returns" (dbg/with-break-handler (fn [_ _] (reset! called true)) 42) 42)
(check "a throwing handler's throw propagates"
       (try (dbg/with-break-handler (fn [_ _] (throw (ex-info "from handler" {})))
              (throw (ex-info "original" {})))
            (catch Exception e (ex-message e)))
       "from handler")
(let [threads (atom [])]
  @(future (try (dbg/with-break-handler
                  (fn [_ _] (swap! threads conj (Thread/currentThread)))
                  (swap! threads conj (Thread/currentThread))
                  (throw (ex-info "t" {})))
                (catch Exception _ nil)))
  (check "the handler runs on the throwing thread"
         (and (= 2 (count @threads)) (identical? (first @threads) (second @threads))) true))
;; a handler that blocks holds the throw where it is until it returns
(let [gate (promise) out (atom [])
      f (future (try (dbg/with-break-handler (fn [_ fs] (swap! out conj (:fn (first fs))) @gate)
                       ((fx "outer") 1))
                     (catch Exception _ (swap! out conj :unwound))))]
  (Thread/sleep 100)
  (check "a blocked handler has not unwound" @out '[debug-fixture/inner])
  (deliver gate true)
  @f
  (check "...until it returns" @out '[debug-fixture/inner :unwound]))

(if (empty? @failures)
  (println "DEBUG-TEST OK")
  (do (doseq [f @failures] (println "FAIL:" f))
      (println "DEBUG-TEST FAILED:" (count @failures))))
