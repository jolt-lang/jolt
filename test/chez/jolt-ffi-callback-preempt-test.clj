;; A fiber is not preempted inside a foreign callback. The C function that called
;; back is on the carrier thread's stack under the callback; preempting there let
;; another fiber call into C on the same thread over those frames, and the first
;; fiber's return through them raised `attempt to return to stale foreign
;; context`. Fifty fibers sorting with a Clojure qsort comparator lost forty of
;; themselves to it at the default quantum. The callback body now runs as a
;; counted-lock region (host/chez/java/ffi.ss jolt-ffi-callable-body), so the
;; preempt handler defers, and a park inside a callback raises in jolt's words.
;; Run: bin/jolt run test/chez/jolt-ffi-callback-preempt-test.clj, or `make ffi`;
;; exits nonzero on a failure, and prints "JOLT-FFI-CALLBACK-PREEMPT-TEST OK".
(ns jolt-ffi-callback-preempt-test
  (:refer-clojure :exclude [sorted?]))

(require '[jolt.ffi :as ffi] '[jolt.fibers :as f] '[jolt.scheme :as scm])

(def failures (atom []))

(defmacro check-eq [label got want]
  `(let [g# ~got w# ~want]
     (when-not (= g# w#)
       (swap! failures conj (str ~label ": want " (pr-str w#) " got " (pr-str g#))))))

(ffi/defcfn c-qsort "qsort" [:pointer :size_t :size_t :pointer] :void)

(defn- descending [n]
  (let [b (ffi/alloc (* 4 n))]
    (dotimes [i n] (ffi/write b :int (- n i) (* 4 i)))
    b))

(defn- sorted? [b n] (every? #(= (inc %) (ffi/read b :int (* 4 %))) (range n)))

(defn- busy [n] (loop [i 0 acc 0] (if (< i n) (recur (inc i) (+ acc (* i i))) acc)))

(defn- comparator-calling [g]
  (ffi/foreign-callable
   (fn [a b] (g) (compare (ffi/read a :int) (ffi/read b :int)))
   [:pointer :pointer] :int))

(defn- outcome [thunk]
  (f/join (f/spawn (fn [] (try (thunk) (catch Throwable t (str "threw: " (.getMessage t))))))))

;; The repro: every fiber spends most of its time inside the comparator, so the
;; quantum keeps expiring there. 100 ticks is the floor, the tightest setting.
(def slow-cmp (comparator-calling #(busy 200)))
(doseq [ticks [100 1000000]]
  (f/set-preempt-ticks! ticks)
  (let [fs (mapv (fn [_] (f/spawn #(let [b (descending 300)]
                                     (c-qsort b 300 4 slow-cmp)
                                     (let [ok (sorted? b 300)] (ffi/free b) ok))))
                 (range 50))
        rs (mapv #(try (f/join %) (catch Throwable t (.getMessage t))) fs)]
    (check-eq (str "50 fibers sort through a Clojure comparator at " ticks " ticks")
              (frequencies rs) {true 50})))
(f/set-preempt-ticks! nil)

;; A park inside a callback cannot leave the CPU either; it raises, naming the
;; callback, instead of resuming later over someone else's C frames.
(let [r (outcome #(let [b (descending 4)] (c-qsort b 4 4 (comparator-calling (fn [] (Thread/sleep 1)))) :returned))]
  (check-eq "a park inside a callback raises naming the callback"
            (boolean (and (string? r) (re-find #"foreign callback" r))) true))

;; The count is balanced on the way out, normally and by a raise: the same fiber
;; parks afterwards.
(check-eq "a fiber parks after a callback returned"
          (outcome #(let [b (descending 50)] (c-qsort b 50 4 (comparator-calling (fn [] nil))) (Thread/sleep 2) (f/yield) :parked))
          :parked)
(check-eq "a fiber parks after a callback raised"
          (outcome #(let [b (descending 4)
                          r (try (c-qsort b 4 4 (comparator-calling (fn [] (throw (ex-info "boom" {})))))
                                 (catch Throwable _ :caught))]
                      (Thread/sleep 2)
                      [r :parked]))
          [:caught :parked])

;; The callback entry mask preserves the caller's interrupt depth on both
;; normal and exceptional exits, including an already masked caller. Cover both
;; adapter spellings: collect-safe callbacks have the same boundary obligation.
(let [raise? (atom false)
      cmp (fn [a b] (if @raise? (throw (ex-info "depth-check" {}))
                       (compare (ffi/read a :int) (ffi/read b :int))))
      ordinary (ffi/foreign-callable cmp [:pointer :pointer] :int)
      collect-safe (ffi/foreign-callable cmp [:pointer :pointer] :int :collect-safe)]
  (doseq [cb [ordinary collect-safe] masked? [false true]]
    (when masked? (scm/call "disable-interrupts"))
    (try
      (let [depth (scm/call "sa-disable-count")]
        (doseq [throws? [false true]]
          (reset! raise? throws?)
          (let [b (descending 4)
                result (try (c-qsort b 4 4 cb) :returned (catch Throwable _ :caught))]
            (ffi/free b)
            (check-eq "callback normal/raise result" result (if throws? :caught :returned))
            (check-eq "callback preserves incoming interrupt depth"
                      (scm/call "sa-disable-count") depth))))
      (finally (when masked? (scm/call "enable-interrupts")))))
  (ffi/free-callable ordinary)
  (ffi/free-callable collect-safe))

;; A void foreign callback ignores zero or multiple Scheme values, as Chez's
;; ordinary foreign-callable does. Public FFI spellings retain that contract.
(let [zero (scm/proc "values")
      many (scm/eval-string "(lambda () (values 1 2))")
      callbacks [(ffi/foreign-callable zero [] :void)
                 (ffi/foreign-callable many [] :void)]]
  (doseq [cb callbacks]
    (let [call ((scm/eval-string "(lambda (addr) (foreign-procedure addr () void))") cb)
          depth (scm/call "sa-disable-count")]
      (check-eq "void callback discards zero/multiple values" (do (call) :returned) :returned)
      (check-eq "void callback preserves interrupt depth" (scm/call "sa-disable-count") depth))
    (ffi/free-callable cb)))

(let [zero (scm/proc "values")
      many (scm/eval-string "(lambda () (values 1 2))")
      callbacks [(ffi/foreign-callable zero [] :int)
                 (ffi/foreign-callable many [] :int)]]
  (doseq [cb callbacks]
    (let [call ((scm/eval-string "(lambda (addr) (foreign-procedure addr () int))") cb)
          depth (scm/call "sa-disable-count")]
      (check-eq "nonvoid callback rejects zero/multiple values"
                (try (call) :returned (catch Throwable _ :caught)) :caught)
      (check-eq "rejected nonvoid callback preserves interrupt depth"
                (scm/call "sa-disable-count") depth))
    (ffi/free-callable cb)))

;; Preemption still works outside callbacks: two spinning fibers on a one-carrier
;; pool both make progress after each made a callback.
(f/set-preempt-ticks! 10000)
(let [p (f/pool {:name :callback-preempt-one :size 1})
      counts (atom [0 0])
      stop (atom false)
      spin (fn [i] (fn []
                     (let [b (descending 20)] (c-qsort b 20 4 (comparator-calling (fn [] nil))))
                     (loop [] (when-not @stop (swap! counts update i inc) (recur)))))
      a (f/spawn (spin 0) {:pool p})
      b (f/spawn (spin 1) {:pool p})]
  (Thread/sleep 200)
  (reset! stop true)
  (f/join a) (f/join b)
  (f/shutdown! p)
  (check-eq "two spinners on one carrier both run after a callback" (every? pos? @counts) true))
(f/set-preempt-ticks! nil)

(if (empty? @failures)
  (do (println "JOLT-FFI-CALLBACK-PREEMPT-TEST OK") (flush) (System/exit 0))
  (do (doseq [x @failures] (println "FAIL:" x))
      (println "JOLT-FFI-CALLBACK-PREEMPT-TEST FAILED:" (count @failures))
      (flush)
      (System/exit 1)))
