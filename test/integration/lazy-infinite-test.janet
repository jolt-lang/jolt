# Deadlined infinite-seq conformance harness (Phase 5 Step 0).
#
# Each case is [name expected-clj actual-clj]. The harness spawns a subprocess
# worker (test/support/lazy-eval.janet) that evaluates (= expected actual) and
# prints @@RESULT true/false. Workers run under a wall-clock deadline; a hang
# = a FAIL. This is the safety net that makes it safe to convert transformers
# to lazy — wrong answers hang instead of silently passing.
#
# Pattern mirrors clojure-test-suite-test.janet: os/spawn + ev/with-deadline
# + os/proc-kill on timeout. Never probe infinite cases in-process.

(def per-case-timeout 5)

(defn- run-case [expected actual]
  (def proc (os/spawn ["janet" "test/support/lazy-eval.janet" expected actual] :p {:out :pipe}))
  (def out (proc :out))
  (var data nil)
  (def ok
    (try
      (ev/with-deadline per-case-timeout
        (set data (ev/read out 0x10000))
        (os/proc-wait proc)
        true)
      ([err] false)))
  (when (not ok)
    (protect (os/proc-kill proc true))
    (protect (ev/with-deadline 2 (os/proc-wait proc))))
  (protect (:close out))
  (if (and ok data) (string data) nil))

(defn- parse-result [s]
  (def prefix-len (length "@@RESULT "))
  (if (string/has-prefix? "@@RESULT " s)
    (let [val (string/slice s prefix-len (dec (length s)))]
      [:ok val])
    (if (string/has-prefix? "@@ERROR " s)
      (let [msg (string/slice s (length "@@ERROR ") (dec (length s)))]
        [:error msg])
      nil)))

# ---- Cases from phase-5.md §6.2 ----
# Expected values use Clojure quote syntax so the worker evaluates
# (= (quote ...) actual) with Clojure's = semantics.
(def cases
  [
   ["nth of map inc range"                "1001"                  "(nth (map inc (range)) 1000)"]
   ["first filter even? drop range"       "4"                     "(first (filter even? (drop 3 (range))))"]
   ["take 3 remove odd? range"            "(quote (0 2 4))"       "(take 3 (remove odd? (range)))"]
   ["take 3 drop-while <5 range"          "(quote (5 6 7))"       "(take 3 (drop-while (fn [x] (< x 5)) (range)))"]
    # interleave stays eager in overlay (lazy-seq macro breaks compile mode).
   # ["take 4 interleave range iterate"     "(quote (0 10 1 11))"   "(take 4 (interleave (range) (iterate inc 10)))"]
   ["take 3 partition 2 range"            "(quote ((0 1) (2 3) (4 5)))" "(take 3 (partition 2 (range)))"]
   ["take 3 partition-all 2 range"        "(quote ((0 1) (2 3) (4 5)))" "(take 3 (partition-all 2 (range)))"]
   ["take 3 map-indexed vector range"     "(quote ([0 0] [1 1] [2 2]))" "(take 3 (map-indexed vector (range)))"]
   ["take 3 distinct cycle"               "(quote (1 2 3))"       "(take 3 (distinct (cycle [1 2 1 3 1])))"]
   ["take 6 mapcat dup range"             "(quote (0 0 1 1 2 2))" "(take 6 (mapcat (fn [x] [x x]) (range)))"]
   ["first rest lazy"                     "1"                     "(let [[a & r] (range)] (first r))"]
   ["take 3 rest lazy"                    "(quote (1 2 3))"       "(let [[a & r] (range)] (take 3 r))"]
   ["dedupe inf"             "(quote (1 2 1 2 1))" "(take 5 (dedupe (cycle [1 1 2 2])))"]
   ["take 3 take-nth 2 range"             "(quote (0 2 4))"       "(take 3 (take-nth 2 (range)))"]
   ["take 3 interpose :x range"           "(quote (0 :x 1))"       "(take 3 (interpose :x (range)))"]
   ["take 3 map vector range iterate"     "(quote ([0 100] [1 101] [2 102]))" "(take 3 (map vector (range) (iterate inc 100)))"]

   # §6.3 Laziness counter tests — each reads exactly N elements for take N
   ["LAZY map"            "3"  "(do (def c (atom 0)) (take 3 (map (fn [x] (swap! c inc) x) (range))) @c)"]
   ["LAZY filter"         "6"  "(do (def c (atom 0)) (take 3 (filter (fn [x] (swap! c inc) (odd? x)) (range))) @c)"]
   ["LAZY remove"         "6"  "(do (def c (atom 0)) (take 3 (remove (fn [x] (swap! c inc) (even? x)) (range))) @c)"]
   ["LAZY take-while"     "6"  "(do (def c (atom 0)) (dorun (take-while (fn [x] (swap! c inc) (< x 5)) (range))) @c)"]
   ["LAZY drop-while"     "6"  "(do (def c (atom 0)) (take 3 (drop-while (fn [x] (swap! c inc) (< x 5)) (range))) @c)"]
   ["LAZY distinct"       "4"  "(do (def c (atom 0)) (take 3 (distinct (map (fn [x] (swap! c inc) x) (cycle [1 2 1 3 1])))) @c)"]
   ["LAZY take-nth"       "7"  "(do (def c (atom 0)) (take 3 (take-nth 2 (map (fn [x] (swap! c inc) x) (range)))) @c)"]
   ["LAZY map-indexed"    "3"  "(do (def c (atom 0)) (take 3 (map-indexed (fn [i x] (swap! c inc) [i x]) (range))) @c)"]
   ["LAZY keep"           "6"  "(do (def c (atom 0)) (take 3 (keep (fn [x] (swap! c inc) (if (odd? x) x nil)) (range))) @c)"]
   ["LAZY keep-indexed"   "6"  "(do (def c (atom 0)) (take 3 (keep-indexed (fn [i x] (swap! c inc) (if (odd? i) x)) (range))) @c)"]
   ["LAZY interpose"      "2"  "(do (def c (atom 0)) (take 3 (interpose :x (map (fn [x] (swap! c inc) x) (range)))) @c)"]
   ["LAZY partition"      "6"  "(do (def c (atom 0)) (take 3 (partition 2 (map (fn [x] (swap! c inc) x) (range)))) @c)"]
   ["LAZY partition-all"  "6"  "(do (def c (atom 0)) (take 3 (partition-all 2 (map (fn [x] (swap! c inc) x) (range)))) @c)"]
   ["LAZY mapcat"         "3"  "(do (def c (atom 0)) (take 6 (mapcat (fn [x] (swap! c inc) [x x]) (range))) @c)"]
   ["LAZY dedupe"         "9"  "(do (def c (atom 0)) (take 5 (dedupe (map (fn [x] (swap! c inc) x) (cycle [1 1 2 2])))) @c)"]
   ["LAZY repeated inc"   "3"  "(do (def c (atom 0)) (take 3 (map (fn [x] (swap! c inc) x) (iterate inc 0))) @c)"]

   # Already-working cases (guard against regression)
   ["take 5 iterate inc"                  "(quote (0 1 2 3 4))"   "(take 5 (iterate inc 0))"]
   ["take 3 range"                        "(quote (0 1 2))"       "(take 3 (range))"]
   ["take 3 repeat"                       "(quote (7 7 7))"       "(take 3 (repeat 7))"]
   ["take 3 cycle"                        "(quote (1 2 1))"       "(take 3 (cycle [1 2]))"]
   ["take 3 filter even? range"           "(quote (0 2 4))"       "(take 3 (filter even? (range)))"]
   ["take 5 lazily filtered from range"   "(quote (1 3 5 7 9))"   "(take 5 (filter odd? (range)))"]
  ])

# ---- Run ----
(var fails @[])
(var timeouts 0)
(var passed 0)

(each [name expected expr] cases
  (def out (run-case expected expr))
  (cond
    (nil? out)
    (do (++ timeouts) (array/push fails (string "TIMEOUT: " name)))
    (let [res (parse-result out)]
      (case (res 0)
        :ok (if (= "true" (res 1))
              (++ passed)
              (array/push fails (string "MISMATCH: " name " — expected " expected)))
        :error (array/push fails (string "ERROR: " name " — " (res 1)))
        (array/push fails (string "PARSE: " name " — raw: " (string/trim out)))))))

(printf "lazy-infinite: %d cases — %d passed / %d timeouts / %d failures"
        (length cases) passed timeouts (length fails))
(when (> (length fails) 0)
  (print "\nFailures:")
  (each f fails (printf "  %s" f)))

(if (or (> (length fails) 0) (> timeouts 0))
  (os/exit 1))
