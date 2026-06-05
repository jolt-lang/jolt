# Specification: clojure.core.async on Janet fibers (Phase 1 — API layer).
# Each case is self-contained: it requires the ns, sets up channels/go blocks,
# and ends with a take that pumps the event loop and yields the value compared.
(use ../support/harness)

(def REQ
  "(require '[clojure.core.async :refer [go go-loop chan <! >! close! alts! timeout put! take! chan?]]) ")
(defn- a [body] (string "(do " REQ body ")"))

(defspec "core.async / go & channels"
  ["go produce, <! consume"
   "42" (a "(def c (chan)) (go (>! c (+ 40 2))) (<! c)")]
  ["go returns a result channel"
   "42" (a "(<! (go (* 6 7)))")]
  ["go body nil -> channel closes"
   "nil" (a "(<! (go nil))")]
  ["<! parks mid-expression"
   "42" (a "(def x (chan)) (def y (chan)) (go (>! x 10)) (go (>! y 32)) (<! (go (+ (<! x) (<! y))))")]
  ["chan? true"  "true"  (a "(chan? (chan))")]
  ["chan? false" "false" (a "(chan? [1 2])")])

(defspec "core.async / buffering & close"
  ["buffered channel holds values"
   "[1 2 3]"
   (a "(def c (chan 5)) (go (>! c 1) (>! c 2) (>! c 3) (close! c)) (<! (go-loop [o []] (let [v (<! c)] (if (nil? v) o (recur (conj o v))))))")]
  ["closed channel drains then nil"
   "true"
   (a "(def c (chan 2)) (go (>! c :a) (close! c)) (<! (go (and (= :a (<! c)) (nil? (<! c)))))")]
  [">! to a closed channel is false"
   "false" (a "(def c (chan 1)) (close! c) (>! c 1)")]
  ["take from closed empty channel is nil"
   "nil"   (a "(def c (chan)) (close! c) (<! c)")])

(defspec "core.async / go-loop & pipelines"
  ["go-loop accumulates"
   "6"
   (a "(def in (chan 5)) (go (>! in 1) (>! in 2) (>! in 3) (close! in)) (<! (go-loop [acc 0] (let [v (<! in)] (if (nil? v) acc (recur (+ acc v))))))")]
  ["3-stage concurrent pipeline"
   "50"
   (a "(def in (chan)) (def mid (chan)) (def out (chan)) (go (>! mid (inc (<! in)))) (go (>! out (* 10 (<! mid)))) (go (>! in 4)) (<! out)")])

(defspec "core.async / alts! & timeout"
  ["alts! picks the ready channel"
   "true"
   (a "(def x (chan)) (def y (chan)) (go (>! y :v)) (<! (go (let [[v ch] (alts! [x y])] (and (= v :v) (= ch y)))))")]
  ["timeout wins over an idle channel"
   "true"
   (a "(def slow (chan)) (<! (go (let [[v ch] (alts! [slow (timeout 30)])] (nil? v))))")])

# Janet fibers are stackful, so <! works in positions Clojure's go macro forbids.
(defspec "core.async / parking anywhere"
  ["<! inside try/catch"
   "99" (a "(<! (go (try (<! (go 99)) (catch :default e -1))))")]
  ["<! inside a nested fn called in a go"
   "7"  (a "(def c (chan)) (go (>! c 7)) (<! (go ((fn [] (<! c)))))")])

# Dynamic-var binding conveyance (Phase 2): a go block sees the dynamic bindings
# in effect when it was spawned, concurrent go blocks don't interleave, and a
# go block's own binding shadows the conveyed one.
(defn- d [body] (string "(do " REQ "(def ^:dynamic *x* 0) " body ")"))
(defspec "core.async / binding conveyance"
  ["go conveys the binding"
   "10" (d "(<! (binding [*x* 10] (go (<! (timeout 5)) *x*)))")]
  ["concurrent go blocks isolated"
   "[:a :b]"
   (d "(def ra (binding [*x* :a] (go (<! (timeout 20)) *x*))) (def rb (binding [*x* :b] (go (<! (timeout 5)) *x*))) [(<! ra) (<! rb)]")]
  ["binding doesn't leak to root"
   "0" (d "(<! (binding [*x* 99] (go (<! (timeout 5))))) *x*")]
  ["go's own binding shadows conveyed"
   ":inner"
   (d "(<! (binding [*x* :outer] (go (binding [*x* :inner] (<! (timeout 5)) *x*))))")])
