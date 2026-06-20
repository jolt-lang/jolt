# Chez Phase 3 inc9b (jolt-9phg) — the pure-Chez runtime CLI (no Janet).
#
# bin/joltc execs `chez --script host/chez/cli.ss`, which loads the checked-in
# bootstrap seed (host/chez/seed/) + the zero-Janet spine and compiles+evals a
# Clojure -e expression entirely on Chez. This test drives bin/joltc and checks
# results: with only Chez installed, jolt runs end to end — no Janet at build or
# run time. (This harness is Janet only to spawn the process; the compile+eval is
# 100% Chez.)
#
#   janet test/chez/cli-test.janet
(import ../../host/chez/driver :as d)

(var total 0) (var fails 0)
(defn ok [name pred &opt extra]
  (++ total)
  (if pred (printf "ok: %s" name)
    (do (++ fails) (printf "FAIL: %s   %s" name (or extra "")))))

(unless (d/chez-available?)
  (print "chez not on PATH — skipping cli-test")
  (os/exit 0))

(defn- joltc [expr]
  (def proc (os/spawn ["bin/joltc" "-e" expr] :p {:out :pipe :err :pipe}))
  (def out (string/trim (string (:read (proc :out) :all))))
  (def err (string/trim (string (or (:read (proc :err) :all) ""))))
  (def code (os/proc-wait proc))
  [code out err])

(def cases
  [["(+ 1 2)" "3"]
   ["(defn fib [n] (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))) (fib 15)" "610"]
   ["(->> (range 10) (filter even?) (map #(* % %)) (reduce +))" "120"]
   ["(let [{:keys [a b] :or {b 99}} {:a 1}] [a b])" "[1 99]"]
   ["(require '[clojure.string :as s]) (s/upper-case \"hello\")" "HELLO"]
   ["(case 3 1 :a 2 :b :other)" ":other"]
   ["(reduce + (vals (reduce (fn [m k] (assoc m k (* k k))) {} [1 2 3])))" "14"]
   ["(map inc [1 2 3])" "(2 3 4)"]
   ["nil" ""]
   # jolt-r8ku: runtime eval / load-string / defmacro on the Chez spine.
   ["(eval (quote (+ 1 2)))" "3"]
   ["(eval (list (quote +) 1 2 3))" "6"]
   ["(eval (quote (let [a 2 b 3] (* a b))))" "6"]
   ["(load-string \"(+ 1 2)\")" "3"]
   ["(load-string \"(def y 5) (* y y)\")" "25"]
   ["(load-string \"\")" ""]
   ["(map eval [(quote (+ 1 1)) (quote (* 3 3))])" "(2 9)"]
   ["(defmacro add1 [x] (list (quote +) x 1)) (add1 10)" "11"]
   ["(defmacro twice [x] `(do ~x ~x)) (twice (+ 2 3))" "5"]
   ["(defmacro m [x] `(+ ~x 1)) (m (m (m 0)))" "3"]
   # jolt-byjr: concurrency — futures/pmap/promise on real OS threads (shared heap).
   ["(deref (future (+ 1 2)))" "3"]
   ["@(future (* 6 7))" "42"]
   ["(deref (future (mapv inc [1 2 3])))" "[2 3 4]"]
   ["(let [f (future (+ 1 1))] [(deref f) (deref f)])" "[2 2]"]
   ["(future? (future 1))" "true"]
   ["(future? 42)" "false"]
   ["(let [f (future 1)] (deref f) (future-done? f))" "true"]
   ["(let [f (future 1)] (deref f) (realized? f))" "true"]
   ["(let [f (future 42)] (deref f) (deref f 1000 :nope))" "42"]
   ["(vec (pmap inc [1 2 3]))" "[2 3 4]"]
   ["(vec (pmap + [1 2 3] [4 5 6]))" "[5 7 9]"]
   ["(vec (pcalls (fn [] 1) (fn [] 2)))" "[1 2]"]
   ["(vec (pvalues (+ 1 2) (+ 3 4)))" "[3 7]"]
   # shared heap = JVM semantics (NOT Janet's isolated-heap snapshot): a captured
   # atom is shared, so the future's swap! is visible to the parent.
   ["(let [a (atom 0)] (deref (future (swap! a inc))) @a)" "1"]
   ["(let [a (atom 0)] (dorun (pmap (fn [_] (swap! a inc)) [1 2 3 4])) @a)" "4"]
   # promise blocks until delivered (JVM), unlike the Janet atom-shim.
   ["(let [p (promise)] (deliver p 7) @p)" "7"]
   ["(let [p (promise)] (future (deliver p :hi)) @p)" ":hi"]
   # jolt-byjr: clojure.core.async on real-thread blocking channels.
   ["(require (quote [clojure.core.async :refer [chan go <! >! <!!]])) (def c (chan)) (go (>! c (+ 40 2))) (<!! c)" "42"]
   ["(require (quote [clojure.core.async :refer [chan go go-loop <! >! <!! close!]])) (def c (chan 5)) (go (>! c 1) (>! c 2) (>! c 3) (close! c)) (<!! (go-loop [o []] (let [v (<! c)] (if (nil? v) o (recur (conj o v))))))" "[1 2 3]"]
   ["(require (quote [clojure.core.async :refer [chan go <! >! <!!]])) (def x (chan)) (def y (chan)) (go (>! x 10)) (go (>! y 32)) (<!! (go (+ (<! x) (<! y))))" "42"]
   ["(require (quote [clojure.core.async :refer [chan go <! >! <!! alts!]])) (def x (chan)) (def y (chan)) (go (>! y :v)) (<!! (go (let [[v ch] (alts! [x y])] (and (= v :v) (= ch y)))))" "true"]
   ["(require (quote [clojure.core.async :refer [chan go go-loop <! >! <!! close!]])) (def c (chan 10 (map inc))) (go (>! c 1) (>! c 2) (>! c 3) (close! c)) (<!! (go-loop [o []] (let [v (<! c)] (if (nil? v) o (recur (conj o v))))))" "[2 3 4]"]
   ["(require (quote [clojure.core.async :refer [timeout <!!]])) (<!! (timeout 10)) :done" ":done"]
   ["(require (quote [clojure.core.async :refer [chan go <! >! <!!]])) (def ^:dynamic *x* 0) (<!! (binding [*x* 7] (go (<! (clojure.core.async/timeout 5)) *x*)))" "7"]
   # jolt-byjr: async agents (serialized per-agent dispatch); await for determinism.
   ["(deref (agent 0))" "0"]
   ["(let [a (agent 0)] (send-off a + 5) (await a) (deref a))" "5"]
   ["(let [a (agent 1)] (send a + 6) (await a) (deref a))" "7"]
   ["(let [a (agent 0)] (dotimes [_ 100] (send a inc)) (await a) (deref a))" "100"]
   ["(agent-error (agent 0))" ""]
   ["(let [a (agent 0)] (send a (fn [_] (throw (ex-info \"boom\" {})))) (await a) (boolean (agent-error a)))" "true"]])

(each [src want] cases
  (def [code out err] (joltc src))
  (ok (string "joltc: " (if (> (length src) 48) (string (string/slice src 0 48) "...") src))
      (and (= code 0) (= out want))
      (string "[" code "] got " (string/format "%j" out) " want " (string/format "%j" want)
              (if (= "" err) "" (string " err: " (string/slice err 0 (min 120 (length err))))))))

(printf "\ncli-test: %d/%d checks passed" (- total fails) total)
(os/exit (if (zero? fails) 0 1))
