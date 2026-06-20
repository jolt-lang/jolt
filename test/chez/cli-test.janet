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
   ["nil" ""]])

(each [src want] cases
  (def [code out err] (joltc src))
  (ok (string "joltc: " (if (> (length src) 48) (string (string/slice src 0 48) "...") src))
      (and (= code 0) (= out want))
      (string "[" code "] got " (string/format "%j" out) " want " (string/format "%j" want)
              (if (= "" err) "" (string " err: " (string/slice err 0 (min 120 (length err))))))))

(printf "\ncli-test: %d/%d checks passed" (- total fails) total)
(os/exit (if (zero? fails) 0 1))
