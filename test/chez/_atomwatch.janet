# jolt-mn9o — atom watches + validators on Chez. The Chez atom record carries
# watches (alist) + validator slots; swap!/reset! validate-then-set-then-notify;
# add-watch/remove-watch/set-validator!/get-validator are native (post-prelude.ss
# re-asserts them over the overlay's ref-put!-based versions).
#
#   janet test/chez/_atomwatch.janet
(def jolt-bin (or (os/getenv "JOLT_BIN") "bin/joltc"))

# [expr expected] — :throws asserts a non-zero exit (validator rejection); the
# exception message text is not compared.
(def cases
  [["(let [a (atom 0) seen (atom 0)] (add-watch a :k (fn [k r o n] (reset! seen 1))) (reset! a 5) @seen)" "1"]
   ["(let [a (atom 0) seen (atom 0)] (add-watch a :k (fn [k r o n] (swap! seen inc))) (remove-watch a :k) (reset! a 5) @seen)" "0"]
   ["(let [a (atom 0) log (atom [])] (add-watch a :k (fn [k r o n] (swap! log conj [o n]))) (reset! a 1) (reset! a 2) @log)" "[[0 1] [1 2]]"]
   ["(let [a (atom 0)] (set-validator! a number?) (reset! a 5) @a)" "5"]
   ["(let [a (atom 5)] (set-validator! a pos?) (swap! a inc) @a)" "6"]
   ["(let [a (atom 0)] (set-validator! a number?) (fn? (get-validator a)))" "true"]
   ["(let [a (atom 0)] (nil? (get-validator a)))" "true"]
   ["(let [a (atom 0)] (set-validator! a pos?) (reset! a -1))" :throws]
   ["(let [a (atom 5)] (set-validator! a pos?) (swap! a (fn [_] -1)) @a)" :throws]
   ["(let [a (atom 0) c (atom 0)] (add-watch a :k (fn [k r o n] (swap! c inc))) (swap! a inc) (swap! a inc) @c)" "2"]])

(defn run-capture [bin expr]
  (def proc (os/spawn [bin "-e" expr] :p {:out :pipe :err :pipe}))
  (def out (ev/read (proc :out) 0x100000))
  (def err (ev/read (proc :err) 0x100000))
  (def code (os/proc-wait proc))
  (def lines (filter (fn [l] (not (empty? l)))
                     (string/split "\n" (string/trim (if out (string out) "")))))
  [code (if (empty? lines) "" (last lines)) (string/trim (if err (string err) ""))])

(var pass 0)
(def fails @[])
(each [expr expected] cases
  (def [code got err] (run-capture jolt-bin expr))
  (cond
    (= expected :throws)
      (if (not= code 0) (++ pass)
        (array/push fails [expr (string "expected throw; exit " code)]))
    (not= code 0) (array/push fails [expr (string "exit " code "; err: " err)])
    (= got expected) (++ pass)
    (array/push fails [expr (string "want `" expected "`, got `" got "`")])))

(printf "\n_atomwatch parity [%s]: %d/%d passed" jolt-bin pass (length cases))
(when (> (length fails) 0)
  (printf "%d FAIL(s):" (length fails))
  (each [e m] fails (printf "  FAIL %s\n    %s" e m)))
(flush)
(os/exit (if (empty? fails) 0 1))
