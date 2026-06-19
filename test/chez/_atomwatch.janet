# jolt-mn9o — atom watches + validators on Chez. The Chez atom record carries
# watches (alist) + validator slots; swap!/reset! validate-then-set-then-notify;
# add-watch/remove-watch/set-validator!/get-validator are native (post-prelude.ss
# re-asserts them over the overlay's ref-put!-based versions). Oracle = build/jolt.
#
#   janet test/chez/_atomwatch.janet
(def jolt-bin (or (os/getenv "JOLT_BIN") "bin/jolt-chez"))

# [expr throws?] — when throws? the case is expected to exit non-zero on both
# (the corpus :throws contract; the exception message text may differ).
(def cases
  [["(let [a (atom 0) seen (atom 0)] (add-watch a :k (fn [k r o n] (reset! seen 1))) (reset! a 5) @seen)" false]
   ["(let [a (atom 0) seen (atom 0)] (add-watch a :k (fn [k r o n] (swap! seen inc))) (remove-watch a :k) (reset! a 5) @seen)" false]
   ["(let [a (atom 0) log (atom [])] (add-watch a :k (fn [k r o n] (swap! log conj [o n]))) (reset! a 1) (reset! a 2) @log)" false]
   ["(let [a (atom 0)] (set-validator! a number?) (reset! a 5) @a)" false]
   ["(let [a (atom 5)] (set-validator! a pos?) (swap! a inc) @a)" false]
   ["(let [a (atom 0)] (set-validator! a number?) (fn? (get-validator a)))" false]
   ["(let [a (atom 0)] (nil? (get-validator a)))" false]
   ["(let [a (atom 0)] (set-validator! a pos?) (reset! a -1))" true]
   ["(let [a (atom 5)] (set-validator! a pos?) (swap! a (fn [_] -1)) @a)" true]
   ["(let [a (atom 0) c (atom 0)] (add-watch a :k (fn [k r o n] (swap! c inc))) (swap! a inc) (swap! a inc) @c)" false]])

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
(each [expr throws?] cases
  (def [ocode oracle _] (run-capture "build/jolt" expr))
  (def [code got err] (run-capture jolt-bin expr))
  (cond
    throws?
      (if (and (not= ocode 0) (not= code 0)) (++ pass)
        (array/push fails [expr (string "expected both throw; oracle exit " ocode ", chez exit " code)]))
    (not= ocode 0) (array/push fails [expr (string "ORACLE FAILED exit " ocode)])
    (not= code 0) (array/push fails [expr (string "exit " code "; err: " err)])
    (= got oracle) (++ pass)
    (array/push fails [expr (string "want `" oracle "`, got `" got "`")])))

(printf "\n_atomwatch parity [%s]: %d/%d passed" jolt-bin pass (length cases))
(when (> (length fails) 0)
  (printf "%d FAIL(s):" (length fails))
  (each [e m] fails (printf "  FAIL %s\n    %s" e m)))
(flush)
(os/exit (if (empty? fails) 0 1))
