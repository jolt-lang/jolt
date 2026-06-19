# jolt-75sv — list? (a list marker on cseq, since cseq backs both lists and
# realized/lazy seqs) + map-entry-as-vector + clojure.walk. Oracle = build/jolt.
#
#   janet test/chez/_walk.janet
(def jolt-bin (or (os/getenv "JOLT_BIN") "bin/jolt-chez"))

# -e reads only the FIRST form — wrap require + use in a single (do ...).
(defn w [body] (string "(do (require (quote [clojure.walk :as w])) " body ")"))

(def cases
  [# --- list? : true for lists, cons/reverse/conj-on-list; false for seqs ---
   ["(list? (list 1 2))"                    "true"]
   ["(list? (list 1))"                      "true"]
   ["(list? '(1 2))"                        "true"]
   ["(list? '())"                           "true"]
   ["(list? (list))"                        "true"]
   ["(list? (cons 1 nil))"                  "true"]
   ["(list? (cons 1 [2]))"                  "true"]
   ["(list? (cons 1 '(2)))"                 "true"]
   ["(list? (conj (list 1) 0))"             "true"]
   ["(list? (conj '() 1))"                  "true"]
   ["(list? (reverse [1 2]))"               "true"]
   ["(list? (reverse '(1 2)))"              "true"]
   ["(list? [1 2])"                         "false"]
   ["(list? {:a 1})"                        "false"]
   ["(list? (map inc [1 2]))"               "false"]
   ["(list? (filter odd? [1 2 3]))"         "false"]
   ["(list? (seq [1 2]))"                   "false"]
   ["(list? (rest (list 1 2)))"             "false"]
   ["(list? (next (list 1 2)))"             "false"]
   ["(list? (take 2 (list 1 2 3)))"         "false"]
   ["(list? (concat '(1) '(2)))"            "false"]
   ["(list? (rest [1 2]))"                  "false"]
   ["(list? 5)"                             "false"]
   ["(list? nil)"                           "false"]
   # --- map-entry IS a vector (Clojure MapEntry; seed agrees) ---
   ["(vector? (first {:a 1}))"              "true"]
   ["(vector? (first (seq {:a 1})))"        "true"]
   ["(map-entry? (first {:a 1}))"           "true"]
   ["(= (first {:a 1}) [:a 1])"             "true"]
   ["(vector? [1 2])"                       "true"]
   ["(vector? (rest [1 2 3]))"              "false"]
   # --- clojure.walk ---
   [(w "(w/postwalk (fn [x] (if (number? x) (inc x) x)) {:a 1})") "{:a 2}"]
   [(w "(w/keywordize-keys {\"a\" 1})")                            "{:a 1}"]
   [(w "(= {\"a\" 1} (w/stringify-keys {:a 1}))")                  "true"]
   [(w "(w/postwalk-replace {'x 2} '(+ x x))")                     "(+ 2 2)"]
   [(w "(w/postwalk (fn [n] (if (symbol? n) :a n)) '(x y))")       "(:a :a)"]
   [(w "(w/prewalk-replace {'* '* 'y 3} '(* y y))")                "(* 3 3)"]
   [(w "(w/postwalk-replace {:a 1 :b 2} '(:a [:b :a]))")           "(1 [2 1])"]
   [(w "(w/postwalk-replace {1 :one} [1 2 1])")                    "[:one 2 :one]"]
   ["(do (require (quote [clojure.template :as t])) (t/apply-template '[x y] '(+ x y) '(1 2)))" "(+ 1 2)"]])

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
  (def [ocode oracle _] (run-capture "build/jolt" expr))
  (def [code got err] (run-capture jolt-bin expr))
  (cond
    (not= ocode 0) (array/push fails [expr (string "ORACLE FAILED exit " ocode)])
    (not= oracle expected) (array/push fails [expr (string "ORACLE MISMATCH want `" expected "` got `" oracle "`")])
    (not= code 0) (array/push fails [expr (string "exit " code "; err: " err)])
    (= got expected) (++ pass)
    (array/push fails [expr (string "want `" expected "`, got `" got "`")])))

(printf "\n_walk parity [%s]: %d/%d passed" jolt-bin pass (length cases))
(when (> (length fails) 0)
  (printf "%d FAIL(s):" (length fails))
  (each [e m] fails (printf "  FAIL %s\n    %s" e m)))
(flush)
(os/exit (if (empty? fails) 0 1))
