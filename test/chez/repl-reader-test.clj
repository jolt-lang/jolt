;; Self-checking regression for the REPL's read-until-complete predicate
;; (jolt.main/repl-form-complete?), which decides whether a line buffer is a whole
;; form or the REPL should keep reading continuation lines. Runs via `bin/joltc run`
;; (jolt.main is loaded, so the private var resolves); prints a sentinel the smoke
;; gate greps. The regex cases are the ones that regressed: a #"..." literal opens a
;; regex whose body — parens, quotes and all — must not be miscounted as delimiters.
(require 'jolt.main)

(def complete? jolt.main/repl-form-complete?)

;; [input expected-complete?]
(def cases
  [["(+ 1 2)"                     true]    ; balanced
   ["(+ 1"                        false]   ; open paren -> keep reading
   ["(defn g [] (foo (bar"        false]   ; deeply unbalanced
   ["[1 2 {:a 3}]"                true]    ; mixed bracket types balance
   ["(str \")\")"                 true]    ; a close-paren inside a string doesn't count
   ["\\("                         true]    ; a paren char literal doesn't count
   ["(+ 1 2) ; ) ) trailing"      true]    ; parens in a line comment don't count
   ["(re-find #\"(a)(b)\" \"ab\")" true]   ; groups inside a regex must not count as depth
   ["#\"[0-9]+\""                 true]    ; a bare regex literal is a complete form
   ["#\"a(b"                      false]]) ; an unterminated regex is incomplete

(let [bad (remove (fn [[in exp]] (= exp (boolean (complete? in)))) cases)]
  (println (if (empty? bad)
             "REPL-READER OK"
             (str "REPL-READER FAIL " (pr-str (map first bad))))))
