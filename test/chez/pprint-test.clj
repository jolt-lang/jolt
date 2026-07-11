;; clojure.pprint acceptance gate — a representative subset of the upstream JVM
;; (test/clojure/test_clojure/pprint/test_cl_format.clj) and cljs pprint suites,
;; adapted to run under joltc. Each case is [fmt args want]; cl-format nil returns
;; the formatted string. Cases are JVM-certified (lifted from the upstream suite)
;; unless tagged ;cljs. Runs in-process; prints per-case PASS/FAIL + a sentinel
;; line `PPRINT-RESULT pass N fail M` the gate greps.
;;
;; Scope is the mainstream directives: ~A ~S ~D ~F (~,2f) ~$ ~% ~& ~C ~( ~) ~{ ~}
;; (plain/colon/at) ~[ ~] (plain/colon/at/#) ~< ~> (justify) ~T (column) ~* (goto),
;; ~R (roman — a corner; documented as residual if it diverges). Non-portable JVM
;; cases (Java interop, platform-newlines, *math-context*) are intentionally absent;
;; see the final commit message for the residual-corner enumeration.
(ns pprint-gate
  (:require [clojure.pprint :refer [cl-format pprint]]))

(defmacro cf [fmt args]
  ;; wrap so a throw on an unimplemented directive is recorded, not fatal
  `(try (apply cl-format nil ~fmt ~args)
        (catch Throwable e#
          (str "<THREW " (.getMessage e#) ">"))))

(def ^:private cases
  ;; [label fmt args want  & [source]]
  [;; ---- ~A / ~a (aesthetic) ----
   ["a-basic" "~A" [10] "10"]
   ["a-sym" "The ~a jumped" ['elephant] "The elephant jumped"]
   ["a-nil" "~A" [nil] "nil"]
   ["a-str" "~A" ["a b"] "a b"]
   ["a-width" "~5A" ['ab] "ab   "]
   ["a+amp-d" "The quick brown ~a jumped over ~d lazy dogs" ['elephant 5]
    "The quick brown elephant jumped over 5 lazy dogs"]
   ["a+amp-newline" "The quick brown ~&~a jumped over ~d lazy dogs" ['elephant 5]
    "The quick brown \nelephant jumped over 5 lazy dogs"]

   ;; ---- ~S (standard / escapng) ----
   ["s-basic" "~S" ['foo] "foo"]
   ["s-str-escapes" "~S" ["a b"] "\"a b\""]
   ["s-nil" "~S" [nil] "nil"]

   ;; ---- ~D ----
   ["d-basic" "~D" [42] "42"]
   ["d-neg" "~D" [-7] "-7"]
   ["d-zeropad" "~5,'0D" [42] "00042"]
   ["d-comma-group" "~:D" [1234567] "1,234,567"]
   ["d-signed" "~@D" [42] "+42"]

   ;; ---- ~F (fixed float) — the user's ~,2f set ----
   ["f-pi-2f" "~,2f" [3.14159] "3.14"]
   ["f-1dp-neg" "~,1f" [-12.0] "-12.0"]
   ["f-0dp" "~,0f" [9.4] "9."]
   ["f-round-up" "~,0f" [9.5] "10."]
   ["f-2dp-neg" "~,2f" [-0.99] "-0.99"]
   ["f-3dp-pad" "~,3f" [-0.99] "-0.990"]
   ["f-int" "~f" [-1] "-1.0"]
   ["f-width" "~8f" [-1] "    -1.0"]
   ["f-wp" "~5,2f" [111.11111] "111.11"]
   ["f-precision10" "~12,10f" [1.23456789014] "1.2345678901"]
   ["f-round-pi" "~,2f" [0.999] "1.00"]

   ;; ---- ~$ (monetary) ----
   ["$-basic" "~$" [22.3] "22.30"]
   ["$-round" "~$" [22.375] "22.38"]
   ["$-neg" "~1,1$" [-12.0] "-12.0"]

   ;; ---- ~% and ~& (fresh line) ----
   ["pct" "a~%b" [] "a\nb"]
   ["ampersand-at-col0" "~&x" [] "x"]
   ["ampersand-midline" "a~&b" [] "a\nb"]

   ;; ---- ~( ~) case conversion ----
   ["conv-lower" "~(PLEASE SPEAK QUIETLY IN HERE~)" [] "please speak quietly in here"]
   ["conv-capfirst" "~@(PLEASE SPEAK QUIETLY IN HERE~)" [] "Please speak quietly in here"]
   ["conv-upper" "~@:(but this Is imporTant~)" [] "BUT THIS IS IMPORTANT"]
   ["conv-title" "~:(the greAt gatsby~)!" [] "The Great Gatsby!"]
   ["conv-title-apos" "~:(~A~)" ["DON'T!"] "Don'T!"]

   ;; ---- ~[ ~] conditional ----
   ["cond-0" "I ~[don't ~]have one~%" [0] "I don't have one\n"]
   ["cond-1" "I ~[don't ~]have one~%" [1] "I have one\n"]
   ["cond-idx-0" "I ~[don't ~;do ~]have one~%" [0] "I don't have one\n"]
   ["cond-idx-1" "I ~[don't ~;do ~]have one~%" [1] "I do have one\n"]
   ["cond-oob" "I ~[don't ~;do ~]have one~%" [2] "I have one\n"]
   ["cond-else" "I ~[don't ~:;do ~]have one~%" [2] "I do have one\n"]
   ["cond-colon-true" "I ~:[don't ~;do ~]have one~%" [true] "I do have one\n"]
   ["cond-colon-nil" "I ~:[don't ~;do ~]have one~%" [nil] "I don't have one\n"]
   ["cond-at-nil" "We had ~D wins~@[ (out of ~D tries)~].~%" [15 nil]
    "We had 15 wins.\n"]
   ["cond-at-present" "We had ~D wins~@[ (out of ~D tries)~].~%" [15 17]
    "We had 15 wins (out of 17 tries).\n"]
   ["cond-hash-empty" "The answer is ~#[nothing~;~D~;~D out of ~D~:;something crazy~]."
    [] "The answer is nothing."]
   ["cond-hash-one" "The answer is ~#[nothing~;~D~;~D out of ~D~:;something crazy~]."
    [4] "The answer is 4."]
   ["cond-hash-two" "The answer is ~#[nothing~;~D~;~D out of ~D~:;something crazy~]."
    [7 22] "The answer is 7 out of 22."]

   ;; ---- ~{ ~} iteration (plain) ----
   ["iter-plain" "Coordinates are~{ [~D,~D]~}~%" [[0 1 1 0 3 5 2 1]]
    "Coordinates are [0,1] [1,0] [3,5] [2,1]\n"]
   ["iter-limit" "Coordinates are~2{ [~D,~D]~}~%" [[0 1 1 0 3 5 2 1]]
    "Coordinates are [0,1] [1,0]\n"]
   ["iter-sep" "~{~a~^, ~}" [['a 'quick 'brown 'fox]]
    "a, quick, brown, fox"]

   ;; ---- ~{ ~} iteration (colon = list of sublists) ----
   ["iter-colon" "Coordinates are~:{ [~D,~D]~}~%" [[[0 1] [1 0] [3 5] [2 1]]]
    "Coordinates are [0,1] [1,0] [3,5] [2,1]\n"]

   ;; ---- ~{ ~} iteration (at = main list) ----
   ["iter-at" "Coordinates are~@{ [~D,~D]~}~%" [0 1 1 0 3 5 2 1]
    "Coordinates are [0,1] [1,0] [3,5] [2,1]\n"]

   ;; ---- ~< ~> justification (angle bracket) ----
   ["ang-plain" "~<foo~;bar~;baz~>" [] "foobarbaz"]
   ["ang-width" "~20<foo~;bar~;baz~>" [] "foo      bar     baz"]
   ["ang-width-args" "~20<~A~;~A~;~A~>" ['foo 'bar 'baz] "foo      bar     baz"]
   ["ang-colon-width" "~20:<~A~;~A~;~A~>" ['foo 'bar 'baz] "    foo    bar   baz"]
   ["ang-at-width" "~20@<~A~;~A~;~A~>" ['foo 'bar 'baz] "foo    bar    baz   "]

   ;; ---- ~T column tabulation ----
   ["tab-abs" "a~5Tb" [] "a    b"]
   ["tab-simple" "~{~&~A~8,4T~:*~A~}"
    [['a 'aa 'aaa 'aaaa]]
    "a       a\naa      aa\naaa     aaa\naaaa    aaaa"]

   ;; ---- ~* goto ----
   ["goto-abs" "~4@*~D ~3@*~D ~2@*~D ~1@*~D ~0@*~D" [0 1 2 3 4] "4 3 2 1 0"]
   ["goto-rel" "~*~A" ['a 'b] "b"]

   ;; ---- ~R roman (corner) ----
   ["roman-3" "~@R" [3] "III"]
   ["roman-4" "~@R" [4] "IV"]
   ["roman-3429" "~@R" [3429] "MMMCDXXIX"]

   ;; ---- ~C char ----
   ["char-at" "~@C~%" [\m] "\\m\n"]])

(defn -main [& _]
  (let [results
        (for [[label fmt args want] cases
              :let [got (cf fmt args)
                    ok (= got want)]]
          {:label label :fmt fmt :ok ok :want want :got got})
        passed (count (filter :ok results))
        failed (- (count results) passed)]
    (doseq [{:keys [label fmt ok want got]} results]
      (if ok
        (println "pprint PASS " label)
        (do (println "pprint FAIL " label "  fmt=" (pr-str fmt))
            (println "   want " (pr-str want))
            (println "   got  " (pr-str got)))))
    (println "PPRINT-RESULT pass" passed "fail" failed)
    (println (if (zero? failed) "PPRINT OK" "PPRINT FAIL"))
    (flush)))
(-main)
