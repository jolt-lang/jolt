;; jolt.parser gate — ports rm-hull/jasentaa's own suite for the pieces jolt
;; adopted (collections/position/basic/combinators) and covers the combinators
;; jolt adds on top (eof, between, sep-by, optional-default, digit/letter/
;; alpha-num). Parse errors are a jolt ex-info (not java.text.ParseException).
;; Self-checks and prints PARSER OK. Run: bin/joltc run test/chez/parser-test.clj
(ns parser-test
  (:require
   [jolt.parser.monad :as m]
   [jolt.parser.collections :refer [join]]
   [jolt.parser.position :as pos]
   [jolt.parser.basic :as pb]
   [jolt.parser.combinators :as pc]))

(def failures (atom []))
(defn fail! [msg] (swap! failures conj msg))
(defn chk= [label got want]
  (when-not (= got want)
    (fail! (str label ": want " (pr-str want) " got " (pr-str got)))))
(defn threw [thunk] (try (thunk) ::none (catch Throwable e e)))
(defn chk-throws [label pred thunk]
  (let [e (threw thunk)]
    (cond
      (= e ::none) (fail! (str label ": expected throw, got none"))
      (not (pred e)) (fail! (str label ": wrong throw " (pr-str (type e))
                                 " / " (pr-str (.getMessage e)))))))
(defn ex? [e] (instance? clojure.lang.ExceptionInfo e))

;; jasentaa's test harness: run a parser over augmented input and format the
;; first result as [value remaining], with value stripped back to char(s).
(defn harness [parser input]
  (let [result (first (parser (pos/augment-location input)))]
    (if (empty? result)
      (m/failure)
      (list [(if (char? (-> result first :char))
               (-> result first :char)
               (mapv :char (first result)))
             (pos/strip-location (fnext result))]))))
(def FAIL (m/failure))

;; ------------------------------------------------------------- collections/join
(chk= "join-1-2"   (join 1 2) [1 2])
(chk= "join-v3-4"  (join [3] 4) [3 4])
(chk= "join-5-v6"  (join 5 [6]) [5 6])
(chk= "join-v7-v8" (join [7] [8]) [7 8])
(chk= "join-9-nil" (join 9 nil) [9])
(chk= "join-nil-0" (join nil 0) [0])
(chk= "join-nil-nil" (join nil nil) [])
(chk= "join-str"   (join "a" "b") "ab")
(chk= "join-str-a" (join "a" nil) "a")
(chk= "join-str-b" (join nil "b") "b")
(let [[a b] (pos/augment-location "ab")]
  (chk= "join-rec-a"  (join a nil) [a])
  (chk= "join-rec-b"  (join nil b) [b])
  (chk= "join-rec-ab" (join a b) [a b]))

;; ---------------------------------------------------------------------- position
(chk= "augment-then-strip"
      (pos/strip-location (pos/augment-location "the quick brown fox"))
      "the quick brown fox")
(chk= "augment-empty" (pos/augment-location "") nil)
(chk= "augment-plain"
      (pos/augment-location "Hello\nWorld!")
      (list
       (pos/->Location \H 1 1 0 "Hello\nWorld!")
       (pos/->Location \e 1 2 1 "Hello\nWorld!")
       (pos/->Location \l 1 3 2 "Hello\nWorld!")
       (pos/->Location \l 1 4 3 "Hello\nWorld!")
       (pos/->Location \o 1 5 4 "Hello\nWorld!")
       (pos/->Location \newline 1 6 5 "Hello\nWorld!")
       (pos/->Location \W 2 1 6 "Hello\nWorld!")
       (pos/->Location \o 2 2 7 "Hello\nWorld!")
       (pos/->Location \r 2 3 8 "Hello\nWorld!")
       (pos/->Location \l 2 4 9 "Hello\nWorld!")
       (pos/->Location \d 2 5 10 "Hello\nWorld!")
       (pos/->Location \! 2 6 11 "Hello\nWorld!")))
(chk= "strip-loc-char" (pos/strip-location (pos/->Location \h 1 1 0 "help")) \h)
(chk= "strip-loc-nil"  (pos/strip-location nil) nil)
(chk= "strip-loc-str"  (pos/strip-location "Hello") "Hello")
(chk-throws "exc-nil"
            (fn [e] (and (ex? e) (re-find #"Unable to parse text" (str (.getMessage e)))))
            #(throw (pos/parse-exception nil)))
(chk-throws "exc-loc"
            (fn [e] (and (ex? e) (re-find #"Failed to parse text at line: 6, col: 31" (str (.getMessage e)))))
            #(throw (pos/parse-exception (pos/->Location \Y 6 31 321 "Makes no sense"))))
(let [text "We choked on street tap water well I'm gonna have to try the real thing\n
I took your laugh by the collar and it knew not to swing\n
Anytime I tried an honest job well the till had a hole and ha-ha\n
We laughed about payin' rent 'cause the county jails they're free"
      loc (vec (pos/augment-location text))]
  (chk= "show-err-10"  (pos/show-error (get loc 10))
        "We choked on street tap water well I'm gonna have to try the real thing\n          ^\n")
  (chk= "show-err-110" (pos/show-error (get loc 110))
        "I took your laugh by the collar and it knew not to swing\n                                     ^\n")
  (chk= "show-err-210" (pos/show-error (get loc 210))
        "We laughed about payin' rent 'cause the county jails they're free\n             ^\n")
  (chk= "show-err-nil" (pos/show-error nil) nil)
  (chk= "show-err-oob" (pos/show-error (pos/->Location \h 1 1 1000 "wut?")) nil))

;; ------------------------------------------------------------------------- basic
(chk= "any-apple" (harness pb/any "apple") (list [\a "pple"]))
(chk= "any-a"     (harness pb/any "a")     (list [\a ""]))
(chk= "any-empty-vec" (harness pb/any [])  FAIL)
(chk= "any-nil"   (harness pb/any nil)     FAIL)
(chk= "any-empty" (harness pb/any "")      FAIL)
(chk= "match-apple" (harness (pb/match "a") "apple") (list [\a "pple"]))
(chk= "match-a"     (harness (pb/match "a") "a")     (list [\a ""]))
(chk= "match-no"    (harness (pb/match "a") "banana") FAIL)
(chk= "none-banana" (harness (pb/none-of "a") "banana") (list [\b "anana"]))
(chk= "none-b"      (harness (pb/none-of "a") "b")      (list [\b ""]))
(chk= "none-no"     (harness (pb/none-of "b") "banana") FAIL)
(chk= "re-apple"  (harness (pb/from-re #"[a-z]") "apple")  (list [\a "pple"]))
(chk= "re-banana" (harness (pb/from-re #"[a-z]") "banana") (list [\b "anana"]))
(chk= "re-pear"   (harness (pb/from-re #"[a-z]") "pear")   (list [\p "ear"]))
(chk= "re-no"     (harness (pb/from-re #"[a-z]") "Tomtato") FAIL)

;; ------------------------------------------------------------------ combinators
(let [parser (pc/and-then (pb/match "a") (pb/match "b"))]
  (chk= "and-then-abel" (harness parser "abel") (list [[\a \b] "el"]))
  (chk= "and-then-no"   (harness parser "apple") FAIL)
  (chk= "and-then-empty" (harness parser "") FAIL))
(let [parser (pc/or-else (pb/match "a") (pb/match "b"))]
  (chk= "or-else-apple"  (harness parser "apple")  (list [\a "pple"]))
  (chk= "or-else-banana" (harness parser "banana") (list [\b "anana"]))
  (chk= "or-else-no"     (harness parser "orange") FAIL))
(let [parser (pc/many (pb/match "a"))]
  (chk= "many-a"   (first (harness parser "a"))      [[\a] ""])
  (chk= "many-aaa" (first (harness parser "aaabbb")) [[\a \a \a] "bbb"])
  (chk= "many-empty" (first (harness parser ""))     [[] nil])
  (chk= "many-apple" (first (harness parser "apple")) [[\a] "pple"])
  (chk= "many-orange" (first (harness parser "orange")) [[] "orange"]))

;; ------------------------------------------------------- jolt-added combinators
;; eof consumes nothing at end of input (tested directly: the harness formatter
;; isn't meaningful for eof's nil result).
(chk= "eof-empty" (pb/eof "") (list [nil ""]))
(chk= "eof-no"    (pb/eof "x") FAIL)
(chk= "digit"  (harness pc/digit "7x") (list [\7 "x"]))
(chk= "digit-no" (harness pc/digit "x") FAIL)
(chk= "letter" (harness pc/letter "Ab") (list [\A "b"]))
(chk= "letter-no" (harness pc/letter "1") FAIL)
(chk= "alpha-num-l" (harness pc/alpha-num "a1") (list [\a "1"]))
(chk= "alpha-num-d" (harness pc/alpha-num "9z") (list [\9 "z"]))
(chk= "alpha-num-no" (harness pc/alpha-num "-") FAIL)
;; between / sep-by / optional through the real parse driver
(require '[jolt.parser :refer [parse-all]])
(chk= "between" (mapv :char (parse-all (pc/between (pb/match "(") (pb/match ")") (pc/plus pc/digit)) "(42)"))
      [\4 \2])
(chk= "sep-by-3" (mapv (fn [g] (mapv :char g))
                       (parse-all (pc/sep-by (pc/plus pc/digit) (pb/match ",")) "1,22,333"))
      [[\1] [\2 \2] [\3 \3 \3]])
(chk= "sep-by-1" (mapv (fn [g] (mapv :char g))
                       (parse-all (pc/sep-by (pc/plus pc/digit) (pb/match ",")) "5"))
      [[\5]])
(chk= "optional-default" (parse-all (pc/optional (pb/match "x") :none) "") :none)
(chk= "optional-match"   (:char (parse-all (pc/optional (pb/match "x") :none) "x")) \x)

;; monad do* / >>= sanity
(chk= "do*-return" ((m/do* (m/return 42)) "in") (list [42 "in"]))

;; ---------------------------------------------------------------------- report
(if (empty? @failures)
  (println "PARSER OK")
  (do
    (println "PARSER FAIL" (count @failures) "failures:")
    (doseq [f @failures] (println "  FAIL:" f))))
