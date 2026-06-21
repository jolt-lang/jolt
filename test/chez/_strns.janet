# jolt-nfca (clojure.string half) — the clojure.string namespace on Chez via the
# alias `s` established by a runtime (require '[clojure.string :as s]). The Chez
# AOT driver pre-evals require forms against the ctx so the alias resolves at
# analyze time, and clojure.string is emitted as a prelude tier over the str-*
# primitives. Each case carries its expected value.
#
#   janet test/chez/_strns.janet
(def jolt-bin (or (os/getenv "JOLT_BIN") "bin/joltc"))

(defn- with-req [body] (string "(do (require (quote [clojure.string :as s])) " body ")"))

# [label body-after-require expected]
(def cases
  [["upper-case"     "(s/upper-case \"abc\")"                 "ABC"]
   ["lower-case"     "(s/lower-case \"ABC\")"                 "abc"]
   ["capitalize"     "(s/capitalize \"hello\")"              "Hello"]
   ["trim"           "(s/trim \"  x  \")"                     "x"]
   ["triml"          "(= \"x  \" (s/triml \"  x  \"))"        "true"]
   ["trimr"          "(= \"  x\" (s/trimr \"  x  \"))"        "true"]
   ["blank? empty"   "(s/blank? \"\")"                        "true"]
   ["blank? ws"      "(s/blank? \"  \")"                      "true"]
   ["blank? no"      "(s/blank? \"x\")"                       "false"]
   ["blank? nil"     "(s/blank? nil)"                         "true"]
   ["includes? y"    "(s/includes? \"abcd\" \"bc\")"          "true"]
   ["includes? n"    "(s/includes? \"abcd\" \"zz\")"          "false"]
   ["starts-with? y" "(s/starts-with? \"abc\" \"ab\")"        "true"]
   ["starts-with? n" "(s/starts-with? \"abc\" \"bc\")"        "false"]
   ["ends-with? y"   "(s/ends-with? \"abc\" \"bc\")"          "true"]
   ["join no sep"    "(s/join [\"a\" \"b\" \"c\"])"           "abc"]
   ["join sep"       "(s/join \",\" [\"a\" \"b\" \"c\"])"     "a,b,c"]
   ["join nums"      "(s/join \"-\" [1 2 3])"                 "1-2-3"]
   ["split literal"  "(s/split \"a,b,c\" \",\")"              "[a b c]"]
   ["split regex"    "(s/split \"a1b2c\" #\"[0-9]\")"         "[a b c]"]
   ["split-lines"    "(s/split-lines \"a\\nb\\nc\")"          "[a b c]"]
   ["replace lit"    "(s/replace \"a_b_c\" \"_\" \"-\")"      "a-b-c"]
   ["replace regex"  "(s/replace \"a1b2\" #\"[0-9]\" \"\")"   "ab"]
   ["replace-first"  "(s/replace-first \"a_b_c\" \"_\" \"-\")" "a-b_c"]
   ["reverse"        "(s/reverse \"abc\")"                    "cba"]
   ["index-of hit"   "(s/index-of \"abc\" \"b\")"             "1"]
   ["index-of miss"  "(nil? (s/index-of \"abc\" \"z\"))"      "true"]
   ["trim-newline"   "(s/trim-newline \"abc\\n\\n\")"         "abc"]])

(defn run-capture [expr]
  (def proc (os/spawn [jolt-bin "-e" expr] :p {:out :pipe :err :pipe}))
  (def out (ev/read (proc :out) 0x100000))
  (def err (ev/read (proc :err) 0x100000))
  (def code (os/proc-wait proc))
  (def lines (filter (fn [l] (not (empty? l)))
                     (string/split "\n" (string/trim (if out (string out) "")))))
  [code (if (empty? lines) "" (last lines)) (if err (string err) "")])

(var pass 0)
(def fails @[])
(each [label body expected] cases
  (def [code got err] (run-capture (with-req body)))
  (cond
    (not= code 0) (array/push fails [label (string "exit " code "; err: " (string/trim err))])
    (= got expected) (++ pass)
    (array/push fails [label (string "want `" expected "`, got `" got "`")])))

(printf "\n_strns parity [%s]: %d/%d passed" jolt-bin pass (length cases))
(when (> (length fails) 0)
  (printf "%d FAIL(s):" (length fails))
  (each [l m] fails (printf "  FAIL [%s] %s" l m)))
(flush)
(os/exit (if (empty? fails) 0 1))