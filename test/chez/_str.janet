# jolt-nfca — host java.lang.String method interop on Chez: (.toUpperCase s),
# (.indexOf s x), (.substring s a b), the regex methods (.matches/.replaceAll/
# .replaceFirst), etc. The string-methods surface. Each case carries its
# expected value.
# An expected of :throws asserts a non-zero exit (unsupported method).
#
#   janet test/chez/_str.janet
(def jolt-bin (or (os/getenv "JOLT_BIN") "bin/joltc"))

(def cases
  [["toLowerCase"      "(.toLowerCase \"HI\")"                  "hi"]
   ["toUpperCase"      "(.toUpperCase \"hi\")"                  "HI"]
   ["trim"             "(.trim \"  x  \")"                      "x"]
   ["length"           "(.length \"abc\")"                      "3"]
   ["isEmpty"          "[(.isEmpty \"\") (.isEmpty \"a\")]"     "[true false]"]
   ["indexOf hit"      "(.indexOf \"abc\" \"b\")"               "1"]
   ["indexOf miss"     "(.indexOf \"abc\" \"z\")"               "-1"]
   ["indexOf from"     "(.indexOf \"abab\" \"a\" 1)"            "2"]
   ["indexOf int code" "(.indexOf \"a=b\" 61)"                  "1"]
   ["lastIndexOf"      "(.lastIndexOf \"abab\" \"b\")"          "3"]
   ["substring 1"      "(.substring \"abc\" 1)"                 "bc"]
   ["substring 1 2"    "(.substring \"abc\" 1 2)"               "b"]
   ["startsWith"       "(.startsWith \"abc\" \"ab\")"           "true"]
   ["endsWith"         "(.endsWith \"abc\" \"bc\")"             "true"]
   ["contains"         "(.contains \"abc\" \"b\")"              "true"]
   ["replace literal"  "(.replace \"abc\" \"b\" \"x\")"         "axc"]
   ["replace all occ"  "(.replace \"aaa\" \"a\" \"b\")"         "bbb"]
   ["charAt"           "(.charAt \"abc\" 1)"                    "\\b"]
   ["equalsIgnoreCase" "(.equalsIgnoreCase \"AbC\" \"aBc\")"    "true"]
   ["toString"         "(.toString \"hi\")"                     "hi"]
   ["concat"           "(.concat \"ab\" \"cd\")"                "abcd"]
   ["matches whole"    "(.matches \"abc\" \"a.c\")"             "true"]
   ["matches partial"  "(.matches \"abcd\" \"a.c\")"            "false"]
   ["replaceAll"       "(.replaceAll \"a_b_c\" \"_\" \"-\")"    "a-b-c"]
   ["replaceFirst"     "(.replaceFirst \"a_b_c\" \"_\" \"-\")"  "a-b_c"]
   ["split regex"      "(vec (.split \"a,b,c\" \",\"))"         "[a b c]"]
   ["unsupported"      "(.frobnicate \"abc\")"                  :throws]])

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
(each [label expr expected] cases
  (def [code got err] (run-capture expr))
  (cond
    (= expected :throws)
      (if (not= code 0) (++ pass)
        (array/push fails [label (string "want throw, got `" got "` (exit 0)")]))
    (not= code 0) (array/push fails [label (string "exit " code "; err: " (string/trim err))])
    (= got expected) (++ pass)
    (array/push fails [label (string "want `" expected "`, got `" got "`")])))

(printf "\n_str parity [%s]: %d/%d passed" jolt-bin pass (length cases))
(when (> (length fails) 0)
  (printf "%d FAIL(s):" (length fails))
  (each [l m] fails (printf "  FAIL [%s] %s" l m)))
(flush)
(os/exit (if (empty? fails) 0 1))