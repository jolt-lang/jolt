# Specification: regular expressions — #"…" literals and the re-* fns.
# (Whole area previously unspecced; some cases adapted from jank reader-macro/regex.)
(use ../support/harness)

(defspec "regex / literals & predicate"
  ["regex? literal"   "true"   "(regex? #\"\\d+\")"]
  ["regex? non-regex" "false"  "(regex? \"\\d+\")"]
  ["escaped digits"   "\"42\"" "(re-find #\"\\d+\" \"x42y\")"]
  ["escaped ws/non-ws" "\"x a\"" "(re-find #\"\\S\\s\\S\" \"x a b y\")"])

(defspec "regex / re-find"
  ["match"            "\"123\"" "(re-find #\"\\d+\" \"abc123def\")"]
  ["no match nil"     "nil"    "(re-find #\"\\d+\" \"abc\")"]
  ["with groups"      "[\"a1\" \"a\" \"1\"]" "(re-find #\"([a-z])(\\d)\" \"--a1--\")"]
  ["first match only" "\"1\""  "(re-find #\"\\d\" \"1 2 3\")"])

(defspec "regex / re-matches"
  ["full match"       "\"123\"" "(re-matches #\"\\d+\" \"123\")"]
  ["partial = nil"    "nil"    "(re-matches #\"\\d+\" \"123abc\")"]
  ["groups"           "[\"12\" \"1\" \"2\"]" "(re-matches #\"(\\d)(\\d)\" \"12\")"]
  ["no match nil"     "nil"    "(re-matches #\"x+\" \"yyy\")"])

(defspec "regex / re-seq"
  ["all matches"      "(quote (\"1\" \"22\" \"333\"))" "(re-seq #\"\\d+\" \"a1b22c333\")"]
  ["empty when none"  "nil"    "(seq (re-seq #\"z\" \"abc\"))"]
  ["words"            "(quote (\"foo\" \"bar\"))" "(re-seq #\"\\w+\" \"foo bar\")"])

(defspec "regex / re-pattern & string ops"
  ["re-pattern build" "\"hi\"" "(re-find (re-pattern \"\\\\w+\") \"hi!\")"]
  ["re-pattern is regex?" "true" "(regex? (re-pattern \"a\"))"]
  ["split on regex"   "[\"a\" \"b\" \"c\"]" "(do (require '[clojure.string :as s]) (s/split \"a1b2c\" #\"\\d\"))"]
  ["replace regex"    "\"X-X\"" "(do (require '[clojure.string :as s]) (s/replace \"a-b\" #\"[a-z]\" \"X\"))"]
  ["replace $1"       "\"[a][b]\"" "(do (require '[clojure.string :as s]) (s/replace \"ab\" #\"([a-z])\" \"[$1]\"))"])

# Unicode property classes (jolt-xlp), byte-PEG approximation: ASCII exact,
# any high byte (inside a UTF-8 sequence) counts as a LETTER for \p{L}.
# Acceptance target was cuerdas (kebab/snake/capital now pass conformance).
(defspec "regex / \\p property classes"
  ["p{L} ascii"      "\"hello\"" `(re-matches #"^\p{L}+$" "hello")`]
  ["p{L} utf-8"      "true"      `(boolean (re-matches #"^\p{L}+$" "héllo"))`]
  ["p{L} rejects digits" "false" `(boolean (re-matches #"^\p{L}+$" "ab1"))`]
  ["p{N}"            "(quote (\"12\" \"345\"))" `(re-seq #"\p{N}+" "a12b345")`]
  ["P{N} negation"   "\"abc\""   `(re-matches #"^\P{N}+$" "abc")`]
  ["inside class"    "\"a-1_b\"" `(re-matches #"^[\p{N}\p{L}_-]+$" "a-1_b")`]
  ["p{Lu}/p{Ll}"     "\"aB\""    `(re-matches #"^\p{Ll}\p{Lu}$" "aB")`]
  ["p{Z} space"      "\"  \""    `(re-matches #"(?u)^[\s\p{Z}]+$" "  ")`]
  ["p{Ps}/p{Pe}"     "\"(x)\""   `(re-matches #"^\p{Ps}x\p{Pe}$" "(x)")`]
  ["(?u) accepted"   "\"hi\""    `(re-matches #"(?u)^hi$" "hi")`]
  ["unknown class throws" :throws `(re-pattern "\p{Greek}")`])

# java.util.regex.Pattern statics (jolt-47b): compile returns jolt's native
# regex value (so .split / str-ops accept it), MULTILINE maps to (?m), quote
# escapes metachars. Plus the regex String methods migratus uses.
(defspec "regex / Pattern statics & String regex methods"
  ["Pattern/compile is a regex" "true" "(regex? (Pattern/compile \"a.c\"))"]
  ["compiled .split"     "[\"a\" \"b\" \"c\"]" "(.split (Pattern/compile \",\") \"a,b,c\")"]
  ["str/replace w/ Pattern" "\"ab\"" "(do (require '[clojure.string :as s]) (s/replace \"a1b2\" (Pattern/compile \"[0-9]\") \"\"))"]
  ["Pattern/MULTILINE ^"  "true"  "(boolean (re-find (Pattern/compile \"^x\" Pattern/MULTILINE) \"y\\nx\"))"]
  ["Pattern/quote literal" "true" "(boolean (re-find (re-pattern (Pattern/quote \"a.c\")) \"za.cy\"))"]
  ["Pattern/quote not meta" "false" "(boolean (re-find (re-pattern (Pattern/quote \"a.c\")) \"zabcy\"))"]
  [".matches whole string" "true"  "(.matches \"abc\" \"a.c\")"]
  [".matches partial -> false" "false" "(.matches \"abcd\" \"a.c\")"]
  [".replaceAll regex"   "\"a-b-c\"" "(.replaceAll \"a_b_c\" \"_\" \"-\")"]
  [".replaceFirst regex" "\"a-b_c\"" "(.replaceFirst \"a_b_c\" \"_\" \"-\")"])

(defspec "regex / bounded quantifiers (jolt-3xur)"
  ["exact {n}"        "\"aaa\""  "(re-matches #\"a{3}\" \"aaa\")"]
  ["range {n,m} max"  "\"aaaa\"" "(re-find #\"a{2,4}\" \"aaaaa\")"]
  ["zero lower {0,n}" "\"aa\""   "(re-find #\"a{0,3}\" \"aa\")"]
  ["{n,} unbounded"   "\"aaaa\"" "(re-find #\"a{2,}\" \"aaaa\")"]
  # nested bounded quantifiers must not blow up the PEG compiler — this used to
  # expand to ~2^61 nodes and hang at compile time.
  ["nested bounds compile" "true"
   "(boolean (re-matches #\"[a-z](?:[a-z]{0,61}[a-z])?(?:-[a-z]{0,61}[a-z])*\" \"abc-def\"))"])
