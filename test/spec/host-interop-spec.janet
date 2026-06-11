# Specification: host (Janet) interop — the `.` forms and jolt.interop.
(use ../support/harness)

(defspec "interop / dot forms"
  ["method call"        "\"v=41\""
   "(. {:value 41 :describe (fn [self] (str \"v=\" (:value self)))} describe)"]
  ["method with args"   "\"Hello Alice\""
   "(. {:greet (fn [self n] (str \"Hello \" n))} greet \"Alice\")"]
  ["field access .-"    "41"        "(.-value {:value 41})"]
  ["dot field keyword"  "41"        "(. {:value 41} :value)"])

# The `janet` namespace segment is the explicit Janet-stdlib bridge added for
# the networking layer (and used by jolt.nrepl). `janet/<name>` resolves a Janet
# root binding; `janet.<module>/<name>` resolves a module binding. The boundary
# is explicit so it's visible where host semantics take over.
(defspec "interop / janet bridge"
  ["root builtin janet/<name>"   "\"123\"" "(janet/string 1 2 3)"]
  ["root builtin janet/type"     ":string" "(janet/type \"x\")"]
  ["module fn janet.<mod>/<name>" "4"      "(janet.math/sqrt 16)"]
  ["janet.string module fn"      "\"HI\""  "(janet.string/ascii-upper \"hi\")"]
  ["janet.os/clock is a number"  "true"    "(number? (janet.os/clock))"]
  # crossing the boundary uses Janet representations: a Jolt vector is a table
  ["jolt vector crosses as a janet table" ":table" "(janet/type [1 2])"]
  # interop is explicit-only: an unprefixed Janet module is not auto-exposed
  ["unprefixed janet module not exposed" :throws "net/server"]
  ["unknown janet symbol throws"         :throws "(janet.os/definitely-not-a-real-fn)"])

(defspec "interop / jolt.interop"
  ["janet-type quoted list" ":array" "(do (require (quote [jolt.interop :as j])) (j/janet-type (quote (1 2))))"]
  ["janet-type list"    ":array"    "(do (require (quote [jolt.interop :as j])) (j/janet-type (list 1 2)))"]
  ["janet-type string"  ":string"   "(do (require (quote [jolt.interop :as j])) (j/janet-type \"x\"))"]
  ["janet-type number"  ":number"   "(do (require (quote [jolt.interop :as j])) (j/janet-type 1))"]
  ["janet-type keyword" ":keyword"  "(do (require (quote [jolt.interop :as j])) (j/janet-type :a))"])

(defspec "interop / arrays (aget/aset/alength)"
  ["alength"            "3"      "(alength (object-array [1 2 3]))"]
  ["aget"               "20"     "(aget (object-array [10 20 30]) 1)"]
  ["aset returns val"   "9"      "(aset (object-array [1 2 3]) 1 9)"]
  ["aset mutates"       "[7 2 3]" "(let [a (object-array [1 2 3])] (aset a 0 7) (vec a))"]
  ["aget 2d"            "4"      "(aget (to-array-2d [[1 2] [3 4]]) 1 1)"])

# java.lang.String surface + .method sugar (clj-compat: what portable cljc
# libraries call — landed for the cuerdas acceptance run). ASCII case mapping.
(defspec "interop / String methods"
  [".toLowerCase"   "\"hi\""  "(.toLowerCase \"HI\")"]
  [".toUpperCase"   "\"HI\""  "(.toUpperCase \"hi\")"]
  ["dot-form"       "\"hi\""  "(. \"HI\" toLowerCase)"]
  [".trim"          "\"x\""   "(.trim \"  x  \")"]
  [".length"        "3"       "(.length \"abc\")"]
  [".isEmpty"       "[true false]" "[(.isEmpty \"\") (.isEmpty \"a\")]"]
  [".indexOf hit"   "1"       "(.indexOf \"abc\" \"b\")"]
  [".indexOf miss is -1" "-1" "(.indexOf \"abc\" \"z\")"]
  [".lastIndexOf"   "3"       "(.lastIndexOf \"abab\" \"b\")"]
  [".substring"     "\"bc\""  "(.substring \"abc\" 1)"]
  [".substring end" "\"b\""   "(.substring \"abc\" 1 2)"]
  [".startsWith"    "true"    "(.startsWith \"abc\" \"ab\")"]
  [".endsWith"      "true"    "(.endsWith \"abc\" \"bc\")"]
  [".contains"      "true"    "(.contains \"abc\" \"b\")"]
  [".replace"       "\"axc\"" "(.replace \"abc\" \"b\" \"x\")"]
  [".charAt"        "\\b"     "(.charAt \"abc\" 1)"]
  [".equalsIgnoreCase" "true" "(.equalsIgnoreCase \"AbC\" \"aBc\")"]
  ["Long/MAX_VALUE" "true"    "(pos? Long/MAX_VALUE)"]
  ["unsupported method throws" :throws "(.frobnicate \"abc\")"])

# java.time shims (jolt-ea7): epoch-ms backed values + a DateTimeFormatter
# pattern subset — the surface Selmer's date filters drive. Formatting uses
# the HOST's local timezone, so rows assert structure, not wall-clock values.
(defspec "interop / java.time shims"
  ["ofPattern formats #inst"    "true"
   "(string? (.format (DateTimeFormatter/ofPattern \"yyyy-MM-dd\") #inst \"2020-03-05T13:45:30Z\"))"]
  ["pattern shape"              "true"
   "(boolean (re-matches #\"\\d{4}-\\d{2}-\\d{2}\" (.format (DateTimeFormatter/ofPattern \"yyyy-MM-dd\") #inst \"2020-03-05T13:45:30Z\")))"]
  ["month name + ampm"          "true"
   "(boolean (re-matches #\"[A-Z][a-z]{2} \\d{1,2}, 2020 \\d{1,2}:\\d{2} [AP]M\" (.format (DateTimeFormatter/ofPattern \"MMM d, yyyy h:mm a\") #inst \"2020-03-05T13:45:30Z\")))"]
  ["quoted literal"             "true"
   "(boolean (re-matches #\"\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}\" (.format DateTimeFormatter/ISO_LOCAL_DATE_TIME #inst \"2020-03-05T13:45:30Z\")))"]
  ["localized style"            "true"
   "(string? (.format (DateTimeFormatter/ofLocalizedDate FormatStyle/MEDIUM) #inst \"2020-03-05T13:45:30Z\"))"]
  ["withLocale chain"           "true"
   "(string? (.format (.withLocale (DateTimeFormatter/ofPattern \"yyyy\") (java.util.Locale. \"en\")) #inst \"2020-01-01T00:00:00Z\"))"]
  ["fix-date chain"             "true"
   "(instance? LocalDateTime (-> #inst \"2020-03-05T13:45:30Z\" (.toInstant) (.atZone (ZoneId/systemDefault)) (.toLocalDateTime)))"]
  ["inst is java.util.Date"     "true"  "(instance? java.util.Date #inst \"2020-01-01T00:00:00Z\")"]
  ["Instant instance"           "true"  "(instance? java.time.Instant (Instant/ofEpochMilli 0))"]
  ["getTime epoch ms"           "0"     "(.getTime #inst \"1970-01-01T00:00:00Z\")"]
  ["toEpochMilli round trip"    "1234"  "(.toEpochMilli (Instant/ofEpochMilli 1234))"]
  ["Instant/now is current"     "true"  "(> (.toEpochMilli (Instant/now)) 1500000000000)"]
  ["sql types are not"          "false" "(instance? java.sql.Timestamp #inst \"2020-01-01T00:00:00Z\")"])

# java.io / java.lang shims that carry Selmer's char-by-char template reader.
(defspec "interop / StringReader & StringBuilder"
  ["StringReader read"     "[97 98 -1]"
   "(let [r (java.io.StringReader. \"ab\")] [(.read r) (.read r) (.read r)])"]
  ["mark/reset"            "[97 97]"
   "(let [r (StringReader. \"ab\")] (.mark r 1) [(.read r) (do (.reset r) (.read r))])"]
  ["StringBuilder append"  "\"ab1\""
   "(.toString (-> (StringBuilder.) (.append \"a\") (.append \\b) (.append 1)))"]
  ["capacity arg is not content" "\"x\""
   "(.toString (.append (StringBuilder. 16) \"x\"))"]
  ["setLength truncates"   "\"ab\""
   "(let [sb (StringBuilder.)] (.append sb \"abcd\") (.setLength sb 2) (.toString sb))"]
  ["char-array of string"  "true"
   "(instance? (Class/forName \"[C\") (char-array \"ab\"))"]
  ["reader over char[]"    "97"
   "(do (require (quote clojure.java.io)) (.read (clojure.java.io/reader (char-array \"abc\"))))"]
  ["line-seq over file reader" "[\"a\" \"b\"]"
   "(do (require (quote clojure.java.io)) (janet/spit \"/tmp/jolt-lineseq-spec.txt\" \"a\\nb\\n\") (vec (line-seq (clojure.java.io/reader \"/tmp/jolt-lineseq-spec.txt\"))))"]
  ["with-open closes shim" "97"
   "(with-open [r (StringReader. \"a\")] (.read r))"]
  ["vector :import shares deftype ctor" "\"hi!\""
   "(do (ns spec.nodea) (defprotocol SpecP (spec-pm [this])) (deftype SpecTN [t] SpecP (spec-pm [this] (str t \"!\"))) (ns spec.nodeb (:import [spec.nodea SpecTN])) (.spec-pm (SpecTN. \"hi\")))"])
