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

# Shims for yogthos/config: PushbackReader, numeric/boolean parse statics,
# System/getenv + getProperties as iterable maps, edn/read from a reader.
(defspec "interop / PushbackReader & parse statics"
  ["PushbackReader read"   "[97 98]"
   "(let [r (java.io.PushbackReader. (java.io.StringReader. \"ab\"))] [(.read r) (.read r)])"]
  ["unread pushes back"    "[97 97 98]"
   "(let [r (PushbackReader. (StringReader. \"ab\")) a (.read r)] (.unread r a) [a (.read r) (.read r)])"]
  ["unread accepts a char" "[120 97]"
   "(let [r (PushbackReader. (StringReader. \"a\"))] (.unread r \\x) [(.read r) (.read r)])"]
  ["edn/read from reader"  "5432"
   "(do (require (quote clojure.edn)) (clojure.edn/read (java.io.PushbackReader. (java.io.StringReader. \"{:db {:port 5432}}\\nrest\"))) (get-in (clojure.edn/read-string \"{:db {:port 5432}}\") [:db :port]))"]
  ["edn/read multi-line"   "true"
   "(do (require (quote clojure.edn)) (= {:a 1 :b 2} (clojure.edn/read (PushbackReader. (StringReader. \"{:a 1\\n :b 2}\")))))"]
  ["Long/parseLong"        "42"     "(Long/parseLong \"42\")"]
  ["parseLong rejects non-numeric" :throws "(Long/parseLong \"4x\")"]
  ["BigInteger."           "123"    "(BigInteger. \"123\")"]
  ["Boolean/parseBoolean"  "[true false false]"
   "[(Boolean/parseBoolean \"true\") (Boolean/parseBoolean \"false\") (Boolean/parseBoolean \"yes\")]"]
  ["System/getenv is a map" "true"
   "(string? (get (System/getenv) \"HOME\"))"]
  # NOT every? alone — it held vacuously while seq over a raw host table
  # yielded nothing, hiding that read-system-env came back empty
  ["getenv entries destructure (non-empty)" "true"
   "(let [es (map (fn [[k v]] [k v]) (System/getenv))] (and (pos? (count es)) (every? vector? es)))"]
  ["seq over a raw host table" "true"
   "(pos? (count (seq (System/getenv))))"]
  ["into {} from host table" "true"
   "(string? (get (into {} (map (fn [[k v]] [k v]) (System/getenv))) \"HOME\"))"]
  ["System/getProperties"  "true"
   "(string? (get (System/getProperties) \"os.name\"))"])

# ring-core enablement (host shims + protocol/reduce fixes): the java.net /
# java.util surface ring.util.codec needs, extend-protocol on Map and nil,
# and reduce over a reified clojure.lang.IReduceInit.
(defspec "host-interop / ring-codec surface"
  ["URLEncoder www form"   "\"a+b%3Dc\"" "(URLEncoder/encode \"a b=c\")"]
  ["URLDecoder www form"   "\"a b=c\""   "(URLDecoder/decode \"a+b%3Dc\" (Charset/forName \"UTF-8\"))"]
  ["url round trip"        "\"x &=%?\""  "(URLDecoder/decode (URLEncoder/encode \"x &=%?\"))"]
  ["Base64 encode"         "\"aGVsbG8=\"" "(String. (.encode (Base64/getEncoder) (.getBytes \"hello\")))"]
  ["Base64 round trip"     "\"hello\""   "(String. (.decode (Base64/getDecoder) (String. (.encode (Base64/getEncoder) (.getBytes \"hello\")))))"]
  ["Integer radix + byteValue" "-1"      "(.byteValue (Integer/valueOf \"ff\" 16))"]
  ["Integer parseInt"      "255"         "(Integer/parseInt \"ff\" 16)"]
  ["StringTokenizer"       "[\"a=1\" \"b=2\"]" "(let [t (StringTokenizer. \"a=1&b=2\" \"&\")] [(.nextToken t) (.nextToken t)])"]
  ["MapEntry key/val"      "[:a 1]"      "(let [e (MapEntry. :a 1)] [(key e) (val e)])"]
  ["String ctor from bytes" "\"hi\""     "(String. (.getBytes \"hi\"))"]
  ["extend-protocol Map"   ":map"
   "(do (defprotocol Pe (pe [x])) (extend-protocol Pe Map (pe [m] :map) Object (pe [o] :obj)) (pe {:a 1}))"]
  ["extend-protocol nil"   ":nil"
   "(do (defprotocol Pn (pn [x])) (extend-protocol Pn nil (pn [n] :nil) Object (pn [o] :obj)) (pn nil))"]
  ["extend-protocol Map covers sorted" ":map"
   "(do (defprotocol Ps (ps [x])) (extend-protocol Ps Map (ps [m] :map) Object (ps [o] :obj)) (ps (sorted-map 1 2)))"]
  ["reduce over reified IReduceInit" "42"
   "(reduce + 0 (reify clojure.lang.IReduceInit (reduce [_ f init] (f (f init 40) 2))))"])

# ring-core enablement, part 2: class-name symbols evaluate to canonical
# class-name strings (so class-dispatch defmultis match), ctor sugar still
# constructs, type-hinted param vectors parse, slurp drains reader shims,
# str/replace takes fn replacements, and int needles are char codes.
(defspec "host-interop / class tokens & readers"
  ["class name evaluates to canonical string" "\"java.lang.String\"" "String"]
  ["dispatch-only class name" "\"java.io.InputStream\"" "InputStream"]
  ["(class x) matches the token" "true" "(= String (class \"abc\"))"]
  ["defmulti on class dispatches" ":str"
   "(do (defmulti cm (fn [x] (class x))) (defmethod cm String [x] :str) (cm \"a\"))"]
  ["defmethod on nil dispatch value" ":nil"
   "(do (defmulti cn (fn [x] (class x))) (defmethod cn nil [x] :nil) (defmethod cn String [x] :str) (cn nil))"]
  ["ctor sugar still constructs" "\"x\"" "(.toString (StringBuilder. \"x\"))"]
  ["return-hinted defn parses" "7" "(do (defn- hb ^bytes [b] b) (hb 7))"]
  ["hinted multi-arity parses" ":two" "((fn ([x] :one) (^String [x y] :two)) 1 2)"]
  ["slurp drains a StringReader" "\"a=1\"" "(slurp (StringReader. \"a=1\"))"]
  ["slurp accepts :encoding opts" "\"b\"" "(slurp (StringReader. \"b\") :encoding \"UTF-8\")"]
  ["replace with fn replacement is literal" "\"$0\""
   "(do (require (quote [clojure.string :as s9])) (s9/replace \"x\" #\".\" (fn [m] \"$0\")))"]
  ["replace fn gets group vector" "\"v=k\""
   "(do (require (quote [clojure.string :as s9])) (s9/replace \"k=v\" #\"(\\w+)=(\\w+)\" (fn [[_ k v]] (str v \"=\" k))))"]
  ["indexOf int needle is a char code" "1" "(.indexOf \"a=b\" 61)"])

# JOLT_BAKE_ENV_ALLOWLIST (jolt-s3j): with the allowlist set, System/getenv
# serves only the listed names — so an image bake can't marshal the builder's
# secrets into the binary. Unset, reads are live and unfiltered.
(defspec "host-interop / bake env scrub"
  ["unlisted name reads nil under the allowlist" "nil"
   "(do (janet.os/setenv \"JOLT_BAKE_ENV_ALLOWLIST\" \"PATH\") (let [r (System/getenv \"HOME\")] (janet.os/setenv \"JOLT_BAKE_ENV_ALLOWLIST\" nil) r))"]
  ["listed name still reads" "true"
   "(do (janet.os/setenv \"JOLT_BAKE_ENV_ALLOWLIST\" \"HOME\") (let [r (System/getenv \"HOME\")] (janet.os/setenv \"JOLT_BAKE_ENV_ALLOWLIST\" nil) (string? r)))"]
  ["full snapshot filtered to the allowlist" "true"
   "(do (janet.os/setenv \"JOLT_BAKE_ENV_ALLOWLIST\" \"HOME\") (let [e (System/getenv)] (janet.os/setenv \"JOLT_BAKE_ENV_ALLOWLIST\" nil) (and (contains? (set (keys e)) \"HOME\") (= 1 (count (keys e))))))"]
  ["no allowlist: unfiltered live reads" "true"
   "(string? (System/getenv \"HOME\"))"])

# Host-class shim registration exposed to Clojure (reitit.Trie mirror, etc.):
# statics resolve as (Class/method ...), ctors as (Class. ...), and registered
# tag methods dispatch. Also: .getMessage on an exception/string, HashMap.
(defspec "host-interop / exception + HashMap shims"
  ["getMessage on a thrown string" "\"boom\""
   "(try (throw \"boom\") (catch Throwable e (.getMessage e)))"]
  ["getMessage on ex-info" "\"bad\""
   "(try (throw (ex-info \"bad\" {})) (catch Throwable e (.getMessage e)))"]
  ["HashMap get" "2"
   "(let [m (HashMap. {:a 1 :b 2})] (.get m :b))"]
  ["HashMap put + size" "2"
   "(let [m (HashMap. {})] (.put m :x 1) (.put m :y 2) (.size m))"])

# Reader-feature toggle exposed to Clojure (scoped clj-lib loading): a
# namespace can load a clj-targeted library under :clj without forcing the
# whole process — set features, require, restore.
(defspec "host-interop / reader-feature toggle"
  ["features default to jolt+default" "true"
   "(contains? (set (__reader-features)) \"jolt\")"]
  ["set + read back" "true"
   "(do (def prev (__reader-features)) (__reader-features-set! [\"clj\" \"jolt\" \"default\"]) (def r (contains? (set (__reader-features)) \"clj\")) (__reader-features-set! prev) r)"]
  ["restore returns to default" "false"
   "(do (def prev (__reader-features)) (__reader-features-set! [\"clj\"]) (__reader-features-set! prev) (contains? (set (__reader-features)) \"clj\"))"])

# JVM class shims migratus relies on. Exception constructors resolve as bare
# class symbols (jolt-6xk) and carry a message; Character/Thread/Long statics;
# java.sql.Timestamp is the millis number; SimpleDateFormat formats UTC.
(defspec "host-interop / migratus class shims"
  ["Exception. message"        "\"boom\""
   "(try (throw (Exception. \"boom\")) (catch Throwable e (.getMessage e)))"]
  ["IllegalArgumentException."  "\"bad\""
   "(try (throw (IllegalArgumentException. \"bad\")) (catch Exception e (.getMessage e)))"]
  ["InterruptedException."      "\"stop\""
   "(try (throw (InterruptedException. \"stop\")) (catch Throwable e (.getMessage e)))"]
  ["Character/isUpperCase"      "true"   "(Character/isUpperCase \\A)"]
  ["Character/isLowerCase"      "true"   "(Character/isLowerCase \\a)"]
  ["Character/isUpperCase neg"  "false"  "(Character/isUpperCase \\a)"]
  ["Thread/interrupted"         "false"  "(Thread/interrupted)"]
  ["Long/valueOf"               "42"     "(Long/valueOf \"42\")"]
  ["Timestamp is millis"        "1000"   "(.getTime (java.util.Date. (java.sql.Timestamp. 1000)))"]
  ["SimpleDateFormat UTC"       "\"19700101000000\""
   "(let [f (doto (java.text.SimpleDateFormat. \"yyyyMMddHHmmss\") (.setTimeZone (java.util.TimeZone/getTimeZone \"UTC\")))] (.format f (java.util.Date. 0)))"])

# java.io.File model (jolt-hjw): io/file builds a value that answers
# (instance? File _) so migratus's File-vs-jar branch takes the filesystem path;
# the method surface and a File-aware file-seq back it; str/slurp coerce to path.
(defspec "host-interop / java.io.File"
  ["instance? File"      "true"   "(do (require '[clojure.java.io :as io]) (instance? java.io.File (io/file \"/a/b\")))"]
  ["str is the path"     "\"/a/b\"" "(do (require '[clojure.java.io :as io]) (str (io/file \"/a/b\")))"]
  ["getName"             "\"c.txt\"" "(do (require '[clojure.java.io :as io]) (.getName (io/file \"/a/b/c.txt\")))"]
  ["getPath joins"       "\"/a/b\"" "(do (require '[clojure.java.io :as io]) (.getPath (io/file \"/a\" \"b\")))"]
  ["isDirectory of repo dir" "true" "(do (require '[clojure.java.io :as io]) (.isDirectory (io/file \"docs\")))"]
  ["isFile of repo file" "true"    "(do (require '[clojure.java.io :as io]) (.isFile (io/file \"project.janet\")))"]
  ["exists is false off-disk" "false" "(do (require '[clojure.java.io :as io]) (.exists (io/file \"/no/such/path/xyz\")))"]
  ["file-seq yields File values" "true"
   "(do (require '[clojure.java.io :as io]) (every? (fn [f] (instance? java.io.File f)) (file-seq (io/file \"docs\"))))"]
  ["file-seq finds files"  "true"
   "(do (require '[clojure.java.io :as io]) (pos? (count (filter (fn [f] (.isFile f)) (file-seq (io/file \"docs\"))))))"])

# Host shims that let libraries like clojure.tools.logging load: no STM (a
# transaction is never running) and a minimal clojure.pprint (used by spy).
(defspec "host-interop / logging host shims"
  ["LockingTransaction/isRunning" "false" "(clojure.lang.LockingTransaction/isRunning)"]
  ["pprint writes value"  "\"[1 2 3]\\n\""
   "(do (require '[clojure.pprint :as pp]) (with-out-str (pp/pprint [1 2 3])))"]
  ["with-pprint-dispatch runs body" "42"
   "(do (require '[clojure.pprint :as pp]) (pp/with-pprint-dispatch pp/code-dispatch 42))"])

# Language capabilities that real-world macros like clojure.tools.logging exercise:
# multi-arity defmacro dispatch, conditional-eval macros (short-circuit like
# debug), and macros that eval+print+return (like spy). Self-contained — no
# external library needed.
(defspec "host-interop / macro dispatch & short-circuit patterns"
  ["conditional-eval suppresses" "0"
   "(do (def ^:dynamic *enabled* false) (defmacro when-on [& body] `(when *enabled* ~@body)) (let [a (atom 0)] (when-on (reset! a 9)) @a))"]
  ["conditional-eval fires" "9"
   "(do (def ^:dynamic *enabled* true) (defmacro when-on [& body] `(when *enabled* ~@body)) (let [a (atom 0)] (when-on (reset! a 9)) @a))"]
  ["spy-like eval+print+return" "3"
   "(do (defmacro spylog [expr] `(let [v# ~expr] (println v#) v#)) (spylog (+ 1 2)))"]
  ["multi-arity 4 dispatch" "[:single :double :triple :quad]"
   "(do (defmacro ml ([a] :single) ([a b] :double) ([a b c] :triple) ([a b c d] :quad)) [(ml 1) (ml 1 2) (ml 1 2 3) (ml 1 2 3 4)])"])
